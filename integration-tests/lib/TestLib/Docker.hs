{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE ViewPatterns #-}

module TestLib.Docker where

import Control.Monad.IO.Unlift
import Control.Monad.Logger
import qualified Data.ByteString.Lazy.Char8 as BL8
import Data.Either
import Data.IP
import qualified Data.Map as M
import Data.String.Interpolate
import Data.Text as T
import DockerEngine.API.Network
import DockerEngine.Client
import DockerEngine.Core
import DockerEngine.MimeTypes
import DockerEngine.Model hiding (Map)
import GHC.Stack
import Network.HTTP.Client as NH
import Network.HTTP.Types.Status as HTTP
import qualified Network.Socket as S
import qualified Network.Socket.ByteString as SBS
import Test.Sandwich
import UnliftIO.Exception


data DockerState = DockerState {
  dockerEngineConfig :: DockerEngineConfig
  , dockerHttpManager :: Manager
  }

dockerState :: Label "dockerState" DockerState
dockerState = Label
type HasDockerState context = HasLabel context "dockerState" DockerState

introduceDockerState :: (
  HasCallStack, MonadUnliftIO m
  ) => SpecFree (LabelValue "dockerState" DockerState :> context) m () -> SpecFree context m ()
introduceDockerState = introduce "introduce Docker state" dockerState getDockerState (const $ return ())

getDockerState :: MonadLoggerIO m => m DockerState
getDockerState = do
  config <- liftIO (newConfig >>= return)
  manager <- liftIO $ newUnixDomainSocketManager "/var/run/docker.sock"
  return $ DockerState config manager
  where
    newUnixDomainSocketManager :: FilePath -> IO Manager
    newUnixDomainSocketManager path = do
      newManager $ defaultManagerSettings { managerRawConnection = return $ openUnixSocket path }
      where
        openUnixSocket filePath _ _ _ = do
          s <- S.socket S.AF_UNIX S.Stream S.defaultProtocol
          S.connect s (S.SockAddrUnix filePath)
          makeConnection (SBS.recv s 8096)
                         (SBS.sendAll s)
                         (S.close s)

runDockerEngineLBS :: (HasCallStack, Produces req accept, MimeType contentType, MonadLoggerIO m)
  => DockerState -> DockerEngineRequest req contentType res accept -> m (NH.Response BL8.ByteString)
runDockerEngineLBS ds req = do
  runDockerEngineLBS' ds req

runDockerEngineLBS' :: (HasCallStack, Produces req accept, MimeType contentType, MonadLoggerIO m)
  => DockerState -> DockerEngineRequest req contentType res accept -> m (NH.Response BL8.ByteString)
runDockerEngineLBS' (DockerState config manager) req = do
  debug [i|---> #{req}|]
  liftIO $ dispatchLbs manager config req



-- * Networks

doesNetworkExist :: (
  HasCallStack, MonadLoggerIO m, MonadUnliftIO m
  ) => DockerState -> Text -> m Bool
doesNetworkExist ds networkName = isRight <$> inspectNetwork ds (Id networkName)

inspectNetwork :: (
  HasCallStack, MonadLoggerIO m, MonadUnliftIO m
  ) => DockerState -> Id -> m (Either Text Network)
inspectNetwork (DockerState config manager) ident = leftOnException $ do
  let req = networkInspect ident
  debug [i|---> #{req}|]
  liftIO (dispatchMime manager config req) >>= \case
    MimeResult (Left err) _ -> return $ Left [i|(#{ident}) inspectNetwork failed: '#{err}'|]
    MimeResult (Right result) _ -> return $ Right result

createNetwork :: (HasCallStack, MonadUnliftIO m, MonadLoggerIO m) => DockerState -> Text -> M.Map Text Text -> m (Either Text ())
createNetwork ds networkName labels = leftOnException $ do
  let networkConfig = (mkNetworkCreateRequest networkName) {
        networkCreateRequestLabels = Just $ M.mapKeys T.unpack labels
        }
  let req = networkCreate networkConfig
  runDockerEngineLBS ds req >>= \case
    (is2xx -> True) -> return $ Right ()
    x@(is403 -> True) -> return $ Left [i|Failed to create network '#{networkName}'. operation not supported for pre-defined networks: '#{x}'|]
    x@(is404 -> True) -> return $ Left [i|Failed to create network '#{networkName}'. Plugin not found: '#{x}'|]
    x@(is5xx -> True) -> return $ Left [i|Server error in createNetwork for '#{networkName}': '#{x}'|]
    x -> return $ Left [i|Unexpected response in createNetwork for '#{networkName}': '#{x}'|]


-- * HTTP

is2xx :: NH.Response a -> Bool
is2xx (responseStatus -> (HTTP.Status code _)) = code >= 200 && code < 300

-- is304 :: NH.Response a -> Bool
-- is304 (responseStatus -> (HTTP.Status code _)) = code == 304

-- is400 :: NH.Response a -> Bool
-- is400 (responseStatus -> (HTTP.Status code _)) = code == 400

is403 :: NH.Response a -> Bool
is403 (responseStatus -> (HTTP.Status code _)) = code == 403

is404 :: NH.Response a -> Bool
is404 (responseStatus -> (HTTP.Status code _)) = code == 404

-- is409 :: NH.Response a -> Bool
-- is409 (responseStatus -> (HTTP.Status code _)) = code == 409

is5xx :: NH.Response a -> Bool
is5xx (responseStatus -> (HTTP.Status code _)) = code >= 500 && code < 600

-- * Util

leftOnException :: (MonadUnliftIO m) => m (Either Text a) -> m (Either Text a)
leftOnException = handleAny $ \e -> return $ Left $ T.pack $ case fromException e of
  Just (Reason _ msg) -> msg
  _ -> show e

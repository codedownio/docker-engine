{-# LANGUAGE ViewPatterns #-}

module TestLib.Docker.Networks where

import Control.Monad.Catch
import Control.Monad.IO.Unlift
import Control.Monad.Logger
import Data.Either
import qualified Data.Map as M
import Data.String.Interpolate
import Data.Text as T
import DockerEngine.API.Network
import DockerEngine.Client
import DockerEngine.Model hiding (Map)
import GHC.Stack
import Test.Sandwich
import TestLib.Docker.Core
import TestLib.Docker.Types
import TestLib.Docker.Util


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

createNetwork :: (
  HasCallStack, MonadUnliftIO m, MonadLoggerIO m, MonadFail m, MonadThrow m
  ) => DockerState -> Text -> M.Map Text Text -> m Id
createNetwork ds networkName labels = do
  let networkConfig = (mkNetworkCreateRequest networkName) {
        networkCreateRequestLabels = Just $ M.mapKeys T.unpack labels
        }
  NetworkCreateResponse {networkCreateResponseId=(Just x)} <- runDockerException ds (networkCreate networkConfig)
  return (Id x)

deleteNetwork :: (HasCallStack, MonadUnliftIO m, MonadLoggerIO m, MonadThrow m) => DockerState -> Id -> m ()
deleteNetwork ds networkId = do
  _ <- runDockerException ds (networkDelete networkId)
  return ()

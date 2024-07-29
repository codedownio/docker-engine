
module TestLib.Docker.Core where

import Control.Monad.Catch (MonadThrow)
import Control.Monad.IO.Unlift
import Control.Monad.Logger
import Data.String.Interpolate
import DockerEngine.Client
import DockerEngine.Core
import DockerEngine.MimeTypes
import GHC.Stack
import Network.HTTP.Client as NH
import qualified Network.Socket as S
import qualified Network.Socket.ByteString as SBS
import Test.Sandwich
import TestLib.Docker.Types


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

runDocker :: (
  HasCallStack, Produces req accept, MimeType contentType, MimeUnrender accept res, MonadLoggerIO m
  )
  => DockerState
  -> DockerEngineRequest req contentType res accept
  -> m (MimeResult res)
runDocker (DockerState config manager) req = do
  debug [i|---> #{req}|]
  liftIO $ dispatchMime manager config req

runDocker' :: (
  HasCallStack, Produces req accept, MimeType contentType, MimeUnrender accept res, MonadLoggerIO m
  )
  => DockerState
  -> DockerEngineRequest req contentType res accept
  -> m (Either MimeError res)
runDocker' (DockerState config manager) req = do
  debug [i|---> #{req}|]
  liftIO $ dispatchMime' manager config req

runDockerException :: (
  HasCallStack, Produces req accept, MimeType contentType, MimeUnrender accept res, MonadLoggerIO m, MonadThrow m
  )
  => DockerState
  -> DockerEngineRequest req contentType res accept
  -> m res
runDockerException (DockerState config manager) req = do
  debug [i|---> #{req}|]
  liftIO (dispatchMime' manager config req) >>= \case
    Left mimeError -> expectationFailure [i|Got MimeError: #{mimeError}|]
    Right x -> return x

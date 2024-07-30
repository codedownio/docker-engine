{-# LANGUAGE ViewPatterns #-}

module TestLib.Docker.Containers where

import Control.Monad.Catch
import Control.Monad.IO.Unlift
import Control.Monad.Logger
import Data.Either
import qualified Data.Map as M
import Data.String.Interpolate
import Data.Text as T
import DockerEngine.API.Container
import DockerEngine.Client
import DockerEngine.Core
import DockerEngine.MimeTypes
import DockerEngine.Model hiding (Map)
import GHC.Stack
import Test.Sandwich
import TestLib.Docker.Core
import TestLib.Docker.Types
import TestLib.Docker.Util


doesContainerExist :: (
  HasCallStack, MonadLoggerIO m, MonadUnliftIO m
  ) => DockerState -> Text -> m Bool
doesContainerExist ds containerName = isRight <$> inspectContainer ds (Id containerName)

inspectContainer :: (
  HasCallStack, MonadLoggerIO m, MonadUnliftIO m
  ) => DockerState -> Id -> m (Either Text ContainerInspectResponse)
inspectContainer (DockerState config manager) ident = leftOnException $ do
  let req = containerInspect ident
  debug [i|---> #{req}|]
  liftIO (dispatchMime manager config req) >>= \case
    MimeResult (Left err) _ -> return $ Left [i|(#{ident}) inspectContainer failed: '#{err}'|]
    MimeResult (Right result) _ -> return $ Right result

createContainer :: (
  HasCallStack, MonadUnliftIO m, MonadLoggerIO m, MonadFail m, MonadThrow m
  ) => DockerState -> Text -> Text -> M.Map Text Text -> m Id
createContainer ds containerName image labels = do
  let containerConfig = mkContainerCreateRequest {
        containerCreateRequestLabels = Just $ M.mapKeys T.unpack labels
        , containerCreateRequestImage = Just image
        }

  let req = containerCreate (ContentType MimeJSON) containerConfig
          -&- (Name containerName)

  ContainerCreateResponse {containerCreateResponseId=x} <- runDockerException ds req
  return (Id x)

deleteContainer :: (HasCallStack, MonadUnliftIO m, MonadLoggerIO m, MonadThrow m) => DockerState -> Id -> m ()
deleteContainer ds containerId = do
  _ <- runDockerException ds (containerDelete containerId)
  return ()

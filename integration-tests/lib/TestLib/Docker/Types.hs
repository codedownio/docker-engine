{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}

module TestLib.Docker.Types where


import DockerEngine.Core
import Network.HTTP.Client as NH
import Test.Sandwich


data DockerState = DockerState {
  dockerEngineConfig :: DockerEngineConfig
  , dockerHttpManager :: Manager
  }

dockerState :: Label "dockerState" DockerState
dockerState = Label
type HasDockerState context = HasLabel context "dockerState" DockerState

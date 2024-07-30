{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE ViewPatterns #-}

module TestLib.Docker (
  introduceDockerState

  , module TestLib.Docker.Core
  , module TestLib.Docker.Types
  , module TestLib.Docker.Util

  , module TestLib.Docker.Containers
  , module TestLib.Docker.Networks
  ) where

import Control.Monad.IO.Unlift
import GHC.Stack
import Test.Sandwich
import TestLib.Docker.Containers
import TestLib.Docker.Core
import TestLib.Docker.Networks
import TestLib.Docker.Types
import TestLib.Docker.Util


introduceDockerState :: (
  HasCallStack, MonadUnliftIO m
  ) => SpecFree (LabelValue "dockerState" DockerState :> context) m () -> SpecFree context m ()
introduceDockerState = introduce "introduce Docker state" dockerState getDockerState (const $ return ())

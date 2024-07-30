module Main where

import Data.String.Interpolate
import DockerEngine.Model
import Test.Sandwich
import TestLib.Docker


basic :: TopSpec
basic = introduceDockerState $ do
  describe "Networks" $ do
    it "creates and deletes a container" $ do
      ds <- getContext dockerState

      let name = "test-container"
      let image = "busybox"

      doesContainerExist ds name >>= (`shouldBe` False)

      containerId <- createContainer ds name image mempty
      info [i|Created container ID: #{containerId}|]

      inspectResult <- inspectContainer ds containerId
      info [i|Inspect result by ID: #{inspectResult}|]

      inspectResult' <- inspectContainer ds (Id name)
      info [i|Inspect result by name: #{inspectResult'}|]

      doesContainerExist ds name >>= (`shouldBe` True)

      deleteContainer ds containerId
      doesContainerExist ds name >>= (`shouldBe` False)

    it "creates and deletes a network" $ do
      ds <- getContext dockerState

      let name = "test-network"
      doesNetworkExist ds name >>= (`shouldBe` False)

      networkId <- createNetwork ds name mempty
      doesNetworkExist ds name >>= (`shouldBe` True)

      deleteNetwork ds networkId
      doesNetworkExist ds name >>= (`shouldBe` False)

main :: IO ()
main = runSandwichWithCommandLineArgs defaultOptions basic

module Main where

import Test.Sandwich
import TestLib.Docker


basic :: TopSpec
basic = introduceDockerState $ do
  describe "Networks" $ do
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

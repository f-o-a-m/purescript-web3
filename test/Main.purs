module Test.Main where

import Prelude

import Data.Identity (Identity(..))
import Data.Maybe (Maybe(..))
import Data.Newtype (un)
import Effect (Effect)
import Effect.Aff (Aff, Milliseconds(..), launchAff_)
import Effect.Class (liftEffect)
import Network.Ethereum.Web3.Types.Provider (httpProvider)
import Test.Spec (Spec, SpecT, mapSpecTree)
import Test.Spec.Reporter.Console (consoleReporter)
import Test.Spec.Runner (defaultConfig, runSpecT)
import Web3Spec.Contract (simpleStorageSpec)
import Web3Spec.Encoding.Containers (encodingContainersSpec)
import Web3Spec.Encoding.DataSpec (dataMakerSpec)
import Web3Spec.Encoding.Generic (encodingGenericSpec)
import Web3Spec.Encoding.Simple (encodingSimpleSpec)
import Web3Spec.EtherUnitSpec (etherUnitTests)
import Web3Spec.Live.LiveSpec (liveSpec)
import Web3Spec.Types.Vector (vectorSpec)


main :: Effect Unit
main = launchAff_ do
  let cfg = defaultConfig {timeout = Just (Milliseconds $ 60.0 * 1000.0)}
  p <- liftEffect $ httpProvider "http://localhost:8545"
  runSpecT cfg [consoleReporter] do
    hoist do
      dataMakerSpec
      vectorSpec
      encodingContainersSpec
      encodingSimpleSpec
      encodingGenericSpec
      simpleStorageSpec
      etherUnitTests
    liveSpec p
  where
    hoist :: Spec ~> SpecT Aff Unit Aff
    hoist = mapSpecTree (pure <<< un Identity) identity

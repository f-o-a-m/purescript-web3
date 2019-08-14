module Test.Main where

import Prelude

import Data.Identity (Identity(..))
import Data.Maybe (Maybe(..))
import Data.Newtype (un)
import Effect (Effect)
import Effect.Aff (Milliseconds(..), launchAff_)
import Network.Ethereum.Web3.Types.Provider (httpProvider)
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
main = do
  let cfg =  defaultConfig {timeout = Just (Milliseconds $ 60.0 * 1000.0)}
      idRunner = void <<< un Identity <<< runSpecT cfg [consoleReporter]
  p <- httpProvider "http://localhost:8545"
  launchAff_ do
    idRunner do
      dataMakerSpec
      vectorSpec
      encodingContainersSpec
      encodingSimpleSpec
      encodingGenericSpec
      simpleStorageSpec
      etherUnitTests
    runSpecT cfg [consoleReporter] do
      liveSpec p

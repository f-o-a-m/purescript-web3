module Test.Main where

import Prelude

import Data.Maybe (Maybe(..))
import Control.Monad.Eff (Eff)
import Network.Ethereum.Web3.Types (ETH)
import Test.Spec.Reporter.Console (consoleReporter)
import Test.Spec.Runner (RunnerEffects, run', defaultConfig)
import Web3Spec.Contract (simpleStorageSpec)
import Web3Spec.Encoding.Containers (encodingContainersSpec)
import Web3Spec.Encoding.Generic (encodingGenericSpec)
import Web3Spec.Encoding.Simple (encodingSimpleSpec)
import Web3Spec.EtherUnitSpec (etherUnitTests)
import Web3Spec.Types.Vector (vectorSpec)
import Web3Spec.Live.LiveSpec (liveSpec)
import Web3Spec.Encoding.DataSpec (dataMakerSpec)
import Network.Ethereum.Web3.Types.Provider (httpProvider)


main :: Eff (RunnerEffects (eth :: ETH)) Unit
main = do
  p <- httpProvider "http://localhost:8545"
  run' defaultConfig {timeout = Just (60 * 1000)} [consoleReporter] $ do
    dataMakerSpec
    vectorSpec
    encodingContainersSpec
    encodingSimpleSpec
    encodingGenericSpec
    simpleStorageSpec
    etherUnitTests
    liveSpec p

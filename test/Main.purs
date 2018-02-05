module Test.Main where

import Prelude

import Control.Monad.Eff (Eff)
import Network.Ethereum.Web3.Types (ETH)
import Test.Spec.Reporter.Console (consoleReporter)
import Test.Spec.Runner (RunnerEffects, run)
import Web3Spec.Contract (simpleStorageSpec)
import Web3Spec.Encoding.Containers (encodingContainersSpec)
import Web3Spec.Encoding.Generic (encodingGenericSpec)
import Web3Spec.Encoding.Simple (encodingSimpleSpec)
import Web3Spec.Types.BigNumber (bigNumberSpec)
import Web3Spec.Types.Sha3 (sha3Spec)
import Web3Spec.Types.Utils (utilsSpec)

main :: Eff (RunnerEffects (eth :: ETH)) Unit
main = run [consoleReporter] $ do
  sha3Spec
  utilsSpec
  bigNumberSpec
  encodingContainersSpec
  encodingSimpleSpec
  encodingGenericSpec
  simpleStorageSpec

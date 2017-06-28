module Test.Main where

import Prelude
import Control.Monad.Eff (Eff)
import Web3Spec.Utils.Sha3 (sha3Spec)
import Web3Spec.Utils.Utils (utilsSpec)
import Web3Spec.Utils.BigNumber (bigNumberSpec)
import Test.Spec.Reporter.Console (consoleReporter)
import Test.Spec.Runner (RunnerEffects, run)

main :: Eff (RunnerEffects ()) Unit
main = run [consoleReporter] $ do
  sha3Spec
  utilsSpec
  bigNumberSpec


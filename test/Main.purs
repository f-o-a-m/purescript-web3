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
import Web3Spec.Encoding.Containers as EncodingContainersSpec
import Web3Spec.Encoding.DataSpec as EncodingDataSpec
import Web3Spec.Encoding.Generic as EncodingGenericSpec
import Web3Spec.Encoding.Simple as EncodingSimpleSpec
import Web3Spec.Live.SimpleStorage as SimpleStorageSpec
import Web3Spec.Live.RPC as RPCSpec
import Web3Spec.Types.EtherUnit as EtherUnitSpec
import Web3Spec.Types.Vector as VectorSpec


main :: Effect Unit
main = launchAff_ do
  let cfg = defaultConfig {timeout = Just (Milliseconds $ 60.0 * 1000.0)}
  p <- liftEffect $ httpProvider "http://localhost:8545"
  join $ runSpecT cfg [consoleReporter] do
    hoist do
      EncodingDataSpec.spec
      VectorSpec.spec
      EncodingContainersSpec.spec
      EncodingSimpleSpec.spec
      EncodingGenericSpec.spec
      EtherUnitSpec.spec
    RPCSpec.spec p
    SimpleStorageSpec.spec p
  where
    hoist :: Spec ~> SpecT Aff Unit Aff
    hoist = mapSpecTree (pure <<< un Identity) identity

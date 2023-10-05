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
import Web3Spec.Encoding.ContainersSpec as EncodingContainersSpec
import Web3Spec.Encoding.DataSpec as EncodingDataSpec
import Web3Spec.Encoding.GenericSpec as EncodingGenericSpec
import Web3Spec.Encoding.SimpleSpec as EncodingSimpleSpec
import Web3Spec.Live.RPCSpec as RPCSpec
import Web3Spec.Types.EtherUnitSpec as EtherUnitSpec
import Web3Spec.Types.VectorSpec as VectorSpec

-- import Web3Spec.Types.EtherUnitSpec as EtherUnitSpec

main :: Effect Unit
main =
  launchAff_
    do
      let
        cfg = defaultConfig { timeout = Just (Milliseconds $ 120.0 * 1000.0) }
      p <- liftEffect $ httpProvider "http://localhost:8545"
      void $ join $ runSpecT cfg [ consoleReporter ] do
        hoist do
          --EncodingDataSpec.spec
          EncodingContainersSpec.spec
          --EncodingSimpleSpec.spec
          EncodingGenericSpec.spec
  --  EtherUnitSpec.spec
  --  VectorSpec.spec
  --RPCSpec.spec p
  where
  hoist :: Spec ~> SpecT Aff Unit Aff
  hoist = mapSpecTree (pure <<< un Identity) identity

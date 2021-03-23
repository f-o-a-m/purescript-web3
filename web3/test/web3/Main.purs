module Test.Main where

import Prelude
import Data.Identity (Identity(..))
import Data.Maybe (Maybe(..))
import Data.Newtype (un)
import Effect (Effect)
import Effect.Aff (Aff, Milliseconds(..), launchAff_)
import Effect.Class (liftEffect)
import Network.Ethereum.Web3.Types.Provider (httpProvider)
import Test.Spec (Spec, SpecT, parallel, mapSpecTree)
import Test.Spec.Reporter.Console (consoleReporter)
import Test.Spec.Runner (defaultConfig, runSpecT)
import Web3Spec.Encoding.ContainersSpec as EncodingContainersSpec
import Web3Spec.Encoding.DataSpec as EncodingDataSpec
import Web3Spec.Encoding.GenericSpec as EncodingGenericSpec
import Web3Spec.Encoding.SimpleSpec as EncodingSimpleSpec
import Web3Spec.Live.SimpleStorageSpec as SimpleStorageSpec
import Web3Spec.Live.ComplexStorageSpec as ComplexStorageSpec
import Web3Spec.Live.MockERC20Spec as MockERC20Spec
import Web3Spec.Live.PayableTestSpec as PayableTestSpec
import Web3Spec.Live.SimpleErrorTestSpec as SimpleErrorTestSpec
import Web3Spec.Live.MultifilterSpec as MultifilterSpec
import Web3Spec.Live.RPCSpec as RPCSpec
import Web3Spec.Live.FilterSpec as FilterSpec
import Web3Spec.Types.EtherUnitSpec as EtherUnitSpec
import Web3Spec.Types.VectorSpec as VectorSpec

main :: Effect Unit
main =
  launchAff_ do
    let
      cfg = defaultConfig { timeout = Just (Milliseconds $ 120.0 * 1000.0) }
    p <- liftEffect $ httpProvider "http://localhost:8545"
    join
      $ runSpecT cfg [ consoleReporter ] do
          hoist do
            EncodingDataSpec.spec
            VectorSpec.spec
            EncodingContainersSpec.spec
            EncodingSimpleSpec.spec
            EncodingGenericSpec.spec
            EtherUnitSpec.spec
          RPCSpec.spec p
          FilterSpec.spec p
          -- payable spec can't be run in parallel :/
          PayableTestSpec.spec p
          -- all of these tests only have one `it` statement and
          -- are dealing with separate contracts so they can be run
          -- in parallel
          parallel do
            SimpleStorageSpec.spec p
            ComplexStorageSpec.spec p
            MockERC20Spec.spec p
            SimpleErrorTestSpec.spec p
            MultifilterSpec.spec p
  where
  hoist :: Spec ~> SpecT Aff Unit Aff
  hoist = mapSpecTree (pure <<< un Identity) identity

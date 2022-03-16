module Test.Main where

import Prelude

import Control.Monad.Error.Class (throwError)
import Data.Identity (Identity(..))
import Data.Maybe (Maybe(..), maybe)
import Data.Newtype (un)
import Effect (Effect)
import Effect.Aff (Aff, Milliseconds(..), launchAff_)
import Effect.Class (liftEffect)
import Effect.Exception (error)
import Network.Ethereum.Web3.Types.Provider (httpProvider)
import Node.Process as Node.Process
import Test.Spec (Spec, SpecT, parallel, mapSpecTree)
import Test.Spec.Reporter.Console (consoleReporter)
import Test.Spec.Runner (defaultConfig, runSpecT)

import Web3Spec.Encoding.ContainersSpec                as Web3Spec.Encoding.ContainersSpec
import Web3Spec.Encoding.DataSpec                      as Web3Spec.Encoding.DataSpec
import Web3Spec.Encoding.GenericSpec                   as Web3Spec.Encoding.GenericSpec
import Web3Spec.Encoding.SimpleSpec                    as Web3Spec.Encoding.SimpleSpec
import Web3Spec.Live.ComplexStorageSpec  as Web3Spec.Live.ComplexStorageSpec
import Web3Spec.Live.FilterSpec          as Web3Spec.Live.FilterSpec
import Web3Spec.Live.MockERC20Spec       as Web3Spec.Live.MockERC20Spec
import Web3Spec.Live.MultifilterSpec     as Web3Spec.Live.MultifilterSpec
import Web3Spec.Live.PayableTestSpec     as Web3Spec.Live.PayableTestSpec
import Web3Spec.Live.RPCSpec             as Web3Spec.Live.RPCSpec
import Web3Spec.Live.SimpleErrorTestSpec as Web3Spec.Live.SimpleErrorTestSpec
import Web3Spec.Live.SimpleStorageSpec   as Web3Spec.Live.SimpleStorageSpec
import Web3Spec.Types.EtherUnitSpec     as Web3Spec.Types.EtherUnitSpec
import Web3Spec.Types.VectorSpec        as Web3Spec.Types.VectorSpec

main :: Effect Unit
main = do
  providerUrl <- Node.Process.lookupEnv "PROVIDER_URL" >>= maybe (throwError $ error "PROVIDER_URL is empty") pure
  launchAff_ do
    let
      cfg = defaultConfig { timeout = Just (Milliseconds $ 120.0 * 1000.0) }
    p <- liftEffect $ httpProvider providerUrl
    join
      $ runSpecT cfg [ consoleReporter ] do
          hoist do
            Web3Spec.Encoding.DataSpec.spec
            Web3Spec.Types.VectorSpec.spec
            Web3Spec.Encoding.ContainersSpec.spec
            Web3Spec.Encoding.SimpleSpec.spec
            Web3Spec.Encoding.GenericSpec.spec
            Web3Spec.Types.EtherUnitSpec.spec
          Web3Spec.Live.RPCSpec.spec p
          Web3Spec.Live.FilterSpec.spec p
          -- payable spec can't be run in parallel :/
          Web3Spec.Live.PayableTestSpec.spec p
          -- all of these tests only have one `it` statement and
          -- are dealing with separate contracts so they can be run
          -- in parallel
          parallel do
            Web3Spec.Live.SimpleStorageSpec.spec p
            Web3Spec.Live.ComplexStorageSpec.spec p
            Web3Spec.Live.MockERC20Spec.spec p
            Web3Spec.Live.SimpleErrorTestSpec.spec p
            Web3Spec.Live.MultifilterSpec.spec p
  where
  hoist :: Spec ~> SpecT Aff Unit Aff
  hoist = mapSpecTree (pure <<< un Identity) identity

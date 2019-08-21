module Web3Spec.Live.MultifilterSpec (spec) where

import Prelude

import Data.Lens ((?~))
import Effect.Aff (Aff) 
import Effect.Class.Console as C
import Network.Ethereum.Web3.Api as Api
import Network.Ethereum.Web3 (Provider, _data, _value, mkValue, Value, Wei)
import Test.Spec (SpecT, describe, it, beforeAll)
import Web3Spec.Live.Utils (deployContract)
import Web3Spec.Live.Code.Multifilter as MultifilterCode

spec :: Provider -> SpecT Aff Unit Aff Unit
spec provider =
  describe "Multifilter" $
    beforeAll ( deployContract provider C.log "Multifilter" $ \txOpts ->
                  Api.eth_sendTransaction $ txOpts # _data ?~ MultifilterCode.deployBytecode
                                                   # _value ?~ (mkValue zero :: Value Wei)
              ) $ 
      it "can receive multiple events in the correct order" \contractCfg -> do
        let {contractAddress: multifilterAddress, userAddress} = contractCfg
        pure unit
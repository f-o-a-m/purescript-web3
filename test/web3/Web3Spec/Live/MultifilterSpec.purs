module Web3Spec.Live.MultifilterSpec (spec) where

import Prelude

import Data.Lens ((?~))
import Effect.Aff (Aff) 
import Effect.Class.Console as C
import Network.Ethereum.Web3.Api as Api
import Network.Ethereum.Web3 (Provider, _data, _from, _to, _value, mkValue, Value, Wei)
import Network.Ethereum.Web3.Solidity.Sizes (s256)
import Test.Spec (SpecT, describe, it, beforeAll)
import Web3Spec.Live.Utils (deployContract, defaultTestTxOptions, mkUIntN)
import Web3Spec.Live.Code.Multifilter as MultifilterCode
import Web3Spec.Live.Contract.Multifilter as Multifilter

spec :: Provider -> SpecT Aff Unit Aff Unit
spec provider =
  describe "Multifilter" $
    beforeAll ( deployContract provider C.log "Multifilter" $ \txOpts ->
                  Api.eth_sendTransaction $ txOpts # _data ?~ MultifilterCode.deployBytecode
                                                   # _value ?~ (mkValue zero :: Value Wei)
              ) $
      it "can receive multiple events in the correct order" \contractCfg -> do
        let {contractAddress: multifilterAddress, userAddress} = contractCfg
            txOpts = defaultTestTxOptions
                       # _from ?~ userAddress
                       # _to ?~ multifilterAddress
            fireE1 n = Multifilter.fireE1 txOpts {_value : mkUIntN s256 n}
            fireE2 n = Multifilter.fireE2 txOpts {_value: mkUIntN s256 n}
        pure unit
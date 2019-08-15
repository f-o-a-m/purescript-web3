module Web3Spec.Live.ComplexStorageSpec (spec) where

import Prelude

import Data.Lens ((?~))
import Data.Tuple (Tuple(..))
import Effect.Aff (Aff)
import Network.Ethereum.Web3 (Provider, Value, Wei, _data, _from, _to, _value, mkValue, nilVector)
import Network.Ethereum.Web3.Api as Api
import Network.Ethereum.Web3.Solidity ((:<))
import Network.Ethereum.Web3.Solidity.Sizes (s16, s2, s224, s256)
import Test.Spec (SpecT, beforeAll, describe, it)
import Test.Spec.Assertions (shouldEqual)
import Type.Proxy (Proxy(..))
import Web3Spec.Live.Contract.ComplexStorage as ComplexStorage
import Web3Spec.LiveSpec.Utils (assertWeb3, defaultTestTxOptions, deployContract, mkBytesN, mkIntN, mkUIntN, takeEvent)

spec :: Provider -> SpecT Aff Unit Aff Unit
spec provider =
  describe "Complex Storage" $
    beforeAll ( deployContract provider "ComplexStorage" $ \txOpts ->
                  Api.eth_sendTransaction $ txOpts # _data ?~ ComplexStorage.deployBytecode
                                                   # _value ?~ (mkValue zero :: Value Wei)
              ) $
      it "Can encode and decode complex objects to / from a smart contract" $ \complexStorageCfg -> do
        let {contractAddress: complexStorageAddress, userAddress} = complexStorageCfg
            uint = mkUIntN s256 1
            int = mkIntN s256 $ negate 1
            bool = true
            int224 = mkIntN s224 221
            bools = true :< false :< nilVector
            ints = map (mkIntN s256)  [1, negate 1, 3]
            string = "hello"
            bytes16 = mkBytesN s16 "12345678123456781234567812345678"
            elem = mkBytesN s2 "1234"
            bytes2s = [elem :< elem :< elem :< elem :< nilVector, elem :< elem :< elem :< elem :< nilVector]
            txOptions = defaultTestTxOptions # _from ?~ userAddress
                                             # _to ?~ complexStorageAddress
            arg = { _uintVal : uint
                  , _intVal : int
                  , _boolVal : bool
                  , _int224Val : int224
                  , _boolVectorVal : bools
                  , _intListVal : ints
                  , _stringVal : string
                  , _bytes16Val : bytes16
                  , _bytes2VectorListVal : bytes2s
                  }
            setValsAction = ComplexStorage.setValues txOptions arg
        pure unit
        Tuple _ _event <- assertWeb3 provider $
          takeEvent (Proxy :: Proxy ComplexStorage.ValsSet) complexStorageAddress setValsAction
        _event `shouldEqual` ComplexStorage.ValsSet  {a: uint, b: int, c: bool, d: int224, e: bools, f: ints, g: string, h: bytes16,  i:bytes2s}

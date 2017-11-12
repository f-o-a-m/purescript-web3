module Web3Spec.Encoding.Containers (encodingContainersSpec) where


import Prelude

import Control.Monad.Aff (Aff)
import Data.ByteString as BS
import Data.Maybe (Maybe(..), fromJust)
import Network.Ethereum.Web3.Solidity (type (:&), BytesN, D1, D2, D4, D5, D6, IntN, N1, N2, N4, Singleton(..), Tuple2(..), Tuple4(..), Tuple9(..), UIntN, fromByteString, intNFromBigNumber, nilVector, uIntNFromBigNumber, (:<))
import Network.Ethereum.Web3.Solidity.AbiEncoding (class ABIEncoding, toDataBuilder, fromData)
import Network.Ethereum.Web3.Solidity.Vector (Vector, toVector)
import Network.Ethereum.Web3.Types (Address, HexString, embed, addressFromHex, hexFromString)
import Partial.Unsafe (unsafePartial)
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldEqual)


encodingContainersSpec :: forall r . Spec r Unit
encodingContainersSpec = describe "encoding-spec" do
  staticArraysTests
  dynamicArraysTests
  tuplesTest

roundTrip :: forall r a . Show a => Eq a => ABIEncoding a => a -> HexString -> Aff r Unit
roundTrip decoded encoded = do
  encoded `shouldEqual` toDataBuilder decoded
  fromData encoded `shouldEqual` Just decoded

staticArraysTests :: forall r . Spec r Unit
staticArraysTests =
    describe "statically sized array tests" do

      it "can encode statically sized vectors of addresses" do
         let mgivenElement = toVector $ [false]
             givenElement = (unsafePartial $ fromJust $ mgivenElement) :: Vector N1 Boolean
             given = (unsafePartial $ fromJust $ toVector [givenElement, givenElement]) :: Vector N2 (Vector N1 Boolean)
             expected = unsafePartial hexFromString $ "0000000000000000000000000000000000000000000000000000000000000000"
                                  <> "0000000000000000000000000000000000000000000000000000000000000000"
         roundTrip given expected

      it "can encode statically sized vectors of statically sized vectors of type bool" do
         let mgiven = toVector $ map (unsafePartial addressFromHex <<< unsafePartial hexFromString) [ "407d73d8a49eeb85d32cf465507dd71d507100c1"
                                                             , "407d73d8a49eeb85d32cf465507dd71d507100c3"
                                                             ]
             given = (unsafePartial $ fromJust $ mgiven) :: Vector N2 Address
             expected = unsafePartial hexFromString $ "000000000000000000000000407d73d8a49eeb85d32cf465507dd71d507100c1"
                                  <> "000000000000000000000000407d73d8a49eeb85d32cf465507dd71d507100c3"
         roundTrip given expected

      it "can encode statically sized vectors of statically sized bytes" do
         let elem1 = unsafePartial $ fromJust (fromByteString =<< flip BS.fromString BS.Hex "cf") :: BytesN D1
             elem2 = unsafePartial $ fromJust (fromByteString =<< flip BS.fromString BS.Hex "68") :: BytesN D1
             elem3 = unsafePartial $ fromJust (fromByteString =<< flip BS.fromString BS.Hex "4d") :: BytesN D1
             elem4 = unsafePartial $ fromJust (fromByteString =<< flip BS.fromString BS.Hex "fb") :: BytesN D1
             given = unsafePartial $ fromJust (toVector $ [elem1, elem2, elem3, elem4]) :: Vector N4 (BytesN D1)
             expected = unsafePartial hexFromString $ "cf00000000000000000000000000000000000000000000000000000000000000"
                                  <> "6800000000000000000000000000000000000000000000000000000000000000"
                                  <> "4d00000000000000000000000000000000000000000000000000000000000000"
                                  <> "fb00000000000000000000000000000000000000000000000000000000000000"
         roundTrip given expected

dynamicArraysTests :: forall r . Spec r Unit
dynamicArraysTests =
    describe "dynamically sized array tests" do

      it "can encode dynamically sized lists of bools" do
         let given = [true, true, false]
             expected = unsafePartial hexFromString $ "0000000000000000000000000000000000000000000000000000000000000003"
                                  <> "0000000000000000000000000000000000000000000000000000000000000001"
                                  <> "0000000000000000000000000000000000000000000000000000000000000001"
                                  <> "0000000000000000000000000000000000000000000000000000000000000000"
         roundTrip given expected

tuplesTest :: forall r . Spec r Unit
tuplesTest =
  describe "tuples test" do

    it "can encode 2-tuples with both static args" do
      let given = Tuple2 true false
          expected = unsafePartial hexFromString $ "0000000000000000000000000000000000000000000000000000000000000001"
                              <> "0000000000000000000000000000000000000000000000000000000000000000"
      roundTrip given expected

    it "can encode 1-tuples with dynamic arg" do
      let given = Singleton [true, false]
          expected = unsafePartial hexFromString $ "0000000000000000000000000000000000000000000000000000000000000020"
                              <> "0000000000000000000000000000000000000000000000000000000000000002"
                              <> "0000000000000000000000000000000000000000000000000000000000000001"
                              <> "0000000000000000000000000000000000000000000000000000000000000000"
      roundTrip given expected

    it "can encode 4-tuples with a mix of args -- (UInt, String, Boolean, Array Int)" do
      let given = Tuple4 1 "dave" true [1,2,3]
          expected = unsafePartial hexFromString $ "0000000000000000000000000000000000000000000000000000000000000001"
                              <> "0000000000000000000000000000000000000000000000000000000000000080"
                              <> "0000000000000000000000000000000000000000000000000000000000000001"
                              <> "00000000000000000000000000000000000000000000000000000000000000c0"
                              <> "0000000000000000000000000000000000000000000000000000000000000004"
                              <> "6461766500000000000000000000000000000000000000000000000000000000"
                              <> "0000000000000000000000000000000000000000000000000000000000000003"
                              <> "0000000000000000000000000000000000000000000000000000000000000001"
                              <> "0000000000000000000000000000000000000000000000000000000000000002"
                              <> "0000000000000000000000000000000000000000000000000000000000000003"

      roundTrip given expected


    it "can do something really complicated" do
      let uint = unsafePartial $ fromJust <<< uIntNFromBigNumber <<< embed $ 1
          int = unsafePartial $ fromJust <<< intNFromBigNumber <<< embed $ (negate 1)
          bool = true
          int224 = unsafePartial $ fromJust <<< intNFromBigNumber <<< embed $  221
          bools = true :< false :< nilVector
          ints = [ unsafePartial $ fromJust <<< intNFromBigNumber <<< embed $ 1
                 , unsafePartial $ fromJust <<< intNFromBigNumber <<< embed $ negate 1
                 , unsafePartial $ fromJust <<< intNFromBigNumber <<< embed $  3
                 ]
          string = "hello"
          bytes16 = unsafePartial $ fromJust $ fromByteString =<< flip BS.fromString BS.Hex "12345678123456781234567812345678"
          elem = unsafePartial $ fromJust $ fromByteString =<< flip BS.fromString BS.Hex "1234"
          bytes2s = [ elem :< elem :< elem :< elem :< nilVector
                    , elem :< elem :< elem :< elem :< nilVector
                    ]

          given = Tuple9 uint int bool int224 bools ints string bytes16 bytes2s :: Tuple9 (UIntN (D2 :& D5 :& D6))
                                                                                          (IntN (D2 :& D5 :& D6))
                                                                                          Boolean
                                                                                          (IntN (D2 :& D2 :& D4))
                                                                                          (Vector N2 Boolean)
                                                                                          (Array (IntN (D2 :& D5 :& D6)))
                                                                                          String
                                                                                          (BytesN (D1 :& D6))
                                                                                          (Array (Vector N4 (BytesN D2)))

          expected = unsafePartial hexFromString $ "0000000000000000000000000000000000000000000000000000000000000001"
                              <> "ffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffff"
                              <> "0000000000000000000000000000000000000000000000000000000000000001"
                              <> "00000000000000000000000000000000000000000000000000000000000000dd"
                              <> "0000000000000000000000000000000000000000000000000000000000000001"
                              <> "0000000000000000000000000000000000000000000000000000000000000000"
                              <> "0000000000000000000000000000000000000000000000000000000000000140"
                              <> "00000000000000000000000000000000000000000000000000000000000001c0"
                              <> "1234567812345678123456781234567800000000000000000000000000000000"
                              <> "0000000000000000000000000000000000000000000000000000000000000200"
                              <> "0000000000000000000000000000000000000000000000000000000000000003"
                              <> "0000000000000000000000000000000000000000000000000000000000000001"
                              <> "ffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffff"
                              <> "0000000000000000000000000000000000000000000000000000000000000003"
                              <> "0000000000000000000000000000000000000000000000000000000000000005"
                              <> "68656c6c6f000000000000000000000000000000000000000000000000000000"
                              <> "0000000000000000000000000000000000000000000000000000000000000002"
                              <> "1234000000000000000000000000000000000000000000000000000000000000"
                              <> "1234000000000000000000000000000000000000000000000000000000000000"
                              <> "1234000000000000000000000000000000000000000000000000000000000000"
                              <> "1234000000000000000000000000000000000000000000000000000000000000"
                              <> "1234000000000000000000000000000000000000000000000000000000000000"
                              <> "1234000000000000000000000000000000000000000000000000000000000000"
                              <> "1234000000000000000000000000000000000000000000000000000000000000"
                              <> "1234000000000000000000000000000000000000000000000000000000000000"

      roundTrip given expected

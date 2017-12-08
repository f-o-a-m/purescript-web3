module Web3Spec.Types.Utils (utilsSpec) where

import Prelude

import Data.Maybe (Maybe(..), fromJust)
import Network.Ethereum.Web3.Types.BigNumber (decimal, embed, parseBigNumber)
import Network.Ethereum.Web3.Types.EtherUnit (convert, Value, mkValue, toWei, Ether, Wei)
import Network.Ethereum.Web3.Types.Types (mkHexString)
import Network.Ethereum.Web3.Types.Utils (toUtf8, toAscii, fromUtf8, fromAscii, toInt)
import Partial.Unsafe (unsafePartial)
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldEqual)

utilsSpec :: forall r . Spec r Unit
utilsSpec = describe "utils-spec" do

    describe "utf tests" do

      it "can convert hex strings to utf8" do
        toUtf8 (unsafePartial (fromJust <<< mkHexString) "6d79537472696e67") `shouldEqual` "myString"
        toUtf8 (unsafePartial (fromJust <<< mkHexString) "6d79537472696e67\00") `shouldEqual` "myString"
        toUtf8 (unsafePartial (fromJust <<< mkHexString) "65787065637465642076616c7565\00\00\00\00\00\00\00\00\00\00\00\00\00\00\00\00\00\00")
          `shouldEqual` "expected value"

      it "can convert strings to hex" do
        fromUtf8 "myString" `shouldEqual` unsafePartial (fromJust <<< mkHexString) "6d79537472696e67"
        fromUtf8 "myString\00" `shouldEqual` unsafePartial (fromJust <<< mkHexString) "6d79537472696e67"
        fromUtf8 "expected value\00\00\00" `shouldEqual` unsafePartial (fromJust <<< mkHexString) "65787065637465642076616c7565"

    describe "ascii tests" do

      it "can convert hex strings to ascii" do

        toAscii (unsafePartial (fromJust <<< mkHexString) "6d79537472696e67") `shouldEqual` "myString"
        toAscii (unsafePartial (fromJust <<< mkHexString) "6d79537472696e6700") `shouldEqual` "myString\0000"
      --  toAscii ((fromJust <<< mkHexString) "0300000035e8c6d54c5d127c9dcebe9e1a37ab9b05321128d097590a3c100000000000006521df642ff1f5ec0c3a7aa6cea6b1e7b7f7cda2cbdf07362a85088e97f19ef94331c955c0e9321ad386428c")
      --    `shouldEqual` "\0003\0000\0000\00005èÆÕL]\0012|Î¾\001a7«\00052\0011(ÐY\n<\0010\0000\0000\0000\0000\0000\0000e!ßd/ñõì\f:z¦Î¦±ç·÷Í¢Ëß\00076*\bñùC1ÉUÀé2\001aÓB"

      it "can convert asci to hex" do
        fromAscii "myString" `shouldEqual` unsafePartial (fromJust <<< mkHexString) "6d79537472696e67"
        fromAscii "myString\00" `shouldEqual` unsafePartial (fromJust <<< mkHexString) "6d79537472696e6700"

    describe "int tests" do

      it "can convert hex to maybe ints" do
        toInt (unsafePartial (fromJust <<< mkHexString) "ff") `shouldEqual` (Just 255)
        toInt (unsafePartial (fromJust <<< mkHexString) "0xff") `shouldEqual` (Just 255)
        toInt (unsafePartial (fromJust <<< mkHexString) "0x1f1f1f1") `shouldEqual` (Just 32633329)
        toInt (unsafePartial (fromJust <<< mkHexString) "1f1f1f1") `shouldEqual` (Just 32633329)
        toInt (unsafePartial (fromJust <<< mkHexString) "0x000000000000") `shouldEqual` (Just 0)
        toInt (unsafePartial (fromJust <<< mkHexString) "000000000000000000000000000") `shouldEqual` (Just 0)
        toInt (unsafePartial (fromJust <<< mkHexString) "0xffffffffffffffff") `shouldEqual` Nothing
        toInt (unsafePartial (fromJust <<< mkHexString) "ffffffffffffffffffffffffffffffff") `shouldEqual` Nothing

    describe "ether conversion tests" do

      it "can convert units of ether" do
        let val1 = mkValue one :: Value Ether
        Just (toWei val1) `shouldEqual` (parseBigNumber decimal "1000000000000000000")
        let val2 = mkValue (embed 10) :: Value Ether
        Just (toWei val2) `shouldEqual` parseBigNumber decimal "10000000000000000000"
        let val3 = mkValue one :: Value Ether
            val4 = mkValue (unsafePartial $ fromJust $ parseBigNumber decimal "1000000000000000000") :: Value Wei
        convert val3 `shouldEqual` val4

--setCount(int count) returns (bool)
--
--"setcount(int)" -->> sha >>> -take 4 bytes = "abcd"
--
--data GetCount = GetCount ()
--....
--
--data SetCount = SetCount (Bytes (D1 :& D2))
--
--instance AbiEncoding SetCount where
--   toDataBuilder (SetCount n) = "abcd" <> toDatabuilder n
--   fromDataParser = error "Don't need this"
--
--setCount :: Int -> Web3A Bool
--setCount n = call simpleStorage Latest (SetCount n)

module Web3Spec.Types.Utils (utilsSpec) where

import Prelude
import Data.Either (Either(Right))
import Data.Maybe  (Maybe(Just))
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldEqual)
import Network.Ethereum.Web3.Types.Utils (toUtf8, toAscii, fromUtf8, fromAscii
                                         , EtherUnit(..), toWei, fromWei)
import Network.Ethereum.Web3.Types.Types (HexString(..))
import Network.Ethereum.Web3.Types.BigNumber (decimal, embed, parseBigNumber)

utilsSpec :: forall r . Spec r Unit
utilsSpec = describe "utils-spec" do

    describe "utf tests" do

      it "can convert hex strings to utf8" do
        toUtf8 (HexString "6d79537472696e67") `shouldEqual` "myString"
        toUtf8 (HexString "6d79537472696e67\00") `shouldEqual` "myString"
        toUtf8 (HexString "65787065637465642076616c7565\00\00\00\00\00\00\00\00\00\00\00\00\00\00\00\00\00\00")
          `shouldEqual` "expected value"

      it "can convert strings to hex" do
        fromUtf8 "myString" `shouldEqual` HexString "6d79537472696e67"
        fromUtf8 "myString\00" `shouldEqual` HexString "6d79537472696e67"
        fromUtf8 "expected value\00\00\00" `shouldEqual` HexString "65787065637465642076616c7565"

    describe "ascii tests" do

      it "can convert hex strings to ascii" do

        toAscii (HexString "6d79537472696e67") `shouldEqual` "myString"
        toAscii (HexString "6d79537472696e6700") `shouldEqual` "myString\0000"
      --  toAscii (HexString "0300000035e8c6d54c5d127c9dcebe9e1a37ab9b05321128d097590a3c100000000000006521df642ff1f5ec0c3a7aa6cea6b1e7b7f7cda2cbdf07362a85088e97f19ef94331c955c0e9321ad386428c")
      --    `shouldEqual` "\0003\0000\0000\00005èÆÕL]\0012|Î¾\001a7«\00052\0011(ÐY\n<\0010\0000\0000\0000\0000\0000\0000e!ßd/ñõì\f:z¦Î¦±ç·÷Í¢Ëß\00076*\bñùC1ÉUÀé2\001aÓB"

      it "can convert asci to hex" do
        fromAscii "myString" `shouldEqual` HexString "6d79537472696e67"
        fromAscii "myString\00" `shouldEqual` HexString "6d79537472696e6700"

    describe "ether conversion tests" do

      it "can convert units of ether" do
        Just (toWei one Ether) `shouldEqual` parseBigNumber decimal "100000000000000000"
        Just (toWei (embed 10) TEther) `shouldEqual` parseBigNumber decimal "10000000000000000000000000000"
        (fromWei (embed 1000) KWei) `shouldEqual` (embed 1)

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









module Web3Spec.Utils.Utils (utilsSpec) where

import Prelude
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldEqual)
import Web3.Utils.Utils (toUtf8, toAscii, fromUtf8, fromAscii)
import Web3.Utils.Sha3 (HexString(..))

utilsSpec :: forall r . Spec r Unit
utilsSpec = describe "utils-spec" do

    describe "utf tests" do

      it "can convert hex strings to utf8" do
        toUtf8 (HexString "6d79537472696e67") `shouldEqual` "myString"
        toUtf8 (HexString "6d79537472696e6700") `shouldEqual` "myString"
        toUtf8 (HexString "65787065637465642076616c7565000000000000000000000000000000000000")
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


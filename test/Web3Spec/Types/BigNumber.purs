module Web3Spec.Types.BigNumber (bigNumberSpec) where


import Prelude
import Data.Maybe (Maybe(Just))
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldEqual)
import Network.Ethereum.Web3.Types.BigNumber (BigNumber, decimal, embed, hexadecimal, parseBigNumber)

bigNumberSpec :: forall r . Spec r Unit
bigNumberSpec = describe "BigNumber-spec" do

    describe "toBigNumber tests" do
      it "can handle turning strings into numbers" do
       map show (parseBigNumber decimal "1")  `shouldEqual` Just  "1"
       map show (parseBigNumber hexadecimal "0x1") `shouldEqual` Just "1"
       map show (parseBigNumber hexadecimal "0x01") `shouldEqual` Just "1"
       map show (parseBigNumber decimal "15") `shouldEqual` Just "15"
       map show (parseBigNumber hexadecimal "0xf") `shouldEqual` Just "15"
       map show (parseBigNumber hexadecimal "0x0f") `shouldEqual` Just "15"
       map show (parseBigNumber decimal "-1") `shouldEqual` Just "-1"
       map show (parseBigNumber hexadecimal "-0x1") `shouldEqual` Just "-1"
       map show (parseBigNumber hexadecimal "-0x01") `shouldEqual` Just "-1"
       map show (parseBigNumber decimal "-15") `shouldEqual` Just "-15"
       map show (parseBigNumber hexadecimal "-0xf") `shouldEqual` Just "-15"
       map show (parseBigNumber hexadecimal "-0x0f") `shouldEqual` Just "-15"
       map show (parseBigNumber hexadecimal "0xffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffff") `shouldEqual` Just "115792089237316195423570985008687907853269984665640564039457584007913129639935"
       map show (parseBigNumber hexadecimal "0xfffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffd") `shouldEqual` Just "115792089237316195423570985008687907853269984665640564039457584007913129639933"
       map show (parseBigNumber hexadecimal "-0xffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffff") `shouldEqual` Just "-115792089237316195423570985008687907853269984665640564039457584007913129639935"
       map show (parseBigNumber hexadecimal "-0xfffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffd") `shouldEqual` Just "-115792089237316195423570985008687907853269984665640564039457584007913129639933"
       map show (parseBigNumber hexadecimal "0") `shouldEqual` Just "0"
       map show (parseBigNumber hexadecimal "0x0") `shouldEqual` Just "0"
       map show (parseBigNumber hexadecimal "-0") `shouldEqual` Just "0"
       map show (parseBigNumber hexadecimal "-0x0") `shouldEqual` Just "0"

      it "can handle turning ints into big numbers" do
        Just (one :: BigNumber) `shouldEqual` (parseBigNumber decimal "1")
        Just (zero :: BigNumber) `shouldEqual` (parseBigNumber decimal "0")
        Just (embed 0 :: BigNumber) `shouldEqual` (parseBigNumber decimal "0")
        Just (embed 15 :: BigNumber) `shouldEqual` (parseBigNumber decimal "15")

    describe "BigNumber arithmetic" do
      it "can add, subtract, and multiply BigNumbers as an Int-Alegbra" do
        (add <$> (parseBigNumber decimal "1") <*> (parseBigNumber decimal "1")) `shouldEqual` (parseBigNumber decimal "2")
        (sub <$> (parseBigNumber hexadecimal "0xf") <*> (parseBigNumber decimal "1")) `shouldEqual` (parseBigNumber decimal "14")
        ((parseBigNumber hexadecimal "0xf") >>= \x -> pure $ embed 15 + x) `shouldEqual` parseBigNumber decimal "30"
        ((parseBigNumber decimal "21") >>= \x -> pure $ x - zero) `shouldEqual` parseBigNumber hexadecimal "0x15"
        (Just $ one `mul` one) `shouldEqual` parseBigNumber decimal "1"
        (Just $ one * embed (-7)) `shouldEqual` parseBigNumber hexadecimal "-0x7"

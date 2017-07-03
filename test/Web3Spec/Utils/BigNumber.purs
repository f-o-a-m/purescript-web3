module Web3Spec.Utils.BigNumber (bigNumberSpec) where


import Prelude (Unit, add, discard, map, mul, negate, one, pure, show
               , sub, zero, ($), (<$>), (<*>), (>>=))
import Data.Maybe (Maybe(Just))
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldEqual)
import Web3.Utils.BigNumber

bigNumberSpec :: forall r . Spec r Unit
bigNumberSpec = describe "BigNumber-spec" do

    describe "toBigNumber tests" do
      it "can handle turning strings into numbers" do
       map show (fromString decimal "1")  `shouldEqual` Just  "1"
       map show (fromString hexadecimal "0x1") `shouldEqual` Just "1"
       map show (fromString hexadecimal "0x01") `shouldEqual` Just "1"
       map show (fromString decimal "15") `shouldEqual` Just "15"
       map show (fromString hexadecimal "0xf") `shouldEqual` Just "15"
       map show (fromString hexadecimal "0x0f") `shouldEqual` Just "15"
       map show (fromString decimal "-1") `shouldEqual` Just "-1"
       map show (fromString hexadecimal "-0x1") `shouldEqual` Just "-1"
       map show (fromString hexadecimal "-0x01") `shouldEqual` Just "-1"
       map show (fromString decimal "-15") `shouldEqual` Just "-15"
       map show (fromString hexadecimal "-0xf") `shouldEqual` Just "-15"
       map show (fromString hexadecimal "-0x0f") `shouldEqual` Just "-15"
       map show (fromString hexadecimal "0xffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffff") `shouldEqual` Just "115792089237316195423570985008687907853269984665640564039457584007913129639935"
       map show (fromString hexadecimal "0xfffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffd") `shouldEqual` Just "115792089237316195423570985008687907853269984665640564039457584007913129639933"
       map show (fromString hexadecimal "-0xffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffff") `shouldEqual` Just "-115792089237316195423570985008687907853269984665640564039457584007913129639935"
       map show (fromString hexadecimal "-0xfffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffd") `shouldEqual` Just "-115792089237316195423570985008687907853269984665640564039457584007913129639933"
       map show (fromString hexadecimal "0") `shouldEqual` Just "0"
       map show (fromString hexadecimal "0x0") `shouldEqual` Just "0"
       map show (fromString hexadecimal "-0") `shouldEqual` Just "0"
       map show (fromString hexadecimal "-0x0") `shouldEqual` Just "0"

      it "can handle turning ints into big numbers" do
        Just (one :: BigNumber) `shouldEqual` (fromString decimal "1")
        Just (zero :: BigNumber) `shouldEqual` (fromString decimal "0")
        Just (embed 0 :: BigNumber) `shouldEqual` (fromString decimal "0")
        Just (embed 15 :: BigNumber) `shouldEqual` (fromString decimal "15")

    describe "BigNumber arithmetic" do
      it "can add, subtract, and multiply BigNumbers as an Int-Alegbra" do
        (add <$> (fromString decimal "1") <*> (fromString decimal "1")) `shouldEqual` (fromString decimal "2")
        (sub <$> (fromString hexadecimal "0xf") <*> (fromString decimal "1")) `shouldEqual` (fromString decimal "14")
        ((fromString hexadecimal "0xf") >>= \x -> pure $ 15 >+ x) `shouldEqual` fromString decimal "30"
        ((fromString decimal "21") >>= \x -> pure $ x -< 0) `shouldEqual` fromString hexadecimal "0x15"
        (Just $ one `mul` one) `shouldEqual` fromString decimal "1"
        (Just $ one *< (-7)) `shouldEqual` fromString hexadecimal "-0x7"

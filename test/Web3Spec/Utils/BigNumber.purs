module Web3Spec.Utils.BigNumber (bigNumberSpec) where

import Prelude
import Data.Maybe (Maybe(..))
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldEqual)
import Web3.Utils.BigNumber (toBigNumber, embed, BigNumber)

bigNumberSpec :: forall r . Spec r Unit
bigNumberSpec = describe "BigNumber-spec" do

    describe "toBigNumber tests" do
      it "can handle turning strings into numbers" do
       show (toBigNumber  "1")  `shouldEqual`  "1"
       show (toBigNumber  "0x1") `shouldEqual` "1"
       show (toBigNumber  "0x01") `shouldEqual` "1"
       show (toBigNumber  "15") `shouldEqual` "15"
       show (toBigNumber  "0xf") `shouldEqual` "15"
       show (toBigNumber  "0x0f") `shouldEqual` "15"
       show (toBigNumber  "-1") `shouldEqual` "-1"
       show (toBigNumber  "-0x1") `shouldEqual` "-1"
       show (toBigNumber  "-0x01") `shouldEqual` "-1"
       show (toBigNumber  "-15") `shouldEqual` "-15"
       show (toBigNumber  "-0xf") `shouldEqual` "-15"
       show (toBigNumber  "-0x0f") `shouldEqual` "-15"
       show (toBigNumber  "0xffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffff") `shouldEqual` "115792089237316195423570985008687907853269984665640564039457584007913129639935"
       show (toBigNumber  "0xfffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffd") `shouldEqual` "115792089237316195423570985008687907853269984665640564039457584007913129639933"
       show (toBigNumber  "-0xffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffff") `shouldEqual` "-115792089237316195423570985008687907853269984665640564039457584007913129639935"
       show (toBigNumber  "-0xfffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffd") `shouldEqual` "-115792089237316195423570985008687907853269984665640564039457584007913129639933"
       show (toBigNumber  "0") `shouldEqual` "0"
       show (toBigNumber  "0x0") `shouldEqual` "0"
       show (toBigNumber  "-0") `shouldEqual` "0"
       show (toBigNumber  "-0x0") `shouldEqual` "0"

      it "can handle turning ints into big numbers" do
        show (one :: BigNumber) `shouldEqual` show (toBigNumber "1")
        show (zero :: BigNumber) `shouldEqual` show (toBigNumber "0")
        show (embed 0 :: BigNumber) `shouldEqual` show (toBigNumber "0")
        show (embed 15 :: BigNumber) `shouldEqual` show (toBigNumber "15")

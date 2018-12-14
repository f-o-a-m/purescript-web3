module Web3Spec.EtherUnitSpec  (etherUnitTests) where

import Prelude

import Data.Lens ((.~), (^.))
import Data.Maybe (Maybe(..))
import Data.Ring.Module (mzeroL, (^*), (^+), (^-))
import Network.Ethereum.Core.BigNumber (pow)
import Network.Ethereum.Web3 (Ether, Shannon, Szabo, Value, Wei, _value, convert, defaultTransactionOptions, embed, fromMinorUnit, mkValue, formatValue)
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldEqual)

etherUnitTests:: Spec Unit
etherUnitTests =
    describe "conversion tests" do

      it "can encode convert from a higher denomination to lower" $ do
        let inEth = convert (mkValue one :: Value Ether)
            inWei = (mkValue $ (embed 10) `pow` 18) :: Value Wei
        inEth `shouldEqual` inWei

        let shannon = mkValue (embed 10 `pow` 3) :: Value Shannon
            szabo = mkValue one :: Value Szabo
        convert shannon `shouldEqual` szabo

      it "can perform arithmetic" do
        let two = mkValue (embed 1 + embed 1) :: Value Shannon
            two' = mkValue one ^+ mkValue one
        two `shouldEqual` two'
        (two ^- two') `shouldEqual` mzeroL
        (2 ^* two') `shouldEqual` mkValue (embed 4)

      it "can use the lens properly" do
        let noPay = defaultTransactionOptions
            opts = defaultTransactionOptions # _value .~ Just (convert (mkValue one :: Value Ether))

        (noPay ^. _value) `shouldEqual` Nothing
        (opts ^. _value) `shouldEqual` (Just $ fromMinorUnit (embed 10 `pow` 18 ) :: Value Wei)

      it "can format currencies correctly" do
        let n = mkValue (embed 1) :: Value Ether
            m = convert n :: Value Wei
            -- making the loop shouldn't change the result
            n' = convert m :: Value Ether
        formatValue n `shouldEqual` "1"
        formatValue n' `shouldEqual` "1"
        formatValue m `shouldEqual` "1000000000000000000"

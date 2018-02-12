module Web3Spec.EtherUnitSpec  (etherUnitTests) where

import Prelude

import Data.Lens ((.~))
import Data.Maybe (Maybe(..))
import Data.Module (mzeroL, (^*), (^+), (^-))
import Network.Ethereum.Web3 (Ether, Shannon, Szabo, TransactionOptions, Value, Wei, _value, convert, defaultTransactionOptions, embed, mkValue, pow)
import Network.Ethereum.Web3.Types (NoPay)
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldEqual)

etherUnitTests :: forall r . Spec r Unit
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

-- this other tests are just to make sure bad things dont compil

noPay :: TransactionOptions NoPay
noPay = defaultTransactionOptions

-- fails to compile without the convert
opts :: TransactionOptions Wei
opts = defaultTransactionOptions # _value .~ Just (convert (mkValue one :: Value Ether))

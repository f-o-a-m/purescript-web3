module Web3Spec.Contract  where

import Prelude
import Test.Spec (Spec, describe, it)
import Data.Maybe (Maybe(..))
import Control.Monad.Aff (Aff)
import Test.Spec.Assertions (shouldEqual)
import Text.Parsing.Parser (fail)
import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Eff.Unsafe (unsafeCoerceEff)
import Control.Monad.Eff.Console (CONSOLE, logShow)

import Network.Ethereum.Web3.Types (HexString(..), Address(..), Web3M, ETH, unWeb3M)
import Network.Ethereum.Web3.Contract (sendTx)
import Network.Ethereum.Web3.Encoding.AbiEncoding (class ABIEncoding, toDataBuilder)

ssAddress :: Address
ssAddress = Address <<< HexString $ "c29313014a78b440876bac21be369c3047e313e7"

adminAddress :: Address
adminAddress = Address <<< HexString $ "44cba02c089789b3299069c93832b7a8b8723b3e"

data Set = Set Int

instance abiEncodingSet :: ABIEncoding Set where
  toDataBuilder (Set n) = HexString "60fe47b1" <> toDataBuilder n
  fromDataParser = fail "No function parser"

set :: Int -> Web3M () HexString
set n = sendTx (Just ssAddress) adminAddress zero (Set n)

simpleStorageSpec :: forall r . Spec (eth :: ETH, console :: CONSOLE | r) Unit
simpleStorageSpec =
  describe "interacting with a SimpleStorage Contract" do
    it "can set the value of simple storage" do
      txHash <- liftEff $ unsafeCoerceEff $ unWeb3M $ set 100
      _ <-  liftEff $ logShow txHash
      true `shouldEqual` true

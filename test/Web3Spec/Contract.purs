module Web3Spec.Contract  where

import Prelude

import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Eff.Console (logShow)
import Data.Maybe (Maybe(..))
import Network.Ethereum.Web3.Contract (sendTx)
import Network.Ethereum.Web3.Provider (class IsAsyncProvider, httpProvider, runWeb3)
import Network.Ethereum.Web3.Solidity.AbiEncoding (class ABIEncoding, toDataBuilder)
import Network.Ethereum.Web3.Types (HexString(..), Address(..), Web3(..), Ether, Value, unHex)
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldEqual)
import Text.Parsing.Parser (fail)
import Type.Proxy (Proxy(..))

ssAddress :: Address
ssAddress = Address <<< HexString $ "c29313014a78b440876bac21be369c3047e313e7"

adminAddress :: Address
adminAddress = Address <<< HexString $ "44cba02c089789b3299069c93832b7a8b8723b3e"

data Set = Set Int

instance abiEncodingSet :: ABIEncoding Set where
  toDataBuilder (Set n) = HexString "60fe47b1" <> toDataBuilder n
  fromDataParser = fail "No function parser"

data HttpProvider

http :: Proxy HttpProvider
http = Proxy

instance isAsyncHttp :: IsAsyncProvider HttpProvider where
  getAsyncProvider = Web3 <<< liftEff <<< httpProvider $ "http://localhost:8545"

setA :: forall e . Int -> Web3 HttpProvider e HexString
setA n = sendTx (Just ssAddress) adminAddress (zero :: Value Ether) (Set n)

simpleStorageSpec :: forall r . Spec _ Unit
simpleStorageSpec =
  describe "interacting with a SimpleStorage Contract" do
    it "can set the value of simple storage asynchronously" do
      txHash <- runWeb3 http $ setA 200
      _ <-  liftEff $ logShow $ "txHash: " <> unHex txHash
      true `shouldEqual` true

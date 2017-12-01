module Web3Spec.Contract  where

import Prelude

import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Eff.Console (CONSOLE, logShow)
import Data.Generic.Rep (class Generic)
import Data.Maybe (Maybe(..), fromJust)
import Network.Ethereum.Web3.Contract (sendTx)
import Network.Ethereum.Web3.Provider (class IsAsyncProvider, httpProvider, runWeb3)
import Network.Ethereum.Web3.Solidity.AbiEncoding (class ABIEncode, toDataBuilder)
import Network.Ethereum.Web3.Types (Address, ETH, Ether, HexString, Value, Web3(..), mkAddress, mkHexString, unHex)
import Partial.Unsafe (unsafePartial)
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldEqual)
import Type.Proxy (Proxy(..))

ssAddress :: Address
ssAddress = unsafePartial fromJust $ mkAddress =<< mkHexString "c29313014a78b440876bac21be369c3047e313e7"

adminAddress :: Address
adminAddress = unsafePartial fromJust $ mkAddress =<< mkHexString "44cba02c089789b3299069c93832b7a8b8723b3e"

data Set = Set Int

derive instance genericSet :: Generic Set _

instance abiEncodingSet :: ABIEncode Set where
  toDataBuilder (Set n) = (unsafePartial fromJust <<< mkHexString $ "60fe47b1") <> toDataBuilder n

data HttpProvider

http :: Proxy HttpProvider
http = Proxy

instance isAsyncHttp :: IsAsyncProvider HttpProvider where
  getAsyncProvider = Web3 <<< liftEff <<< httpProvider $ "http://localhost:8545"

setA :: forall e . Int -> Web3 HttpProvider e HexString
setA n = sendTx (Just ssAddress) adminAddress (zero :: Value Ether) (Set n)

simpleStorageSpec :: forall r. Spec (eth :: ETH, console :: CONSOLE | r) Unit
simpleStorageSpec =
  describe "interacting with a SimpleStorage Contract" do
    it "can set the value of simple storage asynchronously" do
      txHash <- runWeb3 http $ setA 200
      _ <-  liftEff $ logShow $ "txHash: " <> unHex txHash
      true `shouldEqual` true

module Web3Spec.Contract  where

import Prelude

import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Eff.Console (CONSOLE, logShow)
import Data.Functor.Tagged (Tagged, tagged)
import Data.Maybe (Maybe(..), fromJust)
import Data.Symbol (SProxy)
import Network.Ethereum.Web3.Contract (sendTx)
import Network.Ethereum.Web3.Provider (class IsAsyncProvider, httpProvider, runWeb3)
import Network.Ethereum.Web3.Solidity (type (:&), D2, D5, D6, IntN, Tuple1(..), intNFromBigNumber)
import Network.Ethereum.Web3.Types (Address, ETH, Ether, HexString, Value, Web3(..), embed, mkAddress, mkHexString, unHex)
import Partial.Unsafe (unsafePartial)
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldEqual)
import Type.Proxy (Proxy(..))

ssAddress :: Address
ssAddress = unsafePartial fromJust $ mkAddress =<< mkHexString "c29313014a78b440876bac21be369c3047e313e7"

adminAddress :: Address
adminAddress = unsafePartial fromJust $ mkAddress =<< mkHexString "44cba02c089789b3299069c93832b7a8b8723b3e"

type FnSet = Tagged (SProxy "(int256)") (Tuple1 (IntN (D2 :& D5 :& D6)))

data HttpProvider

http :: Proxy HttpProvider
http = Proxy

instance isAsyncHttp :: IsAsyncProvider HttpProvider where
  getAsyncProvider = Web3 <<< liftEff <<< httpProvider $ "http://localhost:8545"

setA :: forall e . IntN (D2 :& D5 :& D6) -> Web3 HttpProvider e HexString
setA n = sendTx (Just ssAddress) adminAddress (zero :: Value Ether) ((tagged <<< Tuple1 $ n) :: FnSet)

simpleStorageSpec :: forall r. Spec (eth :: ETH, console :: CONSOLE | r) Unit
simpleStorageSpec =
  describe "interacting with a SimpleStorage Contract" do
    it "can set the value of simple storage asynchronously" do
      let n = unsafePartial fromJust <<< intNFromBigNumber <<< embed $ 200
      txHash <- runWeb3 http $ setA n
      _ <-  liftEff $ logShow $ "txHash: " <> unHex txHash
      true `shouldEqual` true

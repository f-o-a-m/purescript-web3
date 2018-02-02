module Web3Spec.Contract  where

import Prelude

import Control.Monad.Aff (Fiber)
import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Eff.Console (CONSOLE, logShow)
import Data.Functor.Tagged (Tagged, tagged)
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Eq (genericEq)
import Data.Generic.Rep.Show (genericShow)
import Data.Lens ((.~))
import Data.Maybe (Maybe(..), fromJust)
import Data.Newtype (class Newtype, wrap)
import Data.Symbol (SProxy)
import Network.Ethereum.Web3 (class IsAsyncProvider, Address, ChainCursor(..), ETH, Ether, EventAction(..), Value, Web3, _address, _fromBlock, _toBlock, _topics, defaultFilter, embed, event, forkWeb3', httpProvider, mkAddress, mkHexString, sendTx, class EventFilter, eventFilter, defaultTransactionOptions)
import Network.Ethereum.Web3.Solidity (class IndexedEvent, type (:&), D2, D5, D6, IntN, Tuple0, Tuple1(..), UIntN)
import Network.Ethereum.Web3.Types.Types (HexString(..))
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

--------------------------------------------------------------------------------
-- | CountSet
--------------------------------------------------------------------------------

-- This is what the auto-generated code should look like

newtype CountSet = CountSet {_count :: (UIntN (D2 :& D5 :& D6))}

derive instance newtypeCountSet :: Newtype CountSet _

instance indexedEventCountSet :: IndexedEvent (Tuple0 ) (Tuple1 (Tagged (SProxy "_count") (UIntN (D2 :& D5 :& D6)))) CountSet where
  isAnonymous _ = false

derive instance genericCountSet :: Generic CountSet _

instance eventGenericCountSetShow :: Show CountSet where
        show = genericShow

instance eventGenericCountSeteq :: Eq CountSet where
        eq = genericEq

instance eventFilterCountSet :: EventFilter CountSet where
  eventFilter _  addr = defaultFilter
                         # _topics .~ Just [Just (HexString "a32bc18230dd172221ac5c4821a5f1f1a831f27b1396d244cdd891c58f132435")]
                         # _address .~ Just ssAddress

-- this is the application code

instance isAsyncHttp :: IsAsyncProvider HttpProvider where
  getAsyncProvider = liftEff <<< httpProvider $ "http://localhost:8545"

setA :: forall e . IntN (D2 :& D5 :& D6) -> Web3 HttpProvider e HexString
setA n = sendTx defaultTransactionOptions ((tagged <<< Tuple1 $ n) :: FnSet)

countMonitor :: forall e. Web3 HttpProvider (console :: CONSOLE | e) (Fiber (eth :: ETH, console :: CONSOLE | e) Unit)
countMonitor =
  let fltr = eventFilter (Proxy :: Proxy CountSet) ssAddress
                # _fromBlock .~ (BN <<< wrap <<< embed $ 10)
                # _toBlock .~ Latest
  in forkWeb3' http $ event fltr \(CountSet cs) -> do
    liftEff <<< logShow $ cs._count
    pure ContinueEvent

simpleStorageSpec :: forall r. Spec (eth :: ETH, console :: CONSOLE | r) Unit
simpleStorageSpec =
  describe "Bounded event handlers" do
    it "can print the lifespan of a filter producing machine" do
      true `shouldEqual` true

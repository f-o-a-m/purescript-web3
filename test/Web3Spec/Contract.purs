module Web3Spec.Contract  where

import Prelude

import Effect.Aff (Fiber)
import Effect.Class (liftEffect)
import Effect.Console (logShow)
import Data.Either (Either)
import Data.Functor.Tagged (Tagged, tagged)
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Eq (genericEq)
import Data.Generic.Rep.Show (genericShow)
import Data.Lens ((.~))
import Data.Maybe (Maybe(..), fromJust)
import Data.Newtype (class Newtype, wrap)
import Data.Symbol (SProxy)
import Network.Ethereum.Web3 (class EventFilter, Address, ChainCursor(..), EventAction(..), HexString, Web3, Web3Error, _address, _fromBlock, _toBlock, _topics, defaultFilter, defaultTransactionOptions, embed, event, eventFilter, forkWeb3', mkAddress, mkHexString, sendTx, Filter)
import Network.Ethereum.Web3.Solidity (class IndexedEvent, type (:%), type (:&), DOne, D2, D5, D6, IntN, Tuple0, Tuple1(..), UIntN)
import Partial.Unsafe (unsafePartial)
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldEqual)
import Type.Proxy (Proxy(..))

import Network.Ethereum.Web3.Contract (eventMulti')

ssAddress :: Address
ssAddress = unsafePartial fromJust $ mkAddress =<< mkHexString "c29313014a78b440876bac21be369c3047e313e7"

adminAddress :: Address
adminAddress = unsafePartial fromJust $ mkAddress =<< mkHexString "44cba02c089789b3299069c93832b7a8b8723b3e"

type FnSet = Tagged (SProxy "(int256)") (Tuple1 (IntN (D2 :& D5 :% D6)))

--------------------------------------------------------------------------------
-- | CountSet
--------------------------------------------------------------------------------

-- This is what the auto-generated code should look like

newtype CountSet = CountSet {_count :: (UIntN (D2 :& D5 :% D6))}

derive instance newtypeCountSet :: Newtype CountSet _

instance indexedEventCountSet :: IndexedEvent (Tuple0 ) (Tuple1 (Tagged (SProxy "_count") (UIntN (D2 :& D5 :& DOne D6)))) CountSet where
  isAnonymous _ = false

derive instance genericCountSet :: Generic CountSet _

instance eventGenericCountSetShow :: Show CountSet where
        show = genericShow

instance eventGenericCountSeteq :: Eq CountSet where
        eq = genericEq

instance eventFilterCountSet :: EventFilter CountSet where
  eventFilter _  addr = defaultFilter
                         # _topics .~ Just [Just (unsafePartial fromJust $ mkHexString "a32bc18230dd172221ac5c4821a5f1f1a831f27b1396d244cdd891c58f132435")]
                         # _address .~ Just ssAddress

--------------------------------------------------------------------------------
-- | CountSet1
--------------------------------------------------------------------------------

-- This is what the auto-generated code should look like

newtype CountSet1 = CountSet1 {_count :: (UIntN (D2 :& D5 :% D6))}

derive instance newtypeCountSet1 :: Newtype CountSet1 _

instance indexedEventCountSet1 :: IndexedEvent (Tuple0 ) (Tuple1 (Tagged (SProxy "_count") (UIntN (D2 :& D5 :& DOne D6)))) CountSet1 where
  isAnonymous _ = false

derive instance genericCountSet1 :: Generic CountSet1 _

instance eventGenericCountSet1Show :: Show CountSet1 where
        show = genericShow

instance eventGenericCountSet1eq :: Eq CountSet1 where
        eq = genericEq

instance eventFilterCountSet1 :: EventFilter CountSet1 where
  eventFilter _  addr = defaultFilter
                         # _topics .~ Just [Just (unsafePartial fromJust $ mkHexString "a32bc18230dd172221ac5c4821a5f1f1a831f27b1396d244cdd891c58f132435")]
                         # _address .~ Just ssAddress

-- this is the application code
-----------------------------------------------------
filters :: { event1 :: Filter CountSet
           , event2 :: Filter CountSet1
           }
filters = { event1:  eventFilter (Proxy :: Proxy CountSet) ssAddress
          , event2:  eventFilter (Proxy :: Proxy CountSet1) ssAddress
          }

handlers = { event1: const $ pure ContinueEvent
           , event2: const $ pure ContinueEvent
           }

process = eventMulti' filters 1 handlers


------------------------------------------------------

setA :: IntN (D2 :& D5 :% D6) -> Web3 HexString
setA n = sendTx defaultTransactionOptions ((tagged <<< Tuple1 $ n) :: FnSet)

countMonitor :: Web3 (Fiber (Either Web3Error Unit))
countMonitor =
  let fltr = eventFilter (Proxy :: Proxy CountSet) ssAddress
                # _fromBlock .~ (BN <<< wrap <<< embed $ 10)
                # _toBlock .~ Latest
  in forkWeb3' $ event fltr \(CountSet cs) -> do
    liftEffect <<< logShow $ cs._count
    pure ContinueEvent

simpleStorageSpec :: Spec Unit
simpleStorageSpec =
  describe "Bounded event handlers" do
    it "can print the lifespan of a filter producing machine" do
      true `shouldEqual` true

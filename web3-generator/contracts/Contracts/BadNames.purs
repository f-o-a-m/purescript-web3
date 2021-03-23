--------------------------------------------------------------------------------
-- | BadNames
--------------------------------------------------------------------------------

module Contracts.BadNames where

import Prelude 

import Data.Functor.Tagged (Tagged, tagged)
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Eq (genericEq)
import Data.Generic.Rep.Show (genericShow)
import Data.Lens ((.~))
import Data.Maybe (Maybe(..), fromJust)
import Data.Newtype (class Newtype)
import Data.Symbol (SProxy)
import Network.Ethereum.Web3 (_address, _topics, class EventFilter, sendTx)
import Network.Ethereum.Web3.Contract.Internal (uncurryFields)
import Network.Ethereum.Web3.Solidity (D2, D5, D6, DOne, Tuple0, Tuple1(..), UIntN, class IndexedEvent)
import Network.Ethereum.Web3.Solidity.Size (type (:&))
import Network.Ethereum.Web3.Types (HexString, NoPay, TransactionOptions, Web3, defaultFilter, mkHexString)
import Partial.Unsafe (unsafePartial)
--------------------------------------------------------------------------------
-- | EvT_BadEventName
--------------------------------------------------------------------------------


newtype EvT_BadEventName = EvT_BadEventName {n :: (UIntN (D2 :& D5 :& DOne D6))}

derive instance newtypeEvT_BadEventName :: Newtype EvT_BadEventName _

instance eventFilterEvT_BadEventName :: EventFilter EvT_BadEventName where
  eventFilter _ addr = defaultFilter
    # _address .~ Just addr
    # _topics .~ Just [Just ( unsafePartial $ fromJust $ mkHexString "0a74e27e86665d15520ffa6f5318114704bde7889bbee91711c5153b43401abd")]

instance indexedEventEvT_BadEventName :: IndexedEvent (Tuple0 ) (Tuple1 (Tagged (SProxy "n") (UIntN (D2 :& D5 :& DOne D6)))) EvT_BadEventName where
  isAnonymous _ = false

derive instance genericEvT_BadEventName :: Generic EvT_BadEventName _

instance eventGenericEvT_BadEventNameShow :: Show EvT_BadEventName where
  show = genericShow

instance eventGenericEvT_BadEventNameeq :: Eq EvT_BadEventName where
  eq = genericEq

--------------------------------------------------------------------------------
-- | FnT_dumbFunctionFn
--------------------------------------------------------------------------------


type FnT_dumbFunctionFn = Tagged (SProxy "_dumbFunction(uint256)") (Tuple1 (Tagged (SProxy "n") (UIntN (D2 :& D5 :& DOne D6))))

_dumbFunction :: TransactionOptions NoPay -> { n :: (UIntN (D2 :& D5 :& DOne D6)) } -> Web3 HexString
_dumbFunction x0 r = uncurryFields  r $ _dumbFunction' x0
   where
    _dumbFunction' :: TransactionOptions NoPay -> (Tagged (SProxy "n") (UIntN (D2 :& D5 :& DOne D6))) -> Web3 HexString
    _dumbFunction' y0 y1 = sendTx y0 ((tagged $ Tuple1 y1) :: FnT_dumbFunctionFn)
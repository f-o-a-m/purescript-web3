--------------------------------------------------------------------------------
-- | Multifilter
--------------------------------------------------------------------------------
module Web3Spec.Live.Contract.Multifilter where

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
-- | E1
--------------------------------------------------------------------------------
newtype E1
  = E1 { value1 :: (UIntN (D2 :& D5 :& DOne D6)) }

derive instance newtypeE1 :: Newtype E1 _

instance eventFilterE1 :: EventFilter E1 where
  eventFilter _ addr =
    defaultFilter
      # _address
      .~ Just addr
      # _topics
      .~ Just [ Just (unsafePartial $ fromJust $ mkHexString "47e2689743f14e97f7dcfa5eec10ba1dff02f83b3d1d4b9c07b206cbbda66450") ]

instance indexedEventE1 :: IndexedEvent (Tuple0) (Tuple1 (Tagged (SProxy "value1") (UIntN (D2 :& D5 :& DOne D6)))) E1 where
  isAnonymous _ = false

derive instance genericE1 :: Generic E1 _

instance eventGenericE1Show :: Show E1 where
  show = genericShow

instance eventGenericE1eq :: Eq E1 where
  eq = genericEq

--------------------------------------------------------------------------------
-- | E2
--------------------------------------------------------------------------------
newtype E2
  = E2 { value2 :: (UIntN (D2 :& D5 :& DOne D6)) }

derive instance newtypeE2 :: Newtype E2 _

instance eventFilterE2 :: EventFilter E2 where
  eventFilter _ addr =
    defaultFilter
      # _address
      .~ Just addr
      # _topics
      .~ Just [ Just (unsafePartial $ fromJust $ mkHexString "a48a6b249a5084126c3da369fbc9b16827ead8cb5cdc094b717d3f1dcd995e29") ]

instance indexedEventE2 :: IndexedEvent (Tuple0) (Tuple1 (Tagged (SProxy "value2") (UIntN (D2 :& D5 :& DOne D6)))) E2 where
  isAnonymous _ = false

derive instance genericE2 :: Generic E2 _

instance eventGenericE2Show :: Show E2 where
  show = genericShow

instance eventGenericE2eq :: Eq E2 where
  eq = genericEq

--------------------------------------------------------------------------------
-- | FireE1Fn
--------------------------------------------------------------------------------
type FireE1Fn
  = Tagged (SProxy "fireE1(uint256)") (Tuple1 (Tagged (SProxy "_value") (UIntN (D2 :& D5 :& DOne D6))))

fireE1 :: TransactionOptions NoPay -> { _value :: (UIntN (D2 :& D5 :& DOne D6)) } -> Web3 HexString
fireE1 x0 r = uncurryFields r $ fireE1' x0
  where
  fireE1' :: TransactionOptions NoPay -> (Tagged (SProxy "_value") (UIntN (D2 :& D5 :& DOne D6))) -> Web3 HexString
  fireE1' y0 y1 = sendTx y0 ((tagged $ Tuple1 y1) :: FireE1Fn)

--------------------------------------------------------------------------------
-- | FireE2Fn
--------------------------------------------------------------------------------
type FireE2Fn
  = Tagged (SProxy "fireE2(uint256)") (Tuple1 (Tagged (SProxy "_value") (UIntN (D2 :& D5 :& DOne D6))))

fireE2 :: TransactionOptions NoPay -> { _value :: (UIntN (D2 :& D5 :& DOne D6)) } -> Web3 HexString
fireE2 x0 r = uncurryFields r $ fireE2' x0
  where
  fireE2' :: TransactionOptions NoPay -> (Tagged (SProxy "_value") (UIntN (D2 :& D5 :& DOne D6))) -> Web3 HexString
  fireE2' y0 y1 = sendTx y0 ((tagged $ Tuple1 y1) :: FireE2Fn)

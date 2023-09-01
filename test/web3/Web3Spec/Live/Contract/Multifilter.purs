module Web3Spec.Live.Contract.Multifilter where

import Prelude

import Data.Eq.Generic (genericEq)
import Data.Functor.Tagged (Tagged, tagged)
import Data.Generic.Rep (class Generic)
import Data.Lens (set)
import Data.Maybe (Maybe(..), fromJust)
import Data.Newtype (class Newtype)
import Data.Show.Generic (genericShow)
import Network.Ethereum.Web3 (class EventFilter, _address, _topics, sendTx)
import Network.Ethereum.Web3.Contract.Internal (uncurryFields)
import Network.Ethereum.Web3.Solidity (Tuple0, UIntN, Tuple1(..), class IndexedEvent)
import Network.Ethereum.Web3.Types (HexString, NoPay, TransactionOptions, Web3, defaultFilter, mkHexString)
import Partial.Unsafe (unsafePartial)

newtype E1 = E1 { value1 :: UIntN 256 }

derive instance Newtype E1 _
derive instance Generic E1 _
instance Show E1 where
  show = genericShow

instance Eq E1 where
  eq = genericEq

instance EventFilter E1 where
  eventFilter _ addr = defaultFilter # set _address (Just addr) # set _topics
    ( Just
        [ Just $ unsafePartial $ fromJust $ mkHexString
            "47e2689743f14e97f7dcfa5eec10ba1dff02f83b3d1d4b9c07b206cbbda66450"
        , Nothing
        , Nothing
        ]
    )

instance IndexedEvent Tuple0 (Tuple1 (Tagged "value1" (UIntN 256))) E1 where
  isAnonymous _ = false

newtype E2 = E2 { value2 :: UIntN 256 }

derive instance Newtype E2 _
derive instance Generic E2 _
instance Show E2 where
  show = genericShow

instance Eq E2 where
  eq = genericEq

instance EventFilter E2 where
  eventFilter _ addr = defaultFilter # set _address (Just addr) # set _topics
    ( Just
        [ Just $ unsafePartial $ fromJust $ mkHexString
            "a48a6b249a5084126c3da369fbc9b16827ead8cb5cdc094b717d3f1dcd995e29"
        , Nothing
        , Nothing
        ]
    )

instance IndexedEvent Tuple0 (Tuple1 (Tagged "value2" (UIntN 256))) E2 where
  isAnonymous _ = false

type FireE1Fn = Tagged "fireE1(uint256)" (Tuple1 (Tagged "_value" (UIntN 256)))

fireE1 :: TransactionOptions NoPay -> { _value :: UIntN 256 } -> Web3 HexString
fireE1 x1 x2 = uncurryFields x2 $ fireE1' x1
  where
  fireE1' :: TransactionOptions NoPay -> Tagged "_value" (UIntN 256) -> Web3 HexString
  fireE1' _x1 _x2 = sendTx _x1 (tagged $ Tuple1 _x2 :: FireE1Fn)

type FireE2Fn = Tagged "fireE2(uint256)" (Tuple1 (Tagged "_value" (UIntN 256)))

fireE2 :: TransactionOptions NoPay -> { _value :: UIntN 256 } -> Web3 HexString
fireE2 x1 x2 = uncurryFields x2 $ fireE2' x1
  where
  fireE2' :: TransactionOptions NoPay -> Tagged "_value" (UIntN 256) -> Web3 HexString
  fireE2' _x1 _x2 = sendTx _x1 (tagged $ Tuple1 _x2 :: FireE2Fn)

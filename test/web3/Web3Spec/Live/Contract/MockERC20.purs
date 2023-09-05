module Web3Spec.Live.Contract.MockERC20 where

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
import Network.Ethereum.Web3.Solidity (D2, D5, D6, DOne, Tuple1, UIntN, Tuple2(..), class IndexedEvent)
import Network.Ethereum.Web3.Solidity.Size (type (:&))
import Network.Ethereum.Web3.Types (Address, HexString, NoPay, TransactionOptions, Web3, defaultFilter, mkHexString)
import Partial.Unsafe (unsafePartial)
import Type.Proxy (Proxy)

newtype Transfer = Transfer
  { to :: Address, from :: Address, amount :: UIntN (D2 :& D5 :& DOne D6) }

derive instance Newtype Transfer _
derive instance Generic Transfer _
instance Show Transfer where
  show = genericShow

instance Eq Transfer where
  eq = genericEq

instance EventFilter Transfer where
  eventFilter _ addr = defaultFilter # set _address (Just addr) # set _topics
    ( Just
        [ Just $ unsafePartial $ fromJust $ mkHexString
            "ddf252ad1be2c89b69c2b068fc378daa952ba7f163c4a11628f55a4df523b3ef"
        , Nothing
        , Nothing
        ]
    )

instance
  IndexedEvent (Tuple2 (Tagged (Proxy "to") Address) (Tagged (Proxy "from") Address))
    (Tuple1 (Tagged (Proxy "amount") (UIntN (D2 :& D5 :& DOne D6))))
    Transfer where
  isAnonymous _ = false

type TransferFn = Tagged (Proxy "transfer(address,uint256)")
  (Tuple2 (Tagged (Proxy "to") Address) (Tagged (Proxy "amount") (UIntN (D2 :& D5 :& DOne D6))))

transfer
  :: TransactionOptions NoPay
  -> { to :: Address, amount :: UIntN (D2 :& D5 :& DOne D6) }
  -> Web3 HexString
transfer x1 x2 = uncurryFields x2 $ transfer' x1
  where
  transfer'
    :: TransactionOptions NoPay
    -> Tagged (Proxy "to") Address
    -> Tagged (Proxy "amount") (UIntN (D2 :& D5 :& DOne D6))
    -> Web3 HexString
  transfer' _x1 _x2 _x3 = sendTx _x1 (tagged $ Tuple2 _x2 _x3 :: TransferFn)

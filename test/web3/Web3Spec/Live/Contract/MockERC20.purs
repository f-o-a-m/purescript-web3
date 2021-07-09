--------------------------------------------------------------------------------
-- | MockERC20
--------------------------------------------------------------------------------
module Web3Spec.Live.Contract.MockERC20 where

import Prelude
import Data.Functor.Tagged (Tagged, tagged)
import Data.Generic.Rep (class Generic)
import Data.Eq.Generic (genericEq)
import Data.Show.Generic (genericShow)
import Data.Lens ((.~))
import Data.Maybe (Maybe(..), fromJust)
import Data.Newtype (class Newtype)
import Data.Symbol (SProxy)
import Network.Ethereum.Web3 (_address, _topics, class EventFilter, sendTx)
import Network.Ethereum.Web3.Contract.Internal (uncurryFields)
import Network.Ethereum.Web3.Solidity (D2, D5, D6, DOne, Tuple1, Tuple2(..), UIntN, class IndexedEvent)
import Network.Ethereum.Web3.Solidity.Size (type (:&))
import Network.Ethereum.Web3.Types (Address, HexString, NoPay, TransactionOptions, Web3, defaultFilter, mkHexString)
import Partial.Unsafe (unsafePartial)

--------------------------------------------------------------------------------
-- | Transfer
--------------------------------------------------------------------------------
newtype Transfer
  = Transfer { to :: Address, from :: Address, amount :: (UIntN (D2 :& D5 :& DOne D6)) }

derive instance newtypeTransfer :: Newtype Transfer _

instance eventFilterTransfer :: EventFilter Transfer where
  eventFilter _ addr =
    defaultFilter
      # _address
      .~ Just addr
      # _topics
      .~ Just [ Just (unsafePartial $ fromJust $ mkHexString "ddf252ad1be2c89b69c2b068fc378daa952ba7f163c4a11628f55a4df523b3ef"), Nothing, Nothing ]

instance indexedEventTransfer :: IndexedEvent (Tuple2 (Tagged (SProxy "to") Address) (Tagged (SProxy "from") Address)) (Tuple1 (Tagged (SProxy "amount") (UIntN (D2 :& D5 :& DOne D6)))) Transfer where
  isAnonymous _ = false

derive instance genericTransfer :: Generic Transfer _

instance eventGenericTransferShow :: Show Transfer where
  show = genericShow

instance eventGenericTransfereq :: Eq Transfer where
  eq = genericEq

--------------------------------------------------------------------------------
-- | TransferFn
--------------------------------------------------------------------------------
type TransferFn
  = Tagged (SProxy "transfer(address,uint256)") (Tuple2 (Tagged (SProxy "to") Address) (Tagged (SProxy "amount") (UIntN (D2 :& D5 :& DOne D6))))

transfer :: TransactionOptions NoPay -> { to :: Address, amount :: (UIntN (D2 :& D5 :& DOne D6)) } -> Web3 HexString
transfer x0 r = uncurryFields r $ transfer' x0
  where
  transfer' :: TransactionOptions NoPay -> (Tagged (SProxy "to") Address) -> (Tagged (SProxy "amount") (UIntN (D2 :& D5 :& DOne D6))) -> Web3 HexString
  transfer' y0 y1 y2 = sendTx y0 ((tagged $ Tuple2 y1 y2) :: TransferFn)

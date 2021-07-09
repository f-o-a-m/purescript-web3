--------------------------------------------------------------------------------
-- | PayableTest
--------------------------------------------------------------------------------
module Web3Spec.Live.Contract.PayableTest where

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
import Network.Ethereum.Web3.Solidity (D2, D5, D6, DOne, Tuple0(..), Tuple1, UIntN, class IndexedEvent)
import Network.Ethereum.Web3.Solidity.Size (type (:&))
import Network.Ethereum.Web3.Types (HexString, TransactionOptions, Web3, defaultFilter, mkHexString)
import Network.Ethereum.Web3.Types.TokenUnit (MinorUnit)
import Partial.Unsafe (unsafePartial)

--------------------------------------------------------------------------------
-- | Content
--------------------------------------------------------------------------------
newtype Content
  = Content { _paidContent :: (UIntN (D2 :& D5 :& DOne D6)) }

derive instance newtypeContent :: Newtype Content _

instance eventFilterContent :: EventFilter Content where
  eventFilter _ addr =
    defaultFilter
      # _address
      .~ Just addr
      # _topics
      .~ Just [ Just (unsafePartial $ fromJust $ mkHexString "78692973dbc680e9276487808ebf9b485db7b4fbb74c05799e397695b5c7686b") ]

instance indexedEventContent :: IndexedEvent (Tuple0) (Tuple1 (Tagged (SProxy "_paidContent") (UIntN (D2 :& D5 :& DOne D6)))) Content where
  isAnonymous _ = false

derive instance genericContent :: Generic Content _

instance eventGenericContentShow :: Show Content where
  show = genericShow

instance eventGenericContenteq :: Eq Content where
  eq = genericEq

--------------------------------------------------------------------------------
-- | SeeContentFn
--------------------------------------------------------------------------------
type SeeContentFn
  = Tagged (SProxy "seeContent()") (Tuple0)

seeContent :: TransactionOptions MinorUnit -> Web3 HexString
seeContent x0 = sendTx x0 ((tagged $ Tuple0) :: SeeContentFn)

module Web3Spec.Live.Contract.PayableTest where

import Prelude

import Data.Eq.Generic (genericEq)
import Data.Functor.Tagged (Tagged, tagged)
import Data.Generic.Rep (class Generic)
import Data.Lens (set)
import Data.Maybe (Maybe(..), fromJust)
import Data.Newtype (class Newtype)
import Data.Show.Generic (genericShow)
import Network.Ethereum.Web3 (class EventFilter, _address, _topics, sendTx)
import Network.Ethereum.Web3.Solidity (D2, D5, D6, DOne, Tuple1, UIntN, Tuple0(..), class IndexedEvent)
import Network.Ethereum.Web3.Solidity.Size (type (:&))
import Network.Ethereum.Web3.Types (HexString, TransactionOptions, Web3, defaultFilter, mkHexString)
import Network.Ethereum.Web3.Types.TokenUnit (MinorUnit)
import Partial.Unsafe (unsafePartial)
import Type.Proxy (Proxy)

newtype Content = Content { _paidContent :: UIntN (D2 :& D5 :& DOne D6) }

derive instance Newtype Content _
derive instance Generic Content _
instance Show Content where
  show = genericShow

instance Eq Content where
  eq = genericEq

instance EventFilter Content where
  eventFilter _ addr = defaultFilter # set _address (Just addr) # set _topics
    ( Just
        [ Just $ unsafePartial $ fromJust $ mkHexString
            "78692973dbc680e9276487808ebf9b485db7b4fbb74c05799e397695b5c7686b"
        ]
    )

instance
  IndexedEvent Tuple0 (Tuple1 (Tagged (Proxy "_paidContent") (UIntN (D2 :& D5 :& DOne D6)))) Content where
  isAnonymous _ = false

type SeeContentFn = Tagged (Proxy "seeContent()") Tuple0

seeContent :: TransactionOptions MinorUnit -> Web3 HexString
seeContent x1 = sendTx x1 (tagged Tuple0 :: SeeContentFn)

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
import Network.Ethereum.Web3.Solidity (Tuple1, UIntN, Tuple0(..), class IndexedEvent)
import Network.Ethereum.Web3.Types (HexString, TransactionOptions, Web3, defaultFilter, mkHexString)
import Network.Ethereum.Web3.Types.TokenUnit (MinorUnit)
import Partial.Unsafe (unsafePartial)

newtype Content = Content { _paidContent :: UIntN 256 }

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
        , Nothing
        , Nothing
        ]
    )

instance IndexedEvent Tuple0 (Tuple1 (Tagged "_paidContent" (UIntN 256))) Content where
  isAnonymous _ = false

type SeeContentFn = Tagged "seeContent()" Tuple0

seeContent :: TransactionOptions MinorUnit -> Web3 HexString
seeContent x1 = sendTx x1 (tagged Tuple0 :: SeeContentFn)

module Web3Spec.Live.Contract.SimpleStorage where

import Prelude

import Data.Either (Either)
import Data.Eq.Generic (genericEq)
import Data.Functor.Tagged (Tagged, tagged)
import Data.Generic.Rep (class Generic)
import Data.Lens (set)
import Data.Maybe (Maybe(..), fromJust)
import Data.Newtype (class Newtype)
import Data.Show.Generic (genericShow)
import Network.Ethereum.Web3 (class EventFilter, _address, _topics, call, deployContract, sendTx)
import Network.Ethereum.Web3.Contract.Internal (uncurryFields)
import Network.Ethereum.Web3.Solidity (D2, D5, D6, DOne, UIntN, Tuple0(..), Tuple1(..), class IndexedEvent, unTuple1)
import Network.Ethereum.Web3.Solidity.Size (type (:&))
import Network.Ethereum.Web3.Types (CallError, ChainCursor, HexString, NoPay, TransactionOptions, Web3, defaultFilter, mkHexString)
import Partial.Unsafe (unsafePartial)
import Type.Proxy (Proxy)

type ConstructorFn = Tagged (Proxy Void) Tuple0

constructor :: TransactionOptions NoPay -> HexString -> Web3 HexString
constructor x1 x2 = deployContract x1 x2 (tagged Tuple0 :: ConstructorFn)

newtype CountSet = CountSet { _count :: UIntN (D2 :& D5 :& DOne D6) }

derive instance Newtype CountSet _
derive instance Generic CountSet _
instance Show CountSet where
  show = genericShow

instance Eq CountSet where
  eq = genericEq

instance EventFilter CountSet where
  eventFilter _ addr = defaultFilter # set _address (Just addr) # set _topics
    ( Just
        [ Just $ unsafePartial $ fromJust $ mkHexString
            "a32bc18230dd172221ac5c4821a5f1f1a831f27b1396d244cdd891c58f132435"
        ]
    )

instance
  IndexedEvent Tuple0 (Tuple1 (Tagged (Proxy "_count") (UIntN (D2 :& D5 :& DOne D6)))) CountSet where
  isAnonymous _ = false

newtype Deployed = Deployed { _blockNumber :: UIntN (D2 :& D5 :& DOne D6) }

derive instance Newtype Deployed _
derive instance Generic Deployed _
instance Show Deployed where
  show = genericShow

instance Eq Deployed where
  eq = genericEq

instance EventFilter Deployed where
  eventFilter _ addr = defaultFilter # set _address (Just addr) # set _topics
    ( Just
        [ Just $ unsafePartial $ fromJust $ mkHexString
            "b94ae47ec9f4248692e2ecf9740b67ab493f3dcc8452bedc7d9cd911c28d1ca5"
        ]
    )

instance
  IndexedEvent Tuple0
    (Tuple1 (Tagged (Proxy "_blockNumber") (UIntN (D2 :& D5 :& DOne D6))))
    Deployed where
  isAnonymous _ = false

type CountFn = Tagged (Proxy "count()") Tuple0

count
  :: TransactionOptions NoPay
  -> ChainCursor
  -> Web3 (Either CallError (UIntN (D2 :& D5 :& DOne D6)))
count x1 x2 = map unTuple1 <$> call x1 x2 (tagged Tuple0 :: CountFn)

type SetCountFn = Tagged (Proxy "setCount(uint256)")
  (Tuple1 (Tagged (Proxy "_count") (UIntN (D2 :& D5 :& DOne D6))))

setCount :: TransactionOptions NoPay -> { _count :: UIntN (D2 :& D5 :& DOne D6) } -> Web3 HexString
setCount x1 x2 = uncurryFields x2 $ setCount' x1
  where
  setCount'
    :: TransactionOptions NoPay
    -> Tagged (Proxy "_count") (UIntN (D2 :& D5 :& DOne D6))
    -> Web3 HexString
  setCount' _x1 _x2 = sendTx _x1 (tagged $ Tuple1 _x2 :: SetCountFn)

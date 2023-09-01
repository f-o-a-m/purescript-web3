module Web3Spec.Live.Contract.ComplexStorage where

import Prelude

import Data.Either (Either)
import Data.Eq.Generic (genericEq)
import Data.Functor.Tagged (Tagged, tagged)
import Data.Generic.Rep (class Generic)
import Data.Lens (set)
import Data.Maybe (Maybe(..), fromJust)
import Data.Newtype (class Newtype)
import Data.Show.Generic (genericShow)
import Network.Ethereum.Web3 (Vector, class EventFilter, _address, _topics, call, sendTx)
import Network.Ethereum.Web3.Contract.Internal (uncurryFields)
import Network.Ethereum.Web3.Solidity (BytesN, IntN, UIntN, Tuple0(..), Tuple1(..), Tuple2(..), Tuple9(..), class IndexedEvent, unTuple1)
import Network.Ethereum.Web3.Types (CallError, ChainCursor, HexString, NoPay, TransactionOptions, Web3, defaultFilter, mkHexString)
import Partial.Unsafe (unsafePartial)

newtype ValsSet = ValsSet
  { a :: UIntN 256
  , b :: IntN 256
  , c :: Boolean
  , d :: IntN 224
  , e :: Vector 2 Boolean
  , f :: Array (IntN 256)
  , g :: String
  , h :: BytesN 16
  , i :: Array (Vector 4 (BytesN 2))
  }

derive instance Newtype ValsSet _
derive instance Generic ValsSet _
instance Show ValsSet where
  show = genericShow

instance Eq ValsSet where
  eq = genericEq

instance EventFilter ValsSet where
  eventFilter _ addr = defaultFilter # set _address (Just addr) # set _topics
    ( Just
        [ Just $ unsafePartial $ fromJust $ mkHexString
            "88d23351ad32a937b11ca10530404f8297d29803e94709336b48c1f82c15b3cc"
        , Nothing
        , Nothing
        ]
    )

instance
  IndexedEvent Tuple0
    ( Tuple9 (Tagged "a" (UIntN 256)) (Tagged "b" (IntN 256)) (Tagged "c" Boolean)
        (Tagged "d" (IntN 224))
        (Tagged "e" (Vector 2 Boolean))
        (Tagged "f" (Array (IntN 256)))
        (Tagged "g" String)
        (Tagged "h" (BytesN 16))
        (Tagged "i" (Array (Vector 4 (BytesN 2))))
    )
    ValsSet where
  isAnonymous _ = false

type BoolValFn = Tagged "boolVal()" Tuple0

boolVal :: TransactionOptions NoPay -> ChainCursor -> Web3 (Either CallError Boolean)
boolVal x1 x2 = map unTuple1 <$> call x1 x2 (tagged Tuple0 :: BoolValFn)

type BoolVectorValFn = Tagged "boolVectorVal(uint256)" (Tuple1 (UIntN 256))

boolVectorVal
  :: TransactionOptions NoPay -> ChainCursor -> UIntN 256 -> Web3 (Either CallError Boolean)
boolVectorVal x1 x2 x3 = boolVectorVal' x1 x2 x3
  where
  boolVectorVal'
    :: TransactionOptions NoPay -> ChainCursor -> UIntN 256 -> Web3 (Either CallError Boolean)
  boolVectorVal' _x1 _x2 _x3 = map unTuple1 <$> call _x1 _x2
    (tagged $ Tuple1 _x3 :: BoolVectorValFn)

type Bytes16ValFn = Tagged "bytes16Val()" Tuple0

bytes16Val :: TransactionOptions NoPay -> ChainCursor -> Web3 (Either CallError (BytesN 16))
bytes16Val x1 x2 = map unTuple1 <$> call x1 x2 (tagged Tuple0 :: Bytes16ValFn)

type Bytes2VectorListValFn = Tagged "bytes2VectorListVal(uint256,uint256)"
  (Tuple2 (UIntN 256) (UIntN 256))

bytes2VectorListVal
  :: TransactionOptions NoPay
  -> ChainCursor
  -> UIntN 256
  -> UIntN 256
  -> Web3 (Either CallError (BytesN 2))
bytes2VectorListVal x1 x2 x3 x4 = bytes2VectorListVal' x1 x2 x3 x4
  where
  bytes2VectorListVal'
    :: TransactionOptions NoPay
    -> ChainCursor
    -> UIntN 256
    -> UIntN 256
    -> Web3 (Either CallError (BytesN 2))
  bytes2VectorListVal' _x1 _x2 _x3 _x4 = map unTuple1 <$> call _x1 _x2
    (tagged $ Tuple2 _x3 _x4 :: Bytes2VectorListValFn)

type Int224ValFn = Tagged "int224Val()" Tuple0

int224Val :: TransactionOptions NoPay -> ChainCursor -> Web3 (Either CallError (IntN 224))
int224Val x1 x2 = map unTuple1 <$> call x1 x2 (tagged Tuple0 :: Int224ValFn)

type IntListValFn = Tagged "intListVal(uint256)" (Tuple1 (UIntN 256))

intListVal
  :: TransactionOptions NoPay -> ChainCursor -> UIntN 256 -> Web3 (Either CallError (IntN 256))
intListVal x1 x2 x3 = intListVal' x1 x2 x3
  where
  intListVal'
    :: TransactionOptions NoPay -> ChainCursor -> UIntN 256 -> Web3 (Either CallError (IntN 256))
  intListVal' _x1 _x2 _x3 = map unTuple1 <$> call _x1 _x2 (tagged $ Tuple1 _x3 :: IntListValFn)

type IntValFn = Tagged "intVal()" Tuple0

intVal :: TransactionOptions NoPay -> ChainCursor -> Web3 (Either CallError (IntN 256))
intVal x1 x2 = map unTuple1 <$> call x1 x2 (tagged Tuple0 :: IntValFn)

type SetValuesFn = Tagged
  "setValues(uint256,int256,bool,int224,bool[2],int256[],string,bytes16,bytes2[4][])"
  ( Tuple9 (Tagged "_uintVal" (UIntN 256)) (Tagged "_intVal" (IntN 256)) (Tagged "_boolVal" Boolean)
      (Tagged "_int224Val" (IntN 224))
      (Tagged "_boolVectorVal" (Vector 2 Boolean))
      (Tagged "_intListVal" (Array (IntN 256)))
      (Tagged "_stringVal" String)
      (Tagged "_bytes16Val" (BytesN 16))
      (Tagged "_bytes2VectorListVal" (Array (Vector 4 (BytesN 2))))
  )

setValues
  :: TransactionOptions NoPay
  -> { _uintVal :: UIntN 256
     , _intVal :: IntN 256
     , _boolVal :: Boolean
     , _int224Val :: IntN 224
     , _boolVectorVal :: Vector 2 Boolean
     , _intListVal :: Array (IntN 256)
     , _stringVal :: String
     , _bytes16Val :: BytesN 16
     , _bytes2VectorListVal :: Array (Vector 4 (BytesN 2))
     }
  -> Web3 HexString
setValues x1 x2 = uncurryFields x2 $ setValues' x1
  where
  setValues'
    :: TransactionOptions NoPay
    -> Tagged "_uintVal" (UIntN 256)
    -> Tagged "_intVal" (IntN 256)
    -> Tagged "_boolVal" Boolean
    -> Tagged "_int224Val" (IntN 224)
    -> Tagged "_boolVectorVal" (Vector 2 Boolean)
    -> Tagged "_intListVal" (Array (IntN 256))
    -> Tagged "_stringVal" String
    -> Tagged "_bytes16Val" (BytesN 16)
    -> Tagged "_bytes2VectorListVal" (Array (Vector 4 (BytesN 2)))
    -> Web3 HexString
  setValues' _x1 _x2 _x3 _x4 _x5 _x6 _x7 _x8 _x9 _x10 = sendTx _x1
    (tagged $ Tuple9 _x2 _x3 _x4 _x5 _x6 _x7 _x8 _x9 _x10 :: SetValuesFn)

type StringValFn = Tagged "stringVal()" Tuple0

stringVal :: TransactionOptions NoPay -> ChainCursor -> Web3 (Either CallError String)
stringVal x1 x2 = map unTuple1 <$> call x1 x2 (tagged Tuple0 :: StringValFn)

type UintValFn = Tagged "uintVal()" Tuple0

uintVal :: TransactionOptions NoPay -> ChainCursor -> Web3 (Either CallError (UIntN 256))
uintVal x1 x2 = map unTuple1 <$> call x1 x2 (tagged Tuple0 :: UintValFn)

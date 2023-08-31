--------------------------------------------------------------------------------
-- | ComplexStorage
--------------------------------------------------------------------------------
module Web3Spec.Live.Contract.ComplexStorage where

import Prelude
import Data.Either (Either)
import Data.Eq.Generic (genericEq)
import Data.Functor.Tagged (Tagged, tagged)
import Data.Generic.Rep (class Generic)
import Data.Lens ((.~))
import Data.Maybe (Maybe(..), fromJust)
import Data.Newtype (class Newtype)
import Data.Show.Generic (genericShow)
import Network.Ethereum.Web3 (Vector, _address, _topics, call, class EventFilter, sendTx)
import Network.Ethereum.Web3.Contract.Internal (uncurryFields)
import Network.Ethereum.Web3.Solidity (BytesN, IntN, Tuple0(..), Tuple1(..), Tuple2(..), Tuple9(..), UIntN, class IndexedEvent, unTuple1)
import Network.Ethereum.Web3.Types (CallError, ChainCursor, HexString, NoPay, TransactionOptions, Web3, defaultFilter, mkHexString)
import Partial.Unsafe (unsafePartial)
import Type.Proxy (Proxy)

--------------------------------------------------------------------------------
-- | ValsSet
--------------------------------------------------------------------------------
newtype ValsSet
  = ValsSet { a :: (UIntN 256), b :: (IntN 256), c :: Boolean, d :: (IntN 224), e :: (Vector 2 Boolean), f :: (Array (IntN 256)), g :: String, h :: (BytesN 16), i :: (Array (Vector 4 (BytesN 2))) }

derive instance newtypeValsSet :: Newtype ValsSet _

instance eventFilterValsSet :: EventFilter ValsSet where
  eventFilter _ addr =
    defaultFilter
      # _address
      .~ Just addr
      # _topics
      .~ Just [ Just (unsafePartial $ fromJust $ mkHexString "88d23351ad32a937b11ca10530404f8297d29803e94709336b48c1f82c15b3cc") ]

instance indexedEventValsSet :: IndexedEvent (Tuple0) (Tuple9 (Tagged (Proxy "a") (UIntN 256)) (Tagged (Proxy "b") (IntN 256)) (Tagged (Proxy "c") Boolean) (Tagged (Proxy "d") (IntN 224)) (Tagged (Proxy "e") (Vector 2 Boolean)) (Tagged (Proxy "f") (Array (IntN 256))) (Tagged (Proxy "g") String) (Tagged (Proxy "h") (BytesN 16)) (Tagged (Proxy "i") (Array (Vector 4 (BytesN 2))))) ValsSet where
  isAnonymous _ = false

derive instance genericValsSet :: Generic ValsSet _

instance eventGenericValsSetShow :: Show ValsSet where
  show = genericShow

instance eventGenericValsSeteq :: Eq ValsSet where
  eq = genericEq

--------------------------------------------------------------------------------
-- | BoolValFn
--------------------------------------------------------------------------------
type BoolValFn
  = Tagged (Proxy "boolVal()") (Tuple0)

boolVal :: TransactionOptions NoPay -> ChainCursor -> Web3 (Either CallError Boolean)
boolVal x0 cm = map unTuple1 <$> call x0 cm ((tagged $ Tuple0) :: BoolValFn)

--------------------------------------------------------------------------------
-- | BoolVectorValFn
--------------------------------------------------------------------------------
type BoolVectorValFn
  = Tagged (Proxy "boolVectorVal(uint256)") (Tuple1 (UIntN 256))

boolVectorVal :: TransactionOptions NoPay -> ChainCursor -> (UIntN 256) -> Web3 (Either CallError Boolean)
boolVectorVal x0 cm x2 = map unTuple1 <$> call x0 cm ((tagged $ Tuple1 x2) :: BoolVectorValFn)

--------------------------------------------------------------------------------
-- | Bytes16ValFn
--------------------------------------------------------------------------------
type Bytes16ValFn
  = Tagged (Proxy "bytes16Val()") (Tuple0)

bytes16Val :: TransactionOptions NoPay -> ChainCursor -> Web3 (Either CallError (BytesN 16))
bytes16Val x0 cm = map unTuple1 <$> call x0 cm ((tagged $ Tuple0) :: Bytes16ValFn)

--------------------------------------------------------------------------------
-- | Bytes2VectorListValFn
--------------------------------------------------------------------------------
type Bytes2VectorListValFn
  = Tagged (Proxy "bytes2VectorListVal(uint256,uint256)") (Tuple2 (UIntN 256) (UIntN 256))

bytes2VectorListVal :: TransactionOptions NoPay -> ChainCursor -> (UIntN 256) -> (UIntN 256) -> Web3 (Either CallError (BytesN 2))
bytes2VectorListVal x0 cm x2 x3 = map unTuple1 <$> call x0 cm ((tagged $ Tuple2 x2 x3) :: Bytes2VectorListValFn)

--------------------------------------------------------------------------------
-- | Int224ValFn
--------------------------------------------------------------------------------
type Int224ValFn
  = Tagged (Proxy "int224Val()") (Tuple0)

int224Val :: TransactionOptions NoPay -> ChainCursor -> Web3 (Either CallError (IntN 224))
int224Val x0 cm = map unTuple1 <$> call x0 cm ((tagged $ Tuple0) :: Int224ValFn)

--------------------------------------------------------------------------------
-- | IntListValFn
--------------------------------------------------------------------------------
type IntListValFn
  = Tagged (Proxy "intListVal(uint256)") (Tuple1 (UIntN 256))

intListVal :: TransactionOptions NoPay -> ChainCursor -> (UIntN 256) -> Web3 (Either CallError (IntN 256))
intListVal x0 cm x2 = map unTuple1 <$> call x0 cm ((tagged $ Tuple1 x2) :: IntListValFn)

--------------------------------------------------------------------------------
-- | IntValFn
--------------------------------------------------------------------------------
type IntValFn
  = Tagged (Proxy "intVal()") (Tuple0)

intVal :: TransactionOptions NoPay -> ChainCursor -> Web3 (Either CallError (IntN 256))
intVal x0 cm = map unTuple1 <$> call x0 cm ((tagged $ Tuple0) :: IntValFn)

--------------------------------------------------------------------------------
-- | SetValuesFn
--------------------------------------------------------------------------------
type SetValuesFn
  = Tagged (Proxy "setValues(uint256,int256,bool,int224,bool[2],int256[],string,bytes16,bytes2[4][])") (Tuple9 (Tagged (Proxy "_uintVal") (UIntN 256)) (Tagged (Proxy "_intVal") (IntN 256)) (Tagged (Proxy "_boolVal") Boolean) (Tagged (Proxy "_int224Val") (IntN 224)) (Tagged (Proxy "_boolVectorVal") (Vector 2 Boolean)) (Tagged (Proxy "_intListVal") (Array (IntN 256))) (Tagged (Proxy "_stringVal") String) (Tagged (Proxy "_bytes16Val") (BytesN 16)) (Tagged (Proxy "_bytes2VectorListVal") (Array (Vector 4 (BytesN 2)))))

setValues :: TransactionOptions NoPay -> { _uintVal :: (UIntN 256), _intVal :: (IntN 256), _boolVal :: Boolean, _int224Val :: (IntN 224), _boolVectorVal :: (Vector 2 Boolean), _intListVal :: (Array (IntN 256)), _stringVal :: String, _bytes16Val :: (BytesN 16), _bytes2VectorListVal :: (Array (Vector 4 (BytesN 2))) } -> Web3 HexString
setValues x0 r = uncurryFields r $ setValues' x0
  where
  setValues' :: TransactionOptions NoPay -> (Tagged (Proxy "_uintVal") (UIntN 256)) -> (Tagged (Proxy "_intVal") (IntN 256)) -> (Tagged (Proxy "_boolVal") Boolean) -> (Tagged (Proxy "_int224Val") (IntN 224)) -> (Tagged (Proxy "_boolVectorVal") (Vector 2 Boolean)) -> (Tagged (Proxy "_intListVal") (Array (IntN 256))) -> (Tagged (Proxy "_stringVal") String) -> (Tagged (Proxy "_bytes16Val") (BytesN 16)) -> (Tagged (Proxy "_bytes2VectorListVal") (Array (Vector 4 (BytesN 2)))) -> Web3 HexString
  setValues' y0 y1 y2 y3 y4 y5 y6 y7 y8 y9 = sendTx y0 ((tagged $ Tuple9 y1 y2 y3 y4 y5 y6 y7 y8 y9) :: SetValuesFn)

--------------------------------------------------------------------------------
-- | StringValFn
--------------------------------------------------------------------------------
type StringValFn
  = Tagged (Proxy "stringVal()") (Tuple0)

stringVal :: TransactionOptions NoPay -> ChainCursor -> Web3 (Either CallError String)
stringVal x0 cm = map unTuple1 <$> call x0 cm ((tagged $ Tuple0) :: StringValFn)

--------------------------------------------------------------------------------
-- | UintValFn
--------------------------------------------------------------------------------
type UintValFn
  = Tagged (Proxy "uintVal()") (Tuple0)

uintVal :: TransactionOptions NoPay -> ChainCursor -> Web3 (Either CallError (UIntN 256))
uintVal x0 cm = map unTuple1 <$> call x0 cm ((tagged $ Tuple0) :: UintValFn)

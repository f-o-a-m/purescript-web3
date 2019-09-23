--------------------------------------------------------------------------------
-- | ComplexStorage
--------------------------------------------------------------------------------

module Web3Spec.Live.Contract.ComplexStorage where

import Prelude 

import Data.Either (Either)
import Data.Functor.Tagged (Tagged, tagged)
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Eq (genericEq)
import Data.Generic.Rep.Show (genericShow)
import Data.Lens ((.~))
import Data.Maybe (Maybe(..), fromJust)
import Data.Newtype (class Newtype)
import Data.Symbol (SProxy)
import Network.Ethereum.Web3 (Vector, _address, _topics, call, class EventFilter, sendTx)
import Network.Ethereum.Web3.Contract.Internal (uncurryFields)
import Network.Ethereum.Web3.Solidity (BytesN, D1, D2, D4, D5, D6, DOne, IntN, Tuple0(..), Tuple1(..), Tuple2(..), Tuple9(..), UIntN, class IndexedEvent, unTuple1)
import Network.Ethereum.Web3.Solidity.Size (type (:&))
import Network.Ethereum.Web3.Types (CallError, ChainCursor, HexString, NoPay, TransactionOptions, Web3, defaultFilter, mkHexString)
import Partial.Unsafe (unsafePartial)
--------------------------------------------------------------------------------
-- | ValsSet
--------------------------------------------------------------------------------


newtype ValsSet = ValsSet {a :: (UIntN (D2 :& D5 :& DOne D6)),b :: (IntN (D2 :& D5 :& DOne D6)),c :: Boolean,d :: (IntN (D2 :& D2 :& DOne D4)),e :: (Vector (DOne D2) Boolean),f :: (Array (IntN (D2 :& D5 :& DOne D6))),g :: String,h :: (BytesN (D1 :& DOne D6)),i :: (Array (Vector (DOne D4) (BytesN (DOne D2))))}

derive instance newtypeValsSet :: Newtype ValsSet _

instance eventFilterValsSet :: EventFilter ValsSet where
	eventFilter _ addr = defaultFilter
		# _address .~ Just addr
		# _topics .~ Just [Just ( unsafePartial $ fromJust $ mkHexString "88d23351ad32a937b11ca10530404f8297d29803e94709336b48c1f82c15b3cc")]

instance indexedEventValsSet :: IndexedEvent (Tuple0 ) (Tuple9 (Tagged (SProxy "a") (UIntN (D2 :& D5 :& DOne D6))) (Tagged (SProxy "b") (IntN (D2 :& D5 :& DOne D6))) (Tagged (SProxy "c") Boolean) (Tagged (SProxy "d") (IntN (D2 :& D2 :& DOne D4))) (Tagged (SProxy "e") (Vector (DOne D2) Boolean)) (Tagged (SProxy "f") (Array (IntN (D2 :& D5 :& DOne D6)))) (Tagged (SProxy "g") String) (Tagged (SProxy "h") (BytesN (D1 :& DOne D6))) (Tagged (SProxy "i") (Array (Vector (DOne D4) (BytesN (DOne D2)))))) ValsSet where
  isAnonymous _ = false

derive instance genericValsSet :: Generic ValsSet _

instance eventGenericValsSetShow :: Show ValsSet where
	show = genericShow

instance eventGenericValsSeteq :: Eq ValsSet where
	eq = genericEq

--------------------------------------------------------------------------------
-- | BoolValFn
--------------------------------------------------------------------------------


type BoolValFn = Tagged (SProxy "boolVal()") (Tuple0 )

boolVal :: TransactionOptions NoPay -> ChainCursor -> Web3 (Either CallError Boolean)
boolVal x0 cm = map unTuple1 <$> call x0 cm ((tagged $ Tuple0 ) :: BoolValFn)

--------------------------------------------------------------------------------
-- | BoolVectorValFn
--------------------------------------------------------------------------------


type BoolVectorValFn = Tagged (SProxy "boolVectorVal(uint256)") (Tuple1 (UIntN (D2 :& D5 :& DOne D6)))

boolVectorVal :: TransactionOptions NoPay -> ChainCursor -> (UIntN (D2 :& D5 :& DOne D6)) -> Web3 (Either CallError Boolean)
boolVectorVal x0 cm x2 = map unTuple1 <$> call x0 cm ((tagged $ Tuple1 x2) :: BoolVectorValFn)

--------------------------------------------------------------------------------
-- | Bytes16ValFn
--------------------------------------------------------------------------------


type Bytes16ValFn = Tagged (SProxy "bytes16Val()") (Tuple0 )

bytes16Val :: TransactionOptions NoPay -> ChainCursor -> Web3 (Either CallError (BytesN (D1 :& DOne D6)))
bytes16Val x0 cm = map unTuple1 <$> call x0 cm ((tagged $ Tuple0 ) :: Bytes16ValFn)

--------------------------------------------------------------------------------
-- | Bytes2VectorListValFn
--------------------------------------------------------------------------------


type Bytes2VectorListValFn = Tagged (SProxy "bytes2VectorListVal(uint256,uint256)") (Tuple2 (UIntN (D2 :& D5 :& DOne D6)) (UIntN (D2 :& D5 :& DOne D6)))

bytes2VectorListVal :: TransactionOptions NoPay -> ChainCursor -> (UIntN (D2 :& D5 :& DOne D6)) -> (UIntN (D2 :& D5 :& DOne D6)) -> Web3 (Either CallError (BytesN (DOne D2)))
bytes2VectorListVal x0 cm x2 x3 = map unTuple1 <$> call x0 cm ((tagged $ Tuple2 x2 x3) :: Bytes2VectorListValFn)

--------------------------------------------------------------------------------
-- | Int224ValFn
--------------------------------------------------------------------------------


type Int224ValFn = Tagged (SProxy "int224Val()") (Tuple0 )

int224Val :: TransactionOptions NoPay -> ChainCursor -> Web3 (Either CallError (IntN (D2 :& D2 :& DOne D4)))
int224Val x0 cm = map unTuple1 <$> call x0 cm ((tagged $ Tuple0 ) :: Int224ValFn)

--------------------------------------------------------------------------------
-- | IntListValFn
--------------------------------------------------------------------------------


type IntListValFn = Tagged (SProxy "intListVal(uint256)") (Tuple1 (UIntN (D2 :& D5 :& DOne D6)))

intListVal :: TransactionOptions NoPay -> ChainCursor -> (UIntN (D2 :& D5 :& DOne D6)) -> Web3 (Either CallError (IntN (D2 :& D5 :& DOne D6)))
intListVal x0 cm x2 = map unTuple1 <$> call x0 cm ((tagged $ Tuple1 x2) :: IntListValFn)

--------------------------------------------------------------------------------
-- | IntValFn
--------------------------------------------------------------------------------


type IntValFn = Tagged (SProxy "intVal()") (Tuple0 )

intVal :: TransactionOptions NoPay -> ChainCursor -> Web3 (Either CallError (IntN (D2 :& D5 :& DOne D6)))
intVal x0 cm = map unTuple1 <$> call x0 cm ((tagged $ Tuple0 ) :: IntValFn)

--------------------------------------------------------------------------------
-- | SetValuesFn
--------------------------------------------------------------------------------


type SetValuesFn = Tagged (SProxy "setValues(uint256,int256,bool,int224,bool[2],int256[],string,bytes16,bytes2[4][])") (Tuple9 (Tagged (SProxy "_uintVal") (UIntN (D2 :& D5 :& DOne D6))) (Tagged (SProxy "_intVal") (IntN (D2 :& D5 :& DOne D6))) (Tagged (SProxy "_boolVal") Boolean) (Tagged (SProxy "_int224Val") (IntN (D2 :& D2 :& DOne D4))) (Tagged (SProxy "_boolVectorVal") (Vector (DOne D2) Boolean)) (Tagged (SProxy "_intListVal") (Array (IntN (D2 :& D5 :& DOne D6)))) (Tagged (SProxy "_stringVal") String) (Tagged (SProxy "_bytes16Val") (BytesN (D1 :& DOne D6))) (Tagged (SProxy "_bytes2VectorListVal") (Array (Vector (DOne D4) (BytesN (DOne D2))))))

setValues :: TransactionOptions NoPay -> { _uintVal :: (UIntN (D2 :& D5 :& DOne D6)), _intVal :: (IntN (D2 :& D5 :& DOne D6)), _boolVal :: Boolean, _int224Val :: (IntN (D2 :& D2 :& DOne D4)), _boolVectorVal :: (Vector (DOne D2) Boolean), _intListVal :: (Array (IntN (D2 :& D5 :& DOne D6))), _stringVal :: String, _bytes16Val :: (BytesN (D1 :& DOne D6)), _bytes2VectorListVal :: (Array (Vector (DOne D4) (BytesN (DOne D2)))) } -> Web3 HexString
setValues x0 r = uncurryFields  r $ setValues' x0
   where
    setValues' :: TransactionOptions NoPay -> (Tagged (SProxy "_uintVal") (UIntN (D2 :& D5 :& DOne D6))) -> (Tagged (SProxy "_intVal") (IntN (D2 :& D5 :& DOne D6))) -> (Tagged (SProxy "_boolVal") Boolean) -> (Tagged (SProxy "_int224Val") (IntN (D2 :& D2 :& DOne D4))) -> (Tagged (SProxy "_boolVectorVal") (Vector (DOne D2) Boolean)) -> (Tagged (SProxy "_intListVal") (Array (IntN (D2 :& D5 :& DOne D6)))) -> (Tagged (SProxy "_stringVal") String) -> (Tagged (SProxy "_bytes16Val") (BytesN (D1 :& DOne D6))) -> (Tagged (SProxy "_bytes2VectorListVal") (Array (Vector (DOne D4) (BytesN (DOne D2))))) -> Web3 HexString
    setValues' y0 y1 y2 y3 y4 y5 y6 y7 y8 y9 = sendTx y0 ((tagged $ Tuple9 y1 y2 y3 y4 y5 y6 y7 y8 y9) :: SetValuesFn)

--------------------------------------------------------------------------------
-- | StringValFn
--------------------------------------------------------------------------------


type StringValFn = Tagged (SProxy "stringVal()") (Tuple0 )

stringVal :: TransactionOptions NoPay -> ChainCursor -> Web3 (Either CallError String)
stringVal x0 cm = map unTuple1 <$> call x0 cm ((tagged $ Tuple0 ) :: StringValFn)

--------------------------------------------------------------------------------
-- | UintValFn
--------------------------------------------------------------------------------


type UintValFn = Tagged (SProxy "uintVal()") (Tuple0 )

uintVal :: TransactionOptions NoPay -> ChainCursor -> Web3 (Either CallError (UIntN (D2 :& D5 :& DOne D6)))
uintVal x0 cm = map unTuple1 <$> call x0 cm ((tagged $ Tuple0 ) :: UintValFn)
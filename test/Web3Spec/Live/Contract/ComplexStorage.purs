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
import Network.Ethereum.Web3 (class EventFilter, HexString, Vector, _address, _topics, call, sendTx)
import Network.Ethereum.Web3.Contract.Internal (uncurryFields)
import Network.Ethereum.Web3.Solidity (BytesN, D1, D2, D4, D5, D6, DOne, IntN, Tuple0(..), Tuple1(..), Tuple2(..), Tuple9(..), UIntN, class IndexedEvent, unTuple1)
import Network.Ethereum.Web3.Solidity.Size (type (:&))
import Network.Ethereum.Web3.Types (CallError, ChainCursor, NoPay, TransactionOptions, Web3, defaultFilter, mkHexString)
import Partial.Unsafe (unsafePartial)
import Web3Spec.LiveSpec.Utils (mkHexString')

deployBytecode :: HexString
deployBytecode = mkHexString'
  "608060405234801561001057600080fd5b50610d12806100206000396000f3006080604052600436106100a4576000357c0100000000000000000000000000000000000000000000000000000000900463ffffffff1680631aadfbdb146100a95780632316afea1461024d578063579c738a1461027c57806382a35c771461030c5780638ba27dca146103375780639a0283ed146103785780639d49a304146103a35780639eb7a6b2146103d4578063c84a410014610425578063f4f67ad71461046a575b600080fd5b3480156100b557600080fd5b5061024b60048036038101908080359060200190929190803590602001909291908035151590602001909291908035601b0b9060200190929190806040019060028060200260405190810160405280929190826002602002808284378201915050505050919291929080359060200190820180359060200190808060200260200160405190810160405280939291908181526020018383602002808284378201915050505050509192919290803590602001908201803590602001908080601f016020809104026020016040519081016040528093929190818152602001838380828437820191505050505050919291929080356fffffffffffffffffffffffffffffffff1916906020019092919080359060200190820180359060200190808060200260200160405190810160405280939291908181526020016000905b828210156102395784848390506080020160048060200260405190810160405280929190826004602002808284378201915050505050815260200190600101906101f4565b505050505091929192905050506104f7565b005b34801561025957600080fd5b506102626107e9565b604051808215151515815260200191505060405180910390f35b34801561028857600080fd5b506102916107fc565b6040518080602001828103825283818151815260200191508051906020019080838360005b838110156102d15780820151818401526020810190506102b6565b50505050905090810190601f1680156102fe5780820380516001836020036101000a031916815260200191505b509250505060405180910390f35b34801561031857600080fd5b5061032161089a565b6040518082815260200191505060405180910390f35b34801561034357600080fd5b50610362600480360381019080803590602001909291905050506108a0565b6040518082815260200191505060405180910390f35b34801561038457600080fd5b5061038d6108c3565b6040518082815260200191505060405180910390f35b3480156103af57600080fd5b506103b86108c9565b6040518082601b0b601b0b815260200191505060405180910390f35b3480156103e057600080fd5b506103e96108dc565b60405180826fffffffffffffffffffffffffffffffff19166fffffffffffffffffffffffffffffffff1916815260200191505060405180910390f35b34801561043157600080fd5b50610450600480360381019080803590602001909291905050506108ff565b604051808215151515815260200191505060405180910390f35b34801561047657600080fd5b5061049f6004803603810190808035906020019092919080359060200190929190505050610928565b60405180827dffffffffffffffffffffffffffffffffffffffffffffffffffffffffffff19167dffffffffffffffffffffffffffffffffffffffffffffffffffffffffffff1916815260200191505060405180910390f35b886000819055508760018190555086600260006101000a81548160ff02191690831515021790555085600260016101000a8154817bffffffffffffffffffffffffffffffffffffffffffffffffffffffff0219169083601b0b7bffffffffffffffffffffffffffffffffffffffffffffffffffffffff16021790555084600390600261058492919061098c565b50836004908051906020019061059b929190610a25565b5082600590805190602001906105b2929190610a72565b5081600660006101000a8154816fffffffffffffffffffffffffffffffff0219169083700100000000000000000000000000000000900402179055508060079080519060200190610604929190610af2565b507f88d23351ad32a937b11ca10530404f8297d29803e94709336b48c1f82c15b3cc898989898989898989604051808a81526020018981526020018815151515815260200187601b0b601b0b815260200186600260200280838360005b8381101561067c578082015181840152602081019050610661565b505050509050018060200180602001856fffffffffffffffffffffffffffffffff19166fffffffffffffffffffffffffffffffff1916815260200180602001848103845288818151815260200191508051906020019060200280838360005b838110156106f65780820151818401526020810190506106db565b50505050905001848103835287818151815260200191508051906020019080838360005b8381101561073557808201518184015260208101905061071a565b50505050905090810190601f1680156107625780820380516001836020036101000a031916815260200191505b508481038252858181518152602001915080516000925b818410156107c457828490602001906020020151600460200280838360005b838110156107b3578082015181840152602081019050610798565b505050509050019260010192610779565b925050509c5050505050505050505050505060405180910390a1505050505050505050565b600260009054906101000a900460ff1681565b60058054600181600116156101000203166002900480601f0160208091040260200160405190810160405280929190818152602001828054600181600116156101000203166002900480156108925780601f1061086757610100808354040283529160200191610892565b820191906000526020600020905b81548152906001019060200180831161087557829003601f168201915b505050505081565b60015481565b6004818154811015156108af57fe5b906000526020600020016000915090505481565b60005481565b600260019054906101000a9004601b0b81565b600660009054906101000a90047001000000000000000000000000000000000281565b60038160028110151561090e57fe5b60209182820401919006915054906101000a900460ff1681565b60078281548110151561093757fe5b906000526020600020018160048110151561094e57fe5b60109182820401919006600202915091509054906101000a90047e010000000000000000000000000000000000000000000000000000000000000281565b826002601f01602090048101928215610a145791602002820160005b838211156109e557835183826101000a81548160ff02191690831515021790555092602001926001016020816000010492830192600103026109a8565b8015610a125782816101000a81549060ff02191690556001016020816000010492830192600103026109e5565b505b509050610a219190610b4d565b5090565b828054828255906000526020600020908101928215610a61579160200282015b82811115610a60578251825591602001919060010190610a45565b5b509050610a6e9190610b7d565b5090565b828054600181600116156101000203166002900490600052602060002090601f016020900481019282601f10610ab357805160ff1916838001178555610ae1565b82800160010185558215610ae1579182015b82811115610ae0578251825591602001919060010190610ac5565b5b509050610aee9190610ba2565b5090565b828054828255906000526020600020908101928215610b3c579160200282015b82811115610b3b57825182906004610b2b929190610bc7565b5091602001919060010190610b12565b5b509050610b499190610c82565b5090565b610b7a91905b80821115610b7657600081816101000a81549060ff021916905550600101610b53565b5090565b90565b610b9f91905b80821115610b9b576000816000905550600101610b83565b5090565b90565b610bc491905b80821115610bc0576000816000905550600101610ba8565b5090565b90565b826004600f01601090048101928215610c715791602002820160005b83821115610c4157835183826101000a81548161ffff02191690837e01000000000000000000000000000000000000000000000000000000000000900402179055509260200192600201602081600101049283019260010302610be3565b8015610c6f5782816101000a81549061ffff0219169055600201602081600101049283019260010302610c41565b505b509050610c7e9190610cae565b5090565b610cab91905b80821115610ca75760008181610c9e9190610cdf565b50600101610c88565b5090565b90565b610cdc91905b80821115610cd857600081816101000a81549061ffff021916905550600101610cb4565b5090565b90565b50600090555600a165627a7a723058204e19d01de6878116648075644b8109dc8582337aaa5164ca6a954c3ee0c65e1a0029"

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

--------------------------------------------------------------------------------
-- | ERC20
--------------------------------------------------------------------------------

module Contracts.ERC20 where

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
import Network.Ethereum.Web3 (_address, _topics, call, class EventFilter, deployContract, sendTx)
import Network.Ethereum.Web3.Contract.Internal (uncurryFields)
import Network.Ethereum.Web3.Solidity (D2, D5, D6, D8, DOne, Tuple0(..), Tuple1(..), Tuple2(..), Tuple3(..), Tuple4(..), UIntN, class IndexedEvent, unTuple1)
import Network.Ethereum.Web3.Solidity.Size (type (:&))
import Network.Ethereum.Web3.Types (Address, CallError, ChainCursor, HexString, NoPay, TransactionOptions, Web3, defaultFilter, mkHexString)
import Partial.Unsafe (unsafePartial)
--------------------------------------------------------------------------------
-- | ConstructorFn
--------------------------------------------------------------------------------


type ConstructorFn = Tagged (SProxy "constructor(string,string,uint8,uint256)") (Tuple4 (Tagged (SProxy "_name") String) (Tagged (SProxy "_symbol") String) (Tagged (SProxy "_decimals") (UIntN (DOne D8))) (Tagged (SProxy "_count") (UIntN (D2 :& D5 :& DOne D6))))

constructor :: TransactionOptions NoPay -> HexString -> { _name :: String, _symbol :: String, _decimals :: (UIntN (DOne D8)), _count :: (UIntN (D2 :& D5 :& DOne D6)) } -> Web3 HexString
constructor x0 bc r = uncurryFields  r $ constructor' x0 bc
   where
    constructor' :: TransactionOptions NoPay -> HexString -> (Tagged (SProxy "_name") String) -> (Tagged (SProxy "_symbol") String) -> (Tagged (SProxy "_decimals") (UIntN (DOne D8))) -> (Tagged (SProxy "_count") (UIntN (D2 :& D5 :& DOne D6))) -> Web3 HexString
    constructor' y0 bc' y2 y3 y4 y5 = deployContract y0 bc' ((tagged $ Tuple4 y2 y3 y4 y5) :: ConstructorFn)

--------------------------------------------------------------------------------
-- | Transfer
--------------------------------------------------------------------------------


newtype Transfer = Transfer {_from :: Address,_to :: Address,_value :: (UIntN (D2 :& D5 :& DOne D6))}

derive instance newtypeTransfer :: Newtype Transfer _

instance eventFilterTransfer :: EventFilter Transfer where
  eventFilter _ addr = defaultFilter
    # _address .~ Just addr
    # _topics .~ Just [Just ( unsafePartial $ fromJust $ mkHexString "ddf252ad1be2c89b69c2b068fc378daa952ba7f163c4a11628f55a4df523b3ef"),Nothing,Nothing]

instance indexedEventTransfer :: IndexedEvent (Tuple2 (Tagged (SProxy "_from") Address) (Tagged (SProxy "_to") Address)) (Tuple1 (Tagged (SProxy "_value") (UIntN (D2 :& D5 :& DOne D6)))) Transfer where
  isAnonymous _ = false

derive instance genericTransfer :: Generic Transfer _

instance eventGenericTransferShow :: Show Transfer where
  show = genericShow

instance eventGenericTransfereq :: Eq Transfer where
  eq = genericEq

--------------------------------------------------------------------------------
-- | Approval
--------------------------------------------------------------------------------


newtype Approval = Approval {_owner :: Address,_spender :: Address,_value :: (UIntN (D2 :& D5 :& DOne D6))}

derive instance newtypeApproval :: Newtype Approval _

instance eventFilterApproval :: EventFilter Approval where
  eventFilter _ addr = defaultFilter
    # _address .~ Just addr
    # _topics .~ Just [Just ( unsafePartial $ fromJust $ mkHexString "8c5be1e5ebec7d5bd14f71427d1e84f3dd0314c0f7b2291e5b200ac8c7c3b925"),Nothing,Nothing]

instance indexedEventApproval :: IndexedEvent (Tuple2 (Tagged (SProxy "_owner") Address) (Tagged (SProxy "_spender") Address)) (Tuple1 (Tagged (SProxy "_value") (UIntN (D2 :& D5 :& DOne D6)))) Approval where
  isAnonymous _ = false

derive instance genericApproval :: Generic Approval _

instance eventGenericApprovalShow :: Show Approval where
  show = genericShow

instance eventGenericApprovaleq :: Eq Approval where
  eq = genericEq

--------------------------------------------------------------------------------
-- | AllowanceFn
--------------------------------------------------------------------------------


type AllowanceFn = Tagged (SProxy "allowance(address,address)") (Tuple2 (Tagged (SProxy "_owner") Address) (Tagged (SProxy "_spender") Address))

allowance :: TransactionOptions NoPay -> ChainCursor -> { _owner :: Address, _spender :: Address } -> Web3 (Either CallError (UIntN (D2 :& D5 :& DOne D6)))
allowance x0 cm r = uncurryFields  r $ allowance' x0 cm
   where
    allowance' :: TransactionOptions NoPay -> ChainCursor -> (Tagged (SProxy "_owner") Address) -> (Tagged (SProxy "_spender") Address) -> Web3 (Either CallError (UIntN (D2 :& D5 :& DOne D6)))
    allowance' y0 cm' y2 y3 = map unTuple1 <$> call y0 cm' ((tagged $ Tuple2 y2 y3) :: AllowanceFn)

--------------------------------------------------------------------------------
-- | ApproveFn
--------------------------------------------------------------------------------


type ApproveFn = Tagged (SProxy "approve(address,uint256)") (Tuple2 (Tagged (SProxy "_spender") Address) (Tagged (SProxy "_value") (UIntN (D2 :& D5 :& DOne D6))))

approve :: TransactionOptions NoPay -> { _spender :: Address, _value :: (UIntN (D2 :& D5 :& DOne D6)) } -> Web3 HexString
approve x0 r = uncurryFields  r $ approve' x0
   where
    approve' :: TransactionOptions NoPay -> (Tagged (SProxy "_spender") Address) -> (Tagged (SProxy "_value") (UIntN (D2 :& D5 :& DOne D6))) -> Web3 HexString
    approve' y0 y1 y2 = sendTx y0 ((tagged $ Tuple2 y1 y2) :: ApproveFn)

--------------------------------------------------------------------------------
-- | BalanceOfFn
--------------------------------------------------------------------------------


type BalanceOfFn = Tagged (SProxy "balanceOf(address)") (Tuple1 (Tagged (SProxy "_owner") Address))

balanceOf :: TransactionOptions NoPay -> ChainCursor -> { _owner :: Address } -> Web3 (Either CallError (UIntN (D2 :& D5 :& DOne D6)))
balanceOf x0 cm r = uncurryFields  r $ balanceOf' x0 cm
   where
    balanceOf' :: TransactionOptions NoPay -> ChainCursor -> (Tagged (SProxy "_owner") Address) -> Web3 (Either CallError (UIntN (D2 :& D5 :& DOne D6)))
    balanceOf' y0 cm' y2 = map unTuple1 <$> call y0 cm' ((tagged $ Tuple1 y2) :: BalanceOfFn)

--------------------------------------------------------------------------------
-- | DecimalsFn
--------------------------------------------------------------------------------


type DecimalsFn = Tagged (SProxy "decimals()") (Tuple0 )

decimals :: TransactionOptions NoPay -> ChainCursor -> Web3 (Either CallError (UIntN (DOne D8)))
decimals x0 cm = map unTuple1 <$> call x0 cm ((tagged $ Tuple0 ) :: DecimalsFn)

--------------------------------------------------------------------------------
-- | DelegateFn
--------------------------------------------------------------------------------


type DelegateFn = Tagged (SProxy "delegate(address)") (Tuple1 (Tagged (SProxy "_owner") Address))

delegate :: TransactionOptions NoPay -> { _owner :: Address } -> Web3 HexString
delegate x0 r = uncurryFields  r $ delegate' x0
   where
    delegate' :: TransactionOptions NoPay -> (Tagged (SProxy "_owner") Address) -> Web3 HexString
    delegate' y0 y1 = sendTx y0 ((tagged $ Tuple1 y1) :: DelegateFn)

--------------------------------------------------------------------------------
-- | KillFn
--------------------------------------------------------------------------------


type KillFn = Tagged (SProxy "kill()") (Tuple0 )

kill :: TransactionOptions NoPay -> Web3 HexString
kill x0 = sendTx x0 ((tagged $ Tuple0 ) :: KillFn)

--------------------------------------------------------------------------------
-- | NameFn
--------------------------------------------------------------------------------


type NameFn = Tagged (SProxy "name()") (Tuple0 )

name :: TransactionOptions NoPay -> ChainCursor -> Web3 (Either CallError String)
name x0 cm = map unTuple1 <$> call x0 cm ((tagged $ Tuple0 ) :: NameFn)

--------------------------------------------------------------------------------
-- | OwnerFn
--------------------------------------------------------------------------------


type OwnerFn = Tagged (SProxy "owner()") (Tuple0 )

owner :: TransactionOptions NoPay -> ChainCursor -> Web3 (Either CallError Address)
owner x0 cm = map unTuple1 <$> call x0 cm ((tagged $ Tuple0 ) :: OwnerFn)

--------------------------------------------------------------------------------
-- | SymbolFn
--------------------------------------------------------------------------------


type SymbolFn = Tagged (SProxy "symbol()") (Tuple0 )

symbol :: TransactionOptions NoPay -> ChainCursor -> Web3 (Either CallError String)
symbol x0 cm = map unTuple1 <$> call x0 cm ((tagged $ Tuple0 ) :: SymbolFn)

--------------------------------------------------------------------------------
-- | TotalSupplyFn
--------------------------------------------------------------------------------


type TotalSupplyFn = Tagged (SProxy "totalSupply()") (Tuple0 )

totalSupply :: TransactionOptions NoPay -> ChainCursor -> Web3 (Either CallError (UIntN (D2 :& D5 :& DOne D6)))
totalSupply x0 cm = map unTuple1 <$> call x0 cm ((tagged $ Tuple0 ) :: TotalSupplyFn)

--------------------------------------------------------------------------------
-- | TransferFn
--------------------------------------------------------------------------------


type TransferFn = Tagged (SProxy "transfer(address,uint256)") (Tuple2 (Tagged (SProxy "_to") Address) (Tagged (SProxy "_value") (UIntN (D2 :& D5 :& DOne D6))))

transfer :: TransactionOptions NoPay -> { _to :: Address, _value :: (UIntN (D2 :& D5 :& DOne D6)) } -> Web3 HexString
transfer x0 r = uncurryFields  r $ transfer' x0
   where
    transfer' :: TransactionOptions NoPay -> (Tagged (SProxy "_to") Address) -> (Tagged (SProxy "_value") (UIntN (D2 :& D5 :& DOne D6))) -> Web3 HexString
    transfer' y0 y1 y2 = sendTx y0 ((tagged $ Tuple2 y1 y2) :: TransferFn)

--------------------------------------------------------------------------------
-- | TransferFromFn
--------------------------------------------------------------------------------


type TransferFromFn = Tagged (SProxy "transferFrom(address,address,uint256)") (Tuple3 (Tagged (SProxy "_from") Address) (Tagged (SProxy "_to") Address) (Tagged (SProxy "_value") (UIntN (D2 :& D5 :& DOne D6))))

transferFrom :: TransactionOptions NoPay -> { _from :: Address, _to :: Address, _value :: (UIntN (D2 :& D5 :& DOne D6)) } -> Web3 HexString
transferFrom x0 r = uncurryFields  r $ transferFrom' x0
   where
    transferFrom' :: TransactionOptions NoPay -> (Tagged (SProxy "_from") Address) -> (Tagged (SProxy "_to") Address) -> (Tagged (SProxy "_value") (UIntN (D2 :& D5 :& DOne D6))) -> Web3 HexString
    transferFrom' y0 y1 y2 y3 = sendTx y0 ((tagged $ Tuple3 y1 y2 y3) :: TransferFromFn)

--------------------------------------------------------------------------------
-- | UnapproveFn
--------------------------------------------------------------------------------


type UnapproveFn = Tagged (SProxy "unapprove(address)") (Tuple1 (Tagged (SProxy "_spender") Address))

unapprove :: TransactionOptions NoPay -> { _spender :: Address } -> Web3 HexString
unapprove x0 r = uncurryFields  r $ unapprove' x0
   where
    unapprove' :: TransactionOptions NoPay -> (Tagged (SProxy "_spender") Address) -> Web3 HexString
    unapprove' y0 y1 = sendTx y0 ((tagged $ Tuple1 y1) :: UnapproveFn)
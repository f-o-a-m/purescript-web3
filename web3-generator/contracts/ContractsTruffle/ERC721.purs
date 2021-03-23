--------------------------------------------------------------------------------
-- | ERC721
--------------------------------------------------------------------------------

module ContractsTruffle.ERC721 where

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
import Network.Ethereum.Web3.Solidity (ByteString, D2, D5, D6, DOne, Tuple1(..), Tuple2(..), Tuple3(..), Tuple4(..), UIntN, class IndexedEvent, unTuple1)
import Network.Ethereum.Web3.Solidity.Size (type (:&))
import Network.Ethereum.Web3.Types (Address, CallError, ChainCursor, HexString, NoPay, TransactionOptions, Web3, defaultFilter, mkHexString)
import Partial.Unsafe (unsafePartial)
--------------------------------------------------------------------------------
-- | ConstructorFn
--------------------------------------------------------------------------------


type ConstructorFn = Tagged (SProxy "constructor(uint256)") (Tuple1 (Tagged (SProxy "totalSupply") (UIntN (D2 :& D5 :& DOne D6))))

constructor :: TransactionOptions NoPay -> HexString -> { totalSupply :: (UIntN (D2 :& D5 :& DOne D6)) } -> Web3 HexString
constructor x0 bc r = uncurryFields  r $ constructor' x0 bc
   where
    constructor' :: TransactionOptions NoPay -> HexString -> (Tagged (SProxy "totalSupply") (UIntN (D2 :& D5 :& DOne D6))) -> Web3 HexString
    constructor' y0 bc' y2 = deployContract y0 bc' ((tagged $ Tuple1 y2) :: ConstructorFn)

--------------------------------------------------------------------------------
-- | Transfer
--------------------------------------------------------------------------------


newtype Transfer = Transfer {_from :: Address,_to :: Address,_tokenId :: (UIntN (D2 :& D5 :& DOne D6))}

derive instance newtypeTransfer :: Newtype Transfer _

instance eventFilterTransfer :: EventFilter Transfer where
  eventFilter _ addr = defaultFilter
    # _address .~ Just addr
    # _topics .~ Just [Just ( unsafePartial $ fromJust $ mkHexString "ddf252ad1be2c89b69c2b068fc378daa952ba7f163c4a11628f55a4df523b3ef"),Nothing,Nothing]

instance indexedEventTransfer :: IndexedEvent (Tuple2 (Tagged (SProxy "_from") Address) (Tagged (SProxy "_to") Address)) (Tuple1 (Tagged (SProxy "_tokenId") (UIntN (D2 :& D5 :& DOne D6)))) Transfer where
  isAnonymous _ = false

derive instance genericTransfer :: Generic Transfer _

instance eventGenericTransferShow :: Show Transfer where
  show = genericShow

instance eventGenericTransfereq :: Eq Transfer where
  eq = genericEq

--------------------------------------------------------------------------------
-- | Approval
--------------------------------------------------------------------------------


newtype Approval = Approval {_owner :: Address,_approved :: Address,_tokenId :: (UIntN (D2 :& D5 :& DOne D6))}

derive instance newtypeApproval :: Newtype Approval _

instance eventFilterApproval :: EventFilter Approval where
  eventFilter _ addr = defaultFilter
    # _address .~ Just addr
    # _topics .~ Just [Just ( unsafePartial $ fromJust $ mkHexString "8c5be1e5ebec7d5bd14f71427d1e84f3dd0314c0f7b2291e5b200ac8c7c3b925"),Nothing,Nothing]

instance indexedEventApproval :: IndexedEvent (Tuple2 (Tagged (SProxy "_owner") Address) (Tagged (SProxy "_approved") Address)) (Tuple1 (Tagged (SProxy "_tokenId") (UIntN (D2 :& D5 :& DOne D6)))) Approval where
  isAnonymous _ = false

derive instance genericApproval :: Generic Approval _

instance eventGenericApprovalShow :: Show Approval where
  show = genericShow

instance eventGenericApprovaleq :: Eq Approval where
  eq = genericEq

--------------------------------------------------------------------------------
-- | ApprovalForAll
--------------------------------------------------------------------------------


newtype ApprovalForAll = ApprovalForAll {_owner :: Address,_operator :: Address,_approved :: Boolean}

derive instance newtypeApprovalForAll :: Newtype ApprovalForAll _

instance eventFilterApprovalForAll :: EventFilter ApprovalForAll where
  eventFilter _ addr = defaultFilter
    # _address .~ Just addr
    # _topics .~ Just [Just ( unsafePartial $ fromJust $ mkHexString "17307eab39ab6107e8899845ad3d59bd9653f200f220920489ca2b5937696c31"),Nothing,Nothing]

instance indexedEventApprovalForAll :: IndexedEvent (Tuple2 (Tagged (SProxy "_owner") Address) (Tagged (SProxy "_operator") Address)) (Tuple1 (Tagged (SProxy "_approved") Boolean)) ApprovalForAll where
  isAnonymous _ = false

derive instance genericApprovalForAll :: Generic ApprovalForAll _

instance eventGenericApprovalForAllShow :: Show ApprovalForAll where
  show = genericShow

instance eventGenericApprovalForAlleq :: Eq ApprovalForAll where
  eq = genericEq

--------------------------------------------------------------------------------
-- | ApproveFn
--------------------------------------------------------------------------------


type ApproveFn = Tagged (SProxy "approve(address,uint256)") (Tuple2 (Tagged (SProxy "_to") Address) (Tagged (SProxy "_tokenId") (UIntN (D2 :& D5 :& DOne D6))))

approve :: TransactionOptions NoPay -> { _to :: Address, _tokenId :: (UIntN (D2 :& D5 :& DOne D6)) } -> Web3 HexString
approve x0 r = uncurryFields  r $ approve' x0
   where
    approve' :: TransactionOptions NoPay -> (Tagged (SProxy "_to") Address) -> (Tagged (SProxy "_tokenId") (UIntN (D2 :& D5 :& DOne D6))) -> Web3 HexString
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
-- | ExistsFn
--------------------------------------------------------------------------------


type ExistsFn = Tagged (SProxy "exists(uint256)") (Tuple1 (Tagged (SProxy "_tokenId") (UIntN (D2 :& D5 :& DOne D6))))

exists :: TransactionOptions NoPay -> ChainCursor -> { _tokenId :: (UIntN (D2 :& D5 :& DOne D6)) } -> Web3 (Either CallError Boolean)
exists x0 cm r = uncurryFields  r $ exists' x0 cm
   where
    exists' :: TransactionOptions NoPay -> ChainCursor -> (Tagged (SProxy "_tokenId") (UIntN (D2 :& D5 :& DOne D6))) -> Web3 (Either CallError Boolean)
    exists' y0 cm' y2 = map unTuple1 <$> call y0 cm' ((tagged $ Tuple1 y2) :: ExistsFn)

--------------------------------------------------------------------------------
-- | GetApprovedFn
--------------------------------------------------------------------------------


type GetApprovedFn = Tagged (SProxy "getApproved(uint256)") (Tuple1 (Tagged (SProxy "_tokenId") (UIntN (D2 :& D5 :& DOne D6))))

getApproved :: TransactionOptions NoPay -> ChainCursor -> { _tokenId :: (UIntN (D2 :& D5 :& DOne D6)) } -> Web3 (Either CallError Address)
getApproved x0 cm r = uncurryFields  r $ getApproved' x0 cm
   where
    getApproved' :: TransactionOptions NoPay -> ChainCursor -> (Tagged (SProxy "_tokenId") (UIntN (D2 :& D5 :& DOne D6))) -> Web3 (Either CallError Address)
    getApproved' y0 cm' y2 = map unTuple1 <$> call y0 cm' ((tagged $ Tuple1 y2) :: GetApprovedFn)

--------------------------------------------------------------------------------
-- | IsApprovedForAllFn
--------------------------------------------------------------------------------


type IsApprovedForAllFn = Tagged (SProxy "isApprovedForAll(address,address)") (Tuple2 (Tagged (SProxy "_owner") Address) (Tagged (SProxy "_operator") Address))

isApprovedForAll :: TransactionOptions NoPay -> ChainCursor -> { _owner :: Address, _operator :: Address } -> Web3 (Either CallError Boolean)
isApprovedForAll x0 cm r = uncurryFields  r $ isApprovedForAll' x0 cm
   where
    isApprovedForAll' :: TransactionOptions NoPay -> ChainCursor -> (Tagged (SProxy "_owner") Address) -> (Tagged (SProxy "_operator") Address) -> Web3 (Either CallError Boolean)
    isApprovedForAll' y0 cm' y2 y3 = map unTuple1 <$> call y0 cm' ((tagged $ Tuple2 y2 y3) :: IsApprovedForAllFn)

--------------------------------------------------------------------------------
-- | OwnerOfFn
--------------------------------------------------------------------------------


type OwnerOfFn = Tagged (SProxy "ownerOf(uint256)") (Tuple1 (Tagged (SProxy "_tokenId") (UIntN (D2 :& D5 :& DOne D6))))

ownerOf :: TransactionOptions NoPay -> ChainCursor -> { _tokenId :: (UIntN (D2 :& D5 :& DOne D6)) } -> Web3 (Either CallError Address)
ownerOf x0 cm r = uncurryFields  r $ ownerOf' x0 cm
   where
    ownerOf' :: TransactionOptions NoPay -> ChainCursor -> (Tagged (SProxy "_tokenId") (UIntN (D2 :& D5 :& DOne D6))) -> Web3 (Either CallError Address)
    ownerOf' y0 cm' y2 = map unTuple1 <$> call y0 cm' ((tagged $ Tuple1 y2) :: OwnerOfFn)

--------------------------------------------------------------------------------
-- | SafeTransferFrom4Fn
--------------------------------------------------------------------------------


type SafeTransferFrom4Fn = Tagged (SProxy "safeTransferFrom4(address,address,uint256,bytes)") (Tuple4 (Tagged (SProxy "_from") Address) (Tagged (SProxy "_to") Address) (Tagged (SProxy "_tokenId") (UIntN (D2 :& D5 :& DOne D6))) (Tagged (SProxy "_data") ByteString))

safeTransferFrom4 :: TransactionOptions NoPay -> { _from :: Address, _to :: Address, _tokenId :: (UIntN (D2 :& D5 :& DOne D6)), _data :: ByteString } -> Web3 HexString
safeTransferFrom4 x0 r = uncurryFields  r $ safeTransferFrom4' x0
   where
    safeTransferFrom4' :: TransactionOptions NoPay -> (Tagged (SProxy "_from") Address) -> (Tagged (SProxy "_to") Address) -> (Tagged (SProxy "_tokenId") (UIntN (D2 :& D5 :& DOne D6))) -> (Tagged (SProxy "_data") ByteString) -> Web3 HexString
    safeTransferFrom4' y0 y1 y2 y3 y4 = sendTx y0 ((tagged $ Tuple4 y1 y2 y3 y4) :: SafeTransferFrom4Fn)

--------------------------------------------------------------------------------
-- | SafeTransferFrom3Fn
--------------------------------------------------------------------------------


type SafeTransferFrom3Fn = Tagged (SProxy "safeTransferFrom3(address,address,uint256)") (Tuple3 (Tagged (SProxy "_from") Address) (Tagged (SProxy "_to") Address) (Tagged (SProxy "_tokenId") (UIntN (D2 :& D5 :& DOne D6))))

safeTransferFrom3 :: TransactionOptions NoPay -> { _from :: Address, _to :: Address, _tokenId :: (UIntN (D2 :& D5 :& DOne D6)) } -> Web3 HexString
safeTransferFrom3 x0 r = uncurryFields  r $ safeTransferFrom3' x0
   where
    safeTransferFrom3' :: TransactionOptions NoPay -> (Tagged (SProxy "_from") Address) -> (Tagged (SProxy "_to") Address) -> (Tagged (SProxy "_tokenId") (UIntN (D2 :& D5 :& DOne D6))) -> Web3 HexString
    safeTransferFrom3' y0 y1 y2 y3 = sendTx y0 ((tagged $ Tuple3 y1 y2 y3) :: SafeTransferFrom3Fn)

--------------------------------------------------------------------------------
-- | SetApprovalForAllFn
--------------------------------------------------------------------------------


type SetApprovalForAllFn = Tagged (SProxy "setApprovalForAll(address,bool)") (Tuple2 (Tagged (SProxy "_to") Address) (Tagged (SProxy "_approved") Boolean))

setApprovalForAll :: TransactionOptions NoPay -> { _to :: Address, _approved :: Boolean } -> Web3 HexString
setApprovalForAll x0 r = uncurryFields  r $ setApprovalForAll' x0
   where
    setApprovalForAll' :: TransactionOptions NoPay -> (Tagged (SProxy "_to") Address) -> (Tagged (SProxy "_approved") Boolean) -> Web3 HexString
    setApprovalForAll' y0 y1 y2 = sendTx y0 ((tagged $ Tuple2 y1 y2) :: SetApprovalForAllFn)

--------------------------------------------------------------------------------
-- | TransferFromFn
--------------------------------------------------------------------------------


type TransferFromFn = Tagged (SProxy "transferFrom(address,address,uint256)") (Tuple3 (Tagged (SProxy "_from") Address) (Tagged (SProxy "_to") Address) (Tagged (SProxy "_tokenId") (UIntN (D2 :& D5 :& DOne D6))))

transferFrom :: TransactionOptions NoPay -> { _from :: Address, _to :: Address, _tokenId :: (UIntN (D2 :& D5 :& DOne D6)) } -> Web3 HexString
transferFrom x0 r = uncurryFields  r $ transferFrom' x0
   where
    transferFrom' :: TransactionOptions NoPay -> (Tagged (SProxy "_from") Address) -> (Tagged (SProxy "_to") Address) -> (Tagged (SProxy "_tokenId") (UIntN (D2 :& D5 :& DOne D6))) -> Web3 HexString
    transferFrom' y0 y1 y2 y3 = sendTx y0 ((tagged $ Tuple3 y1 y2 y3) :: TransferFromFn)
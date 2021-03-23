--------------------------------------------------------------------------------
-- | SimpleTupleStorage
--------------------------------------------------------------------------------

module Contracts.SimpleTupleStorage where

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
import Network.Ethereum.Web3.Solidity (D2, D5, D6, DOne, Tuple0(..), Tuple2(..), UIntN, class IndexedEvent, unTuple1)
import Network.Ethereum.Web3.Solidity.Size (type (:&))
import Network.Ethereum.Web3.Types (CallError, ChainCursor, HexString, NoPay, TransactionOptions, Web3, defaultFilter, mkHexString)
import Partial.Unsafe (unsafePartial)
--------------------------------------------------------------------------------
-- | ConstructorFn
--------------------------------------------------------------------------------


type ConstructorFn = Tagged (SProxy "constructor()") (Tuple0 )

constructor :: TransactionOptions NoPay -> HexString -> Web3 HexString
constructor x0 bc = deployContract x0 bc ((tagged $ Tuple0 ) :: ConstructorFn)

--------------------------------------------------------------------------------
-- | Stored
--------------------------------------------------------------------------------


newtype Stored = Stored {x :: (UIntN (D2 :& D5 :& DOne D6)),y :: (UIntN (D2 :& D5 :& DOne D6))}

derive instance newtypeStored :: Newtype Stored _

instance eventFilterStored :: EventFilter Stored where
  eventFilter _ addr = defaultFilter
    # _address .~ Just addr
    # _topics .~ Just [Just ( unsafePartial $ fromJust $ mkHexString "793333530bc27d845c36d8799afd0c1152c33d074b4419780fc2a82658267614")]

instance indexedEventStored :: IndexedEvent (Tuple0 ) (Tuple2 (Tagged (SProxy "x") (UIntN (D2 :& D5 :& DOne D6))) (Tagged (SProxy "y") (UIntN (D2 :& D5 :& DOne D6)))) Stored where
  isAnonymous _ = false

derive instance genericStored :: Generic Stored _

instance eventGenericStoredShow :: Show Stored where
  show = genericShow

instance eventGenericStoredeq :: Eq Stored where
  eq = genericEq

--------------------------------------------------------------------------------
-- | GetTupleFn
--------------------------------------------------------------------------------


type GetTupleFn = Tagged (SProxy "getTuple()") (Tuple0 )

getTuple :: TransactionOptions NoPay -> ChainCursor -> Web3 (Either CallError (Tuple2 (UIntN (D2 :& D5 :& DOne D6)) (UIntN (D2 :& D5 :& DOne D6))))
getTuple x0 cm = call x0 cm ((tagged $ Tuple0 ) :: GetTupleFn)

--------------------------------------------------------------------------------
-- | SetTupleFn
--------------------------------------------------------------------------------


type SetTupleFn = Tagged (SProxy "setTuple(uint256,uint256)") (Tuple2 (Tagged (SProxy "x") (UIntN (D2 :& D5 :& DOne D6))) (Tagged (SProxy "y") (UIntN (D2 :& D5 :& DOne D6))))

setTuple :: TransactionOptions NoPay -> { x :: (UIntN (D2 :& D5 :& DOne D6)), y :: (UIntN (D2 :& D5 :& DOne D6)) } -> Web3 HexString
setTuple x0 r = uncurryFields  r $ setTuple' x0
   where
    setTuple' :: TransactionOptions NoPay -> (Tagged (SProxy "x") (UIntN (D2 :& D5 :& DOne D6))) -> (Tagged (SProxy "y") (UIntN (D2 :& D5 :& DOne D6))) -> Web3 HexString
    setTuple' y0 y1 y2 = sendTx y0 ((tagged $ Tuple2 y1 y2) :: SetTupleFn)

--------------------------------------------------------------------------------
-- | StoredDataXFn
--------------------------------------------------------------------------------


type StoredDataXFn = Tagged (SProxy "storedDataX()") (Tuple0 )

storedDataX :: TransactionOptions NoPay -> ChainCursor -> Web3 (Either CallError (UIntN (D2 :& D5 :& DOne D6)))
storedDataX x0 cm = map unTuple1 <$> call x0 cm ((tagged $ Tuple0 ) :: StoredDataXFn)

--------------------------------------------------------------------------------
-- | StoredDataYFn
--------------------------------------------------------------------------------


type StoredDataYFn = Tagged (SProxy "storedDataY()") (Tuple0 )

storedDataY :: TransactionOptions NoPay -> ChainCursor -> Web3 (Either CallError (UIntN (D2 :& D5 :& DOne D6)))
storedDataY x0 cm = map unTuple1 <$> call x0 cm ((tagged $ Tuple0 ) :: StoredDataYFn)
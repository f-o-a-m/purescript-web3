--------------------------------------------------------------------------------
-- | WeirdTypeStorage
--------------------------------------------------------------------------------

module Contracts.WeirdTypeStorage where

import Prelude 

import Data.Either (Either)
import Data.Functor.Tagged (Tagged, tagged)
import Data.Symbol (SProxy)
import Network.Ethereum.Web3 (Vector, call, sendTx)
import Network.Ethereum.Web3.Solidity (BytesN, D2, D3, D5, D6, DOne, Tuple0(..), Tuple1(..), Tuple2(..), UIntN, unTuple1)
import Network.Ethereum.Web3.Solidity.Size (type (:&))
import Network.Ethereum.Web3.Types (CallError, ChainCursor, HexString, NoPay, TransactionOptions, Web3)
--------------------------------------------------------------------------------
-- | GetTypesFn
--------------------------------------------------------------------------------


type GetTypesFn = Tagged (SProxy "getTypes()") (Tuple0 )

getTypes :: TransactionOptions NoPay -> ChainCursor -> Web3 (Either CallError (Tuple2 (BytesN (D3 :& DOne D2)) (Vector (DOne D2) (UIntN (D2 :& D5 :& DOne D6)))))
getTypes x0 cm = call x0 cm ((tagged $ Tuple0 ) :: GetTypesFn)

--------------------------------------------------------------------------------
-- | NameFn
--------------------------------------------------------------------------------


type NameFn = Tagged (SProxy "name()") (Tuple0 )

name :: TransactionOptions NoPay -> ChainCursor -> Web3 (Either CallError (BytesN (D3 :& DOne D2)))
name x0 cm = map unTuple1 <$> call x0 cm ((tagged $ Tuple0 ) :: NameFn)

--------------------------------------------------------------------------------
-- | NumbersFn
--------------------------------------------------------------------------------


type NumbersFn = Tagged (SProxy "numbers(uint256)") (Tuple1 (UIntN (D2 :& D5 :& DOne D6)))

numbers :: TransactionOptions NoPay -> ChainCursor -> (UIntN (D2 :& D5 :& DOne D6)) -> Web3 (Either CallError (UIntN (D2 :& D5 :& DOne D6)))
numbers x0 cm x2 = map unTuple1 <$> call x0 cm ((tagged $ Tuple1 x2) :: NumbersFn)

--------------------------------------------------------------------------------
-- | SetTypesFn
--------------------------------------------------------------------------------


type SetTypesFn = Tagged (SProxy "setTypes(bytes32,uint256[2])") (Tuple2 (BytesN (D3 :& DOne D2)) (Vector (DOne D2) (UIntN (D2 :& D5 :& DOne D6))))

setTypes :: TransactionOptions NoPay -> (BytesN (D3 :& DOne D2)) -> (Vector (DOne D2) (UIntN (D2 :& D5 :& DOne D6))) -> Web3 HexString
setTypes x0 x1 x2 = sendTx x0 ((tagged $ Tuple2 x1 x2) :: SetTypesFn)
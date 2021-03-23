--------------------------------------------------------------------------------
-- | WeirdArrayTypeStorage
--------------------------------------------------------------------------------

module Contracts.WeirdArrayTypeStorage where

import Prelude 

import Data.Either (Either)
import Data.Functor.Tagged (Tagged, tagged)
import Data.Symbol (SProxy)
import Network.Ethereum.Web3 (call, sendTx)
import Network.Ethereum.Web3.Solidity (BytesN, D2, D3, D5, D6, DOne, Tuple0(..), Tuple1(..), Tuple2(..), UIntN, unTuple1)
import Network.Ethereum.Web3.Solidity.Size (type (:&))
import Network.Ethereum.Web3.Types (CallError, ChainCursor, HexString, NoPay, TransactionOptions, Web3)
--------------------------------------------------------------------------------
-- | GetHashTypesFn
--------------------------------------------------------------------------------


type GetHashTypesFn = Tagged (SProxy "getHashTypes(bytes32[],string)") (Tuple2 (Array (BytesN (D3 :& DOne D2))) String)

getHashTypes :: TransactionOptions NoPay -> (Array (BytesN (D3 :& DOne D2))) -> String -> Web3 HexString
getHashTypes x0 x1 x2 = sendTx x0 ((tagged $ Tuple2 x1 x2) :: GetHashTypesFn)

--------------------------------------------------------------------------------
-- | GetTypesFn
--------------------------------------------------------------------------------


type GetTypesFn = Tagged (SProxy "getTypes()") (Tuple0 )

getTypes :: TransactionOptions NoPay -> ChainCursor -> Web3 (Either CallError (Tuple2 (Array (BytesN (D3 :& DOne D2))) String))
getTypes x0 cm = call x0 cm ((tagged $ Tuple0 ) :: GetTypesFn)

--------------------------------------------------------------------------------
-- | HashesFn
--------------------------------------------------------------------------------


type HashesFn = Tagged (SProxy "hashes(uint256)") (Tuple1 (UIntN (D2 :& D5 :& DOne D6)))

hashes :: TransactionOptions NoPay -> ChainCursor -> (UIntN (D2 :& D5 :& DOne D6)) -> Web3 (Either CallError (BytesN (D3 :& DOne D2)))
hashes x0 cm x2 = map unTuple1 <$> call x0 cm ((tagged $ Tuple1 x2) :: HashesFn)

--------------------------------------------------------------------------------
-- | NumbersFn
--------------------------------------------------------------------------------


type NumbersFn = Tagged (SProxy "numbers()") (Tuple0 )

numbers :: TransactionOptions NoPay -> ChainCursor -> Web3 (Either CallError String)
numbers x0 cm = map unTuple1 <$> call x0 cm ((tagged $ Tuple0 ) :: NumbersFn)
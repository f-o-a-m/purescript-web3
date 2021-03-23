--------------------------------------------------------------------------------
-- | Adaption
--------------------------------------------------------------------------------

module Contracts.Adaption where

import Prelude 

import Data.Either (Either)
import Data.Functor.Tagged (Tagged, tagged)
import Data.Symbol (SProxy)
import Network.Ethereum.Web3 (Vector, call, sendTx)
import Network.Ethereum.Web3.Contract.Internal (uncurryFields)
import Network.Ethereum.Web3.Solidity (D0, D1, D2, D3, D4, D5, D6, D7, D8, DOne, Tuple0(..), Tuple1(..), UIntN, unTuple1)
import Network.Ethereum.Web3.Solidity.Size (type (:&))
import Network.Ethereum.Web3.Types (Address, CallError, ChainCursor, HexString, NoPay, TransactionOptions, Web3)
--------------------------------------------------------------------------------
-- | AdoptFn
--------------------------------------------------------------------------------


type AdoptFn = Tagged (SProxy "adopt(uint256)") (Tuple1 (Tagged (SProxy "petId") (UIntN (D2 :& D5 :& DOne D6))))

adopt :: TransactionOptions NoPay -> { petId :: (UIntN (D2 :& D5 :& DOne D6)) } -> Web3 HexString
adopt x0 r = uncurryFields  r $ adopt' x0
   where
    adopt' :: TransactionOptions NoPay -> (Tagged (SProxy "petId") (UIntN (D2 :& D5 :& DOne D6))) -> Web3 HexString
    adopt' y0 y1 = sendTx y0 ((tagged $ Tuple1 y1) :: AdoptFn)

--------------------------------------------------------------------------------
-- | AdoptersFn
--------------------------------------------------------------------------------


type AdoptersFn = Tagged (SProxy "adopters(uint256)") (Tuple1 (UIntN (D2 :& D5 :& DOne D6)))

adopters :: TransactionOptions NoPay -> ChainCursor -> (UIntN (D2 :& D5 :& DOne D6)) -> Web3 (Either CallError Address)
adopters x0 cm x2 = map unTuple1 <$> call x0 cm ((tagged $ Tuple1 x2) :: AdoptersFn)

--------------------------------------------------------------------------------
-- | GetAdoptersManyDigitFn
--------------------------------------------------------------------------------


type GetAdoptersManyDigitFn = Tagged (SProxy "getAdoptersManyDigit()") (Tuple0 )

getAdoptersManyDigit :: TransactionOptions NoPay -> ChainCursor -> Web3 (Either CallError (Vector (D2 :& D1 :& D4 :& D7 :& D4 :& D8 :& D3 :& D6 :& D4 :& DOne D7) Address))
getAdoptersManyDigit x0 cm = map unTuple1 <$> call x0 cm ((tagged $ Tuple0 ) :: GetAdoptersManyDigitFn)

--------------------------------------------------------------------------------
-- | GetAdoptersOneDigitFn
--------------------------------------------------------------------------------


type GetAdoptersOneDigitFn = Tagged (SProxy "getAdoptersOneDigit()") (Tuple0 )

getAdoptersOneDigit :: TransactionOptions NoPay -> ChainCursor -> Web3 (Either CallError (Vector (DOne D1) Address))
getAdoptersOneDigit x0 cm = map unTuple1 <$> call x0 cm ((tagged $ Tuple0 ) :: GetAdoptersOneDigitFn)

--------------------------------------------------------------------------------
-- | GetAdoptersTwoDigitFn
--------------------------------------------------------------------------------


type GetAdoptersTwoDigitFn = Tagged (SProxy "getAdoptersTwoDigit()") (Tuple0 )

getAdoptersTwoDigit :: TransactionOptions NoPay -> ChainCursor -> Web3 (Either CallError (Vector (D1 :& DOne D0) Address))
getAdoptersTwoDigit x0 cm = map unTuple1 <$> call x0 cm ((tagged $ Tuple0 ) :: GetAdoptersTwoDigitFn)
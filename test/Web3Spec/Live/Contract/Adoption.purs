--------------------------------------------------------------------------------
-- | Adoption
--------------------------------------------------------------------------------

module Contract.Adoption where

import Prelude 

import Data.Either (Either)
import Data.Functor.Tagged (Tagged, tagged)
import Data.Symbol (SProxy)
import Network.Ethereum.Web3 (Vector, call)
import Network.Ethereum.Web3.Solidity (D0, D1, D2, D3, D4, D5, D6, DOne, Tuple0(..), Tuple1(..), UIntN, unTuple1)
import Network.Ethereum.Web3.Solidity.Size (type (:&))
import Network.Ethereum.Web3.Types (Address, CallError, ChainCursor, NoPay, TransactionOptions, Web3)
--------------------------------------------------------------------------------
-- | Adopters1Fn
--------------------------------------------------------------------------------


type Adopters1Fn = Tagged (SProxy "adopters1(uint256)") (Tuple1 (UIntN (D2 :& D5 :& DOne D6)))

adopters1 :: TransactionOptions NoPay -> ChainCursor -> (UIntN (D2 :& D5 :& DOne D6)) -> Web3 (Either CallError Address)
adopters1 x0 cm x2 = map unTuple1 <$> call x0 cm ((tagged $ Tuple1 x2) :: Adopters1Fn)

--------------------------------------------------------------------------------
-- | Adopters2Fn
--------------------------------------------------------------------------------


type Adopters2Fn = Tagged (SProxy "adopters2(uint256)") (Tuple1 (UIntN (D2 :& D5 :& DOne D6)))

adopters2 :: TransactionOptions NoPay -> ChainCursor -> (UIntN (D2 :& D5 :& DOne D6)) -> Web3 (Either CallError Address)
adopters2 x0 cm x2 = map unTuple1 <$> call x0 cm ((tagged $ Tuple1 x2) :: Adopters2Fn)

--------------------------------------------------------------------------------
-- | Adopters3Fn
--------------------------------------------------------------------------------


type Adopters3Fn = Tagged (SProxy "adopters3(uint256)") (Tuple1 (UIntN (D2 :& D5 :& DOne D6)))

adopters3 :: TransactionOptions NoPay -> ChainCursor -> (UIntN (D2 :& D5 :& DOne D6)) -> Web3 (Either CallError Address)
adopters3 x0 cm x2 = map unTuple1 <$> call x0 cm ((tagged $ Tuple1 x2) :: Adopters3Fn)

--------------------------------------------------------------------------------
-- | GetAdoptersManyDigitFn
--------------------------------------------------------------------------------


type GetAdoptersManyDigitFn = Tagged (SProxy "getAdoptersManyDigit()") (Tuple0 )

getAdoptersManyDigit :: TransactionOptions NoPay -> ChainCursor -> Web3 (Either CallError (Vector (D1 :& D2 :& D3 :& D4 :& D5 :& DOne D6) Address))
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
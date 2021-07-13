--------------------------------------------------------------------------------
-- | SimpleErrorTest
--------------------------------------------------------------------------------

module Web3Spec.Live.Contract.SimpleErrorTest where

import Prelude 

import Data.Either (Either)
import Data.Functor.Tagged (Tagged, tagged)
import Network.Ethereum.Web3 (call)
import Network.Ethereum.Web3.Contract.Internal (uncurryFields)
import Network.Ethereum.Web3.Solidity (D2, D5, D6, DOne, Tuple1(..), UIntN, unTuple1)
import Network.Ethereum.Web3.Solidity.Size (type (:&))
import Network.Ethereum.Web3.Types (CallError, ChainCursor, NoPay, TransactionOptions, Web3)
import Type.Proxy (Proxy)
--------------------------------------------------------------------------------
-- | NamesFn
--------------------------------------------------------------------------------


type NamesFn = Tagged (Proxy "names(uint256)") (Tuple1 (UIntN (D2 :& D5 :& DOne D6)))

names :: TransactionOptions NoPay -> ChainCursor -> (UIntN (D2 :& D5 :& DOne D6)) -> Web3 (Either CallError String)
names x0 cm x2 = map unTuple1 <$> call x0 cm ((tagged $ Tuple1 x2) :: NamesFn)

--------------------------------------------------------------------------------
-- | TableFn
--------------------------------------------------------------------------------


type TableFn = Tagged (Proxy "table(uint256)") (Tuple1 (UIntN (D2 :& D5 :& DOne D6)))

table :: TransactionOptions NoPay -> ChainCursor -> (UIntN (D2 :& D5 :& DOne D6)) -> Web3 (Either CallError String)
table x0 cm x2 = map unTuple1 <$> call x0 cm ((tagged $ Tuple1 x2) :: TableFn)

--------------------------------------------------------------------------------
-- | TestBoolFn
--------------------------------------------------------------------------------


type TestBoolFn = Tagged (Proxy "testBool(bool)") (Tuple1 (Tagged (Proxy "_arg") Boolean))

testBool :: TransactionOptions NoPay -> ChainCursor -> { _arg :: Boolean } -> Web3 (Either CallError Boolean)
testBool x0 cm r = uncurryFields  r $ testBool' x0 cm
   where
    testBool' :: TransactionOptions NoPay -> ChainCursor -> (Tagged (Proxy "_arg") Boolean) -> Web3 (Either CallError Boolean)
    testBool' y0 cm' y2 = map unTuple1 <$> call y0 cm' ((tagged $ Tuple1 y2) :: TestBoolFn)
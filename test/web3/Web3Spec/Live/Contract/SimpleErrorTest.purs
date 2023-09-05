module Web3Spec.Live.Contract.SimpleErrorTest where

import Prelude

import Data.Either (Either)
import Data.Functor.Tagged (Tagged, tagged)
import Network.Ethereum.Web3 (call)
import Network.Ethereum.Web3.Contract.Internal (uncurryFields)
import Network.Ethereum.Web3.Solidity (D2, D5, D6, DOne, UIntN, Tuple1(..), unTuple1)
import Network.Ethereum.Web3.Solidity.Size (type (:&))
import Network.Ethereum.Web3.Types (CallError, ChainCursor, NoPay, TransactionOptions, Web3)
import Type.Proxy (Proxy)

type NamesFn = Tagged (Proxy "names(uint256)") (Tuple1 (UIntN (D2 :& D5 :& DOne D6)))

names
  :: TransactionOptions NoPay
  -> ChainCursor
  -> UIntN (D2 :& D5 :& DOne D6)
  -> Web3 (Either CallError String)
names x1 x2 x3 = names' x1 x2 x3
  where
  names'
    :: TransactionOptions NoPay
    -> ChainCursor
    -> UIntN (D2 :& D5 :& DOne D6)
    -> Web3 (Either CallError String)
  names' _x1 _x2 _x3 = map unTuple1 <$> call _x1 _x2 (tagged $ Tuple1 _x3 :: NamesFn)

type TableFn = Tagged (Proxy "table(uint256)") (Tuple1 (UIntN (D2 :& D5 :& DOne D6)))

table
  :: TransactionOptions NoPay
  -> ChainCursor
  -> UIntN (D2 :& D5 :& DOne D6)
  -> Web3 (Either CallError String)
table x1 x2 x3 = table' x1 x2 x3
  where
  table'
    :: TransactionOptions NoPay
    -> ChainCursor
    -> UIntN (D2 :& D5 :& DOne D6)
    -> Web3 (Either CallError String)
  table' _x1 _x2 _x3 = map unTuple1 <$> call _x1 _x2 (tagged $ Tuple1 _x3 :: TableFn)

type TestBoolFn = Tagged (Proxy "testBool(bool)") (Tuple1 (Tagged (Proxy "_arg") Boolean))

testBool
  :: TransactionOptions NoPay
  -> ChainCursor
  -> { _arg :: Boolean }
  -> Web3 (Either CallError Boolean)
testBool x1 x2 x3 = uncurryFields x3 $ testBool' x1 x2
  where
  testBool'
    :: TransactionOptions NoPay
    -> ChainCursor
    -> Tagged (Proxy "_arg") Boolean
    -> Web3 (Either CallError Boolean)
  testBool' _x1 _x2 _x3 = map unTuple1 <$> call _x1 _x2 (tagged $ Tuple1 _x3 :: TestBoolFn)

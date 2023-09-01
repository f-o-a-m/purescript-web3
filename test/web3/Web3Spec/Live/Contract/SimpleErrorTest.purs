module Web3Spec.Live.Contract.SimpleErrorTest where

import Prelude

import Data.Either (Either)
import Data.Functor.Tagged (Tagged, tagged)
import Network.Ethereum.Web3 (call)
import Network.Ethereum.Web3.Contract.Internal (uncurryFields)
import Network.Ethereum.Web3.Solidity (UIntN, Tuple1(..), unTuple1)
import Network.Ethereum.Web3.Types (CallError, ChainCursor, NoPay, TransactionOptions, Web3)

type NamesFn = Tagged "names(uint256)" (Tuple1 (UIntN 256))

names :: TransactionOptions NoPay -> ChainCursor -> UIntN 256 -> Web3 (Either CallError String)
names x1 x2 x3 = names' x1 x2 x3
  where
  names' :: TransactionOptions NoPay -> ChainCursor -> UIntN 256 -> Web3 (Either CallError String)
  names' _x1 _x2 _x3 = map unTuple1 <$> call _x1 _x2 (tagged $ Tuple1 _x3 :: NamesFn)

type TableFn = Tagged "table(uint256)" (Tuple1 (UIntN 256))

table :: TransactionOptions NoPay -> ChainCursor -> UIntN 256 -> Web3 (Either CallError String)
table x1 x2 x3 = table' x1 x2 x3
  where
  table' :: TransactionOptions NoPay -> ChainCursor -> UIntN 256 -> Web3 (Either CallError String)
  table' _x1 _x2 _x3 = map unTuple1 <$> call _x1 _x2 (tagged $ Tuple1 _x3 :: TableFn)

type TestBoolFn = Tagged "testBool(bool)" (Tuple1 (Tagged "_arg" Boolean))

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
    -> Tagged "_arg" Boolean
    -> Web3 (Either CallError Boolean)
  testBool' _x1 _x2 _x3 = map unTuple1 <$> call _x1 _x2 (tagged $ Tuple1 _x3 :: TestBoolFn)

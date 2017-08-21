module Network.Ethereum.Web3.Solidity.Size where

import Prelude
import Type.Proxy (Proxy(..))

--------------------------------------------------------------------------------
-- * Type level byte array lengths
--------------------------------------------------------------------------------

data D0
data D1
data D2
data D3
data D4
data D5
data D6
data D7
data D8
data D9

data NumCons a b
infix 6 type NumCons as :&

class KnownSize d where
  sizeVal :: Proxy d -> Int

instance sizeD0 :: KnownSize D0 where
  sizeVal _ = 0

instance sizeD1 :: KnownSize D1 where
  sizeVal _ = 1

instance sizeN2 :: KnownSize D2 where
  sizeVal _ = 2

instance sizeN3 :: KnownSize D3 where
  sizeVal _ = 3

instance sizeN4 :: KnownSize D4 where
  sizeVal _ = 4

instance sizeN5 :: KnownSize D5 where
  sizeVal _ = 5

instance sizeN6 :: KnownSize D6 where
  sizeVal _ = 6

instance sizeN7 :: KnownSize D7 where
  sizeVal _ = 7

instance sizeN8 :: KnownSize D8 where
  sizeVal _ = 8

instance sizeN9 :: KnownSize D9 where
  sizeVal _ = 9

instance sizeCons :: (KnownSize tens, KnownSize ones) => KnownSize (tens :& ones) where
  sizeVal _ = 10 * (sizeVal (Proxy :: Proxy tens)) + sizeVal (Proxy :: Proxy ones)

--------------------------------------------------------------------------------
-- | Naturals
--------------------------------------------------------------------------------

data Z

data S n

class Succ n m | n -> m, m -> n

instance inductiveSucc :: Succ (S n) n

class KnownNat n where
  natVal :: Proxy n -> Int

instance natZ :: KnownNat Z where
  natVal _ = 0

instance natInd :: KnownNat n => KnownNat (S n) where
  natVal _ = 1 + natVal (Proxy :: Proxy n)

type N0 = Z
type N1 = S N0
type N2 = S N1
type N3 = S N2
type N4 = S N3
type N5 = S N4
type N6 = S N5
type N7 = S N6
type N8 = S N7
type N9 = S N8

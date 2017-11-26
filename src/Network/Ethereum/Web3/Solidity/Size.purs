module Network.Ethereum.Web3.Solidity.Size where

import Prelude
import Type.Proxy (Proxy(..))
import Data.Int (pow)
import Data.String (length)

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
infixr 6 type NumCons as :&

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

instance sizeCons :: (KnownSize new, KnownSize old) => KnownSize (new :& old) where
  sizeVal _ = let currentPow = (length <<< show <<< sizeVal $ (Proxy :: Proxy old)) - 1
              in (10 `pow` (currentPow + 1)) * (sizeVal (Proxy :: Proxy new)) + sizeVal (Proxy :: Proxy old)

class KnownSize n <= IntSize n
instance intSize8 :: IntSize D8
instance intSize16 :: IntSize (D1 :& D6)
instance intSize24 :: IntSize (D2 :& D4)
instance intSize32 :: IntSize (D3 :& D2)
instance intSize40 :: IntSize (D4 :& D0)
instance intSize48 :: IntSize (D4 :& D8)
instance intSize56 :: IntSize (D5 :& D6)
instance intSize64 :: IntSize (D6 :& D4)
instance intSize72 :: IntSize (D7 :& D2)
instance intSize80 :: IntSize (D8 :& D0)
instance intSize88 :: IntSize (D8 :& D8)
instance intSize96 :: IntSize (D9 :& D6)
instance intSize104 :: IntSize (D1 :& D0 :& D4)
instance intSize112 :: IntSize (D1 :& D1 :& D2)
instance intSize120 :: IntSize (D1 :& D2 :& D0)
instance intSize128 :: IntSize (D1 :& D2 :& D8)
instance intSize136 :: IntSize (D1 :& D3 :& D6)
instance intSize144 :: IntSize (D1 :& D4 :& D4)
instance intSize152 :: IntSize (D1 :& D5 :& D2)
instance intSize160 :: IntSize (D1 :& D6 :& D0)
instance intSize168 :: IntSize (D1 :& D6 :& D8)
instance intSize176 :: IntSize (D1 :& D7 :& D6)
instance intSize184 :: IntSize (D1 :& D8 :& D4)
instance intSize192 :: IntSize (D1 :& D9 :& D2)
instance intSize200 :: IntSize (D2 :& D0 :& D0)
instance intSize208 :: IntSize (D2 :& D0 :& D8)
instance intSize216 :: IntSize (D2 :& D1 :& D6)
instance intSize224 :: IntSize (D2 :& D2 :& D4)
instance intSize232 :: IntSize (D2 :& D3 :& D2)
instance intSize240 :: IntSize (D2 :& D4 :& D0)
instance intSize248 :: IntSize (D2 :& D4 :& D8)
instance intSize256 :: IntSize (D2 :& D5 :& D6)

class KnownSize n <= ByteSize n
instance byteSize1 :: ByteSize D1
instance byteSize2 :: ByteSize D2
instance byteSize3 :: ByteSize D3
instance byteSize4 :: ByteSize D4
instance byteSize5 :: ByteSize D5
instance byteSize6 :: ByteSize D6
instance byteSize7 :: ByteSize D7
instance byteSize8 :: ByteSize D8
instance byteSize9 :: ByteSize D9
instance byteSize10 :: ByteSize (D1 :& D0)
instance byteSize11 :: ByteSize (D1 :& D1)
instance byteSize12 :: ByteSize (D1 :& D2)
instance byteSize13 :: ByteSize (D1 :& D3)
instance byteSize14 :: ByteSize (D1 :& D4)
instance byteSize15 :: ByteSize (D1 :& D5)
instance byteSize16 :: ByteSize (D1 :& D6)
instance byteSize17 :: ByteSize (D1 :& D7)
instance byteSize18 :: ByteSize (D1 :& D8)
instance byteSize19 :: ByteSize (D1 :& D9)
instance byteSize20 :: ByteSize (D2 :& D0)
instance byteSize21 :: ByteSize (D2 :& D1)
instance byteSize22 :: ByteSize (D2 :& D2)
instance byteSize23 :: ByteSize (D2 :& D3)
instance byteSize24 :: ByteSize (D2 :& D4)
instance byteSize25 :: ByteSize (D2 :& D5)
instance byteSize26 :: ByteSize (D2 :& D6)
instance byteSize27 :: ByteSize (D2 :& D7)
instance byteSize28 :: ByteSize (D2 :& D8)
instance byteSize29 :: ByteSize (D2 :& D9)
instance byteSize30 :: ByteSize (D3 :& D0)
instance byteSize31 :: ByteSize (D3 :& D1)
instance byteSize32 :: ByteSize (D3 :& D2)

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
type N10 = S N9
type N11 = S N10
type N12 = S N11
type N13 = S N12
type N14 = S N13
type N15 = S N14
type N16 = S N15

module Network.Ethereum.Web3.Solidity.Size where

import Prelude

import Data.Int (pow)

foreign import kind Digit
foreign import data D0 :: Digit
foreign import data D1 :: Digit
foreign import data D2 :: Digit
foreign import data D3 :: Digit
foreign import data D4 :: Digit
foreign import data D5 :: Digit
foreign import data D6 :: Digit
foreign import data D7 :: Digit
foreign import data D8 :: Digit
foreign import data D9 :: Digit

foreign import kind DigitList
foreign import data DCons ∷ Digit → DigitList → DigitList
foreign import data DNil ∷ DigitList

infixr 6 type DCons as :&

data DProxy (d :: Digit) = DProxy
data DLProxy (d :: DigitList) = DLProxy

class DigitCount (d :: DigitList) where
  digitCount :: DLProxy d -> Int

instance countBase :: DigitCount (a :& DNil) where
  digitCount _ = 1

instance countLoop :: DigitCount (b :& rest) => DigitCount (a :& b :& rest) where
  digitCount _ = digitCount (DLProxy :: DLProxy (b :& rest)) + 1

class KnownDigit (d :: Digit) where
  digitVal :: DProxy d -> Int

instance sizeD0 :: KnownDigit D0 where digitVal _ = 0
instance sizeD1 :: KnownDigit D1 where digitVal _ = 1
instance sizeN2 :: KnownDigit D2 where digitVal _ = 2
instance sizeN3 :: KnownDigit D3 where digitVal _ = 3
instance sizeN4 :: KnownDigit D4 where digitVal _ = 4
instance sizeN5 :: KnownDigit D5 where digitVal _ = 5
instance sizeN6 :: KnownDigit D6 where digitVal _ = 6
instance sizeN7 :: KnownDigit D7 where digitVal _ = 7
instance sizeN8 :: KnownDigit D8 where digitVal _ = 8
instance sizeN9 :: KnownDigit D9 where digitVal _ = 9

class KnownSize (d :: DigitList) where
  sizeVal :: DLProxy d -> Int

instance sizedg1 :: KnownDigit head => KnownSize (head :& DNil) where
  sizeVal p = digitVal (DProxy :: DProxy head)

instance sizedg2 :: 
  ( DigitCount (b :& rest)
  , KnownSize (b :& rest)
  , KnownDigit a
  ) => KnownSize (a :& b :& rest) where
  sizeVal _ =
    let
      currentPow = digitCount (DLProxy :: DLProxy (b :& rest))
      head = (10 `pow` currentPow) * (digitVal (DProxy :: DProxy a))
    in head + sizeVal (DLProxy :: DLProxy (b :& rest))


foreign import kind Carrying
foreign import data Carry :: Carrying
foreign import data NoCarry :: Carrying

data CProxy (c :: Carrying) = CProxy

class IncD
  (input :: Digit)
  (output :: Digit)
  (carryOut :: Carrying) | input -> output carryOut, output carryOut -> input

instance incD0 :: IncD D0 D1 NoCarry
instance incD1 :: IncD D1 D2 NoCarry
instance incD2 :: IncD D2 D3 NoCarry
instance incD3 :: IncD D3 D4 NoCarry
instance incD4 :: IncD D4 D5 NoCarry
instance incD5 :: IncD D5 D6 NoCarry
instance incD6 :: IncD D6 D7 NoCarry
instance incD7 :: IncD D7 D8 NoCarry
instance incD8 :: IncD D8 D9 NoCarry
instance incD9 :: IncD D9 D0 Carry

class OnCarrying
  (carry :: Carrying)
  (onCarry :: Type)
  (onNoCarry :: Type)
  output | carry onCarry onNoCarry -> output

instance onCarry :: OnCarrying Carry a b a
instance onNoCarry :: OnCarrying NoCarry a b b

class Inc (input :: DigitList) (output :: DigitList) | input -> output

instance inc :: 
  ( IncP a aInc carry
  , OnCarrying carry (DLProxy (D1 :& aInc)) (DLProxy aInc) (DLProxy pout)
  ) => Inc a out

class IncP
  (input :: DigitList)
  (output :: DigitList)
  (carryOut :: Carrying) | input -> output carryOut

instance incPNil1 ::
  ( IncD d dInc dcarry
  ) => IncP (d :& DNil) (dInc :& DNil) dcarry

instance incPLoop1 ::
  ( IncP (b :& rest) bRestIncOut bRestIncCarry
  , IncD a aInc aCarry
  , OnCarrying bRestIncCarry (CProxy aCarry) (CProxy NoCarry) (CProxy carryOut)
  , OnCarrying bRestIncCarry (DLProxy (aInc :& bRestIncOut)) (DLProxy (a :& bRestIncOut)) (DLProxy pOut)
  ) => IncP (a :& b :& rest) out carryOut

newtype Vector (n :: DigitList) a = Vector (Array a)

vCons :: forall a n nInc. Inc n nInc => a -> Vector n a -> Vector nInc a
vCons a (Vector as) = Vector ([a] <> as)

test1 :: Vector (D1 :& D0 :& DNil) Int
test1 = vCons 1 a
  where
  a :: Vector (D9 :& DNil) Int
  a = Vector []

test2 :: Vector (D1 :& D0 :& D0 :& D0 :& DNil) Int
test2 = vCons 1 a
  where
  a :: Vector (D9 :& D9 :& D9 :& DNil) Int
  a = Vector []

test3 :: Vector (D2 :& D0 :& D0 :& DNil) Int
test3 = vCons 1 a
  where
  a :: Vector (D1 :& D9 :& D9 :& DNil) Int
  a = Vector []

test4 :: Vector (D9 :& D9 :& DNil) Int
test4 = vCons 1 a
  where
  a :: Vector (D9 :& D8 :& DNil) Int
  a = Vector []


test5 :: Vector (D2 :& D0 :& D0 :& D0 :& D0 :& D0 :& D0 :& D0 :& D0 :& D0 :& D0 :& D0 :& D0 :& D0 :& D0 :& D0 :& D0 :& D0 :& DNil) Int
test5 = vCons 1 a
  where
  a :: Vector (D1 :& D9 :& D9 :& D9 :& D9 :& D9 :& D9 :& D9 :& D9 :& D9 :& D9 :& D9 :& D9 :& D9 :& D9 :& D9 :& D9 :& D9 :& DNil) Int
  a = Vector []


test6 :: Vector (D1 :& D9 :& D9 :& D9 :& D9 :& D9 :& D9 :& D9 :& D9 :& D9 :& D9 :& D9 :& D9 :& D9 :& D9 :& D9 :& D9 :& D9 :& DNil) Int
test6 = vCons 1 a
  where
  a :: Vector (D1 :& D9 :& D9 :& D9 :& D9 :& D9 :& D9 :& D9 :& D9 :& D9 :& D9 :& D9 :& D9 :& D9 :& D9 :& D9 :& D9 :& D8 :& DNil) Int
  a = Vector []

module Network.Ethereum.Web3.Solidity.Size
  ( D0
  , D1
  , D2
  , D3
  , D4
  , D5
  , D6
  , D7
  , D8
  , D9
  , type (:&)
  , type (:%)
  , DOne
  , DCons
  , class KnownSize
  , sizeVal
  , DLProxy(..)
  , class IntSize
  , class ByteSize
  , class Inc
  -- more low level staff
  , Digit
  , DProxy(..)
  , DigitList
  , DTwo
  , class IncP
  , class IncD
  , class KnownDigit
  , digitVal
  , class DigitCount
  , digitCount
  ) where

import Prelude
import Data.Int (pow)
import Type.Data.Boolean (class If, BProxy, False, True)

-- | `Digit` is a new `Kind` used to represent digits in base 10 counting system.
-- | Alongside this kind we have types `D0`, `D1` ... `D9`, which have kind `Digit`.
-- | This two parts conceptually translate to this sum type:
-- | We could represent all digits used in base 10 counting counting system like this:
-- |
-- | ``` purescript
-- | data Digit = D0 | D1 | D2 | D3 | D4 | D5 | D6 | D7 | D8 | D9
-- | ```
-- |
-- | here `Digit` has kind `Type` and D0, D1 ... are values of type `Digit`,
-- | to be able to represent this digits in type-level we need to have distinct
-- | types for each such value when we just have one Digit for all of them.
-- | We could create empty types for each digit like `data Void`.
-- | But they will have kind `Type` and it's not good as one could
-- | use them incorrectly. for example this will compile:
-- |
-- | ``` purescript
-- | data D1
-- | data D2
-- | ....
-- | data D9
-- |
-- | x :: Array D1
-- | x = []
-- | ```
-- |
-- | It compiles as Array kas kind `Type -> Type`, i.e. it's type constructor,
-- | it takes a `Type` and returns `Type`. We want such program to be rejected by compiler,
-- | so to be more type-safe, we create new kind `Digit`, and our types representing digits,
-- | will be of this kind instead of "default" kind `Type`.
-- |
-- | ``` purescript
-- | foreign import kind Digit
-- | foreign import data D0 :: Digit
-- | foreign import data D2 :: Digit
-- | ....
-- | foreign import data D3 :: Digit
-- | ```
-- |
-- | now you can't  have `x :: Array D3` will not compile at all.
data Digit

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

-- | As we have kind `Digit` we introduce kind DigitList for list of digits.
-- | in value level this could be represented as:
-- |
-- | ``` purescript
-- | data DigitList = DCons Digit DigitList | DOne Digit
-- | ```
-- |
-- | Note: you can't have empty list here as terminal node takes a `Digit`.
-- | the builtin `Array` in purescript could be defined like this
-- |
-- | ``` purescript
-- | foreign import data Array ∷ Type -> Type
-- | ```
-- |
-- | Similarly we are defining `DCons` and `DOne`, but with kinds we have defined already:
-- |
-- | ``` purescript
-- | foreign import data DCons ∷ Digit -> DigitList -> DigitList
-- | foreign import data DOne ∷ Digit -> DigitList
-- | ```
-- |
-- | Now we can represent number 1995 in type-level like this:
-- |
-- | ``` purescript
-- | type MyNum = DCons D1 (DCons D9 (DCons D9 (DOne D5)))
-- | ```
-- |
-- | Yes it's too verbose, that's why we have type-level operator `:&` for `DCons`
-- | like `:` for `Cons` of `List`.
-- |
-- | ``` purescript
-- | infixr 6 type DCons as :&
-- |
-- | type MyNum = D1 :& D9 :& D9 :& DOne D5
-- | ```
-- |
-- | the `_ :& DOne _` part is a bit annoying, so we fix it with a new type alisa for such
-- | types and an operator for it :%:
-- |
-- | ``` purescript
-- | type DTwo d1 d2 = d1 :& DOne d2
-- | infixr 6 type DTwo as :%
-- |
-- | type MyNum = D1 :& D9 :& D9 :% D5
-- | ```
-- |
-- | Nice and sweet.
-- | Note in typeclass instances type aliases can't be used, which `DTwo` and `:%` are.
-- | so this will not compile `instance myInstance :: SomeCls (D1 :% D0)`
-- | instead it should be written as `instance myInstance :: SomeCls (D1 :& DOne D0)`
data DigitList

foreign import data DCons ∷ Digit -> DigitList -> DigitList

foreign import data DOne ∷ Digit -> DigitList

type DTwo d1 d2
  = d1 :& DOne d2

infixr 6 type DTwo as :%

infixr 6 type DCons as :&

-- | For types of kind `Type` there is already [`Type.Proxy`](https://pursuit.purescript.org/packages/purescript-proxy/2.1.0/docs/Type.Proxy).
-- | this is basicity the same thing but for types of kind `DigitList`.
-- | Documentation of `Type.Proxy` module has motivation for why would
-- | one need a Proxy for some type which we will not cover here.
data DLProxy (d :: DigitList)
  = DLProxy

-- | Same as `DLProxy` but for types of kind `Digit`
data DProxy (d :: Digit)
  = DProxy

class DigitCount (d :: DigitList) where
  -- | Given proxy of a DigitList returns number of digits in the list.
  digitCount :: DLProxy d -> Int

instance countBase :: DigitCount (DOne a) where
  digitCount _ = 1

instance countLoop :: DigitCount rest => DigitCount (a :& rest) where
  digitCount _ = digitCount (DLProxy :: DLProxy rest) + 1

class KnownDigit (d :: Digit) where
  -- | Given proxy of a Digit returns a digit it represents
  digitVal :: DProxy d -> Int

instance sizeD0 :: KnownDigit D0 where
  digitVal _ = 0

instance sizeD1 :: KnownDigit D1 where
  digitVal _ = 1

instance sizeN2 :: KnownDigit D2 where
  digitVal _ = 2

instance sizeN3 :: KnownDigit D3 where
  digitVal _ = 3

instance sizeN4 :: KnownDigit D4 where
  digitVal _ = 4

instance sizeN5 :: KnownDigit D5 where
  digitVal _ = 5

instance sizeN6 :: KnownDigit D6 where
  digitVal _ = 6

instance sizeN7 :: KnownDigit D7 where
  digitVal _ = 7

instance sizeN8 :: KnownDigit D8 where
  digitVal _ = 8

instance sizeN9 :: KnownDigit D9 where
  digitVal _ = 9

class KnownSize (d :: DigitList) where
  -- | Given proxy of a Digit returns a number it represents
  -- |
  -- | ``` purescript
  -- | 1995 == sizeVal (DLProxy :: DLProxy (D1 :& D9 :& D9 :% D5))
  -- | ```
  sizeVal :: DLProxy d -> Int

instance knownSizeBase :: KnownDigit head => KnownSize (DOne head) where
  sizeVal p = digitVal (DProxy :: DProxy head)

instance knownSizeLoop ::
  ( DigitCount (rest)
  , KnownSize (rest)
  , KnownDigit a
  ) =>
  KnownSize (a :& rest) where
  sizeVal _ =
    let
      currentPow = digitCount (DLProxy :: DLProxy rest)

      head = (10 `pow` currentPow) * (digitVal (DProxy :: DProxy a))
    in
      head + sizeVal (DLProxy :: DLProxy rest)

class IncD (input :: Digit) (output :: Digit) (carry :: Boolean) | input -> output carry

instance incD0 :: IncD D0 D1 False

instance incD1 :: IncD D1 D2 False

instance incD2 :: IncD D2 D3 False

instance incD3 :: IncD D3 D4 False

instance incD4 :: IncD D4 D5 False

instance incD5 :: IncD D5 D6 False

instance incD6 :: IncD D6 D7 False

instance incD7 :: IncD D7 D8 False

instance incD8 :: IncD D8 D9 False

instance incD9 :: IncD D9 D0 True

-- | This is like `inc` but in type-level for DigitList, it computes
-- | increment of it's input. It could be used like this for example:
-- | ```purescript
-- | cons :: forall a n nInc. Inc n nInc => a -> Vector n a -> Vector nInc a
-- | uncons :: forall a n nDec. Inc nDec n => Vector n a -> { head :: a, tail :: Vector nDec a }
-- | ```
-- | see [Network.Ethereum.Web3.Solidity.Vector](/Network.Ethereum.Web3.Solidity.Vector#v:vCons)
--
class Inc (input :: DigitList) (output :: DigitList) | input -> output

instance inc ::
  ( IncP a aInc carry
  , If carry (DLProxy (D1 :& aInc)) (DLProxy aInc) (DLProxy out)
  ) =>
  Inc a out

class IncP (input :: DigitList) (output :: DigitList) (carry :: Boolean) | input -> output carry

instance incPNil1 ::
  ( IncD d dInc dCarry
    ) =>
  IncP (DOne d) (DOne dInc) dCarry

instance incPLoop1 ::
  ( IncP rest restIncOut restIncTrue
  , IncD a aInc aCarry
  , If restIncTrue (BProxy aCarry) (BProxy False) (BProxy carryOut)
  , If restIncTrue (DLProxy (aInc :& restIncOut)) (DLProxy (a :& restIncOut)) (DLProxy out)
  ) =>
  IncP (a :& rest) out carryOut

-- | `IntSize` is empty class, if there is instance of `IntSize` for some number it means there
-- | is solidity type `int` of that size specific number in like `int16`, `int24` ... `int256`
class KnownSize n <= IntSize n

instance intSize8 :: IntSize (DOne D8)

instance intSize16 :: IntSize (D1 :& DOne D6)

instance intSize24 :: IntSize (D2 :& DOne D4)

instance intSize32 :: IntSize (D3 :& DOne D2)

instance intSize40 :: IntSize (D4 :& DOne D0)

instance intSize48 :: IntSize (D4 :& DOne D8)

instance intSize56 :: IntSize (D5 :& DOne D6)

instance intSize64 :: IntSize (D6 :& DOne D4)

instance intSize72 :: IntSize (D7 :& DOne D2)

instance intSize80 :: IntSize (D8 :& DOne D0)

instance intSize88 :: IntSize (D8 :& DOne D8)

instance intSize96 :: IntSize (D9 :& DOne D6)

instance intSize104 :: IntSize (D1 :& D0 :& DOne D4)

instance intSize112 :: IntSize (D1 :& D1 :& DOne D2)

instance intSize120 :: IntSize (D1 :& D2 :& DOne D0)

instance intSize128 :: IntSize (D1 :& D2 :& DOne D8)

instance intSize136 :: IntSize (D1 :& D3 :& DOne D6)

instance intSize144 :: IntSize (D1 :& D4 :& DOne D4)

instance intSize152 :: IntSize (D1 :& D5 :& DOne D2)

instance intSize160 :: IntSize (D1 :& D6 :& DOne D0)

instance intSize168 :: IntSize (D1 :& D6 :& DOne D8)

instance intSize176 :: IntSize (D1 :& D7 :& DOne D6)

instance intSize184 :: IntSize (D1 :& D8 :& DOne D4)

instance intSize192 :: IntSize (D1 :& D9 :& DOne D2)

instance intSize200 :: IntSize (D2 :& D0 :& DOne D0)

instance intSize208 :: IntSize (D2 :& D0 :& DOne D8)

instance intSize216 :: IntSize (D2 :& D1 :& DOne D6)

instance intSize224 :: IntSize (D2 :& D2 :& DOne D4)

instance intSize232 :: IntSize (D2 :& D3 :& DOne D2)

instance intSize240 :: IntSize (D2 :& D4 :& DOne D0)

instance intSize248 :: IntSize (D2 :& D4 :& DOne D8)

instance intSize256 :: IntSize (D2 :& D5 :& DOne D6)

-- | `ByteSize` is empty class, if there is instance of `ByteSize` for some number it means there
-- | is solidity type `bytes` of that size specific number in like `bytes1`, `bytes2` ... `bytes32`
class KnownSize n <= ByteSize n

instance byteSize1 :: ByteSize (DOne D1)

instance byteSize2 :: ByteSize (DOne D2)

instance byteSize3 :: ByteSize (DOne D3)

instance byteSize4 :: ByteSize (DOne D4)

instance byteSize5 :: ByteSize (DOne D5)

instance byteSize6 :: ByteSize (DOne D6)

instance byteSize7 :: ByteSize (DOne D7)

instance byteSize8 :: ByteSize (DOne D8)

instance byteSize9 :: ByteSize (DOne D9)

instance byteSize10 :: ByteSize (D1 :& DOne D0)

instance byteSize11 :: ByteSize (D1 :& DOne D1)

instance byteSize12 :: ByteSize (D1 :& DOne D2)

instance byteSize13 :: ByteSize (D1 :& DOne D3)

instance byteSize14 :: ByteSize (D1 :& DOne D4)

instance byteSize15 :: ByteSize (D1 :& DOne D5)

instance byteSize16 :: ByteSize (D1 :& DOne D6)

instance byteSize17 :: ByteSize (D1 :& DOne D7)

instance byteSize18 :: ByteSize (D1 :& DOne D8)

instance byteSize19 :: ByteSize (D1 :& DOne D9)

instance byteSize20 :: ByteSize (D2 :& DOne D0)

instance byteSize21 :: ByteSize (D2 :& DOne D1)

instance byteSize22 :: ByteSize (D2 :& DOne D2)

instance byteSize23 :: ByteSize (D2 :& DOne D3)

instance byteSize24 :: ByteSize (D2 :& DOne D4)

instance byteSize25 :: ByteSize (D2 :& DOne D5)

instance byteSize26 :: ByteSize (D2 :& DOne D6)

instance byteSize27 :: ByteSize (D2 :& DOne D7)

instance byteSize28 :: ByteSize (D2 :& DOne D8)

instance byteSize29 :: ByteSize (D2 :& DOne D9)

instance byteSize30 :: ByteSize (D3 :& DOne D0)

instance byteSize31 :: ByteSize (D3 :& DOne D1)

instance byteSize32 :: ByteSize (D3 :& DOne D2)

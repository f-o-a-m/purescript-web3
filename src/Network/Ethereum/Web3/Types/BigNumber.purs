module Network.Ethereum.Web3.Types.BigNumber
  ( BigNumber
  , class Algebra, embed
  , (*<), rmul
  , (>*), lmul
  , (+<), radd
  , (>+), ladd
  , (-<), rsub
  , (>-), lsub
  , pow
  , toString
  , parseBigNumber
  , toTwosComplement
  , toInt
  , module Int
  ) where

import Prelude
import Data.Int (Radix, binary, decimal, hexadecimal, floor) as Int
import Data.Maybe (Maybe(..))
import Data.Foreign (Foreign)
import Data.Foreign.Class (class Decode, class Encode, encode)

--------------------------------------------------------------------------------
-- * BigNumber
--------------------------------------------------------------------------------

foreign import data BigNumber :: Type

foreign import toString :: Int.Radix -> BigNumber -> String

instance showBigNumber :: Show BigNumber where
  show = toString Int.decimal

foreign import _eqBigNumber :: BigNumber -> BigNumber -> Boolean

instance eqBigNumber :: Eq BigNumber where
  eq = _eqBigNumber

foreign import comparedTo :: BigNumber -> BigNumber -> Int

instance ordBigNumber :: Ord BigNumber where
  compare bn1 bn2 =
    let n = comparedTo bn1 bn2
    in case n of
         0 -> EQ
         1 -> GT
         _ -> LT

foreign import _addBigNumber :: BigNumber -> BigNumber -> BigNumber

foreign import _mulBigNumber :: BigNumber -> BigNumber -> BigNumber

foreign import _intToBigNumber :: Int -> BigNumber

embedInt :: Int -> BigNumber
embedInt = _intToBigNumber

foreign import _numberToBigNumber :: Number -> BigNumber

instance semiringBigNumber :: Semiring BigNumber where
  add = _addBigNumber
  mul = _mulBigNumber
  zero = embedInt 0
  one = embedInt 1

foreign import _subBigNumber :: BigNumber -> BigNumber -> BigNumber

instance ringBigNumber :: Ring BigNumber where
  sub = _subBigNumber

class (Ring r, Ring a) <= Algebra r a where
  embed :: r -> a

instance embedInt' :: Algebra Int BigNumber where
  embed = embedInt

instance embedNumber :: Algebra Number BigNumber where
  embed = _numberToBigNumber

instance embedBigNumber :: Algebra BigNumber BigNumber where
  embed = id

radd :: forall r a . Algebra r a => a -> r -> a
radd a r = a `add` embed r

ladd :: forall r a . Algebra r a => r -> a -> a
ladd r a = embed r `add` a

infixr 6 radd as +<
infixl 6 ladd as >+

rsub :: forall r a . Algebra r a => a -> r -> a
rsub a r = a `sub` embed r

lsub :: forall r a . Algebra r a => r -> a -> a
lsub r a = embed r `sub` a

infixr 6 rsub as -<
infixl 6 lsub as >-

rmul :: forall r a . Algebra r a => a -> r -> a
rmul a r = a `mul` embed r

lmul :: forall r a . Algebra r a => r -> a -> a
lmul r a = embed r `mul` a

infixr 7 rmul as *<
infixl 7 lmul as >*

foreign import reciprical :: BigNumber -> BigNumber

instance recipBigNumber :: DivisionRing BigNumber where
  recip = reciprical

foreign import fromStringAsImpl
  :: (forall a . a -> Maybe a)
  -> (forall a . Maybe a)
  -> Int.Radix
  -> String
  -> Maybe BigNumber

parseBigNumber :: Int.Radix -> String -> Maybe BigNumber
parseBigNumber = fromStringAsImpl Just Nothing

foreign import toTwosComplement :: BigNumber -> BigNumber

foreign import pow :: BigNumber -> Int -> BigNumber

foreign import toNumber :: BigNumber -> Number

toInt :: BigNumber -> Int
toInt = Int.floor <<< toNumber

foreign import toBigNumber :: Foreign -> BigNumber

instance decodeBigNumber :: Decode BigNumber where
  decode = pure <<< toBigNumber

instance encodeBigNumber :: Encode BigNumber where
  encode = encode <<< (append "0x") <<< toString Int.hexadecimal

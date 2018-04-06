module Network.Ethereum.Web3.Types.BigNumber
  ( BigNumber
  , class Algebra, embed
  , pow
  , toString
  , parseBigNumber
  , toTwosComplement
  , unsafeToInt
  , floorBigNumber
  , module Int
  ) where

import Prelude

import Data.Foreign (Foreign)
import Data.Foreign.Class (class Decode, class Encode, decode, encode)
import Data.Int (Radix, binary, decimal, hexadecimal, floor) as Int
import Data.Maybe (Maybe(..))
import Data.Module (class LeftModule, class RightModule)
import Simple.JSON (class ReadForeign)

--------------------------------------------------------------------------------
-- * BigNumber
--------------------------------------------------------------------------------

-- | Large Integer, needed for handling numbers of up to 32 bytes
foreign import data BigNumber :: Type

-- | Convert a Big number into a string in the given base
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

instance bigNumberLModule :: LeftModule BigNumber Int where
  mzeroL = embedInt 0
  maddL = add
  msubL = sub
  mmulL a b = embedInt a * b

instance bigNumberRModule :: RightModule BigNumber Int where
  mzeroR = embedInt 0
  maddR = add
  msubR = sub
  mmulR a b = a * embedInt b

class (Ring r, Ring a, LeftModule a r, RightModule a r) <= Algebra a r where
  embed :: r -> a

instance embedInt' :: Algebra BigNumber Int where
  embed = embedInt

foreign import reciprical :: BigNumber -> BigNumber

instance recipBigNumber :: DivisionRing BigNumber where
  recip = reciprical

foreign import fromStringAsImpl
  :: (forall a . a -> Maybe a)
  -> (forall a . Maybe a)
  -> Int.Radix
  -> String
  -> Maybe BigNumber


-- | Convert a string in the given base to a `BigNumber`
parseBigNumber :: Int.Radix -> String -> Maybe BigNumber
parseBigNumber = fromStringAsImpl Just Nothing

-- | Take the twos complement of a `BigNumer`
foreign import toTwosComplement :: BigNumber -> BigNumber

-- | Exponentiate a `BigNumber`
foreign import pow :: BigNumber -> Int -> BigNumber

foreign import toNumber :: BigNumber -> Number

-- | Unsafely coerce a BigNumber to an Int.
unsafeToInt :: BigNumber -> Int
unsafeToInt = Int.floor <<< toNumber

-- | Take the integer part of a big number
foreign import floorBigNumber :: BigNumber -> BigNumber

foreign import toBigNumber :: Foreign -> BigNumber

instance decodeBigNumber :: Decode BigNumber where
  decode = pure <<< toBigNumber

instance readFBigNumber :: ReadForeign BigNumber where
  readImpl = decode

instance encodeBigNumber :: Encode BigNumber where
  encode = encode <<< (append "0x") <<< toString Int.hexadecimal

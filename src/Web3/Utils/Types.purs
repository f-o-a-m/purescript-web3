module Web3.Utils.Types where

import Data.Ring
import Prelude

import Data.List (List)

--------------------------------------------------------------------------------
-- * HexString
--------------------------------------------------------------------------------

newtype HexString = HexString String

instance showHexString :: Show HexString where
  show (HexString hx) = "0x" <> hx

derive newtype instance hexStringEq :: Eq HexString

--------------------------------------------------------------------------------
-- * BigNumber
--------------------------------------------------------------------------------

foreign import data BigNumber :: Type

foreign import _showBigNumber :: BigNumber -> String

foreign import _eqBigNumber :: BigNumber -> BigNumber -> Boolean

instance showBigNumber :: Show BigNumber where
  show = _showBigNumber

instance eqBigNumber :: Eq BigNumber where
  eq = _eqBigNumber

foreign import _addBigNumber :: BigNumber -> BigNumber -> BigNumber

foreign import _mulBigNumber :: BigNumber -> BigNumber -> BigNumber

foreign import _zero :: BigNumber

foreign import _one :: BigNumber



instance semiringBigNumber :: Semiring BigNumber where
  add = _addBigNumber
  mul = _mulBigNumber
  zero = _zero
  one = _one

foreign import _subBigNumber :: BigNumber -> BigNumber -> BigNumber

instance ringBigNumber :: Ring BigNumber where
  sub = _subBigNumber

foreign import _intToBigNumber :: Int -> BigNumber

class RModule r m where
  rmul :: r -> m -> m
  lmul :: m -> r -> m

infixr 5 rmul as *<
infixl 5 append as >*

embedInt :: Int -> BigNumber
embedInt = _intToBigNumber

instance rmoduleIntBigNumber :: RModule Int BigNumber where
  rmul r m = (embedInt r) * m
  lmul m r = m * (embedInt r)

--------------------------------------------------------------------------------
-- * Decimal
--------------------------------------------------------------------------------

newtype Decimal = Decimal String

derive newtype instance showDecimal :: Show Decimal
derive newtype instance eqDecimal :: Eq Decimal

class IsDecimal a where
  toDecimal :: a -> Decimal
  fromDecimal :: Decimal -> a

--------------------------------------------------------------------------------
-- * Contract Interface and Event Description
--------------------------------------------------------------------------------

data AbiElement = FunctionType | Event

type FunctionType = { type_ :: FunctionClass
                    , name :: String
                    , inputs :: List FunctionInputs
                    , constant :: Boolean
                    , payable :: Boolean
                    }

data FunctionClass =
    Function
  | Constructor
  | Fallback

type FunctionInputs = { name :: String
                      , type_ :: String
                      }

type Event = { name :: String
             , inputs :: List EventInputs
             , anonymous :: Boolean
             }

type EventInputs = { name :: String
                   , type_ :: String
                   , indexed :: Boolean
                   }

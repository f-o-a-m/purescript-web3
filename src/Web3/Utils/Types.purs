module Web3.Utils.Types where

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

foreign import numberToBigNumber :: Number -> BigNumber

foreign import _hexStringToBigNumber :: String -> BigNumber

hexStringToBigNumber :: HexString -> BigNumber
hexStringToBigNumber (HexString hx) = _hexStringToBigNumber hx

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

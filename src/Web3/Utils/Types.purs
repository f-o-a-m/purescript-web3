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

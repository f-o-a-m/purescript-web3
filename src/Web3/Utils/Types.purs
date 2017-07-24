module Web3.Utils.Types where

import Prelude
import Data.Monoid (class Monoid)
import Data.Array (all ,elem)
import Data.ByteString (ByteString, Encoding(Hex))
import Data.ByteString as BS
import Data.List (List)
import Data.Maybe (Maybe(..))
import Data.String (toCharArray, stripPrefix, Pattern(..))
import Data.String (length) as S
import Data.Foreign.Class (class Decode, class Encode, decode, encode)

--------------------------------------------------------------------------------
-- * Signed Values
--------------------------------------------------------------------------------

data Sign = Pos | Neg

derive instance eqSign :: Eq Sign

data Signed a = Signed Sign a

instance showSigned :: Show a => Show (Signed a) where
  show (Signed s a) = show s' <> show a
    where
      s' = case s of
        Pos -> ""
        Neg -> "-"

instance eqSigned :: Eq a => Eq (Signed a) where
  eq (Signed s a) (Signed s' a') = (s `eq` s') && (a `eq` a')

instance mapSigned :: Functor Signed where
  map f (Signed s a) = Signed s (f a)

dropSign :: forall a . Signed a -> a
dropSign (Signed _ a) = a

asSigned :: forall a . a -> Signed a
asSigned a = Signed Pos a

--------------------------------------------------------------------------------
-- * HexString
--------------------------------------------------------------------------------

newtype HexString = HexString String

instance showHexString :: Show HexString where
  show (HexString hx) = "0x" <> hx

derive newtype instance hexStringEq :: Eq HexString

derive newtype instance semigpStringEq :: Semigroup HexString

derive newtype instance monoidStringEq :: Monoid HexString

instance decodeHexString :: Decode HexString where
  decode s = do
    str <- decode s
    case stripPrefix (Pattern "0x") str of
      Nothing -> pure <<< HexString $ str
      Just res -> pure <<< HexString $ res

instance encodeHexString :: Encode HexString where
  encode = encode <<< unHex

fromString :: String -> Maybe HexString
fromString s =
    let res = all go <<< toCharArray $ s
    in if res then Just <<< HexString $ s else Nothing
  where
    go :: Char -> Boolean
    go c = c `elem` ['0','1','2','3','4','5','6','7','8','9','a','b','c','d','e','f']

unHex :: HexString -> String
unHex (HexString hx) = hx

pack :: HexString -> ByteString
pack (HexString hx) = BS.fromString hx Hex

length :: HexString -> Int
length (HexString hx) = S.length hx

--------------------------------------------------------------------------------
-- * Addresses
--------------------------------------------------------------------------------

newtype Address = Address HexString

derive newtype instance showAddress :: Show Address

derive newtype instance eqAddress :: Eq Address

derive newtype instance decodeAddress :: Decode Address

derive newtype instance encodeAddress :: Encode Address

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

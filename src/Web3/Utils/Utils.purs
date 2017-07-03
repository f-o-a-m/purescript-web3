module Web3.Utils.Utils
  ( EtherUnit(..)
  , padLeft
  , padRight
  , toUtf8
  , fromUtf8
  , toAscii
  , fromAscii
  , transformToFullName
  , extractDisplayName
  , extractTypeName
  , toWei
  , fromWei
  ) where

import Control.Applicative ((*>), pure)
import Control.Fold (mconcat, foldl)
import Control.Monad (bind)
import Data.Array (unsafeIndex, many, fromFoldable)
import Data.ByteString (toString, fromString) as BS
import Data.Either (Either)
import Data.List (List)
import Data.Maybe (Maybe(..), fromJust)
import Data.String (Pattern(..), split, length, indexOf, take, joinWith, fromCharArray)
import Node.Encoding (Encoding(Hex, UTF8, ASCII))
import Partial.Unsafe (unsafePartial)
import Prelude (flip, map, ($), (-), (<<<), (<=), (<>), (<$>), (*), recip)
import Text.Parsing.Parser (Parser, ParseError, runParser)
import Text.Parsing.Parser.Combinators (skipMany, sepBy, between)
import Text.Parsing.Parser.String (char, skipSpaces)
import Text.Parsing.Parser.Token (alphaNum)

import Web3.Utils.Types (HexString(..))
import Web3.Utils.BigNumber (BigNumber, decimal)
import Web3.Utils.BigNumber (fromString) as BN

data EtherUnit =
    Wei
  | KWei
  | MWei
  | GWei
  | Szabo
  | Finney
  | Ether
  | MEther
  | GEther
  | TEther

-- | Takes a unit to convert from to get the value in Wei
toWeiRate :: EtherUnit -> BigNumber
toWeiRate eu =
  let rate = case eu of
               Wei    -> "1"
               KWei   -> "1000"
               MWei   -> "1000000"
               GWei   -> "1000000000"
               Szabo  -> "1000000000000"
               Finney -> "1000000000000000"
               Ether  -> "100000000000000000"
               MEther -> "1000000000000000000000"
               GEther -> "1000000000000000000000000"
               TEther -> "1000000000000000000000000000"
  in unsafePartial $ fromJust <<< BN.fromString decimal $ rate

-- | Convert 'val' many of 'eu' to its value in Wei
toWei :: BigNumber -> EtherUnit -> BigNumber
toWei val eu =
  let rate = toWeiRate eu
  in val * rate

-- | Convert 'val many' Wei to get its value in 'eu'.
fromWei :: BigNumber -> EtherUnit -> BigNumber
fromWei val eu =
  let rate = recip $ toWeiRate eu
  in val * rate

-- | Pad a 'HexString' with '0's on the left until it has the
-- desired length.
padLeft :: HexString -> Int -> HexString
padLeft a@(HexString hx) desiredLength =
    let padLength = desiredLength - length hx
    in if padLength <= 0 then a else HexString $ go hx padLength
  where
    go s 0 = s
    go s n = go ("0" <> s) (n - 1)

-- | Pad a 'HexString' with '0's on the right until it has the
-- desired length.
padRight :: HexString -> Int -> HexString
padRight a@(HexString hx) desiredLength =
    let padLength = desiredLength - length hx
    in if padLength <= 0 then a else HexString $ go hx padLength
  where
    go s 0 = s
    go s n = go (s <> "0") (n - 1)

-- | Takes a hex string and produces the corresponding UTF8-decoded string.
-- This breaks at the first null octet, following the web3 function 'toUft8'.
-- Since 'split' always returns a nonempty list, this index is actually safe.
toUtf8 :: HexString -> String
toUtf8 (HexString hx) =
  let hx' = unsafePartial $ split (Pattern "00") hx `unsafeIndex` 0
  in flip BS.toString UTF8 $ BS.fromString hx' Hex

-- | Takes a hex string and produces the corresponding ASCII decoded string.
toAscii :: HexString -> String
toAscii (HexString hx) = flip BS.toString ASCII $ BS.fromString hx Hex

-- | Get the 'HexString' corresponding to the UTF8 encoding.
fromUtf8 :: String -> HexString
fromUtf8 s =
  let s' = unsafePartial $ split (Pattern "\0000") s `unsafeIndex` 0
  in HexString <<< flip BS.toString Hex <<< flip BS.fromString UTF8 $ s'

-- | Get the 'HexString' corresponding to the ASCII encoding.
fromAscii :: String -> HexString
fromAscii = HexString <<< flip BS.toString Hex <<< flip BS.fromString ASCII

--  | Should be used to create full function/event name from json abi
transformToFullName :: forall r s . { name :: String , inputs :: List { type_ :: String | r } | s } -> String
transformToFullName a = case indexOf (Pattern "(") a.name of
  Nothing -> a.name
  Just _ -> let is = a.inputs
                typeName = foldl mconcat $ map (\i -> i.type_) is
            in "(" <> typeName <> ")"

-- | Should be called to get display name of contract function.
extractDisplayName :: String -> String
extractDisplayName a = case indexOf (Pattern "(") a of
  Nothing -> a
  Just n -> take n a

-- | Returns overloaded part of function/event name
extractTypeName :: String -> Either ParseError String
extractTypeName a = runParser a extractTypeNameParser

extractTypeNameParser :: Parser String String
extractTypeNameParser = do
    _ <- skipMany alphaNum
    types <- between (char '(') (char ')') typesParser
    pure <<< joinWith "," <<< fromFoldable $ types
  where
    typesParser :: Parser String (List String)
    typesParser = sepBy typeParser $ char ','
    typeParser :: Parser String String
    typeParser = skipSpaces *> (fromCharArray <$> many alphaNum)

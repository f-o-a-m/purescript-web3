module Network.Ethereum.Web3.Encoding.Internal
  ( class EncodingType, typeName, isDynamic
  , int256HexBuilder
  , int256HexParser
  , textBuilder
  , textParser
  , take
  ) where

import Prelude
import Data.String (fromCharArray)
import Data.Unfoldable (replicateA)
import Data.Word (Word32)
import Text.Parsing.Parser (ParserT)
import Text.Parsing.Parser.Token (hexDigit)

import Network.Ethereum.Web3.Types

--------------------------------------------------------------------------------
-- | Encoding Types
--------------------------------------------------------------------------------

class EncodingType a where
  typeName :: a -> String
  isDynamic :: a -> Boolean

instance encodingTypeBoolean :: EncodingType Boolean where
    typeName  = const "bool"
    isDynamic = const false

instance encodingTypeInt :: EncodingType Int where
    typeName  = const "int"
    isDynamic = const false

instance encodingTypeBigNumber:: EncodingType BigNumber where
    typeName  = const "int"
    isDynamic = const false

instance encodingTypeWord :: EncodingType Word32 where
    typeName  = const "uint"
    isDynamic = const false

instance encodingTypeString :: EncodingType String where
    typeName  = const "string"
    isDynamic = const true

instance encodingTypeAddress :: EncodingType Address where
    typeName  = const "address"
    isDynamic = const false

instance encodingTypeArray :: EncodingType a => EncodingType (Array a) where
    typeName  = const "[]"
    isDynamic = const true

--------------------------------------------------------------------------------
-- | Builders and Parsers
--------------------------------------------------------------------------------

-- | Encode anything any type of number that fits in a big numbed
int256HexBuilder :: forall a . Algebra a BigNumber => a -> HexString
int256HexBuilder x =
  let x' = embed x
  in if x' < zero
       then int256HexBuilder <<< toTwosComplement $ x'
       else padLeftSigned <<< toSignedHexString $ x'

-- | Parse a big number
int256HexParser :: forall m . Monad m => ParserT String m BigNumber
int256HexParser = fromHexString <$> take 64

-- | Encode dynamically sized string
textBuilder :: String -> HexString
textBuilder s = int256HexBuilder (length hx `div` 2) <> padLeftSigned (asSigned hx)
  where hx = fromUtf8 s

-- | Parse dynamically sized string.
textParser :: forall m . Monad m => ParserT String m String
textParser = do
    len <- toInt <$> int256HexParser
    let zeroBytes = getPadLength len
    void $ take (zeroBytes * 2)
    toUtf8 <$> take (len * 2)

-- | Read any number of HexDigits
take :: forall m . Monad m => Int -> ParserT String m HexString
take n = HexString <<< fromCharArray <$> replicateA n hexDigit

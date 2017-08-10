module Network.Ethereum.Web3.Encoding.Internal
  ( class EncodingType, typeName, isDynamic
  , int256HexBuilder
  , int256HexParser
  , take
  ) where

import Prelude
import Type.Proxy (Proxy)
import Data.String (fromCharArray)
import Data.Unfoldable (replicateA)
import Data.Word (Word32)
import Text.Parsing.Parser (ParserT)
import Text.Parsing.Parser.Token (hexDigit)

import Network.Ethereum.Web3.Types (class Algebra, Address, BigNumber, HexString(..),
                                    embed, fromHexStringSigned, padLeftSigned,
                                    toSignedHexString, toTwosComplement)


--------------------------------------------------------------------------------
-- | Encoding Types
--------------------------------------------------------------------------------

class EncodingType a where
  typeName :: Proxy a -> String
  isDynamic :: Proxy a -> Boolean

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
int256HexParser = fromHexStringSigned <$> take 64

-- | Read any number of HexDigits
take :: forall m . Monad m => Int -> ParserT String m HexString
take n = HexString <<< fromCharArray <$> replicateA n hexDigit

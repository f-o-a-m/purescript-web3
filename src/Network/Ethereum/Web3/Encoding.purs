module Network.Ethereum.Web3.Encoding where

import Prelude

import Network.Ethereum.Web3.Types
import Network.Ethereum.Web3.Encoding.Internal (int256HexBuilder, int256HexParser
                                               ,textBuilder, textParser, take)
import Text.Parsing.Parser (Parser)

class ABIEncoding a where
  toDataBuilder :: a -> HexString
  fromDataParser :: Parser String a

instance abiEncodingAlgebra :: ABIEncoding BigNumber where
  toDataBuilder = int256HexBuilder
  fromDataParser = int256HexParser

fromBool :: Boolean -> BigNumber
fromBool b = if b then one else zero

toBool :: BigNumber -> Boolean
toBool bn = not $ bn == zero

instance abiEncodingBool :: ABIEncoding Boolean where
    toDataBuilder  = int256HexBuilder <<< fromBool
    fromDataParser = toBool <$> int256HexParser

instance abiEncodingInt :: ABIEncoding Int where
    toDataBuilder  = int256HexBuilder
    fromDataParser = toInt <$> int256HexParser

instance abiEncodingString :: ABIEncoding String where
    toDataBuilder  = textBuilder
    fromDataParser = textParser

instance abiEncodingAddress :: ABIEncoding Address where
    toDataBuilder (Address addr) = padLeft addr
    fromDataParser = do
      _ <- take 24
      Address <$> take 40

--instance ABIEncoding a => ABIEncoding (Array a) where
--    toDataBuilder x = int256HexBuilder (length x)
--                      <> foldMap toDataBuilder x
--    fromDataParser = do len <- int256HexParser
--                        take len <$> P.many1 fromDataParser

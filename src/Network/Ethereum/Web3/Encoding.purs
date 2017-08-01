module Network.Ethereum.Web3.Encoding where

import Network.Ethereum.Web3.Types (HexString, BigNumber)
import Network.Ethereum.Web3.Encoding.Internal (int256HexBuilder, int256HexParser)
import Text.Parsing.Parser (Parser)

class ABIEncoding a where
  toDataBuilder :: a -> HexString
  fromDataParser :: Parser String a

instance abiEncodingAlgebra :: ABIEncoding BigNumber where
  toDataBuilder = int256HexBuilder
  fromDataParser = int256HexParser

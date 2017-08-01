module Web3.Solidity.Encoding where

import Web3.Utils.Types (HexString)
import Web3.Utils.BigNumber (BigNumber)
import Web3.Solidity.Param (int256HexBuilder, int256HexParser)
import Text.Parsing.Parser (Parser)

class ABIEncoding a where
  toDataBuilder :: a -> HexString
  fromDataParser :: Parser String a

instance abiEncodingAlgebra :: ABIEncoding BigNumber where
  toDataBuilder = int256HexBuilder
  fromDataParser = int256HexParser

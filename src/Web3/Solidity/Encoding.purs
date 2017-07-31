module Web3.Solidity.Encoding where

import Web3.Utils.Types (HexString)
import Text.Parsing.Parser (Parser)

class ABIEncoding a where
  toDataBuilder :: a -> HexString
  fromDataParser :: Parser String a

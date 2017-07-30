module Web3.Solidity.Bytes where

import Data.Typelevel.Num
import Web3.Utils.Types (HexString)

data BytesN a = BytesN HexString

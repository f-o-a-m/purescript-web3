module Web3.Solidity.Formatters where

import Prelude
import Data.Maybe (Maybe(Nothing))

import Web3.Solidity.Param (SolidityParam(..))
import Web3.Utils.BigNumber (BigNumber, toSignedHexString, toTwosComplement)
import Web3.Utils.Types (HexString, dropSign)
import Web3.Utils.Utils (padLeft)

formatInputInt :: BigNumber -> SolidityParam
formatInputInt v =
  let value' = flip padLeft 64 <<< toSignedHexString <<< toTwosComplement $ v
  in SolidityParam {value : value',  offset : Nothing}

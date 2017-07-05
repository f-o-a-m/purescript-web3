module Web3.Solidity.Formatters where

import Prelude
import Data.Maybe (Maybe(Nothing))

import Web3.Solidity.Param (SolidityParam(..), paramValue)
import Web3.Utils.BigNumber (BigNumber, embed, toSignedHexString, toTwosComplement)
import Web3.Utils.Types (HexString, asSigned, length)
import Web3.Utils.Utils (padLeft, padRight)

formatInputInt :: BigNumber -> SolidityParam
formatInputInt v =
  let value' = flip padLeft 64 <<< toSignedHexString <<< toTwosComplement $ v
  in SolidityParam {value : value',  offset : Nothing}

formatInputBytes :: HexString -> SolidityParam
formatInputBytes hx =
  let len = (length hx + 63) `div` 64
  in SolidityParam {value : padRight (asSigned hx) (len * 64), offset : Nothing}

formatDynamicInputBytes :: HexString -> SolidityParam
formatDynamicInputBytes hx =
  let len = embed $ length hx `div` 2
      l = (length hx + 63) `div` 64
      res = padRight (asSigned hx) (l * 64)
  in SolidityParam {value : (paramValue <<< formatInputInt $ len) <> res, offset : Nothing}

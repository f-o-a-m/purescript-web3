module Web3.Solidity.Formatters where

import Prelude
import Data.Maybe (Maybe(Nothing), fromMaybe)
import Data.String (null, take)

import Web3.Solidity.Param (SolidityParam(..), paramValue)
import Web3.Utils.BigNumber (BigNumber, embed, pow, hexadecimal, binary, toString, fromString, toSignedHexString, toTwosComplement)
import Web3.Utils.Types (HexString(..), asSigned, length)
import Web3.Utils.Utils (padLeft, padRight, fromUtf8)

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

formatInputString :: String -> SolidityParam
formatInputString s =
  let res = fromUtf8 s
      len = embed $ length res `div` 2
      l = (length res + 63)  `div` 64
      result = padRight (asSigned res) (l * 64)
  in SolidityParam {value : (paramValue <<< formatInputInt $ len) <> result, offset : Nothing}

formatInputBool :: Boolean -> SolidityParam
formatInputBool b = formatInputInt <<< embed $ if b then 1 else 0

formatInputReal :: BigNumber -> SolidityParam
formatInputReal a = formatInputInt $ a * (embed 2 `pow` 128)

signedIsNegative :: HexString -> Boolean
signedIsNegative (HexString hx) =
  if null hx
    then false
    else fromMaybe false $ do
           leadingChar <- fromString hexadecimal $ take 1 hx
           pure $ "1" `eq` (take 1 $ toString binary $ leadingChar)

--formatOutputInt :: SolidityParam -> BigNumber
--formatOutputInt (SolidityParam p) =
--  let sgn = if signedIsNegative p.value then Neg else Pos
--     fromSignedHexString (Signed sgn p.value)

--var formatOutputInt = function (param) {
--    var value = param.staticPart() || "0";
--
--    // check if it's negative number
--    // it it is, return two's complement
--    if (signedIsNegative(value)) {
--        return new BigNumber(value, 16).minus(new BigNumber('ffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffff', 16)).minus(1);
--    }
--    return new BigNumber(value, 16);
--};


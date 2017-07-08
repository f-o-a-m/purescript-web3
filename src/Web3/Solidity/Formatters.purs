module Web3.Solidity.Formatters where

import Prelude
import Data.DivisionRing (rightDiv)
import Data.Maybe (Maybe(Nothing), fromMaybe)
import Data.String (null, take, splitAt)

import Web3.Solidity.Param (SolidityParam(..), paramValue, staticPart, dynamicPart)
import Web3.Utils.BigNumber (BigNumber, embed, pow, hexadecimal, binary, toString, fromString, fromHexString, toSignedHexString, toTwosComplement, toInt)
import Web3.Utils.Types (HexString(..), unHex, asSigned, length)
import Web3.Utils.Utils (padLeft, padRight, fromUtf8, toUtf8)

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
           pure $ "1" `eq` (take 1 <<< toString binary $ leadingChar)

formatOutputInt :: SolidityParam -> BigNumber
formatOutputInt param =
  let pVal = paramValue param
  in if signedIsNegative pVal
       then fromHexString pVal - (embed <<< negate $ 1) - embed 1
       else fromHexString pVal

formatOutputUInt :: SolidityParam -> BigNumber
formatOutputUInt = fromHexString <<< paramValue

formatOutputUReal :: SolidityParam -> BigNumber
formatOutputUReal param = formatOutputUInt param `rightDiv` (embed 2 `pow` 128)

formatOutputBool :: SolidityParam -> Boolean
formatOutputBool param =
  (unHex <<< staticPart $ param) `eq` "0000000000000000000000000000000000000000000000000000000000000001"

formatOutputDynamicBytes :: SolidityParam -> Maybe HexString
formatOutputDynamicBytes param = do
  let (HexString dynPart) = dynamicPart param
  sizeAndContent <- 64 `splitAt` dynPart
  let len = 2 * (toInt <<< fromHexString <<< HexString $ sizeAndContent.before)
  pure <<< HexString <<< take len $ sizeAndContent.after

formatOutputString :: SolidityParam -> Maybe String
formatOutputString = map toUtf8 <<< formatOutputDynamicBytes

--formatOutputAddress :: SolidityParam -> Address
--formatOutputAddress param =
--  let stPart param.staticPart();
--    return "0x" + value.slice(value.length - 40, value.length);

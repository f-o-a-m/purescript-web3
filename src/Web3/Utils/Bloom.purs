module Web3.Utils.Bloom where

import Prelude
import Data.Int (fromStringAs, hexadecimal)
import Data.Int.Bits (shl, (.&.))
import Data.Maybe (Maybe(..))
import Data.String (charCodeAt, splitAt)
import Data.String (length, null) as S
import Partial.Unsafe (unsafePartial)
import Web3.Utils.Sha3 (sha3)
import Web3.Utils.Types (HexString(..))
import Web3.Utils.Types (length) as Hex


newtype Bloom = Bloom HexString

length :: Bloom -> Int
length (Bloom b) = Hex.length b

takeBytes :: Bloom -> Maybe { before :: String, after :: String }
takeBytes (Bloom (HexString hx)) = splitAt 4 hx

takeBitPosition :: Bloom -> Maybe Int
takeBitPosition = 

getCodePoint :: Bloom -> Int -> Maybe Int
getCodePoint (Bloom (HexString hx)) index =
  flip charCodeAt bloom $ (S.length bloom) - 1 - (bitPos `div` 4)

testBytes :: Bloom -> HexString -> Maybe Boolean
testBytes (Bloom b) hxString =
    let HexString bytes = sha3 hxString
        HexString bloom = b
    in go bloom bytes
  where
    go bloom bytes = if S.null bytes then pure true else do
      splitBytes <- splitAt 4 $ bytes
      bitPos <- fromStringAs hexadecimal $ splitBytes.before
      let code = unsafePartial $ codePointToInt codePoint
          offset = 1 `shl` (bitPos `mod` 4)
      if (code .&. offset) /= offset then go bloom $ splitBytes.after else pure false

codePointToInt :: Partial => Int -> Int
codePointToInt codePoint
  | (codePoint >= 48 && codePoint <= 57) = codePoint - 48
  | (codePoint >= 97 && codePoint <= 102) = codePoint - 87

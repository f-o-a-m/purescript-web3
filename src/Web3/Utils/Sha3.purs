module Web3.Utils.Sha3 where

import Prelude

newtype HexString = HexString String

instance showHexString :: Show HexString where
  show (HexString hx) = "0x" <> hx

derive newtype instance hexStringEq :: Eq HexString


class SHA3 a where
  sha3 :: a -> HexString

instance stringSha3 :: SHA3 String where
  sha3 = _sha3

instance hexStringSha3 :: SHA3 HexString where
  sha3 = sha3 <<< _parseHexString

foreign import _sha3 :: String -> HexString
foreign import _parseHexString :: HexString -> String


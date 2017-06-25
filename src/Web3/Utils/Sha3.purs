module Web3.Utils.Sha3
  ( class SHA3
  , sha3
  ) where

import Web3.Utils.Types (HexString)

--------------------------------------------------------------------------------

class SHA3 a where
  sha3 :: a -> HexString

instance stringSha3 :: SHA3 String where
  sha3 = sha3StringImpl

instance hexStringSha3 :: SHA3 HexString where
  sha3 = sha3HexImpl

foreign import sha3StringImpl :: String -> HexString
foreign import sha3HexImpl :: HexString -> HexString

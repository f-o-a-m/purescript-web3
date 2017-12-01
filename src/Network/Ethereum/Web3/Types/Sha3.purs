module Network.Ethereum.Web3.Types.Sha3
  ( class SHA3
  , sha3
  , toSelector
  ) where

import Prelude
import Network.Ethereum.Web3.Types.Types (HexString, takeHex)

--------------------------------------------------------------------------------

-- | A class for things which you can hash. Mostly used as a utility for calculating selectors and
-- | event topics
class SHA3 a where
  sha3 :: a -> HexString

instance stringSha3 :: SHA3 String where
  sha3 = sha3StringImpl

instance hexStringSha3 :: SHA3 HexString where
  sha3 = sha3HexImpl

foreign import sha3StringImpl :: String -> HexString
foreign import sha3HexImpl :: HexString -> HexString

-- | convert a string representing a type signature into a selector
toSelector :: String -> HexString
toSelector s = takeHex 8 $ sha3 s

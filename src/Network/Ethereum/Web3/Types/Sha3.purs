module Network.Ethereum.Web3.Types.Sha3
  ( class SHA3
  , sha3
  , toSelector
  ) where

import Prelude
import Data.ByteString (ByteString, fromString, toString)
import Data.Maybe (fromJust)
import Network.Ethereum.Web3.Types.Types (HexString, takeHex, unHex, mkHexString)
import Node.Encoding (Encoding(Hex, UTF8))
import Partial.Unsafe (unsafePartial)

--------------------------------------------------------------------------------

-- | A class for things which you can hash. Mostly used as a utility for calculating selectors and
-- | event topics
class SHA3 a where
  sha3 :: a -> HexString

foreign import _sha3 :: ByteString -> ByteString

instance stringSha3 :: SHA3 String where
  sha3 = unsafePartial fromJust <<< mkHexString <<< flip toString Hex <<< _sha3 <<< unsafePartial fromJust <<< flip fromString UTF8

instance hexStringSha3 :: SHA3 HexString where
  sha3 = unsafePartial fromJust <<< mkHexString <<< flip toString Hex <<< _sha3 <<< unsafePartial fromJust <<< flip fromString Hex <<< unHex

-- | convert a string representing a type signature into a selector
toSelector :: String -> HexString
toSelector s = takeHex 8 $ sha3 s

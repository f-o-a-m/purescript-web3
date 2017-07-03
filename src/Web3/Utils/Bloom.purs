module Web3.Utils.Bloom
  ( testHexString
  , Bloom(..)
  ) where

import Prelude
import Web3.Utils.Sha3 (sha3)
import Web3.Utils.Types (HexString)

--------------------------------------------------------------------------------
-- | TODO: make smart constructor for Bloom

newtype Bloom = Bloom HexString

foreign import testBytes :: Bloom -> HexString -> Boolean

testHexString :: Bloom -> HexString -> Boolean
testHexString b hx = testBytes b $ sha3 hx

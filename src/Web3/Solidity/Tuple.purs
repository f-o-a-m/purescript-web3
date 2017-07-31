module Web3.Solidity.Tuple where

import Prelude
import Data.Monoid.Additive (Additive(..))
import Data.Foldable (foldMap)

import Web3.Utils.Types (HexString)
import Web3.Utils.Types (length) as Hex
-- import Web3.Solidity.Param (class EncodingType, typeName, isDynamic)
-- import Web3.Solidity.Encoding (class ABIEncoding, toDataBuilder, fromDataParser)

-- | Argument offset calculator
offset :: Int
       -- ^ Count of arguments
       -> Array HexString
       -- ^ Previous dynamic arguments
       -> Int
       -- ^ Offset
offset totalArgs args = headerOffset + dataOffset
  where
    headerOffset = totalArgs * 32
    dataOffset   = let (Additive rawLength) = foldMap (Additive <<< Hex.length) args
                   in rawLength `div` 2

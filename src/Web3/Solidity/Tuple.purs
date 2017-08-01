module Web3.Solidity.Tuple where

import Prelude
import Data.Array (reverse, (:))
import Data.Tuple (Tuple(..), snd)
import Data.Monoid (class Monoid, mempty)
import Data.Monoid.Additive (Additive(..))
import Data.Foldable (fold, foldMap)
import Text.Parsing.Parser (Parser)

import Web3.Utils.Types (HexString(..))
import Web3.Utils.Types (length) as Hex
import Web3.Utils.BigNumber (BigNumber, embed)
import Web3.Solidity.Param (class EncodingType, isDynamic, take)
import Web3.Solidity.Encoding (class ABIEncoding, toDataBuilder, fromDataParser)

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

data EncodedValue =
  EncodedValue { headEnc :: HexString
               , tailEnc :: HexString
               }

instance encodedValueSemigroup :: Semigroup EncodedValue where
  append (EncodedValue val1) (EncodedValue val2) =
    EncodedValue { headEnc : val2.headEnc <> val2.headEnc
                 , tailEnc : val2.tailEnc <> val2.tailEnc
                 }

instance encodedValueMonoid :: Monoid EncodedValue where
  mempty = EncodedValue {headEnc : mempty, tailEnc : mempty}

-- | ABI data multiparam internal serializer
class ABIData a where
    _serialize :: Tuple Int (Array EncodedValue) -> a
    -- ^ Serialize with accumulator:
    -- pair of argument count and list of pair header and
    -- data part (for dynamic arguments)

instance abiDataHexString :: ABIData HexString where
    _serialize = (\(EncodedValue e) -> e.headEnc <> e.tailEnc) <<< fold <<< reverse <<< snd

instance abiDataInductive :: (EncodingType b, ABIEncoding b, ABIData a) => ABIData (b -> a) where
  _serialize (Tuple n l) x =
    if isDynamic x
       then _serialize $ Tuple n (dynEncoding  : l)
       else _serialize $ Tuple n (staticEncoding  : l)
    where
      dynOffset = offset n <<< map (\(EncodedValue a) -> a.tailEnc) $ l
      dynEncoding = EncodedValue { headEnc : (toDataBuilder (embed dynOffset :: BigNumber))
                                 , tailEnc : toDataBuilder x
                                 }
      staticEncoding = EncodedValue { headEnc : toDataBuilder x
                                    , tailEnc : mempty
                                    }
-- | Static argument parser
sParser :: (EncodingType a, ABIEncoding a) => a -> Parser a
sParser x | isDynamic x = take 64 >> return undefined
          | otherwise   = fromDataParser

-- | Dynamic argument parser
dParser :: forall a . EncodingType a => ABIEncoding a => a -> Parser String a
dParser x
  | isDynamic x = fromDataParser
  | otherwise   = pure x

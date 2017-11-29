module Network.Ethereum.Web3.Solidity.Generic
 ( class GenericABIEncode
 , class GenericABIDecode
 , class ABIData
 , EncodedValue
 , _serialize
 , genericFromDataParser
 , genericToDataBuilder
 , genericABIEncode
 , genericABIDecode
 , genericFromData
 , class ToRecordFields
 , toRecordFields
 , genericToRecordFields
 , class FromRecordFields
 , fromRecordFields
 , genericFromRecordFields
 ) where

import Data.Record.Builder
import Prelude
import Type.Row

import Control.Error.Util (hush)
import Control.Monad.State.Class (get)
import Data.Array (foldl, length, reverse, sort, uncons, (:))
import Data.Functor.Tagged (Tagged, untagged)
import Data.Generic.Rep (class Generic, Argument(..), Constructor(..), Field(..), Product(..), Rec(..), from, to)
import Data.Maybe (Maybe(..))
import Data.Monoid (mempty)
import Data.Symbol (class IsSymbol, SProxy)
import Network.Ethereum.Web3.Solidity.AbiEncoding (class ABIDecode, class ABIEncode, fromDataParser, take, toDataBuilder)
import Network.Ethereum.Web3.Solidity.EncodingType (class EncodingType, isDynamic)
import Network.Ethereum.Web3.Types (HexString, hexLength, unHex, unsafeToInt)
import Text.Parsing.Parser (ParseState(..), Parser, runParser)
import Text.Parsing.Parser.Combinators (lookAhead)
import Text.Parsing.Parser.Pos (Position(..))
import Type.Proxy (Proxy(..))

-- | A class for encoding generically composed datatypes to their abi encoding
class GenericABIEncode a where
  genericToDataBuilder :: a -> HexString

-- | A class for decoding generically composed datatypes from their abi encoding
class GenericABIDecode a where
  genericFromDataParser :: Parser String a

-- | An internally used type for encoding
data EncodedValue =
  EncodedValue { order :: Int
               , offset :: Maybe Int
               , encoding :: HexString
               }

instance eqEncodedValue :: Eq EncodedValue where
  eq (EncodedValue a) (EncodedValue b) = a.order == b.order

instance ordEncodedValue :: Ord EncodedValue where
  compare (EncodedValue a) (EncodedValue b) = a.order `compare` b.order

combineEncodedValues :: Array EncodedValue -> HexString
combineEncodedValues encodings =
  let sortedEs = adjust headsOffset $ sort encodings
      encodings' = addTailOffsets headsOffset [] sortedEs
  in let heads = foldl (\acc (EncodedValue e) -> case e.offset of
                          Nothing -> acc <> e.encoding
                          Just o -> acc <> toDataBuilder o
                      ) mempty encodings'
         tails = foldl (\acc (EncodedValue e) -> case e.offset of
                          Nothing -> acc
                          Just _ -> acc <> e.encoding
                      ) mempty encodings'
      in heads <> tails
  where
    adjust :: Int -> Array EncodedValue -> Array EncodedValue
    adjust n = map (\(EncodedValue e) -> EncodedValue e {offset = add n <$> e.offset})
    addTailOffsets :: Int -> Array EncodedValue -> Array EncodedValue -> Array EncodedValue
    addTailOffsets init acc es = case uncons es of
      Nothing -> reverse acc
      Just {head, tail} ->
        let EncodedValue e = head
        in case e.offset of
          Nothing -> addTailOffsets init (head : acc) tail
          Just _ -> addTailOffsets init (head : acc) (adjust (hexLength e.encoding `div` 2) tail)
    headsOffset :: Int
    headsOffset = foldl (\acc (EncodedValue e) -> case e.offset of
                                Nothing -> acc + (hexLength e.encoding `div` 2)
                                Just _ -> acc + 32
                            ) 0 encodings

-- | An internally used class for encoding
class ABIData a where
    _serialize :: Array EncodedValue -> a -> Array EncodedValue

instance abiDataBase :: (EncodingType b, ABIEncode b) => ABIData (Argument b) where
  _serialize encoded (Argument b) =
    if isDynamic (Proxy :: Proxy b)
       then dynEncoding  : encoded
       else staticEncoding : encoded
    where
      currentLength = hexLength $ combineEncodedValues encoded
      staticEncoding = EncodedValue { encoding : toDataBuilder b
                                    , offset : Nothing
                                    , order : 1 + length encoded
                                    }
      dynEncoding = EncodedValue { encoding : toDataBuilder b
                                 , offset : Just 0
                                 , order : 1 + length encoded
                                 }

instance abiDataInductive :: (EncodingType b, ABIEncode b, ABIData a) => ABIData (Product (Argument b) a) where
  _serialize encoded (Product (Argument b) a) =
    if isDynamic (Proxy :: Proxy b)
       then _serialize (dynEncoding  : encoded) a
       else _serialize (staticEncoding : encoded) a
    where
      currentLength = hexLength $ combineEncodedValues encoded
      staticEncoding = EncodedValue { encoding : toDataBuilder b
                                    , offset : Nothing
                                    , order : 1 + length encoded
                                    }
      dynEncoding = EncodedValue { encoding : toDataBuilder b
                                 , offset : Just 0
                                 , order : 1 + length encoded
                                 }

instance abiEncodeConstructor :: ABIData a => GenericABIEncode (Constructor name a) where
  genericToDataBuilder (Constructor a) = combineEncodedValues $ _serialize [] a

-- | Encode a generic type into its abi encoding, works only for types of the form
-- | `Constructor name (Product (Argument a1) (Product ... (Argument an)))`
genericABIEncode :: forall a rep.
                    Generic a rep
                 => GenericABIEncode rep
                 => a
                 -> HexString
genericABIEncode = genericToDataBuilder <<< from

instance baseAbiDecode :: (EncodingType a, ABIDecode a) => GenericABIDecode (Argument a) where
  genericFromDataParser = Argument <$> factorParser

instance inductiveAbiDecode :: (EncodingType b, ABIDecode b, GenericABIDecode a) => GenericABIDecode (Product (Argument b) a) where
  genericFromDataParser = Product <$> (Argument <$> factorParser) <*> genericFromDataParser

instance abiDecodeConstructor :: GenericABIDecode a => GenericABIDecode (Constructor name a) where
  genericFromDataParser = Constructor <$> genericFromDataParser

-- | Encode a generic type into its abi encoding, works only for types of the form
-- | `Constructor name (Product (Argument a1) (Product ... (Argument an)))`
genericABIDecode :: forall a rep.
                    Generic a rep
                 => GenericABIDecode rep
                 => Parser String a
genericABIDecode = to <$> genericFromDataParser

genericFromData :: forall a rep.
                   Generic a rep
                => GenericABIDecode rep
                => HexString
                -> Maybe a
genericFromData = hush <<< flip runParser genericABIDecode <<< unHex

-- helpers

factorParser :: forall a . ABIDecode a => EncodingType a => Parser String a
factorParser
  | not $ isDynamic (Proxy :: Proxy a) = fromDataParser
  | otherwise = dParser

dParser :: forall a . ABIDecode a => Parser String a
dParser = do
  dataOffset <- unsafeToInt <$> fromDataParser
  lookAhead $ do
    (ParseState _ (Position p) _) <- get
    _ <- take (dataOffset * 2 - (p.column - 1))
    fromDataParser

--------------------------------------------------------------------------------
-- * Generator Helpers
--------------------------------------------------------------------------------

class ToRecordFields args fields | args -> fields, fields -> args where
  toRecordFields :: args -> fields

instance toRecordBase :: ToRecordFields (Argument (Tagged (SProxy s) a)) (Field s a) where
  toRecordFields (Argument a) = Field $ untagged a

instance toRecordInductive :: ToRecordFields as fields => ToRecordFields (Product (Argument (Tagged (SProxy s) a)) as) (Product (Field s a) fields) where
  toRecordFields (Product (Argument a) as) = Product (Field $ untagged a) (toRecordFields as)

genericToRecordFields :: forall a n args b m fields .
                         Generic a (Constructor n args)
                      => Generic b (Constructor m (Rec fields))
                      => ToRecordFields args fields
                      => a
                      -> b
genericToRecordFields a =
  let Constructor args = from a
  in to <<< Constructor <<< Rec <<< toRecordFields $ args

genericParseRecordFields :: forall a n args b m fields .
                            Generic a (Constructor n args)
                         => Generic b (Constructor m (Rec fields))
                         => ToRecordFields args fields
                         => ABIDecode a
                         => Proxy a
                         -> Parser String b
genericParseRecordFields (Proxy :: Proxy a) = do
  (a :: a) <- fromDataParser
  pure $ genericToRecordFields a

class FromRecordFields fields args | args -> fields, fields -> args where
  fromRecordFields :: fields -> args

instance fromRecordFieldsBase :: FromRecordFields (Field s a) (Argument a) where
  fromRecordFields (Field a) = Argument a

instance fromRecordFieldsInductive :: FromRecordFields fields args => FromRecordFields (Product (Field s a) fields) (Product (Argument a) args) where
  fromRecordFields (Product (Field a) fields) = Product (Argument a) (fromRecordFields fields)

genericFromRecordFields :: forall a n args b m fields .
                           Generic b (Constructor m (Rec fields))
                        => Generic a (Constructor n args)
                        => FromRecordFields fields args
                        => b
                        -> a
genericFromRecordFields b =
  let (Constructor (Rec a)) = from b
  in to <<< Constructor <<< fromRecordFields $ a


class GenericFieldsRow fields (row :: # Type) | fields -> row

instance genericFieldsRowBase :: (RowCons s a () r) => GenericFieldsRow (Field s a) r

instance genericFieldsRowInductive :: (RowLacks s r, GenericFieldsRow fs r, RowCons s a r r') => GenericFieldsRow (Product (Field s a) fs) r'

class GenericMerge fs gs hs | fs gs -> hs, hs fs -> gs, gs fs -> hs

genericMerge :: forall a b c fs gs hs j l k.
                Generic a (Constructor (Rec fs))
             => Generic b (Constructor (Rec gs))
             => Generic c (Constructor (Rec hs))
             => GenericFieldsRow fs j
             => GenericFieldsRow gs k
             => GenericFieldsRow hs l
             => Union j k l
             -> a
             -> b
             -> c
genericMerge a b c =
  let Constructor fs = from a
      Constructor gs = from b
      j = to fs
      k = to gs
 in Constructor $ build (merge j) k

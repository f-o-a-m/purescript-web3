module Network.Ethereum.Web3.Solidity.Generic where

import Prelude
import Data.Array (foldMap, foldl, length, sortBy, (:))
import Data.Either (Either)
import Data.Functor.Tagged (Tagged, untagged, tagged)
import Data.Generic.Rep (class Generic, Argument(..), Constructor(..), NoArguments(..), Product(..), from, to)
import Data.Symbol (class IsSymbol)
import Network.Ethereum.Core.BigNumber (embed, unsafeToInt)
import Network.Ethereum.Core.HexString (HexString, numberOfBytes)
import Parsing (ParseError, ParseState(..), Parser, Position(..), getParserT, runParser)
import Parsing.Combinators (lookAhead)
import Type.Proxy (Proxy(..))
import Prim.Row as Row
import Type.RowList (class ListToRow, Cons, Nil, RowList)
import Network.Ethereum.Web3.Solidity.AbiEncoding (class ABIDecode, class ABIEncode, fromDataParser, parseBytes, toDataBuilder, uInt256HexBuilder)
import Network.Ethereum.Web3.Solidity.EncodingType (class EncodingType, isDynamic)
import Record as Record

-- | A class for encoding generically composed datatypes to their abi encoding
class GenericABIEncode a where
  genericToDataBuilder :: a -> HexString

-- | A class for decoding generically composed datatypes from their abi encoding
class GenericABIDecode a where
  genericFromDataParser :: Parser HexString a

-- | An internally used type for encoding
type EncodedValue
  = { order :: Int
    , isDynamic :: Boolean
    , encoding :: HexString
    , encodingLengthInBytes :: Int -- cache
    }

combineEncodedValues :: Array EncodedValue -> HexString
combineEncodedValues =
  sortBy (\a b -> a.order `compare` b.order)
    >>> \encodings ->
        let
          wordLengthInBytes = 32

          headsOffsetInBytes :: Int
          headsOffsetInBytes = foldl (+) 0 $ map (\encodedValueSimple -> if encodedValueSimple.isDynamic then wordLengthInBytes else encodedValueSimple.encodingLengthInBytes) encodings

          (heads :: HexString) =
            foldl
              ( \{ accumulator, lengthOfPreviousDynamicValues } encodedValue ->
                  if encodedValue.isDynamic then
                    { accumulator: accumulator <> uInt256HexBuilder (embed $ headsOffsetInBytes + lengthOfPreviousDynamicValues)
                    , lengthOfPreviousDynamicValues: lengthOfPreviousDynamicValues + encodedValue.encodingLengthInBytes
                    }
                  else
                    { accumulator: accumulator <> encodedValue.encoding
                    , lengthOfPreviousDynamicValues: lengthOfPreviousDynamicValues
                    }
              )
              { accumulator: mempty
              , lengthOfPreviousDynamicValues: 0
              }
              encodings
              # _.accumulator

          (tails :: HexString) =
            foldMap
              ( \encodedValue ->
                  if encodedValue.isDynamic then
                    encodedValue.encoding
                  else
                    mempty
              )
              encodings
        in
          heads <> tails

mkEncodedValue :: forall a. EncodingType a => ABIEncode a => Array EncodedValue -> a -> EncodedValue
mkEncodedValue otherEncodedArray a =
  let
    encoding = toDataBuilder a
  in
    { encoding
    , order: 1 + length otherEncodedArray
    , isDynamic: isDynamic (Proxy :: Proxy a)
    , encodingLengthInBytes: numberOfBytes encoding
    }

-- | An internally used class for encoding
class ABIData a where
  _serialize :: Array EncodedValue -> a -> Array EncodedValue

instance abiDataBaseNull :: ABIData NoArguments where
  _serialize encoded _ = encoded

instance abiDataBase :: (EncodingType b, ABIEncode b) => ABIData (Argument b) where
  _serialize encoded (Argument b) = mkEncodedValue encoded b : encoded

instance abiDataInductive :: (EncodingType b, ABIEncode b, ABIData a) => ABIData (Product (Argument b) a) where
  _serialize encoded (Product (Argument b) a) = _serialize (mkEncodedValue encoded b : encoded) a

instance abiEncodeConstructor :: ABIData a => GenericABIEncode (Constructor name a) where
  genericToDataBuilder (Constructor a) = combineEncodedValues $ _serialize [] a

-- | Encode a generic type into its abi encoding, works only for types of the form
-- | `Constructor name (Product (Argument a1) (Product ... (Argument an)))`
genericABIEncode ::
  forall a rep.
  Generic a rep =>
  GenericABIEncode rep =>
  a ->
  HexString
genericABIEncode = genericToDataBuilder <<< from

instance baseAbiDecode :: (EncodingType a, ABIDecode a) => GenericABIDecode (Argument a) where
  genericFromDataParser = Argument <$> factorParser

instance baseNullAbiDecode :: GenericABIDecode NoArguments where
  genericFromDataParser = pure NoArguments

instance inductiveAbiDecode :: (EncodingType b, ABIDecode b, GenericABIDecode a) => GenericABIDecode (Product (Argument b) a) where
  genericFromDataParser = Product <$> (Argument <$> factorParser) <*> genericFromDataParser

instance abiDecodeConstructor :: GenericABIDecode a => GenericABIDecode (Constructor name a) where
  genericFromDataParser = Constructor <$> genericFromDataParser

-- | Encode a generic type into its abi encoding, works only for types of the form
-- | `Constructor name (Product (Argument a1) (Product ... (Argument an)))`
genericABIDecode ::
  forall a rep.
  Generic a rep =>
  GenericABIDecode rep =>
  Parser HexString a
genericABIDecode = to <$> genericFromDataParser

genericFromData ::
  forall a rep.
  Generic a rep =>
  GenericABIDecode rep =>
  HexString ->
  Either ParseError a
genericFromData = flip runParser genericABIDecode

-- helpers
factorParser ::
  forall a.
  ABIDecode a =>
  EncodingType a =>
  Parser HexString a
factorParser
  | isDynamic (Proxy :: Proxy a) = dynamicFactorParser
  | otherwise = fromDataParser

dynamicFactorParser :: forall a. ABIDecode a => Parser HexString a
dynamicFactorParser = do
  dataOffset <- unsafeToInt <$> fromDataParser
  lookAhead
    $ do
        (ParseState _ (Position p) _) <- getParserT
        _ <- parseBytes (dataOffset - (p.column - 1))
        fromDataParser

class ArgsToRowListProxy :: forall k. k -> RowList Type -> Constraint
class ArgsToRowListProxy args l | args -> l, l -> args where
  argsToRowListProxy :: Proxy args -> Proxy l

instance argsToRowListProxyBaseNull :: ArgsToRowListProxy NoArguments Nil where
  argsToRowListProxy _ = Proxy

instance argsToRowListProxyBase :: ArgsToRowListProxy (Argument (Tagged (Proxy s) a)) (Cons s a Nil) where
  argsToRowListProxy _ = Proxy
else instance argsToRowListProxyInductive :: ArgsToRowListProxy as l => ArgsToRowListProxy (Product (Argument (Tagged (Proxy s) a)) as) (Cons s a l) where
  argsToRowListProxy _ = Proxy

class RecordFieldsIso args fields (rowList :: RowList Type) | args -> rowList, rowList -> args fields where
  toRecordFields :: forall proxy. proxy rowList -> args -> Record fields
  fromRecordFields :: forall proxy. proxy rowList -> Record fields -> args

instance isoRecordBase ::
  ( IsSymbol s
  , Row.Cons s a () r
  , Row.Lacks s ()
  ) =>
  RecordFieldsIso (Argument (Tagged s a)) r (Cons s a Nil) where
  toRecordFields _ (Argument a) = Record.insert (Proxy :: Proxy s) (untagged a) {}
  fromRecordFields _ r = Argument (tagged $ Record.get (Proxy :: Proxy s) r)

instance isoRecordBaseNull :: RecordFieldsIso NoArguments () Nil where
  toRecordFields _ _ = {}
  fromRecordFields _ _ = NoArguments

instance isoRecordInductive ::
  ( RecordFieldsIso as r1 (Cons ls la ll)
  , Row.Cons s a r1 r2
  , Row.Lacks s r1
  , IsSymbol s
  , ListToRow (Cons ls la ll) r1
  ) =>
  RecordFieldsIso (Product (Argument (Tagged s a)) as) r2 (Cons s a (Cons ls la ll)) where
  toRecordFields _ (Product (Argument a) as) = Record.insert (Proxy :: Proxy s) (untagged a) rest
    where
    rest = (toRecordFields (Proxy :: Proxy (Cons ls la ll)) as :: Record r1)
  fromRecordFields _ r =
    let
      a = Argument (tagged $ Record.get (Proxy :: Proxy s) r)

      before = Record.delete (Proxy :: Proxy s) r :: Record r1

      rest = fromRecordFields (Proxy :: Proxy (Cons ls la ll)) before
    in
      Product a rest

genericToRecordFields ::
  forall args fields l a name.
  RecordFieldsIso args fields l =>
  Generic a (Constructor name args) =>
  a ->
  Record fields
genericToRecordFields a =
  let
    Constructor row = from a
  in
    toRecordFields (Proxy :: Proxy l) row

genericFromRecordFields ::
  forall args fields l a name.
  RecordFieldsIso args fields l =>
  Generic a (Constructor name args) =>
  Record fields ->
  a
genericFromRecordFields r = to $ Constructor $ fromRecordFields (Proxy :: Proxy l) r

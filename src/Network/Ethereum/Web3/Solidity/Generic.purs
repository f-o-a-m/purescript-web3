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
  , class RecordFieldsIso
  , toRecordFields
  , fromRecordFields
  , genericToRecordFields
  , genericFromRecordFields
  , class ArgsToRowListProxy
  , argsToRowListProxy
  ) where

import Prelude

import Control.Monad.State.Class (get)
import Data.Array (foldMap, foldl, length, sortBy, (:))
import Data.Either (Either)
import Data.Functor.Tagged (Tagged, untagged, tagged)
import Data.Generic.Rep (class Generic, Argument(..), Constructor(..), NoArguments(..), Product(..), from, to)
import Data.Symbol (class IsSymbol)
import Network.Ethereum.Core.BigNumber as BigNumber
import Network.Ethereum.Core.HexString (HexString, hexLength)
import Network.Ethereum.Web3.Solidity.AbiEncoding (class ABIDecode, class ABIEncode, fromDataParser, takeBytes, toDataBuilder)
import Network.Ethereum.Web3.Solidity.EncodingType (class EncodingType, isDynamic)
import Prim.Row as Row
import Record as Record
import Text.Parsing.Parser (ParseError, ParseState(..), Parser, runParser)
import Text.Parsing.Parser.Combinators (lookAhead)
import Text.Parsing.Parser.Pos (Position(..))
import Type.Proxy (Proxy(..))
import Type.RowList as RowList

-- | A class for encoding generically composed datatypes to their abi encoding
class GenericABIEncode a where
-- TODO: rename to genericABIEncodeImplementation
  genericToDataBuilder :: a -> HexString

-- | A class for decoding generically composed datatypes from their abi encoding
class GenericABIDecode a where
-- TODO: rename to genericABIDecodeParserImplementation
  genericFromDataParser :: Parser HexString a

-- | An internally used type for encoding
type EncodedValue
  = { order :: Int
    , isDynamic :: Boolean
    , encoding :: HexString
    , encodingLengthInBytes :: Int -- cache
    }

-- from https://docs.soliditylang.org/en/v0.8.12/abi-spec.html#examples
--
-- if Ti is static:
--   head(X(i)) = enc(X(i)) and tail(X(i)) = "" (the empty string)
-- otherwise, i.e. if Ti is dynamic:
--   head(X(i)) = enc(len( head(X(1)) ... head(X(k)) tail(X(1)) ... tail(X(i-1)) )) tail(X(i)) = enc(X(i))
combineEncodedValues :: Array EncodedValue -> HexString
combineEncodedValues = sortBy (\a b -> a.order `compare` b.order) >>> \encodings ->
  let
    wordLengthInBytes = 32

    headsOffsetInBytes :: Int
    headsOffsetInBytes = foldl (+) 0 $ map (\encodedValueSimple -> if encodedValueSimple.isDynamic then wordLengthInBytes else encodedValueSimple.encodingLengthInBytes) encodings

    (heads :: HexString) =
      foldl
        ( \{ accumulator, lengthOfPreviousDynamicValues } encodedValue -> if encodedValue.isDynamic
            then
            { accumulator: accumulator <> (toDataBuilder :: Int -> HexString) (headsOffsetInBytes + lengthOfPreviousDynamicValues)
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
        (\encodedValue -> if encodedValue.isDynamic
            then encodedValue.encoding
            else mempty
        )
        encodings
    in
      heads <> tails

-- | An internally used class for encoding
-- TODO: rename to GenericABIDataSerialize or GenericABIEncodeWithOffset
class ABIData a where
-- TODO: rename to _fromGenericABIDataSerialize
  _serialize :: Array EncodedValue -> a -> Array EncodedValue

instance abiDataBaseNull :: ABIData NoArguments where
  _serialize otherEncodedArray _ = otherEncodedArray

mkEncodedValueSimple :: forall a . EncodingType a => ABIEncode a => Array EncodedValue -> a -> EncodedValue
mkEncodedValueSimple otherEncodedArray a =
  let encoding = toDataBuilder a
  in
  { encoding
  , order: 1 + length otherEncodedArray
  , isDynamic: isDynamic (Proxy :: Proxy a)
  , encodingLengthInBytes: lengthOfHexStringInBytes encoding
  }
  where
  lengthOfHexStringInBytes hexString = hexLength hexString `div` 2

instance abiDataBase :: (EncodingType a, ABIEncode a) => ABIData (Argument a) where
  _serialize otherEncodedArray (Argument a) = mkEncodedValueSimple otherEncodedArray a : otherEncodedArray

instance abiDataInductive :: (EncodingType b, ABIEncode b, ABIData a) => ABIData (Product (Argument b) a) where
  _serialize otherEncodedArray (Product (Argument b) a) = _serialize (mkEncodedValueSimple otherEncodedArray b : otherEncodedArray) a

instance abiEncodeConstructor :: ABIData a => GenericABIEncode (Constructor tupleNName a) where
-- TODO: rename to genericABIEncodeImplementation
  genericToDataBuilder (Constructor a) = combineEncodedValues $ _serialize [] a

-- | Encode a generic type into its abi encoding, works only for types of the form
-- | `Constructor tupleNName (Product (Argument a1) (Product ... (Argument an)))`
genericABIEncode ::
  forall tupleN tupleNRep.
  Generic tupleN tupleNRep =>
  GenericABIEncode tupleNRep =>
  tupleN ->
  HexString
genericABIEncode = genericToDataBuilder <<< from

instance baseAbiDecode :: (EncodingType a, ABIDecode a) => GenericABIDecode (Argument a) where
  -- TODO: rename to genericABIDecodeParserImplementation
  genericFromDataParser = Argument <$> factorParser

instance baseNullAbiDecode :: GenericABIDecode NoArguments where
  genericFromDataParser = pure NoArguments

instance inductiveAbiDecode :: (EncodingType b, ABIDecode b, GenericABIDecode a) => GenericABIDecode (Product (Argument b) a) where
  genericFromDataParser = Product <$> (Argument <$> factorParser) <*> genericFromDataParser

instance abiDecodeConstructor :: GenericABIDecode a => GenericABIDecode (Constructor tupleNName a) where
  genericFromDataParser = Constructor <$> genericFromDataParser

-- TODO: rename to genericABIDecodeParser
-- | Encode a generic type into its abi encoding, works only for types of the form
-- | `Constructor name (Product (Argument a1) (Product ... (Argument an)))`
genericABIDecode ::
  forall a rep.
  Generic a rep =>
  GenericABIDecode rep =>
  Parser HexString a
genericABIDecode = to <$> genericFromDataParser

-- TODO: rename to genericABIDecode
genericFromData ::
  forall a rep.
  Generic a rep =>
  GenericABIDecode rep =>
  HexString ->
  Either ParseError a
genericFromData = flip runParser genericABIDecode

-- helpers
factorParser :: forall a. ABIDecode a => EncodingType a => Parser HexString a
factorParser
  | not $ isDynamic (Proxy :: Proxy a) = fromDataParser
  | otherwise = dynamicFactorParser

dynamicFactorParser :: forall a. ABIDecode a => Parser HexString a
dynamicFactorParser = do
  dataOffset <- BigNumber.unsafeToInt <$> fromDataParser
  lookAhead
    $ do
        (ParseState _ (Position p) _) <- get
        _ <- takeBytes (dataOffset - (p.column - 1))
        fromDataParser

-- ArgsToRowListProxy
class ArgsToRowListProxy :: forall k. k -> RowList.RowList Type -> Constraint
class ArgsToRowListProxy args l | args -> l, l -> args where
  argsToRowListProxy :: Proxy args -> Proxy l

instance argsToRowListProxyBaseNull :: ArgsToRowListProxy NoArguments RowList.Nil where
  argsToRowListProxy _ = Proxy

instance argsToRowListProxyBase :: ArgsToRowListProxy (Argument (Tagged (Proxy s) a)) (RowList.Cons s a RowList.Nil) where
  argsToRowListProxy _ = Proxy
else instance argsToRowListProxyInductive :: ArgsToRowListProxy as l => ArgsToRowListProxy (Product (Argument (Tagged (Proxy s) a)) as) (RowList.Cons s a l) where
  argsToRowListProxy _ = Proxy

-- Example:
-- Tagged (Proxy "foo") a => { foo :: a }
-- to
-- Data.Generic.Rep.Product (Tagged (Proxy "foo") a) (Tagged (Proxy "bar") b)) => { foo :: a, bar :: b }

-- TODO(srghma): rename to GenericToRecordFieldsIso
class RecordFieldsIso :: forall k. Type -> Row Type -> k -> Constraint
class RecordFieldsIso genericTaggedRepresentation recordRow rowList | genericTaggedRepresentation -> rowList, rowList -> genericTaggedRepresentation recordRow where
  -- TODO: rename to genericToRecordFieldsImplementation, genericFromRecordFieldsImplementation
  toRecordFields :: Proxy rowList -> genericTaggedRepresentation -> Record recordRow
  fromRecordFields :: Proxy rowList -> Record recordRow -> genericTaggedRepresentation

instance isoRecordBase ::
  ( IsSymbol s
  , Row.Cons s a () r
  , Row.Lacks s ()
  ) =>
  RecordFieldsIso (Argument (Tagged (Proxy s) a)) r (RowList.Cons s a RowList.Nil) where
  toRecordFields _ (Argument a) = Record.insert (Proxy :: Proxy s) (untagged a) {}
  fromRecordFields _ r = Argument (tagged $ Record.get (Proxy :: Proxy s) r)

instance isoRecordBaseNull :: RecordFieldsIso NoArguments () RowList.Nil where
  toRecordFields _ _ = {}
  fromRecordFields _ _ = NoArguments

instance isoRecordInductive ::
  ( RecordFieldsIso as r1 (RowList.Cons ls la ll)
  , Row.Cons s a r1 r2
  , Row.Lacks s r1
  , IsSymbol s
  , RowList.ListToRow (RowList.Cons ls la ll) r1
  ) =>
  RecordFieldsIso (Product (Argument (Tagged (Proxy s) a)) as) r2 (RowList.Cons s a (RowList.Cons ls la ll)) where
  toRecordFields _ (Product (Argument a) as) = Record.insert (Proxy :: Proxy s) (untagged a) rest
    where
    rest = (toRecordFields (Proxy :: Proxy (RowList.Cons ls la ll)) as :: Record r1)
  fromRecordFields _ r =
    let
      a = Argument (tagged $ Record.get (Proxy :: Proxy s) r)

      before = Record.delete (Proxy :: Proxy s) r :: Record r1

      rest = fromRecordFields (Proxy :: Proxy (RowList.Cons ls la ll)) before
    in
      Product a rest

genericToRecordFields ::
  forall genericTaggedRepresentation recordRow recordRowList a tupleNName.
  RecordFieldsIso genericTaggedRepresentation recordRow recordRowList =>
  Generic a (Constructor tupleNName genericTaggedRepresentation) =>
  a ->
  Record recordRow
genericToRecordFields a =
  let
    Constructor row = from a
  in
    toRecordFields (Proxy :: Proxy recordRowList) row

-- Example:
-- genericFromRecordFields { foo :: foo, bar :: bar, baz :: baz }
-- =>
--    (Tuple3
--      (Tagged (Proxy "foo") foo)
--      (Tagged (Proxy "bar") bar)
--      (Tagged (Proxy "baz") baz)
--    )
genericFromRecordFields ::
  forall genericTaggedRepresentation recordRow recordRowList a tupleNName.
  RecordFieldsIso genericTaggedRepresentation recordRow recordRowList =>
  Generic a (Constructor tupleNName genericTaggedRepresentation) =>
  Record recordRow ->
  a
genericFromRecordFields r = to $ Constructor $ fromRecordFields (Proxy :: Proxy recordRowList) r

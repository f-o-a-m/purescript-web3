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
import Data.Array (foldl, length, reverse, sort, uncons, (:))
import Data.Either (Either)
import Data.Functor.Tagged (Tagged, untagged, tagged)
import Data.Generic.Rep (class Generic, Argument(..), Constructor(..), NoArguments(..), Product(..), from, to)
import Data.Maybe (Maybe(..))
import Record as Record
import Data.Symbol (class IsSymbol)
import Network.Ethereum.Web3.Solidity.AbiEncoding (class ABIDecode, class ABIEncode, fromDataParser, takeBytes, toDataBuilder)
import Network.Ethereum.Web3.Solidity.EncodingType (class EncodingType, isDynamic)
import Network.Ethereum.Core.HexString (HexString, hexLength)
import Network.Ethereum.Core.BigNumber (unsafeToInt)
import Text.Parsing.Parser (ParseError, ParseState(..), Parser, runParser)
import Text.Parsing.Parser.Combinators (lookAhead)
import Text.Parsing.Parser.Pos (Position(..))
import Type.Proxy (Proxy(..))
import Prim.Row as Row
import Type.RowList as RowList

-- | A class for encoding generically composed datatypes to their abi encoding
class GenericABIEncode a where
  genericToDataBuilder :: a -> HexString

-- | A class for decoding generically composed datatypes from their abi encoding
class GenericABIDecode a where
  genericFromDataParser :: Parser HexString a

-- | An internally used type for encoding
newtype EncodedValue
  = EncodedValue
    { order :: Int
    , offset :: Maybe Int
    , encoding :: HexString
    }

instance eqEncodedValue :: Eq EncodedValue where
  eq (EncodedValue a) (EncodedValue b) = a.order == b.order

instance ordEncodedValue :: Ord EncodedValue where
  compare (EncodedValue a) (EncodedValue b) = a.order `compare` b.order

combineEncodedValues :: Array EncodedValue -> HexString
combineEncodedValues encodings =
  let
    sortedEs = adjust headsOffset $ sort encodings

    encodings' = addTailOffsets headsOffset [] sortedEs
  in
    let
      heads =
        foldl
          ( \acc (EncodedValue e) -> case e.offset of
              Nothing -> acc <> e.encoding
              Just o -> acc <> toDataBuilder o
          )
          mempty
          encodings'

      tails =
        foldl
          ( \acc (EncodedValue e) -> case e.offset of
              Nothing -> acc
              Just _ -> acc <> e.encoding
          )
          mempty
          encodings'
    in
      heads <> tails
  where
  adjust :: Int -> Array EncodedValue -> Array EncodedValue
  adjust n = map (\(EncodedValue e) -> EncodedValue e { offset = add n <$> e.offset })

  addTailOffsets :: Int -> Array EncodedValue -> Array EncodedValue -> Array EncodedValue
  addTailOffsets init acc es = case uncons es of
    Nothing -> reverse acc
    Just { head, tail } ->
      let
        EncodedValue e = head
      in
        case e.offset of
          Nothing -> addTailOffsets init (head : acc) tail
          Just _ -> addTailOffsets init (head : acc) (adjust (hexLength e.encoding `div` 2) tail)

  headsOffset :: Int
  headsOffset =
    foldl
      ( \acc (EncodedValue e) -> case e.offset of
          Nothing -> acc + (hexLength e.encoding `div` 2) -- acc + number of bytes in HexString
          Just _ -> acc + 32 -- acc + max number of bytes
      )
      0
      encodings

-- | An internally used class for encoding
-- TODO: rename to FromGenericABIDataSerialize
class ABIData a where
  _serialize :: Array EncodedValue -> a -> Array EncodedValue

instance abiDataBaseNull :: ABIData NoArguments where
  _serialize encoded _ = encoded

instance abiDataBase :: (EncodingType b, ABIEncode b) => ABIData (Argument b) where
  _serialize encoded (Argument b) =
    if isDynamic (Proxy :: Proxy b) then
      dynEncoding unit : encoded
    else
      staticEncoding unit : encoded
    where
    staticEncoding = \_ ->
      EncodedValue
        { encoding: toDataBuilder b
        , offset: Nothing
        , order: 1 + length encoded
        }

    dynEncoding = \_ ->
      EncodedValue
        { encoding: toDataBuilder b
        , offset: Just 0
        , order: 1 + length encoded
        }

instance abiDataInductive :: (EncodingType b, ABIEncode b, ABIData a) => ABIData (Product (Argument b) a) where
  _serialize encoded (Product (Argument b) a) =
    if isDynamic (Proxy :: Proxy b) then
      _serialize (dynEncoding unit : encoded) a
    else
      _serialize (staticEncoding unit : encoded) a
    where
    staticEncoding = \_ ->
      EncodedValue
        { encoding: toDataBuilder b
        , offset: Nothing
        , order: 1 + length encoded
        }

    dynEncoding = \_ ->
      EncodedValue
        { encoding: toDataBuilder b
        , offset: Just 0
        , order: 1 + length encoded
        }

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
factorParser :: forall a. ABIDecode a => EncodingType a => Parser HexString a
factorParser
  | not $ isDynamic (Proxy :: Proxy a) = fromDataParser
  | otherwise = dParser

dParser :: forall a. ABIDecode a => Parser HexString a
dParser = do
  dataOffset <- unsafeToInt <$> fromDataParser
  lookAhead
    $ do
        (ParseState _ (Position p) _) <- get
        _ <- takeBytes (dataOffset - (p.column - 1))
        fromDataParser

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

-- TODO(srghma): rename to GenericProductToRecordFieldsIso
class RecordFieldsIso :: forall k. Type -> Row Type -> k -> Constraint
class RecordFieldsIso genericTaggedRepresentation recordRow rowList | genericTaggedRepresentation -> rowList, rowList -> genericTaggedRepresentation recordRow where
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

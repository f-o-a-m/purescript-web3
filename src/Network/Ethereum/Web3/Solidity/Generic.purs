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
import Data.Symbol (class IsSymbol, SProxy(..))
import Network.Ethereum.Web3.Solidity.AbiEncoding (class ABIDecode, class ABIEncode, fromDataParser, take, toDataBuilder)
import Network.Ethereum.Web3.Solidity.EncodingType (class EncodingType, isDynamic)
import Network.Ethereum.Core.HexString (HexString, hexLength, unHex)
import Network.Ethereum.Core.BigNumber (unsafeToInt)
import Text.Parsing.Parser (ParseError, ParseState(..), Parser, runParser)
import Text.Parsing.Parser.Combinators (lookAhead)
import Text.Parsing.Parser.Pos (Position(..))
import Type.Proxy (Proxy(..))
import Prim.Row as Row
import Type.Row (class ListToRow, Cons, Nil, RLProxy(..), kind RowList)

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

instance abiDataBaseNull :: ABIData NoArguments where
    _serialize encoded _ = encoded

instance abiDataBase :: (EncodingType b, ABIEncode b) => ABIData (Argument b) where
  _serialize encoded (Argument b) =
    if isDynamic (Proxy :: Proxy b)
       then dynEncoding  : encoded
       else staticEncoding : encoded
    where
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

instance baseNullAbiDecode :: GenericABIDecode NoArguments where
  genericFromDataParser = pure NoArguments

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
                -> Either ParseError a
genericFromData = flip runParser genericABIDecode <<< unHex

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

class ArgsToRowListProxy args (l :: RowList) | args -> l, l -> args where
  argsToRowListProxy :: Proxy args -> RLProxy l

instance argsToRowListProxyBaseNull :: ArgsToRowListProxy NoArguments Nil where
  argsToRowListProxy _ = RLProxy

instance argsToRowListProxyBase :: ArgsToRowListProxy (Argument (Tagged (SProxy s) a)) (Cons s a Nil) where
  argsToRowListProxy _ = RLProxy
else instance argsToRowListProxyInductive :: ArgsToRowListProxy as l => ArgsToRowListProxy (Product (Argument (Tagged (SProxy s) a)) as) (Cons s a l) where
  argsToRowListProxy _ = RLProxy

class RecordFieldsIso args fields rowList | args -> rowList, rowList -> args fields where
  toRecordFields :: RLProxy rowList -> args -> Record fields
  fromRecordFields :: RLProxy rowList -> Record fields -> args

instance isoRecordBase ::
  ( IsSymbol s
  , Row.Cons s a () r
  , Row.Lacks s ()
  ) => RecordFieldsIso (Argument (Tagged (SProxy s) a)) r (Cons s a Nil)
  where
  toRecordFields _ (Argument a) = Record.insert (SProxy :: SProxy s) (untagged a) {}
  fromRecordFields _ r = Argument (tagged $ Record.get (SProxy :: SProxy s) r)

instance isoRecordBaseNull :: RecordFieldsIso NoArguments () Nil where
  toRecordFields _ _ = {}
  fromRecordFields _ _ = NoArguments

instance isoRecordInductive ::
  ( RecordFieldsIso as r1 (Cons ls la ll)
  , Row.Cons s a r1 r2
  , Row.Lacks s r1
  , IsSymbol s
  , ListToRow (Cons ls la ll) r1
  ) => RecordFieldsIso (Product (Argument (Tagged (SProxy s) a)) as) r2 (Cons s a (Cons ls la ll)) where
  toRecordFields _ (Product (Argument a) as) = Record.insert (SProxy :: SProxy s) (untagged a) rest
    where rest = (toRecordFields (RLProxy :: RLProxy (Cons ls la ll)) as :: Record r1)

  fromRecordFields _ r =
    let a = Argument (tagged $ Record.get (SProxy :: SProxy s) r)
        before = Record.delete (SProxy :: SProxy s) r :: Record r1
        rest = fromRecordFields (RLProxy :: RLProxy (Cons ls la ll)) before
    in Product a rest

genericToRecordFields :: forall args fields l a name .
                         RecordFieldsIso args fields l
                      => Generic a (Constructor name args)
                      => a
                      -> Record fields
genericToRecordFields a =
  let Constructor row = from a
  in toRecordFields (RLProxy :: RLProxy l) row

genericFromRecordFields :: forall args fields l a name .
                         RecordFieldsIso args fields l
                      => Generic a (Constructor name args)
                      => Record fields
                      -> a
genericFromRecordFields r = to $ Constructor $ fromRecordFields (RLProxy :: RLProxy l) r

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
 , class ArgsToRowListProxy
 , argsToRowListProxy
 ) where

import Prelude

import Control.Error.Util (hush)
import Control.Monad.State.Class (get)
import Data.Array (foldl, length, reverse, sort, uncons, (:))
import Data.Functor.Tagged (Tagged, untagged)
import Data.Generic.Rep (class Generic, Argument(..), Constructor(..), Product(..), from, to)
import Data.Maybe (Maybe(..))
import Data.Monoid (mempty)
import Data.Record (insert)
import Data.Symbol (class IsSymbol, SProxy(..))
import Network.Ethereum.Web3.Solidity.AbiEncoding (class ABIDecode, class ABIEncode, fromDataParser, take, toDataBuilder)
import Network.Ethereum.Web3.Solidity.EncodingType (class EncodingType, isDynamic)
import Network.Ethereum.Web3.Types (HexString, hexLength, unHex, unsafeToInt)
import Text.Parsing.Parser (ParseState(..), Parser, runParser)
import Text.Parsing.Parser.Combinators (lookAhead)
import Text.Parsing.Parser.Pos (Position(..))
import Type.Data.Boolean (BProxy(..), False, True, kind Boolean)
import Type.Proxy (Proxy(..))
import Type.Row (class ListToRow, class RowLacks, Cons, Nil, RLProxy(..), RProxy(..), kind RowList)

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

class RLProxyEquality (a :: RowList) (b :: RowList)

instance rlproxyEqualityBase :: RLProxyEquality Nil Nil
instance rlproxyEqualityInductive :: RLProxyEquality l1 l2 => RLProxyEquality (Cons s a l1) (Cons s a l2)

class ArgsToRowListProxy args (l :: RowList) | args -> l, l -> args where
  argsToRowListProxy :: Proxy args -> RLProxy l

instance argsToRowListProxyBase :: ArgsToRowListProxy (Argument (Tagged (SProxy s) a)) (Cons s a Nil) where
  argsToRowListProxy _ = RLProxy

instance argsToRowListProxyInductive :: ArgsToRowListProxy as l => ArgsToRowListProxy (Product (Argument (Tagged (SProxy s) a)) as) (Cons s a l) where
  argsToRowListProxy _ = RLProxy

class ToRecordFields args fields (rowList :: RowList) | args -> rowList, rowList -> args, rowList -> fields where
  toRecordFields :: RLProxy rowList -> args -> Record fields

instance toRecordBase :: (IsSymbol s, RowCons s a () r, RowLacks s ()) => ToRecordFields (Argument (Tagged (SProxy s) a)) r (Cons s a Nil) where
  toRecordFields _ (Argument a) = insert (SProxy :: SProxy s) (untagged a) {}

instance toRecordInductive :: (ToRecordFields as r1 l, RowCons s a r1 r2, RowLacks s r1, IsSymbol s, ListToRow l r1) => ToRecordFields (Product (Argument (Tagged (SProxy s) a)) as) r2 (Cons s a l) where
  toRecordFields _ (Product (Argument a) as) = insert (SProxy :: SProxy s) (untagged a) rest
    where rest = (toRecordFields (RLProxy :: RLProxy l) as :: Record r1)

genericToRecordFields :: forall args fields l a name .
                         ToRecordFields args fields l
                      => Generic a (Constructor name args)
                      => ArgsToRowListProxy args l
                      => ListToRow l fields
                      => a -> Record fields
genericToRecordFields a =
  let Constructor row = from a
  in toRecordFields (RLProxy :: RLProxy l) row

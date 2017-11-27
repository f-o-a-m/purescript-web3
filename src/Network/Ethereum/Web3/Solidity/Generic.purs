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
 ) where

import Data.Record.Builder
import Prelude
import Type.Row

import Control.Error.Util (hush)
import Control.Monad.State.Class (get)
import Data.Array (foldl, insert, length, reverse, sort, uncons, (:))
import Data.Generic.Rep (class Generic, Argument(..), Constructor(..), Field(..), Product(..), Rec(..), from, to)
import Data.Maybe (Maybe(..))
import Data.Monoid (mempty)
import Data.Symbol (class IsSymbol, SProxy(..))
import Network.Ethereum.Web3.Solidity.AbiEncoding (class ABIDecode, class ABIEncode, fromDataParser, take, toDataBuilder)
import Network.Ethereum.Web3.Solidity.EncodingType (class EncodingType, isDynamic)
import Network.Ethereum.Web3.Types (HexString, asSigned, hexLength, unHex, unsafeToInt)
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

instance toRecordBase :: ToRecordFields (Argument (Field s a)) (Field s a) where
  toRecordFields (Argument a) = a

instance toRecordInductive :: ToRecordFields as fields => ToRecordFields (Product (Argument (Field s a)) as) (Product (Field s a) fields) where
  toRecordFields (Product (Argument a) as) = Product a (toRecordFields as)

-- Tuple (Field na a) (Field nb b) (Field nc c) -> {na :: a, nb :: b , nc :: c}
instance toRecordConstructor :: ToRecordFields as fields => ToRecordFields (Constructor name as) (Rec fields) where
  toRecordFields (Constructor as) = Rec $ toRecordFields as

-- | MergeRecs (A {a :: Int}) (B {b :: String}) (C {a :: Int, b :: String})
class MergeRecs as bs cs | as bs -> cs, as cs -> bs, bs cs -> as where
  mergeRecs :: as -> bs -> cs

instance mergeRecsBase :: MergeRecs (Field s a) (Field t b) (Product (Field s a) (Field t b)) where
  mergeRecs a b = Product a b

instance mergeRecsInductive :: MergeRecs (Field s a) b (Product (Field s a) b) where
  mergeRecs a b = Product a b

instance mergeRecsInductive' :: MergeRecs as bs cs =>  MergeRecs (Product (Field s a) as) bs (Product (Field s a) cs) where
  mergeRecs (Product a as) bs = Product a $ mergeRecs as bs


class BuildCombined as bs where
  buildCombined :: as -> bs

instance buildCombinedBase :: (RowLacks s r1, RowCons s r1 a r2, IsSymbol s) => BuildCombined (Field s a) (Builder (Record r1) (Record r2)) where
  buildCombined (Field a) = insert (SProxy :: SProxy s) a

--instance buildCombinedInductive :: (BuildCombined as (Record r1), RowCons s a r1 r2, RowLacks s r1) => BuildCombined (Product (Field s a) as) (Builder (Record r1) (Record r2)) where
--  buildCombined (Product (Field a) as) = insert (SProxy :: SProxy s) a >>> buildCombined


{-
class GenericToRowList fields (as :: RowList) | fields -> as, as -> fields

instance genericToRowListBase :: GenericToRowList (Field s a) (Cons s a Nil)

instance genericToRowListInductive :: GenericToRowList fields as => GenericToRowList (Product (Field s a) fields) (Cons s a as)
-}


data Event as bs = Event as bs

-- Event (Tuple2 (Field "to" Address) (Field "from" Address)) (Tuple1 "amount" UInt256) -> Transfer {to :: Address, from :: Address, amount :: UInt256}

--productToRow :: forall a b as bs fas fbs ras rbs r.
--                Generic a as
--             => Generic b bs
--             => Generic b bs
--             => ToRecordFields as fas
--             => ToRecordFields bs fbs
--             => GenericToRowList fas las
--             => GenericToRowList fbs lbs
--             => ListToRow las ras
--             => ListToRow lbs rbs
--             => Union ras rbs r
--             -> Event a b
--             -> r
--productToRow (Event a b) = (toRecordFields <<< to $ a) (toRecordFields <<< to $ b)

--
--buildEvent :: forall as bs fas fbs r.
--              ToRecordFields as fas
--           => ToRecordFields bs fbs
--           => Union fas fbs r
--           -> Event as bs
--           -> Record r
--buildEvent (Event as bs) = build (to <<< toRecordFields $ as) (to <<< toRecordFields $ bs) {}

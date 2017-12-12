module Network.Ethereum.Web3.Types.Types
       ( Sign(..)
       , Signed(..)
       , asSigned
       , HexString(..)
       , mkHexString
       , unHex
       , hexLength
       , takeHex
       , Address
       , unAddress
       , mkAddress
       , CallMode(..)
       , Block(..)
       , Transaction(..)
       , TransactionReceipt(..)
       , TransactionOptions(..)
       , defaultTransactionOptions
       , _from
       , _to
       , _data
       , _value
       , _gas
       , _gasPrice
       , ETH
       , _nonce
       , Web3(..)
       , unsafeCoerceWeb3
       , Filter(..)
       , defaultFilter
       , _address
       , _topics
       , _fromBlock
       , _toBlock
       , FilterId(..)
       , Change(..)
       , FalseOrObject(..)
       , unFalseOrObject
       , SyncStatus(..)
       ) where

import Prelude

import Control.Error.Util (hush)
import Control.Monad.Aff (Aff)
import Control.Monad.Aff.Class (class MonadAff)
import Control.Monad.Aff.Unsafe (unsafeCoerceAff)
import Control.Monad.Eff (kind Effect)
import Control.Monad.Eff.Class (class MonadEff)
import Control.Monad.Eff.Exception (Error)
import Control.Monad.Error.Class (class MonadThrow, catchError)
import Data.Array (many)
import Data.Foreign (readBoolean, Foreign, F)
import Data.Foreign.Class (class Decode, class Encode, encode, decode)
import Data.Foreign.Generic (defaultOptions, genericDecode, genericEncode)
import Data.Foreign.NullOrUndefined (NullOrUndefined(..), unNullOrUndefined)
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Eq (genericEq)
import Data.Generic.Rep.Show (genericShow)
import Data.Lens.Lens (Lens', lens)
import Data.Maybe (Maybe(..))
import Data.Monoid (class Monoid)
import Data.Newtype (class Newtype, unwrap)
import Data.String (length, take) as S
import Data.String (stripPrefix, Pattern(..), fromCharArray)
import Network.Ethereum.Web3.Types.BigNumber (BigNumber)
import Network.Ethereum.Web3.Types.EtherUnit (Value, Wei)
import Text.Parsing.Parser (runParser)
import Text.Parsing.Parser.Token (hexDigit)

--------------------------------------------------------------------------------
-- * Signed Values
--------------------------------------------------------------------------------

data Sign = Pos | Neg

derive instance eqSign :: Eq Sign


-- | Represents values that can be either positive or negative.
data Signed a = Signed Sign a

instance showSigned :: Show a => Show (Signed a) where
  show (Signed s a) = s' <> show a
    where
      s' = case s of
        Pos -> ""
        Neg -> "-"

instance eqSigned :: Eq a => Eq (Signed a) where
  eq (Signed s a) (Signed s' a') = (s `eq` s') && (a `eq` a')

instance mapSigned :: Functor Signed where
  map f (Signed s a) = Signed s (f a)

-- | Coerce a value into a positive signed value
asSigned :: forall a . a -> Signed a
asSigned a = Signed Pos a

--------------------------------------------------------------------------------
-- * HexString
--------------------------------------------------------------------------------

-- | Represents a base16, utf8 encoded bytestring
newtype HexString = HexString String

instance showHexString :: Show HexString where
  show (HexString hx) = "0x" <> hx

derive newtype instance hexStringEq :: Eq HexString

derive newtype instance semigpStringEq :: Semigroup HexString

derive newtype instance monoidStringEq :: Monoid HexString

instance decodeHexString :: Decode HexString where
  decode s = do
    str <- decode s
    case stripPrefix (Pattern "0x") str of
      Nothing -> pure <<< HexString $ str
      Just res -> pure <<< HexString $ res

instance encodeHexString :: Encode HexString where
  encode = encode <<< append "0x" <<< unHex

unHex :: HexString -> String
unHex (HexString hx) = hx

mkHexString :: String -> Maybe HexString
mkHexString str = HexString <$>
  case stripPrefix (Pattern "0x") str of
    Nothing -> go str
    Just res -> go res
  where
    go s = hush $ runParser s (fromCharArray <$> many hexDigit)

-- | Compute the length of the hex string, which is twice the number of bytes it represents
hexLength :: HexString -> Int
hexLength (HexString hx) = S.length hx

takeHex :: Int -> HexString -> HexString
takeHex n (HexString hx) = HexString $ S.take n hx

--------------------------------------------------------------------------------
-- * Addresses
--------------------------------------------------------------------------------

-- | Represents and Ethereum address, which is a 20 byte `HexString`
newtype Address = Address HexString

derive newtype instance addressShow :: Show Address

derive newtype instance addressEq :: Eq Address

derive newtype instance decodeAddress :: Decode Address

derive newtype instance encodeAddress :: Encode Address

unAddress :: Address -> HexString
unAddress (Address a) = a

mkAddress :: HexString -> Maybe Address
mkAddress hx = if hexLength hx == 40 then Just <<< Address $ hx else Nothing

--------------------------------------------------------------------------------
-- * Block
--------------------------------------------------------------------------------

-- | Refers to a particular block time, used when making calls, transactions, or watching for events.
data CallMode =
    Latest
  | Pending
  | Earliest
  | BlockNumber BigNumber

instance encodeCallMode :: Encode CallMode where
  encode cm = case cm of
    Latest -> encode "latest"
    Pending -> encode "pending"
    Earliest -> encode "earliest"
    BlockNumber n -> encode n

newtype Block
  = Block { difficulty :: BigNumber
          , extraData :: HexString
          , gasLimit :: BigNumber
          , gasUsed :: BigNumber
          , hash :: HexString
          , logsBloom :: HexString
          , miner :: HexString
          , nonce :: HexString
          , number :: BigNumber
          , parentHash :: HexString
          , receiptsRoot :: HexString
          , sha3Uncles :: HexString
          , size :: BigNumber
          , stateRoot :: HexString
          , timestamp :: BigNumber
          , totalDifficulty :: BigNumber
          , transactions :: Array HexString
          , transactionsRoot :: HexString
          , uncles :: Array HexString
          }

derive instance genericBlock :: Generic Block _
derive instance eqBlock :: Eq Block

instance showBlock :: Show Block where
  show = genericShow

instance decodeBlock :: Decode Block where
  decode x = genericDecode (defaultOptions { unwrapSingleConstructors = true }) x

--------------------------------------------------------------------------------
-- * Transaction
--------------------------------------------------------------------------------

newtype Transaction =
  Transaction { hash :: HexString
              , nonce :: BigNumber
              , blockHash :: HexString
              , blockNumber :: BigNumber
              , transactionIndex :: BigNumber
              , from :: Address
              , to :: NullOrUndefined Address
              , value :: Value Wei
              , gas :: BigNumber
              , gasPrice :: BigNumber
              , input :: HexString
              }

derive instance genericTransaction :: Generic Transaction _
derive instance eqTransaction :: Eq Transaction

instance showTransaction :: Show Transaction where
  show = genericShow

instance decodeTransaction :: Decode Transaction where
  decode x = genericDecode (defaultOptions { unwrapSingleConstructors = true }) x

--------------------------------------------------------------------------------
-- * TransactionReceipt
--------------------------------------------------------------------------------

newtype TransactionReceipt = 
  TransactionReceipt { transactionHash :: HexString
                     , transactionIndex :: BigNumber
                     , blockHash :: HexString
                     , blockNumber :: BigNumber
                     , cumulativeGasUsed :: BigNumber
                     , gasUsed :: BigNumber
                     , contractAddress :: NullOrUndefined Address
                     , logs :: Array Change
                     }

derive instance genericTxReceipt :: Generic TransactionReceipt _
derive instance eqTxReceipt :: Eq TransactionReceipt

instance showTxReceipt :: Show TransactionReceipt where
  show = genericShow

instance decodeTxReceipt :: Decode TransactionReceipt where
  decode = genericDecode (defaultOptions { unwrapSingleConstructors = true })

--------------------------------------------------------------------------------
-- * TransactionOptions
--------------------------------------------------------------------------------

newtype TransactionOptions =
  TransactionOptions { from :: NullOrUndefined Address
                     , to :: NullOrUndefined Address
                     , value :: NullOrUndefined (Value Wei)
                     , gas :: NullOrUndefined BigNumber
                     , gasPrice :: NullOrUndefined BigNumber
                     , data :: NullOrUndefined HexString
                     , nonce :: NullOrUndefined BigNumber
                     }

derive instance genericTransactionOptions :: Generic TransactionOptions _

instance showTransactionOptions :: Show TransactionOptions where
  show = genericShow

instance encodeTransactionOptions :: Encode TransactionOptions where
  encode = genericEncode (defaultOptions { unwrapSingleConstructors = true })

defaultTransactionOptions :: TransactionOptions
defaultTransactionOptions =
  TransactionOptions { from : NullOrUndefined Nothing
                     , to : NullOrUndefined Nothing
                     , value : NullOrUndefined Nothing
                     , gas : NullOrUndefined Nothing
                     , gasPrice : NullOrUndefined Nothing
                     , data : NullOrUndefined Nothing
                     , nonce : NullOrUndefined Nothing
                     }

-- * Lens Boilerplate
_from :: Lens' TransactionOptions (Maybe Address)
_from = lens (\(TransactionOptions txOpt) -> unNullOrUndefined $ txOpt.from)
          (\(TransactionOptions txOpts) addr -> TransactionOptions $ txOpts {from = NullOrUndefined addr})

_to :: Lens' TransactionOptions (Maybe Address)
_to = lens (\(TransactionOptions txOpt) -> unNullOrUndefined $ txOpt.to)
           (\(TransactionOptions txOpts) addr -> TransactionOptions $ txOpts {to = NullOrUndefined addr})

_data :: Lens' TransactionOptions (Maybe HexString)
_data = lens (\(TransactionOptions txOpt) -> unNullOrUndefined $ txOpt.data)
           (\(TransactionOptions txOpts) dat -> TransactionOptions $ txOpts {data = NullOrUndefined dat})

_value :: Lens' TransactionOptions (Maybe (Value Wei))
_value = lens (\(TransactionOptions txOpt) -> unNullOrUndefined $ txOpt.value)
           (\(TransactionOptions txOpts) val -> TransactionOptions $ txOpts {value = NullOrUndefined val})

_gas :: Lens' TransactionOptions (Maybe BigNumber)
_gas = lens (\(TransactionOptions txOpt) -> unNullOrUndefined $ txOpt.gas)
           (\(TransactionOptions txOpts) g -> TransactionOptions $ txOpts {gas = NullOrUndefined g})

_gasPrice :: Lens' TransactionOptions (Maybe BigNumber)
_gasPrice = lens (\(TransactionOptions txOpt) -> unNullOrUndefined $ txOpt.gasPrice)
              (\(TransactionOptions txOpts) gp -> TransactionOptions $ txOpts {gasPrice = NullOrUndefined gp})

_nonce :: Lens' TransactionOptions (Maybe BigNumber)
_nonce = lens (\(TransactionOptions txOpt) -> unNullOrUndefined $ txOpt.nonce)
           (\(TransactionOptions txOpts) n -> TransactionOptions $ txOpts {nonce = NullOrUndefined n})

--------------------------------------------------------------------------------
-- * Node Synchronisation
--------------------------------------------------------------------------------

newtype SyncStatus = SyncStatus
    { startingBlock :: BigNumber
    , currentBlock :: BigNumber
    , highestBlock :: BigNumber
    }

derive instance genericSyncStatus :: Generic SyncStatus _
derive instance eqSyncStatus :: Eq SyncStatus

instance decodeSyncStatus :: Decode SyncStatus where
    decode = genericDecode (defaultOptions { unwrapSingleConstructors = true })

instance showSyncStatus :: Show SyncStatus where
    show = genericShow

--------------------------------------------------------------------------------
-- * Web3M
--------------------------------------------------------------------------------

foreign import data ETH :: Effect

-- | A monad for asynchronous Web3 actions

newtype Web3 p e a = Web3 (Aff (eth :: ETH | e) a)

derive newtype instance functorWeb3 :: Functor (Web3 p e)

derive newtype instance applyWeb3 :: Apply (Web3 p e)

derive newtype instance applicativeWeb3 :: Applicative (Web3 p e)

derive newtype instance bindWeb3 :: Bind (Web3 p e)

derive newtype instance monadWeb3 :: Monad (Web3 p e)

derive newtype instance monadEffWeb3 :: MonadEff (eth :: ETH | e) (Web3 p e)

derive newtype instance monadAffWeb3 ∷ MonadAff (eth :: ETH | e) (Web3 p e)

derive newtype instance monadThrowWeb3 :: MonadThrow Error (Web3 p e)

unsafeCoerceWeb3 :: forall p e1 e2 . Web3 p e1 ~> Web3 p e2
unsafeCoerceWeb3 (Web3 action) = Web3 $ unsafeCoerceAff action

--------------------------------------------------------------------------------
-- * Filters
--------------------------------------------------------------------------------

-- | Low-level event filter data structure
newtype Filter = Filter
  { address   :: NullOrUndefined Address
  , topics    :: NullOrUndefined (Array (NullOrUndefined HexString))
  , fromBlock :: NullOrUndefined HexString
  , toBlock   :: NullOrUndefined HexString
  }

derive instance genericFilter :: Generic Filter _

instance showFilter :: Show Filter where
  show = genericShow

instance eqFilter :: Eq Filter where
  eq = genericEq

instance encodeFilter :: Encode Filter where
  encode x = genericEncode (defaultOptions { unwrapSingleConstructors = true }) x

defaultFilter :: Filter
defaultFilter =
  Filter { address: NullOrUndefined Nothing
         , topics: NullOrUndefined Nothing
         , fromBlock: NullOrUndefined Nothing
         , toBlock: NullOrUndefined Nothing
         }

_address :: Lens' Filter (Maybe Address)
_address = lens (\(Filter f) -> unNullOrUndefined $ f.address)
          (\(Filter f) addr -> Filter $ f {address = NullOrUndefined addr})

_topics :: Lens' Filter (Maybe (Array (Maybe HexString)))
_topics = lens (\(Filter f) -> map unNullOrUndefined <$> unNullOrUndefined f.topics)
          (\(Filter f) ts -> Filter $ f {topics = NullOrUndefined (map NullOrUndefined <$> ts)})

_fromBlock :: Lens' Filter (Maybe HexString)
_fromBlock = lens (\(Filter f) -> unNullOrUndefined $ f.fromBlock)
          (\(Filter f) b -> Filter $ f {fromBlock = NullOrUndefined b})

_toBlock :: Lens' Filter (Maybe HexString)
_toBlock = lens (\(Filter f) -> unNullOrUndefined $ f.fromBlock)
          (\(Filter f) b -> Filter $ f {fromBlock = NullOrUndefined b})

-- | Used by the ethereum client to identify the filter you are querying
newtype FilterId = FilterId HexString

derive instance genericFilterId :: Generic FilterId _

instance showFilterId :: Show FilterId where
  show = genericShow

instance eqFilterId :: Eq FilterId where
  eq = genericEq

instance encodeFilterId :: Encode FilterId where
  encode x = genericEncode (defaultOptions { unwrapSingleConstructors = true }) x

instance decodeFilterId :: Decode FilterId where
  decode x = genericDecode (defaultOptions { unwrapSingleConstructors = true }) x

--------------------------------------------------------------------------------
-- * Raw Event Log Changes
--------------------------------------------------------------------------------

-- | Changes pulled by low-level call 'eth_getFilterChanges', 'eth_getLogs',
-- | and 'eth_getFilterLogs'
newtype Change = Change
  { logIndex         :: HexString
  , transactionIndex :: HexString
  , transactionHash  :: HexString
  , blockHash        :: HexString
  , blockNumber      :: HexString
  , address          :: Address
  , data             :: HexString
  , topics           :: Array HexString
  }

derive instance genericChange :: Generic Change _

instance showChange :: Show Change where
  show = genericShow

instance eqChange :: Eq Change where
  eq = genericEq

instance decodeChange :: Decode Change where
  decode x = genericDecode (defaultOptions { unwrapSingleConstructors = true }) x


--------------------------------------------------------------------------------
-- * Json Decode Types
--------------------------------------------------------------------------------

-- | Newtype wrapper around `Maybe` to handle cases where Web3 passes back
-- | either `false` or some data type
newtype FalseOrObject a = FalseOrObject (Maybe a)

derive instance newtypeFalseOrObj :: Newtype (FalseOrObject a) _
derive instance eqFalseOrObj :: Eq a => Eq (FalseOrObject a)
derive instance ordFalseOrObj :: Ord a => Ord (FalseOrObject a)
derive instance genericFalseOrObj :: Generic (FalseOrObject a) _ 

instance showFalseOrObj :: Show a => Show (FalseOrObject a) where
    show x = "(FalseOrObject " <> show (unwrap x) <> ")"

unFalseOrObject :: forall a. FalseOrObject a -> Maybe a
unFalseOrObject (FalseOrObject a) = a

readFalseOrObject :: forall a. (Foreign -> F a) -> Foreign -> F (FalseOrObject a)
readFalseOrObject f value = do
    isBool <- catchError ((\_ -> true) <$> readBoolean value) (\_ -> pure false) 
    if isBool then
        pure $ FalseOrObject Nothing
      else 
        FalseOrObject <<< Just <$> f value

instance decodeFalseOrObj :: Decode a => Decode (FalseOrObject a) where
    decode x = readFalseOrObject decode x

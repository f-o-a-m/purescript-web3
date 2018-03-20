module Network.Ethereum.Web3.Types.Types
       ( Sign(..)
       , Signed(..)
       , asSigned
       , HexString(..)
       , mkHexString
       , unHex
       , hexLength
       , takeHex
       , nullWord
       , Address
       , unAddress
       , mkAddress
       , BlockNumber
       , ChainCursor(..)
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
       , forkWeb3
       , forkWeb3'
       , runWeb3
       , Web3(..)
       , throwWeb3
       , Filter
       , defaultFilter
       , _address
       , _topics
       , _fromBlock
       , _toBlock
       , FilterId
       , EventAction(..)
       , Change(..)
       , FalseOrObject(..)
       , unFalseOrObject
       , SyncStatus(..)
       , MethodName
       , Request
       , mkRequest
       , Response(..)
       , Web3Error(..)
       , RpcError(..)
       , CallError(..)
       ) where

import Prelude

import Control.Alternative ((<|>))
import Control.Monad.Aff (Aff, Fiber, forkAff, liftEff')
import Control.Monad.Aff.Class (class MonadAff, liftAff)
import Control.Monad.Eff (kind Effect)
import Control.Monad.Eff.Class (class MonadEff)
import Control.Monad.Eff.Exception (Error, throwException)
import Control.Monad.Error.Class (class MonadThrow, catchError)
import Control.Monad.Except (ExceptT, runExceptT)
import Control.Monad.Reader (class MonadAsk, class MonadReader, ReaderT, ask, runReaderT)
import Control.Monad.Rec.Class (class MonadRec)
import Data.Array (uncons)
import Data.Either (Either(..))
import Data.Foreign (F, Foreign, ForeignError(..), fail, isNull, readBoolean, readString)
import Data.Foreign.Class (class Decode, class Encode, decode, encode)
import Data.Foreign.Generic (defaultOptions, genericDecode, genericEncode)
import Data.Foreign.Index (readProp)
import Data.Foreign.NullOrUndefined (NullOrUndefined(..), unNullOrUndefined)
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Eq (genericEq)
import Data.Generic.Rep.Show (genericShow)
import Data.Lens.Lens (Lens', Lens, lens)
import Data.Maybe (Maybe(..), isJust)
import Data.Monoid (class Monoid)
import Data.Newtype (class Newtype, unwrap)
import Data.Ordering (invert)
import Data.Set (fromFoldable, member) as Set
import Data.String (length, take, toLower) as S
import Data.String (stripPrefix, Pattern(..), toCharArray)
import Network.Ethereum.Web3.Types.BigNumber (BigNumber)
import Network.Ethereum.Web3.Types.EtherUnit (class EtherUnit, NoPay, Value, Wei, convert)
import Network.Ethereum.Web3.Types.Provider (Provider)

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

instance hexStringEq :: Eq HexString where
  eq (HexString h1) (HexString h2) = S.toLower h1 == S.toLower h2

derive newtype instance hexStringOrd :: Ord HexString
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
mkHexString str | S.length str `mod` 2 /= 0 = Nothing
                | otherwise =
    HexString <$>
      case stripPrefix (Pattern "0x") str of
        Nothing -> if isJust (go <<< toCharArray $ str)
                    then Just $ S.toLower str
                    else Nothing
        Just res -> if isJust (go <<< toCharArray $ res)
                      then Just $ S.toLower res
                      else Nothing
      where
        hexAlph = Set.fromFoldable <<< toCharArray $ "0123456789abcdefABCDEF"
        go s = case uncons s of
          Nothing -> pure unit
          Just {head, tail} ->
            if head `Set.member` hexAlph
              then go tail
              else Nothing

-- | Compute the length of the hex string, which is twice the number of bytes it represents
hexLength :: HexString -> Int
hexLength (HexString hx) = S.length hx

takeHex :: Int -> HexString -> HexString
takeHex n (HexString hx) = HexString $ S.take n hx

nullWord :: HexString
nullWord = HexString "0000000000000000000000000000000000000000000000000000000000000000"

--------------------------------------------------------------------------------
-- * Addresses
--------------------------------------------------------------------------------

-- | Represents and Ethereum address, which is a 20 byte `HexString`
newtype Address = Address HexString

derive newtype instance addressShow :: Show Address
derive newtype instance addressEq :: Eq Address
derive newtype instance addressOrd :: Ord Address
derive newtype instance decodeAddress :: Decode Address
derive newtype instance encodeAddress :: Encode Address

unAddress :: Address -> HexString
unAddress (Address a) = a

mkAddress :: HexString -> Maybe Address
mkAddress hx = if hexLength hx == 40 then Just <<< Address $ hx else Nothing

--------------------------------------------------------------------------------
-- * Block
--------------------------------------------------------------------------------

newtype BlockNumber = BlockNumber BigNumber

derive newtype instance showBlockNumber :: Show BlockNumber
derive newtype instance eqBlockNumber :: Eq BlockNumber
derive newtype instance ordBlockNumber :: Ord BlockNumber
derive newtype instance decodeBlockNumber :: Decode BlockNumber
derive newtype instance encodeBlockNumber :: Encode BlockNumber
derive instance newtypeBlockNumber :: Newtype BlockNumber _

-- | Refers to a particular block time, used when making calls, transactions, or watching for events.
data ChainCursor =
    Latest
  | Pending
  | Earliest
  | BN BlockNumber

derive instance genericChainCursor :: Generic ChainCursor _

instance eqChainCursor :: Eq ChainCursor where
  eq = genericEq

instance showChainCursor :: Show ChainCursor where
  show = genericShow

instance ordChainCursor :: Ord ChainCursor where
  compare Pending Pending = EQ
  compare Latest Latest = EQ
  compare Earliest Earliest = EQ
  compare (BN a) (BN b) = compare a b
  compare _ Pending = LT
  compare Pending Latest = GT
  compare _ Latest = LT
  compare Earliest _ = LT
  compare a b = invert $ compare b a

instance encodeChainCursor :: Encode ChainCursor where
  encode cm = case cm of
    Latest -> encode "latest"
    Pending -> encode "pending"
    Earliest -> encode "earliest"
    BN n -> encode n

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
derive instance newtypeBlock :: Newtype Block _
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
              , blockNumber :: BlockNumber
              , transactionIndex :: BigNumber
              , from :: Address
              , to :: NullOrUndefined Address
              , value :: Value Wei
              , gas :: BigNumber
              , gasPrice :: BigNumber
              , input :: HexString
              }

derive instance genericTransaction :: Generic Transaction _
derive instance newtypeTransaction :: Newtype Transaction _
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
                     , blockNumber :: BlockNumber
                     , cumulativeGasUsed :: BigNumber
                     , gasUsed :: BigNumber
                     , contractAddress :: NullOrUndefined Address
                     , logs :: Array Change
                     }

derive instance genericTxReceipt :: Generic TransactionReceipt _
derive instance newtypeTxReceipt :: Newtype TransactionReceipt _
derive instance eqTxReceipt :: Eq TransactionReceipt

instance showTxReceipt :: Show TransactionReceipt where
  show = genericShow

instance decodeTxReceipt :: Decode TransactionReceipt where
  decode = genericDecode (defaultOptions { unwrapSingleConstructors = true })

--------------------------------------------------------------------------------
-- * TransactionOptions
--------------------------------------------------------------------------------

newtype TransactionOptions u =
  TransactionOptions { from :: NullOrUndefined Address
                     , to :: NullOrUndefined Address
                     , value :: NullOrUndefined (Value u)
                     , gas :: NullOrUndefined BigNumber
                     , gasPrice :: NullOrUndefined BigNumber
                     , data :: NullOrUndefined HexString
                     , nonce :: NullOrUndefined BigNumber
                     }

derive instance genericTransactionOptions :: Generic (TransactionOptions u) _
derive instance newtypeTransactionOptions :: Newtype (TransactionOptions u) _
derive instance eqTransactionOptions :: Eq (TransactionOptions u)

instance showTransactionOptions :: Show (TransactionOptions u) where
  show = genericShow

instance encodeTransactionOptions :: Encode (TransactionOptions u) where
  encode = genericEncode (defaultOptions { unwrapSingleConstructors = true })

defaultTransactionOptions :: TransactionOptions NoPay
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
_from :: forall u. Lens' (TransactionOptions u) (Maybe Address)
_from = lens (\(TransactionOptions txOpt) -> unNullOrUndefined $ txOpt.from)
          (\(TransactionOptions txOpts) addr -> TransactionOptions $ txOpts {from = NullOrUndefined addr})

_to :: forall u. Lens' (TransactionOptions u) (Maybe Address)
_to = lens (\(TransactionOptions txOpt) -> unNullOrUndefined $ txOpt.to)
           (\(TransactionOptions txOpts) addr -> TransactionOptions $ txOpts {to = NullOrUndefined addr})

_data :: forall u. Lens' (TransactionOptions u) (Maybe HexString)
_data = lens (\(TransactionOptions txOpt) -> unNullOrUndefined $ txOpt.data)
           (\(TransactionOptions txOpts) dat -> TransactionOptions $ txOpts {data = NullOrUndefined dat})

_value :: forall u. EtherUnit (Value u) => Lens (TransactionOptions u) (TransactionOptions Wei) (Maybe (Value u)) (Maybe (Value Wei))
_value = lens (\(TransactionOptions txOpt) -> unNullOrUndefined $ txOpt.value)
           (\(TransactionOptions txOpts) val -> TransactionOptions $ txOpts {value = NullOrUndefined $ map convert val})

_gas :: forall u. Lens' (TransactionOptions u) (Maybe BigNumber)
_gas = lens (\(TransactionOptions txOpt) -> unNullOrUndefined $ txOpt.gas)
           (\(TransactionOptions txOpts) g -> TransactionOptions $ txOpts {gas = NullOrUndefined g})

_gasPrice :: forall u. Lens' (TransactionOptions u) (Maybe BigNumber)
_gasPrice = lens (\(TransactionOptions txOpt) -> unNullOrUndefined $ txOpt.gasPrice)
              (\(TransactionOptions txOpts) gp -> TransactionOptions $ txOpts {gasPrice = NullOrUndefined gp})

_nonce :: forall u. Lens' (TransactionOptions u) (Maybe BigNumber)
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
derive instance newtypeSyncStatus :: Newtype SyncStatus _
derive instance eqSyncStatus :: Eq SyncStatus

instance decodeSyncStatus :: Decode SyncStatus where
    decode = genericDecode (defaultOptions { unwrapSingleConstructors = true })

instance showSyncStatus :: Show SyncStatus where
    show = genericShow

--------------------------------------------------------------------------------
-- * Web3
--------------------------------------------------------------------------------

foreign import data ETH :: Effect

-- | A monad for asynchronous Web3 actions

newtype Web3 e a = Web3 (ReaderT Provider (ExceptT Web3Error (Aff (eth :: ETH | e))) a)

derive newtype instance functorWeb3 :: Functor (Web3 e)

derive newtype instance applyWeb3 :: Apply (Web3 e)

derive newtype instance applicativeWeb3 :: Applicative (Web3 e)

derive newtype instance bindWeb3 :: Bind (Web3 e)

derive newtype instance monadWeb3 :: Monad (Web3 e)

derive newtype instance monadEffWeb3 :: MonadEff (eth :: ETH | e) (Web3 e)

derive newtype instance monadAffWeb3 ∷ MonadAff (eth :: ETH | e) (Web3 e)

derive newtype instance monadThrowWeb3 :: MonadThrow Web3Error (Web3 e)

derive newtype instance monadAskWeb3 :: MonadAsk Provider (Web3 e)

derive newtype instance monadReaderWeb3 :: MonadReader Provider (Web3 e)

derive newtype instance monadRecWeb3 :: MonadRec (Web3 e)

throwWeb3 :: forall e a. Error -> Web3 e a
throwWeb3 = liftAff <<< liftEff' <<< throwException

-- | Run an asynchronous `ETH` action
runWeb3 :: forall e a . Provider -> Web3 e a -> Aff (eth :: ETH | e) (Either Web3Error a)
runWeb3 p (Web3 action) = runExceptT (runReaderT action p)

-- | Fork an asynchronous `ETH` action
forkWeb3 :: forall e a .
            Provider
         -> Web3 e a
         -> Aff (eth :: ETH | e) (Fiber (eth :: ETH | e) (Either Web3Error a))
forkWeb3 p = forkAff <<< runWeb3 p

-- | Fork an asynchronous `ETH` action inside Web3 monad
forkWeb3' :: forall e a. Web3 e a -> Web3 e (Fiber (eth :: ETH | e) (Either Web3Error a))
forkWeb3' web3Action = do
  p <- ask
  liftAff $ forkWeb3 p web3Action

--------------------------------------------------------------------------------
-- * Filters
--------------------------------------------------------------------------------

-- | Low-level event filter data structure
newtype Filter = Filter
  { address   :: NullOrUndefined Address
  , topics    :: NullOrUndefined (Array (NullOrUndefined HexString))
  , fromBlock :: ChainCursor
  , toBlock   :: ChainCursor
  }

derive instance genericFilter :: Generic Filter _
derive instance newtypeFilter :: Newtype Filter _

instance showFilter :: Show Filter where
  show = genericShow

instance eqFilter :: Eq Filter where
  eq = genericEq

instance encodeFilter :: Encode Filter where
  encode x = genericEncode (defaultOptions { unwrapSingleConstructors = true }) x

defaultFilter :: Filter
defaultFilter = Filter { address: NullOrUndefined Nothing
                       , topics: NullOrUndefined Nothing
                       , fromBlock: Latest
                       , toBlock: Latest
                       }

_address :: Lens' Filter (Maybe Address)
_address = lens (\(Filter f) -> unNullOrUndefined f.address)
          (\(Filter f) addr -> Filter $ f {address = NullOrUndefined addr})

_topics :: Lens' Filter (Maybe (Array (Maybe HexString)))
_topics = lens (\(Filter f) -> map unNullOrUndefined <$> unNullOrUndefined f.topics)
          (\(Filter f) ts -> Filter $ f {topics = NullOrUndefined (map NullOrUndefined <$> ts)})

_fromBlock :: Lens' Filter ChainCursor
_fromBlock = lens (\(Filter f) -> f.fromBlock)
          (\(Filter f) b -> Filter $ f {fromBlock = b})

_toBlock :: Lens' Filter ChainCursor
_toBlock = lens (\(Filter f) -> f.toBlock)
          (\(Filter f) b -> Filter $ f {toBlock = b})

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
-- | EventAction
--------------------------------------------------------------------------------

-- | Represents a flag to continue or discontinue listening to the filter
data EventAction = ContinueEvent
                 -- ^ Continue to listen events
                 | TerminateEvent
                 -- ^ Terminate event listener

derive instance genericEventAction :: Generic EventAction _

instance showEventAction :: Show EventAction where
  show = genericShow

instance eqEventAction :: Eq EventAction where
  eq = genericEq


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
  , blockNumber      :: BlockNumber
  , address          :: Address
  , data             :: HexString
  , topics           :: Array HexString
  }

derive instance genericChange :: Generic Change _
derive instance newtypeChange :: Newtype Change _

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

--------------------------------------------------------------------------------
-- | Web3 RPC
--------------------------------------------------------------------------------

type MethodName = String

newtype Request =
  Request { jsonrpc :: String
          , id :: Int
          , method :: MethodName
          , params :: Array Foreign
          }

derive instance genericRequest :: Generic Request _

instance encodeRequest :: Encode Request where
  encode x = genericEncode (defaultOptions { unwrapSingleConstructors = true }) x

mkRequest :: MethodName -> Int -> Array Foreign -> Request
mkRequest name reqId ps = Request { jsonrpc : "2.0"
                                  , id : reqId
                                  , method : name
                                  , params : ps
                                  }

newtype Response a = Response (Either Web3Error a)

instance decodeResponse' :: Decode a => Decode (Response a) where
  decode a = Response <$> ((Left <$> decode a) <|> (Right <$> (readProp "result" a >>= decode)))

--------------------------------------------------------------------------------
-- * Errors
--------------------------------------------------------------------------------

data CallError =
  NullStorageError { signature :: String
                   , _data :: HexString
                   }

derive instance genericCallError :: Generic CallError _

instance showCallError :: Show CallError where
  show = genericShow

newtype RpcError =
  RpcError { code     :: Int
           , message  :: String
           }

derive instance newtypeRPCError :: Newtype RpcError _

derive instance genericRpcError :: Generic RpcError _

instance showRpcError :: Show RpcError where
  show = genericShow

instance eqRpcError :: Eq RpcError where
  eq = genericEq

instance decodeRpcError :: Decode RpcError where
  decode x = genericDecode (defaultOptions { unwrapSingleConstructors = true }) x

data Web3Error =
    Rpc RpcError
  | NullError

derive instance genericWeb3Error :: Generic Web3Error _

instance showWeb3Error :: Show Web3Error where
  show = genericShow

instance eqWeb3Error :: Eq Web3Error where
  eq = genericEq

instance decodeWeb3Error :: Decode Web3Error where
  decode x = (map Rpc $ readProp "error" x >>= decode) <|> nullParser
    where
      nullParser = do
        res <- readProp "result" x
        if isNull res
          then pure NullError
          else readString res >>= \r -> fail (TypeMismatch "NullError" r)

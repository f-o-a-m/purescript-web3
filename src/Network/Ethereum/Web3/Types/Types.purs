module Network.Ethereum.Web3.Types.Types
       ( BlockNumber(..)
       , ChainCursor(..)
       , Block(..)
       , Transaction(..)
       , TransactionReceipt(..)
       , TransactionStatus(..)
       , TransactionOptions(..)
       , defaultTransactionOptions
       , _from
       , _to
       , _data
       , _value
       , _gas
       , _gasPrice
       , _nonce
       , forkWeb3
       , forkWeb3'
       , runWeb3
       , Web3(..)
       , Web3Par
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

import Control.Alt (class Alt)
import Control.Alternative (class Alternative, class Plus, (<|>))
import Control.Error.Util (hush)
import Control.Monad.Error.Class (class MonadError, class MonadThrow, catchError)
import Control.Monad.Except (runExcept)
import Control.Monad.Fork.Class (class MonadBracket, class MonadFork, class MonadKill, bracket, fork, join, kill, suspend, uninterruptible, never) as MFork
import Control.Monad.Reader (class MonadAsk, class MonadReader, ReaderT, ask, lift, runReaderT)
import Control.Monad.Rec.Class (class MonadRec)
import Control.Parallel.Class (class Parallel, parallel, sequential)
import Data.Argonaut as A
import Data.Either (Either(..))
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Eq (genericEq)
import Data.Generic.Rep.Show (genericShow)
import Data.Lens.Lens (Lens', Lens, lens)
import Data.Maybe (Maybe(..), maybe)
import Data.Newtype (class Newtype, unwrap)
import Data.Ordering (invert)
import Data.Tuple (Tuple(..))
import Effect.Aff (Aff, Fiber, ParAff, attempt, forkAff, message, throwError)
import Effect.Aff.Class (class MonadAff, liftAff)
import Effect.Class (class MonadEffect, liftEffect)
import Effect.Exception (Error, throwException)
import Foreign (F, Foreign, ForeignError(..), fail, isNull, readBoolean, readString)
import Foreign.Class (class Decode, class Encode, decode, encode)
import Foreign.Generic (defaultOptions, genericDecode, genericDecodeJSON, genericEncode)
import Foreign.Index (readProp)
import Foreign.NullOrUndefined (undefined)
import Foreign.Object as FO
import Network.Ethereum.Types (Address, BigNumber, HexString)
import Network.Ethereum.Web3.Types.EtherUnit (ETHER, Wei)
import Network.Ethereum.Web3.Types.Provider (Provider)
import Network.Ethereum.Web3.Types.TokenUnit (class TokenUnit, MinorUnit, NoPay, Value, convert)
import Simple.JSON (class ReadForeign, class WriteForeign)

--------------------------------------------------------------------------------
-- * Block
--------------------------------------------------------------------------------

newtype BlockNumber = BlockNumber BigNumber

derive instance genericBlockNumber :: Generic BlockNumber _
derive newtype instance showBlockNumber :: Show BlockNumber
derive newtype instance eqBlockNumber :: Eq BlockNumber
derive newtype instance ordBlockNumber :: Ord BlockNumber
derive newtype instance decodeBlockNumber :: Decode BlockNumber
derive newtype instance encodeBlockNumber :: Encode BlockNumber
derive instance newtypeBlockNumber :: Newtype BlockNumber _

instance encodeJsonBlockNumber :: A.EncodeJson BlockNumber where
  encodeJson (BlockNumber bn) = A.encodeJson bn

instance decodeJsonBlockNumber :: A.DecodeJson BlockNumber where
  decodeJson = map BlockNumber <<< A.decodeJson

instance readFHexString :: ReadForeign BlockNumber where
  readImpl = map BlockNumber <<< decode

instance writeFHexString :: WriteForeign BlockNumber where
  writeImpl (BlockNumber bn) = encode bn

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
          , hash :: Maybe HexString
          , logsBloom :: Maybe HexString
          , miner :: HexString
          , nonce :: Maybe HexString
          , number :: Maybe BigNumber
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
              , blockHash :: Maybe HexString
              , blockNumber :: Maybe BlockNumber
              , transactionIndex :: Maybe BigNumber
              , from :: Address
              , to :: Maybe Address
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

data TransactionStatus = Succeeded | Failed

derive instance genericTransactionStatus :: Generic TransactionStatus _
derive instance eqTransactionStatus :: Eq TransactionStatus

instance showTransactionStatus :: Show TransactionStatus where
  show = genericShow

instance decodeTransactionStatus :: Decode TransactionStatus where
  decode x = do
    str <- readString x
    case str of
      "0x1" -> pure Succeeded
      "0x0" -> pure Failed
      otherwise -> fail $ TypeMismatch "TransactionStatus" str

newtype TransactionReceipt =
  TransactionReceipt { transactionHash :: HexString
                     , transactionIndex :: BigNumber
                     , blockHash :: HexString
                     , blockNumber :: BlockNumber
                     , cumulativeGasUsed :: BigNumber
                     , gasUsed :: BigNumber
                     , contractAddress :: Maybe Address
                     , logs :: Array Change
                     , status :: TransactionStatus
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
  TransactionOptions { from :: Maybe Address
                     , to :: Maybe Address
                     , value :: Maybe (Value (u ETHER))
                     , gas :: Maybe BigNumber
                     , gasPrice :: Maybe BigNumber
                     , data :: Maybe HexString
                     , nonce :: Maybe BigNumber
                     }

derive instance genericTransactionOptions :: Generic (TransactionOptions u) _
derive instance newtypeTransactionOptions :: Newtype (TransactionOptions u) _
derive instance eqTransactionOptions :: Eq (TransactionOptions u)

instance showTransactionOptions :: Show (TransactionOptions u) where
  show = genericShow

instance encodeTransactionOptions :: Encode (TransactionOptions u) where
  encode (TransactionOptions txOpts) =
    let encodeMaybe :: forall a. Encode a => Maybe a -> Foreign
        encodeMaybe = maybe undefined encode
    in encode $ FO.fromFoldable [ Tuple "from" $ encodeMaybe txOpts.from
                                , Tuple "to" $ encodeMaybe txOpts.to
                                , Tuple "value" $ encodeMaybe txOpts.value
                                , Tuple "gas" $ encodeMaybe txOpts.gas
                                , Tuple "gasPrice" $ encodeMaybe txOpts.gasPrice
                                , Tuple "data" $ encodeMaybe txOpts.data
                                , Tuple "nonce" $ encodeMaybe txOpts.nonce
                                ]

defaultTransactionOptions :: TransactionOptions NoPay
defaultTransactionOptions =
  TransactionOptions { from: Nothing
                     , to: Nothing
                     , value: Nothing
                     , gas: Nothing
                     , gasPrice: Nothing
                     , data: Nothing
                     , nonce: Nothing
                     }
-- * Lens Boilerplate
_from :: forall u. Lens' (TransactionOptions u) (Maybe Address)
_from = lens (\(TransactionOptions txOpt) -> txOpt.from)
          (\(TransactionOptions txOpts) addr -> TransactionOptions $ txOpts {from = addr})

_to :: forall u. Lens' (TransactionOptions u) (Maybe Address)
_to = lens (\(TransactionOptions txOpt) -> txOpt.to)
           (\(TransactionOptions txOpts) addr -> TransactionOptions $ txOpts {to = addr})

_data :: forall u. Lens' (TransactionOptions u) (Maybe HexString)
_data = lens (\(TransactionOptions txOpt) -> txOpt.data)
           (\(TransactionOptions txOpts) dat -> TransactionOptions $ txOpts {data = dat})

_value :: forall u. TokenUnit (Value (u ETHER)) => Lens (TransactionOptions u) (TransactionOptions MinorUnit) (Maybe (Value (u ETHER))) (Maybe (Value Wei))
_value = lens (\(TransactionOptions txOpt) -> txOpt.value)
           (\(TransactionOptions txOpts) val -> TransactionOptions $ txOpts {value = map convert val})

_gas :: forall u. Lens' (TransactionOptions u) (Maybe BigNumber)
_gas = lens (\(TransactionOptions txOpt) -> txOpt.gas)
           (\(TransactionOptions txOpts) g -> TransactionOptions $ txOpts {gas = g})

_gasPrice :: forall u. Lens' (TransactionOptions u) (Maybe BigNumber)
_gasPrice = lens (\(TransactionOptions txOpt) -> txOpt.gasPrice)
              (\(TransactionOptions txOpts) gp -> TransactionOptions $ txOpts {gasPrice = gp})

_nonce :: forall u. Lens' (TransactionOptions u) (Maybe BigNumber)
_nonce = lens (\(TransactionOptions txOpt) -> txOpt.nonce)
           (\(TransactionOptions txOpts) n -> TransactionOptions $ txOpts {nonce = n})

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

-- | A monad for asynchronous Web3 actions

newtype Web3 a = Web3 (ReaderT Provider Aff a)


unWeb3 :: Web3 ~> ReaderT Provider Aff
unWeb3 (Web3 s) = s

derive newtype instance functorWeb3 :: Functor Web3
derive newtype instance applyWeb3 :: Apply Web3
derive newtype instance applicativeWeb3 :: Applicative Web3
derive newtype instance bindWeb3 :: Bind Web3
derive newtype instance monadWeb3 :: Monad Web3
derive newtype instance monadEffectWeb3 :: MonadEffect Web3
derive newtype instance monadAffWeb3 :: MonadAff Web3
derive newtype instance monadThrowWeb3 :: MonadThrow Error Web3
derive newtype instance monadErrorWeb3 :: MonadError Error Web3
derive newtype instance monadAskWeb3 :: MonadAsk Provider Web3
derive newtype instance monadReaderWeb3 :: MonadReader Provider Web3
derive newtype instance monadRecWeb3 :: MonadRec Web3

instance monadForkWeb3 :: MFork.MonadFork Fiber Web3 where
  suspend = Web3 <<< MFork.suspend <<< unWeb3
  fork = Web3 <<< MFork.fork <<< unWeb3
  join = Web3 <<< lift <<< MFork.join

instance monadKillWeb3 :: MFork.MonadKill Error Fiber Web3 where
  kill e = Web3 <<< MFork.kill e

instance monadBracketWeb3 :: MFork.MonadBracket Error Fiber Web3 where
  bracket acquire release run = Web3 $ MFork.bracket (unWeb3 acquire) (\c a -> unWeb3 (release c a)) (\a -> unWeb3 (run a))
  uninterruptible = Web3 <<< MFork.uninterruptible <<< unWeb3
  never = Web3 MFork.never

newtype Web3Par a = Web3Par (ReaderT Provider ParAff a)

derive newtype instance functorWeb3Par :: Functor Web3Par

derive newtype instance applyWeb3Par :: Apply Web3Par

derive newtype instance applicativeWeb3Par :: Applicative Web3Par

instance monadParWeb3 :: Parallel Web3Par Web3 where
  parallel (Web3 m) = Web3Par (parallel m)
  sequential (Web3Par m) = Web3 (sequential m)

derive newtype instance altParWeb3 :: Alt Web3Par

derive newtype instance plusParWeb3 :: Plus Web3Par

derive newtype instance alternativeParWeb3 :: Alternative Web3Par

throwWeb3 :: forall a. Error -> Web3 a
throwWeb3 = liftEffect <<< throwException

-- | Run an asynchronous `ETH` action
runWeb3 :: forall a . Provider -> Web3 a -> Aff (Either Web3Error a)
runWeb3 p (Web3 action) = attempt (runReaderT action p) >>= case _ of
  Left err -> maybe (throwError err) (pure <<< Left) $ parseMsg $ message err
  Right x -> pure $ Right x
  where
    -- NOTE: it's a bit hacky
    -- for this to work, errors of type `Web3Error` should be converted to json
    -- using `genericEncodeJSON defaultOptions` and then Error
    -- should be created with json string as a message.
    -- see Network.Ethereum.Web3.JsonRPC#asError
    parseMsg :: String -> Maybe Web3Error
    parseMsg msg = hush $ runExcept $ genericDecodeJSON defaultOptions msg

-- | Fork an asynchronous `ETH` action
forkWeb3 :: forall a .
            Provider
         -> Web3 a
         -> Aff (Fiber (Either Web3Error a))
forkWeb3 p = forkAff <<< runWeb3 p

-- | Fork an asynchronous `ETH` action inside Web3 monad
forkWeb3' :: forall a. Web3 a -> Web3 (Fiber (Either Web3Error a))
forkWeb3' web3Action = do
  p <- ask
  liftAff $ forkWeb3 p web3Action

--------------------------------------------------------------------------------
-- * Filters
--------------------------------------------------------------------------------

-- | Low-level event filter data structure
newtype Filter a = Filter
  { address   :: Maybe Address
  , topics    :: Maybe (Array (Maybe HexString))
  , fromBlock :: ChainCursor
  , toBlock   :: ChainCursor
  }

derive instance genericFilter :: Generic (Filter a) _
derive instance newtypeFilter :: Newtype (Filter a) _

instance showFilter :: Show (Filter a) where
  show = genericShow

instance eqFilter :: Eq (Filter a) where
  eq = genericEq

instance encodeFilter :: Encode (Filter a) where
  encode x = genericEncode (defaultOptions { unwrapSingleConstructors = true }) x

defaultFilter :: forall a. Filter a
defaultFilter = Filter { address: Nothing
                       , topics: Nothing
                       , fromBlock: Latest
                       , toBlock: Latest
                       }

_address :: forall a. Lens' (Filter a) (Maybe Address)
_address = lens (\(Filter f) -> f.address)
          (\(Filter f) addr -> Filter $ f {address = addr})

_topics :: forall a. Lens' (Filter a) (Maybe (Array (Maybe HexString)))
_topics = lens (\(Filter f) -> f.topics)
          (\(Filter f) ts -> Filter $ f {topics = ts})

_fromBlock :: forall a. Lens' (Filter a) ChainCursor
_fromBlock = lens (\(Filter f) -> f.fromBlock)
          (\(Filter f) b -> Filter $ f {fromBlock = b})

_toBlock :: forall a. Lens' (Filter a) ChainCursor
_toBlock = lens (\(Filter f) -> f.toBlock)
          (\(Filter f) b -> Filter $ f {toBlock = b})

-- | Used by the ethereum client to identify the filter you are querying
newtype FilterId = FilterId BigNumber

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
  { logIndex         :: BigNumber
  , transactionIndex :: BigNumber
  , transactionHash  :: HexString
  , removed          :: Boolean
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

instance eqCallError :: Eq CallError where
  eq = genericEq

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

instance encodeRpcError :: Encode RpcError where
  encode x = genericEncode (defaultOptions { unwrapSingleConstructors = true }) x

data Web3Error =
    Rpc RpcError
  | RemoteError String
  | ParserError String
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

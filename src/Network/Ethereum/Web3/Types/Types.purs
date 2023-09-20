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
  , Filter(..)
  , defaultFilter
  , _address
  , _topics
  , _fromBlock
  , _toBlock
  , FilterId(..)
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
import Data.Show.Generic (genericShow)
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
import Foreign.Object as FO
import Foreign.Index (readProp)
import Network.Ethereum.Types (Address, BigNumber, HexString)
import Network.Ethereum.Web3.Types.EtherUnit (ETHER, Wei)
import Network.Ethereum.Web3.Types.Provider (Provider)
import Network.Ethereum.Web3.Types.TokenUnit (class TokenUnit, MinorUnit, NoPay, Value, convert)
import Simple.JSON (class ReadForeign, class WriteForeign, readImpl, writeImpl, readJSON', undefined)

--------------------------------------------------------------------------------
-- * Block
--------------------------------------------------------------------------------
newtype BlockNumber = BlockNumber BigNumber

derive instance genericBlockNumber :: Generic BlockNumber _
derive newtype instance showBlockNumber :: Show BlockNumber
derive newtype instance eqBlockNumber :: Eq BlockNumber
derive newtype instance ordBlockNumber :: Ord BlockNumber
derive newtype instance readFBlockNumber :: ReadForeign BlockNumber
derive newtype instance writeFBlockNumber :: WriteForeign BlockNumber
derive instance newtypeBlockNumber :: Newtype BlockNumber _

instance A.EncodeJson BlockNumber where
  encodeJson (BlockNumber bn) = A.encodeJson bn

instance A.DecodeJson BlockNumber where
  decodeJson = map BlockNumber <<< A.decodeJson

-- | Refers to a particular block time, used when making calls, transactions, or watching for events.
data ChainCursor
  = Latest
  | BN BlockNumber

derive instance Generic ChainCursor _
derive instance Eq ChainCursor

instance Show ChainCursor where
  show = genericShow

instance Ord ChainCursor where
  compare Latest Latest = EQ
  compare (BN a) (BN b) = compare a b
  compare _ Latest = LT
  compare a b = invert $ compare b a

instance ReadForeign ChainCursor where
  readImpl f = readLatest <|> readBN
    where
    readLatest = do
      s <- readString f
      if s == "latest" then
        pure Latest
      else
        fail (TypeMismatch "Latest" s)
    readBN = BN <$> readImpl f

instance WriteForeign ChainCursor where
  writeImpl cm = case cm of
    Latest -> writeImpl "latest"
    BN n -> writeImpl n

newtype Block = Block
  { difficulty :: BigNumber
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

derive instance Generic Block _
derive instance Newtype Block _
derive instance Eq Block
derive newtype instance ReadForeign Block
derive newtype instance WriteForeign Block

instance Show Block where
  show = genericShow

--------------------------------------------------------------------------------
-- * Transaction
--------------------------------------------------------------------------------
newtype Transaction = Transaction
  { hash :: HexString
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

derive instance Generic Transaction _
derive instance Newtype Transaction _
derive newtype instance ReadForeign Transaction
derive newtype instance WriteForeign Transaction
derive instance Eq Transaction

instance Show Transaction where
  show = genericShow

--------------------------------------------------------------------------------
-- * TransactionReceipt
--------------------------------------------------------------------------------
data TransactionStatus
  = Succeeded
  | Failed

derive instance Generic TransactionStatus _

derive instance Eq TransactionStatus

instance Show TransactionStatus where
  show = genericShow

instance ReadForeign TransactionStatus where
  readImpl x = do
    str <- readString x
    case str of
      "0x1" -> pure Succeeded
      "0x0" -> pure Failed
      _ -> fail $ TypeMismatch "TransactionStatus" str

instance WriteForeign TransactionStatus where
  writeImpl = case _ of
    Succeeded -> writeImpl "0x1"
    Failed -> writeImpl "0x0"

newtype TransactionReceipt = TransactionReceipt
  { transactionHash :: HexString
  , transactionIndex :: BigNumber
  , blockHash :: HexString
  , blockNumber :: BlockNumber
  , cumulativeGasUsed :: BigNumber
  , gasUsed :: BigNumber
  , contractAddress :: Maybe Address
  , logs :: Array Change
  , status :: TransactionStatus
  }

derive instance Generic TransactionReceipt _
derive instance Newtype TransactionReceipt _
derive instance Eq TransactionReceipt
derive newtype instance ReadForeign TransactionReceipt
derive newtype instance WriteForeign TransactionReceipt

instance Show TransactionReceipt where
  show = genericShow

--------------------------------------------------------------------------------
-- * TransactionOptions
--------------------------------------------------------------------------------
newtype TransactionOptions u = TransactionOptions
  { from :: Maybe Address
  , to :: Maybe Address
  , value :: Maybe (Value (u ETHER))
  , gas :: Maybe BigNumber
  , gasPrice :: Maybe BigNumber
  , data :: Maybe HexString
  , nonce :: Maybe BigNumber
  }

derive instance Generic (TransactionOptions u) _
derive instance Newtype (TransactionOptions u) _
derive instance Eq (TransactionOptions u)

instance Show (TransactionOptions u) where
  show = genericShow

instance WriteForeign (Value (u ETHER)) => WriteForeign (TransactionOptions u) where
  writeImpl (TransactionOptions txOpts) =
    let
      encodeMaybe :: forall a. WriteForeign a => Maybe a -> Foreign
      encodeMaybe = maybe undefined writeImpl
    in
      writeImpl
        $ FO.fromFoldable
            [ Tuple "from" $ encodeMaybe txOpts.from
            , Tuple "to" $ encodeMaybe txOpts.to
            , Tuple "value" $ encodeMaybe txOpts.value
            , Tuple "gas" $ encodeMaybe txOpts.gas
            , Tuple "gasPrice" $ encodeMaybe txOpts.gasPrice
            , Tuple "data" $ encodeMaybe txOpts.data
            , Tuple "nonce" $ encodeMaybe txOpts.nonce
            ]

defaultTransactionOptions :: TransactionOptions NoPay
defaultTransactionOptions =
  TransactionOptions
    { from: Nothing
    , to: Nothing
    , value: Nothing
    , gas: Nothing
    , gasPrice: Nothing
    , data: Nothing
    , nonce: Nothing
    }

-- * Lens Boilerplate
_from :: forall u. Lens' (TransactionOptions u) (Maybe Address)
_from =
  lens (\(TransactionOptions txOpt) -> txOpt.from)
    (\(TransactionOptions txOpts) addr -> TransactionOptions $ txOpts { from = addr })

_to :: forall u. Lens' (TransactionOptions u) (Maybe Address)
_to =
  lens (\(TransactionOptions txOpt) -> txOpt.to)
    (\(TransactionOptions txOpts) addr -> TransactionOptions $ txOpts { to = addr })

_data :: forall u. Lens' (TransactionOptions u) (Maybe HexString)
_data =
  lens (\(TransactionOptions txOpt) -> txOpt.data)
    (\(TransactionOptions txOpts) dat -> TransactionOptions $ txOpts { data = dat })

_value :: forall u. TokenUnit (Value (u ETHER)) => Lens (TransactionOptions u) (TransactionOptions MinorUnit) (Maybe (Value (u ETHER))) (Maybe (Value Wei))
_value =
  lens (\(TransactionOptions txOpt) -> txOpt.value)
    (\(TransactionOptions txOpts) val -> TransactionOptions $ txOpts { value = map convert val })

_gas :: forall u. Lens' (TransactionOptions u) (Maybe BigNumber)
_gas =
  lens (\(TransactionOptions txOpt) -> txOpt.gas)
    (\(TransactionOptions txOpts) g -> TransactionOptions $ txOpts { gas = g })

_gasPrice :: forall u. Lens' (TransactionOptions u) (Maybe BigNumber)
_gasPrice =
  lens (\(TransactionOptions txOpt) -> txOpt.gasPrice)
    (\(TransactionOptions txOpts) gp -> TransactionOptions $ txOpts { gasPrice = gp })

_nonce :: forall u. Lens' (TransactionOptions u) (Maybe BigNumber)
_nonce =
  lens (\(TransactionOptions txOpt) -> txOpt.nonce)
    (\(TransactionOptions txOpts) n -> TransactionOptions $ txOpts { nonce = n })

--------------------------------------------------------------------------------
-- * Node Synchronisation
--------------------------------------------------------------------------------
newtype SyncStatus = SyncStatus
  { startingBlock :: BigNumber
  , currentBlock :: BigNumber
  , highestBlock :: BigNumber
  }

derive instance Generic SyncStatus _
derive instance Newtype SyncStatus _
derive instance Eq SyncStatus
derive newtype instance ReadForeign SyncStatus
derive newtype instance WriteForeign SyncStatus

instance showSyncStatus :: Show SyncStatus where
  show = genericShow

--------------------------------------------------------------------------------
-- * Web3
--------------------------------------------------------------------------------
-- | A monad for asynchronous Web3 actions
newtype Web3 a = Web3 (ReaderT Provider Aff a)

unWeb3 :: Web3 ~> ReaderT Provider Aff
unWeb3 (Web3 s) = s

derive newtype instance Functor Web3
derive newtype instance Apply Web3
derive newtype instance Applicative Web3
derive newtype instance Bind Web3
derive newtype instance Monad Web3
derive newtype instance MonadEffect Web3
derive newtype instance MonadAff Web3
derive newtype instance MonadThrow Error Web3
derive newtype instance MonadError Error Web3
derive newtype instance MonadAsk Provider Web3
derive newtype instance MonadReader Provider Web3
derive newtype instance MonadRec Web3

instance MFork.MonadFork Fiber Web3 where
  suspend = Web3 <<< MFork.suspend <<< unWeb3
  fork = Web3 <<< MFork.fork <<< unWeb3
  join = Web3 <<< lift <<< MFork.join

instance MFork.MonadKill Error Fiber Web3 where
  kill e = Web3 <<< MFork.kill e

instance MFork.MonadBracket Error Fiber Web3 where
  bracket acquire release run = Web3 $ MFork.bracket (unWeb3 acquire) (\c a -> unWeb3 (release c a)) (\a -> unWeb3 (run a))
  uninterruptible = Web3 <<< MFork.uninterruptible <<< unWeb3
  never = Web3 MFork.never

newtype Web3Par a = Web3Par (ReaderT Provider ParAff a)

derive newtype instance Functor Web3Par
derive newtype instance Apply Web3Par
derive newtype instance Applicative Web3Par
derive newtype instance Alt Web3Par
derive newtype instance Plus Web3Par
derive newtype instance Alternative Web3Par

instance Parallel Web3Par Web3 where
  parallel (Web3 m) = Web3Par (parallel m)
  sequential (Web3Par m) = Web3 (sequential m)

throwWeb3 :: forall a. Error -> Web3 a
throwWeb3 = liftEffect <<< throwException

-- | Run an asynchronous `ETH` action
runWeb3 :: forall a. Provider -> Web3 a -> Aff (Either Web3Error a)
runWeb3 p (Web3 action) =
  attempt (runReaderT action p)
    >>= case _ of
      Left err -> maybe (throwError err) (pure <<< Left) $ parseMsg $ message err
      Right x -> pure $ Right x
  where
  -- NOTE: it's a bit hacky
  -- for this to work, errors of type `Web3Error` should be converted to json
  -- using `genericEncodeJSON defaultOptions` and then Error
  -- should be created with json string as a message.
  -- see Network.Ethereum.Web3.JsonRPC#asError
  parseMsg :: String -> Maybe Web3Error
  parseMsg = hush <<< runExcept <<< readJSON'

-- parseMsg msg = hush $ runExcept $ genericDecodeJSON defaultOptions msg

-- | Fork an asynchronous `ETH` action
forkWeb3
  :: forall a
   . Provider
  -> Web3 a
  -> Aff (Fiber (Either Web3Error a))
forkWeb3 p = forkAff <<< runWeb3 p

-- | Fork an asynchronous `ETH` action inside Web3 monad
forkWeb3' :: forall a. Web3 a -> Web3 (Fiber (Either Web3Error a))
forkWeb3' web3Action = do
  p <- ask
  liftAff $ forkWeb3 p web3Action

newtype Filter :: forall k. k -> Type
newtype Filter a = Filter
  { address :: Maybe Address
  , topics :: Maybe (Array (Maybe HexString))
  , fromBlock :: ChainCursor
  , toBlock :: ChainCursor
  }

derive instance Generic (Filter a) _
derive instance Newtype (Filter a) _
derive instance Eq (Filter a)
derive newtype instance ReadForeign (Filter a)
derive newtype instance WriteForeign (Filter a)

instance Show (Filter a) where
  show = genericShow

defaultFilter :: forall a. Filter a
defaultFilter =
  Filter
    { address: Nothing
    , topics: Nothing
    , fromBlock: Latest
    , toBlock: Latest
    }

_address :: forall a. Lens' (Filter a) (Maybe Address)
_address =
  lens (\(Filter f) -> f.address)
    (\(Filter f) addr -> Filter $ f { address = addr })

_topics :: forall a. Lens' (Filter a) (Maybe (Array (Maybe HexString)))
_topics =
  lens (\(Filter f) -> f.topics)
    (\(Filter f) ts -> Filter $ f { topics = ts })

_fromBlock :: forall a. Lens' (Filter a) ChainCursor
_fromBlock =
  lens (\(Filter f) -> f.fromBlock)
    (\(Filter f) b -> Filter $ f { fromBlock = b })

_toBlock :: forall a. Lens' (Filter a) ChainCursor
_toBlock =
  lens (\(Filter f) -> f.toBlock)
    (\(Filter f) b -> Filter $ f { toBlock = b })

-- | Used by the ethereum client to identify the filter you are querying
newtype FilterId = FilterId BigNumber

derive instance Generic FilterId _
derive newtype instance ReadForeign FilterId
derive newtype instance WriteForeign FilterId
derive instance Eq FilterId

instance Show FilterId where
  show = genericShow

--------------------------------------------------------------------------------
-- | EventAction
--------------------------------------------------------------------------------
-- | Represents a flag to continue or discontinue listening to the filter
data EventAction
  = ContinueEvent
  -- ^ Continue to listen events
  | TerminateEvent

-- ^ Terminate event listener
derive instance Generic EventAction _
derive instance Eq EventAction

instance Show EventAction where
  show = genericShow

--------------------------------------------------------------------------------
-- * Raw Event Log Changes
--------------------------------------------------------------------------------
-- | Changes pulled by low-level call `eth_getFilterChanges`, `eth_getLogs`,
-- | and `eth_getFilterLogs`
newtype Change = Change
  { logIndex :: BigNumber
  , transactionIndex :: BigNumber
  , transactionHash :: HexString
  , removed :: Boolean
  , blockHash :: HexString
  , blockNumber :: BlockNumber
  , address :: Address
  , data :: HexString
  , topics :: Array HexString
  }

derive instance Generic Change _
derive instance Newtype Change _
derive newtype instance ReadForeign Change
derive newtype instance WriteForeign Change
derive instance Eq Change

instance showChange :: Show Change where
  show = genericShow

--------------------------------------------------------------------------------
-- * Json Decode Types
--------------------------------------------------------------------------------
-- | Newtype wrapper around `Maybe` to handle cases where Web3 passes back
-- | either `false` or some data type
newtype FalseOrObject a = FalseOrObject (Maybe a)

derive instance Newtype (FalseOrObject a) _
derive instance Eq a => Eq (FalseOrObject a)
derive instance Ord a => Ord (FalseOrObject a)
derive instance Generic (FalseOrObject a) _

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

instance ReadForeign a => ReadForeign (FalseOrObject a) where
  readImpl = readFalseOrObject readImpl

--------------------------------------------------------------------------------
-- | Web3 RPC
--------------------------------------------------------------------------------
type MethodName = String

newtype Request = Request
  { jsonrpc :: String
  , id :: Int
  , method :: MethodName
  , params :: Array Foreign
  }

derive newtype instance readFRequest :: ReadForeign Request
derive newtype instance writeFRequest :: WriteForeign Request

mkRequest :: MethodName -> Int -> Array Foreign -> Request
mkRequest name reqId ps =
  Request
    { jsonrpc: "2.0"
    , id: reqId
    , method: name
    , params: ps
    }

newtype Response a = Response (Either Web3Error a)

instance ReadForeign a => ReadForeign (Response a) where
  readImpl a = Response <$> ((Left <$> readImpl a) <|> (Right <$> (readProp "result" a >>= readImpl)))

--------------------------------------------------------------------------------
-- * Errors
--------------------------------------------------------------------------------
data CallError = NullStorageError
  { signature :: String
  , _data :: HexString
  }

derive instance Generic CallError _
derive instance Eq CallError

instance showCallError :: Show CallError where
  show = genericShow

newtype RpcError = RpcError
  { code :: Int
  , message :: String
  }

derive instance Newtype RpcError _
derive instance Generic RpcError _
derive newtype instance ReadForeign RpcError
derive newtype instance WriteForeign RpcError
derive instance Eq RpcError

instance showRpcError :: Show RpcError where
  show = genericShow

data Web3Error
  = Rpc RpcError
  | RemoteError String
  | ParserError String
  | NullError

derive instance Generic Web3Error _
derive instance Eq Web3Error

instance Show Web3Error where
  show = genericShow

instance ReadForeign Web3Error where
  readImpl x = remoteParser <|> parserErrorParser <|> rpcParser <|> nullParser
    where
    remoteParser = (map RemoteError $ readProp "_remoteError" x >>= readImpl)
    parserErrorParser = (map ParserError $ readProp "_parserError" x >>= readImpl)
    rpcParser = (map Rpc $ readProp "error" x >>= readImpl)
    nullParser = do
      res <- readProp "result" x
      if isNull res then
        pure NullError
      else
        readString res >>= \r -> fail (TypeMismatch "NullError" r)

foreign import _null :: Foreign

instance WriteForeign Web3Error where
  writeImpl x = case x of
    Rpc rpcErr -> writeImpl { error: rpcErr }
    NullError -> writeImpl { result: _null }
    RemoteError _remoteError -> writeImpl { _remoteError }
    ParserError _parserError -> writeImpl { _parserError }

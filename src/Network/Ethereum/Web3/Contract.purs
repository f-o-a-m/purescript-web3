module Network.Ethereum.Web3.Contract
 ( class EventFilter
 , eventFilter
 , EventAction(..)
 , event
 , event'
 , class CallMethod
 , call
 , class TxMethod
 , sendTx
 ) where

import Prelude

import Control.Coroutine (Producer, Consumer, Process, pullFrom, producer, consumer, runProcess, emit)
import Control.Monad.Aff (delay)
import Control.Monad.Aff.Class (liftAff)
import Control.Monad.Eff.Exception (error)
import Control.Monad.Error.Class (throwError)
import Control.Monad.Reader (ReaderT, runReaderT)
import Control.Monad.Rec.Class (class MonadRec)
import Control.Monad.Trans.Class (lift)
import Data.Array (catMaybes, uncons, dropWhile)
import Data.Either (Either(..))
import Data.Functor.Tagged (Tagged, untagged)
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Eq (genericEq)
import Data.Generic.Rep.Show (genericShow)
import Data.Lens ((.~), (^.))
import Data.Maybe (Maybe(..))
import Data.Newtype (wrap, unwrap)
import Data.Symbol (class IsSymbol, SProxy(..), reflectSymbol)
import Data.Time.Duration (Milliseconds(..))
import Data.Traversable (for)
import Data.Tuple (Tuple(..), fst, snd)
import Network.Ethereum.Web3.Api (eth_blockNumber, eth_call, eth_getFilterChanges, eth_getLogs, eth_newFilter, eth_sendTransaction, eth_uninstallFilter)
import Network.Ethereum.Web3.Provider (class IsAsyncProvider)
import Network.Ethereum.Web3.Solidity (class DecodeEvent, class GenericABIDecode, class GenericABIEncode, decodeEvent, genericABIEncode, genericFromData)
import Network.Ethereum.Web3.Types (class EtherUnit, Address, BlockNumber, ChainCursor(..), Change(..), Filter, FilterId, HexString, Web3, _data, _from, _fromBlock, _gas, _to, _toBlock, _value, convert, defaultTransactionOptions, embed, hexadecimal, parseBigNumber, toSelector)
import Type.Proxy (Proxy)

--------------------------------------------------------------------------------
-- * Events
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

class EventFilter a where
    -- | Event filter structure used by low-level subscription methods
    eventFilter :: Proxy a -> Address -> Filter

-- | run `event'` one block at a time.
event :: forall p e a i ni.
         IsAsyncProvider p
      => DecodeEvent i ni a
      => Filter
      -> (a -> ReaderT Change (Web3 p e) EventAction)
      -> Web3 p e Unit
event fltr handler = event' fltr zero handler


-- | Takes a `Filter` and a handler, as well as a windowSize.
-- | It runs the handler over the `eventLogs` using `reduceEventStream`. If no
-- | `TerminateEvent` is thrown, it then transitions to polling.
event' :: forall p e a i ni.
          IsAsyncProvider p
       => DecodeEvent i ni a
       => Filter
       -> Int
       -> (a -> ReaderT Change (Web3 p e) EventAction)
       -> Web3 p e Unit
event' fltr w handler = do
  start <- mkBlockNumber $ fltr ^. _fromBlock
  let initialState = { currentBlock: start
                     , initialFilter: fltr
                     , windowSize: w
                     }
  pollingFromBlock <- runProcess $ reduceEventStream (logsStream initialState) handler
  endingBlock <- mkBlockNumber $ fltr ^. _toBlock
  if endingBlock < pollingFromBlock
     then pure unit
     else do
        filterId <- eth_newFilter $ fltr # _fromBlock .~ BN pollingFromBlock
        void <<< runProcess $ reduceEventStream (pollFilter filterId (fltr ^. _toBlock)) handler


-- | `reduceEventStream` takes a handler and an initial state and attempts to run
-- | the handler over the event stream. If the machine ends without a `TerminateEvent`
-- | result, we return the current state. Otherwise we return `Nothing`.
reduceEventStream :: forall f a.
                     Monad f
                  => MonadRec f
                  => Producer (Array (FilterChange a)) f BlockNumber
                  -> (a -> ReaderT Change f EventAction)
                  -> Process f BlockNumber
reduceEventStream prod handler = eventRunner `pullFrom` prod
  where
    eventRunner :: Consumer (Array (FilterChange a)) f BlockNumber
    eventRunner = consumer \changes -> do
      acts <- processChanges handler changes
      let nos = dropWhile ((==) ContinueEvent <<< fst) acts
      pure $ snd <<< _.head <$> uncons nos


-- | `pollFilter` takes a `FilterId` and a max `ChainCursor` and polls a filter
-- | for changes until the chainHead's `BlockNumber` exceeds the `ChainCursor`,
-- | if ever. There is a minimum delay of 1 second between polls.
pollFilter :: forall p e a i ni .
               IsAsyncProvider p
           => DecodeEvent i ni a
           => FilterId
           -> ChainCursor
           -> Producer (Array (FilterChange a)) (Web3 p e) BlockNumber
pollFilter filterId stop = producer $ do
  bn <- eth_blockNumber
  if BN bn > stop
     then do
       _ <- eth_uninstallFilter filterId
       pure <<< Right $ bn
     else do
       liftAff $ delay (Milliseconds 1000.0)
       changes <- eth_getFilterChanges filterId
       pure <<< Left $ mkFilterChanges changes

-- * Process Filter Changes helpers

type FilterChange a =
  { rawChange :: Change
  , event :: a
  }

mkFilterChanges :: forall i ni a .
                   DecodeEvent i ni a
                => Array Change
                -> Array (FilterChange a)
mkFilterChanges cs = catMaybes $ map pairChange cs
  where
    pairChange rawChange = do
      a <- decodeEvent rawChange
      pure { rawChange : rawChange
           , event : a
           }

processChanges :: forall a f.
                  Monad f
               => (a -> ReaderT Change f EventAction)
               -> Array (FilterChange a)
               -> f (Array (Tuple EventAction BlockNumber))
processChanges handler changes = for changes \c -> do
    act <- runReaderT (handler c.event) c.rawChange
    let (Change change) = c.rawChange
    pure $ Tuple act change.blockNumber


-- * Filter Stream

type FilterStreamState =
  { currentBlock :: BlockNumber
  , initialFilter :: Filter
  , windowSize :: Int
  }

logsStream :: forall p e i ni a.
              IsAsyncProvider p
           => DecodeEvent i ni a
           => FilterStreamState
           -> Producer (Array (FilterChange a)) (Web3 p e) BlockNumber
logsStream currentState = do
    end <- lift <<< mkBlockNumber $ currentState.initialFilter ^. _toBlock
    if currentState.currentBlock > end
       then pure currentState.currentBlock
       else do
            let to' = newTo end currentState.currentBlock currentState.windowSize
                fltr = currentState.initialFilter
                         # _fromBlock .~ BN currentState.currentBlock
                         # _toBlock .~ BN to'
            changes <- lift $ eth_getLogs fltr
            emit $ mkFilterChanges changes
            logsStream currentState {currentBlock = succ to'}
  where
    newTo :: BlockNumber -> BlockNumber -> Int -> BlockNumber
    newTo upper current window = min upper ((wrap $ (unwrap current) + embed window))
    succ :: BlockNumber -> BlockNumber
    succ bn = wrap $ unwrap bn + one


-- | Coerce a 'ChainCursor' to an actual 'BlockNumber'.
mkBlockNumber :: forall p e . IsAsyncProvider p => ChainCursor -> Web3 p e BlockNumber
mkBlockNumber bm = case bm of
  BN bn -> pure bn
  Earliest -> pure <<< wrap $ zero
  _ -> eth_blockNumber

--------------------------------------------------------------------------------
-- * Methods
--------------------------------------------------------------------------------

-- | Class paramaterized by values which are ABIEncodable, allowing the templating of
-- | of a transaction with this value as the payload.
class TxMethod (selector :: Symbol) a where
    -- | Send a transaction for given contract 'Address', value and input data
    sendTx :: forall p e u.
              IsAsyncProvider p
           => IsSymbol selector
           => EtherUnit u
           => Maybe Address
           -- ^ Contract address
           -> Address
           -- ^ from address
           -> u
           -- ^ paymentValue
           -> Tagged (SProxy selector) a
           -- ^ Method data
           -> Web3 p e HexString
           -- ^ 'Web3' wrapped tx hash

class CallMethod (selector :: Symbol) a b where
    -- | Constant call given contract 'Address' in mode and given input data
    call :: forall p e.
            IsAsyncProvider p
         => IsSymbol selector
         => Address
         -- ^ Contract address
         -> Maybe Address
         -- from address
         -> ChainCursor
         -- ^ State mode for constant call (latest or pending)
         -> Tagged (SProxy selector) a
         -- ^ Method data
         -> Web3 p e b
         -- ^ 'Web3' wrapped result

instance txmethodAbiEncode :: (Generic a rep, GenericABIEncode rep) => TxMethod s a where
  sendTx = _sendTransaction

instance callmethodAbiEncode :: (Generic a arep, GenericABIEncode arep, Generic b brep, GenericABIDecode brep) => CallMethod s a b where
  call = _call

_sendTransaction :: forall p a rep e u selector .
                    IsAsyncProvider p
                 => IsSymbol selector
                 => Generic a rep
                 => GenericABIEncode rep
                 => EtherUnit u
                 => Maybe Address
                 -> Address
                 -> u
                 -> Tagged (SProxy selector) a
                 -> Web3 p e HexString
_sendTransaction mto f val dat = do
    let sel = toSelector <<< reflectSymbol $ (SProxy :: SProxy selector)
    eth_sendTransaction <<< txdata $ sel <> (genericABIEncode <<< untagged $ dat)
  where
    defaultGas = parseBigNumber hexadecimal "0x2dc2dc"
    txdata d =
      defaultTransactionOptions # _to .~ mto
                                # _from .~ Just f
                                # _data .~ Just d
                                # _value .~ Just (convert val)
                                # _gas .~ defaultGas

_call :: forall p a arep b brep e selector .
         IsAsyncProvider p
      => IsSymbol selector
      => Generic a arep
      => GenericABIEncode arep
      => Generic b brep
      => GenericABIDecode brep
      => Address
      -> Maybe Address
      -> ChainCursor
      -> Tagged (SProxy selector) a
      -> Web3 p e b
_call t mf cm dat = do
    let sel = toSelector <<< reflectSymbol $ (SProxy :: SProxy selector)
    res <- eth_call (txdata $ sel <> (genericABIEncode <<< untagged $ dat)) cm
    case genericFromData res of
        Nothing -> throwError <<< error $ "Unable to parse result"
        Just x -> pure x
  where
    txdata d  =
      defaultTransactionOptions # _to .~ Just t
                                # _from .~ mf
                                # _data .~ Just d

--------------------------------------------------------------------------------
-- * Machines
--------------------------------------------------------------------------------
--runWhile :: forall f s . Monad f => (s -> Boolean) -> MealyT f s s
--runWhile p = do
--  s <- id
--  if p s
--    then runWhile p
--    else singleton s
--
--final :: forall f s . Monad f => MealyT f s s
--final = mealy $ \s -> do
--  res <- runMealyT id s
--  case res of
--    Halt -> pure $ Emit s halt
--    Emit _ m -> runMealyT m s
--
--test :: forall f . Monad f =>  MealyT f Unit Unit
--test = do
--  a <- fromArray [1,2,3] >>> final
--  wrapEffect $ traceA $ show a

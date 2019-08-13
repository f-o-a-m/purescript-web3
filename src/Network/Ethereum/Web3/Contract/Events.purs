module Network.Ethereum.Web3.Contract.Events
 ( reduceEventStream
 , aquireFilter
 , pollFilter
 , logsStream
 , mkBlockNumber
 , EventHandler
 , FilterStreamState
 , ChangeReceipt
 ) where

import Prelude

import Control.Coroutine (Producer, Consumer, Process, producer, consumer, pullFrom)
import Control.Coroutine.Transducer (Transducer, awaitForever, toProducer, yieldT, (=>=))
import Control.Monad.Fork.Class (bracket)
import Control.Monad.Reader.Trans (ReaderT, runReaderT)
import Control.Monad.Rec.Class (class MonadRec)
import Control.Monad.Trans.Class (lift)
import Control.Parallel (class Parallel)
import Data.Array (catMaybes)
import Data.Either (Either(..))
import Data.Lens ((.~), (^.))
import Data.Maybe (Maybe(..))
import Data.Newtype (wrap, unwrap)
import Data.Time.Duration (Milliseconds(..))
import Data.Traversable (for, for_)
import Data.Tuple (Tuple(..), fst)
import Effect.Aff (delay)
import Effect.Aff.Class (liftAff)
import Network.Ethereum.Core.BigNumber (BigNumber, embed)
import Network.Ethereum.Core.HexString (HexString)
import Network.Ethereum.Web3.Api (eth_blockNumber, eth_getLogs, eth_getFilterChanges, eth_newFilter, eth_uninstallFilter)
import Network.Ethereum.Web3.Solidity (class DecodeEvent, decodeEvent)
import Network.Ethereum.Web3.Types (EventAction(..), BlockNumber, ChainCursor(..), Filter, FilterId, Change(..), _toBlock, _fromBlock, Web3)

--import Debug.Trace (traceM)
--------------------------------------------------------------------------------

  -- * Filter Stream

type FilterStreamState a =
  { currentBlock :: BlockNumber
  , initialFilter :: Filter a
  , windowSize :: Int
  }

produceFilter
  :: forall e.
     FilterStreamState e
  -> Transducer Void (Filter e) Web3 (FilterStreamState e)
produceFilter currentState = do
    let initialToBlock = currentState.initialFilter ^. _toBlock
    --traceM ("[produceFilter] inititailToBlock=" <> show initialToBlock)
    end <- lift <<< mkBlockNumber $ initialToBlock
    --traceM ("[produceFilter] end=" <> show end)
    if currentState.currentBlock > end
      then
        let
          continue = do
            --traceM "Waiting For Blocks..."
            lift $ liftAff $ delay (Milliseconds 3000.0)
            produceFilter currentState
        in
          case initialToBlock of
             Pending -> continue
             Latest -> continue
             _ -> pure currentState
      else do
        let to' = newTo end currentState.currentBlock currentState.windowSize
            fltr = currentState.initialFilter
                     # _fromBlock .~ BN currentState.currentBlock
                     # _toBlock .~ BN to'
        yieldT fltr
        produceFilter $ currentState {currentBlock = succ to'}
  where
    newTo :: BlockNumber -> BlockNumber -> Int -> BlockNumber
    newTo upper current window = min upper (wrap $ unwrap current + embed window)
    succ :: BlockNumber -> BlockNumber
    succ bn = wrap $ unwrap bn + one

makeFilterChanges
  :: forall i ni e.
     DecodeEvent i ni e
  => Transducer (Filter e) (FilterChange e) Web3 Unit
makeFilterChanges = awaitForever \fltr -> do
  --traceM $ "[makeFilterChanges] fltr : " <> show fltr
  changes <- lift $ eth_getLogs fltr
  --traceM $ "[makeFilterChanges] changes : " <> show changes
  for_ (mkFilterChanges changes) yieldT

logsStream
  :: forall i ni e.
     DecodeEvent i ni e
  => FilterStreamState e
  -> Transducer Void (FilterChange e) Web3 (FilterStreamState e)
logsStream initialState =
  fst <$> (produceFilter initialState =>= makeFilterChanges)

--------------------------------------------------------------------------------

type EventHandler f e = e -> ReaderT Change f EventAction

-- | `reduceEventStream` takes a handler and an initial state and attempts to run
-- | the handler over the event stream. If the machine ends without a `TerminateEvent`
-- | result, we return the current state. Otherwise we return `Nothing`.
reduceEventStream
  :: forall f par e.
     Monad f
  => MonadRec f
  => Parallel par f
  => Transducer Void (FilterChange e) f (FilterStreamState e)
  -> EventHandler f e
  -> Process f (Either (FilterStreamState e) ChangeReceipt)
reduceEventStream prod handler =
    (Right <$> eventRunner) `pullFrom` (Left <$> toProducer prod)
  where
    eventRunner :: Consumer (FilterChange e) f ChangeReceipt
    eventRunner = consumer \change -> do
      receipt <- processChange handler change
      pure case receipt.action of
        ContinueEvent -> Nothing
        TerminateEvent -> Just receipt

-- | Aquire a filter using the bracket operator to release the
-- | filter on the node when done.
aquireFilter
  :: forall a b.
     Filter a
  -> (FilterId -> Web3 b)
  -> Web3 Unit
aquireFilter fltr h =
  let aquire = eth_newFilter fltr
      onRelease = const $ void <<< eth_uninstallFilter
      withFilter = void <<< h
  in bracket aquire onRelease withFilter

-- | `pollFilter` takes a `FilterId` and a max `ChainCursor` and polls a filter
-- | for changes until the chainHead's `BlockNumber` exceeds the `ChainCursor`,
-- | if ever. There is a minimum delay of 1 second between polls.
pollFilter
  :: forall a i ni .
     DecodeEvent i ni a
  => FilterId
  -> ChainCursor
  -> Producer (Array (FilterChange a)) (Web3) BlockNumber
pollFilter filterId stop = producer $ do
  bn <- eth_blockNumber
  if BN bn > stop
     then do
       pure <<< Right $ bn
     else do
       liftAff $ delay (Milliseconds 1000.0)
       changes <- eth_getFilterChanges filterId
       pure <<< Left $ mkFilterChanges changes

--------------------------------------------------------------------------------

-- * Process Filter Changes helpers

type FilterChange a =
  { rawChange :: Change
  , event :: a
  }

mkFilterChanges
  :: forall i ni a .
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

type ChangeReceipt =
  { logIndex :: BigNumber
  , blockHash :: HexString
  , blockNumber :: BlockNumber
  , action :: EventAction
  }

processChange
  :: forall e f.
     Monad f
  => EventHandler f e
  -> FilterChange e
  -> f ChangeReceipt
processChange handler c@{rawChange: Change change} = do
  action <- runReaderT (handler c.event) c.rawChange
  pure { logIndex: change.logIndex
       , blockHash: change.blockHash
       , blockNumber: change.blockNumber
       , action
       }

processChanges
  :: forall e f.
     Monad f
  => EventHandler f e
  -> Array (FilterChange e)
  -> f (Array (Tuple EventAction BlockNumber))
processChanges handler changes = for changes \c -> do
    act <- runReaderT (handler c.event) c.rawChange
    let (Change change) = c.rawChange
    pure $ Tuple act change.blockNumber


-- | Coerce a 'ChainCursor' to an actual 'BlockNumber'.
mkBlockNumber :: ChainCursor -> Web3 BlockNumber
mkBlockNumber bm = case bm of
  BN bn -> pure bn
  Earliest -> pure <<< wrap $ zero
  _ -> eth_blockNumber

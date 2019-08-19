module Network.Ethereum.Web3.Contract.Events
 ( reduceEventStream
 , aquireFilter
 , pollFilter
 , logsStream
 , EventHandler
 , FilterStreamState
 , ChangeReceipt
 ) where

import Prelude

import Control.Coroutine (Consumer, Process, producer, consumer, pullFrom)
import Control.Coroutine.Transducer (Transducer, awaitForever, fromProducer, toProducer, yieldT, (=>=))
import Control.Monad.Fork.Class (bracket)
import Control.Monad.Reader.Trans (ReaderT, runReaderT)
import Control.Monad.Rec.Class (class MonadRec)
import Control.Monad.Trans.Class (lift)
import Control.Parallel (class Parallel)
import Data.Array (catMaybes)
import Data.Either (Either(..))
import Data.Lens ((.~), (^.))
import Data.Maybe (Maybe(..))
import Data.Newtype (wrap, unwrap, over)
import Data.Time.Duration (Milliseconds(..))
import Data.Traversable (for_)
import Data.Tuple (fst)
import Effect.Aff (delay)
import Effect.Aff.Class (liftAff)
import Network.Ethereum.Core.BigNumber (BigNumber, embed)
import Network.Ethereum.Core.HexString (HexString)
import Network.Ethereum.Web3.Api (eth_blockNumber, eth_getFilterChanges, eth_getLogs, eth_newFilter, eth_uninstallFilter)
import Network.Ethereum.Web3.Solidity (class DecodeEvent, decodeEvent)
import Network.Ethereum.Web3.Types (BlockNumber(..), ChainCursor(..), Change(..), EventAction(..), Filter, FilterId, Web3, _fromBlock, _toBlock)

--------------------------------------------------------------------------------
-- * Types
--------------------------------------------------------------------------------

type EventHandler f e = e -> ReaderT Change f EventAction

type FilterStreamState e =
  { currentBlock :: BlockNumber
  , initialFilter :: Filter e
  , windowSize :: Int
  , trailBy :: Int
  }

type FilterChange e =
  { rawChange :: Change
  , event :: e
  }

type ChangeReceipt =
  { logIndex :: BigNumber
  , blockHash :: HexString
  , blockNumber :: BlockNumber
  , action :: EventAction
  }

--------------------------------------------------------------------------------
-- * Event Coroutines
--------------------------------------------------------------------------------

-- | Taking an initial state, create a stream of filters used for querying event logs.
-- | The coroutine terminates when it has read up to the `toBlock` field, yielding
-- | the current state.
filterProducer
  :: forall e.
     FilterStreamState e
  -> Transducer Void (Filter e) Web3 (FilterStreamState e)
filterProducer currentState = do
    let -- hang out until the chain makes progress
        waitForMoreBlocks = do
          lift $ liftAff $ delay (Milliseconds 3000.0)
          filterProducer currentState
        -- resume the filter production
        continueTo maxEndBlock = do
          let endBlock = newTo maxEndBlock currentState.currentBlock currentState.windowSize
              fltr = currentState.initialFilter
                       # _fromBlock .~ BN currentState.currentBlock
                       # _toBlock .~ BN endBlock
          yieldT fltr
          filterProducer currentState { currentBlock = succ endBlock }
    chainHead <- lift eth_blockNumber
    -- if the chain head is less than the current block we want to process
    -- then wait until the chain progresses
    if chainHead < currentState.currentBlock
       then waitForMoreBlocks
       -- otherwise try make progress
       else case currentState.initialFilter ^. _toBlock of
         -- consume as many as possible up to the chain head
         Latest -> continueTo $ over BlockNumber (\bn -> bn - embed currentState.trailBy) chainHead
         -- if the original fitler ends at a specific block, consume as many as possible up to that block
         -- or terminate if we're already past it
         BN targetEnd -> 
           let targetEnd' = min targetEnd $ over BlockNumber (\bn -> bn - embed currentState.trailBy) chainHead
           in if currentState.currentBlock <= targetEnd'
                then continueTo targetEnd'
                else pure currentState
  where
    newTo :: BlockNumber -> BlockNumber -> Int -> BlockNumber
    newTo upper current window = min upper (wrap $ unwrap current + embed window)
    succ :: BlockNumber -> BlockNumber
    succ bn = wrap $ unwrap bn + one

-- | Taking in a stream of filters, produce a stream of `FilterChange`s from querying
-- | the getLogs method.
makeFilterChanges
  :: forall i ni e.
     DecodeEvent i ni e
  => Transducer (Filter e) (Array (FilterChange e)) Web3 Unit
makeFilterChanges = awaitForever \fltr -> do
  changes <- lift $ eth_getLogs fltr
  yieldT $ mkFilterChanges changes

-- | A stateless (on the server) stream of filter changes starting from an initial
-- | filter.
logsStream
  :: forall i ni e.
     DecodeEvent i ni e
  => FilterStreamState e
  -> Transducer Void (FilterChange e) Web3 (FilterStreamState e)
logsStream initialState =
  fst <$> (filterProducer initialState =>= stagger makeFilterChanges)

-- | `pollFilter` takes a `FilterId` and a max `ChainCursor` and polls a filter
-- | for changes until the chainHead's `BlockNumber` exceeds the `ChainCursor`,
-- | if ever. There is a minimum delay of 1 second between polls.
pollFilter
  :: forall a i ni .
     DecodeEvent i ni a
  => FilterId
  -> ChainCursor
  -> Transducer Void (Array (FilterChange a)) Web3 BlockNumber
pollFilter filterId stop = fromProducer $ producer $ do
  bn <- eth_blockNumber
  if BN bn > stop
     then do
       pure <<< Right $ bn
     else do
       liftAff $ delay (Milliseconds 1000.0)
       changes <- eth_getFilterChanges filterId
       pure <<< Left $ mkFilterChanges changes


--------------------------------------------------------------------------------
-- * Utils
--------------------------------------------------------------------------------

-- | `reduceEventStream` takes a handler and an initial state and attempts to run
-- | the handler over the event stream. If the machine ends without a `TerminateEvent`
-- | result, we return the current state. Otherwise we return the `ChangeReceipt`
-- | for the event that caused the termination.
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

stagger
  :: forall i o m a par.
     Monad m
  => MonadRec m
  => Parallel par m
  => Transducer i (Array o) m a
  -> Transducer i o m a
stagger osT =
  let trickle = awaitForever \os -> for_ os yieldT
  in fst <$> (osT =>= trickle)

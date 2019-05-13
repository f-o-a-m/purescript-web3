module Network.Ethereum.Web3.Contract.Internal
 ( reduceEventStream
 , pollFilter
 , logsStream
 , mkBlockNumber
 , FilterChange(..)
 , class UncurryFields
 , uncurryFields
 ) where

import Prelude

import Control.Coroutine (Producer, Consumer, Process, pullFrom, producer, consumer, emit)
import Control.Monad.Reader.Trans (ReaderT, runReaderT)
import Control.Monad.Rec.Class (class MonadRec)
import Control.Monad.Trans.Class (lift)
import Data.Array (catMaybes, dropWhile, uncons)
import Data.Either (Either(..))
import Data.Functor.Tagged (Tagged, tagged)
import Data.Lens ((.~), (^.))
import Data.Newtype (un, unwrap, wrap)
import Data.Symbol (class IsSymbol, SProxy(..))
import Data.Time.Duration (Milliseconds(..))
import Data.Traversable (for)
import Data.Tuple (Tuple(..), fst, snd)
import Data.Variant (Variant, expand, inj)
import Effect.Aff (delay)
import Effect.Aff.Class (liftAff)
import Heterogeneous.Folding (class FoldingWithIndex, class FoldlRecord, class HFoldl, hfoldlWithIndex)
import Heterogeneous.Mapping (class MapRecordWithIndex, class Mapping, ConstMapping, hmap)
import Network.Ethereum.Core.BigNumber (BigNumber, embed)
import Network.Ethereum.Web3.Api (eth_blockNumber, eth_getFilterChanges, eth_getLogs)
import Network.Ethereum.Web3.Solidity (class DecodeEvent, decodeEvent)
import Network.Ethereum.Web3.Types (BlockNumber(..), ChainCursor(..), Change(..), EventAction(..), Filter, FilterId, Web3, _fromBlock, _toBlock)
import Prim.RowList as RowList
import Record as Record
import Type.Row as Row




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
pollFilter :: forall a i ni .
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

-- * Process Filter Changes helpers

newtype FilterChange a =
  FilterChange { rawChange :: Change
               , event :: a
               }

filterChangeToIndex :: forall a. FilterChange a -> Tuple BigNumber BigNumber
filterChangeToIndex (FilterChange {rawChange: Change change}) = Tuple (un BlockNumber change.blockNumber) change.logIndex

instance eqFilterChange :: Eq (FilterChange a) where
  eq f1 f2 = filterChangeToIndex f1 `eq` filterChangeToIndex f2

instance ordFilterChange :: Ord (FilterChange a) where
  compare f1 f2 = filterChangeToIndex f1 `compare` filterChangeToIndex f2

mkFilterChanges :: forall i ni a .
                   DecodeEvent i ni a
                => Array Change
                -> Array (FilterChange a)
mkFilterChanges cs = catMaybes $ map pairChange cs
  where
    pairChange rawChange = do
      a <- decodeEvent rawChange
      pure $ FilterChange { rawChange : rawChange
                          , event : a
                          }

processChanges :: forall a f.
                  Monad f
               => (a -> ReaderT Change f EventAction)
               -> Array (FilterChange a)
               -> f (Array (Tuple EventAction BlockNumber))
processChanges handler changes = for changes \(FilterChange c) -> do
    act <- runReaderT (handler c.event) c.rawChange
    let (Change change) = c.rawChange
    pure $ Tuple act change.blockNumber


-- * Filter Stream

type FilterStreamState a =
  { currentBlock :: BlockNumber
  , initialFilter :: Filter a
  , windowSize :: Int
  }

logsStream :: forall i ni a.
              DecodeEvent i ni a
           => FilterStreamState a
           -> Producer (Array (FilterChange a)) Web3 BlockNumber
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
mkBlockNumber :: ChainCursor -> Web3 BlockNumber
mkBlockNumber bm = case bm of
  BN bn -> pure bn
  Earliest -> pure <<< wrap $ zero
  _ -> eth_blockNumber

--------------------------------------------------------------------------------
-- * Uncurry Helper
--------------------------------------------------------------------------------

-- | Useful class for using records as arguments to solidity functions

class UncurryFields fields curried result | curried -> result fields where
  uncurryFields :: Record fields -> curried -> result

instance uncurryFieldsEmpty :: UncurryFields () (Web3 b) (Web3 b) where
  uncurryFields _ = identity

instance uncurryFieldsInductive :: (IsSymbol s, Row.Cons s a before after, Row.Lacks s before, UncurryFields before f b) => UncurryFields after (Tagged (SProxy s) a -> f) b where
  uncurryFields r f =
    let arg = (Record.get (SProxy :: SProxy s) r)
        before = Record.delete (SProxy :: SProxy s) r :: Record before
        partiallyApplied = f (tagged arg :: Tagged (SProxy s) a)
    in uncurryFields before partiallyApplied

--------------------------------------------------------------------------------
-- Multifilters
--------------------------------------------------------------------------------

{-

type MyMultiFilter =
  { filter1 :: Filter e1
  , filter2 :: Filter e2
  ...
  }

-}

data MultiFilterMinToBlock = MultiFilterMinToBlock

instance foldMinToBlock :: HFoldl MultiFilterMinToBlock ChainCursor (Filter e) ChainCursor where
  hfoldl MultiFilterMinToBlock acc f = min acc (f ^. _toBlock)

data MultiFilterMinFromBlock = MultiFilterMinFromBlock

instance foldMinFromBlock :: HFoldl MultiFilterMinFromBlock ChainCursor (Filter e) ChainCursor where
  hfoldl MultiFilterMinFromBlock acc f = min acc (f ^. _fromBlock)

data ModifyFilter = ModifyFilter (forall e. Filter e -> Filter e)

instance modifyFilter :: Mapping ModifyFilter (Filter e) (Filter e) where
  mapping (ModifyFilter f) filter = f filter

data QueryAllLogs = QueryAllLogs

-- can't use type synonyms so must use the explicit record type here
instance queryAllLogs ::
  ( DecodeEvent i ni a
  , IsSymbol sym
  , Row.Union r' b r
  , Row.Cons sym (FilterChange a) r' r
  ) => FoldingWithIndex QueryAllLogs (SProxy sym) (Web3 (Array (Variant r'))) (Filter e) (Web3 (Array (Variant r))) where
  foldingWithIndex QueryAllLogs (prop :: SProxy sym) acc filter = do
    changes :: Array (FilterChange a) <- mkFilterChanges <$> eth_getLogs filter
    (<>) (inj prop <$> changes) <$> (map expand <$> acc)

data MultiFilterStreamState fs =
  MultiFilterStreamState { currentBlock :: BlockNumber
                         , filters :: Record fs
                         , windowSize :: Int
                         }

multiLogsStream
  :: forall fs fsList r.
     RowList.RowToList fs fsList
  => FoldlRecord MultiFilterMinToBlock ChainCursor fsList fs ChainCursor
  => MapRecordWithIndex fsList (ConstMapping ModifyFilter) fs fs
  => FoldlRecord QueryAllLogs (Web3 (Array (Variant ()))) fsList fs (Web3 (Array (Variant r)))
  => MultiFilterStreamState fs
  -> Producer (Array (Variant r)) Web3 BlockNumber
multiLogsStream (MultiFilterStreamState state) = do
  end <- lift <<< mkBlockNumber $ hfoldlWithIndex MultiFilterMinToBlock Pending state.filters
  if state.currentBlock > end
    then pure state.currentBlock
    else do
      let to' = newTo end state.currentBlock state.windowSize
          g :: forall e. Filter e -> Filter e
          g fltr = fltr # _fromBlock .~ BN state.currentBlock
                        # _toBlock .~ BN to'
          fs' = hmap (ModifyFilter g) state.filters
      changes <- lift $ hfoldlWithIndex QueryAllLogs (pure [] :: Web3 (Array (Variant ()))) fs'
      emit changes
      multiLogsStream $ MultiFilterStreamState state {currentBlock = succ to'}
  where
    newTo :: BlockNumber -> BlockNumber -> Int -> BlockNumber
    newTo upper current window = min upper ((wrap $ (unwrap current) + embed window))
    succ :: BlockNumber -> BlockNumber
    succ bn = wrap $ unwrap bn + one

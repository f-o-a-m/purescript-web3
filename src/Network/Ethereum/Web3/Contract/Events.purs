module Network.Ethereum.Web3.Contract.Events
  ( event'
  , pollEvent'
  , reduceEventStream
  , aquireFilter
  , pollFilter
  , logsStream
  , EventHandler
  , FilterStreamState
  , ChangeReceipt
  , FilterChange(..)
  , MultiFilterMinToBlock
  , MultiFilterMinFromBlock
  , ModifyFilter
  , QueryAllLogs
  , MultiFilterStreamState(..)
  , OpenMultiFilter
  , CloseMultiFilter
  , CheckMultiFilter
  ) where

import Prelude
import Control.Coroutine (Process, Consumer, producer, consumer, pullFrom, runProcess)
import Control.Coroutine.Transducer (Transducer, awaitForever, fromProducer, toProducer, yieldT, (=>=))
import Control.Monad.Fork.Class (bracket)
import Control.Monad.Reader.Trans (ReaderT, runReaderT)
import Control.Monad.Rec.Class (class MonadRec)
import Control.Monad.Trans.Class (lift)
import Control.Parallel (class Parallel)
import Data.Array (catMaybes, sort)
import Data.Either (Either(..))
import Data.Lens ((.~), (^.))
import Data.Maybe (Maybe(..))
import Data.Newtype (over)
import Data.Symbol (class IsSymbol, SProxy)
import Data.Functor.Tagged (Tagged, tagged, untagged)
import Data.Traversable (for_)
import Data.Tuple (Tuple(..), fst)
import Data.Variant (Variant, class VariantMatchCases, expand, inj, match)
import Effect.Aff (delay, Milliseconds(..))
import Effect.Aff.Class (liftAff)
import Heterogeneous.Folding (class FoldingWithIndex, class FoldlRecord, hfoldlWithIndex)
import Heterogeneous.Mapping (class MapRecordWithIndex, class Mapping, ConstMapping, hmap)
import Network.Ethereum.Core.BigNumber (BigNumber, embed)
import Network.Ethereum.Core.HexString (HexString)
import Network.Ethereum.Web3.Api (eth_blockNumber, eth_getFilterChanges, eth_getLogs, eth_newFilter, eth_uninstallFilter)
import Network.Ethereum.Web3.Solidity (class DecodeEvent, decodeEvent)
import Network.Ethereum.Web3.Types (BlockNumber(..), ChainCursor(..), Change(..), EventAction(..), Filter, FilterId, Web3, _fromBlock, _toBlock)
import Record as Record
import Type.Proxy (Proxy(..))
import Type.Row as Row
import Prim.RowList as RowList

--------------------------------------------------------------------------------
-- * Types
--------------------------------------------------------------------------------
type EventHandler f e
  = e -> ReaderT Change f EventAction

type FilterStreamState :: forall (k :: Type) (e :: k). Type -> Type
type FilterStreamState e
  = { currentBlock :: BlockNumber
    , initialFilter :: Filter e
    , windowSize :: Int
    , trailBy :: Int
    }

newtype FilterChange a
  = FilterChange
  { rawChange :: Change
  , event :: a
  }

filterChangeToIndex :: forall a. FilterChange a -> Tuple BlockNumber BigNumber
filterChangeToIndex (FilterChange { rawChange: Change change }) = Tuple change.blockNumber change.logIndex

instance eqFilterChange :: Eq (FilterChange a) where
  eq f1 f2 = filterChangeToIndex f1 `eq` filterChangeToIndex f2

instance ordFilterChange :: Ord (FilterChange a) where
  compare f1 f2 = filterChangeToIndex f1 `compare` filterChangeToIndex f2

instance functorFilterChange :: Functor FilterChange where
  map f (FilterChange e) = FilterChange e { event = f e.event }

type ChangeReceipt
  = { logIndex :: BigNumber
    , blockHash :: HexString
    , blockNumber :: BlockNumber
    , action :: EventAction
    }

--------------------------------------------------------------------------------
-- | Takes a record of `Filter`s and a key-corresponding record of `EventHandler`s
-- | to match. It also has options for trailing the chain head by a certain
-- | number of blocks (where applicable), as well as a window size for requesting
-- | larger intervals of blocks (where applicable). When the underlying coroutine
-- | terminates, it will return either the state at the time of termination, or a
-- | `ChangeReceipt` for the event that caused the termination.
event' ::
  forall fs handlers fsList handlersList r1 r.
  FoldlRecord MultiFilterMinFromBlock ChainCursor fsList fs ChainCursor =>
  FoldlRecord MultiFilterMinToBlock ChainCursor fsList fs ChainCursor =>
  RowList.RowToList handlers handlersList =>
  MapRecordWithIndex fsList (ConstMapping ModifyFilter) fs fs =>
  RowList.RowToList fs fsList =>
  VariantMatchCases handlersList r1 (ReaderT Change Web3 EventAction) =>
  Row.Union r1 () r =>
  FoldlRecord QueryAllLogs (Web3 (Array (FilterChange (Variant ())))) fsList fs (Web3 (Array (FilterChange (Variant r)))) =>
  Record fs ->
  Record handlers ->
  { windowSize :: Int, trailBy :: Int } ->
  Web3 (Either (MultiFilterStreamState fs) ChangeReceipt)
event' filters handlerR { windowSize, trailBy } = do
  currentBlock <- case hfoldlWithIndex MultiFilterMinFromBlock Latest filters of
    BN bn -> pure bn
    Latest -> eth_blockNumber
  let
    initialState =
      MultiFilterStreamState
        { currentBlock
        , filters
        , windowSize
        , trailBy
        }
  runProcess $ reduceEventStream (logsStream initialState) handlerR

-- | Takes a record of filters and a key-corresponding record of handlers.
-- | Establishes filters for polling on the server a la the filterIds.
-- | Automatically handles cleaning up resources on the server.
pollEvent' ::
  forall fs fsList handlers handlersList fsIds fsIdsList r r1.
  RowList.RowToList handlers handlersList =>
  RowList.RowToList fs fsList =>
  RowList.RowToList fsIds fsIdsList =>
  MapRecordWithIndex fsList (ConstMapping ModifyFilter) fs fs =>
  FoldlRecord MultiFilterMinFromBlock ChainCursor fsList fs ChainCursor =>
  FoldlRecord MultiFilterMinToBlock ChainCursor fsList fs ChainCursor =>
  VariantMatchCases handlersList r1 (ReaderT Change Web3 EventAction) =>
  FoldlRecord OpenMultiFilter (Web3 (Record ())) fsList fs (Web3 (Record fsIds)) =>
  FoldlRecord CloseMultiFilter (Web3 Unit) fsIdsList fsIds (Web3 Unit) =>
  FoldlRecord CheckMultiFilter (Web3 (Array (FilterChange (Variant ())))) fsIdsList fsIds (Web3 (Array (FilterChange (Variant r)))) =>
  Row.Union r1 () r =>
  Record fs ->
  Record handlers ->
  Web3 (Either BlockNumber ChangeReceipt)
pollEvent' filters handlers =
  let
    processor fids stop =
      runProcess
        $ reduceEventStream (stagger $ pollFilter fids stop) handlers
  in
    aquireFilter filters processor

--------------------------------------------------------------------------------
-- * Event Coroutines
--------------------------------------------------------------------------------
eventRunner ::
  forall handlers handlersList r r1 f.
  RowList.RowToList handlers handlersList =>
  Monad f =>
  VariantMatchCases handlersList r1 (ReaderT Change f EventAction) =>
  Row.Union r1 () r =>
  Record handlers ->
  Consumer (FilterChange (Variant r)) f ChangeReceipt
eventRunner handlersR =
  consumer \change -> do
    receipt <- processChange handlersR change
    pure case receipt.action of
      ContinueEvent -> Nothing
      TerminateEvent -> Just receipt

-- | Taking an initial state, create a stream of filter records used for querying event logs.
-- | The coroutine terminates when it has read up to the `toBlock` field, yielding
-- | the current state.
filterProducer ::
  forall fs fsList.
  RowList.RowToList fs fsList =>
  FoldlRecord MultiFilterMinToBlock ChainCursor fsList fs ChainCursor =>
  MapRecordWithIndex fsList (ConstMapping ModifyFilter) fs fs =>
  MultiFilterStreamState fs ->
  Transducer Void (Record fs) Web3 (MultiFilterStreamState fs)
filterProducer cs@(MultiFilterStreamState currentState) = do
  let -- hang out until the chain makes progress
    waitForMoreBlocks = do
      lift $ liftAff $ delay (Milliseconds 3000.0)
      filterProducer cs

    -- resume the filter production
    continueTo maxEndBlock = do
      let
        endBlock = newTo maxEndBlock currentState.currentBlock currentState.windowSize

        modify :: forall (k :: Type) (e :: k). Filter e -> Filter e
        modify fltr =
          fltr # _fromBlock .~ BN currentState.currentBlock
            # _toBlock
            .~ BN endBlock

        fs' = hmap (ModifyFilter modify) currentState.filters
      yieldT fs'
      filterProducer $ MultiFilterStreamState currentState { currentBlock = succ endBlock }
  chainHead <- lift eth_blockNumber
  -- if the chain head is less than the current block we want to process
  -- then wait until the chain progresses
  if chainHead < currentState.currentBlock then
    waitForMoreBlocks
  -- otherwise try make progress
  else case hfoldlWithIndex MultiFilterMinToBlock Latest currentState.filters of
    -- consume as many as possible up to the chain head
    Latest -> continueTo $ over BlockNumber (_ - embed currentState.trailBy) chainHead
    -- if the original fitler ends at a specific block, consume as many as possible up to that block
    -- or terminate if we're already past it
    BN targetEnd ->
      let
        targetEnd' = min targetEnd $ over BlockNumber (_ - embed currentState.trailBy) chainHead
      in
        if currentState.currentBlock <= targetEnd' then
          continueTo targetEnd'
        else
          pure cs
  where
  newTo :: BlockNumber -> BlockNumber -> Int -> BlockNumber
  newTo upper current window = min upper $ over BlockNumber (_ + embed window) current

  succ :: BlockNumber -> BlockNumber
  succ = over BlockNumber (_ + one)

-- | Taking in a stream of filter records, produce a stream of `FilterChange`s from querying
-- | the getLogs method.
makeFilterChanges ::
  forall fs fsList r.
  RowList.RowToList fs fsList =>
  FoldlRecord QueryAllLogs (Web3 (Array (FilterChange (Variant ())))) fsList fs (Web3 (Array (FilterChange (Variant r)))) =>
  Transducer (Record fs) (Array (FilterChange (Variant r))) Web3 Unit
makeFilterChanges =
  awaitForever \fltrs -> do
    changes <- lift $ hfoldlWithIndex QueryAllLogs (pure [] :: Web3 (Array (FilterChange (Variant ())))) fltrs
    yieldT $ sort changes

-- | A stateless (on the server) stream of filter changes starting from an initial
-- | filter record.
logsStream ::
  forall fs fsList r.
  RowList.RowToList fs fsList =>
  FoldlRecord MultiFilterMinToBlock ChainCursor fsList fs ChainCursor =>
  MapRecordWithIndex fsList (ConstMapping ModifyFilter) fs fs =>
  FoldlRecord QueryAllLogs (Web3 (Array (FilterChange (Variant ())))) fsList fs (Web3 (Array (FilterChange (Variant r)))) =>
  MultiFilterStreamState fs ->
  Transducer Void (FilterChange (Variant r)) Web3 (MultiFilterStreamState fs)
logsStream initialState = fst <$> (filterProducer initialState =>= stagger makeFilterChanges)

-- | Aquire a record of server-side filters using the bracket operator to release the
-- | filters on the node when done.
aquireFilter ::
  forall fs fsList fsIds fsIdsList r b.
  RowList.RowToList fsIds fsIdsList =>
  RowList.RowToList fs fsList =>
  MapRecordWithIndex fsList (ConstMapping ModifyFilter) fs fs =>
  FoldlRecord MultiFilterMinFromBlock ChainCursor fsList fs ChainCursor =>
  FoldlRecord MultiFilterMinToBlock ChainCursor fsList fs ChainCursor =>
  FoldlRecord OpenMultiFilter (Web3 (Record ())) fsList fs (Web3 (Record fsIds)) =>
  FoldlRecord CloseMultiFilter (Web3 Unit) fsIdsList fsIds (Web3 Unit) =>
  FoldlRecord CheckMultiFilter (Web3 (Array (FilterChange (Variant ())))) fsIdsList fsIds (Web3 (Array (FilterChange (Variant r)))) =>
  Record fs ->
  (Record fsIds -> ChainCursor -> Web3 b) ->
  Web3 b
aquireFilter fltrs hs =
  let
    pollingFromBlock = hfoldlWithIndex MultiFilterMinFromBlock Latest fltrs

    fltrs' = hmap (ModifyFilter (_ # _fromBlock .~ pollingFromBlock)) fltrs

    aquire = openMultiFilter fltrs'

    onRelease = const $ hfoldlWithIndex CloseMultiFilter (pure unit :: Web3 Unit)

    stopPollingAt = hfoldlWithIndex MultiFilterMinToBlock Latest fltrs

    withFilter fids = hs fids stopPollingAt
  in
    bracket aquire onRelease withFilter

-- | `pollFilter` takes a `FilterId` and a max `ChainCursor` and polls a filter
-- | for changes until the chainHead's `BlockNumber` exceeds the `ChainCursor`,
-- | if ever. There is a minimum delay of 1 second between polls.
pollFilter ::
  forall fidsList r fids.
  RowList.RowToList fids fidsList =>
  FoldlRecord CheckMultiFilter (Web3 (Array (FilterChange (Variant ())))) fidsList fids (Web3 (Array (FilterChange (Variant r)))) =>
  Record fids ->
  ChainCursor ->
  Transducer Void (Array (FilterChange (Variant r))) Web3 BlockNumber
pollFilter fids stop = do
  fromProducer
    $ producer do
        bn <- eth_blockNumber
        if BN bn > stop then do
          pure <<< Right $ bn
        else do
          liftAff $ delay (Milliseconds 1000.0)
          changes <- hfoldlWithIndex CheckMultiFilter (pure [] :: Web3 (Array (FilterChange (Variant ())))) fids
          pure <<< Left $ sort changes

--------------------------------------------------------------------------------
-- * Utils
--------------------------------------------------------------------------------
-- | Takes a producer of filter changes and a record of handlers and runs the handlers
-- | as a consumer. If one of the handlers chooses to `TerminateEvent`, we return
-- | the change receipt that caused the termination. Otherwise if the producer
-- | terminates and yields an `a`, we return that.
reduceEventStream ::
  forall f par r handlers handlersList r1 a.
  Monad f =>
  MonadRec f =>
  Parallel par f =>
  RowList.RowToList handlers handlersList =>
  VariantMatchCases handlersList r1 (ReaderT Change f EventAction) =>
  Row.Union r1 () r =>
  Transducer Void (FilterChange (Variant r)) f a ->
  Record handlers ->
  Process f (Either a ChangeReceipt)
reduceEventStream prod handlersR = (Right <$> eventRunner handlersR) `pullFrom` (Left <$> toProducer prod)

processChange ::
  forall f r rl r1 r2.
  Monad f =>
  RowList.RowToList r rl =>
  VariantMatchCases rl r1 (ReaderT Change f EventAction) =>
  Row.Union r1 () r2 =>
  Record r ->
  FilterChange (Variant r2) ->
  f ChangeReceipt
processChange handlerRec (FilterChange { rawChange: rawChange@(Change change), event }) = do
  action <- runReaderT (match handlerRec event) rawChange
  pure
    { logIndex: change.logIndex
    , blockHash: change.blockHash
    , blockNumber: change.blockNumber
    , action
    }

-- | Used to find the minimum `toBlock` among a record of filters.
data MultiFilterMinToBlock
  = MultiFilterMinToBlock

instance foldMinToBlock :: FoldingWithIndex MultiFilterMinToBlock (SProxy sym) ChainCursor (Filter e) ChainCursor where
  foldingWithIndex MultiFilterMinToBlock _ acc f = min acc (f ^. _toBlock)

-- | Used to find the minimum `fromBlock` among a record of filters.
data MultiFilterMinFromBlock
  = MultiFilterMinFromBlock

instance foldMinFromBlock :: FoldingWithIndex MultiFilterMinFromBlock (SProxy sym) ChainCursor (Filter e) ChainCursor where
  foldingWithIndex MultiFilterMinFromBlock _ acc f = min acc (f ^. _fromBlock)

-- data ModifyFilter :: Type
data ModifyFilter
  = ModifyFilter (forall (k :: Type) (e :: k). Filter e -> Filter e)

instance modifyFilter :: Mapping ModifyFilter (Filter e) (Filter e) where
  mapping (ModifyFilter f) filter = f filter

-- | Parse an array of `Changes` into an array of `FilterChange`s
-- | that contain this event.
mkFilterChanges ::
  forall i ni e sym r b.
  DecodeEvent i ni e =>
  Row.Cons sym e b r =>
  IsSymbol sym =>
  SProxy sym ->
  Proxy e ->
  Array Change ->
  Array (FilterChange (Variant r))
mkFilterChanges sp _ cs = catMaybes $ map pairChange cs
  where
  pairChange rawChange = do
    a :: e <- decodeEvent rawChange
    pure
      $ FilterChange
          { rawChange: rawChange
          , event: inj sp a
          }

-- | Used to query eth_getLogs for all the filters in record of filters.
data QueryAllLogs
  = QueryAllLogs

instance queryAllLogs ::
  ( DecodeEvent i ni e
  , IsSymbol sym
  , Row.Union r' b r
  , Row.Cons sym e r' r
  ) =>
  FoldingWithIndex QueryAllLogs (SProxy sym) (Web3 (Array (FilterChange (Variant r')))) (Filter e) (Web3 (Array (FilterChange (Variant r)))) where
  foldingWithIndex QueryAllLogs (prop :: SProxy sym) acc filter = do
    changes :: Array (FilterChange (Variant r)) <- mkFilterChanges prop (Proxy :: Proxy e) <$> eth_getLogs (filter :: Filter e)
    (<>) changes <$> (map (map expand) <$> acc)

data MultiFilterStreamState fs
  = MultiFilterStreamState
    { currentBlock :: BlockNumber
    , filters :: Record fs
    , windowSize :: Int
    , trailBy :: Int
    }

data OpenMultiFilter
  = OpenMultiFilter

instance openMultiFilterFold ::
  ( Row.Lacks sym r'
  , IsSymbol sym
  , Row.Union r' b r
  , Row.Cons sym (Tagged e FilterId) r' r
  ) =>
  FoldingWithIndex OpenMultiFilter (SProxy sym) (Web3 (Record r')) (Filter e) (Web3 (Record r)) where
  foldingWithIndex OpenMultiFilter (prop :: SProxy sym) acc filter = do
    filterId <- eth_newFilter filter
    Record.insert prop (tagged filterId :: Tagged e FilterId) <$> acc

openMultiFilter ::
  forall fs fis fsList.
  FoldlRecord OpenMultiFilter (Web3 (Record ())) fsList fs (Web3 (Record fis)) =>
  RowList.RowToList fs fsList =>
  Record fs ->
  Web3 (Record fis)
openMultiFilter = hfoldlWithIndex OpenMultiFilter (pure {} :: Web3 (Record ()))

data CheckMultiFilter
  = CheckMultiFilter

instance checkMultiFilterLogs ::
  ( DecodeEvent i ni e
  , IsSymbol sym
  , Row.Union r' b r
  , Row.Cons sym e r' r
  ) =>
  FoldingWithIndex CheckMultiFilter (SProxy sym) (Web3 (Array (FilterChange (Variant r')))) (Tagged e FilterId) (Web3 (Array (FilterChange (Variant r)))) where
  foldingWithIndex CheckMultiFilter (prop :: SProxy sym) acc filterId = do
    changes :: Array (FilterChange (Variant r)) <- mkFilterChanges prop (Proxy :: Proxy e) <$> eth_getFilterChanges (untagged filterId)
    (<>) changes <$> (map (map expand) <$> acc)

data CloseMultiFilter
  = CloseMultiFilter

instance closeMultiFilterFold ::
  ( IsSymbol sym
    ) =>
  FoldingWithIndex CloseMultiFilter (SProxy sym) (Web3 Unit) (Tagged e FilterId) (Web3 Unit) where
  foldingWithIndex CloseMultiFilter (_ :: SProxy sym) acc filter = do
    void $ eth_uninstallFilter $ untagged filter
    acc

-- Should belong to coroutines lib.
stagger ::
  forall i o m a par.
  Monad m =>
  MonadRec m =>
  Parallel par m =>
  Transducer i (Array o) m a ->
  Transducer i o m a
stagger osT =
  let
    trickle = awaitForever \os -> for_ os yieldT
  in
    fst <$> (osT =>= trickle)

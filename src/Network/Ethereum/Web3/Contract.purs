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

import Control.Monad.Aff (delay)
import Control.Monad.Aff.Class (liftAff)
import Control.Monad.Eff.Exception (error)
import Control.Monad.Error.Class (throwError)
import Control.Monad.Reader (ReaderT, runReaderT)
import Data.Array (catMaybes, notElem)
import Data.Functor.Tagged (Tagged, untagged)
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Eq (genericEq)
import Data.Generic.Rep.Show (genericShow)
import Data.Lens ((.~), (^.))
import Data.Machine.Mealy (MealyT, Step(..), mealy, stepMealy, wrapEffect)
import Data.Maybe (Maybe(..))
import Data.Newtype (wrap, unwrap)
import Data.Symbol (class IsSymbol, SProxy(..), reflectSymbol)
import Data.Time.Duration (Milliseconds(..))
import Data.Traversable (for)
import Network.Ethereum.Web3.Api (eth_blockNumber, eth_call, eth_getFilterChanges, eth_getLogs, eth_newFilter, eth_sendTransaction, eth_uninstallFilter)
import Network.Ethereum.Web3.Provider (class IsAsyncProvider)
import Network.Ethereum.Web3.Solidity (class DecodeEvent, class GenericABIDecode, class GenericABIEncode, decodeEvent, genericABIEncode, genericFromData)
import Network.Ethereum.Web3.Types (class EtherUnit, Address, BlockMode(..), BlockNumber, Change, Filter, FilterId, HexString, Web3, _data, _from, _fromBlock, _gas, _to, _toBlock, _value, convert, defaultTransactionOptions, embed, hexadecimal, parseBigNumber, toSelector)
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
    eventFilter :: Proxy a -> {topics :: Array (Maybe HexString)}

-- | run 'event'' one block at a time.
event :: forall p e a i ni.
         IsAsyncProvider p
      => DecodeEvent i ni a
      => Filter
      -> Int
      -> (a -> ReaderT Change (Web3 p e) EventAction)
      -> Web3 p e Unit
event fltr w handler = event' fltr 1 handler

-- | 'event'' takes a 'Filter' and a handler, as well as a windowSize.
-- | It runs the handler over the 'eventLogs' using 'reduceEventStream'. If no
-- | 'TerminateEvent' is thrown, it then transitions to polling.
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
  mbn <- reduceEventStream playLogs handler initialState
  case mbn of
    Nothing -> pure unit
    Just newStart -> do
      filterId <- eth_newFilter $ fltr # _fromBlock .~ BN newStart.currentBlock
      void $ reduceEventStream (pollFilter filterId (fltr ^. _toBlock)) handler unit

-- | 'reduceEventStream' takes a handler and an initial state and attempts to run
-- | the handler over the event stream. If the machine ends without a 'TerminateEvent'
-- | result, we return the current state. Otherwise we return 'Nothing'.
reduceEventStream :: forall f s a i ni.
                     Monad f
                  => DecodeEvent i ni a
                  => MealyT f s (Array (FilterChange a))
                  -> (a -> ReaderT Change f EventAction)
                  -> s
                  -> f (Maybe s)
reduceEventStream m handler s = do
  res <- stepMealy s m
  case res of
    Halt -> pure <<< Just $ s
    Emit changes m' -> do
      acts <- processChanges handler changes
      if TerminateEvent `notElem` acts
         then reduceEventStream m' handler s
         else pure Nothing

-- | 'playLogs' streams the 'filterStream' and calls eth_getLogs on these
-- | 'Filter' objects.
playLogs :: forall p e a i ni.
            IsAsyncProvider p
         => DecodeEvent i ni a
         => MealyT (Web3 p e) FilterStreamState (Array (FilterChange a))
playLogs  = do
  filter <- filterStream
  changes <- wrapEffect $ eth_getLogs filter
  pure $ mkFilterChanges changes

-- | 'pollFilter' takes a 'FilterId' and a max 'BlockMode' and polls a filter
-- | for changes until the chainHead's 'BlockNumber' exceeds the 'BlockMode',
-- | if ever. There is a minimum delay of 1 second between polls.
pollFilter :: forall p e a i ni s.
               IsAsyncProvider p
            => DecodeEvent i ni a
            => FilterId
            -> BlockMode
            -> MealyT (Web3 p e) s (Array (FilterChange a))
pollFilter filterId stop = mealy $ \s -> do
  bn <- eth_blockNumber
  if BN bn > stop
     then eth_uninstallFilter filterId *> pure Halt
     else do
       liftAff $ delay (Milliseconds 1000.0)
       changes <- eth_getFilterChanges filterId
       pure $ Emit (mkFilterChanges changes) (pollFilter filterId stop)

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

processChanges :: forall i ni a f.
                  DecodeEvent i ni a
               => Monad f
               => (a -> ReaderT Change f EventAction)
               -> Array (FilterChange a)
               -> f (Array EventAction)
processChanges handler changes = for changes \c ->
    runReaderT (handler c.event) c.rawChange


-- * Filter Stream

type FilterStreamState =
  { currentBlock :: BlockNumber
  , initialFilter :: Filter
  , windowSize :: Int
  }

-- | 'filterStream' is a machine which represents taking an initial filter
-- | over a range of blocks b1, ... bn (where bn is possibly 'Latest' or 'Pending',
-- | but b1 is an actual 'BlockNumber'), and making a stream of filter objects
-- | which cover this filter in intervals of size 'windowSize'. The machine
-- | halts whenever the 'fromBlock' of a spanning filter either (1) excedes the
-- | initial filter's 'toBlock' or (2) is greater than the chain head's 'BlockNumber'.
filterStream :: forall p e.
                IsAsyncProvider p
             => MealyT (Web3 p e) FilterStreamState Filter
filterStream = mealy filterStream'
  where
    newTo :: BlockNumber -> BlockNumber -> Int -> BlockNumber
    newTo upper current window = min upper ((wrap $ (unwrap current) + embed window))
    succ :: BlockNumber -> BlockNumber
    succ bn = wrap $ unwrap bn + one
    filterStream' = \s -> do
      end <- mkBlockNumber $ s.initialFilter ^. _toBlock
      if s.currentBlock > end
         then pure Halt
         else let to' = newTo end s.currentBlock s.windowSize
                  fltr = s.initialFilter
                           # _fromBlock .~ BN s.currentBlock
                           # _toBlock .~ BN to'
              in pure $ Emit fltr $ mealy \s' ->
                   filterStream' s' {currentBlock = succ to'}

-- | Coerce a 'BlockMode' to an actual 'BlockNumber'.
mkBlockNumber :: forall p e . IsAsyncProvider p => BlockMode -> Web3 p e BlockNumber
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
         -> BlockMode
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
      -> BlockMode
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

module Network.Ethereum.Web3.Contract
 ( class EventFilter
 , eventFilter
 , EventAction(..)
 , event
 , runEventsFromBlock
 , runEventsBounded
 , class CallMethod
 , call
 , class TxMethod
 , sendTx
 ) where

import Prelude

import Control.Monad.Aff (Fiber, delay)
import Control.Monad.Aff.Class (liftAff)
import Control.Monad.Eff.Exception (error)
import Control.Monad.Error.Class (throwError)
import Control.Monad.Reader (ReaderT, runReaderT)
import Data.Array (catMaybes, notElem)
import Data.Functor.Tagged (Tagged, untagged)
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Eq (genericEq)
import Data.Generic.Rep.Show (genericShow)
import Data.Lens ((.~))
import Data.Machine.Mealy (MealyT, Step(..), halt, mealy, runMealy, runMealyT, wrapEffect)
import Data.Maybe (Maybe(..))
import Data.Newtype (wrap, unwrap)
import Data.Symbol (class IsSymbol, SProxy(..), reflectSymbol)
import Data.Time.Duration (Milliseconds(..))
import Data.Traversable (for)
import Data.Tuple (Tuple(..))
import Network.Ethereum.Web3.Api (eth_blockNumber, eth_call, eth_getFilterChanges, eth_getLogs, eth_newFilter, eth_sendTransaction, eth_uninstallFilter)
import Network.Ethereum.Web3.Provider (class IsAsyncProvider, forkWeb3')
import Network.Ethereum.Web3.Solidity (class DecodeEvent, class GenericABIDecode, class GenericABIEncode, decodeEvent, genericABIEncode, genericFromData)
import Network.Ethereum.Web3.Types (class EtherUnit, Address, BlockMode(..), BlockNumber, Change, ETH, Filter, FilterId, HexString, Web3, _data, _from, _fromBlock, _gas, _to, _toBlock, _value, convert, defaultTransactionOptions, embed, hexadecimal, parseBigNumber, toSelector)
import Type.Proxy (Proxy(..))

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

-- | 'event' creates a new event filter and starts listening for events
-- | from the chain head until a 'TerminateEvent' result.
event :: forall p e a i ni.
         IsAsyncProvider p
      => DecodeEvent i ni a
      => EventFilter a
      => Address
      -> (a -> ReaderT Change (Web3 p e) EventAction)
      -> Web3 p e (Fiber (eth :: ETH | e) Unit)
event addr handler = do
  filterId <- eth_newFilter $ eventFilter (Proxy :: Proxy a) addr
  forkWeb3' (Proxy :: Proxy p) <<< runMealy $ pollChanges filterId handler

-- | 'runEventsFromBlock' will replay events from a lower bound 'BlockNumber'
-- | making batch requests for changes until it catches up with the chain head,
-- | at which point it will begin polling for changes.
runEventsFromBlock :: forall p e a i ni.
                      IsAsyncProvider p
                   => DecodeEvent i ni a
                   => EventFilter a
                   => Address
                   -> BlockNumber
                   -- ^ lower bound block number
                   -> Int
                   -- ^ window size
                   -> (a -> ReaderT Change (Web3 p e) Unit)
                   -> Web3 p e (Fiber (eth :: ETH | e) Unit)
runEventsFromBlock addr leftBound window handler = do
    let pa = Proxy :: Proxy a
        handler' = \a -> ContinueEvent <$ handler a
    bn <- catchUpEvents addr leftBound window handler'
    forkWeb3' (Proxy :: Proxy p) $ runMealy (filterChangesStream addr bn handler')

-- | 'runEventsBounded' processes events from a 'BlockNumber' to a 'BlockNumber'
-- | batching the changes using the window range until it finishes or reaches a
-- | 'TerminateEvent' result.
runEventsBounded :: forall p e a i ni.
                    IsAsyncProvider p
                 => DecodeEvent i ni a
                 => EventFilter a
                 => Address
                 -> BlockNumber
                 -- ^ fromBlock
                 -> BlockNumber
                 -- ^ toBlock
                 -> Int
                 -- ^ window
                 -> (a -> ReaderT Change (Web3 p e) EventAction)
                 -> Web3 p e (Fiber (eth :: ETH | e) Unit)
runEventsBounded addr from to window handler =
  forkWeb3' (Proxy :: Proxy p) <<< void $ playEvents addr from (BN to) window handler

-- Internal Event Helpers

type FilterStreamState =
  { currentBlock :: BlockNumber
  , endingBlock :: BlockMode
  , windowSize :: Int
  }

-- | 'catchUpEvents' proceeses events from a lower bound with a handler until
-- | the chain head is reached or until a 'TerminateEvent' result. The 'BlockNumber'
-- | returned is the latest block that was not processed.
catchUpEvents :: forall p e a i ni.
                 IsAsyncProvider p
              => DecodeEvent i ni a
              => EventFilter a
              => Address
              -> BlockNumber
              -- ^ lower bound block number
              -> Int
              -- ^ window size
              -> (a -> ReaderT Change (Web3 p e) EventAction)
              -> Web3 p e BlockNumber
catchUpEvents addr bn window handler = playEvents addr bn Latest window handler

-- | 'playEvents' starts playing the event logs from a starting 'BlockNumber'
-- | until it has caught up to the latest. It then returns the most recent 'BlockNumber'
-- | that has not been processed.
playEvents :: forall p e a i ni.
              IsAsyncProvider p
           => DecodeEvent i ni a
           => EventFilter a
           => Address
           -> BlockNumber
           -- ^ lower bound block number
           -> BlockMode
           -- ^ stopping block mode
           -> Int
           -- ^ window size
           -> (a -> ReaderT Change (Web3 p e) EventAction)
           -> Web3 p e BlockNumber
playEvents addr bn bm w handler =
    let s = { currentBlock: bn
            , endingBlock: bm
            , windowSize: w
            }
        pa = Proxy :: Proxy a
        eventMachine = leftBoundedFilterStream pa addr
    in go eventMachine s
  where
    go machine state = do
      a <- runMealyT machine state
      case a of
        Halt -> pure state.currentBlock
        Emit filter continue -> do
          changes <- eth_getLogs filter
          acts <- processChanges handler changes
          if (TerminateEvent `notElem` acts)
            then go continue state
            else go halt state {currentBlock = wrap $ unwrap state.currentBlock + one}

-- | 'filterChangesStream' creates a filter from a  lower bound 'BlockNumber' to latest
-- | then polls the node for changes once per second.
filterChangesStream :: forall p e a s i ni.
                       IsAsyncProvider p
                    => DecodeEvent i ni a
                    => EventFilter a
                    => Address
                    -> BlockNumber
                    -> (a -> ReaderT Change (Web3 p e) EventAction)
                    -> MealyT (Web3 p e) s Unit
filterChangesStream addr from handler = do
    let pa = Proxy :: Proxy a
    filterId <- wrapEffect <<< eth_newFilter $ eventFilter pa addr # _fromBlock .~ Just (BN from)
    pollChanges filterId handler

-- | 'leftBoundedFilterStream' creates filters for batch fetching event logs.
-- | It will play until 'endingBlock' is reached, or the chain head depending on
-- | the 'BlockMode' in 'FilterStreamState'.
leftBoundedFilterStream
  :: forall p e a.
     IsAsyncProvider p
  => EventFilter a
  => Proxy a
  -> Address
  -> MealyT (Web3 p e) FilterStreamState Filter
leftBoundedFilterStream pa addr =
    mealy $ leftBoundedFilterStream' pa addr
  where
    newTo :: BlockNumber -> BlockNumber -> Int -> BlockNumber
    newTo bn current window = min bn ((wrap $ (unwrap current) + embed window))
    succ :: BlockNumber -> BlockNumber
    succ bn = wrap $ unwrap bn + one
    leftBoundedFilterStream' pa' addr' = \s -> do
      end <- case s.endingBlock of
        BN bn -> pure bn
        Earliest -> pure <<< wrap $ zero
        _ -> eth_blockNumber
      if s.currentBlock > end
         then pure Halt
         else let to' = newTo end s.currentBlock s.windowSize
                  fltr = eventFilter pa' addr'
                           # _fromBlock .~ (Just <<< BN $ s.currentBlock)
                           # _toBlock .~ (Just $ BN to')
              in pure $ Emit fltr $ mealy \s' ->
                   leftBoundedFilterStream' pa' addr' s' {currentBlock = succ to'}

pollChanges :: forall p e a i ni s.
               IsAsyncProvider p
            => DecodeEvent i ni a
            => FilterId
            -> (a -> ReaderT Change (Web3 p e) EventAction)
            -> MealyT (Web3 p e) s Unit
pollChanges filterId handler = mealy $ \s -> do
    liftAff $ delay (Milliseconds 1000.0)
    changes <- eth_getFilterChanges filterId
    acts <- processChanges handler changes
    if TerminateEvent `notElem` acts
       then eth_uninstallFilter filterId *> pure Halt
       else pure $ Emit unit $ pollChanges filterId handler

processChanges :: forall i ni a f.
                  DecodeEvent i ni a
               => Monad f
               => (a -> ReaderT Change f EventAction)
               -> Array Change
               -> f (Array EventAction)
processChanges handler changes = for (catMaybes $ map pairChange changes) \(Tuple changeWithMeta changeEvent) ->
    runReaderT (handler changeEvent) changeWithMeta
  where
    pairChange :: Change -> Maybe (Tuple Change a)
    pairChange rawChange = do
      (change :: a) <- decodeEvent rawChange
      pure (Tuple rawChange change)

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

module Network.Ethereum.Web3.Contract where

import Prelude

import Control.Monad.Aff (Fiber, delay)
import Control.Monad.Aff.Class (liftAff)
import Control.Monad.Eff.Exception (error)
import Control.Monad.Error.Class (throwError)
import Control.Monad.Reader (ReaderT, runReaderT)
import Data.Array (notElem, catMaybes)
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Eq (genericEq)
import Data.Generic.Rep.Show (genericShow)
import Data.Lens ((.~))
import Data.Maybe (Maybe(..))
import Data.Time.Duration (Milliseconds(..))
import Data.Traversable (for)
import Data.Tuple (Tuple(..))
import Network.Ethereum.Web3.Api (eth_call, eth_call_async, eth_getFilterChanges, eth_newFilter, eth_sendTransaction, eth_sendTransaction_async, eth_uninstallFilter)
import Network.Ethereum.Web3.Solidity.AbiEncoding (class ABIEncoding, fromData, toDataBuilder)
import Network.Ethereum.Web3.Types (Address, BigNumber, CallMode, Change(..), ETH, Filter, FilterId, HexString, Provider, Web3M, Web3MA, _data, _from, _gas, _to, _value, defaultTransactionOptions, hexadecimal, parseBigNumber, forkWeb3MA)
import Type.Proxy (Proxy(..))


import Debug.Trace (traceA)
--------------------------------------------------------------------------------
-- | Events
--------------------------------------------------------------------------------

data EventAction = ContinueEvent
                 -- ^ Continue to listen events
                 | TerminateEvent
                 -- ^ Terminate event listener

derive instance genericEventAction :: Generic EventAction _

instance showEventAction :: Show EventAction where
  show = genericShow

instance eqEventAction :: Eq EventAction where
  eq = genericEq

class ABIEncoding a <= EventFilter a where
    -- | Event filter structure used by low-level subscription methods
    eventFilter :: Proxy a -> Address -> Filter


-- | Default implementation for Event class
event :: forall e a.
          EventFilter a
       => Provider
       -> Address
       -> (a -> ReaderT Change (Web3MA e) EventAction)
       -> Web3MA e (Fiber (eth :: ETH | e) Unit)
event p addr handler = do
    fid <- eth_newFilter (eventFilter (Proxy :: Proxy a) addr)
    liftAff <<< forkWeb3MA p $ do
      traceA "about to loop"
      loop fid
      traceA "uninstalling filter"
      _ <- eth_uninstallFilter fid
      pure unit
  where
    loop :: FilterId -> Web3MA e Unit
    loop fltr = do
      liftAff $ delay (Milliseconds 100.0)
      changes <- eth_getFilterChanges fltr
      traceA $ show changes
      acts <- for (catMaybes $ map pairChange changes) $ \(Tuple changeWithMeta changeEvent) -> do
        traceA "for"
        runReaderT (handler changeEvent) changeWithMeta
      when (TerminateEvent `notElem` acts) $ loop fltr
    pairChange :: Change -> Maybe (Tuple Change a)
    pairChange rc@(Change rawChange) = do
      change <- fromData rawChange.data
      pure (Tuple rc change)

--------------------------------------------------------------------------------
-- | Methods
--------------------------------------------------------------------------------

class ABIEncoding a <= Method a where
    -- | Send a transaction for given contract 'Address', value and input data
    sendTx :: forall e .
              Maybe Address
           -- ^ Contract address
           -> Address
           -- ^ from address
           -> BigNumber
           -- ^ paymentValue
           -> a
           -- ^ Method data
           -> Web3M e HexString
           -- ^ 'Web3' wrapped tx hash

    -- | Constant call given contract 'Address' in mode and given input data
    call :: forall e b .
            ABIEncoding b
         => Address
         -- ^ Contract address
         -> Maybe Address
         -- from address
         -> CallMode
         -- ^ State mode for constant call (latest or pending)
         -> a
         -- ^ Method data
         -> Web3M e b
         -- ^ 'Web3' wrapped result

instance methodAbiEncoding :: ABIEncoding a => Method a where
  sendTx = _sendTransaction
  call = _call

_sendTransaction :: forall a e .
                    ABIEncoding a
                 => Maybe Address
                 -> Address
                 -> BigNumber
                 -> a
                 -> Web3M e HexString
_sendTransaction mto f val dat =
    eth_sendTransaction (txdata $ toDataBuilder dat)
  where
    defaultGas = parseBigNumber hexadecimal "0x2dc2dc"
    txdata d =
      defaultTransactionOptions # _to .~ mto
                                # _from .~ Just f
                                # _data .~ Just d
                                # _value .~ Just val
                                # _gas .~ defaultGas

_call :: forall a b e .
         ABIEncoding a
      => ABIEncoding b
      => Address
      -> Maybe Address
      -> CallMode
      -> a
      -> Web3M e b
_call t mf cm dat = do
    res <- eth_call (txdata <<< toDataBuilder $ dat) cm
    case fromData res of
        Nothing -> throwError <<< error $ "Unable to parse result"
        Just x -> pure x
  where
    txdata d  =
      defaultTransactionOptions # _to .~ Just t
                                # _from .~ mf
                                # _data .~ Just d


--------------------------------------------------------------------------------
-- * Asynchronous Methods
--------------------------------------------------------------------------------
class AsyncMethod a where
    -- | Send a transaction for given contract 'Address', value and input data
    sendTxAsync :: forall e .
              Maybe Address
           -- ^ Contract address
           -> Address
           -- ^ from address
           -> BigNumber
           -- ^ paymentValue
           -> a
           -- ^ Method data
           -> Web3MA e HexString
           -- ^ 'Web3' wrapped tx hash

    -- | Constant call given contract 'Address' in mode and given input data
    callAsync :: forall b e .
            ABIEncoding b
         => Address
         -- ^ Contract address
         -> Maybe Address
         -- from address
         -> CallMode
         -- ^ State mode for constant call (latest or pending)
         -> a
         -- ^ Method data
         -> Web3MA e b
         -- ^ 'Web3' wrapped result

instance methodAsyncAbiEncoding :: ABIEncoding a => AsyncMethod a where
  sendTxAsync = _sendTransactionAsync
  callAsync = _callAsync

_sendTransactionAsync :: forall a e .
                    ABIEncoding a
                 => Maybe Address
                 -> Address
                 -> BigNumber
                 -> a
                 -> Web3MA e HexString
_sendTransactionAsync mto f val dat =
    eth_sendTransaction_async (txdata $ toDataBuilder dat)
  where
    defaultGas = parseBigNumber hexadecimal "0x2dc2dc"
    txdata d =
      defaultTransactionOptions # _to .~ mto
                                # _from .~ Just f
                                # _data .~ Just d
                                # _value .~ Just val
                                # _gas .~ defaultGas

_callAsync :: forall a b e .
         ABIEncoding a
      => ABIEncoding b
      => Address
      -> Maybe Address
      -> CallMode
      -> a
      -> Web3MA e b
_callAsync t mf cm dat = do
    res <- eth_call_async (txdata <<< toDataBuilder $ dat) cm
    case fromData res of
        Nothing -> throwError <<< error $ "Unable to parse result"
        Just x -> pure x
  where
    txdata d  =
      defaultTransactionOptions # _to .~ Just t
                                # _from .~ mf
                                # _data .~ Just d

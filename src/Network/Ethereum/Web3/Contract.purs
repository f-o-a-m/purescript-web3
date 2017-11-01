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
import Network.Ethereum.Web3.Provider (class IsAsyncProvider, class IsSyncProvider, forkWeb3MA, getAsyncProvider)
import Network.Ethereum.Web3.Solidity.AbiEncoding (class ABIEncoding, fromData, toDataBuilder)
import Network.Ethereum.Web3.Types (Address, BigNumber, CallMode, Change(..), ETH, Filter, FilterId, HexString, Web3M, Web3MA, _data, _from, _gas, _to, _value, defaultTransactionOptions, hexadecimal, parseBigNumber)
import Type.Proxy (Proxy(..))
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
event :: forall p e a.
          IsAsyncProvider p
       => EventFilter a
       => Address
       -> (a -> ReaderT Change (Web3MA p e) EventAction)
       -> Web3MA p e (Fiber (eth :: ETH | e) Unit)
event addr handler = do
    fid <- eth_newFilter (eventFilter (Proxy :: Proxy a) addr)
    provider <- getAsyncProvider
    liftAff <<< forkWeb3MA $ do
      loop fid
      _ <- eth_uninstallFilter fid
      pure unit
  where
    loop :: FilterId -> Web3MA p e Unit
    loop fltr = do
      _ <- liftAff $ delay (Milliseconds 100.0)
      changes <- eth_getFilterChanges fltr
      acts <- for (catMaybes $ map pairChange changes) $ \(Tuple changeWithMeta changeEvent) -> do
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
    sendTx :: forall p e .
              IsSyncProvider p
           => Maybe Address
           -- ^ Contract address
           -> Address
           -- ^ from address
           -> BigNumber
           -- ^ paymentValue
           -> a
           -- ^ Method data
           -> Web3M p e HexString
           -- ^ 'Web3' wrapped tx hash

    -- | Constant call given contract 'Address' in mode and given input data
    call :: forall p e b .
            IsSyncProvider p
         => ABIEncoding b
         => Address
         -- ^ Contract address
         -> Maybe Address
         -- from address
         -> CallMode
         -- ^ State mode for constant call (latest or pending)
         -> a
         -- ^ Method data
         -> Web3M p e b
         -- ^ 'Web3' wrapped result

instance methodAbiEncoding :: ABIEncoding a => Method a where
  sendTx = _sendTransaction
  call = _call

_sendTransaction :: forall p a e .
                    IsSyncProvider p
                 => ABIEncoding a
                 => Maybe Address
                 -> Address
                 -> BigNumber
                 -> a
                 -> Web3M p e HexString
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

_call :: forall p a b e .
         IsSyncProvider p
      => ABIEncoding a
      => ABIEncoding b
      => Address
      -> Maybe Address
      -> CallMode
      -> a
      -> Web3M p e b
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
    sendTxAsync :: forall p e .
              IsAsyncProvider p
           => Maybe Address
           -- ^ Contract address
           -> Address
           -- ^ from address
           -> BigNumber
           -- ^ paymentValue
           -> a
           -- ^ Method data
           -> Web3MA p e HexString
           -- ^ 'Web3' wrapped tx hash

    -- | Constant call given contract 'Address' in mode and given input data
    callAsync :: forall p b e .
            IsAsyncProvider p
         => ABIEncoding b
         => Address
         -- ^ Contract address
         -> Maybe Address
         -- from address
         -> CallMode
         -- ^ State mode for constant call (latest or pending)
         -> a
         -- ^ Method data
         -> Web3MA p e b
         -- ^ 'Web3' wrapped result

instance methodAsyncAbiEncoding :: ABIEncoding a => AsyncMethod a where
  sendTxAsync = _sendTransactionAsync
  callAsync = _callAsync

_sendTransactionAsync :: forall p a e .
                    IsAsyncProvider p
                 => ABIEncoding a
                 => Maybe Address
                 -> Address
                 -> BigNumber
                 -> a
                 -> Web3MA p e HexString
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

_callAsync :: forall p a b e .
         IsAsyncProvider p
      => ABIEncoding a
      => ABIEncoding b
      => Address
      -> Maybe Address
      -> CallMode
      -> a
      -> Web3MA p e b
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

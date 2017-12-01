module Network.Ethereum.Web3.Contract where

import Prelude

import Control.Monad.Aff (Fiber, delay)
import Control.Monad.Aff.Class (liftAff)
import Control.Monad.Eff.Exception (error)
import Control.Monad.Error.Class (throwError)
import Control.Monad.Reader (ReaderT, runReaderT)
import Data.Array (notElem, catMaybes)
import Data.Functor.Tagged (Tagged, untagged)
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Eq (genericEq)
import Data.Generic.Rep.Show (genericShow)
import Data.Lens ((.~))
import Data.Maybe (Maybe(..))
import Data.Symbol (class IsSymbol, SProxy(..), reflectSymbol)
import Data.Time.Duration (Milliseconds(..))
import Data.Traversable (for)
import Data.Tuple (Tuple(..))
import Network.Ethereum.Web3.Api (eth_call, eth_getFilterChanges, eth_newFilter, eth_sendTransaction, eth_uninstallFilter)
import Network.Ethereum.Web3.Provider (class IsAsyncProvider, forkWeb3')
import Network.Ethereum.Web3.Solidity (class ABIDecode, class GenericABIDecode, class GenericABIEncode, fromData, genericABIEncode, genericFromData)
import Network.Ethereum.Web3.Types (class EtherUnit, Address, CallMode, Change(..), ETH, Filter, FilterId, HexString, Web3, _data, _from, _gas, _to, _value, convert, defaultTransactionOptions, hexadecimal, parseBigNumber, toSelector)
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

class ABIDecode a <= EventFilter a where
    -- | Event filter structure used by low-level subscription methods
    eventFilter :: Proxy a -> Address -> Filter


-- | Start listening to events eminating from the given address and caught by the filter,
-- | using the handler to process the data and decide whether to continue
event :: forall p e a.
          IsAsyncProvider p
       => EventFilter a
       => Address
       -> (a -> ReaderT Change (Web3 p e) EventAction)
       -> Web3 p e (Fiber (eth :: ETH | e) Unit)
event addr handler = do
    fid <- eth_newFilter (eventFilter (Proxy :: Proxy a) addr)
    forkWeb3' (Proxy :: Proxy p) $ do
      loop fid
      _ <- eth_uninstallFilter fid
      pure unit
  where
    loop :: FilterId -> Web3 p e Unit
    loop fltr = do
      _ <- liftAff $ delay (Milliseconds 1000.0)
      changes <- eth_getFilterChanges fltr
      acts <- for (catMaybes $ map pairChange changes) $ \(Tuple changeWithMeta changeEvent) -> do
        runReaderT (handler changeEvent) changeWithMeta
      when (TerminateEvent `notElem` acts) $ loop fltr
    pairChange :: Change -> Maybe (Tuple Change a)
    pairChange rc@(Change rawChange) = do
      change <- fromData rawChange.data
      pure (Tuple rc change)

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
         -> CallMode
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
      -> CallMode
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

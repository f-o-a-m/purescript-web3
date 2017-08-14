module Network.Ethereum.Web3.Contract where

import Prelude
import Data.Maybe (Maybe(..))
import Data.Lens ((.~))
import Control.Monad.Eff.Exception (error)
import Control.Monad.Error.Class (throwError)
import Network.Ethereum.Web3.Api (eth_call, eth_call_async, eth_sendTransaction, eth_sendTransaction_async)
import Network.Ethereum.Web3.Encoding.AbiEncoding (class ABIEncoding, toDataBuilder, fromData)
import Network.Ethereum.Web3.Types (Address, BigNumber, CallMode, HexString, Web3M, Web3MA,
                                    _data, _from, _to, _value, defaultTransactionOptions)



class Method a where
    -- | Send a transaction for given contract 'Address', value and input data
    sendTx :: Maybe Address
           -- ^ Contract address
           -> Maybe Address
           -- ^ from address
           -> BigNumber
           -- ^ paymentValue
           -> a
           -- ^ Method data
           -> Web3M () HexString
           -- ^ 'Web3' wrapped tx hash

    -- | Constant call given contract 'Address' in mode and given input data
    call :: forall b .
            ABIEncoding b
         => Address
         -- ^ Contract address
         -> Maybe Address
         -- from address
         -> CallMode
         -- ^ State mode for constant call (latest or pending)
         -> a
         -- ^ Method data
         -> Web3M () b
         -- ^ 'Web3' wrapped result

instance methodAbiEncoding :: ABIEncoding a => Method a where
  sendTx = _sendTransaction
  call = _call

_sendTransaction :: forall a .
                    ABIEncoding a
                 => Maybe Address
                 -> Maybe Address
                 -> BigNumber
                 -> a
                 -> Web3M () HexString
_sendTransaction mto mf val dat =
    eth_sendTransaction (txdata $ toDataBuilder dat)
  where
    txdata d =
      defaultTransactionOptions # _to .~ mto
                                # _from .~ mf
                                # _data .~ Just d
                                # _value .~ Just val

_call :: forall a b .
         ABIEncoding a
      => ABIEncoding b
      => Address
      -> Maybe Address
      -> CallMode
      -> a
      -> Web3M () b
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
    sendTxAsync :: Maybe Address
           -- ^ Contract address
           -> Maybe Address
           -- ^ from address
           -> BigNumber
           -- ^ paymentValue
           -> a
           -- ^ Method data
           -> Web3MA () HexString
           -- ^ 'Web3' wrapped tx hash

    -- | Constant call given contract 'Address' in mode and given input data
    callAsync :: forall b .
            ABIEncoding b
         => Address
         -- ^ Contract address
         -> Maybe Address
         -- from address
         -> CallMode
         -- ^ State mode for constant call (latest or pending)
         -> a
         -- ^ Method data
         -> Web3MA () b
         -- ^ 'Web3' wrapped result

instance methodAsyncAbiEncoding :: ABIEncoding a => AsyncMethod a where
  sendTxAsync = _sendTransactionAsync
  callAsync = _callAsync

_sendTransactionAsync :: forall a .
                    ABIEncoding a
                 => Maybe Address
                 -> Maybe Address
                 -> BigNumber
                 -> a
                 -> Web3MA () HexString
_sendTransactionAsync mto mf val dat =
    eth_sendTransaction_async (txdata $ toDataBuilder dat)
  where
    txdata d =
      defaultTransactionOptions # _to .~ mto
                                # _from .~ mf
                                # _data .~ Just d
                                # _value .~ Just val

_callAsync :: forall a b .
         ABIEncoding a
      => ABIEncoding b
      => Address
      -> Maybe Address
      -> CallMode
      -> a
      -> Web3MA () b
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

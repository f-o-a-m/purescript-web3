module Network.Ethereum.Web3.Contract where

import Prelude
import Data.Maybe (Maybe(..))
import Data.Lens ((.~))
import Control.Monad.Eff.Exception (error)
import Control.Monad.Error.Class (throwError)
import Network.Ethereum.Web3.Api (eth_call, eth_call_async, eth_sendTransaction, eth_sendTransaction_async)
import Network.Ethereum.Web3.Solidity.AbiEncoding (class ABIEncoding, toDataBuilder, fromData)
import Network.Ethereum.Web3.Types (Address, BigNumber, CallMode, HexString, Web3M, Web3MA,
                                    _data, _from, _gas, _to, _value, defaultTransactionOptions,
                                    hexadecimal, parseBigNumber)



class Method a where
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

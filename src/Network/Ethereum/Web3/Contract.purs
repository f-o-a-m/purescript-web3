module Network.Ethereum.Web3.Contract
 ( class EventFilter
 , eventFilter
 , event
 , event'
 , class CallMethod
 , call
 , class TxMethod
 , sendTx
 , deployContract
 ) where

import Prelude

import Control.Coroutine (runProcess)
import Control.Monad.Eff.Exception (error)
import Control.Monad.Reader (ReaderT)
import Data.Either (Either(..))
import Data.Functor.Tagged (Tagged, untagged)
import Data.Generic.Rep (class Generic)
import Data.Lens ((.~), (^.), (%~), (?~))
import Data.Maybe (Maybe(..))
import Data.Monoid (mempty)
import Data.Symbol (class IsSymbol, SProxy(..), reflectSymbol)
import Network.Ethereum.Types (Address, HexString)
import Network.Ethereum.Core.Keccak256 (toSelector)
import Network.Ethereum.Web3.Api (eth_blockNumber, eth_call, eth_newFilter, eth_sendTransaction)
import Network.Ethereum.Web3.Contract.Internal (reduceEventStream, pollFilter, logsStream, mkBlockNumber)
import Network.Ethereum.Web3.Solidity (class DecodeEvent, class GenericABIDecode, class GenericABIEncode, genericABIEncode, genericFromData)
import Network.Ethereum.Web3.Types (class EtherUnit, CallError(..), ChainCursor(..), Change, EventAction, Filter, NoPay, TransactionOptions, Value, Web3, _data, _fromBlock, _toBlock, _value, convert, throwWeb3)
import Type.Proxy (Proxy)

--------------------------------------------------------------------------------
-- * Events
--------------------------------------------------------------------------------

class EventFilter a where
    -- | Event filter structure used by low-level subscription methods
    eventFilter :: Proxy a -> Address -> Filter a

-- | run `event'` one block at a time.
event :: forall e a i ni.
         DecodeEvent i ni a
      => Filter a
      -> (a -> ReaderT Change (Web3 e) EventAction)
      -> Web3 e Unit
event fltr handler = event' fltr zero handler


-- | Takes a `Filter` and a handler, as well as a windowSize.
-- | It runs the handler over the `eventLogs` using `reduceEventStream`. If no
-- | `TerminateEvent` is thrown, it then transitions to polling.
event' :: forall e a i ni.
          DecodeEvent i ni a
       => Filter a
       -> Int
       -> (a -> ReaderT Change (Web3 e) EventAction)
       -> Web3 e Unit
event' fltr w handler = do
  pollingFromBlock <- case fltr ^. _fromBlock of
    BN startingBlock -> do
      currentBlock <- eth_blockNumber
      if startingBlock < currentBlock
         then let initialState = { currentBlock: startingBlock
                                 , initialFilter: fltr
                                 , windowSize: w
                                 }
              in runProcess $ reduceEventStream (logsStream initialState) handler
              else pure startingBlock
    cursor -> mkBlockNumber cursor
  if fltr ^. _toBlock < BN pollingFromBlock
     then pure unit
     else do
        filterId <- eth_newFilter $ fltr # _fromBlock .~ BN pollingFromBlock
        void <<< runProcess $ reduceEventStream (pollFilter filterId (fltr ^. _toBlock)) handler


--------------------------------------------------------------------------------
-- * Methods
--------------------------------------------------------------------------------

-- | Class paramaterized by values which are ABIEncodable, allowing the templating of
-- | of a transaction with this value as the payload.
class TxMethod (selector :: Symbol) a where
    -- | Send a transaction for given contract 'Address', value and input data
    sendTx :: forall e u.
              EtherUnit (Value u)
           => IsSymbol selector
           => TransactionOptions u
           -> Tagged (SProxy selector) a
           -- ^ Method data
           -> Web3 e HexString
           -- ^ 'Web3' wrapped tx hash

class CallMethod (selector :: Symbol) a b where
    -- | Constant call given contract 'Address' in mode and given input data
    call :: forall e.
            IsSymbol selector
         => TransactionOptions NoPay
         -- ^ TransactionOptions
         -> ChainCursor
         -- ^ State mode for constant call (latest or pending)
         -> Tagged (SProxy selector) a
         -- ^ Method data
         -> Web3 e (Either CallError b)
         -- ^ 'Web3' wrapped result

instance txmethodAbiEncode :: (Generic a rep, GenericABIEncode rep) => TxMethod s a where
  sendTx = _sendTransaction

instance callmethodAbiEncode :: (Generic a arep, GenericABIEncode arep, Generic b brep, GenericABIDecode brep) => CallMethod s a b where
  call = _call

_sendTransaction :: forall a u rep e selector .
                    IsSymbol selector
                 => Generic a rep
                 => GenericABIEncode rep
                 => EtherUnit (Value u)
                 => TransactionOptions u
                 -> Tagged (SProxy selector) a
                 -> Web3 e HexString
_sendTransaction txOptions dat = do
    let sel = toSelector <<< reflectSymbol $ (SProxy :: SProxy selector)
    eth_sendTransaction $ txdata $ sel <> (genericABIEncode <<< untagged $ dat)
  where
    txdata d = txOptions # _data .~ Just d
                         # _value %~ map convert

_call :: forall a arep b brep e selector .
         IsSymbol selector
      => Generic a arep
      => GenericABIEncode arep
      => Generic b brep
      => GenericABIDecode brep
      => TransactionOptions NoPay
      -> ChainCursor
      -> Tagged (SProxy selector) a
      -> Web3 e (Either CallError b)
_call txOptions cursor dat = do
    let sig = reflectSymbol $ (SProxy :: SProxy selector)
        sel = toSelector sig
        fullData = sel <> (genericABIEncode <<< untagged $ dat)
    res <- eth_call (txdata $ sel <> (genericABIEncode <<< untagged $ dat)) cursor
    case genericFromData res of
      Left err ->
        if res == mempty
          then pure <<< Left $ NullStorageError { signature: sig
                                                , _data: fullData
                                                }
          else throwWeb3 <<< error $ show err
      Right x -> pure $ Right x
  where
    txdata d  = txOptions # _data .~ Just d

deployContract :: forall a rep e t.
                    Generic a rep
                 => GenericABIEncode rep
                 => TransactionOptions NoPay
                 -> HexString
                 -> Tagged t a
                 -> Web3 e HexString
deployContract txOptions deployByteCode args =
  let txdata = txOptions # _data ?~ deployByteCode <> genericABIEncode (untagged args)
                         # _value %~ map convert
  in eth_sendTransaction txdata

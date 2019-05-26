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
 , mkDataField
 , eventMulti'
 ) where

import Prelude

import Control.Coroutine (runProcess)
import Effect.Exception (error)
import Control.Monad.Fork.Class (bracket)
import Control.Monad.Reader (ReaderT)
import Data.Either (Either(..))
import Data.Functor.Tagged (Tagged, untagged)
import Data.Generic.Rep (class Generic, Constructor)
import Data.Lens ((.~), (^.), (%~), (?~))
import Data.Maybe (Maybe(..))
import Data.Symbol (class IsSymbol, SProxy(..), reflectSymbol)
import Network.Ethereum.Core.Keccak256 (toSelector)
import Network.Ethereum.Types (Address, HexString)
import Network.Ethereum.Web3.Api (eth_blockNumber, eth_call, eth_newFilter, eth_sendTransaction, eth_uninstallFilter)
import Network.Ethereum.Web3.Contract.Internal (reduceEventStream, pollFilter, logsStream, mkBlockNumber)
import Network.Ethereum.Web3.Solidity (class DecodeEvent, class GenericABIDecode, class GenericABIEncode, class RecordFieldsIso, genericABIEncode, genericFromData, genericFromRecordFields)
import Network.Ethereum.Web3.Types (class TokenUnit, CallError(..), ChainCursor(..), Change, ETHER, EventAction, Filter, NoPay, TransactionOptions, Value, Web3, _data, _fromBlock, _toBlock, _value, convert, throwWeb3, BlockNumber)
import Type.Proxy (Proxy)

import Heterogeneous.Folding (class FoldingWithIndex, class FoldlRecord, class HFoldl, hfoldlWithIndex, hfoldl)
import Heterogeneous.Mapping (class MapRecordWithIndex, class Mapping, ConstMapping, hmap, class MapVariantWithIndex, mapVariantWithIndex)
import Network.Ethereum.Web3.Contract.Internal (MultiFilterMinToBlock(..), MultiFilterMinFromBlock(..), MultiFilterStreamState(..), multiLogsStream, reduceEventStreamMulti, ModifyFilter(..), QueryAllLogs(..), FilterChange(..), openMultiFilter, OpenMultiFilter(..), CloseMultiFilter(..), pollFilterMulti, CheckMultiFilter(..))
import Prim.RowList as RowList
import Type.Row as Row
import Data.Variant (Variant, class VariantMatchCases, expand, inj, match)
import Control.Coroutine (Producer, Consumer, Process, pullFrom, producer, consumer, emit)

--------------------------------------------------------------------------------
-- * Events
--------------------------------------------------------------------------------

class EventFilter a where
    -- | Event filter structure used by low-level subscription methods
    eventFilter :: Proxy a -> Address -> Filter a

-- | run `event'` one block at a time.
event :: forall a i ni.
         DecodeEvent i ni a
      => Filter a
      -> (a -> ReaderT Change Web3 EventAction)
      -> Web3 Unit
event fltr handler = event' fltr zero handler


-- | Takes a `Filter` and a handler, as well as a windowSize.
-- | It runs the handler over the `eventLogs` using `reduceEventStream`. If no
-- | `TerminateEvent` is thrown, it then transitions to polling.
event' :: forall a i ni.
          DecodeEvent i ni a
       => Filter a
       -> Int
       -> (a -> ReaderT Change Web3 EventAction)
       -> Web3 Unit
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
      bracket
        (eth_newFilter $ fltr # _fromBlock .~ BN pollingFromBlock)
        (const $ void <<< eth_uninstallFilter)
        (\filterId -> void $ runProcess $ reduceEventStream (pollFilter filterId (fltr ^. _toBlock)) handler)

-- | Takes a `Filter` and a handler, as well as a windowSize.
-- | It runs the handler over the `eventLogs` using `reduceEventStream`. If no
-- | `TerminateEvent` is thrown, it then transitions to polling.
eventMulti'
      :: forall fs handlers fsList handlersList r1 r fsIds fsIdsList.
         FoldlRecord MultiFilterMinFromBlock ChainCursor fsList fs ChainCursor
      => FoldlRecord MultiFilterMinToBlock ChainCursor fsList fs ChainCursor
      => FoldlRecord MultiFilterMinToBlock ChainCursor fsList fs ChainCursor
      => Row.RowToList handlers handlersList
      => MapRecordWithIndex fsList (ConstMapping ModifyFilter) fs fs
      => RowList.RowToList fs fsList
      => VariantMatchCases handlersList r1 (ReaderT Change Web3 EventAction)
      => Row.Union r1 () r
      => FoldlRecord QueryAllLogs  (Web3 (Array (FilterChange (Variant ())))) fsList fs (Web3 (Array (FilterChange (Variant r))))
      => FoldlRecord OpenMultiFilter (Web3 (Record ())) fsList fs (Web3 (Record fsIds))
      => FoldlRecord CloseMultiFilter (Web3 Unit) fsIdsList fsIds (Web3 Unit)
      => RowList.RowToList fsIds fsIdsList
      => FoldlRecord CheckMultiFilter (Web3 (Array (FilterChange (Variant ())))) fsIdsList fsIds (Web3 (Array (FilterChange (Variant r))))
      => Record fs
      -> Int
      -> Record handlers
      -> Web3 Unit
eventMulti' fltrs w handler = do
  pollingFromBlock <- case hfoldlWithIndex MultiFilterMinFromBlock Pending fltrs of
    BN startingBlock -> do
      currentBlock <- eth_blockNumber
      if startingBlock < currentBlock
         then let initialState =
                    MultiFilterStreamState { currentBlock: startingBlock
                                           , filters: fltrs
                                           , windowSize: w
                                           }
                  producer :: Producer (Array (FilterChange (Variant r))) Web3 BlockNumber
                  producer = multiLogsStream initialState
              in runProcess $ reduceEventStreamMulti producer handler
              else pure startingBlock
    cursor -> mkBlockNumber cursor
  let minToBlock = hfoldlWithIndex MultiFilterMinToBlock Pending fltrs
  if minToBlock < BN pollingFromBlock
    then pure unit
    else
      bracket
        (let fltrs' = hmap (ModifyFilter \fltr -> fltr # _fromBlock .~ BN pollingFromBlock) fltrs
         in openMultiFilter fltrs'
        )
        (const $ hfoldlWithIndex CloseMultiFilter (pure unit :: Web3 Unit))
        (\filterIds -> void $ runProcess $ reduceEventStreamMulti (pollFilterMulti filterIds minToBlock) handler)



--------------------------------------------------------------------------------
-- * Methods
--------------------------------------------------------------------------------

-- | Class paramaterized by values which are ABIEncodable, allowing the templating of
-- | of a transaction with this value as the payload.
class TxMethod (selector :: Symbol) a where
    -- | Send a transaction for given contract 'Address', value and input data
    sendTx :: forall u.
              TokenUnit (Value (u ETHER))
           => IsSymbol selector
           => TransactionOptions u
           -> Tagged (SProxy selector) a
           -- ^ Method data
           -> Web3 HexString
           -- ^ 'Web3' wrapped tx hash

class CallMethod (selector :: Symbol) a b where
    -- | Constant call given contract 'Address' in mode and given input data
    call :: IsSymbol selector
         => TransactionOptions NoPay
         -- ^ TransactionOptions
         -> ChainCursor
         -- ^ State mode for constant call (latest or pending)
         -> Tagged (SProxy selector) a
         -- ^ Method data
         -> Web3 (Either CallError b)
         -- ^ 'Web3' wrapped result

instance txmethodAbiEncode :: (Generic a rep, GenericABIEncode rep) => TxMethod s a where
  sendTx = _sendTransaction

instance callmethodAbiEncode :: (Generic a arep, GenericABIEncode arep, Generic b brep, GenericABIDecode brep) => CallMethod s a b where
  call = _call

_sendTransaction :: forall a u rep selector .
                    IsSymbol selector
                 => Generic a rep
                 => GenericABIEncode rep
                 => TokenUnit (Value (u ETHER))
                 => TransactionOptions u
                 -> Tagged (SProxy selector) a
                 -> Web3 HexString
_sendTransaction txOptions dat = do
    let sel = toSelector <<< reflectSymbol $ (SProxy :: SProxy selector)
    eth_sendTransaction $ txdata $ sel <> (genericABIEncode <<< untagged $ dat)
  where
    txdata d = txOptions # _data .~ Just d
                         # _value %~ map convert

_call :: forall a arep b brep selector .
         IsSymbol selector
      => Generic a arep
      => GenericABIEncode arep
      => Generic b brep
      => GenericABIDecode brep
      => TransactionOptions NoPay
      -> ChainCursor
      -> Tagged (SProxy selector) a
      -> Web3 (Either CallError b)
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

deployContract :: forall a rep t.
                    Generic a rep
                 => GenericABIEncode rep
                 => TransactionOptions NoPay
                 -> HexString
                 -> Tagged t a
                 -> Web3 HexString
deployContract txOptions deployByteCode args =
  let txdata = txOptions # _data ?~ deployByteCode <> genericABIEncode (untagged args)
                         # _value %~ map convert
  in eth_sendTransaction txdata

mkDataField
  :: forall selector a name args fields l.
     IsSymbol selector
  => Generic a (Constructor name args)
  => RecordFieldsIso args fields l
  => GenericABIEncode (Constructor name args)
  => Proxy (Tagged (SProxy selector) a)
  -> Record fields
  -> HexString
mkDataField _ r =
  let sig = reflectSymbol (SProxy :: SProxy selector)
      sel = toSelector sig
      args = genericFromRecordFields r :: a
  in sel <> (genericABIEncode args)

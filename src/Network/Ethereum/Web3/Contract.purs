module Network.Ethereum.Web3.Contract
  ( class EventFilter
  , eventFilter
  , event
  , class CallMethod
  , call
  , class TxMethod
  , sendTx
  , deployContract
  , mkDataField
  ) where

import Prelude

import Control.Monad.Error.Class (throwError)
import Data.Bifunctor (lmap)
import Data.Either (Either(..))
import Data.Functor.Tagged (Tagged, untagged)
import Data.Generic.Rep (class Generic)
import Data.Lens ((.~), (%~), (?~))
import Data.Maybe (Maybe(..))
import Data.Symbol (class IsSymbol, reflectSymbol)
import Effect.Exception (error)
import Network.Ethereum.Core.Keccak256 (toSelector)
import Network.Ethereum.Types (Address, HexString)
import Network.Ethereum.Web3.Api (eth_call, eth_sendTransaction)
import Network.Ethereum.Web3.Contract.Events (MultiFilterStreamState(..), event', FilterStreamState, ChangeReceipt, EventHandler)
import Network.Ethereum.Web3.Solidity (class DecodeEvent, class GenericABIDecode, class GenericABIEncode, class GRecordFieldsIso, fromRecord)
import Network.Ethereum.Web3.Solidity.AbiEncoding (abiDecode, abiEncode)
import Network.Ethereum.Web3.Types (class TokenUnit, CallError(..), ChainCursor, ETHER, Filter, NoPay, TransactionOptions, Value, Web3, _data, _value, convert)
import Type.Proxy (Proxy(..))

class EventFilter :: forall k. k -> Constraint
class EventFilter e where
  -- | Event filter structure used by low-level subscription methods
  eventFilter :: Proxy e -> Address -> Filter e

-- | run `event'` one block at a time.
event
  :: forall e i ni
   . DecodeEvent i ni e
  => Filter e
  -> EventHandler Web3 e
  -> Web3 (Either (FilterStreamState e) ChangeReceipt)
event filter handler = do
  eRes <- event' { ev: filter } { ev: handler } { windowSize: 0, trailBy: 0 }
  pure $ lmap f eRes
  where
  f :: MultiFilterStreamState (ev :: Filter e) -> FilterStreamState e
  f (MultiFilterStreamState { currentBlock, windowSize, trailBy, filters }) =
    let
      { ev: filter } = filters
    in
      { currentBlock
      , windowSize
      , trailBy
      , initialFilter: filter
      }

--------------------------------------------------------------------------------
-- * Methods
--------------------------------------------------------------------------------
-- | Class paramaterized by values which are ABIEncodable, allowing the templating of
-- | of a transaction with this value as the payload.
class TxMethod (selector :: Symbol) a where
  -- | Send a transaction for given contract `Address`, value and input data
  sendTx
    :: forall u
     . TokenUnit (Value (u ETHER))
    => IsSymbol selector
    => TransactionOptions u
    -> Tagged selector a
    -> Web3 HexString

-- ^ `Web3` wrapped tx hash
class CallMethod (selector :: Symbol) a b where
  -- | Constant call given contract `Address` in mode and given input data
  call
    :: IsSymbol selector
    => TransactionOptions NoPay
    -> ChainCursor
    -> Tagged selector a
    -> Web3 (Either CallError b)

-- ^ `Web3` wrapped result
instance (Generic a rep, GenericABIEncode rep) => TxMethod s a where
  sendTx = _sendTransaction

instance (Generic a arep, GenericABIEncode arep, Generic b brep, GenericABIDecode brep) => CallMethod s a b where
  call = _call

_sendTransaction
  :: forall a u rep selector
   . IsSymbol selector
  => Generic a rep
  => GenericABIEncode rep
  => TokenUnit (Value (u ETHER))
  => TransactionOptions u
  -> Tagged selector a
  -> Web3 HexString
_sendTransaction txOptions dat = do
  let
    sel = toSelector <<< reflectSymbol $ (Proxy :: Proxy selector)
  eth_sendTransaction $ txdata $ sel <> (abiEncode <<< untagged $ dat)
  where
  txdata d =
    txOptions # _data .~ Just d
      # _value
          %~ map convert

_call
  :: forall a arep b brep selector
   . IsSymbol selector
  => Generic a arep
  => GenericABIEncode arep
  => Generic b brep
  => GenericABIDecode brep
  => TransactionOptions NoPay
  -> ChainCursor
  -> Tagged selector a
  -> Web3 (Either CallError b)
_call txOptions cursor dat = do
  let
    sig = reflectSymbol $ (Proxy :: Proxy selector)

    sel = toSelector sig

    fullData = sel <> (abiEncode <<< untagged $ dat)
  res <- eth_call (txdata $ sel <> (abiEncode <<< untagged $ dat)) cursor
  case abiDecode res of
    Left err ->
      if res == mempty then
        pure <<< Left
          $ NullStorageError
              { signature: sig
              , _data: fullData
              }
      else
        throwError $ error $ show err
    Right x -> pure $ Right x
  where
  txdata d = txOptions # _data .~ Just d

deployContract
  :: forall a rep t
   . Generic a rep
  => GenericABIEncode rep
  => TransactionOptions NoPay
  -> HexString
  -> Tagged t a
  -> Web3 HexString
deployContract txOptions deployByteCode args =
  let
    txdata =
      txOptions # _data ?~ deployByteCode <> abiEncode (untagged args)
        # _value
            %~ map convert
  in
    eth_sendTransaction txdata

mkDataField
  :: forall selector a rep fields
   . IsSymbol selector
  => Generic a rep
  => GRecordFieldsIso rep () fields
  => GenericABIEncode rep
  => Proxy (Tagged selector a)
  -> Record fields
  -> HexString
mkDataField _ r =
  let
    sig = reflectSymbol (Proxy :: Proxy selector)

    sel = toSelector sig

    args = fromRecord r :: a
  in
    sel <> abiEncode args

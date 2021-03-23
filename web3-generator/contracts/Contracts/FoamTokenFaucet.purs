--------------------------------------------------------------------------------
-- | FoamTokenFaucet
--------------------------------------------------------------------------------

module Contracts.FoamTokenFaucet where

import Prelude 

import Data.Functor.Tagged (Tagged, tagged)
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Eq (genericEq)
import Data.Generic.Rep.Show (genericShow)
import Data.Lens ((.~))
import Data.Maybe (Maybe(..), fromJust)
import Data.Newtype (class Newtype)
import Data.Symbol (SProxy)
import Network.Ethereum.Web3 (_address, _topics, class EventFilter, sendTx)
import Network.Ethereum.Web3.Contract.Internal (uncurryFields)
import Network.Ethereum.Web3.Solidity (D2, D5, D6, DOne, Tuple0, Tuple1(..), Tuple2, Tuple3, UIntN, class IndexedEvent)
import Network.Ethereum.Web3.Solidity.Size (type (:&))
import Network.Ethereum.Web3.Types (Address, HexString, TransactionOptions, Web3, defaultFilter, mkHexString)
import Network.Ethereum.Web3.Types.TokenUnit (MinorUnit)
import Partial.Unsafe (unsafePartial)
--------------------------------------------------------------------------------
-- | TokenChange
--------------------------------------------------------------------------------


newtype TokenChange = TokenChange {_old :: Address,_new :: Address}

derive instance newtypeTokenChange :: Newtype TokenChange _

instance eventFilterTokenChange :: EventFilter TokenChange where
  eventFilter _ addr = defaultFilter
    # _address .~ Just addr
    # _topics .~ Just [Just ( unsafePartial $ fromJust $ mkHexString "2ef4615f87b614ed8e8f7a775a271837b9999df8280dfd816d28c37498ca35a4")]

instance indexedEventTokenChange :: IndexedEvent (Tuple0 ) (Tuple2 (Tagged (SProxy "_old") Address) (Tagged (SProxy "_new") Address)) TokenChange where
  isAnonymous _ = false

derive instance genericTokenChange :: Generic TokenChange _

instance eventGenericTokenChangeShow :: Show TokenChange where
  show = genericShow

instance eventGenericTokenChangeeq :: Eq TokenChange where
  eq = genericEq

--------------------------------------------------------------------------------
-- | PreTransfer
--------------------------------------------------------------------------------


newtype PreTransfer = PreTransfer {_from :: Address,_to :: Address,_value :: (UIntN (D2 :& D5 :& DOne D6))}

derive instance newtypePreTransfer :: Newtype PreTransfer _

instance eventFilterPreTransfer :: EventFilter PreTransfer where
  eventFilter _ addr = defaultFilter
    # _address .~ Just addr
    # _topics .~ Just [Just ( unsafePartial $ fromJust $ mkHexString "6db37187401e2ebf075951204867ea0cfa6150cfd705f503acf8d99105c79b64")]

instance indexedEventPreTransfer :: IndexedEvent (Tuple0 ) (Tuple3 (Tagged (SProxy "_from") Address) (Tagged (SProxy "_to") Address) (Tagged (SProxy "_value") (UIntN (D2 :& D5 :& DOne D6)))) PreTransfer where
  isAnonymous _ = false

derive instance genericPreTransfer :: Generic PreTransfer _

instance eventGenericPreTransferShow :: Show PreTransfer where
  show = genericShow

instance eventGenericPreTransfereq :: Eq PreTransfer where
  eq = genericEq

--------------------------------------------------------------------------------
-- | FaucetFn
--------------------------------------------------------------------------------


type FaucetFn = Tagged (SProxy "faucet(address)") (Tuple1 (Tagged (SProxy "to") Address))

faucet :: TransactionOptions MinorUnit -> { to :: Address } -> Web3 HexString
faucet x0 r = uncurryFields  r $ faucet' x0
   where
    faucet' :: TransactionOptions MinorUnit -> (Tagged (SProxy "to") Address) -> Web3 HexString
    faucet' y0 y1 = sendTx y0 ((tagged $ Tuple1 y1) :: FaucetFn)
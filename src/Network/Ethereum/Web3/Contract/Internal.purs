module Network.Ethereum.Web3.Contract.Internal
  ( class UncurryFields
  , uncurryFields
  ) where

import Prelude
import Data.Functor.Tagged (Tagged, tagged)
import Record as Record
import Data.Symbol (class IsSymbol, SProxy(..))
import Type.Row as Row
import Network.Ethereum.Web3.Types (Web3)

--------------------------------------------------------------------------------
-- * Uncurry Helper
--------------------------------------------------------------------------------
-- | Useful class for using records as arguments to solidity functions
class UncurryFields fields curried result | curried -> result fields where
  uncurryFields :: Record fields -> curried -> result

instance uncurryFieldsEmpty :: UncurryFields () (Web3 b) (Web3 b) where
  uncurryFields _ = identity

instance uncurryFieldsInductive :: (IsSymbol s, Row.Cons s a before after, Row.Lacks s before, UncurryFields before f b) => UncurryFields after (Tagged (SProxy s) a -> f) b where
  uncurryFields r f =
    let
      arg = (Record.get (SProxy :: SProxy s) r)

      before = Record.delete (SProxy :: SProxy s) r :: Record before

      partiallyApplied = f (tagged arg :: Tagged (SProxy s) a)
    in
      uncurryFields before partiallyApplied

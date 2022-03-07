module Network.Ethereum.Web3.Contract.Internal
  ( class UncurryFields
  , uncurryFields
  ) where

import Prelude

import Data.Functor.Tagged (Tagged, tagged)
import Data.Symbol (class IsSymbol)
import Network.Ethereum.Web3.Types (Web3)
import Record as Record
import Type.Proxy (Proxy(..))
import Type.Row as Row

--------------------------------------------------------------------------------
-- * Uncurry Helper
--------------------------------------------------------------------------------
-- | Useful class for using records as arguments to solidity functions

-- Example: in `spago repl`
-- ```
-- :paste
-- import Data.Functor.Tagged
-- import Network.Ethereum.Web3.Types
-- import Type.Proxy
-- doWork :: Tagged (Proxy "b") String -> Tagged (Proxy "a") Int -> Web3 String
-- doWork taggedB taggedA = pure (show (untagged taggedA) <> untagged taggedB)
--
-- :type uncurryFields { a: 1, b: "foo" } doWork
-- ```
-- Will return `Web3 String`

class UncurryFields fields curried result | curried -> result fields where
  uncurryFields :: Record fields -> curried -> result

instance uncurryFieldsEmpty :: UncurryFields () (Web3 b) (Web3 b) where
  uncurryFields _ = identity

instance uncurryFieldsInductive ::
  ( IsSymbol name
  , Row.Cons name a fieldsWithoutName fieldsWithName
  , Row.Lacks name fieldsWithoutName
  , UncurryFields fieldsWithoutName web3OrFunction web3Result)
  => UncurryFields fieldsWithName (Tagged (Proxy name) a -> web3OrFunction) web3Result where
  uncurryFields r f =
    let
      arg = Record.get (Proxy :: Proxy name) r

      fieldsWithoutName = Record.delete (Proxy :: Proxy name) r :: Record fieldsWithoutName

      partiallyApplied = f (tagged arg :: Tagged (Proxy name) a)
    in
      uncurryFields fieldsWithoutName partiallyApplied

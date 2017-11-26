module Network.Ethereum.Web3.Solidity.Splice where

import Prelude

import Data.Functor.Tagged
import Data.Generic.Rep
import Network.Ethereum.Web3.Solidity.Tuple

-- import Type.Proxy (Proxy)

{-
-- | `InsertIndexed n a before after` is capable of inserting a tagged `a` into position `n` in `before`
class InsertIndexed n a before after | n a before -> after, after n -> a before where
  insertArg :: Tagged n a -> before -> after

instance baseInsertIndexed :: InsertIndexed Z a (Argument (Tagged (S Z) b)) (Product (Argument (Tagged (S Z) b)) (Argument (Tagged Z a))) where
  insertArg t a = Product a (Argument t)

instance inductiveInsertIndexed :: InsertIndexed (S n) a (Product (Argument (Tagged n b)) c) (Product (Argument (Tagged (S n) a)) (Product (Argument (Tagged n b)) c)) where
  insertArg t p = Product (Argument t) p

instance inductiveInsertIndexed' :: InsertIndexed n a before after => InsertIndexed n a (Product c before) (Product c after) where
  insertArg t (Product c before) = Product c (insertArg t before)

instance wrapperInsertIndexed :: InsertIndexed n a before after => InsertIndexed n a before (Constructor name after) where
  insertArg t before = Constructor $ insertArg t before

-- | `InsertIndexedMany items bin final` is capable of inserting all the `items` into the `bin` in the appropriate index.
class InsertIndexedMany items bin final | items bin -> final, final bin -> items, items final -> bin where
  insertIndexedMany :: items -> bin -> final

instance baseInsertIndexedMany :: InsertIndexed n a before after => InsertIndexedMany (Argument (Tagged n a)) before after where
  insertIndexedMany (Argument a) b = insertArg a b

instance inductiveInsertIndexedMany :: (InsertIndexed n a current new, InsertIndexedMany rest new after) => InsertIndexedMany (Product (Argument (Tagged n a)) rest) current after where
  insertIndexedMany (Product (Argument a) rest) current = insertIndexedMany rest $ insertArg a current

instance wrapperInsertIndexedMany :: InsertIndexedMany items bin final => InsertIndexedMany (Constructor ni items) (Constructor nb bin) (Constructor nf final) where
  insertIndexedMany (Constructor is) (Constructor bin) = Constructor (insertIndexedMany is bin)

-- `UnTagged` is a class for mapping over a generic product and removing the tagged constructors
class UnTagged t ut | t -> ut, ut -> t where
  untag :: t -> ut

instance baseUntagged :: UnTagged (Argument (Tagged n a)) (Argument a) where
  untag (Argument t) = Argument $ untagged t

instance inductiveUntagged :: UnTagged tb b => UnTagged (Product (Argument (Tagged n a)) tb) (Product (Argument a) b) where
  untag (Product (Argument a) b) = Product (Argument (untagged a)) (untag b)

instance wrappedUntag :: UnTagged tb b => UnTagged (Constructor n2 tb) (Constructor n2 b) where
  untag (Constructor tb) = Constructor (untag tb)

--

data Event indexed nonindexed = Event indexed nonindexed

type TransferEventSep = Event (Tuple2 (Tagged N2 Address) (Tagged N1 Address)) (Singleton (Tagged N0 BigNumber))
type TransferEvent = Tuple3 Address Address BigNumber

toTransferEvent :: TransferEventSep -> TransferEvent
toTransferEvent (Event is nis) = to <<< untag $ insertIndexedMany (from is)  (from nis)

-}


module Web3.Utils.BigNumber
  ( BigNumber
  , class Algebra, embed
  , (*<), rmul
  , (>*), lmul
  , (+<), radd
  , (>+), ladd
  , (-<), rsub
  , (>-), lsub
  ) where

import Prelude

--------------------------------------------------------------------------------
-- * BigNumber
--------------------------------------------------------------------------------

foreign import data BigNumber :: Type

foreign import _showBigNumber :: BigNumber -> String

foreign import _eqBigNumber :: BigNumber -> BigNumber -> Boolean

instance showBigNumber :: Show BigNumber where
  show = _showBigNumber

instance eqBigNumber :: Eq BigNumber where
  eq = _eqBigNumber

foreign import _addBigNumber :: BigNumber -> BigNumber -> BigNumber

foreign import _mulBigNumber :: BigNumber -> BigNumber -> BigNumber

foreign import _intToBigNumber :: Int -> BigNumber

embedInt :: Int -> BigNumber
embedInt = _intToBigNumber

instance semiringBigNumber :: Semiring BigNumber where
  add = _addBigNumber
  mul = _mulBigNumber
  zero = embedInt 0
  one = embedInt 1

foreign import _subBigNumber :: BigNumber -> BigNumber -> BigNumber

instance ringBigNumber :: Ring BigNumber where
  sub = _subBigNumber

class (Ring r, Ring a) <= Algebra r a where
  embed :: r -> a

radd :: forall r a . Algebra r a => a -> r -> a
radd a r = a `add` embed r

ladd :: forall r a . Algebra r a => r -> a -> a
ladd r a = embed r `add` a

infixr 5 radd as +<
infixl 5 ladd as >+

rsub :: forall r a . Algebra r a => a -> r -> a
rsub a r = a `sub` embed r

lsub :: forall r a . Algebra r a => r -> a -> a
lsub r a = embed r `sub` a

infixr 5 rsub as -<
infixl 5 lsub as >-

rmul :: forall r a . Algebra r a => a -> r -> a
rmul a r = a `mul` embed r

lmul :: forall r a . Algebra r a => r -> a -> a
lmul r a = embed r `mul` a

infixr 5 rmul as *<
infixl 5 lmul as >*

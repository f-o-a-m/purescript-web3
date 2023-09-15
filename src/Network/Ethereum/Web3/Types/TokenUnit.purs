module Network.Ethereum.Web3.Types.TokenUnit
  ( class TokenUnit
  , fromMinorUnit
  , toMinorUnit
  , class TokenUnitSpec
  , divider
  , TokenK
  , TokenUnitK
  , Value
  , convert
  , formatValue
  , mkValue
  , generator
  , NoPay
  , MinorUnit
  , MinorUnitE3
  , MinorUnitE6
  , MinorUnitE9
  , MinorUnitE12
  , MinorUnitE15
  , MinorUnitE18
  , MinorUnitE21
  ) where

import Prelude

import Control.Monad.Gen (class MonadGen)
import Data.Maybe (fromJust)
import Data.Ring.Module (class LeftModule)
import Data.String (joinWith)
import Data.Unfoldable (replicate)
import Network.Ethereum.Core.BigNumber (BigNumber, decimal, fromInt, fromStringAs)
import Network.Ethereum.Core.BigNumber as BigNumber
import Partial.Unsafe (unsafePartial)
import Simple.JSON (class ReadForeign, class WriteForeign, writeImpl)
import Type.Proxy (Proxy(..))

data TokenK

data TokenUnitK

-- | A value of some token in specific denomination
newtype Value (a :: TokenUnitK) = Value BigNumber

derive newtype instance Eq (Value a)
derive newtype instance Ord (Value a)
derive newtype instance Show (Value a)
derive newtype instance ReadForeign (Value a)

generator :: forall m a proxy. MonadGen m => proxy a -> m (Value a)
generator _ = Value <$> BigNumber.generator

instance WriteForeign (Value (NoPay t)) where
  writeImpl _ = writeImpl (zero :: BigNumber)
else instance WriteForeign (Value a) where
  writeImpl (Value x) = writeImpl x

instance TokenUnitSpec a => Semigroup (Value a) where
  append a b = Value (unValue a `add` unValue b)

instance TokenUnitSpec a => Monoid (Value a) where
  mempty = mkValue zero

instance TokenUnitSpec a => LeftModule (Value a) Int where
  mzeroL = mkValue zero
  maddL (Value a) (Value b) = Value $ a + b
  msubL (Value a) (Value b) = Value $ a - b
  mmulL a (Value b) = Value $ fromInt a * b

instance TokenUnitSpec a => TokenUnit (Value a) where
  fromMinorUnit = Value
  toMinorUnit = unValue

unValue :: forall a. Value a -> BigNumber
unValue (Value a) = a

class TokenUnit :: Type -> Constraint
class TokenUnit a where
  fromMinorUnit :: BigNumber -> a
  toMinorUnit :: a -> BigNumber

-- | Convert between two denominations
convert :: forall a b. TokenUnit a => TokenUnit b => a -> b
convert = fromMinorUnit <<< toMinorUnit

class TokenUnitSpec (a :: TokenUnitK) where
  divider :: forall proxy. proxy a -> BigNumber

formatValue :: forall a. TokenUnitSpec a => Value a -> String
formatValue v = show $ toMinorUnit v `div` divider (Proxy :: Proxy a)

-- | Convert a big number into value, first using `floor` function to take the integer part
mkValue :: forall a. TokenUnitSpec a => BigNumber -> Value a
mkValue = Value <<< (mul (divider (Proxy :: Proxy a)))

foreign import data NoPay :: TokenK -> TokenUnitK

instance TokenUnitSpec (NoPay t) where
  divider = const zero

foreign import data MinorUnit :: TokenK -> TokenUnitK

instance TokenUnitSpec (MinorUnit t) where
  divider = createDivider 0

foreign import data MinorUnitE3 :: TokenK -> TokenUnitK

instance TokenUnitSpec (MinorUnitE3 t) where
  divider = createDivider 3

foreign import data MinorUnitE6 :: TokenK -> TokenUnitK

instance TokenUnitSpec (MinorUnitE6 t) where
  divider = createDivider 6

foreign import data MinorUnitE9 :: TokenK -> TokenUnitK

instance TokenUnitSpec (MinorUnitE9 t) where
  divider = createDivider 9

foreign import data MinorUnitE12 :: TokenK -> TokenUnitK

instance TokenUnitSpec (MinorUnitE12 t) where
  divider = createDivider 12

foreign import data MinorUnitE15 :: TokenK -> TokenUnitK

instance TokenUnitSpec (MinorUnitE15 t) where
  divider = createDivider 15

foreign import data MinorUnitE18 :: TokenK -> TokenUnitK

instance TokenUnitSpec (MinorUnitE18 t) where
  divider = createDivider 18

foreign import data MinorUnitE21 :: TokenK -> TokenUnitK

instance TokenUnitSpec (MinorUnitE21 t) where
  divider = createDivider 21

createDivider :: forall a. Int -> a -> BigNumber
createDivider denomination _ = unsafeConvert $ "1" <> joinWith "" (replicate denomination "0")
  where
  unsafeConvert :: String -> BigNumber
  unsafeConvert a = unsafePartial fromJust <<< fromStringAs decimal $ a

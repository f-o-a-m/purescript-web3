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
import Data.Maybe (fromJust)
import Data.Ring.Module (class LeftModule, (^*))
import Data.String (joinWith)
import Data.Unfoldable (replicate)
import Network.Ethereum.Core.BigNumber (BigNumber, decimal, floorBigNumber, parseBigNumber, divide)
import Partial.Unsafe (unsafePartial)
import Simple.JSON (class ReadForeign, class WriteForeign, writeImpl)
import Type.Proxy (Proxy(..))

data TokenK

data TokenUnitK

-- | A value of some token in specific denomination
newtype Value (a :: TokenUnitK)
  = Value BigNumber

derive newtype instance eqValue :: Eq (Value a)
derive newtype instance showValue :: Show (Value a)
derive newtype instance readFValue :: ReadForeign (Value a)

instance writeFNoPay :: WriteForeign (Value (NoPay t)) where
  writeImpl _ = writeImpl (zero :: BigNumber)
else instance writeFValue :: WriteForeign (Value a) where
  writeImpl (Value x) = writeImpl x

instance semigroupTokenUnitSpec :: TokenUnitSpec a => Semigroup (Value a) where
  append a b = Value (unValue a `add` unValue b)

instance monoidTokenUnitSpec :: TokenUnitSpec a => Monoid (Value a) where
  mempty = mkValue zero

instance modukeTokenUnitSpec :: TokenUnitSpec a => LeftModule (Value a) Int where
  mzeroL = mkValue zero
  maddL (Value a) (Value b) = Value $ a + b
  msubL (Value a) (Value b) = Value $ a - b
  mmulL a (Value b) = Value $ a ^* b

instance unitTokenUnitSpec :: TokenUnitSpec a => TokenUnit (Value a) where
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
formatValue v = show $ toMinorUnit v `divide` divider (Proxy :: Proxy a)

-- | Convert a big number into value, first using `floor` function to take the integer part
mkValue :: forall a. TokenUnitSpec a => BigNumber -> Value a
mkValue = Value <<< floorBigNumber <<< (mul (divider (Proxy :: Proxy a)))

foreign import data NoPay :: TokenK -> TokenUnitK

instance unitSpecNoPay :: TokenUnitSpec (NoPay t) where
  divider = const zero

foreign import data MinorUnit :: TokenK -> TokenUnitK

instance unitSpecMinorUnit :: TokenUnitSpec (MinorUnit t) where
  divider = createDivider 0

foreign import data MinorUnitE3 :: TokenK -> TokenUnitK

instance unitSpecMinorUnitE3 :: TokenUnitSpec (MinorUnitE3 t) where
  divider = createDivider 3

foreign import data MinorUnitE6 :: TokenK -> TokenUnitK

instance unitSpecMinorUnitE6 :: TokenUnitSpec (MinorUnitE6 t) where
  divider = createDivider 6

foreign import data MinorUnitE9 :: TokenK -> TokenUnitK

instance unitSpecMinorUnitE9 :: TokenUnitSpec (MinorUnitE9 t) where
  divider = createDivider 9

foreign import data MinorUnitE12 :: TokenK -> TokenUnitK

instance unitSpecMinorUnitE12 :: TokenUnitSpec (MinorUnitE12 t) where
  divider = createDivider 12

foreign import data MinorUnitE15 :: TokenK -> TokenUnitK

instance unitSpecMinorUnitE15 :: TokenUnitSpec (MinorUnitE15 t) where
  divider = createDivider 15

foreign import data MinorUnitE18 :: TokenK -> TokenUnitK

instance unitSpecMinorUnitE18 :: TokenUnitSpec (MinorUnitE18 t) where
  divider = createDivider 18

foreign import data MinorUnitE21 :: TokenK -> TokenUnitK

instance unitSpecMinorUnitE21 :: TokenUnitSpec (MinorUnitE21 t) where
  divider = createDivider 21

createDivider :: forall a. Int -> a -> BigNumber
createDivider denomination _ = unsafeConvert $ "1" <> joinWith "" (replicate denomination "0")
  where
  unsafeConvert :: String -> BigNumber
  unsafeConvert a = unsafePartial fromJust <<< parseBigNumber decimal $ a

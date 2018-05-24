module Network.Ethereum.Web3.Types.EtherUnit
  ( class EtherUnit, fromWei, toWei
  , class EtherUnitSpec
  , divider
  , name
  , convert
  , formatValue
  , mkValue
  , Wei
  , Babbage
  , Lovelace
  , Shannon
  , Szabo
  , Finney
  , Ether
  , KEther
  , Value
  , NoPay
  ) where

import Prelude

import Data.Foreign.Class (class Decode, class Encode, encode)
import Data.Maybe (fromJust)
import Data.Module (class LeftModule, (^*))
import Data.Monoid (class Monoid)
import Network.Ethereum.Core.BigNumber (BigNumber, decimal, floorBigNumber, parseBigNumber, divide)
import Partial.Unsafe (unsafePartial)
import Type.Proxy (Proxy(..))


-- | Ether value in denomination `a`
newtype Value a = Value BigNumber

derive newtype instance eqValue :: Eq (Value a)

derive newtype instance showValue :: Show (Value a)

derive newtype instance encodeValue ::  Encode (Value a)

instance encodeNoPay :: Encode (Value NoPay) where
  encode _ = encode (Value zero)

derive newtype instance decodeValue ::  Decode (Value a)

unValue :: forall a . Value a -> BigNumber
unValue (Value a) = a

-- | Useful for converting to and from the base denomination `Wei`
class EtherUnit a where
    fromWei :: BigNumber -> a
    toWei :: a -> BigNumber

-- | Convert between two denominations
convert :: forall a b . EtherUnit a => EtherUnit b => a -> b
convert = fromWei <<< toWei

class EtherUnitSpec a where
    divider :: Proxy a -> BigNumber
    name    :: Proxy a -> String

formatValue :: forall a. EtherUnitSpec a => Value a -> String
formatValue v = show $ toWei v `divide` divider (Proxy :: Proxy a)

-- | Convert a big number into value, first using `floor` function to take the integer part
mkValue :: forall a . EtherUnitSpec a => BigNumber -> Value a
mkValue = modify res <<< floorBigNumber <<< (mul (divider res))
  where res :: EtherUnitSpec a => Proxy a
        res = Proxy
        modify :: Proxy a -> BigNumber -> Value a
        modify _ = Value

instance unitEtherUnitSpec :: EtherUnitSpec a => EtherUnit (Value a) where
    fromWei = Value
    toWei = unValue

instance semigroupEtherUnitSpec :: EtherUnitSpec a => Semigroup (Value a) where
   append a b = Value (unValue a `add` unValue b)

instance monoidEtherUnitSpec :: EtherUnitSpec a => Monoid (Value a) where
   mempty = mkValue zero

instance modukeEtherUnitSpec :: EtherUnitSpec a => LeftModule (Value a) Int where
  mzeroL = mkValue zero
  maddL  (Value a) (Value b) = Value $ a + b
  msubL  (Value a) (Value b) = Value $ a - b
  mmulL a (Value b) = Value $ a ^* b

data Wei

instance unitSpecWei :: EtherUnitSpec Wei where
    divider = const $ unsafeConvert "1"
    name    = const "wei"

-- | Babbage unit type
data Babbage

instance unitSpecB :: EtherUnitSpec Babbage where
    divider = const $ unsafeConvert "1000"
    name    = const "babbage"

-- | Lovelace unit type
data Lovelace

instance unitSpecL :: EtherUnitSpec Lovelace where
    divider = const $ unsafeConvert "1000000"
    name    = const "lovelace"

-- | Shannon unit type
data Shannon

instance unitSpecS :: EtherUnitSpec Shannon where
    divider = const $ unsafeConvert "1000000000"
    name    = const "shannon"

-- | Szabo unit type
data Szabo

instance unitSpecSz :: EtherUnitSpec Szabo where
    divider = const $ unsafeConvert "1000000000000"
    name    = const "szabo"

-- | Finney unit type
data Finney

instance unitSpecF :: EtherUnitSpec Finney where
    divider = const $ unsafeConvert "1000000000000000"
    name    = const "finney"

-- | Ether unit type
data Ether

instance unitSpecE :: EtherUnitSpec Ether where
    divider = const $ unsafeConvert "1000000000000000000"
    name    = const "ether"

-- | KEther unit type
data KEther

instance unitSpecKE :: EtherUnitSpec KEther where
    divider = const $ unsafeConvert $ "1000000000000000000000"
    name    = const "kether"

data NoPay

instance unitSpecNoPay :: EtherUnitSpec NoPay where
    divider = const $ zero
    name    = const "nopay"

unsafeConvert :: String -> BigNumber
unsafeConvert a = unsafePartial fromJust <<< parseBigNumber decimal $ a


module Network.Ethereum.Web3.Types.Unit
  ( class Unit, fromWei, toWei
  , class UnitSpec
  , divider
  , name
  , convert
  , Wei
  , Babbage
  , Lovelace
  , Shannon
  , Szabo
  , Finney
  , Ether
  , KEther
  , Value
  , mkValue
  ) where

import Prelude

import Data.Foreign.Class (class Decode, class Encode)
import Data.Maybe (fromJust)
import Network.Ethereum.Web3.Types.BigNumber (BigNumber, decimal, floorBigNumber, parseBigNumber)
import Partial.Unsafe (unsafePartial)
import Type.Proxy (Proxy(..))


-- | Ether value in denomination `a`
newtype Value a = Value BigNumber

derive newtype instance eqValue :: Eq (Value a)

derive newtype instance showValue :: Show (Value a)

derive newtype instance encodeValue ::  Encode (Value a)

derive newtype instance decodeValue ::  Decode (Value a)

unValue :: forall a . Value a -> BigNumber
unValue (Value a) = a

-- | Useful for converting to and from the base denomination `Wei`
class Unit a where
    fromWei :: BigNumber -> a
    toWei :: a -> BigNumber

-- | Convert between two denominations
convert :: forall a b . Unit a => Unit b => a -> b
convert = fromWei <<< toWei

class UnitSpec a where
    divider :: Proxy a -> BigNumber
    name    :: Proxy a -> String

-- | Convert a big number into value, first using `floor` function to take the integer part
mkValue :: forall a . UnitSpec a => BigNumber -> Value a
mkValue = modify res <<< floorBigNumber <<< (mul (divider res))
  where res :: UnitSpec a => Proxy a
        res = Proxy
        modify :: Proxy a -> BigNumber -> Value a
        modify _ = Value

instance unitUnitSpec :: UnitSpec a => Unit (Value a) where
    fromWei = Value
    toWei = unValue

instance semiringUnitSpec :: UnitSpec a => Semiring (Value a) where
   add a b = Value (unValue a `add` unValue b)
   mul a b = Value (unValue a `mul` unValue b)
   zero = Value zero
   one = Value one

instance ringUnitSpec :: UnitSpec a => Ring (Value a) where
   sub a b = Value (unValue a `sub` unValue b)

data Wei

instance unitSpecWei :: UnitSpec Wei where
    divider = const $ unsafeConvert "1"
    name    = const "wei"

-- | Babbage unit type
data Babbage

instance unitSpecB :: UnitSpec Babbage where
    divider = const $ unsafeConvert "1000"
    name    = const "babbage"

-- | Lovelace unit type
data Lovelace

instance unitSpecL :: UnitSpec Lovelace where
    divider = const $ unsafeConvert "1000000"
    name    = const "lovelace"

-- | Shannon unit type
data Shannon

instance unitSpecS :: UnitSpec Shannon where
    divider = const $ unsafeConvert "1000000000"
    name    = const "shannon"

-- | Szabo unit type
data Szabo

instance unitSpecSz :: UnitSpec Szabo where
    divider = const $ unsafeConvert "1000000000000"
    name    = const "szabo"

-- | Finney unit type
data Finney

instance unitSpecF :: UnitSpec Finney where
    divider = const $ unsafeConvert "1000000000000000"
    name    = const "finney"

-- | Ether unit type
data Ether

instance unitSpecE :: UnitSpec Ether where
    divider = const $ unsafeConvert "1000000000000000000"
    name    = const "ether"

-- | KEther unit type
data KEther

instance unitSpecKE :: UnitSpec KEther where
    divider = const $ unsafeConvert $ "1000000000000000000000"
    name    = const "kether"

unsafeConvert :: String -> BigNumber
unsafeConvert a = unsafePartial $ fromJust <<< parseBigNumber decimal $ a

module Network.Ethereum.Web3.Solidity.EncodingType
  ( class EncodingType
  , typeName
  , isDynamic
  ) where

import Prelude

import Data.ByteString (ByteString)
import Data.Functor.Tagged (Tagged)
import Data.Reflectable (class Reflectable, reflectType)
import Network.Ethereum.Types (Address, BigNumber)
import Network.Ethereum.Web3.Solidity.Bytes (BytesN)
import Network.Ethereum.Web3.Solidity.Int (IntN)
import Network.Ethereum.Web3.Solidity.UInt (UIntN)
import Network.Ethereum.Web3.Solidity.Vector (Vector)
import Type.Proxy (Proxy(..))

class EncodingType :: forall k. k -> Constraint
class EncodingType a where
  typeName :: Proxy a -> String
  isDynamic :: Proxy a -> Boolean

instance encodingTypeBoolean :: EncodingType Boolean where
  typeName = const "bool"
  isDynamic = const false

instance encodingTypeInt :: EncodingType Int where
  typeName = const "int"
  isDynamic = const false

instance encodingTypeBigNumber :: EncodingType BigNumber where
  typeName = const "int"
  isDynamic = const false

instance encodingTypeUIntN :: Reflectable n Int => EncodingType (UIntN n) where
  typeName = const $ "uint" <> (show $ reflectType (Proxy :: Proxy n))
  isDynamic = const false

instance encodingTypeIntN :: Reflectable n Int => EncodingType (IntN n) where
  typeName = const $ "int" <> (show $ reflectType (Proxy :: Proxy n))
  isDynamic = const false

instance encodingTypeString :: EncodingType String where
  typeName = const "string"
  isDynamic = const true

instance encodingTypeAddress :: EncodingType Address where
  typeName = const "address"
  isDynamic = const false

instance encodingTypeArray :: EncodingType a => EncodingType (Array a) where
  typeName = const "[]"
  isDynamic = const true

instance encodingTypeBytes :: Reflectable n Int => EncodingType (BytesN n) where
  typeName =
    let
      n = show (reflectType (Proxy :: Proxy n))
    in
      const $ "bytes[" <> n <> "]"
  isDynamic = const false

instance encodingTypeVector :: (Reflectable n Int, EncodingType a) => EncodingType (Vector n a) where
  typeName =
    let
      n = show (reflectType (Proxy :: Proxy n))

      baseTypeName = typeName (Proxy :: Proxy a)
    in
      const $ baseTypeName <> "[" <> n <> "]"
  isDynamic _ = isDynamic (Proxy :: Proxy a)

instance encodingTypeBytesD :: EncodingType ByteString where
  typeName = const "bytes[]"
  isDynamic = const true

instance encodingTypeTagged :: EncodingType a => EncodingType (Tagged s a) where
  typeName _ = typeName (Proxy :: Proxy a)
  isDynamic _ = isDynamic (Proxy :: Proxy a)

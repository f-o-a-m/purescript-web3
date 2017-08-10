module Network.Ethereum.Web3.Encoding.EncodingType
  ( class EncodingType, typeName, isDynamic
  ) where

import Prelude
import Type.Proxy (Proxy(..))
import Data.Word (Word32)
import Data.ByteString (ByteString)

import Network.Ethereum.Web3.Types (Address, BigNumber)
import Network.Ethereum.Web3.Encoding.Size (class KnownSize, sizeVal)
import Network.Ethereum.Web3.Encoding.Bytes (BytesN)

--------------------------------------------------------------------------------

-- | Encoding Types
--------------------------------------------------------------------------------

class EncodingType a where
  typeName :: Proxy a -> String
  isDynamic :: Proxy a -> Boolean

instance encodingTypeBoolean :: EncodingType Boolean where
    typeName  = const "bool"
    isDynamic = const false

instance encodingTypeInt :: EncodingType Int where
    typeName  = const "int"
    isDynamic = const false

instance encodingTypeBigNumber:: EncodingType BigNumber where
    typeName  = const "int"
    isDynamic = const false

instance encodingTypeWord :: EncodingType Word32 where
    typeName  = const "uint"
    isDynamic = const false

instance encodingTypeString :: EncodingType String where
    typeName  = const "string"
    isDynamic = const true

instance encodingTypeAddress :: EncodingType Address where
    typeName  = const "address"
    isDynamic = const false

instance encodingTypeArray :: EncodingType a => EncodingType (Array a) where
    typeName  = const "[]"
    isDynamic = const true

instance encodingTypeBytes :: KnownSize n => EncodingType (BytesN n) where
    typeName  = let n = show (sizeVal (Proxy :: Proxy n))
                in const $ "bytes[" <> n <> "]"
    isDynamic = const false


instance encodingTypeBytesD :: EncodingType ByteString where
  typeName  = const "bytes[]"
  isDynamic = const true

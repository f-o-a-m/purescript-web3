module Network.Ethereum.Web3.Solidity.Size
  ( class KnownSize
  , sizeVal
  , class IntSize
  , class ByteSize
  ) where

import Data.Reflectable (class Reflectable, reflectType)
import Type.Proxy (Proxy(..))

class Reflectable n Int <= KnownSize (n :: Int) where
  sizeVal :: forall proxy. proxy n -> Int

instance (Reflectable n Int) => KnownSize n where
  sizeVal _ = reflectType (Proxy :: Proxy n)

-- | `IntSize` is empty class, if there is instance of `IntSize` for some number it means there
-- | is solidity type `int` of that size specific number in like `int16`, `int24` ... `int256`
class KnownSize n <= IntSize (n :: Int)
instance intSize8 :: IntSize 8
instance intSize16 :: IntSize 16
instance intSize24 :: IntSize 24
instance intSize32 :: IntSize 32
instance intSize40 :: IntSize 40
instance intSize48 :: IntSize 48
instance intSize56 :: IntSize 56
instance intSize64 :: IntSize 64
instance intSize72 :: IntSize 72
instance intSize80 :: IntSize 80
instance intSize88 :: IntSize 88
instance intSize96 :: IntSize 96
instance intSize104 :: IntSize 104
instance intSize112 :: IntSize 112
instance intSize120 :: IntSize 120
instance intSize128 :: IntSize 128
instance intSize136 :: IntSize 136
instance intSize144 :: IntSize 144
instance intSize152 :: IntSize 152
instance intSize160 :: IntSize 160
instance intSize168 :: IntSize 168
instance intSize176 :: IntSize 176
instance intSize184 :: IntSize 184
instance intSize192 :: IntSize 192
instance intSize200 :: IntSize 200
instance intSize208 :: IntSize 208
instance intSize216 :: IntSize 216
instance intSize224 :: IntSize 224
instance intSize232 :: IntSize 232
instance intSize240 :: IntSize 240
instance intSize248 :: IntSize 248
instance intSize256 :: IntSize 256

-- | `ByteSize` is empty class, if there is instance of `ByteSize` for some number it means there
-- | is solidity type `bytes` of that size specific number in like `bytes1`, `bytes2` ... `bytes32`
class KnownSize n <= ByteSize (n :: Int)
instance byteSize1  :: ByteSize 1
instance byteSize2  :: ByteSize 2
instance byteSize3  :: ByteSize 3
instance byteSize4  :: ByteSize 4
instance byteSize5  :: ByteSize 5
instance byteSize6  :: ByteSize 6
instance byteSize7  :: ByteSize 7
instance byteSize8  :: ByteSize 8
instance byteSize9  :: ByteSize 9
instance byteSize10 :: ByteSize 10
instance byteSize11 :: ByteSize 11
instance byteSize12 :: ByteSize 12
instance byteSize13 :: ByteSize 13
instance byteSize14 :: ByteSize 14
instance byteSize15 :: ByteSize 15
instance byteSize16 :: ByteSize 16
instance byteSize17 :: ByteSize 17
instance byteSize18 :: ByteSize 18
instance byteSize19 :: ByteSize 19
instance byteSize20 :: ByteSize 20
instance byteSize21 :: ByteSize 21
instance byteSize22 :: ByteSize 22
instance byteSize23 :: ByteSize 23
instance byteSize24 :: ByteSize 24
instance byteSize25 :: ByteSize 25
instance byteSize26 :: ByteSize 26
instance byteSize27 :: ByteSize 27
instance byteSize28 :: ByteSize 28
instance byteSize29 :: ByteSize 29
instance byteSize30 :: ByteSize 30
instance byteSize31 :: ByteSize 31
instance byteSize32 :: ByteSize 32

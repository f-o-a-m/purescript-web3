module Web3.Solidity.Types where

import Web3.Solidity.Param

type InputFormatter a = a -> SolidityParam

type OutputFormatter a = SolidityParam -> a

data SolidityType a =
  SolidityType { inputFormattter :: InputFormatter a
               , outputFormatter :: OutputFormatter a
               }

data StaticSizedArray a =
  StaticSizedArray { arrayLength :: Int
                   , arrayElements :: Array a
                   }

data SolidityInt =
    Int8
  | Int16
  | Int24
  | Int32
  | Int40
  | Int48
  | Int56
  | Int64
  | Int72
  | Int80
  | Int88
  | Int96
  | Int104
  | Int112
  | Int120
  | Int128
  | Int136
  | Int144
  | Int152
  | Int160
  | Int168
  | Int176
  | Int184
  | Int192
  | Int200
  | Int208
  | Int216
  | Int224
  | Int232
  | Int240
  | Int248
  | Int256

data UInt =
    UInt8
  | UInt16
  | UInt24
  | UInt32
  | UInt40
  | UInt48
  | UInt56
  | UInt64
  | UInt72
  | UInt80
  | UInt88
  | UInt96
  | UInt104
  | UInt112
  | UInt120
  | UInt128
  | UInt136
  | UInt144
  | UInt152
  | UInt160
  | UInt168
  | UInt176
  | UInt184
  | UInt192
  | UInt200
  | UInt208
  | UInt216
  | UInt224
  | UInt232
  | UInt240
  | UInt248
  | UInt256

data StaticSizedBytes =
    Bytes1
  | Bytes2
  | Bytes3
  | Bytes4
  | Bytes5
  | Bytes6
  | Bytes7
  | Bytes8
  | Bytes9
  | Bytes10
  | Bytes11
  | Bytes12
  | Bytes13
  | Bytes14
  | Bytes15
  | Bytes16
  | Bytes17
  | Bytes18
  | Bytes19
  | Bytes20
  | Bytes21
  | Bytes22
  | Bytes23
  | Bytes24
  | Bytes25
  | Bytes26
  | Bytes27
  | Bytes28
  | Bytes29
  | Bytes30
  | Bytes31
  | Bytes32

bytesLength :: StaticSizedBytes -> Int
bytesLength ssb = case ssb of
   Bytes1 -> 1
   Bytes2 -> 2
   Bytes3 -> 3
   Bytes4 -> 4
   Bytes5 -> 5
   Bytes6 -> 6
   Bytes7 -> 7
   Bytes8 -> 8
   Bytes9 -> 9
   Bytes10 -> 10
   Bytes11 -> 11
   Bytes12 -> 12
   Bytes13 -> 13
   Bytes14 -> 14
   Bytes15 -> 15
   Bytes16 -> 16
   Bytes17 -> 17
   Bytes18 -> 18
   Bytes19 -> 19
   Bytes20 -> 20
   Bytes21 -> 21
   Bytes22 -> 22
   Bytes23 -> 23
   Bytes24 -> 24
   Bytes25 -> 25
   Bytes26 -> 26
   Bytes27 -> 27
   Bytes28 -> 28
   Bytes29 -> 29
   Bytes30 -> 30
   Bytes31 -> 31
   Bytes32 -> 32

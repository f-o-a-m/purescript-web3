module Network.Ethereum.Web3.Types.EtherUnit
  ( ETHER
  , Wei
  , Babbage
  , Lovelace
  , Shannon
  , Szabo
  , Finney
  , Ether
  , KEther
  ) where


import Network.Ethereum.Web3.Types.TokenUnit (MinorUnit, MinorUnitE12, MinorUnitE15, MinorUnitE18, MinorUnitE21, MinorUnitE3, MinorUnitE6, MinorUnitE9, kind Token)

foreign import data ETHER :: Token

type Wei = MinorUnit ETHER
type Babbage = MinorUnitE3 ETHER
type Lovelace = MinorUnitE6 ETHER
type Shannon = MinorUnitE9 ETHER
type Szabo = MinorUnitE12 ETHER
type Finney = MinorUnitE15 ETHER
type Ether = MinorUnitE18 ETHER
type KEther = MinorUnitE21 ETHER


module Network.Ethereum.Web3.Solidity.Sizes
  ( S1, S2, S3, S4, S5, S6, S7, S8, S9, S10
  , s1, s2, s3, s4, s5, s6, s7, s8, s9, s10
  , S11, S12, S13, S14, S15, S16, S17, S18, S19, S20
  , s11, s12, s13, s14, s15, s16, s17, s18, s19, s20
  , S21, S22, S23, S24, S25, S26, S27, S28, S29, S30
  , s21, s22, s23, s24, s25, s26, s27, s28, s29, s30
  , S31, S32, S40, S48, S56, S64, S72, S80, S88, S96
  , s31, s32, s40, s48, s56, s64, s72, s80, s88, s96
  , S104, S112, S120, S128, S136, S144, S152, S160, S168, S176
  , s104, s112, s120, s128, s136, s144, s152, s160, s168, s176
  , S184, S192, S200, S208, S216, S224, S232, S240, S248, S256
  , s184, s192, s200, s208, s216, s224, s232, s240, s248, s256
  ) where

import Network.Ethereum.Web3.Solidity.Size (type (:%), type (:&), D0, D1, D2, D3, D4, D5, D6, D7, D8, D9, DLProxy(..), DOne)

type S1 = DOne D1
s1 :: DLProxy S1
s1 = DLProxy

type S2 = DOne D2
s2 :: DLProxy S2
s2 = DLProxy

type S3 = DOne D3
s3 :: DLProxy S3
s3 = DLProxy

type S4 = DOne D4
s4 :: DLProxy S4
s4 = DLProxy

type S5 = DOne D5
s5 :: DLProxy S5
s5 = DLProxy

type S6 = DOne D6
s6 :: DLProxy S6
s6 = DLProxy

type S7 = DOne D7
s7 :: DLProxy S7
s7 = DLProxy

type S8 = DOne D8
s8 :: DLProxy S8
s8 = DLProxy

type S9 = DOne D9
s9 :: DLProxy S9
s9 = DLProxy

type S10 = D1 :% D0
s10 :: DLProxy S10
s10 = DLProxy

type S11 = D1 :% D1
s11 :: DLProxy S11
s11 = DLProxy

type S12 = D1 :% D2
s12 :: DLProxy S12
s12 = DLProxy

type S13 = D1 :% D3
s13 :: DLProxy S13
s13 = DLProxy

type S14 = D1 :% D4
s14 :: DLProxy S14
s14 = DLProxy

type S15 = D1 :% D5
s15 :: DLProxy S15
s15 = DLProxy

type S16 = D1 :% D6
s16 :: DLProxy S16
s16 = DLProxy

type S17 = D1 :% D7
s17 :: DLProxy S17
s17 = DLProxy

type S18 = D1 :% D8
s18 :: DLProxy S18
s18 = DLProxy

type S19 = D1 :% D9
s19 :: DLProxy S19
s19 = DLProxy

type S20 = D2 :% D0
s20 :: DLProxy S20
s20 = DLProxy

type S21 = D2 :% D1
s21 :: DLProxy S21
s21 = DLProxy

type S22 = D2 :% D2
s22 :: DLProxy S22
s22 = DLProxy

type S23 = D2 :% D3
s23 :: DLProxy S23
s23 = DLProxy

type S24 = D2 :% D4
s24 :: DLProxy S24
s24 = DLProxy

type S25 = D2 :% D5
s25 :: DLProxy S25
s25 = DLProxy

type S26 = D2 :% D6
s26 :: DLProxy S26
s26 = DLProxy

type S27 = D2 :% D7
s27 :: DLProxy S27
s27 = DLProxy

type S28 = D2 :% D8
s28 :: DLProxy S28
s28 = DLProxy

type S29 = D2 :% D9
s29 :: DLProxy S29
s29 = DLProxy

type S30 = D3 :% D0
s30 :: DLProxy S30
s30 = DLProxy

type S31 = D3 :% D1
s31 :: DLProxy S31
s31 = DLProxy

type S32 = D3 :% D2
s32 :: DLProxy S32
s32 = DLProxy

type S40 = D4 :% D0
s40 :: DLProxy S40
s40 = DLProxy

type S48 = D4 :% D8
s48 :: DLProxy S48
s48 = DLProxy

type S56 = D5 :% D6
s56 :: DLProxy S56
s56 = DLProxy

type S64 = D6 :% D4
s64 :: DLProxy S64
s64 = DLProxy

type S72 = D7 :% D2
s72 :: DLProxy S72
s72 = DLProxy

type S80 = D8 :% D0
s80 :: DLProxy S80
s80 = DLProxy

type S88 = D8 :% D8
s88 :: DLProxy S88
s88 = DLProxy

type S96 = D9 :% D6
s96 :: DLProxy S96
s96 = DLProxy

type S104 = D1 :& D0 :% D4
s104 :: DLProxy S104
s104 = DLProxy

type S112 = D1 :& D1 :% D2
s112 :: DLProxy S112
s112 = DLProxy

type S120 = D1 :& D2 :% D0
s120 :: DLProxy S120
s120 = DLProxy

type S128 = D1 :& D2 :% D8
s128 :: DLProxy S128
s128 = DLProxy

type S136 = D1 :& D3 :% D6
s136 :: DLProxy S136
s136 = DLProxy

type S144 = D1 :& D4 :% D4
s144 :: DLProxy S144
s144 = DLProxy

type S152 = D1 :& D5 :% D2
s152 :: DLProxy S152
s152 = DLProxy

type S160 = D1 :& D6 :% D0
s160 :: DLProxy S160
s160 = DLProxy

type S168 = D1 :& D6 :% D8
s168 :: DLProxy S168
s168 = DLProxy

type S176 = D1 :& D7 :% D6
s176 :: DLProxy S176
s176 = DLProxy

type S184 = D1 :& D8 :% D4
s184 :: DLProxy S184
s184 = DLProxy

type S192 = D1 :& D9 :% D2
s192 :: DLProxy S192
s192 = DLProxy

type S200 = D2 :& D0 :% D0
s200 :: DLProxy S200
s200 = DLProxy

type S208 = D2 :& D0 :% D8
s208 :: DLProxy S208
s208 = DLProxy

type S216 = D2 :& D1 :% D6
s216 :: DLProxy S216
s216 = DLProxy

type S224 = D2 :& D2 :% D4
s224 :: DLProxy S224
s224 = DLProxy

type S232 = D2 :& D3 :% D2
s232 :: DLProxy S232
s232 = DLProxy

type S240 = D2 :& D4 :% D0
s240 :: DLProxy S240
s240 = DLProxy

type S248 = D2 :& D4 :% D8
s248 :: DLProxy S248
s248 = DLProxy

type S256 = D2 :& D5 :% D6
s256 :: DLProxy S256
s256 = DLProxy

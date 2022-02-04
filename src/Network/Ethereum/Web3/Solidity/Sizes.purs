module Network.Ethereum.Web3.Solidity.Sizes
  ( S1
  , S2
  , S3
  , S4
  , S5
  , S6
  , S7
  , S8
  , S9
  , S10
  , s1
  , s2
  , s3
  , s4
  , s5
  , s6
  , s7
  , s8
  , s9
  , s10
  , S11
  , S12
  , S13
  , S14
  , S15
  , S16
  , S17
  , S18
  , S19
  , S20
  , s11
  , s12
  , s13
  , s14
  , s15
  , s16
  , s17
  , s18
  , s19
  , s20
  , S21
  , S22
  , S23
  , S24
  , S25
  , S26
  , S27
  , S28
  , S29
  , S30
  , s21
  , s22
  , s23
  , s24
  , s25
  , s26
  , s27
  , s28
  , s29
  , s30
  , S31
  , S32
  , S40
  , S48
  , S56
  , S64
  , S72
  , S80
  , S88
  , S96
  , s31
  , s32
  , s40
  , s48
  , s56
  , s64
  , s72
  , s80
  , s88
  , s96
  , S104
  , S112
  , S120
  , S128
  , S136
  , S144
  , S152
  , S160
  , S168
  , S176
  , s104
  , s112
  , s120
  , s128
  , s136
  , s144
  , s152
  , s160
  , s168
  , s176
  , S184
  , S192
  , S200
  , S208
  , S216
  , S224
  , S232
  , S240
  , S248
  , S256
  , s184
  , s192
  , s200
  , s208
  , s216
  , s224
  , s232
  , s240
  , s248
  , s256
  ) where

import Network.Ethereum.Web3.Solidity.Size (type (:%), type (:&), D0, D1, D2, D3, D4, D5, D6, D7, D8, D9, DOne)
import Type.Proxy (Proxy(..))

type S1
  = DOne D1

s1 :: Proxy S1
s1 = Proxy

type S2
  = DOne D2

s2 :: Proxy S2
s2 = Proxy

type S3
  = DOne D3

s3 :: Proxy S3
s3 = Proxy

type S4
  = DOne D4

s4 :: Proxy S4
s4 = Proxy

type S5
  = DOne D5

s5 :: Proxy S5
s5 = Proxy

type S6
  = DOne D6

s6 :: Proxy S6
s6 = Proxy

type S7
  = DOne D7

s7 :: Proxy S7
s7 = Proxy

type S8
  = DOne D8

s8 :: Proxy S8
s8 = Proxy

type S9
  = DOne D9

s9 :: Proxy S9
s9 = Proxy

type S10
  = D1 :% D0

s10 :: Proxy S10
s10 = Proxy

type S11
  = D1 :% D1

s11 :: Proxy S11
s11 = Proxy

type S12
  = D1 :% D2

s12 :: Proxy S12
s12 = Proxy

type S13
  = D1 :% D3

s13 :: Proxy S13
s13 = Proxy

type S14
  = D1 :% D4

s14 :: Proxy S14
s14 = Proxy

type S15
  = D1 :% D5

s15 :: Proxy S15
s15 = Proxy

type S16
  = D1 :% D6

s16 :: Proxy S16
s16 = Proxy

type S17
  = D1 :% D7

s17 :: Proxy S17
s17 = Proxy

type S18
  = D1 :% D8

s18 :: Proxy S18
s18 = Proxy

type S19
  = D1 :% D9

s19 :: Proxy S19
s19 = Proxy

type S20
  = D2 :% D0

s20 :: Proxy S20
s20 = Proxy

type S21
  = D2 :% D1

s21 :: Proxy S21
s21 = Proxy

type S22
  = D2 :% D2

s22 :: Proxy S22
s22 = Proxy

type S23
  = D2 :% D3

s23 :: Proxy S23
s23 = Proxy

type S24
  = D2 :% D4

s24 :: Proxy S24
s24 = Proxy

type S25
  = D2 :% D5

s25 :: Proxy S25
s25 = Proxy

type S26
  = D2 :% D6

s26 :: Proxy S26
s26 = Proxy

type S27
  = D2 :% D7

s27 :: Proxy S27
s27 = Proxy

type S28
  = D2 :% D8

s28 :: Proxy S28
s28 = Proxy

type S29
  = D2 :% D9

s29 :: Proxy S29
s29 = Proxy

type S30
  = D3 :% D0

s30 :: Proxy S30
s30 = Proxy

type S31
  = D3 :% D1

s31 :: Proxy S31
s31 = Proxy

type S32
  = D3 :% D2

s32 :: Proxy S32
s32 = Proxy

type S40
  = D4 :% D0

s40 :: Proxy S40
s40 = Proxy

type S48
  = D4 :% D8

s48 :: Proxy S48
s48 = Proxy

type S56
  = D5 :% D6

s56 :: Proxy S56
s56 = Proxy

type S64
  = D6 :% D4

s64 :: Proxy S64
s64 = Proxy

type S72
  = D7 :% D2

s72 :: Proxy S72
s72 = Proxy

type S80
  = D8 :% D0

s80 :: Proxy S80
s80 = Proxy

type S88
  = D8 :% D8

s88 :: Proxy S88
s88 = Proxy

type S96
  = D9 :% D6

s96 :: Proxy S96
s96 = Proxy

type S104
  = D1 :& D0 :% D4

s104 :: Proxy S104
s104 = Proxy

type S112
  = D1 :& D1 :% D2

s112 :: Proxy S112
s112 = Proxy

type S120
  = D1 :& D2 :% D0

s120 :: Proxy S120
s120 = Proxy

type S128
  = D1 :& D2 :% D8

s128 :: Proxy S128
s128 = Proxy

type S136
  = D1 :& D3 :% D6

s136 :: Proxy S136
s136 = Proxy

type S144
  = D1 :& D4 :% D4

s144 :: Proxy S144
s144 = Proxy

type S152
  = D1 :& D5 :% D2

s152 :: Proxy S152
s152 = Proxy

type S160
  = D1 :& D6 :% D0

s160 :: Proxy S160
s160 = Proxy

type S168
  = D1 :& D6 :% D8

s168 :: Proxy S168
s168 = Proxy

type S176
  = D1 :& D7 :% D6

s176 :: Proxy S176
s176 = Proxy

type S184
  = D1 :& D8 :% D4

s184 :: Proxy S184
s184 = Proxy

type S192
  = D1 :& D9 :% D2

s192 :: Proxy S192
s192 = Proxy

type S200
  = D2 :& D0 :% D0

s200 :: Proxy S200
s200 = Proxy

type S208
  = D2 :& D0 :% D8

s208 :: Proxy S208
s208 = Proxy

type S216
  = D2 :& D1 :% D6

s216 :: Proxy S216
s216 = Proxy

type S224
  = D2 :& D2 :% D4

s224 :: Proxy S224
s224 = Proxy

type S232
  = D2 :& D3 :% D2

s232 :: Proxy S232
s232 = Proxy

type S240
  = D2 :& D4 :% D0

s240 :: Proxy S240
s240 = Proxy

type S248
  = D2 :& D4 :% D8

s248 :: Proxy S248
s248 = Proxy

type S256
  = D2 :& D5 :% D6

s256 :: Proxy S256
s256 = Proxy

let upstream =
      https://raw.githubusercontent.com/f-o-a-m/package-sets/09b71674a327f7601276846c1afb537342bb57ff/purs-0.15.7-web3.dhall
        sha256:7e32f0c65a7b5d334ee98d7fda1d7d3a557b6b478421f545694bd8e1cd4d16ac

let overrides = {=}

let additions = {=}

in  upstream // overrides // additions

let upstream =
      https://github.com/purescript/package-sets/releases/download/psc-0.15.7-20230306/packages.dhall
        sha256:0757626c7422b8b5b5b1d0df3d3628e5deac755d7f89c433a9bf89009787dcbd

let overrides = {=}

let additions =
      { bytestrings =
        { dependencies =
          [ "arrays"
          , "console"
          , "effect"
          , "exceptions"
          , "foldable-traversable"
          , "integers"
          , "leibniz"
          , "maybe"
          , "newtype"
          , "node-buffer"
          , "partial"
          , "prelude"
          , "quickcheck"
          , "quickcheck-laws"
          , "quotient"
          , "unsafe-coerce"
          ]
        , repo =
            "https://github.com/rightfold/purescript-bytestrings"
        , version = "6733a32fca306015b3428e9985ffac65325a9864"
        }
      , coroutine-transducers =
        { dependencies =
          [ "aff", "coroutines", "effect", "maybe", "psci-support" ]
        , repo =
            "https://github.com/blinky3713/purescript-coroutine-transducers"
        , version = "v1.0.0"
        }
      , errors =
        { dependencies =
          [ "control"
          , "effect"
          , "either"
          , "identity"
          , "maybe"
          , "newtype"
          , "prelude"
          , "test-unit"
          , "transformers"
          ]
        , repo = "https://github.com/passy/purescript-errors"
        , version = "670485beb1e026f77d52ca58ce10c145d96c11ba"
        }
      , eth-core =
        { dependencies =
          [ "argonaut"
          , "bytestrings"
          , "console"
          , "debug"
          , "effect"
          , "ordered-collections"
          , "parsing"
          , "prelude"
          , "psci-support"
          , "ring-modules"
          , "simple-json"
          ]
        , repo = "https://github.com/f-o-a-m/purescript-eth-core.git"
        , version = "v8.0.0"
        }
      , quotient =
        { dependencies = [ "prelude", "quickcheck" ]
        , repo = "https://github.com/rightfold/purescript-quotient.git"
        , version = "v3.0.0"
        }
      , tagged =
        { dependencies = [ "identity", "profunctor" ]
        , repo = "https://github.com/LiamGoodacre/purescript-tagged"
        , version = "v4.0.2"
        }
      }

in  upstream // overrides // additions

let upstream =
      https://github.com/purescript/package-sets/releases/download/psc-0.14.5-20220127/packages.dhall sha256:8ccbd53dbc7dbfd92a9cba9cca7a8bf36cb120a0a3e21106bf19a16d3ad6863e

let overrides = {=}

let additions =
      { coroutine-transducers =
        { dependencies =
          [ "aff", "coroutines", "effect", "maybe", "psci-support" ]
        , repo =
            "https://github.com/blinky3713/purescript-coroutine-transducers"
        , version = "v1.0.0"
        }
      , eth-core =
        { dependencies =
          [ "argonaut"
          , "bytestrings"
          , "console"
          , "debug"
          , "effect"
          , "foreign-generic"
          , "ordered-collections"
          , "parsing"
          , "prelude"
          , "psci-support"
          , "ring-modules"
          , "simple-json"
          ]
        , repo = "https://github.com/srghma/purescript-eth-core.git"
        , version = "purs-14"
        }
      , tagged =
        { dependencies = [ "identity", "profunctor" ]
        , repo = "https://github.com/kejace/purescript-tagged"
        , version = "v0.14"
        }
      }

in  upstream // overrides // additions

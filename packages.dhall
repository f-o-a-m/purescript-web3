let upstream =
      https://github.com/purescript/package-sets/releases/download/psc-0.14.2-20210629/packages.dhall sha256:534c490bb73cae75adb5a39871142fd8db5c2d74c90509797a80b8bb0d5c3f7b

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
        , repo = "https://github.com/f-o-a-m/purescript-eth-core.git"
        , version = "v0.14"
        }
      , tagged =
        { dependencies = [ "identity", "profunctor" ]
        , repo = "https://github.com/kejace/purescript-tagged"
        , version = "v0.14"
        }
      }

in  upstream // overrides // additions

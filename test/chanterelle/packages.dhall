let upstream =
      https://github.com/purescript/package-sets/releases/download/psc-0.14.2-20210629/packages.dhall sha256:534c490bb73cae75adb5a39871142fd8db5c2d74c90509797a80b8bb0d5c3f7b

let overrides = {=}

let additions =
      { web3 =
        { dependencies =
          [ "aff"
          , "avar"
          , "console"
          , "coroutines"
          , "coroutine-transducers"
          , "debug"
          , "effect"
          , "errors"
          , "eth-core"
          , "foreign"
          , "foreign-generic"
          , "fork"
          , "free"
          , "heterogeneous"
          , "identity"
          , "parsing"
          , "partial"
          , "profunctor-lenses"
          , "psci-support"
          , "tagged"
          , "transformers"
          , "typelevel-prelude"
          , "variant"
          ]
        , repo = "https://github.com/f-o-a-m/purescript-web3"
        , version = "v0.14"
        }
      , web3-generator =
          { dependencies =
            [ "ansi"
            , "argonaut"
            , "console"
            , "effect"
            , "errors"
            , "eth-core"
            , "fixed-points"
            , "mkdirp"
            , "node-fs-aff"
            , "ordered-collections"
            , "prelude"
            , "psci-support"
            , "record-extra"
            , "string-parsers"
            , "web3"
            , "yargs"
            ]
          , repo = "https://github.com/f-o-a-m/purescript-web3-generator"
          , version = "v0.14"
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
        , repo =
            "https://github.com/f-o-a-m/purescript-eth-core.git"
        , version =
            "v0.14"
        }
      , coroutine-transducers =
        { dependencies =
            [ "aff"
            , "coroutines"
            , "effect"
            , "maybe"
            , "psci-support"
            ]
        , repo =
            "https://github.com/blinky3713/purescript-coroutine-transducers"
        , version =
            "v1.0.0"
        }
      , solc = 
        { dependencies =
          [ "aff"
          , "argonaut"
          , "console"
          , "effect"
          , "node-path"
          , "prelude"
          , "psci-support"
          , "web3"
          ]
        , repo =
            "https://github.com/f-o-a-m/purescript-solc"
        , version =
            "v0.14"
        }
      , mkdirp =
        { dependencies =
            [ "console"
            , "effect"
            , "either"
            , "exceptions"
            , "functions"
            , "node-fs"
            , "nullable"
            , "prelude"
            , "psci-support"
            ]
        , repo =
            "https://github.com/f-o-a-m/purescript-mkdirp"
        , version =
            "v1.0.0"
        }
      , tagged =
        { dependencies =
            [ "identity"
            , "profunctor"
            ]
        , repo =
            "https://github.com/kejace/purescript-tagged"
        , version =
            "v0.14"
        }
      }

in  upstream // overrides // additions

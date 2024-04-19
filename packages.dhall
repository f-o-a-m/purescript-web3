let upstream =
      https://github.com/purescript/package-sets/releases/download/psc-0.15.7-20230306/packages.dhall
        sha256:0757626c7422b8b5b5b1d0df3d3628e5deac755d7f89c433a9bf89009787dcbd
      with js-bigints.version = "v2.2.0"

let eth-core-deps =
      https://raw.githubusercontent.com/f-o-a-m/purescript-eth-core/master/packages.dhall
        sha256:4f447424877e5d16a78740ab32b371c15ff33728259cbaf0b181ad2ed84478db

let additions =
      { bytestrings = eth-core-deps.bytestrings
      , coroutine-transducers = 
        { dependencies = [
            "console",
            "either",
            "foldable-traversable",
            "freet",
            "functors",
            "newtype",
            "parallel",
            "prelude",
            "tailrec",
            "transformers",
            "tuples",
            "aff",
            "coroutines",
            "effect",
            "maybe",
            "psci-support"
            ]
        , repo = "https://github.com/martyall/purescript-coroutine-transducers"
        , version = "v1.0.0"
        }
      , eth-core =
        { dependencies =
          [ "argonaut"
          , "arrays"
          , "bytestrings"
          , "effect"
          , "either"
          , "foldable-traversable"
          , "foreign"
          , "functions"
          , "gen"
          , "integers"
          , "js-bigints"
          , "lists"
          , "maybe"
          , "newtype"
          , "node-buffer"
          , "nonempty"
          , "ordered-collections"
          , "partial"
          , "prelude"
          , "quotient"
          , "simple-json"
          , "strings"
          , "unfoldable"
          , "unsafe-coerce"
          ]
        , repo = "https://github.com/f-o-a-m/purescript-eth-core"
        , version = "v10.0.0"
        }
      , quotient = eth-core-deps.quotient
      }

in  upstream // additions

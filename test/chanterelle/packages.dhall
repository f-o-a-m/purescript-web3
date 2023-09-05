let upstream =
      https://raw.githubusercontent.com/f-o-a-m/chanterelle/v6.0.0/packages.dhall
        sha256:0366ca8491b2d8e59ce763ae9af541b8d44145a5ad593fa9f6198a070199d5fc
      with eth-core.repo = "https://github.com/f-o-a-m/purescript-eth-core"
      with eth-core.version = "v8.0.0"
      with web3 = ../../spago.dhall as Location
      with web3-generator.repo = "https://github.com/f-o-a-m/purescript-web3-generator"
      with web3-generator.version = "a7f81d900b7fc8f029b0474c5cb0affbae82e0c7"

let additions =
      { chanterelle =
        { dependencies =
          [ "console"
          , "debug"
          , "effect"
          , "foreign-object"
          , "logging"
          , "mkdirp"
          , "node-process"
          , "optparse"
          , "prelude"
          , "psci-support"
          , "solc"
          , "validation"
          , "web3"
          , "web3-generator"
          ]
        , repo = "https://github.com/f-o-a-m/chanterelle"
        , version = "v6.0.0"
        }
        , language-cst-parser =
        { dependencies =
          [ "arrays"
          , "const"
          , "effect"
          , "either"
          , "foldable-traversable"
          , "free"
          , "functors"
          , "maybe"
          , "numbers"
          , "ordered-collections"
          , "strings"
          , "transformers"
          , "tuples"
          , "typelevel-prelude"
          ]
        , repo =
            "https://github.com/natefaubion/purescript-language-cst-parser.git"
        , version = "v0.9.1"
        }
      , dodo-printer =
        { dependencies =
          [ "ansi", "foldable-traversable", "lists", "maybe", "strings" ]
        , repo = "https://github.com/natefaubion/purescript-dodo-printer.git"
        , version = "v2.1.0"
        }
      , tidy =
        { dependencies =
          [ "arrays"
          , "dodo-printer"
          , "foldable-traversable"
          , "lists"
          , "maybe"
          , "ordered-collections"
          , "partial"
          , "prelude"
          , "language-cst-parser"
          , "strings"
          , "tuples"
          ]
        , repo = "https://github.com/natefaubion/purescript-tidy.git"
        , version = "v0.5.3"
        }
      , language-cst-codegen =
        { dependencies =
          [ "aff"
          , "ansi"
          , "arrays"
          , "avar"
          , "bifunctors"
          , "console"
          , "control"
          , "dodo-printer"
          , "effect"
          , "either"
          , "enums"
          , "exceptions"
          , "filterable"
          , "foldable-traversable"
          , "free"
          , "identity"
          , "integers"
          , "language-cst-parser"
          , "lazy"
          , "lists"
          , "maybe"
          , "newtype"
          , "node-buffer"
          , "node-child-process"
          , "node-fs-aff"
          , "node-path"
          , "node-process"
          , "node-streams"
          , "ordered-collections"
          , "parallel"
          , "partial"
          , "posix-types"
          , "prelude"
          , "record"
          , "safe-coerce"
          , "strings"
          , "tidy"
          , "transformers"
          , "tuples"
          , "type-equality"
          , "unicode"
          ]
        , repo = "https://github.com/natefaubion/purescript-tidy-codegen.git"
        , version = "v1.1.1"
        }
      }

in  upstream // additions

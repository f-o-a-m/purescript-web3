let conf = ./spago.dhall

in    conf
    ⫽ { sources = conf.sources # [ "test/web3/**/*.purs" ]
      , dependencies =
            conf.dependencies
          # [ "spec"
            , "node-buffer"
            , "argonaut"
            , "arrays"
            , "bifunctors"
            , "bytestrings"
            , "control"
            , "either"
            , "exceptions"
            , "foldable-traversable"
            , "foreign-object"
            , "integers"
            , "lists"
            , "maybe"
            , "newtype"
            , "orders"
            , "parallel"
            , "prelude"
            , "quotient"
            , "record"
            , "ring-modules"
            , "simple-json"
            , "strings"
            , "tailrec"
            , "tuples"
            , "unfoldable"
            , "unsafe-coerce"
            , "avar"
            , "console"
            , "identity"
            ]
      }

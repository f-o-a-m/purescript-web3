let conf = ./spago.dhall

in    conf
    ⫽ { sources = conf.sources # [ "test/web3/**/*.purs" ]
      , dependencies =
            conf.dependencies
          # [ "console"
            , "enums"
            , "identity"
            , "integers"
            , "lists"
            , "node-buffer"
            , "nonempty"
            , "quickcheck"
            , "quickcheck-laws"
            , "quotient"
            , "spec"
            , "unsafe-coerce"
            ]
      }

{-
Welcome to a Spago project!
You can edit this file as you like.
-}
{ name = "web3-generator"
, dependencies =
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
, packages = ../packages.dhall
, sources = [ "src/**/*.purs" ]
}

{-
Welcome to a Spago project!
You can edit this file as you like.
-}
{ name = "web3"
, dependencies =
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
  , "proxy"
  , "psci-support"
  , "tagged"
  , "transformers"
  , "typelevel-prelude"
  , "variant"
  ]
, packages = ../packages.dhall
, sources = [ "src/**/*.purs" ]
}

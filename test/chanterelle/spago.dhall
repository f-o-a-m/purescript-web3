{-
Welcome to a Spago project!
You can edit this file as you like.
-}
{ name = "web3-tests"
, dependencies =
  [ "console", "effect", "prelude", "chanterelle", "language-cst-codegen" ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs" ]
}

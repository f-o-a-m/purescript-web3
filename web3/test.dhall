let conf = ./spago.dhall

in conf // {
  sources = conf.sources # [ "test/web3/**/*.purs" ],
  dependencies = conf.dependencies # [ "spec", "node-buffer" ]
}

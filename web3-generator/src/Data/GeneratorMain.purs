module Data.GeneratorMain where

import Prelude

import Effect.Aff (launchAff_)
import Effect ( Effect)
import Data.CodeGen (generatePS)
import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Node.Yargs.Applicative (yarg, flag, runY)
import Node.Yargs.Setup (usage, defaultVersion, defaultHelp, example)

generatorMain :: Effect Unit
generatorMain = do
    conf <- runY setup mkConf
    launchAff_ $ generatePS conf
  where
    setup =
      usage "$0 --abis <abis> --dest <dest> ./src --prefix <prefix> --module <module> --truffle <bool>"
        <> example "$0 --abis ./contract-abis --dest ./src --prefix eth_ --module MyApp.Contracts --truffle false " "Generate contract code from solidity compiler output in current directory with prefix `eth_` into the `src/MyApp/Contracts/` directory with module name `Myapp.Contracts.*`"
        <> defaultVersion
        <> defaultHelp
    mkConf =
      mkConf'
        <$> yarg "abis" [] Nothing (Right "Must specify abi source directory.") true
        <*> yarg "dest" [] Nothing (Left "./src") false
        <*> yarg "prefix" [] Nothing (Left "") false
        <*> yarg "module" [] Nothing (Left "Contracts") false
        <*> flag "truffle" [] (Just "Are the abi files truffle artifacts")
    mkConf' jsonDir pursDir exprPrefix modulePrefix truffle = pure
      { jsonDir
      , pursDir
      , exprPrefix
      , modulePrefix
      , truffle
      }



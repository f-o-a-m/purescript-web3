# purescript-web3
<img src=https://github.com/f-o-a-m/purescript-web3/blob/master/purescript-web3-logo.png width="75">


[![Latest release](http://img.shields.io/github/release/f-o-a-m/purescript-web3.svg?branch=master)](https://github.com/f-o-a-m/purescript-web3/releases)
[![purescript-web3 on Pursuit](https://pursuit.purescript.org/packages/purescript-web3/badge)](https://pursuit.purescript.org/packages/purescript-web3)
[![Build status](https://travis-ci.org/f-o-a-m/purescript-web3.svg?branch=master)](https://travis-ci.org/f-o-a-m/purescript-web3?branch=master)

# A Purescript Client for the Web3 API

`purescript-web3` is a library for interacting with an ethereum node purescript. At the moment it covers most endpoints of the web3 api, which means it is suitable for sending transactions, querying blockchain state and metadata, and monitoring events.

Using web3-generator it is also possible (and recommended) to generate a library from a set of smart contract abis which is capable of templating transactions and event filters/watchers. The README has instructions for getting started.

To see an example project using all of the purescript-web3 tools and with thermite/react ui, check out [purescript-web3-example](https://github.com/f-o-a-m/purescript-web3-example).

## Build Instructions
```
> cd web3
> npm install
> npm run build
> npm run test
```

## Examples

Suppose we have the following solidity smart contract:

```solidity
contract TupleStorage {
    
    uint x;
    uint y;
    
    event TupleSet(uint newX, uint newY);
    
    function setTuple(uint _x, uint _y) public {
        x = _x;
        y = _y;
        TupleSet(_x, _y);
    }
    
}
```

If we used web3-generator, we are given a function with the following signature:

```purescript
setTuple :: forall e.
            TransactionOptions NoPay 
         -> {_x :: UInt (D2 :& D5 :& D6), _y :: UInt (D2 :& D5 :& D6)} 
         -> Web3 e HexString 
```

It's pretty clear what this function is doing, but let's look at the `TransactionOptions`. This record keeps track of, for example, who is the transaction from, what contract address is it going to, is there ether being sent, etc. In this case, the function is not "payable", so this is indicated in the type of the `TransactionOptions`. It is set using lenses like:

```purescript
setTupleOpts = defaultTransactionOptions
             # _from ?~ myAddress
             # _to ?~ tupleStorageAddress
```
Now for the `TupleSet` event. In order to start an event watcher, we need to establish the `Filter`, which specifies things like the range of blocks we are interested in, and how to find that particular contract and topic. Again, if you're using web3-generator, things are a lot simpler:

```purescript
tupleFilter = eventFilter (Proxy :: Proxy TupleSet) tupleStorageAddress 
           # _fromBlock .~ BN 100
```

We also need to pass a callback to the event watcher that performs some action and decides whether or not to unregister the filter. For example, we could set up an event monitor starting from block 100 and continuing until the two coordinates that are set are equal:

```purescript
event tupleFilter $ \(TupleSet {newX,newY} -> do
  liftAff <<< log $ "Received New Tuple : " <> show (Tuple newX newY) 
  if newX == newY
    then pure TerminateEvent
    else do
      _ <- performAction newX newY
      pure ContinueEvent
```

For more examples, check out the foam [kitty monitor](https://github.com/f-o-a-m/purescript-kitty-monitor), the [example](https://github.com/f-o-a-m/purescript-web3-example) project using thermite, or the [purescript-web3-tests](https://github.com/f-o-a-m/purescript-web3-tests) repo.

## Resources
 
 - [web3 RPC spec](https://github.com/ethereum/wiki/wiki/JSON-RPC)
 - [solidity documentation](http://solidity.readthedocs.io/en/develop/index.html)



# purescript-web3-generator

Generats purescript modules from Solidity ABIs

## Requirements

- `npm`

## Getting Started
```
> cd web3-generator
> npm install
> npm run build
> npm run test
```

## How to use it

For a complete example that follows the steps below, see [`purescript-web3-example`](https://github.com/f-o-a-m/purescript-web3-example).

We use `purescript-web3-generator` in the absence of template-purescript. Suggested usage is as follows:

1. Create a directory `generator/` in your project with a file that looks like this

```purescript

module Generator where

import Data.GeneratorMain (generatorMain)

main = generatorMain

```

2. From there, add a build step 

```sh

pulp run -m Generator --src-path generator -- --abis <abis> --dest src --module Contracts

```

(note that we specify both a different source directory than `src` and a different module `Generator` that `purs` is looking for `main` in)


Until [this issue](https://github.com/purescript-contrib/pulp/issues/309) is fixed, we have to temporarily replace the step above with something like this

```sh
pulp build -m Generator --src-path generator --to generator.js
node generator.js --abis <abis> --dest src --module Contracts
rm generator.js
```

3. Now you should have created contract modules for each contract into your `src/Contracts` directory that your code can depend on.
  you could use `--module MyApp.Contracts` for example too and it will result in modules created in `src/MyApp/Contracts`

## Resources

 - [web3.js repo](https://github.com/ethereum/web3.js)
 - [web3 Javascript API wiki](https://github.com/ethereum/wiki/wiki/JavaScript-API)


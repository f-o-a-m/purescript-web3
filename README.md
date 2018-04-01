# purescript-web3
<img src=https://github.com/f-o-a-m/purescript-web3/blob/master/purescript-web3-logo.png width="75">


[![Latest release](http://img.shields.io/github/release/f-o-a-m/purescript-web3.svg?branch=master)](https://github.com/f-o-a-m/purescript-web3/releases)
[![purescript-web3 on Pursuit](https://pursuit.purescript.org/packages/purescript-web3/badge)](https://pursuit.purescript.org/packages/purescript-web3)
[![Build status](https://travis-ci.org/f-o-a-m/purescript-web3.svg?branch=master)](https://travis-ci.org/f-o-a-m/purescript-web3?branch=master)

# A Purescript Client for the Web3 API

`purescript-web3` is a library for interacting with an ethereum node purescript. At the moment it covers most endpoints of the web3 api, which means it is suitable for sending transactions, querying blockchain state and metadata, and monitoring events.

Using [purescript-web3-generator](https://github.com/f-o-a-m/purescript-web3-generator) it is also possible (and recommended) to generate a library from a set of smart contract abis which is capable of templating transactions and event filters/watchers. The README has instructions for getting started.

We do not yet have a build tool similar to truffle, but if you are looking for a template of how to use truffle and write your tests using purescript, check out out the [purescript-web3-tests](https://github.com/f-o-a-m/purescript-web3-tests)

To see an example project using all of the purescript-web3 tools and with thermite/react ui, check out [purescript-web3-example](https://github.com/f-o-a-m/purescript-web3-example).

## Build Instructions
```
> npm install
> npm run build
> npm run test
```

## Documentation

Module documentation is [published on Pursuit](http://pursuit.purescript.org/packages/purescript-web3).

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

If we used [purescript-web3-generator](https://github.com/f-o-a-m/purescript-web3-generator), we are given a function with the following signature:

```purescript
setTuple :: forall p e.
            IsAsyncProvider p
         => TransactionOptions NoPay 
         -> {_x :: UInt (D2 :& D5 :& D6), _y :: UInt (D2 :& D5 :& D6)} 
         -> Web3 p e HexString 
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
           # _fromBlock ?~ BN 100
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

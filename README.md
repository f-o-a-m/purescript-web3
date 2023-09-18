# purescript-web3
<img src=https://github.com/f-o-a-m/purescript-web3/blob/master/purescript-web3-logo.png width="75">


# A Purescript Client for the Web3 API

`purescript-web3` is a library for interacting with an ethereum node in purescript.

Using [purescript-web3-generator](https://github.com/f-o-a-m/purescript-web3-generator) or [chanterelle](https://github.com/f-o-a-m/chanterelle) it is also possible (and recommended) to generate a library from a set of smart contract abis which is capable of templating transactions and event filters/watchers. The README has instructions for getting started.

To see an example project, it is recommended to look at the [tests repository](https://github.com/f-o-a-m/purescript-web3-tests) (which uses Chanterelle)

## Build Instructions
```
> npm install
> npm run build
> docker run -d -p 8545:8545 -e ACCOUNTS_TO_CREATE=10 foamspace/cliquebait:v1.9.12
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
setTuple :: forall e.
            TransactionOptions NoPay 
         -> {_x :: UIntN 256, _y :: UIntN 256} 
         -> Web3 HexString 
```

It's pretty clear what this function is doing, but let's look at the `TransactionOptions`. This record keeps track of, for example, who is the transaction from, what contract address is it going to, is there ether being sent, etc. In this case, the function is not "payable", so this is indicated in the type of the `TransactionOptions`. It is set using lenses like:

```purescript
setTupleOpts :: TransactionOptions NoPay
setTupleOpts = defaultTransactionOptions
             # _from ?~ myAddress
             # _to ?~ tupleStorageAddress
```
Now for the `TupleSet` event. In order to start an event watcher, we need to establish the `Filter`, which specifies things like the range of blocks we are interested in, and how to find that particular contract and topic. Again, if you're using web3-generator, things are a lot simpler:

```purescript
tupleFilter :: Filter TupleSet
tupleFilter = eventFilter (Proxy :: Proxy TupleSet) tupleStorageAddress 
           # _fromBlock .~ BN 100
```

We also need to pass a callback to the event watcher that performs some action and decides whether or not to unregister the filter. For example, we could set up an event monitor starting from block 100 and continuing until the two coordinates that are set are equal:

```purescript
event tupleFilter $ \(TupleSet {newX,newY} -> do
  log $ "Received New Tuple : " <> show (Tuple newX newY) 
  if newX == newY
    then pure TerminateEvent
    else do
      _ <- performAction newX newY
      pure ContinueEvent
```

## Resources
 
 - [web3 api spec](https://github.com/ethereum/execution-apis)
 - [solidity ABI spec](https://docs.soliditylang.org/en/latest/abi-spec.html)

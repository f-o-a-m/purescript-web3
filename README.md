# purescript-web3
<img src=https://github.com/f-o-a-m/purescript-web3/blob/master/purescript-web3-logo.svg width="75">


[![Latest release](http://img.shields.io/github/release/f-o-a-m/purescript-web3.svg?branch=master)](https://github.com/f-o-a-m/purescript-web3/releases)
[![Build status](https://travis-ci.org/f-o-a-m/purescript-web3.svg?branch=master)](https://travis-ci.org/f-o-a-m/purescript-web3?branch=master)

# Purescript bindings for Web3

`purescript-web3` is a faithful porting of [hs-web3](https://github.com/airalab/hs-web3) to purescript. At the moment it implements most of the `eth` endpoints of the web3 api, which means it is suitable for sendind transactions, querying blockchain state and metatdata, and monitoring events.

Using [purescript-web3-generator](https://github.com/f-o-a-m/purescript-web3-generator) it is also possible to generate a library from a set of smart contract abis which is capable of templating transactions and event filters/watchers. The README has instructions for getting started.

We do not yet have a build tool similar to truffle, but if you are looking for a template of how to use truffle and write your tests using purescript, check out out the [purescript-web3-tests](https://github.com/f-o-a-m/purescript-web3-tests)

To see an example project using all of the purescript-web3 tools and with thermite/react ui, check out [purescript-web3-example](https://github.com/f-o-a-m/purescript-web3-example).

## Prerequisites
`purescript-web3` is using [psc-package](https://github.com/purescript/psc-package) to manage dependencies. Follow the instructions to install psc-package before building the project.


## Build Instructions
```
> npm install
> psc-package build
> pulp test
```

## Documentation

Module documentation is [published on Pursuit](http://pursuit.purescript.org/packages/purescript-web3).

## Resources
 
 - [web3.js repo](https://github.com/ethereum/web3.js)
 - [web3 Javascript API wiki](https://github.com/ethereum/wiki/wiki/JavaScript-API)

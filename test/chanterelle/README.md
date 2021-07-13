# Purescript-Web3-Tests

This directory contains a [chanterelle](https://github.com/f-o-a-m/chanterelle) project used for generating the FFI and bytecode needed for the actual test suite, rooted in `../web3`. To avoid circular dependencies, we cannot actually use the chanterelle package in the test suite itself, only to compile and generate code -- we are currently checking in generated FFI. You can regenerate it or overwrite it by running 

```bash
> chanterelle build
```

in this directory. 

The downside of avoiding circular dependencies in this is setup that we have to handle deployments in the test suite itself. Currently this means that we have to manually copy paste deployment bytecode from the `solc` output into source files for these deployments. A consequence of this is that **if you change the contract code, you will need to manually re copy/paste the deployment bytecode into the appropriate file**. Hopefully we can fix this soon.
"use strict";

var Web3 = require('web3');

exports._getBalance = function(web3) {
  return web3.eth.getBalance;
};

exports.newWeb3Backend = function (provider) {
    return new Web3(new Web3.providers.HttpProvider(provider));
};

exports.getWeb3Backend = function () {
    if (typeof web3 !== 'undefined') {
        // Use Mist/MetaMask's provider
        global.web3 = new Web3(web3.currentProvider);
    } else {
        console.log(global);
        console.log('No web3? You should consider trying MetaMask!');
        // fallback - use your fallback strategy (local node / hosted node + in-dapp id mgmt / fail)
        global.web3 = new Web3(new Web3.providers.HttpProvider("http://localhost:8545"));
    }
    return global.web3;

};

exports._isConnected = function (web3) {
    return web3.isConnected;
};

exports.web3ShowImpl = function (web3) {
    var host = web3.currentProvider.host;
    return "Connected to rpc node at: " + host;
};

exports._getBlock = function (web3) {
    return web3.eth.getBlock;
};

exports._getTransaction = function (web3) {
    return web3.eth.getTransaction;
};

exports._contract = function (web3) {
    return function (abi) {
        return web3.eth.contract(abi);
    };
};

exports._getContractInstance = function (contract) {
    return function (address) {
        return contract.at(address);
    };
};

exports._callMethod = function (contract) {
    return function (methodName) {
        return function (args) {
            return contract[methodName].apply(this, args);
        };
    };
};

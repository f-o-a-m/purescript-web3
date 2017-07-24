"use strict";

var Web3 = require('web3');

exports._getBalance = function(web3) {
  return web3.eth.getBalance;
};

exports.newWeb3Backend = function (provider) {
    return new Web3(new Web3.providers.HttpProvider(provider));
};

exports.getWeb3Backend = function () {
    return web3;
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
        return contract[methodName].call;
    };
};

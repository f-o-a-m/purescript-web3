"use strict";

var Web3 = require('web3');

exports._getBalance = function(web3) {
  return web3.eth.getBalance;
};

exports.newWeb3 = function (provider) {
    return new Web3(new Web3.providers.HttpProvider(provider));
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

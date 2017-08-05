"use strict";

var Web3 = require('web3');

//var getProvider = function () {
//    var provider;
//    if (typeof web3 !== 'undefined') {
//        // Use Mist/MetaMask's provider
//        provider = web3.currentProvider;
//    } else {
//        // fallback - use your fallback strategy (local node / hosted node + in-dapp id mgmt / fail)
//        provider = new Web3.providers.HttpProvider("http://localhost:8545");
//    }
//    console.log(provider);
//    return provider;
//
//};

var getProvider = function () {
    var provider;
    if (typeof web3 !== 'undefined') {
        // Use Mist/MetaMask's provider
        provider = web3.currentProvider;
    } else {
        // fallback - use your fallback strategy (local node / hosted node + in-dapp id mgmt / fail)
        provider = new Web3.providers.HttpProvider("http://localhost:8545");
    }
    return provider;
};

exports.getProvider = getProvider;

exports.showProviderImpl = function (provider) {
    return provider.host;
};

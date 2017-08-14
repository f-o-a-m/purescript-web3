"use strict";
var Web3 = require('web3');

var getProvider = function () {
    var provider;
    if (typeof web3 !== 'undefined') {
        // Use Mist/MetaMask's provider
        console.log("using metamask provider");
        provider = web3.currentProvider;
    } else {
        console.log("using http provider");
        provider = new Web3.providers.HttpProvider("http://localhost:8545");
    }
    return provider;
};

exports.getProvider = getProvider;

exports.showProviderImpl = function (provider) {
    return provider.host;
};

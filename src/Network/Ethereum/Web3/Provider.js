"use strict";

var Web3 = require('web3');

var getProvider = function () {
    var provider;
    if (typeof web3 !== 'undefined') {
        // Use Mist/MetaMask's provider
        provider = web3.currentProvider;
    } else {
        console.log(global);
        console.log('No web3? You should consider trying MetaMask!');
        // fallback - use your fallback strategy (local node / hosted node + in-dapp id mgmt / fail)
        provider = new Web3.providers.HttpProvider("http://localhost:8545");
    }
    return provider;

};

exports.getProvider = getProvider;

"use strict";

var Web3 = require('web3');

// load it up the first time.
window.addEventListener('load', function() {

    // Checking if Web3 has been injected by the browser (Mist/MetaMask)
    if (typeof web3 !== 'undefined') {
        // Use Mist/MetaMask's provider
        window.web3 = new Web3(web3.currentProvider);
    } else {
        console.log('No web3? You should consider trying MetaMask!');
        // fallback - use your fallback strategy (local node / hosted node + in-dapp id mgmt / fail)
        window.web3 = new Web3(new Web3.providers.HttpProvider("http://localhost:8545"));
    }

    // Now you can start your app & access web3 freely:
});

var getProvider = function () {
    var provider;
    if (typeof web3 !== 'undefined') {
        // Use Mist/MetaMask's provider
        provider = web3.currentProvider;
    } else {
        provider = new Web3.providers.HttpProvider("http://localhost:8545");
    }
    return provider;
};

exports.getProvider = getProvider;

exports.showProviderImpl = function (provider) {
    return provider.host;
};

"use strict";
var Web3 = require('web3');

exports.metamaskProvider = function () {
    if (typeof web3 !== 'undefined') {
        return web3.currentProvider;
    } else {
        return new Error("No Metamask provider found.");
    }
};

exports.httpProvider = function (providerUrl) {
    return function () {
        if (typeof web3 !== 'undefined' && web3.currentProvider.host == providerUrl) {
           return web3.currentProvider;
        } else {
           return new Web3.providers.HttpProvider(providerUrl);
        }
    };
};

"use strict";

var Web3 = require('web3');

exports._getBalance = function(web3) {
    return web3.eth.getBalance;
};

exports.newWeb3 = function (provider) {
    return new Web3(provider);
};

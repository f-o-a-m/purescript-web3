"use strict";

var CryptoJS = require('crypto-js');
var sha3 = require('crypto-js/sha3');

exports.sha3StringImpl = function (value) {
    return sha3(value, {
        outputLength: 256
    }).toString();
};

exports.sha3HexImpl = function (value) {
    const wordArray = CryptoJS.enc.Hex.parse(value);
    return sha3(wordArray, {
        outputLength: 256
    }).toString();
};

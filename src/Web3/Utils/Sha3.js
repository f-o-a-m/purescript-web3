"use strict";

var CryptoJS = require('crypto-js');
var sha3 = require('crypto-js/sha3');

exports._sha3 = function (value) {
    return sha3(value, {
        outputLength: 256
    }).toString();
};

exports._parseHexString = function(value) { 
  return CryptoJS.enc.Hex.parse(value);
};


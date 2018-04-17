"use strict";

var BigNumber = require('ethereumjs-util').BN;

exports.fromHexString = function(str) {
    return new BigNumber(str, 16);
};

var signedIsNegative = function (value) {
    var head = new BigNumber(value.substr(0, 1), 16).toString(2);
    var msb;
    if (head.length == 4) {
        msb = head.substr(0,1);
    } else {
        msb = '0';
    }
    return msb === '1';
};

exports.fromHexStringSigned = function (value) {
    if (signedIsNegative(value)) {
        return new BigNumber(value, 16).sub(new BigNumber('ffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffff', 16)).subn(1);
    }
    return new BigNumber(value, 16);
};

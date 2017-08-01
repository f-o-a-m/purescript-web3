"use strict";

var BigNumber = require('bignumber.js');

BigNumber.config({ ROUNDING_MODE: BigNumber.ROUND_DOWN});

exports.fromHexString = function(str) {
    return new BigNumber(str, 16);
};

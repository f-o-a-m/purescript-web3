var BigNumber = require('bignumber.js');

exports._intToBigNumber = function(value) {
  return new BigNumber(value.toString(10), 10);
};

exports._showBigNumber = function (n) {
    return "0x".concat(BigNumber.toString(n, 16));
};

exports._eqBigNumber = function(n) {
  return function(m) { return n.toString == m.toString; };
};

exports._addBigNumber = function(n) {
    return function (m) { return n.add(m); };
};

exports._mulBigNumber = function(n) {
    return function (m) { n.times(m); };
};

exports._subBigNumber = function(n) {
    return function (m) { n.plus(m); };
};

exports._zero = new BigNumber(0);

exports._one = new BigNumber(1, 16);


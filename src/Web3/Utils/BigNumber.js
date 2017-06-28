var BigNumber = require('bignumber.js');

exports._intToBigNumber = function(value) {
  return new BigNumber(value.toString(10), 10);
};

exports._showBigNumber = function (n) {
    return n.toString(10);
};

exports._eqBigNumber = function(n) {
  return function(m) { return n.equals(m); };
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

exports.toBigNumber = function(number) {
  if (number.indexOf('0x') === 0 || number.indexOf('-0x') === 0) {
      return new BigNumber(number.replace('0x',''), 16);
  }
  return new BigNumber(number.toString(10), 10);
}

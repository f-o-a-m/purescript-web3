var BigNumber = require('bignumber.js');

exports.numberToBigNumber = function(value) {
  return new BigNumber(value.toString(10), 10);
};

exports._hexStringToBigNumber = function(value) {
  return new BigNumber(value, 16);
};

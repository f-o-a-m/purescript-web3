"use strict";
/**
 * @file bloom.js
 * @author Bas van Kervel <bas@ethereum.org>
 * @date 2017
 */

/**
 * Ethereum bloom filter support.
 *
 * @module bloom
 * @class [bloom] bloom
 */

function codePointToInt(codePoint) {
    if (codePoint >= 48 && codePoint <= 57) { /*['0'..'9'] -> [0..9]*/
        return codePoint-48;
    }

    if (codePoint >= 97 && codePoint <= 102) { /*['a'..'f'] -> [10..15]*/
        return codePoint-87;
    }

    throw "invalid bloom";
}

exports.testBytes = function (bloom) {
  return function (bytes) {

    for (var i=0; i < 12; i+=4) {
        // calculate bit position in bloom fiter that must be active
        var bitpos = ((parseInt(hash.substr(i, 2), 16) << 8) + parseInt(hash.substr((i+2), 2), 16)) & 2047;

        // test if bitpos in bloom is active
        var code = codePointToInt(bloom.charCodeAt(bloom.length-1-Math.floor(bitpos/4)));
        var offset = 1 << (bitpos % 4);

        if ((code&offset) !== offset) {
            return false;
        }
    }

    return true;
  };
};


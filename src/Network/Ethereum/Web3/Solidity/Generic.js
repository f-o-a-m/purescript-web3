"use strict";

exports.unsafeMerge = function (l) {
    return function (r) {
        var o = {};
        return Object.assign(o, l, r);
    };
};

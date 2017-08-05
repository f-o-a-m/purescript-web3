"use strict";

//exports._sendAsync = function (provider) {
//    return function ()
//
//    provider.sendAsync(args)
//};

exports._sendAsync = function (callback) {
    return function (provider) { // accepts a callback
        return function(request) { // and a request
            return function() { // returns an effect
                provider.sendAsync(request, function(response) {
                    callback(response)(); // callback itself returns an effect
                });
            };
        };
    };
};

exports.web3Request = 2;

"use strict";

//exports._sendAsync = function (provider) {
//    return function ()
//
//    provider.sendAsync(args)
//};

exports._sendAsync = function (callback) {
    return function (provider) {
        return function(request) {
            return function() {
                provider.sendAsync(request, function(err, response) {
                    callback(response.result)();
                });
            };
        };
    };
};

exports._send = function (provider) {
    return function (request) {
        return function () {
            var res = provider.send(request);
            return res.result;
        };
    };
};

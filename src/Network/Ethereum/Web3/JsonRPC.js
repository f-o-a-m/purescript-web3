"use strict";


exports._sendAsync = function (provider) {
    return function (request) {
        return function(onError, onSuccess) {
            var uncurriedSendAsync = function(req) {
                return function(cb) {
                    provider.sendAsync(req, cb);
                };
            };
            var cancel = uncurriedSendAsync(request);
            return function (cancelError, onCancelerError, onCancelerSuccess) {
                cancel();
                onCancelerSuccess();
            };
        };
    };
};

exports._send = function (provider) {
    return function (request) {
        return function () {
            return provider.send(request);
        };
    };
};

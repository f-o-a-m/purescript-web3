"use strict";


exports._sendAsync = function (provider) {
    return function (request) {
        return function(onError, onSuccess) {
            var cancel = provider.sendAsync(request, function(err, succ) {
                if (err) {
                    onError(err);
                } else {
                    onSuccess(succ);
                }
            });
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

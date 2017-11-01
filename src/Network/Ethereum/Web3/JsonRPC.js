"use strict";


exports._sendAsync = function (provider) {
    return function (request) {
        return function(onError, onSuccess) {
            console.log(request);
            var req = provider.sendAsync(request, function(err, succ) {
                if (err) {
                    console.log("gotError: ", err);
                    onError(err);
                } else {
                    console.log("gotSuccess: ", succ);
                    onSuccess(succ);
                }
            });
            return function (cancelError, onCancelerError, onCancelerSuccess) {
                req.cancel();
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

"use strict";


exports._sendAsync = function (provider) {
    return function (request) {
        return function(onError, onSuccess) {
            console.log(request);
            var cancel = provider.sendAsync(request, function(err, succ) {
                console.log("error", err);
                console.log("result", succ);
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

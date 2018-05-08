"use strict";


exports._sendAsync = function (provider) {
    return function (request) {
        return function(onError, onSuccess) {
            var canceled = false;
            provider.sendAsync(request, function(err, succ) {
                if (canceled) {
                    return;
                } else if (err) {
                    onError(err);
                } else {
                    onSuccess(succ);
                }
            });
            return function (cancelError, onCancelerError, onCancelerSuccess) {
                canceled = true;
                onCancelerSuccess();
            };
        };
    };
};

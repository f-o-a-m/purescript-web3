"use strict";

export const _sendAsync = function (provider) {
    return function (request) {
        return function(onError, onSuccess) {
            provider.sendAsync(request, function(err, succ) {
                if (err) {
                    onError(err);
                } else {
                    onSuccess(succ);
                }
            });
            return function (cancelError, onCancelerError, onCancelerSuccess) {
                onCancelerSuccess();
            };
        };
    };
};

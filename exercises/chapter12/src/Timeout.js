"use strict";

exports.setTimeoutImpl = function(callback, miliseconds) {
    return function() {
        setTimeout(callback, miliseconds);
    };
};

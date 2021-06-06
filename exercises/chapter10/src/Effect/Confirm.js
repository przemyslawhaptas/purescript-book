"use strict";

exports.confirm = function(msg) {
    return function() {
        window.confirm(msg);
    };
};

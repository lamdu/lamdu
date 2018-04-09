/* jshint node: true */
"use strict";

var protocol = require("./protocol.js");

module.exports = {
    builtinTagName: function (x) {
        var raw = ("BI:" + x).substring(0, 16);
        raw += "\0".repeat(16 - raw.length);
        var encoded = "";
        if (raw.charCodeAt(0) < 10 * 16) {
            // Hex is not alphanumeric
            encoded = "_";
        }
        for (var i = 0; i < raw.length; i++) {
            var hexChar = raw.charCodeAt(i).toString(16);
            encoded += "0".repeat(2 - hexChar.length) + hexChar;
        }
        return encoded;
    },
    logRepl: function (res) {
        protocol.sendCompletionSuccess(res);
    },
    logReplErr: function (err) {
        protocol.sendCompletionError(err);
    },
    logResult: function (scope, exprId, result) {
        protocol.sendResult(scope, exprId, result);
        return result;
    },
    logNewScope: function (parentScope, scope, lamId, arg) {
        protocol.sendNewScope(parentScope, scope, lamId, arg);
    },
};

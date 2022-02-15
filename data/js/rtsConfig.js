/* jshint node: true */
"use strict";

var protocol = require("protocol.js");

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
    logReplErr: protocol.sendCompletionError,
    logResult: function (scope, exprId, result) {
        protocol.sendResult(scope, exprId, result);
        return result;
    },
    logNewScope: protocol.sendNewScope,
    error: protocol.error
};

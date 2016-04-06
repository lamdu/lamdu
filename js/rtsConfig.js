"use strict";

module.exports = {
    builtinTagName: function (x) {
        var raw = "BI:" + x;
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
    logRepl: function () {},
}

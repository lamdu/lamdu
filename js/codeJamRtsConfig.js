"use strict";

var nameMap = {
    data: '__data'
}

module.exports = {
    builtinTagName: function (x) {
        if (nameMap.hasOwnProperty(x))
            return nameMap[x];
        return x;
    },
    logRepl: function (x) { console.log(String.fromCharCode.apply(null, x)); },
}

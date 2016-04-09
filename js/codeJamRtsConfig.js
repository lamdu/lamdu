"use strict";

var nameMap = {
    true: 'True',
    false: 'False',
    cons: 'NonEmpty',
}

module.exports = {
    builtinTagName: function (x) {
        if (nameMap.hasOwnProperty(x))
            return nameMap[x];
        return x;
    },
    logRepl: function (x) { console.log(String.fromCharCode.apply(null, x)); },
}

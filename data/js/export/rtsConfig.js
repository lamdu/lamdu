/* jshint node: true */
"use strict";

var nameMap = {
    data: '__data',
    val: 'value',
    srcPath: 'sourcePath',
    dstPath: 'destinationPath',
    subtrees: 'subTrees',
    infixl: 'leftHandSide',
    infixr: 'rightHandSide'
};

module.exports = {
    builtinTagName: function (x) {
        if (nameMap.hasOwnProperty(x))
            return nameMap[x];
        return x;
    },
    logRepl: function (x) {
        console.log(
            x instanceof Uint8Array
            ? String.fromCharCode.apply(null, x)
            : x);
    },
    logReplErr: function (x) { console.log("Error: " + String.fromCharCode.apply(null, x)); },
    // Not used in JS export:
    logResult: undefined,
    logNewScope: undefined,
    error: function (name, desc, globalId, exprId) {
        throw name;
    }
};

"use strict";

// Tag names must match those in Lamdu.Builtins.Anchors
var biTagName = function (x) {
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
}
var trueTag = biTagName('true');
var falseTag = biTagName('false');
var consTag = biTagName('cons');
var headTag = biTagName('head');
var tailTag = biTagName('tail');
var objTag = biTagName('object');
var infixlTag = biTagName('infixl');
var infixrTag = biTagName('infixr');
var indexTag = biTagName('index');
var startTag = biTagName('start');
var stopTag = biTagName('stop');
var valTag = biTagName('val');

var bool = function (x) {
    return {tag: x ? trueTag : falseTag, data: {}};
}

// Assumes "a" and "b" are of same type, and it is an object created by
// Lamdu's JS backend - this need not work for all JS objects..
var isEqual = function (a, b) {
    if (a == b)
        return true;
    if (typeof a != 'object')
        return a == b;
    if (a.length != b.length)
        return false;
    for (var p in a)
        if (!isEqual(a[p], b[p]))
            return false;
    return true;
}

var bytes = function (list) {
    var arr = new Uint8Array(list.length);
    arr.set(list);
    return arr;
}

var arrayFromStream = function (stream) {
    var items = [];
    while (stream['tag'] == consTag)
    {
        items.push(stream['data'][headTag]);
        stream = stream['data'][tailTag]();
    }
    return items;
}

var encode = function (x) {
    var replacer = function(key, value) {
        if (value == null) { // or undefined due to auto-coercion
            return {};
        }
        if (Uint8Array.prototype.isPrototypeOf(value)) {
            return { tag: "bytes", data: Array.from(value) };
        }
        if (Function.prototype.isPrototypeOf(value)) {
            return { tag: "function" };
        }
        return value;
    };
    return JSON.stringify(x, replacer);
};

var STArray = function(arr) {
    this.arr = arr;
    return this;
};

module.exports = {
    logResult: function (scope, exprId, result) {
        console.log(encode(
            { event:"Result"
              , scope:scope
              , exprId:exprId
              , result:result
            }));
        return result;
    },
    logNewScope: function (parentScope, scope, lamId, arg) {
        console.log(encode(
            { event:"NewScope"
              , parentScope:parentScope
              , scope:scope
              , lamId:lamId
              , arg:arg
            }));
    },
    wrap: function (fast, slow) {
        var count = 0;
        var callee = function() {
            count += 1;
            if (count > 10) {
                callee = fast;
                return fast.apply(this, arguments);
            }
            return slow.apply(this, arguments);
        };
        return function () {
            return callee.apply(this, arguments);
        }
    },
    bytes: bytes,
    builtins: {
        Prelude: {
            sqrt: Math.sqrt,
            "+": function (x) { return x[infixlTag] + x[infixrTag]; },
            "-": function (x) { return x[infixlTag] - x[infixrTag]; },
            "*": function (x) { return x[infixlTag] * x[infixrTag]; },
            "/": function (x) { return x[infixlTag] / x[infixrTag]; },
            div: function (x) { return Math.floor(x[infixlTag] / x[infixrTag]); },
            mod: function (x) { return x[infixlTag] % x[infixrTag]; },
            negate: function (x) { return -x; },
            "==": function (x) { return bool(isEqual(x[infixlTag], x[infixrTag])); },
            "/=": function (x) { return bool(!isEqual(x[infixlTag], x[infixrTag])); },
            ">=": function (x) { return bool(x[infixlTag] >= x[infixrTag]); },
            ">": function (x) { return bool(x[infixlTag] > x[infixrTag]); },
            "<=": function (x) { return bool(x[infixlTag] <= x[infixrTag]); },
            "<": function (x) { return bool(x[infixlTag] < x[infixrTag]); },
        },
        Bytes: {
            length: function (x) { return x.length; },
            byteAt: function (x) { return x[objTag][x[indexTag]]; },
            slice: function (x) { return x[objTag].subarray(x[startTag], x[stopTag]); },
            unshare: function (x) { return x.slice(); },
            fromStream: function (x) { return bytes(arrayFromStream(x)); },
        },
        Array: {
            length: function (x) { return x.length; },
            item: function (x) { return x[objTag][x[indexTag]]; },
            fromStream: arrayFromStream,
        },
        ST: {
            return: function(x) { return function() { return x; }; },
            bind: function(x) { return function () { return x[infixrTag](x[infixlTag]())(); } },
            run: function(st) { return st(); },
            Array: {
                length: function (x) { return x.length; },
                read: function (x) { return function() { return x[objTag][x[indexTag]]; } },
                write: function (x) { return function() { x[objTag][x[indexTag]] = x[valTag]; return {}; } },
                fromStream: function (x) { return function () { return arrayFromStream(x); } },
            },
        }
    },
};

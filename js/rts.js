"use strict";

process.stdout._handle.setBlocking(true);

var conf = require('./rtsConfig.js')

// Tag names must match those in Lamdu.Builtins.Anchors
var trueTag = conf.builtinTagName('true');
var falseTag = conf.builtinTagName('false');
var consTag = conf.builtinTagName('cons');
var headTag = conf.builtinTagName('head');
var tailTag = conf.builtinTagName('tail');
var objTag = conf.builtinTagName('object');
var infixlTag = conf.builtinTagName('infixl');
var infixrTag = conf.builtinTagName('infixr');
var indexTag = conf.builtinTagName('index');
var startTag = conf.builtinTagName('start');
var stopTag = conf.builtinTagName('stop');
var valTag = conf.builtinTagName('val');
var oldPathTag = conf.builtinTagName('oldPath');
var newPathTag = conf.builtinTagName('newPath');
var filePathTag = conf.builtinTagName('filePath');
var fileDescTag = conf.builtinTagName('fileDesc');
var dataTag = conf.builtinTagName('data');
var modeTag = conf.builtinTagName('mode');
var sizeTag = conf.builtinTagName('size');
var srcPathTag = conf.builtinTagName('srcPath');
var dstPathTag = conf.builtinTagName('dstPath');
var flagsTag = conf.builtinTagName('flags');

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
        if (p != 'cacheId' && !isEqual(a[p], b[p]))
            return false;
    return true;
}

var bytes = function (list) {
    return new Uint8Array(list);;
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

var encode = function() {
    var cacheId = 0;
    return function (x) {
        var replacer = function(key, value) {
            if (value == null) { // or undefined due to auto-coercion
                return {};
            }
            if (Function.prototype.isPrototypeOf(value)) {
                return { func: {} };
            }
            if (typeof value == "number" && !isFinite(value)) {
                return { "number": String(value) }
            }
            if (typeof value != "object" || key === "array" || key === "bytes") {
                return value;
            }
            if (value.hasOwnProperty("cacheId")) {
                return { cachedVal: value.cacheId };
            }
            value.cacheId = cacheId++;
            if (Array.prototype.isPrototypeOf(value)) {
                return {
                    array: value,
                    cacheId: value.cacheId,
                };
            }
            if (Uint8Array.prototype.isPrototypeOf(value)) {
                return {
                    bytes: Array.from(value),
                    cacheId: value.cacheId,
                };
            }
            return value;
        };
        return JSON.stringify(x, replacer);
    };
}();

module.exports = {
    logRepl: conf.logRepl,
    logResult: function (scope, exprId, result) {
        process.stdout.write(encode(
            { event:"Result"
              , scope:scope
              , exprId:exprId
              , result:result
            }));
        process.stdout.write("\n");
        return result;
    },
    logNewScope: function (parentScope, scope, lamId, arg) {
        process.stdout.write(encode(
            { event:"NewScope"
              , parentScope:parentScope
              , scope:scope
              , lamId:lamId
              , arg:arg
            }));
        process.stdout.write("\n");
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
    bytesFromAscii: function (str) {
        var arr = new Uint8Array(str.length);
        for (var i = 0; i < str.length; ++i) {
            arr[i] = str.charCodeAt(i);
        }
        return arr;
    },
    builtins: {
        Prelude: {
            sqrt: Math.sqrt,
            "+": function (x) { return x[infixlTag] + x[infixrTag]; },
            "-": function (x) { return x[infixlTag] - x[infixrTag]; },
            "*": function (x) { return x[infixlTag] * x[infixrTag]; },
            "/": function (x) { return x[infixlTag] / x[infixrTag]; },
            "^": function (x) { return Math.pow(x[infixlTag], x[infixrTag]); },
            div: function (x) { return Math.floor(x[infixlTag] / x[infixrTag]); },
            mod: function (x) {
                var modulus = x[infixrTag];
                var r = x[infixlTag] % modulus;
                if (r < 0)
                    r += modulus;
                return r;
            },
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
        Mut: {
            return: function(x) { return function() { return x; }; },
            bind: function(x) { return function () { return x[infixrTag](x[infixlTag]())(); } },
            run: function(st) { return st(); },
            Array: {
                length: function (x) { return function() { return x.length; } },
                read: function (x) { return function() { return x[objTag][x[indexTag]]; } },
                write: function (x) { return function() { x[objTag][x[indexTag]] = x[valTag]; return {}; } },
                append: function (x) { return function() { x[objTag].push(x[valTag]); return {}; } },
                pop: function (x) { return function() { return x.pop(); } },
                fromStream: function (x) { return function () { return arrayFromStream(x); } },
                run: function(st) {
                    var result = st();
                    if (result.hasOwnProperty("cacheId")) {
                        delete result.cacheId;
                    }
                    return result;
                },
            },
            Ref: {
                new: function (x) { return function() {return {val: x}; } },
                read: function (x) { return function() {return x.val;} },
                write: function (x) { return function() {x[objTag].val = x[valTag]; return {};} },
            },
        },
        IO: {
            file: {
                unlink: function(path) {
                    return function() { require('fs').unlinkSync(path); };
                },
                rename: function(x) {
                    return function() { require('fs').renameSync(x[oldPathTag], x[newPathTag]); };
                },
                chmod: function(x) {
                    return function() { require('fs').chmodSync(x[filePathTag], x[modeTag]); };
                },
                close: function(fd) {
                    return function() { require('fs').closeSync(fd); };
                },
                fchmod: function(x) {
                    return function() { require('fs').fchmodSync(x[fileDescTag], x[modeTag]); };
                },
                fstat: function(fd) {
                    return function() { return require('fs').fstatSync(fd); };
                    // { dev: 66306,
                    //   mode: 33188,
                    //   nlink: 1,
                    //   uid: 0,
                    //   gid: 0,
                    //   rdev: 0,
                    //   blksize: 4096,
                    //   ino: 7733458,
                    //   size: 2243,
                    //   blocks: 8,
                    //   atime: 2016-06-02T11:35:58.456Z,
                    //   mtime: 2016-05-02T22:06:47.124Z,
                    //   ctime: 2016-05-02T22:06:47.132Z,
                    // }
                },
                fsync: function(fd) {
                    return function() { require('fs').fsyncSync(fd); };
                },
                ftruncate: function(x) {
                    return function() { require('fs').ftruncateSync(x[fileDescTag], x[sizeTag]); };
                },
                link: function(x) {
                    return function() { require('fs').linkSync(x[srcPathTag], x[dstPathTag]); };
                },
                lstat: function(path) {
                    // see fstat for result example
                    return function() { return require('fs').lstatSync(path); };
                },
                mkdir: function(path) {
                    return function() { require('fs').mkdirSync(path); };
                },
                open: function(x) {
                    return function() { return require('fs').openSync(x[filePathTag], x[flagsTag], x[modeTag]); };
                },
                readFile: function(path) {
                    return function() { return bytes(require('fs').readFileSync(path)); };
                },
                readLink: function(path) {
                    return function() { return bytes(require('fs').readlinkSync(path, 'buffer')); };
                },
                appendFile: function(x) {
                    return function() { require('fs').appendFileSync(x[filePathTag], x[dataTag]); };
                },
                writeFile: function(x) {
                    return function() { require('fs').writeFileSync(x[filePathTag], x[dataTag]); };
                },
            }
        }
    },
};

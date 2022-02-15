/* jshint node: true */
/* jshint esversion: 6 */
"use strict";

var anchors = require("anchors.js");
var tags = anchors.tags;

var bool = function (x) {
    return {tag: x ? tags.true : tags.false, data: {}};
};

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
};

var bytes = function (list) {
    return new Uint8Array(list);
};

var bytesFromAscii = function (str) {
    var arr = new Uint8Array(str.length);
    for (var i = 0; i < str.length; ++i) {
        arr[i] = str.charCodeAt(i);
    }
    return arr;
};

var toString = function (bytes) {
    // This is a nodejs only method to convert a UInt8Array to a string.
    // For the browser we'll need to augment this.
    return Buffer(bytes).toString();
};

var makeOpaque = function (obj) {
    obj.cacheId = -1;
};

var mutFunc = function (inner) {
    return function(x) {
        return function(cont) {
            return cont(inner(x));
        };
    };
};

// Create a parameterized Mut action which returns nothing (void),
// from a call with a nodejs callback which may get an error.
var mutVoidWithError = function (inner) {
    return function(x) {
        return function(cont) {
            rerun(inner(x))(err => {
                if (err) throw err;
                cont({});
            });
        };
    };
};

var bytesConcat = function (x) {
    var i, pos = 0;
    for (i = 0; i < x.length; i++) {
        pos += x[i].length;
    }
    var result = new Uint8Array (pos);
    pos = 0;
    for (i = 0; i < x.length; i++) {
        result.set(x[i], pos);
        pos += x[i].length;
    }
    return result;
};

var conf = require('rtsConfig.js');

var curried_error = function(name) {
    return function(globalId, exprId) {
        return conf.error(name, globalId, exprId);
    };
};

var rerun = function (result) {
    while(1) {
        var trampoline = result.trampolineTo;
        if (typeof trampoline === 'undefined') {
            return result;
        }
        result = trampoline();
    }
};

class UnicodeError extends Error {}

module.exports = {
    logReplErr: conf.logReplErr,
    logResult: conf.logResult,
    logNewScope: conf.logNewScope,
    exceptions: {
        DependencyTypeOutOfDate: curried_error("DependencyTypeOutOfDate"),
        ReachedHole: curried_error("ReachedHole"),
        UnhandledCase: curried_error("UnhandledCase"),
    },
    rerun: rerun,
    memo: function (thunk) {
        var done = false;
        var memo;
        return function() {
            if(done) return memo;
            memo = module.exports.rerun(thunk());
            done = true;
            return memo;
        };
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
        };
    },
    bytes: bytes,
    bytesFromAscii: bytesFromAscii,
    builtins: {
        Prelude: {
            sqrt: Math.sqrt,
            "+": function (x) { return x[tags.infixl] + x[tags.infixr]; },
            "-": function (x) { return x[tags.infixl] - x[tags.infixr]; },
            "*": function (x) { return x[tags.infixl] * x[tags.infixr]; },
            "/": function (x) { return x[tags.infixl] / x[tags.infixr]; },
            "^": function (x) { return Math.pow(x[tags.infixl], x[tags.infixr]); },
            div: function (x) { return Math.floor(x[tags.infixl] / x[tags.infixr]); },
            mod: function (x) {
                var modulus = x[tags.infixr];
                var r = x[tags.infixl] % modulus;
                if (r < 0)
                    r += modulus;
                return r;
            },
            negate: function (x) { return -x; },
            "==": function (x) { return bool(isEqual(x[tags.infixl], x[tags.infixr])); },
            "/=": function (x) { return bool(!isEqual(x[tags.infixl], x[tags.infixr])); },
            ">=": function (x) { return bool(x[tags.infixl] >= x[tags.infixr]); },
            ">": function (x) { return bool(x[tags.infixl] > x[tags.infixr]); },
            "<=": function (x) { return bool(x[tags.infixl] <= x[tags.infixr]); },
            "<": function (x) { return bool(x[tags.infixl] < x[tags.infixr]); },
        },
        Bytes: {
            length: function (x) { return x.length; },
            byteAt: function (x) { return x[tags.obj][x[tags.index]]; },
            slice: function (x) { return x[tags.obj].subarray(x[tags.start], x[tags.stop]); },
            unshare: function (x) { return x.slice(); },
            fromArray: function (x) { return bytes(x); },
        },
        Optimized: {
            renderHtml: function (tree) {
                var bufs = [];
                var go = function (node) {
                    var root = node[tags.root];
                    var subtrees = node[tags.subtrees];
                    bufs.push(root);
                    for (var i = 0; i < subtrees.length; i++) {
                        go(subtrees[i]);
                    }
                    if (root.length > 2 && root[0] == 60 && root[root.length] != 47) {
                        bufs.push(new Uint8Array([60, 47]));
                        var k;
                        for (k = 1; k < root.length; k++) {
                            var c = root[k];
                            if (c == 32 || c == 62)
                                break;
                        }
                        bufs.push(root.slice(1, k));
                        bufs.push(new Uint8Array([62]));
                    }
                };
                go(tree);
                return bytesConcat(bufs);
            },
        },
        Char: {
            codePoint: function (x) { return x; },
            fromCodePoint: function (x) {
                if(!Number.isInteger(x) || x < 0 || x > 0xFFFFFFFF) {
                    throw new UnicodeError("fromCodePoint on a non 32-bit unsigned integer value");
                }
                return x;
            }
        },
        Array: {
            length: function (x) { return x.length; },
            item: function (x) { return x[tags.obj][x[tags.index]]; },
        },
        Mut: {
            return: mutFunc(x => x),
            bind: function(x) {
                return function (cont) {
                    return x[tags.infixl](res => rerun(x[tags.infixr](res))(cont));
                };
            },
            run: function(st) { return st(x => x); },
            Array: {
                length: mutFunc(x => x.length),
                read: mutFunc(x => x[tags.obj][x[tags.index]]),
                write: mutFunc(x => { x[tags.obj][x[tags.index]] = x[tags.val]; return {}; } ),
                append: mutFunc(x => { x[tags.obj].push(x[tags.val]); return {}; } ),
                truncate: mutFunc(x => {
                    var arr = x[tags.obj];
                    arr.length = Math.min(arr.length, x[tags.stop]);
                    return {};
                }),
                new: cont => cont([]),
                run: function(st) {
                    var result = st(x => x);
                    if (result.hasOwnProperty("cacheId")) {
                        delete result.cacheId;
                    }
                    return result;
                },
            },
            Ref: {
                new: mutFunc(x => { return {val: x}; }),
                read: mutFunc(x => x.val),
                write: mutFunc(x => { x[tags.obj].val = x[tags.val]; return {}; }),
            },
        },
        IO: {
            file: {
                unlink: mutVoidWithError(path =>
                    require('fs').unlink.bind(null, toString(path))),
                rename: mutVoidWithError(x =>
                    require('fs').rename.bind(null, toString(x[tags.oldPath]), toString(x[tags.newPath]))),
                chmod: mutVoidWithError(x =>
                    require('fs').chmod.bind(null, toString(x[tags.filePath]), x[tags.mode])),
                link: mutVoidWithError(x =>
                    require('fs').link.bind(null, toString(x[tags.srcPath]), toString(x[tags.dstPath]))),
                readFile: function(path) {
                    return function(cont) {
                        require('fs').readFile(toString(path), (err, data) => {
                            if (err) throw err;
                            cont(bytes(data));
                        });
                    };
                },
                appendFile: mutVoidWithError(x =>
                    require('fs').appendFile.bind(null, toString(x[tags.filePath]), Buffer.from(x[tags.data]), null)),
                writeFile: mutVoidWithError(x =>
                    require('fs').writeFile.bind(null, toString(x[tags.filePath]), Buffer.from(x[tags.data])), null),
            },
            os: {
                // TODO: When Lamdu has dicts, maybe just expose the whole env as one piece?
                env: mutFunc(x => {
                    var name = toString(x);
                    if (process.env.hasOwnProperty(name))
                    {
                        return {
                            tag: tags.just,
                            data: bytes(Buffer.from(process.env[name]))
                        };
                    }
                    return {
                        tag: tags.nothing,
                        data: {}
                    };
                }),
                exec: cmd => {
                    return cont => {
                        require('child_process').exec(toString(cmd), x => cont({}));
                    };
                }
            },
            network: {
                openTcpServer: mutFunc(x => {
                    var server = require('net').Server(socket => {
                        makeOpaque(socket);
                        rerun(x[tags.connectionHandler](socket))(
                            dataHandler =>
                            socket.on('data', data => { rerun(dataHandler(new Uint8Array(data)))(x => null); } )
                        );
                    });
                    server.listen({
                        host: toString(x[tags.host]),
                        port: x[tags.port],
                        exclusive: bool(x[tags.exclusive])
                    });
                    makeOpaque(server);
                    return server;
                }),
                closeTcpServer: function (server) {
                    return function (cont) {
                        server.close(cont);
                    };
                },
                socketSend: function (x) {
                    return function (cont) {
                        x[tags.socket].write(Buffer.from(x[tags.data]), null, cont);
                    };
                },
                connect: x => {
                    return cont => {
                        var socket = new require('net').Socket();
                        makeOpaque(socket);
                        var dataHandler = x[tags.dataHandler];
                        socket.on('data', data => { rerun(dataHandler(new Uint8Array(data)))(x => null); });
                        socket.connect(x[tags.port], toString(x[tags.host]), function() {
                            cont(socket);
                        });
                    };
                }
            },
            database: {
                postgres: {
                    connect: function(x) {
                        return function(cont) {
                            var pg = require('pg');
                            var client = new pg.Client({
                                host: toString(x[tags.host]),
                                port: x[tags.port],
                                database: toString(x[tags.database]),
                                user: toString(x[tags.user]),
                                password: toString(x[tags.password])
                            });
                            makeOpaque(client);
                            client.connect(err => {
                                if (err) throw err;
                                cont(client);
                            });
                        };
                    },
                    query: function(x) {
                        return function(cont) {
                            var utf8 = require('utf8');
                            x[tags.database].query(toString(x[tags.obj]), (err, resJs) => {
                                if (err) {
                                    cont({tag: tags.error, data: bytesFromAscii(err.message)});
                                    return;
                                }
                                var fields = [];
                                var rows = [];
                                if (resJs.fields) {
                                    for (var i = 0; i < resJs.fields.length; ++i) {
                                        fields.push(bytesFromAscii(resJs.fields[i].name));
                                    }
                                    for (i = 0; i < resJs.rows.length; ++i) {
                                        var jsRow = resJs.rows[i];
                                        var row = [];
                                        for (var k = 0; k < resJs.fields.length; ++k) {
                                            row.push(bytesFromAscii(utf8.encode(String(jsRow[resJs.fields[k].name]))));
                                        }
                                        rows.push(row);
                                    }
                                }
                                var resLamdu = {};
                                resLamdu[tags.fields] = fields;
                                resLamdu[tags.data] = rows;
                                cont({tag: tags.success, data: resLamdu});
                            });
                        };
                    }
                }
            },
            random: cont => cont(Math.random())
        }
    }
};

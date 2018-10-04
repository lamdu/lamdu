/* jshint node: true */
/* jshint esversion: 6 */
"use strict";

process.stdout._handle.setBlocking(true);

var encode = function() {
    var cacheId = 0;
    return function (x) {
        var replacer = function(key, value) {
            if (value == null) { // or undefined due to auto-coercion
                return {};
            }
            if (typeof value == "number" && !isFinite(value)) {
                return { "number": String(value) };
            }
            if ((typeof value != "object" && typeof value != "function") ||
                key === "array" || key === "bytes") {
                return value;
            }
            if (value.hasOwnProperty("cacheId")) {
                if (value.cacheId == -1) {
                    // Opaque value
                    return {};
                }
                return { cachedVal: value.cacheId };
            }
            value.cacheId = cacheId++;
            if (Function.prototype.isPrototypeOf(value)) {
                return { func: value.cacheId };
            }
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

var sendEvent = function (obj) {
    process.stdout.write(encode(obj));
    process.stdout.write("\n");
};

module.exports = {
    error: function(name, desc, globalId, exprId) {
        return {
            error: name,
            desc: desc,
            globalId: globalId,
            exprId: exprId,
        };
    },
    sendResult: function(scope, exprId, result) {
        sendEvent({
            event:"Result",
            scope:scope,
            exprId:exprId,
            result:result,
        });
    },
    sendNewScope: function(parentScope, scope, lamId, arg) {
        sendEvent({
            event:"NewScope",
            parentScope:parentScope,
            scope:scope,
            lamId:lamId,
            arg:arg
        });
    },
    sendCompletionSuccess: function(result) {
        sendEvent({
            event:"CompletionSuccess",
            result:result
        });
    },
    sendCompletionError: function(err) {
        if (Error.prototype.isPrototypeOf(err))
        {
            sendEvent({
                event:"CompletionError",
                err: {
                    error: "RuntimeError",
                    desc: err.toString()
                }
            });
            return;
        }
        sendEvent({
            event:"CompletionError",
            err:err
        });
    },
};

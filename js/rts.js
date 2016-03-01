"use strict";

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
    bytes: function (list) {
        var arr = new Uint8Array(list.length);
        arr.set(list);
        return arr;
    }
};

/* jshint node: true */
/* jshint esversion: 6 */
"use strict";

var conf = require('rtsConfig.js');

module.exports = {
    tags: {
        true:              conf.builtinTagName('true'),
        false:             conf.builtinTagName('false'),
        obj:               conf.builtinTagName('object'),
        infixl:            conf.builtinTagName('infixl'),
        infixr:            conf.builtinTagName('infixr'),
        index:             conf.builtinTagName('index'),
        start:             conf.builtinTagName('start'),
        stop:              conf.builtinTagName('stop'),
        val:               conf.builtinTagName('val'),
        oldPath:           conf.builtinTagName('oldPath'),
        newPath:           conf.builtinTagName('newPath'),
        filePath:          conf.builtinTagName('filePath'),
        data:              conf.builtinTagName('data'),
        mode:              conf.builtinTagName('mode'),
        srcPath:           conf.builtinTagName('srcPath'),
        dstPath:           conf.builtinTagName('dstPath'),
        host:              conf.builtinTagName('host'),
        port:              conf.builtinTagName('port'),
        user:              conf.builtinTagName('user'),
        password:          conf.builtinTagName('password'),
        database:          conf.builtinTagName('database'),
        fields:            conf.builtinTagName('fields'),
        exclusive:         conf.builtinTagName('exclusive'),
        connectionHandler: conf.builtinTagName('connectionHandler'),
        dataHandler:       conf.builtinTagName('dataHandler'),
        socket:            conf.builtinTagName('socket'),
        error:             conf.builtinTagName('error'),
        success:           conf.builtinTagName('success'),
        nothing:           conf.builtinTagName('nothing'),
        just:              conf.builtinTagName('just'),
        root:              conf.builtinTagName('root'),
        subtrees:          conf.builtinTagName('subtrees')
    },
};

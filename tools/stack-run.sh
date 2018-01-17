#!/bin/sh

# A script to build and run Lamdu using haskell-stack and git-cache

cd $(dirname $0)/..
git-cached-exec tools/git-cache-stack.spec && ./lamdu $@

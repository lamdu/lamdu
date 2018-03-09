#!/bin/sh

# A script to build Lamdu with stack and copy the result to a fixed location,
# so it can be used with `git-cached-exec`.

stack build --test --no-run-tests && cp `stack path --local-install-root`/bin/lamdu .

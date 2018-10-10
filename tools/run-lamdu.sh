#!/bin/bash
dir="$(dirname $0)"

export LD_LIBRARY_PATH=${dir}/lib

# For macOS
export DYLD_FALLBACK_LIBRARY_PATH=${dir}/lib

${dir}/bin/lamdu

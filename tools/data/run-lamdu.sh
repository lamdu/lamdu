#!/bin/bash
set -ue
dir="$(dirname $0)"
export LD_LIBRARY_PATH=${dir}/lib
cd "${dir}"
./bin/lamdu "$@"

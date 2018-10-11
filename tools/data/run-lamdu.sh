#!/bin/bash
dir="$(dirname $0)"
export LD_LIBRARY_PATH=${dir}/lib
${dir}/bin/lamdu

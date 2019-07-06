#! /bin/sh

stack build || exit

b=`basename $1`
f=$(echo $b | sed 's/\.[^\.]*$//')

stack exec ml2wasm -- $1 --memory wasm-memory/memory.wast -v
stack exec ml2wasm -- $1 --memory wasm-memory/memory.wast > $f.wast

wat2wasm $f.wast
echo "The result of execution ="
wasm-interp $f.wasm --run-all-exports --host-print

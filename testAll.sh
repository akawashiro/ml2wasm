#! /bin/sh

stack build || exit

for f in add closure fib fun if adder array div float nestlet print tuple
do
    echo "==========" $f.ml "=========="
    # stack exec ml2wasm -- ml-examples/$f.ml --memory wasm-memory/memory.wast -v
    stack exec ml2wasm -- ml-examples/$f.ml --memory wasm-memory/memory.wast > $f.wast
    wat2wasm $f.wast
    wasm-interp $f.wasm --run-all-exports --host-print
done

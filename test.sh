#! /bin/sh

stack build || exit

for f in let 
do
    stack exec ml2wasm -- ./ml-examples/$f.ml --memory wasm-memory/memory.wast -v
    stack exec ml2wasm -- ./ml-examples/$f.ml --memory wasm-memory/memory.wast > $f.wast
    
    wat2wasm $f.wast
    echo "The result of execution ="
    wasm-interp $f.wasm --run-all-exports --host-print
    # rm $f.wast $f.wasm
done

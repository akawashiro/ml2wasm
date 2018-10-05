#! /bin/sh

stack build || exit

for f in fib arith adder
do
    stack exec ml2wasm -- -d ./ml-examples/$f.ml
    stack exec ml2wasm -- ./ml-examples/$f.ml > $f.wast
    
    wat2wasm $f.wast
    echo "The result of execution ="
    wasm-interp $f.wasm --run-all-exports
    rm $f.wast $f.wasm
done

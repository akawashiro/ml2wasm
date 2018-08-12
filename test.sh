#! /bin/sh

stack build || exit
# stack exec hml-exe -- -d adder.ml
# echo "-------------------------------"
stack exec hml-exe -- -d ./ml-examples/arith.ml
stack exec hml-exe -- ./ml-examples/arith.ml > arith.wast
wat2wasm arith.wast
echo "The result of execution ="
wasm-interp arith.wasm --run-all-exports
# echo "-------------------------------"
#stack exec hml-exe -- -d closure.ml
#echo "-------------------------------"
#stack exec hml-exe -- -d fun.ml
#echo "-------------------------------"
#stack exec hml-exe -- -d if.ml
#echo "-------------------------------"
#stack exec hml-exe -- -d nestlet.ml
#echo "-------------------------------"
#stack exec hml-exe -- -d tuple.ml
#echo "-------------------------------"

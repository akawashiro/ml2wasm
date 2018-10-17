#! /bin/sh

wat2wasm $1 -o tmp.wasm
wasm-interp tmp.wasm --run-all-exports --host-print --trace
rm tmp.wasm

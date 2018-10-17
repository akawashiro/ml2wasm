# ml2wasm
A compiler from [MinCaml](http://esumii.github.io/min-caml/) to [WebAssembly](https://webassembly.org/).
## Requirement
To test this compiler, you must install [stack](https://docs.haskellstack.org/en/stable/README/) and [wabt](https://github.com/WebAssembly/wabt).
You can use this compiler without `wabt`.
But, in that case, you can just see outputed WebAssembly and you cannot run it.

`wabt` is utility for WebAssembly and it contains `wat2wasm` and `wasm-interp`.
I use these two utility in the test.
## How to use
You can build and test this compiler as follows.
```sh
% git clone https://github.com/akawashiro/ml2wasm.git
% stack init
% stack solver
% ./test.sh
```
When the test succeed, you can see following output.
This progam calculate Fibonacci numbers.
The last line is appended to pass the type check process.
```sh
...
The result of execution =
called host host.print(i32:1) =>
called host host.print(i32:1) =>
called host host.print(i32:1) =>
called host host.print(i32:2) =>
called host host.print(i32:3) =>
called host host.print(i32:5) =>
called host host.print(i32:8) =>
called host host.print(i32:13) =>
called host host.print(i32:21) =>
called host host.print(i32:34) =>
called host host.print(i32:55) =>
main() => i32:1
```
If you didn't install `wabt`, you can see only WebAssembly.
```sh
...
(i32.const 0)
(get_local $fun_print_1)
(set_local $stack_top_var)
(get_local $stack_top_var)
(get_local $stack_top_var)
(i32.load)
(call_indirect (param i32) (param i32) (result i32))))
```
## Try other examples
I prepare some examples in `ml-examples` directory.
When you want to try them like following.
```
% stack exec ml2wasm -- -d ./ml-examples/arith.ml
```
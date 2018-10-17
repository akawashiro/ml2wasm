;; square(i32 i) -> i32
(module
  (import "host" "print" (func $print_i32 (param i32)))
  (import "host" "print" (func $print_i32_i32 (param i32 i32)))
  (func (export "test") (result i32)
    i32.const 100
    call $print_i32
    i32.const 200
    i32.const 300
    call $print_i32_i32
    i32.const 1
    return)
)

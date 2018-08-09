;; square(i32 i) -> i32
(module
  (func (export "square") (result i32)
    (i32.add
      (i32.const 1)
      (i32.const 3))))

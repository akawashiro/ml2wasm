(module
  (func (export "local") (result i32)
    (local $i i32)
    (i32.const 10)
    (set_local $i)
    (get_local $i)
    (i32.const 3)
    (i32.add)))

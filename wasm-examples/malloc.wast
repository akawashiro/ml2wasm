(module
  (memory 1000)
  (global $HEAD_SIZE (export "HEAD_SIZE") i32 (i32.const 9))
  (global $FLAG_END (export "FLAG_END") i32 (i32.const 0))
  (global $FLAG_NON_USE (export "FLAG_NON_USE") i32 (i32.const 1))
  (global $FLAG_USE (export "FLAG_USE") i32 (i32.const 2))

  (func $get_flag (param $here i32) (result i32)
        (i32.const 0)
        )

  (func $get_next (param $here i32) (result i32)
        (i32.const 0)
        )

  (func $get_size (param $here i32) (result i32)
        (i32.const 0)
        )

  (func $malloc_body (param $size i32) (param $here i32) (param $prev i32) (result i32)
        (i32.const 0)
        )

  (func $malloc (param $size i32) (result i32)
        (i32.const 0)
        )

  (func $malloc_initalize
        (i32.store (i32.const 0) (i32.const 9))
        (i32.store (i32.const 4) (i32.const 0))
        (i32.store8 (i32.const 8) (get_global $FLAG_END))
        )

  (func (export "main") (result i32)
        (call $malloc_initalize)
        (i32.store (i32.const 10) (i32.const 42))
        (i32.load (i32.const 10))
        ))

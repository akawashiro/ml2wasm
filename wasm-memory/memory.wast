(module
  (memory 1000)
  (global $HEAD_SIZE (export "HEAD_SIZE") i32 (i32.const 9))
  (global $FLAG_NOT_USED (export "FLAG_NOT_USE") i32 (i32.const 0))
  (global $FLAG_USED (export "FLAG_USE") i32 (i32.const 1))
  (global $FLAG_NOT_END (export "FLAG_NOT_END") i32 (i32.const 0))
  (global $FLAG_END (export "FLAG_END") i32 (i32.const 2))

  (func $get_flag (param $here i32) (result i32)
        (i32.load8_u (i32.add (i32.const 8) (get_local $here))))

  (func $get_next (param $here i32) (result i32)
        (i32.load (get_local $here)))

  (func $get_size (param $here i32) (result i32)
        (i32.load (i32.add (i32.const 4) (get_local $here))))

  (func $set_not_used (param $here i32)
        (local $flag_here i32)
        (set_local $flag_here (call $get_flag (get_local $here)))
        (set_local $flag_here (i32.and (get_local $flag_here) (get_global $FLAG_END)))
        (i32.store8 (i32.add (get_local $here) (i32.const 8)) (get_local $flag_here)))

  (func $set_used (param $here i32)
        (local $flag_here i32)
        (set_local $flag_here (call $get_flag (get_local $here)))
        (set_local $flag_here (i32.or (get_local $flag_here) (get_global $FLAG_USED)))
        (i32.store8 (i32.add (get_local $here) (i32.const 8)) (get_local $flag_here)))

  (func $set_not_end (param $here i32)
        (local $flag_here i32)
        (set_local $flag_here (call $get_flag (get_local $here)))
        (set_local $flag_here (i32.and (get_local $flag_here) (get_global $FLAG_USED)))
        (i32.store8 (i32.add (get_local $here) (i32.const 8)) (get_local $flag_here)))

  (func $is_used (param $here i32) (result i32)
        (local $flag_here i32)
        (set_local $flag_here (call $get_flag (get_local $here)))
        (i32.eq (i32.and (i32.const 1) (get_local $flag_here)) (get_global $FLAG_USED)))

  (func $is_not_used (param $here i32) (result i32)
        (local $flag_here i32)
        (set_local $flag_here (call $get_flag (get_local $here)))
        (i32.eq (i32.and (i32.const 1) (get_local $flag_here)) (get_global $FLAG_NOT_USED)))

  (func $is_end (param $here i32) (result i32)
        (local $flag_here i32)
        (set_local $flag_here (call $get_flag (get_local $here)))
        (i32.eq (i32.and (i32.const 2) (get_local $flag_here)) (get_global $FLAG_END)))

  (func $is_sufficient_size (param $here i32) (param $size i32) (result i32)
        (local $size_here i32)
        (set_local $size_here (call $get_size (get_local $here)))
        (i32.le_s (get_local $size) (get_local $size_here)))

  (func $make_new_block (param $here i32) (param $size i32) (result i32)
        (i32.store (get_local $here) (i32.add (get_local $here) (i32.add (get_global $HEAD_SIZE) (get_local $size))))
        (i32.store (i32.add (i32.const 4) (get_local $here)) (get_local $size))
        (i32.store8 (i32.add (get_local $here) (i32.const 8)) (i32.add (get_global $FLAG_END) (get_global $FLAG_USED)))
        (get_local $here))

  (func $malloc_body (param $here i32) (param $size i32) (result i32)
        (local $size_here i32)
        (local $flag_here i32)
        (local $next_here i32)
        (set_local $size_here (call $get_size (get_local $here)))
        (set_local $flag_here (call $get_flag (get_local $here)))
        (set_local $next_here (call $get_next (get_local $here)))
        (if (result i32) (i32.and (call $is_not_used (get_local $here)) (call $is_sufficient_size (get_local $here) (get_local $size)))
          (then
            (call $set_used (get_local $here))
            (get_local $here))
        (else
          (if  (result i32) (call $is_end (get_local $here))
            (then
              (call $set_not_end (get_local $here))
              (call $make_new_block (get_local $next_here) (get_local $size)))
            (else
              (call $malloc_body (get_local $next_here) (get_local $size)))))))

  (func $malloc (param $size i32) (result i32)
        (i32.add (get_global $HEAD_SIZE) (call $malloc_body (i32.const 0) (get_local $size))))

  (func $free_body (param $here i32) (param $adress i32)
        (if (i32.eq (i32.add (get_global $HEAD_SIZE) (get_local $here)) (get_local $adress))
          (then 
            (call $set_not_used (get_local $here)))
          (else
            (if (call $is_end (get_local $here))
              (then
                (return))
              (else
                (call $free_body (call $get_next (get_local $here)) (get_local $adress)))))))

  (func $free (param $adress i32)
        (call $free_body (i32.const 0) (get_local $adress)))

  (func $malloc_initalize
        (i32.store (i32.const 0) (i32.const 9))
        (i32.store (i32.const 4) (i32.const 0))
        (i32.store8 (i32.const 8) 
                    (i32.add (get_global $FLAG_END) (get_global $FLAG_NOT_USED))))

  (func (export "main") (result i32)
        (call $malloc_initalize)
        (call $malloc (i32.const 20)) 
        (drop)
        (call $free (i32.const 18))
        (call $malloc (i32.const 30))))

(module
  (memory 1000)
  (global $HEAD_SIZE (export "HEAD_SIZE") i32 (i32.const 12))
  (global $FLAG_NOT_USED (export "FLAG_NOT_USE") i32 (i32.const 0))
  (global $FLAG_USED (export "FLAG_USE") i32 (i32.const 1))
  (global $FLAG_NOT_END (export "FLAG_NOT_END") i32 (i32.const 0))
  (global $FLAG_END (export "FLAG_END") i32 (i32.const 2))
  (global $GC_HEAD_SIZE (export "GC_HEAD_SIZE") i32 (i32.const 8))
  (global $GC_FLAG_VALUE (export "GC_FLAG_VALUE") i32 (i32.const 1))
  (global $GC_FLAG_SEARCHED (export "GC_FLAG_SEARCHED") i32 (i32.const 1))

  (func $get_flag (param $here i32) (result i32)
        (i32.load (i32.add (i32.const 8) (get_local $here))))

  (func $get_next (param $here i32) (result i32)
        (i32.load (get_local $here)))

  (func $get_size (param $here i32) (result i32)
        (i32.load (i32.add (i32.const 4) (get_local $here))))

  (func $set_not_used (param $here i32)
        (local $flag_here i32)
        (set_local $flag_here (call $get_flag (get_local $here)))
        (set_local $flag_here (i32.and (get_local $flag_here) (get_global $FLAG_END)))
        (i32.store (i32.add (get_local $here) (i32.const 8)) (get_local $flag_here)))

  (func $set_used (param $here i32)
        (local $flag_here i32)
        (set_local $flag_here (call $get_flag (get_local $here)))
        (set_local $flag_here (i32.or (get_local $flag_here) (get_global $FLAG_USED)))
        (i32.store (i32.add (get_local $here) (i32.const 8)) (get_local $flag_here)))

  (func $set_not_end (param $here i32)
        (local $flag_here i32)
        (set_local $flag_here (call $get_flag (get_local $here)))
        (set_local $flag_here (i32.and (get_local $flag_here) (get_global $FLAG_USED)))
        (i32.store (i32.add (get_local $here) (i32.const 8)) (get_local $flag_here)))

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

  (; Returned value does not include HEAD_SIZE ;)
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

  (func $free (param $adress i32)
        (call $set_not_used (i32.add (get_global $HEAD_SIZE) (get_local $adress))))

  (func $malloc_initalize
        (i32.store (i32.const 0) (get_global $HEAD_SIZE))
        (i32.store (i32.const 4) (i32.const 0))
        (i32.store8 (i32.const 8) 
                    (i32.add (get_global $FLAG_END) (get_global $FLAG_NOT_USED))))

  (; In functions with the prefix of "gc_",  ;)
  (; adresses point the head of gc block not the head of malloc block. ;)

  (func $gc_get_size (param $here i32) (result i32)
        (i32.load (get_local $here)))

  (func $gc_get_flag (param $here i32) (result i32)
        (i32.load (i32.add (i32.const 4) (get_local $here))))

  (func $gc_set_flag (param $here i32) (param $flag i32)
        (i32.store (i32.add (i32.const 4) (get_local $here)) (get_local $flag)))

  (func $gc_is_value (param $here i32) (result i32)
        (local $flag_here i32)
        (set_local $flag_here (call $gc_get_flag (get_local $here)))
        (if (result i32) (i32.eq (get_global $GC_FLAG_SEARCHED) 
                    (i32.and (get_local $flag_here) (get_global $GC_FLAG_VALUE)))
          (then (i32.const 1))
          (else (i32.const 0))))

  (func $gc_is_searched (param $here i32) (result i32)
        (local $flag_here i32)
        (set_local $flag_here (call $gc_get_flag (get_local $here)))
        (if (result i32) (i32.eq (get_global $GC_FLAG_SEARCHED) 
                    (i32.and (get_local $flag_here) (get_global $GC_FLAG_SEARCHED)))
          (then (i32.const 1))
          (else (i32.const 0))))

  (func $gc_set_searched (param $here i32)
        (local $flag_here i32)
        (set_local $flag_here (call $gc_get_flag (get_local $here)))
        (set_local $flag_here (i32.and (get_global $GC_FLAG_SEARCHED) (get_local $flag_here)))
        (call $gc_set_flag (get_local $here) (get_local $flag_here)))

  (func $gc_set_not_searched (param $here i32)
        (local $flag_here i32)
        (set_local $flag_here (call $gc_get_flag (get_local $here)))
        (set_local $flag_here (i32.xor (i32.xor (i32.const 0) (get_global $GC_FLAG_SEARCHED)) (get_local $flag_here)))
        (call $gc_set_flag (get_local $here) (get_local $flag_here)))

  (; "rc" means reference count ;)
  (func $gc_get_rc (param $here i32) (result i32)
        (i32.load (get_local $here)))

  (func $gc_increase_rc (param $here i32)
        (i32.store (get_local $here) 
                   (i32.add (i32.const 1) (call $gc_get_rc (get_local $here)))))

  (func $gc_decrease_rc_dfs (param $here i32)
        (local $offset i32)
        (local $size_here i32)

        (if (call $gc_is_searched (get_local $here))
          (then return))

        (set_local $size_here (call $gc_get_size (get_local $here)))
        (call $gc_set_searched (get_local $here))
        (i32.store (get_local $here) 
                   (i32.sub (call $gc_get_rc (get_local $here)) (i32.const 1)))
        (if (call $gc_is_value (get_local $here))
          (then
            (nop))
          (else
            (block $exit
                   (loop $loop
                         (br_if $exit (i32.eq (get_local $offset) (get_local $size_here)))
                         (call $gc_decrease_rc_dfs (i32.add (get_global $GC_HEAD_SIZE) (i32.add (get_local $offset) (get_local $here))))
                         (set_local $offset (i32.add (i32.const 4) (get_local $offset)))
                         (br $loop))))))

  (func $gc_unset_searched_dfs (param $here i32)
        (local $offset i32)
        (local $size_here i32)

        (if (call $gc_is_searched (get_local $here))
          (then nop)
          (else return))

        (set_local $size_here (call $gc_get_size (get_local $here)))
        (call $gc_set_not_searched (get_local $here))
        (if (call $gc_is_value (get_local $here))
          (then
            (nop))
          (else
            (block $exit
                   (loop $loop
                         (br_if $exit (i32.eq (get_local $offset) (get_local $size_here)))
                         (call $gc_decrease_rc_dfs (i32.add (get_global $GC_HEAD_SIZE) (i32.add (get_local $offset) (get_local $here))))
                         (set_local $offset (i32.add (i32.const 4) (get_local $offset)))
                         (br $loop))))))

  (func $gc_decrease_rc (param $here i32)
        (call $gc_decrease_rc_dfs (get_local $here))
        (call $gc_unset_searched_dfs (get_local $here)))

  (; is_value is 0(not a value) or 1(value) ;)
  (func $gc_malloc (param $size i32) (param $is_value i32) (result i32)
        (local $s i32)
        (local $p i32)
        (set_local $s (i32.add (get_local $size) (get_global $GC_HEAD_SIZE)))
        (set_local $p (call $malloc (get_local $s)))
        (i32.store (get_local $p) (get_local $size))
        (i32.store (i32.add (get_local $p) (i32.const 4)) (get_local $is_value))
        (i32.add (get_global $GC_HEAD_SIZE) (get_local $p)))

  (; In this function here is an adress for malloc not for gc_malloc ;)
  (func $gc_free_body (param $here i32)
        (local $next_here i32)
        (if (i32.eq (call $gc_get_rc (i32.add (get_global $GC_HEAD_SIZE) (get_local $here))) 
                    (i32.const 0))
          (then
            (call $free (get_local $here))
            (set_local $next_here (call $get_next (get_local $here)))
            (call $gc_free_body (get_local $next_here)))
          (else
            (set_local $next_here (call $get_next (get_local $here)))
            (call $gc_free_body (get_local $next_here)))))

  (func $gc_free
        (call $gc_free_body (i32.const 0)))

  (func (export "main") (result i32)
        (call $malloc_initalize)
        (; (call $malloc (i32.const 20)))) ;)
        (call $gc_malloc (i32.const 20) (i32.const 1))))

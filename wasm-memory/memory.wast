(; Structure of malloc block                                                        ;)
(; | -- Adress for internal functions                                               ;)
(; |                                    Adress for external functions--|            ;)
(; v                                                                   V            ;)
(; |-----------------------+-----------------------------------+-------+----------| ;)
(; | 4byte                 | 4byte                             | 4byte | 4*n byte | ;)
(; | pointer to next block | size of block without malloc head | flag  | contents | ;)
(; |-----------------------+-----------------------------------+-------+----------| ;)
(; Structure of gc block (contained in ``contents'' of malloc block)                ;)
(; | -- Adress for internal functions                                               ;)
(; |                          Adress for external functions--|                      ;)
(; v                                                         V                      ;)
(; |-------------------------------+-------+-----------------+----------|           ;)
(; | 4byte                         | 4byte | 4byte           | 4*n byte |           ;)
(; | size of block without gc head | flag  | reference count | contents |           ;)
(; |-------------------------------+-------+-----------------+----------|           ;)

(module
  (import "host" "print" (func $print_f32 (param f32)))
  (import "host" "print" (func $print_i32 (param i32)))
  (memory 1000)

  (; BEGIN DEFNITION OF GLOBAL VARIABLES ;)

  (global $HEAD_SIZE (export "HEAD_SIZE") i32 (i32.const 12))
  (global $OFFSET_NEXT (export "OFFSET_NEXT") i32 (i32.const 0))
  (global $OFFSET_SIZE (export "OFFSET_SIZE") i32 (i32.const 4))
  (global $OFFSET_FLAG (export "OFFSET_FLAG") i32 (i32.const 8))
  (global $FLAG_NOT_USED (export "FLAG_NOT_USE") i32 (i32.const 0))
  (global $FLAG_USED (export "FLAG_USE") i32 (i32.const 1))
  (global $FLAG_NOT_END (export "FLAG_NOT_END") i32 (i32.const 0))
  (global $FLAG_END (export "FLAG_END") i32 (i32.const 2))
  (global $GC_HEAD_SIZE (export "GC_HEAD_SIZE") i32 (i32.const 12))
  (global $GC_OFFSET_SIZE (export "GC_OFFSET_SIZE") i32 (i32.const 0))
  (global $GC_OFFSET_FLAG (export "GC_OFFSET_FLAG") i32 (i32.const 4))
  (global $GC_OFFSET_RC (export "GC_OFFSET_RC") i32 (i32.const 8))
  (global $GC_FLAG_VALUE (export "GC_FLAG_VALUE") i32 (i32.const 1))
  (global $GC_FLAG_SEARCHED (export "GC_FLAG_SEARCHED") i32 (i32.const 2))

  (; END DEFNITION OF GLOBAL VARIABLES ;)

  (; BEGIN DEFNITION OF MEMORY FUNCTIONS ;)

  (func $get_flag (param $here i32) (result i32)
        (i32.load (i32.add (get_global $OFFSET_FLAG) (get_local $here))))

  (func $get_next (param $here i32) (result i32)
        (i32.load (get_local $here)))

  (func $get_size (param $here i32) (result i32)
        (i32.load (i32.add (get_global $OFFSET_SIZE) (get_local $here))))

  (func $set_not_used (param $here i32)
        (local $flag_here i32)
        (set_local $flag_here (call $get_flag (get_local $here)))
        (set_local $flag_here (i32.and (get_local $flag_here) (get_global $FLAG_END)))
        (i32.store (i32.add (get_local $here) (get_global $OFFSET_FLAG)) (get_local $flag_here)))

  (func $set_used (param $here i32)
        (local $flag_here i32)
        (set_local $flag_here (call $get_flag (get_local $here)))
        (set_local $flag_here (i32.or (get_local $flag_here) (get_global $FLAG_USED)))
        (i32.store (i32.add (get_local $here) (get_global $OFFSET_FLAG)) (get_local $flag_here)))

  (func $set_not_end (param $here i32)
        (local $flag_here i32)
        (set_local $flag_here (call $get_flag (get_local $here)))
        (set_local $flag_here (i32.and (get_local $flag_here) (get_global $FLAG_USED)))
        (i32.store (i32.add (get_local $here) (get_global $OFFSET_FLAG)) (get_local $flag_here)))

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
        (i32.store (i32.add (get_global $OFFSET_SIZE) (get_local $here)) (get_local $size))
        (i32.store (i32.add (get_local $here) (get_global $OFFSET_FLAG)) (i32.add (get_global $FLAG_END) (get_global $FLAG_USED)))
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

  (; Returned adress does not contain the malloc head ;)
  (func $malloc (param $size i32) (result i32)
        (i32.add (get_global $HEAD_SIZE) (call $malloc_body (i32.const 0) (get_local $size))))

  (; To pass internal functions, we must substract HEAD_SIZE from the given adress ;)
  (func $free (param $adress i32)
        (call $set_not_used (i32.sub (get_local $adress) (get_global $HEAD_SIZE))))

  (func $malloc_initalize
        (i32.store (get_global $OFFSET_NEXT) (get_global $HEAD_SIZE))
        (i32.store (get_global $OFFSET_SIZE) (i32.const 0))
        (i32.store (get_global $OFFSET_FLAG) 
                    (i32.add (get_global $FLAG_END) (get_global $FLAG_NOT_USED))))

  (; In functions with the prefix of "gc_",  ;)
  (; adresses point the head of gc block not the head of malloc block. ;)

  (func $gc_initalize
        (i32.store (get_global $OFFSET_NEXT) (i32.add (get_global $GC_HEAD_SIZE) (get_global $HEAD_SIZE)))
        (i32.store (get_global $OFFSET_SIZE) (i32.const 0))
        (i32.store (get_global $OFFSET_FLAG) 
                    (i32.add (get_global $FLAG_END) (get_global $FLAG_NOT_USED)))
        (i32.store (i32.add (get_global $GC_OFFSET_RC) (get_global $HEAD_SIZE)) (i32.const 1)))

  (func $gc_get_size (param $here i32) (result i32)
        (i32.load (get_local $here)))

  (func $gc_get_flag (param $here i32) (result i32)
        (i32.load (i32.add (get_global $GC_OFFSET_FLAG) (get_local $here))))

  (func $gc_set_flag (param $here i32) (param $flag i32)
        (i32.store (i32.add (get_global $GC_OFFSET_FLAG) (get_local $here)) (get_local $flag)))

  (func $gc_is_value (param $here i32) (result i32)
        (local $flag_here i32)
        (set_local $flag_here (call $gc_get_flag (get_local $here)))
        (if (result i32) (i32.eq (get_global $GC_FLAG_VALUE) 
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
        (set_local $flag_here (i32.or (get_global $GC_FLAG_SEARCHED) (get_local $flag_here)))
        (call $gc_set_flag (get_local $here) (get_local $flag_here)))

  (func $gc_set_not_searched (param $here i32)
        (local $flag_here i32)
        (set_local $flag_here (call $gc_get_flag (get_local $here)))
        (set_local $flag_here (i32.xor (i32.xor (i32.const 0) (get_global $GC_FLAG_SEARCHED)) (get_local $flag_here)))
        (call $gc_set_flag (get_local $here) (get_local $flag_here)))

  (; "rc" means reference count ;)
  (func $gc_get_rc (param $here i32) (result i32)
        (i32.load (i32.add (get_global $GC_OFFSET_RC) (get_local $here))))

  (; ``here'' does not contain GC_HEAD_SIZE because this function is called from outside. ;)
  (func $gc_increase_rc (param $here i32)
        (set_local $here (i32.sub (get_local $here) (get_global $GC_HEAD_SIZE)))
        (i32.store (i32.add (get_global $GC_OFFSET_RC) (get_local $here))
                   (i32.add (i32.const 1) (call $gc_get_rc (get_local $here)))))

  (func $gc_decrease_rc_dfs (param $here i32)
        (local $offset i32)
        (local $size_here i32)

        (if (call $gc_is_searched (get_local $here))
          (then return))

        (set_local $size_here (call $gc_get_size (get_local $here)))
        (call $gc_set_searched (get_local $here))
        (i32.store (i32.add (get_global $GC_OFFSET_RC) (get_local $here))
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
                         (call $gc_unset_searched_dfs (i32.add (get_global $GC_HEAD_SIZE) (i32.add (get_local $offset) (get_local $here))))
                         (set_local $offset (i32.add (i32.const 4) (get_local $offset)))
                         (br $loop))))))

  (; ``here'' does not contain GC_HEAD_SIZE because this function is called from outside. ;)
  (func $gc_decrease_rc (param $here i32)
        (if (i32.eq (get_local $here) (i32.const 0))
          (then)
          (else
            (set_local $here (i32.sub (get_local $here) (get_global $GC_HEAD_SIZE)))
            (call $gc_decrease_rc_dfs (get_local $here))
            (call $gc_unset_searched_dfs (get_local $here)))))

  (; is_value is 0(not a value) or 1(value) ;)
  (func $gc_malloc (param $size i32) (param $is_value i32) (result i32)
        (local $s i32)
        (local $p i32)
        (set_local $s (i32.add (get_local $size) (get_global $GC_HEAD_SIZE)))
        (set_local $p (call $malloc (get_local $s)))
        (i32.store (get_local $p) (get_local $size))
        (i32.store (i32.add (get_local $p) (get_global $GC_OFFSET_FLAG)) (get_local $is_value))
        (i32.add (get_global $GC_HEAD_SIZE) (get_local $p)))

  (; In this function, ``here'' is an adress for malloc block ;)
  (func $gc_free_body (param $here i32)
        (local $next_here i32)
        (local $rc_here i32)
        (set_local $rc_here (call $gc_get_rc (i32.add (get_global $HEAD_SIZE) (get_local $here))))
        (if (i32.eq (get_local $rc_here) (i32.const 0))
          (then
            (call $free (i32.add (get_global $HEAD_SIZE) (get_local $here)))))
        (if (i32.eq (i32.const 0) (call $is_end (get_local $here)))
          (then
            (set_local $next_here (call $get_next (get_local $here)))
            (call $gc_free_body (get_local $next_here)))))

  (func $gc_free
        (call $gc_free_body (i32.const 0)))

  (; END DEFNITION OF MEMORY FUNCTIONS ;)

  (func (export "main") (result i32)
        (local $p1 i32)
        (call $gc_initalize)
        (; (call $malloc (i32.const 20)))) ;)
        (set_local $p1 (call $gc_malloc (i32.const 10) (i32.const 1)))
        (call $gc_increase_rc (get_local $p1))
        (call $gc_decrease_rc (get_local $p1))
        (call $gc_free)
        (; (drop) ;)
        (call $gc_malloc (i32.const 10) (i32.const 1))))

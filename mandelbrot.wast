(module
(import "host" "print" (func $print_f32 (param f32)))
(import "host" "print" (func $print_i32 (param i32)))
(memory 10000)

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

  (; This function may be used as an library function, so ``adress'' do not include HEAD_SIZE ;)
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
  (func $gc_increase_rc (param $here i32) (result i32)
        (set_local $here (i32.sub (get_local $here) (get_global $GC_HEAD_SIZE)))
        (i32.store (i32.add (get_global $GC_OFFSET_RC) (get_local $here))
                   (i32.add (i32.const 1) (call $gc_get_rc (get_local $here))))
        (i32.add (get_local $here) (get_global $GC_HEAD_SIZE)))

  (func $gc_decrease_rc_dfs (param $here i32)
        (local $offset i32)
        (local $size_here i32)
        (; (call $print_i32 (i32.const 1111111)) ;)

        (; When ``here'' is NULL, return immediately ;)
        (if (i32.eq (i32.const 0) (get_local $here))
          (then return))
        (if (call $gc_is_searched (get_local $here))
          (then return))

        (set_local $size_here (call $gc_get_size (get_local $here)))
        (call $gc_set_searched (get_local $here))
        (i32.store (i32.add (get_global $GC_OFFSET_RC) (get_local $here))
                   (i32.sub (call $gc_get_rc (get_local $here)) (i32.const 1)))

        (; (if (call $gc_is_value (get_local $here)) ;)
          (; (then return)) ;)

        (; If the RC is 0, release the block ;)
        (if (i32.eq (call $gc_get_rc (get_local $here)) (i32.const 0))
          (then (call $free (get_local $here))))

        (; If the RC is not 0, return immediately ;)
        (if (call $gc_get_rc (get_local $here))
          (then return))

        (; (if (i32.eq (call $gc_get_rc (get_local $here)) (i32.const 0)) ;)
          (; (then  ;)
            (; (if (call $gc_is_value (get_local $here)) ;)
              (; (then) ;)
              (; (else ;)
                (; (call $free (get_local $here)))))) ;)

        (if (call $gc_is_value (get_local $here))
          (then
            (nop))
          (else
            (block $exit
                   (loop $loop
                        (; (call $print_i32 (get_local $offset)) ;)
                        (; (call $print_i32 (get_local $size_here)) ;)
                         (br_if $exit (i32.eq (get_local $offset) (get_local $size_here)))

                         (; (if (i32.eq (i32.const 0) (i32.load (i32.add (get_global $GC_HEAD_SIZE) (i32.add (get_local $offset) (get_local $here))))) ;)
                         (if (i32.eq (i32.const 0) (call $gc_get_rc (i32.add (get_local $offset) (get_local $here))))
                           (then)
                           (else
                            (call $gc_decrease_rc_dfs (i32.sub (i32.load (i32.add (get_global $GC_HEAD_SIZE) (i32.add (get_local $offset) (get_local $here)))) (get_global $GC_HEAD_SIZE)))))
                         (set_local $offset (i32.add (i32.const 4) (get_local $offset)))
                         (br $loop))))))

  (func $gc_unset_searched_dfs (param $here i32)
        (local $offset i32)
        (local $size_here i32)
        (; (call $print_i32 (i32.const 222222)) ;)

        (; When ``here'' is NULL, return immediately ;)
        (if (i32.eq (i32.const 0) (get_local $here))
          (then return))
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
                        (; (call $print_i32 (get_local $offset)) ;)
                        (; (call $print_i32 (get_local $size_here)) ;)
                         (br_if $exit (i32.eq (get_local $offset) (get_local $size_here)))
                         (if (i32.eq (i32.const 0) (i32.load (i32.add (get_global $GC_HEAD_SIZE) (i32.add (get_local $offset) (get_local $here)))))
                           (then)
                           (else
                         (call $gc_unset_searched_dfs (i32.sub (i32.load (i32.add (get_global $GC_HEAD_SIZE) (i32.add (get_local $offset) (get_local $here)))) (get_global $GC_HEAD_SIZE)))))
                         (set_local $offset (i32.add (i32.const 4) (get_local $offset)))
                         (br $loop))))))

  (; ``here'' does not contain GC_HEAD_SIZE because this function is called from outside. ;)
  (func $gc_decrease_rc (param $here i32) (result i32)
        (if (result i32) (i32.eq (get_local $here) (i32.const 0))
          (then
            (get_local $here))
          (else
            (set_local $here (i32.sub (get_local $here) (get_global $GC_HEAD_SIZE)))
            (call $gc_decrease_rc_dfs (get_local $here))
            (call $gc_unset_searched_dfs (get_local $here))
            (i32.add (get_local $here) (get_global $GC_HEAD_SIZE)))))

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
  (func $gc_all_free_body (param $here i32)
        (local $next_here i32)
        (local $rc_here i32)
        (set_local $rc_here (call $gc_get_rc (i32.add (get_global $HEAD_SIZE) (get_local $here))))
        (if (i32.eq (get_local $rc_here) (i32.const 0))
          (then
            (call $free (i32.add (get_global $HEAD_SIZE) (get_local $here)))))
        (if (i32.eq (i32.const 0) (call $is_end (get_local $here)))
          (then
            (set_local $next_here (call $get_next (get_local $here)))
            (call $gc_all_free_body (get_local $next_here)))))

  (func $gc_all_free
        (call $gc_all_free_body (i32.const 0)))
(table 6 anyfunc)
(func $def_fun_andb_0 (param $val_b1_20 i32) (param $val_b2_21 i32) (param $cls_fun_andb_0 i32) (result i32)
(local $stack_top_var i32)
(; if ;)
(get_local $val_b1_20)
(i32.load)
(if (result i32)
(i32.eqz)
(then
(i32.const 4)
(i32.const 1)
(call $gc_malloc)
(set_local $stack_top_var)
(get_local $stack_top_var)
(get_local $stack_top_var)
(i32.const 0)
(i32.store))
(else
(; if ;)
(get_local $val_b2_21)
(i32.load)
(if (result i32)
(i32.eqz)
(then
(i32.const 4)
(i32.const 1)
(call $gc_malloc)
(set_local $stack_top_var)
(get_local $stack_top_var)
(get_local $stack_top_var)
(i32.const 0)
(i32.store))
(else
(i32.const 4)
(i32.const 1)
(call $gc_malloc)
(set_local $stack_top_var)
(get_local $stack_top_var)
(get_local $stack_top_var)
(i32.const 1)
(i32.store)))))
(call $gc_increase_rc))
(func $def_fun_abs_1 (param $val_f_19 i32) (param $cls_fun_abs_1 i32) (result i32)
(local $stack_top_var i32)
(; if ;)
(; arith op ;)
(i32.const 4)
(i32.const 1)
(call $gc_malloc)
(set_local $stack_top_var)
(get_local $stack_top_var)
(get_local $stack_top_var)
(f32.const 0.0)
(get_local $val_f_19)
(f32.load)
(f32.lt)
(i32.store)
(; arith op end ;)
(i32.load)
(if (result i32)
(i32.eqz)
(then
(; arith op ;)
(i32.const 4)
(i32.const 1)
(call $gc_malloc)
(set_local $stack_top_var)
(get_local $stack_top_var)
(get_local $stack_top_var)
(f32.const -1.0)
(get_local $val_f_19)
(f32.load)
(f32.mul)
(f32.store)
(; arith op end ;))
(else
(get_local $val_f_19)))
(call $gc_increase_rc))
(func $def_fun_isConv_2 (param $val_x_17 i32) (param $val_y_18 i32) (param $cls_fun_isConv_2 i32) (result i32)
(local $fun_abs_1 i32)
(local $fun_andb_0 i32)
(local $tmp_fun_isConv_2 i32)
(local $stack_top_var i32)
(; tuple destruction ;)
(get_local $cls_fun_isConv_2)
(set_local $stack_top_var)
(get_local $stack_top_var)
(i32.const 0)
(i32.add)
(i32.load)
(set_local $tmp_fun_isConv_2)
(get_local $stack_top_var)
(i32.const 4)
(i32.add)
(i32.load)
(set_local $fun_andb_0)
(get_local $stack_top_var)
(i32.const 8)
(i32.add)
(i32.load)
(set_local $fun_abs_1)
(get_local $stack_top_var)
(i32.const 12)
(i32.add)
(i32.load)
(set_local $fun_abs_1)
(; if ;)
(; arith op ;)
(i32.const 4)
(i32.const 1)
(call $gc_malloc)
(set_local $stack_top_var)
(get_local $stack_top_var)
(get_local $stack_top_var)
(get_local $val_x_17)
(get_local $fun_abs_1)
(get_local $fun_abs_1)
(i32.load)
(i32.load)
(call_indirect (param i32) (param i32) (result i32))
(f32.load)
(f32.const 1.0)
(f32.lt)
(i32.store)
(; arith op end ;)
(; arith op ;)
(i32.const 4)
(i32.const 1)
(call $gc_malloc)
(set_local $stack_top_var)
(get_local $stack_top_var)
(get_local $stack_top_var)
(get_local $val_y_18)
(get_local $fun_abs_1)
(get_local $fun_abs_1)
(i32.load)
(i32.load)
(call_indirect (param i32) (param i32) (result i32))
(f32.load)
(f32.const 1.0)
(f32.lt)
(i32.store)
(; arith op end ;)
(get_local $fun_andb_0)
(get_local $fun_andb_0)
(i32.load)
(i32.load)
(call_indirect (param i32) (param i32) (param i32) (result i32))
(i32.load)
(if (result i32)
(i32.eqz)
(then
(i32.const 4)
(i32.const 1)
(call $gc_malloc)
(set_local $stack_top_var)
(get_local $stack_top_var)
(get_local $stack_top_var)
(i32.const 0)
(i32.store))
(else
(i32.const 4)
(i32.const 1)
(call $gc_malloc)
(set_local $stack_top_var)
(get_local $stack_top_var)
(get_local $stack_top_var)
(i32.const 1)
(i32.store)))
(; tuple destruction end ;)
(call $gc_increase_rc)
(get_local $fun_abs_1)
(call $gc_decrease_rc)
(drop)
(get_local $fun_andb_0)
(call $gc_decrease_rc)
(drop)
(get_local $tmp_fun_isConv_2)
(call $gc_decrease_rc)
(drop))
(func $def_fun_m_4 (param $val_a_11 i32) (param $val_b_12 i32) (param $cls_fun_m_4 i32) (result i32)
(local $fun_f_13 i32)
(local $fun_isConv_2 i32)
(local $val_times_3 i32)
(local $tmp_fun_m_4 i32)
(local $stack_top_var i32)
(; tuple destruction ;)
(get_local $cls_fun_m_4)
(set_local $stack_top_var)
(get_local $stack_top_var)
(i32.const 0)
(i32.add)
(i32.load)
(set_local $tmp_fun_m_4)
(get_local $stack_top_var)
(i32.const 4)
(i32.add)
(i32.load)
(set_local $val_times_3)
(get_local $stack_top_var)
(i32.const 8)
(i32.add)
(i32.load)
(set_local $fun_isConv_2)
(; let ;)
(; tuple construction ;)
(i32.const 20)
(i32.const 0)
(call $gc_malloc)
(set_local $stack_top_var)
(get_local $stack_top_var)
(get_local $stack_top_var)
(get_local $stack_top_var)
(get_local $stack_top_var)
(get_local $stack_top_var)
(get_local $stack_top_var)
(i32.const 0)
(i32.add)
(i32.const 4)
(i32.const 1)
(call $gc_malloc)
(set_local $stack_top_var)
(get_local $stack_top_var)
(get_local $stack_top_var)
(i32.const 5)
(i32.store)
(call $gc_increase_rc)
(i32.store)
(i32.const 4)
(i32.add)
(get_local $val_times_3)
(call $gc_increase_rc)
(i32.store)
(i32.const 8)
(i32.add)
(get_local $fun_isConv_2)
(call $gc_increase_rc)
(i32.store)
(i32.const 12)
(i32.add)
(get_local $val_a_11)
(call $gc_increase_rc)
(i32.store)
(i32.const 16)
(i32.add)
(get_local $val_b_12)
(call $gc_increase_rc)
(i32.store)
(; tuple construction end ;)
(call $gc_increase_rc)
(set_local $fun_f_13)
(get_local $val_a_11)
(get_local $val_b_12)
(i32.const 4)
(i32.const 1)
(call $gc_malloc)
(set_local $stack_top_var)
(get_local $stack_top_var)
(get_local $stack_top_var)
(i32.const 0)
(i32.store)
(get_local $fun_f_13)
(get_local $fun_f_13)
(i32.load)
(i32.load)
(call_indirect (param i32) (param i32) (param i32) (param i32) (result i32))
(; tuple destruction end ;)
(call $gc_increase_rc)
(get_local $fun_f_13)
(call $gc_decrease_rc)
(drop)
(get_local $fun_isConv_2)
(call $gc_decrease_rc)
(drop)
(get_local $val_times_3)
(call $gc_decrease_rc)
(drop)
(get_local $tmp_fun_m_4)
(call $gc_decrease_rc)
(drop))
(func $def_fun_plot_6 (param $val_x_7 i32) (param $val_y_8 i32) (param $cls_fun_plot_6 i32) (result i32)
(local $val_yy_10 i32)
(local $val_xx_9 i32)
(local $fun_plot_6 i32)
(local $fun_m_4 i32)
(local $val_dt_5 i32)
(local $fun_andb_0 i32)
(local $tmp_fun_plot_6 i32)
(local $stack_top_var i32)
(; tuple destruction ;)
(get_local $cls_fun_plot_6)
(set_local $stack_top_var)
(get_local $stack_top_var)
(i32.const 0)
(i32.add)
(i32.load)
(set_local $tmp_fun_plot_6)
(get_local $stack_top_var)
(i32.const 4)
(i32.add)
(i32.load)
(set_local $fun_andb_0)
(get_local $stack_top_var)
(i32.const 8)
(i32.add)
(i32.load)
(set_local $val_dt_5)
(get_local $stack_top_var)
(i32.const 12)
(i32.add)
(i32.load)
(set_local $val_dt_5)
(get_local $stack_top_var)
(i32.const 16)
(i32.add)
(i32.load)
(set_local $fun_m_4)
(; let ;)
(; tuple construction ;)
(i32.const 20)
(i32.const 0)
(call $gc_malloc)
(set_local $stack_top_var)
(get_local $stack_top_var)
(get_local $stack_top_var)
(get_local $stack_top_var)
(get_local $stack_top_var)
(get_local $stack_top_var)
(get_local $stack_top_var)
(i32.const 0)
(i32.add)
(i32.const 4)
(i32.const 1)
(call $gc_malloc)
(set_local $stack_top_var)
(get_local $stack_top_var)
(get_local $stack_top_var)
(i32.const 4)
(i32.store)
(call $gc_increase_rc)
(i32.store)
(i32.const 4)
(i32.add)
(get_local $fun_andb_0)
(call $gc_increase_rc)
(i32.store)
(i32.const 8)
(i32.add)
(get_local $val_dt_5)
(call $gc_increase_rc)
(i32.store)
(i32.const 12)
(i32.add)
(get_local $val_dt_5)
(call $gc_increase_rc)
(i32.store)
(i32.const 16)
(i32.add)
(get_local $fun_m_4)
(call $gc_increase_rc)
(i32.store)
(; tuple construction end ;)
(call $gc_increase_rc)
(set_local $fun_plot_6)
(; if ;)
(; arith op ;)
(i32.const 4)
(i32.const 1)
(call $gc_malloc)
(set_local $stack_top_var)
(get_local $stack_top_var)
(get_local $stack_top_var)
(f32.const 1.0)
(get_local $val_x_7)
(f32.load)
(f32.lt)
(i32.store)
(; arith op end ;)
(; arith op ;)
(i32.const 4)
(i32.const 1)
(call $gc_malloc)
(set_local $stack_top_var)
(get_local $stack_top_var)
(get_local $stack_top_var)
(f32.const 1.0)
(get_local $val_y_8)
(f32.load)
(f32.lt)
(i32.store)
(; arith op end ;)
(get_local $fun_andb_0)
(get_local $fun_andb_0)
(i32.load)
(i32.load)
(call_indirect (param i32) (param i32) (param i32) (result i32))
(i32.load)
(if (result i32)
(i32.eqz)
(then
(; let ;)
(; if ;)
(; arith op ;)
(i32.const 4)
(i32.const 1)
(call $gc_malloc)
(set_local $stack_top_var)
(get_local $stack_top_var)
(get_local $stack_top_var)
(f32.const 1.0)
(get_local $val_x_7)
(f32.load)
(f32.lt)
(i32.store)
(; arith op end ;)
(i32.load)
(if (result i32)
(i32.eqz)
(then
(; arith op ;)
(i32.const 4)
(i32.const 1)
(call $gc_malloc)
(set_local $stack_top_var)
(get_local $stack_top_var)
(get_local $stack_top_var)
(get_local $val_x_7)
(f32.load)
(get_local $val_dt_5)
(f32.load)
(f32.add)
(f32.store)
(; arith op end ;))
(else
(i32.const 4)
(i32.const 1)
(call $gc_malloc)
(set_local $stack_top_var)
(get_local $stack_top_var)
(get_local $stack_top_var)
(f32.const -1.0)
(f32.store)))
(call $gc_increase_rc)
(set_local $val_xx_9)
(; let ;)
(; if ;)
(; arith op ;)
(i32.const 4)
(i32.const 1)
(call $gc_malloc)
(set_local $stack_top_var)
(get_local $stack_top_var)
(get_local $stack_top_var)
(f32.const 1.0)
(get_local $val_x_7)
(f32.load)
(f32.lt)
(i32.store)
(; arith op end ;)
(i32.load)
(if (result i32)
(i32.eqz)
(then
(get_local $val_y_8))
(else
(; arith op ;)
(i32.const 4)
(i32.const 1)
(call $gc_malloc)
(set_local $stack_top_var)
(get_local $stack_top_var)
(get_local $stack_top_var)
(get_local $val_y_8)
(f32.load)
(get_local $val_dt_5)
(f32.load)
(f32.add)
(f32.store)
(; arith op end ;)))
(call $gc_increase_rc)
(set_local $val_yy_10)
(; if ;)
(get_local $val_x_7)
(get_local $val_y_8)
(get_local $fun_m_4)
(get_local $fun_m_4)
(i32.load)
(i32.load)
(call_indirect (param i32) (param i32) (param i32) (result i32))
(i32.load)
(if (result i32)
(i32.eqz)
(then
(get_local $val_xx_9)
(get_local $val_yy_10)
(get_local $fun_plot_6)
(get_local $fun_plot_6)
(i32.load)
(i32.load)
(call_indirect (param i32) (param i32) (param i32) (result i32)))
(else
(get_local $val_x_7)
(f32.load)
(call $print_f32)
(get_local $val_y_8)
(f32.load)
(call $print_f32)
(get_local $val_xx_9)
(get_local $val_yy_10)
(get_local $fun_plot_6)
(get_local $fun_plot_6)
(i32.load)
(i32.load)
(call_indirect (param i32) (param i32) (param i32) (result i32)))))
(else
(i32.const 4)
(i32.const 1)
(call $gc_malloc)
(set_local $stack_top_var)
(get_local $stack_top_var)
(get_local $stack_top_var)
(i32.const 1)
(i32.store)))
(; tuple destruction end ;)
(call $gc_increase_rc)
(get_local $val_yy_10)
(call $gc_decrease_rc)
(drop)
(get_local $val_xx_9)
(call $gc_decrease_rc)
(drop)
(get_local $fun_plot_6)
(call $gc_decrease_rc)
(drop)
(get_local $fun_m_4)
(call $gc_decrease_rc)
(drop)
(get_local $val_dt_5)
(call $gc_decrease_rc)
(drop)
(get_local $fun_andb_0)
(call $gc_decrease_rc)
(drop)
(get_local $tmp_fun_plot_6)
(call $gc_decrease_rc)
(drop))
(func $def_fun_f_13 (param $val_x_14 i32) (param $val_y_15 i32) (param $val_n_16 i32) (param $cls_fun_f_13 i32) (result i32)
(local $fun_f_13 i32)
(local $val_b_12 i32)
(local $val_a_11 i32)
(local $fun_isConv_2 i32)
(local $val_times_3 i32)
(local $tmp_fun_f_13 i32)
(local $stack_top_var i32)
(; tuple destruction ;)
(get_local $cls_fun_f_13)
(set_local $stack_top_var)
(get_local $stack_top_var)
(i32.const 0)
(i32.add)
(i32.load)
(set_local $tmp_fun_f_13)
(get_local $stack_top_var)
(i32.const 4)
(i32.add)
(i32.load)
(set_local $val_times_3)
(get_local $stack_top_var)
(i32.const 8)
(i32.add)
(i32.load)
(set_local $fun_isConv_2)
(get_local $stack_top_var)
(i32.const 12)
(i32.add)
(i32.load)
(set_local $val_a_11)
(get_local $stack_top_var)
(i32.const 16)
(i32.add)
(i32.load)
(set_local $val_b_12)
(; let ;)
(; tuple construction ;)
(i32.const 20)
(i32.const 0)
(call $gc_malloc)
(set_local $stack_top_var)
(get_local $stack_top_var)
(get_local $stack_top_var)
(get_local $stack_top_var)
(get_local $stack_top_var)
(get_local $stack_top_var)
(get_local $stack_top_var)
(i32.const 0)
(i32.add)
(i32.const 4)
(i32.const 1)
(call $gc_malloc)
(set_local $stack_top_var)
(get_local $stack_top_var)
(get_local $stack_top_var)
(i32.const 5)
(i32.store)
(call $gc_increase_rc)
(i32.store)
(i32.const 4)
(i32.add)
(get_local $val_times_3)
(call $gc_increase_rc)
(i32.store)
(i32.const 8)
(i32.add)
(get_local $fun_isConv_2)
(call $gc_increase_rc)
(i32.store)
(i32.const 12)
(i32.add)
(get_local $val_a_11)
(call $gc_increase_rc)
(i32.store)
(i32.const 16)
(i32.add)
(get_local $val_b_12)
(call $gc_increase_rc)
(i32.store)
(; tuple construction end ;)
(call $gc_increase_rc)
(set_local $fun_f_13)
(; if ;)
(; arith op ;)
(i32.const 4)
(i32.const 1)
(call $gc_malloc)
(set_local $stack_top_var)
(get_local $stack_top_var)
(get_local $stack_top_var)
(get_local $val_times_3)
(i32.load)
(get_local $val_n_16)
(i32.load)
(i32.lt_s)
(i32.store)
(; arith op end ;)
(i32.load)
(if (result i32)
(i32.eqz)
(then
(; arith op ;)
(i32.const 4)
(i32.const 1)
(call $gc_malloc)
(set_local $stack_top_var)
(get_local $stack_top_var)
(get_local $stack_top_var)
(; arith op ;)
(i32.const 4)
(i32.const 1)
(call $gc_malloc)
(set_local $stack_top_var)
(get_local $stack_top_var)
(get_local $stack_top_var)
(; arith op ;)
(i32.const 4)
(i32.const 1)
(call $gc_malloc)
(set_local $stack_top_var)
(get_local $stack_top_var)
(get_local $stack_top_var)
(get_local $val_x_14)
(f32.load)
(get_local $val_x_14)
(f32.load)
(f32.mul)
(f32.store)
(; arith op end ;)
(f32.load)
(; arith op ;)
(i32.const 4)
(i32.const 1)
(call $gc_malloc)
(set_local $stack_top_var)
(get_local $stack_top_var)
(get_local $stack_top_var)
(get_local $val_y_15)
(f32.load)
(get_local $val_y_15)
(f32.load)
(f32.mul)
(f32.store)
(; arith op end ;)
(f32.load)
(f32.sub)
(f32.store)
(; arith op end ;)
(f32.load)
(get_local $val_a_11)
(f32.load)
(f32.add)
(f32.store)
(; arith op end ;)
(; arith op ;)
(i32.const 4)
(i32.const 1)
(call $gc_malloc)
(set_local $stack_top_var)
(get_local $stack_top_var)
(get_local $stack_top_var)
(; arith op ;)
(i32.const 4)
(i32.const 1)
(call $gc_malloc)
(set_local $stack_top_var)
(get_local $stack_top_var)
(get_local $stack_top_var)
(; arith op ;)
(i32.const 4)
(i32.const 1)
(call $gc_malloc)
(set_local $stack_top_var)
(get_local $stack_top_var)
(get_local $stack_top_var)
(f32.const 2.0)
(get_local $val_x_14)
(f32.load)
(f32.mul)
(f32.store)
(; arith op end ;)
(f32.load)
(get_local $val_y_15)
(f32.load)
(f32.mul)
(f32.store)
(; arith op end ;)
(f32.load)
(get_local $val_b_12)
(f32.load)
(f32.add)
(f32.store)
(; arith op end ;)
(; arith op ;)
(i32.const 4)
(i32.const 1)
(call $gc_malloc)
(set_local $stack_top_var)
(get_local $stack_top_var)
(get_local $stack_top_var)
(get_local $val_n_16)
(i32.load)
(i32.const 1)
(i32.add)
(i32.store)
(; arith op end ;)
(get_local $fun_f_13)
(get_local $fun_f_13)
(i32.load)
(i32.load)
(call_indirect (param i32) (param i32) (param i32) (param i32) (result i32)))
(else
(get_local $val_x_14)
(get_local $val_y_15)
(get_local $fun_isConv_2)
(get_local $fun_isConv_2)
(i32.load)
(i32.load)
(call_indirect (param i32) (param i32) (param i32) (result i32))))
(; tuple destruction end ;)
(call $gc_increase_rc)
(get_local $fun_f_13)
(call $gc_decrease_rc)
(drop)
(get_local $val_b_12)
(call $gc_decrease_rc)
(drop)
(get_local $val_a_11)
(call $gc_decrease_rc)
(drop)
(get_local $fun_isConv_2)
(call $gc_decrease_rc)
(drop)
(get_local $val_times_3)
(call $gc_decrease_rc)
(drop)
(get_local $tmp_fun_f_13)
(call $gc_decrease_rc)
(drop))
(elem (i32.const 0) $def_fun_andb_0 $def_fun_abs_1 $def_fun_isConv_2 $def_fun_m_4 $def_fun_plot_6 $def_fun_f_13)
(func (export "main") (result i32)
(local $fun_plot_6 i32)
(local $val_dt_5 i32)
(local $fun_m_4 i32)
(local $val_times_3 i32)
(local $fun_isConv_2 i32)
(local $fun_abs_1 i32)
(local $fun_andb_0 i32)
(local $stack_top_var i32)
(call $gc_initalize)
(; let ;)
(; tuple construction ;)
(i32.const 4)
(i32.const 0)
(call $gc_malloc)
(set_local $stack_top_var)
(get_local $stack_top_var)
(get_local $stack_top_var)
(i32.const 0)
(i32.add)
(i32.const 4)
(i32.const 1)
(call $gc_malloc)
(set_local $stack_top_var)
(get_local $stack_top_var)
(get_local $stack_top_var)
(i32.const 0)
(i32.store)
(call $gc_increase_rc)
(i32.store)
(; tuple construction end ;)
(call $gc_increase_rc)
(set_local $fun_andb_0)
(; let ;)
(; tuple construction ;)
(i32.const 4)
(i32.const 0)
(call $gc_malloc)
(set_local $stack_top_var)
(get_local $stack_top_var)
(get_local $stack_top_var)
(i32.const 0)
(i32.add)
(i32.const 4)
(i32.const 1)
(call $gc_malloc)
(set_local $stack_top_var)
(get_local $stack_top_var)
(get_local $stack_top_var)
(i32.const 1)
(i32.store)
(call $gc_increase_rc)
(i32.store)
(; tuple construction end ;)
(call $gc_increase_rc)
(set_local $fun_abs_1)
(; let ;)
(; tuple construction ;)
(i32.const 16)
(i32.const 0)
(call $gc_malloc)
(set_local $stack_top_var)
(get_local $stack_top_var)
(get_local $stack_top_var)
(get_local $stack_top_var)
(get_local $stack_top_var)
(get_local $stack_top_var)
(i32.const 0)
(i32.add)
(i32.const 4)
(i32.const 1)
(call $gc_malloc)
(set_local $stack_top_var)
(get_local $stack_top_var)
(get_local $stack_top_var)
(i32.const 2)
(i32.store)
(call $gc_increase_rc)
(i32.store)
(i32.const 4)
(i32.add)
(get_local $fun_andb_0)
(call $gc_increase_rc)
(i32.store)
(i32.const 8)
(i32.add)
(get_local $fun_abs_1)
(call $gc_increase_rc)
(i32.store)
(i32.const 12)
(i32.add)
(get_local $fun_abs_1)
(call $gc_increase_rc)
(i32.store)
(; tuple construction end ;)
(call $gc_increase_rc)
(set_local $fun_isConv_2)
(; let ;)
(i32.const 4)
(i32.const 1)
(call $gc_malloc)
(set_local $stack_top_var)
(get_local $stack_top_var)
(get_local $stack_top_var)
(i32.const 100)
(i32.store)
(call $gc_increase_rc)
(set_local $val_times_3)
(; let ;)
(; tuple construction ;)
(i32.const 12)
(i32.const 0)
(call $gc_malloc)
(set_local $stack_top_var)
(get_local $stack_top_var)
(get_local $stack_top_var)
(get_local $stack_top_var)
(get_local $stack_top_var)
(i32.const 0)
(i32.add)
(i32.const 4)
(i32.const 1)
(call $gc_malloc)
(set_local $stack_top_var)
(get_local $stack_top_var)
(get_local $stack_top_var)
(i32.const 3)
(i32.store)
(call $gc_increase_rc)
(i32.store)
(i32.const 4)
(i32.add)
(get_local $val_times_3)
(call $gc_increase_rc)
(i32.store)
(i32.const 8)
(i32.add)
(get_local $fun_isConv_2)
(call $gc_increase_rc)
(i32.store)
(; tuple construction end ;)
(call $gc_increase_rc)
(set_local $fun_m_4)
(; let ;)
(i32.const 4)
(i32.const 1)
(call $gc_malloc)
(set_local $stack_top_var)
(get_local $stack_top_var)
(get_local $stack_top_var)
(f32.const 4.0e-2)
(f32.store)
(call $gc_increase_rc)
(set_local $val_dt_5)
(; let ;)
(; tuple construction ;)
(i32.const 20)
(i32.const 0)
(call $gc_malloc)
(set_local $stack_top_var)
(get_local $stack_top_var)
(get_local $stack_top_var)
(get_local $stack_top_var)
(get_local $stack_top_var)
(get_local $stack_top_var)
(get_local $stack_top_var)
(i32.const 0)
(i32.add)
(i32.const 4)
(i32.const 1)
(call $gc_malloc)
(set_local $stack_top_var)
(get_local $stack_top_var)
(get_local $stack_top_var)
(i32.const 4)
(i32.store)
(call $gc_increase_rc)
(i32.store)
(i32.const 4)
(i32.add)
(get_local $fun_andb_0)
(call $gc_increase_rc)
(i32.store)
(i32.const 8)
(i32.add)
(get_local $val_dt_5)
(call $gc_increase_rc)
(i32.store)
(i32.const 12)
(i32.add)
(get_local $val_dt_5)
(call $gc_increase_rc)
(i32.store)
(i32.const 16)
(i32.add)
(get_local $fun_m_4)
(call $gc_increase_rc)
(i32.store)
(; tuple construction end ;)
(call $gc_increase_rc)
(set_local $fun_plot_6)
(i32.const 4)
(i32.const 1)
(call $gc_malloc)
(set_local $stack_top_var)
(get_local $stack_top_var)
(get_local $stack_top_var)
(f32.const -1.0)
(f32.store)
(i32.const 4)
(i32.const 1)
(call $gc_malloc)
(set_local $stack_top_var)
(get_local $stack_top_var)
(get_local $stack_top_var)
(f32.const -1.0)
(f32.store)
(get_local $fun_plot_6)
(get_local $fun_plot_6)
(i32.load)
(i32.load)
(call_indirect (param i32) (param i32) (param i32) (result i32))))
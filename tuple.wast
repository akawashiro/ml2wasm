(module
(memory 10)
(func (export "main") (result i32)
(local $val_y_4 i32)
(local $val_x_3 i32)
(local $val__tmp_0_0 i32)
(local $val__tmp_2_2 i32)
(local $val__tmp_1_1 i32)
(local $stack_top_var i32)
(i32.const
10)
(set_local $val__tmp_1_1)
(i32.const
20)
(set_local $val__tmp_2_2)
(i32.const
0)
(i32.load)
(i32.const
4)
(i32.add)
(i32.const
0)
(i32.const
0)
(i32.load)
(i32.const
4)
(i32.add)
(i32.store)
(i32.const
0)
(i32.load)
(get_local $val__tmp_1_1)
(i32.store)
(i32.const
0)
(i32.const
0)
(i32.load)
(i32.const
4)
(i32.add)
(i32.store)
(i32.const
0)
(i32.load)
(get_local $val__tmp_2_2)
(i32.store)
(set_local $val__tmp_0_0)
(get_local $val__tmp_0_0)
(set_local $stack_top_var)
(get_local $stack_top_var)
(i32.const
0)
(i32.add)
(i32.load)
(set_local $val_x_3)
(get_local $stack_top_var)
(i32.const
4)
(i32.add)
(i32.load)
(set_local $val_y_4)
(get_local $val_y_4)))

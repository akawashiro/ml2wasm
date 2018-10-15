(module
 (func (export "local") (result i32)
   (i32.const 0)
   (i32.const 0)
   (i32.add)
  (if (result i32)
   (i32.eqz)
   (then
    (i32.const 12)
   )
   (else
    (i32.const 30)
   )
  )
 )
 )


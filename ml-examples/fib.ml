let rec fib n = 
  if n < 3 
    then 1
    else 
        let a = fib (n - 1) in
        let b = fib (n - 2) in
        a + b in
let rec print x =
  if 10 < x
    then 1
    else
      let a = fib x in
      print_i32 a;
      print (x+1) in
print 0

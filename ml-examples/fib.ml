let rec fib n = 
  if n < 3 
    then 1
    else fib (n - 1) + fib (n - 2) in
let rec print x =
  if 5 < x
    then 1
    else
      let a = fib x in
      print_i32 a;
      print (x+1) in
print 0

let rec fib n = 
  if n < 3 
    then 1.0
    else fib (n - 1) +. fib (n - 2) in
let rec print x =
  if 10 < x
    then 1
    else
      let a = fib x in
      print_f32 a;
      print (x+1) in
print 0


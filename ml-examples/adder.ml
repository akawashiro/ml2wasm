let rec make_adder x =
  let rec adder y = x + y in
  adder in
let r = (make_adder 3) 7 in
print_i32 r; 0

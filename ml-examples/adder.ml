let rec make_adder x =
  let rec adder y = x + y in
  adder in
let r = (make_adder 7) in
let y = r 3 in
print_i32 y;
10

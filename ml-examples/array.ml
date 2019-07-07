let rec f y =
let a = make_array 10 in
set_array a 0 y;
let r = get_array a 0 in
r in
let r = f 1 in 
print_i32 r;
0


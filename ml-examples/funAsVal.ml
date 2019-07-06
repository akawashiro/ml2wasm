let rec f x y = x + y in 
let rec g z = z 1 2 in 
let r = g f in
print_i32 r; r

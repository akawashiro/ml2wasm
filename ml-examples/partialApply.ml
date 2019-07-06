(* We dont support partial application, yet *)
let rec f x y = x + y in
let g = f 1 in 
g 10

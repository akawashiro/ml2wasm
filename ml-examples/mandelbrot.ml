let rec andb b1 b2 =
  if b1
  then if b2
    then true
    else false
  else false in

let rec abs f =
  if 0.0 <. f
  then f
  else -1.0 *. f in

let rec isConv x y =
  if (andb ((abs x) <. 1.0) ((abs y) <. 1.0))
  then true
  else false in

let times = 100 in

let rec m a b =
  let rec f x y n =
    if times < n
    then isConv x y
    else f (x *.x -. y*.y +. a) (2.0 *. x *. y +. b) (n + 1) in
  f a b 0 in

let dt = 0.04 in
let rec plot x y =
  if (andb (1.0 <. x) (1.0 <. y))
  then 
    true
  else
    let xx = if 1.0 <. x then -1.0 else x +. dt in
    let yy = if 1.0 <. x then y +. dt else y in
    if (m x y)
    then
      print_f32 x;
      print_f32 y;
      plot xx yy
    else
      plot xx yy in
plot -1.0 -1.0

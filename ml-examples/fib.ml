let rec f n = 
  if n < 3 then 1
  else f (n - 1) + f (n - 2)
in f 11;;


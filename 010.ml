

open Core.Std

(* not a perfect solution *)
let is_prime n =
  let e = Int.of_float(sqrt(Float.of_int(n))) in
  let rec iter k =
    if k = 1 then
      true
    else
      if n mod k = 0 then
        false
      else
        iter (k - 1) in
  iter e


let sum_prime n =
  let s = ref 0 in 
  for x = 2 to n do
    if is_prime x then
      s := !s + x;
  done;
  !s
   
    

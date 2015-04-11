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
                     
let find_prime n =
  let k = ref 0 in
  let idx = ref 2 in 
  let ans = ref 2 in
  let finish = ref false in 
  while !finish = false do
    if is_prime !idx then
      incr k;
    if !k = n then begin
        ans := !idx;
        finish := true;
      end;
    incr idx;
  done;
  !ans

let () =
  assert(find_prime 10001 = 104743);
  
            

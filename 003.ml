open Core.Std;;
  
let largest_prime_factor n =
  let factors = ref [] in
  let v = ref n in
  while (!v mod 2) = 0 do
    factors := 2::!factors;
    v := !v / 2
  done;

  let i = ref 3 in
  while !i <= Int.of_float(sqrt(Float.of_int(!v))) do 
    while (!v mod !i) = 0 do
      factors := !i::!factors;
      v := !v / !i;
    done;
    i := !i + 2;
  done;

  if !v > 2 then
    factors := !v::!factors;
  match !factors with
    x::tl -> x
  | [x] -> x
  | _ -> assert(false)
  

let () =
  assert(largest_prime_factor(13195) = 29);
  assert(largest_prime_factor(600851475143) = 6857);
                

                
            
    
    
    

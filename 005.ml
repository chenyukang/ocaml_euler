

(* Not a perfect solution *)

let smallest_multiple l h =
  let bound = ref 1 in 
  for x = l to h do
    bound := !bound * x;
  done;

  let rec is_good v n =
    if n = l then true
    else
      if v mod n = 0 then
        (is_good v (n - 1))
      else
        false in

  let check = ref true in 
  let res = ref l in
  let idx = ref l in
  while (!idx <= !bound) && !check do
    if (is_good !idx h) then begin 
      check := false;
      res := !idx
      end;
    idx := !idx + 1;
  done;
  !res

   
let () =
  assert(smallest_multiple 1 10 = 2520);
  assert(smallest_multiple 1 20 = 232792560);
  

    

    
                    
                      
                  

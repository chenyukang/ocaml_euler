
let solve w h =
  let r = ref 1 in
  let m = ref 1 in 
  for i = w+1 to (w + h) do
    r := !r *  i;
    if !m <= h && (!r mod !m) = 0 then begin 
      r := !r / !m;
      incr m;
      end;
  done;
  !r

let () =
  assert((solve 20 20) = 137846528820);
                 




let solve n =
  let rec iter k r =
    if k = n then r
    else
      if (k % 3) = 0 || (k % 5) = 0 then
        iter (k + 1) (r + k) else
        iter (k + 1) r in 
  iter 1 0;;


let () =
  assert(solve 10 = 23);
  assert(solve 1000 = 233168);


let solve n =
  let s1 = ref 0 in
  let s2 = ref 0 in 
  for x = 1 to n do
    s1 := !s1 + (x * x);
  done;

  for x = 1 to n do
    s2 := !s2 + x;
  done;
  s2 := !s2 * !s2;
  !s2 - !s1
         

let () =
  assert(solve 10 = 2640);
  assert(solve 100 = 25164150);
  

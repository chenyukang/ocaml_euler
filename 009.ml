open Core.Std

let generate n =
  let res = ref [] in 
  for a = 1 to n do
    for b = a + 1 to n do
      for c = b + 1 to n do
        if a + b + c = n &&
             a*a + b*b = c*c then
          res := [a; b; c] :: !res;
      done;
    done;
  done;
  !res

let solve n =
  let res = generate n in  
  match res with
    hd :: tl -> (let a = Array.of_list hd in
                 let a1 = Array.get a 0 in
                 let a2 = Array.get a 1 in
                 let a3 = Array.get a 2 in
                 a1 * a2 * a3)
  | _ -> failwith "Not found"
                  
                  

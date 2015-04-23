open Core.Std
open Batteries

let solve n =
  let table = Array.make (2 * n + 1) 0 in
  Array.set table 89 1;
  Array.set table 1  (-1);
  let trans n =
    let digits = Int.to_string n in
    String.fold_left (fun a c ->
                      let x = Int.of_string (String.of_char c) in
                      a + (x * x))
                     0
                     digits in
  let rec it now =
    match Array.get table now with
      1 -> true
    | 0 -> if (it (trans now)) then begin
               Array.set table now 1;
               true
             end
           else begin
               Array.set table now (-1);
               false
             end
    | _ -> false in
  let res = ref 0 in
  for x = 1 to n do
    if it x then
      incr res;
  done;
  !res

let () =
  assert(solve 10000000 = 8581146);;

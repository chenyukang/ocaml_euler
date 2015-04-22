open Core.Std
open Batteries

let is_palid s =
  let len = String.length s in
  let rec iter l r =
    if l >= r then true
    else
      (String.get s l) = (String.get s r) &&
        (iter (l + 1) (r - 1)) in
  iter 0 (len - 1);;

let binary_str n =
  let rec it v now =
    match v with
    | 0 -> '0'::now
    | 1 -> '1'::now
    | _ -> if (v mod 2) = 1 then
             it (v/2) ('1'::now)
           else
             it (v/2) ('0'::now) in
  let a = it n [] in
  String.of_list a;;

let solve n =
  let range = List.of_enum (1--n) in
  let check n =
    if (n mod 2) = 0 then false
    else
      let s = Printf.sprintf "%d" n in
      let b = binary_str n in
      (is_palid s) && (is_palid b) in
  let valid = List.filter check range in
  List.sum valid;;

let () =
  assert(solve 1000000 = 872187);;

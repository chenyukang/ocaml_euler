open Core.Std
open Big_int
open Batteries

let solve a b =
  let res = ref [] in
  let rec find l v =
    match l with
    | [x] -> Big_int.eq_big_int x v
    | a::tl -> (Big_int.eq_big_int a v) || (find tl v)
    | _ -> false in
  for x = a to b do
    for y = a to b do
      let v = Big_int.power_big_int_positive_int (Big_int.big_int_of_int x) y in
      if (find !res v) = false then
        res := v::!res;
    done;
  done;
  List.length !res;;

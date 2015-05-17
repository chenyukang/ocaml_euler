open Core.Std
open Batteries

let solve n =
  let res = ref (Big_int.of_int 0) in
  let max = Big_int.of_int 10000000000 in
  for i = 1 to n do
    res := Big_int.add !res (Big_int.pow (Big_int.of_int i) (Big_int.of_int i));
    res := Big_int.modulo !res max;
  done;
  Big_int.to_int !res

let () =
  assert(solve 1000 = 9110846700);

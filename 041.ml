open Core.Std
open Batteries

let make_primes n =
  let table = Array.make (n+1) true in
  for i = 2 to n do
    if Array.get table i then
      let k = ref 2 in
      while (i * (!k)) <= n do
        Array.set table (i * (!k)) false;
        incr k;
      done;
  done;
  table;;


(* 1 + 2 + ... + 9 = 45 (dividable by 9 => 9-pandigimal number is dividable by 9) *)
(* 1 + 2 + ... + 8 = 36 (dividable by 9 => 9-pandigimal number is dividable by 9) *)

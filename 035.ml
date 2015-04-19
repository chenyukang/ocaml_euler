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

let rotate n =
  let s = String.of_int n in
  let r = ref [] in
  for i = 0 to (String.length s) - 1 do
    r := ((String.tail s i) ^ (String.sub s 0 i)):: !r;
  done;
  List.map (fun x -> Int.of_string x)
           (List.rev (!r))

let solve n =
  let primes = make_primes n in
  let numbers = List.of_enum (2--n) in
  let is_prime n =
    Array.get primes n in
  let res = List.map (fun x ->
                      let rotates = rotate x in
                      let rec it now =
                        match now with
                        | [x] -> is_prime x
                        | x::tl -> if is_prime x then it tl
                                   else false
                        | _ -> true in
                      if it rotates then 1 else 0)
                     numbers in
  List.sum res

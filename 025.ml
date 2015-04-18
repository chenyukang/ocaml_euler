
open Core.Std
open Big_int

let solve n =
  let a = ref (Big_int.big_int_of_int 1) in
  let b = ref (Big_int.big_int_of_int 1) in
  let res = ref 3 in
  let loop = ref true in
  while !loop do
    let c = Big_int.add_big_int !a !b in
    if (String.length (Big_int.string_of_big_int c)) >= n then
      loop := false
    else
      begin
        incr res;
        a := !b;
        b := c;
      end;
  done;
  !res

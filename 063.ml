open Core.Std
open Big_int

let rec iter_test value pow cnt =
  let pow_value = Big_int.power_int_positive_int value pow in
  let bits = String.length (Big_int.string_of_big_int pow_value) in
  if bits <> cnt then
    cnt - 1
  else
    iter_test value (pow + 1) (cnt + 1)

let solve () =
  let res = ref 1 in
  for i = 2 to 9 do
    let cnt = (iter_test i 1 1) in begin
        res := !res + cnt;
        Printf.printf "now: %d %d\n" !res i;
      end;
  done;
  !res

let () =
  assert(solve() = 49);

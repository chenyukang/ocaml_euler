open Core.Std
open Big_int

let solve n =
  let v = ref (Big_int.big_int_of_int 1) in
  for x = 1 to n do
    v := Big_int.mult_big_int !v (Big_int.big_int_of_int x);
  done;
  let s = Big_int.string_of_big_int !v in
  String.fold s ~init:0 ~f: (fun s x ->
                             s + (Int.of_string (String.of_char x)))

let () =
  assert(solve 100 = 648);

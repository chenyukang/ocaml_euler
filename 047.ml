open Core.Std;;

let rec remove_duplicate l =
  match l with
  | [] -> []
  | x :: [] -> [x]
  | x :: y :: rest ->
     if x = y then remove_duplicate (y::rest)
     else x :: remove_duplicate (y::rest)

let factors_count n =
  let factors = ref [] in
  let v = ref n in
  while (!v mod 2) = 0 do
    factors := 2::!factors;
    v := !v / 2
  done;

  let i = ref 3 in
  while !i <= Int.of_float(sqrt(Float.of_int(!v))) do
    while (!v mod !i) = 0 do
      factors := !i::!factors;
      v := !v / !i;
    done;
    i := !i + 2;
  done;

  if !v > 2 then
    factors := !v::!factors;
  List.count (remove_duplicate !factors) (fun x -> true)

let solve n =
  let rec it k num =
    if num = n then true
    else
      if factors_count k <> n then false
      else
        it (k+1) (num+1) in
  let rec test i =
    if it i 0 then i
    else test (i+1) in
  test 1


let () =
  assert(solve 2 = 14);
  assert(solve 3 = 644);
  assert(solve 4 = 134043);

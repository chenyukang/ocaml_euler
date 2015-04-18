open Core.Std

let under_10 n =
  match n with
    0 -> "zero"
  | 1 -> "one"
  | 2 -> "two"
  | 3 -> "three"
  | 4 -> "four"
  | 5 -> "five"
  | 6 -> "six"
  | 7 -> "seven"
  | 8 -> "eight"
  | 9 -> "nine"
  | _ -> failwith "error < 10"

let under_20 n =
  if n < 10 then
    under_10 n
  else
    match n with
      10 -> "ten"
    | 11 -> "eleven"
    | 12 -> "twelve"
    | 13 -> "thirteen"
    | 14 -> "fourteen"
    | 15 -> "fifteen"
    | 16 -> "sixteen"
    | 17 -> "seventeen"
    | 18 -> "eighteen"
    | 19 -> "nineteen"
    | _ -> failwith "error < 20"

let under_100 n =
  if n < 20 then
    (under_20 n)
  else
    let prefix =
      match (n/10) with
        2 -> "twenty"
      | 3 -> "thirty"
      | 4 -> "forty"
      | 5 -> "fifty"
      | 6 -> "sixty"
      | 7 -> "seventy"
      | 8 -> "eighty"
      | 9 -> "ninety"
      | _ -> failwith "error < 100" in
    if (n mod 10) <> 0 then
      Printf.sprintf "%s-%s" prefix (under_10 (n mod 10))
    else
      prefix


let under_1000 n =
  if n < 100 then
    under_100 n
  else
    let prefix = Printf.sprintf "%s hundred" (under_10 (n / 100)) in
    if (n mod 100) <> 0 then
      Printf.sprintf "%s and %s" prefix (under_100 (n mod 100))
    else
      prefix

let to_word n =
  if n < 1000 then
    under_1000 n
  else
    "one thousand"

let length n =
  let filter s =
    String.filter s (fun x -> x <> '-' && x <> ' ') in
  String.length (filter (to_word n))

let solve n =
  let res = ref 0 in
  for x = 1 to n do
    res := !res + (length x);
  done;
  !res

let () =
  assert((solve 1000) = 21124);

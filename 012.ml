let factors n =
  let rec aux d n =
    if n = 1 then [] else
      if n mod d = 0 then
        match aux d (n / d) with
        | (h,n) :: t when h = d -> (h,n+1) :: t
        | l -> (d,1) :: l
      else aux (d+1) n
  in
  aux 2 n;;

let factors_num n =
  let fa = factors n in
  let rec it f acc =
    match f with
    | (_, cnt) :: tl -> it tl (acc * (cnt + 1))
    | [(_, cnt)] -> acc * (cnt + 1)
    | [] -> acc in
  it fa 1
     
let solve n =
  let ans = ref 0 in 
  let i = ref 1 in
  let loop = ref true in
  while !loop do
    let s = (!i * (!i + 1)) / 2 in
    let f = factors_num s in
    if f >= n then begin 
        ans := s;
        loop := false;
      end;
    incr i;
  done;
  !ans;;

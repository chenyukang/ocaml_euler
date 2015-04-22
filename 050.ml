
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
  let res = ref [] in
  Array.iteri (fun i a -> if i > 1 && a then res := i::!res) table;
  Array.of_list  (List.rev !res)

let get_longer primes now min =
  let max_avg = if min = 0 then now else now / min in
  let sum = Array.sum (Array.head primes min) in
  let rec it sum h t =
    let l = t - h in
    if sum > (max_avg * l) then None
    else
      match (Int.compare sum now) with
      | 0 -> if l <= min then None
             else Some(now, l)
      | -1 -> it (sum + (Array.get primes t)) h (t + 1)
      | _ -> it (sum - (Array.get primes h)) (h + 1) t in
  it sum 0 min

let solve n =
  let primes = make_primes n in
  let num = ref 0 in
  let len = ref 1 in
  Array.iter (fun x ->
              let r = get_longer primes x !len in
              match r with
              Some(n, l) -> num := n; len := l;
              | _ -> () )
             primes;
  Some(!num, !len);;

let () =
  assert(solve 1000000 = Some (997651, 543));;

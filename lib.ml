type 'a stream_t = Nil | Cons of 'a * (unit -> 'a stream_t)

let hd = function
  | Nil -> failwith "hd"
  | Cons (v, _) -> v

let tl = function
  | Nil -> failwith "tl"
  | Cons (_, g) -> g()

let rec take n = function
  | Nil -> []
  | Cons (_, _)  when n = 0 -> []
  | Cons (hd, g) -> hd::take (n-1) (g())
                             
let rec filter f = function
  | Nil -> Nil
  | Cons (hd, g) ->
     if f hd then Cons (hd, fun() -> filter f (g()))
     else filter f (g())
                 
let rec from i = Cons (i, fun() -> from (i+1))

(* delete multiples of p from a stream *)
let sift p = filter (fun n -> n mod p <> 0)

(* sieve of Eratosthenes *)
let rec sieve = function
  | Nil -> Nil
  | Cons (p, g) -> 
     let next = sift p (g()) in
     Cons (p, fun () -> sieve next)

(* primes *)
let primes = sieve (from 2)
                   
let factors n =
  let res = ref 1 in 
  let primes = sieve (from 2) in
  let rec it p =
    match p with
    | Cons(p, g) -> begin
        if p > n then
          1
        else begin
            let a = ref 0 in
            let v = ref n in 
            while !v > 0 && !v mod p = 0 do
              incr a;
              v := !v / p;
            done;
            res := !res * (!a+1);
            it (g());
          end;
      end;
    | _ -> failwith "error in primes" in
  let _ = it primes in 
  !res
   

let pps = take 100 primes;;
  
let factors n limit =
  let res = ref 1 in
  let rec it p =
    match p with
      hd::tl -> begin
        if hd > n then 1
        else begin
            let a = ref 0 in
            let v = ref n in
            while !v > 0 && !v mod hd = 0 do
              incr a;
              v := !v / hd;
            done;
            res := !res * (!a + 1);
            if !res >= limit then
              !res
            else
              it tl;
          end;
      end;
    | _ -> failwith "need more primes" in
  let _ = it pps in
  !res
      
let solve n =
  let ans = ref 0 in 
  let i = ref 1 in
  let loop = ref true in
  while !loop do
    let s = (!i * (!i + 1)) / 2 in
    let f = factors s n in
    Printf.printf "now: %d\n" f;
    if f >= n then begin 
        ans := s;
        loop := false;
      end;
    incr i;
  done;
  !ans;;
  
  (* let res = solve 200 in *)
  (*     Printf.printf "res: %d" res;; *)


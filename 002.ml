
type 'a stream = Nil
               | Cons of 'a * (unit -> 'a stream)
               | Thre of 'a * 'a * (unit -> 'a stream)
                                      
let rec from (a: int) (b: int) : int stream =
  Thre(a, b, fun() -> (from (a+b) (a+b+b)))

let hd(s : 'a stream) : 'a =
  match s with
    Nil -> failwith "hd"
  | Cons(x, _) -> x
  | Thre(x, _, _) -> x

let tl(s : 'a stream) : 'a stream =
  match s with
    Nil -> failwith "tl"
  | Cons(_, y) -> y()
  | Thre(_, y, g) -> Cons(y, g)
                        
let rec take (s : 'a stream) (n : int) : 'a list =
  if n <= 0 then
    []
  else match s with
         Nil -> []
       | Cons(x, y) -> x :: take (y()) (n - 1)
       | Thre(x, y, g) -> x :: take (Cons(y, g)) (n - 1)
                                    
let rec sum (s : 'a stream): 'a =
  match s with
  | Nil -> 0
  | Cons(x, y) -> x + sum(y())
  | Thre(x, y, g) ->
     x + y + sum(g())
                
let rec bound (f : 'a -> bool) (s : 'a stream) : 'a stream =
  match s with
    Nil -> Nil
  | Cons(x, g) ->
     if f x then Cons(x, fun() -> bound f (g()))
     else
       Nil
  | Thre(x, y, g) when (f x) = false ->
     Nil
  | Thre(x, y, g) when (f y) = false ->
     Cons(x, fun() -> Nil)
  | Thre(x, y, g)  ->
        Thre(x, y, fun() ->
                   (bound f (g())))
               
            
let rec filter (f : 'a -> bool) (s : 'a stream) : 'a stream =
  match s with
    Nil -> Nil
  | Cons (x, g) when (f x) ->
     Cons(x, fun() -> filter f (g()))
  | Cons (_, g) -> filter f (g())
  | Thre(x, y, g) when (f x) && (f y)  ->
     Thre(x, y,
          fun() ->
          (filter f (g())))
  | Thre(x, y, g) when (f x) ->
     Cons(x, fun() -> (filter f (g())))
  | Thre(x, y, g) when (f y) -> 
     Cons(y, fun() -> (filter f (g())))
  | Thre(_, _, g) ->
     filter f (g())
                    
         
let compute b =
  let fib = from 1 2 in
  let check n =
    (n mod 2) = 0 in
  let b = bound (fun x -> x <= b) fib in 
  let r = filter check b in
  sum r;;

let () =
  assert(compute(90) = 44);
  assert(compute(4000000) = 4613732);
  

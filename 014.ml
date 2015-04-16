open Core.Std

let solve() =
  let res = ref 0 in
  let start = ref 1 in 
  let table = Hashtbl.create ~hashable:Int.hashable() in 
  let rec iter v n  =
    let m = Hashtbl.find table v in
    match m with
      Some(x) -> (n + x - 1)
    | None ->
      begin 
        if v = 1 then n else
          if (v mod 2) = 0 then
            iter (v/2) (n+1)
          else
            iter (3*v + 1) (n+1)
      end in
  for v = 1 to 1000000 do
    let r = iter v 1 in
    ignore(Hashtbl.add table v r);
    if r > !res then begin 
        res := r;
        start := v;
      end
  done;
  !start

let () =
  assert(solve() = 837799);
    
               
    

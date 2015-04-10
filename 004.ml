open Core.Std
       
let is_palindrome x =
  let r = ref [] in
  let rec step n =
    if n < 10 then
      r := n :: !r
    else
      begin
        r := (n mod 10):: !r;
        step (n/10);
      end in
  step x;
  !r = List.rev !r

let largest_palindrome l h =
  let ans = ref 0 in 
  for i = h downto l do
    for j = h downto l do
      if i * j > !ans && is_palindrome(i * j) then
        ans := i * j;
    done;
  done;
  !ans
   
let () =
  assert((largest_palindrome 10 99) = 9009);
  assert((largest_palindrome 100 999) = 906609);
    


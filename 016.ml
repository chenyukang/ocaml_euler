open Big_int      
open Core.Std
       
let solve exp =
  let base = Big_int.big_int_of_int 2 in
  let value = Big_int.power_big_int_positive_int base exp in
  let str = Big_int.string_of_big_int value in
  let trans c =
    Int.of_string (String.of_char c) in 
  let rec sum_digits res now =
    if now = String.length str
    then res
    else
      let digit = String.get str now in
      sum_digits (res + (trans digit)) (now + 1) in
  sum_digits 0 0
             
let () =
  assert((solve 15) = 26);
  assert((solve 1000) = 1366);
  

  

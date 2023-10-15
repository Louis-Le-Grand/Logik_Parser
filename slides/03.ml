#use "02.ml";;

(*Splits stings into a list of chars
  Input: Str
  Output: Char List whit all charkters of the string*)
let explode str =
  let rec exp a b =
    if a < 0 then b
    else exp (a - 1) (str.[a] :: b)
  in
  exp (String.length str - 1) [];;


(*Splits stings into a list of singel charater Strings
  Input: Str
  Output: Str List whit all charkters of the string*)
let explode_str str = 
  let rec exp a b =
    if a < 0 then b
    else let a' = Char.escaped str.[a] in exp (a - 1) ( a' :: b)
  in
  exp (String.length str - 1) [];;
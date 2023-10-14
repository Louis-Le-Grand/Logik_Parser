(*TODO: Check if include is nessesary*)
#use "A-expression.ml";;

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


(*Checks if a chars is in a string
  Input: Str Char
  Output: bool if char in string*)  
let matches s = let chars = explode s in fun c -> List.mem c chars;;  


(*TODO: Überartbeite handhabung '`' ISO-Latin1*)
(*Checks if a 'char' is of type: space, punctuation, symbolic, numeric, alphanumeric
  Input: char
  Output: bool if char is type*)  
let space = matches " \t\n\r"
  and punctuation = matches "()[]{},"
  and symbolic = matches "~‘!@#$%^&*-+=|\\:;<>.?/"
  and numeric = matches "0123456789"
  and alphanumeric = matches
    "abcdefghijklmnopqrstuvwxyz_’ABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789";;


(*TODO: Try to get rit of this mess*)
(*Checks if the first symbol of a str is of type: space, punctuation, symbolic, numeric, alphanumeric
  Input: str
  Output: bool if char is type*)  
let space_str inp = matches " \t\n\r" inp.[0]
  and punctuation_str inp = matches "()[]{},;" inp.[0]
  and symbolic_str inp = matches "~‘!@#$%^&*-+=|\\:;<>.?/" inp.[0]
  and numeric_str inp = matches "0123456789" inp.[0]
  and alphanumeric_str inp = matches
    "abcdefghijklmnopqrstuvwxyz_’ABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789" inp.[0];;


(*split a string in in too the beginnig and rest. Wher beging is the longest substig of type*)
let rec lexwhile prop inp =
  match inp with
    c::cs when prop c -> let tok,rest = lexwhile prop cs in c^tok,rest
  | _ -> "",inp;;


(*Splits input by type usung lexwhile*)
let rec lex inp =
  match snd(lexwhile space_str inp) with
    [] -> []
  | c::cs -> let prop = if alphanumeric_str(c) then alphanumeric_str
                        else if symbolic_str(c) then symbolic_str
                        else fun c -> false in
              let toktl,rest = lexwhile prop cs in
              (c^toktl)::lex rest;;


(*Splits exprssion rekursiv first Disjunktion in Konjunktion in Atoms*)
let rec parse_expression i =
    match parse_product i with
      e1,"+"::i1 -> let e2,i2 = parse_expression i1 in Add(e1,e2),i2
    | e1,i1 -> e1,i1

  and parse_product i =
    match parse_atom i with
      e1,"*"::i1 -> let e2,i2 = parse_product i1 in Mul(e1,e2),i2
    | e1,i1 -> e1,i1

  and parse_atom i =
    match i with
      [] -> failwith "Expected an expression at end of input"
    | "("::i1 -> (match parse_expression i1 with
                    e2,")"::i2 -> e2,i2
                  | _ -> failwith "Expected closing bracket")
    | tok::i1 -> if List.for_all numeric (explode tok)
                  then Const(int_of_string tok),i1
                  else Var(tok),i1;;


(*Builds a parser with an parser function and lex to split the strings*)
let make_parser pfn s =
  let expr,rest = pfn (lex(explode_str s)) in
    if rest = [] then expr else failwith "Unparsed input";;
# use "05.ml";;

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


let make_parser pfn s =
  let expr,rest = pfn (lex(explode_str s)) in
    if rest = [] then expr else failwith "Unparsed input";;

 
(*Bsp*)
let default_parser = make_parser parse_expression;


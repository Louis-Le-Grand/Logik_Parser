(* If you want to use the code in the Terminal you need 
    #use "Übung_src.ml"
  and:
    #use "Übungc.ml"
*)
#use "Übung_src.ml";; 


(*
  Exercise 1: Complete the following function to parse simple arithmetic expressions 
  with Subtraction. The parser and printer should be able to handle expressions like:
  (3 - 4) * 5 + 6 * 7
  (3 - 4 + 5) * 6 + 7
  Be careful to make subtraction associate to the left: 
  x−y−z is understood as (x−y)−z not x−(y−z).
*)

type exp = 
    Var of string
  | Const of int
  | Add of exp * exp
  | Sub of exp * exp (*Subtraction*)
  | Mul of exp * exp;;

let rec parse_expression i =
  match parse_su i with
    e1,"+"::i1 -> let e2,i2 = parse_expression i1 in Add(e1,e2),i2
  | e1,i1 -> e1,i1

  and parse_subtraction i = (* Add code for Parsing Subtraction*)

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


let default_parser = make_parser parse_expression;;

let rec string_of_exp pr e =
  match e with
    Var s -> s
  | Const n -> string_of_int n
  | Add(e1,e2) ->
        let s = (string_of_exp 3 e1)^" + "^(string_of_exp 2 e2) in
        if 2 < pr then "("^s^")" else s
  | Sub (* Add code for Printing Subtraction*)
  | Mul(e1,e2) ->
        let s = (string_of_exp 7 e1)^" * "^(string_of_exp 6 e2) in
        if 6 < pr then "("^s^")" else s;;


(*Gives out printet input*)
let print_exp e = Format.print_string ("<<"^string_of_exp 0 e^">>");;

(* To test you Function*)
let e1 = default_parser "(3 - 4) * 5 + 6 * 7";;
let e2 = default_parser "(3 - 4 + 5) * 6 + 7";;

print_exp e1;;
print_exp e2;;

(*
  Exercise 2: Extend the simplfy function to handle Subtraction in cases like:
  x - x = 0
  x - 0 = x
  x -- y = x + y
  x --- y = x - y 
*)
(*Tipp:   Therfor you need to edit the type expression and the function simplify*)




(*
   Extra Exercise: Modify the parser to handle unary minus. 
   The parser and printer should be able to handle expressions like:
    x --- x
*)
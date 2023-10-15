(*TODO: Check if include is nessesary*)
# use "Parser.ml";;

(*Replaces var with str, Const with int, add whit (str1 + str2), Mul whit (str1 + str2) *)
let rec string_of_exp_old e =
  match e with
    Var s -> s
  | Const n -> string_of_int n
  | Add(e1,e2) -> "("^(string_of_exp_old e1)^" + "^(string_of_exp_old e2)^")"
  | Mul(e1,e2) -> "("^(string_of_exp_old e1)^" * "^(string_of_exp_old e2)^")";;


(*Replaces var with str, Const with int, add whit str1 + str2, Mul whit str1 * str2 and places () if needed*)
let rec string_of_exp pr e =
  match e with
    Var s -> s
  | Const n -> string_of_int n
  | Add(e1,e2) ->
        let s = (string_of_exp 3 e1)^" + "^(string_of_exp 2 e2) in
        if 2 < pr then "("^s^")" else s
  | Mul(e1,e2) ->
        let s = (string_of_exp 5 e1)^" * "^(string_of_exp 4 e2) in
        if 4 < pr then "("^s^")" else s;;


(*Gives out printet input*)
let print_exp e = Format.print_string ("<<"^string_of_exp 0 e^">>");;

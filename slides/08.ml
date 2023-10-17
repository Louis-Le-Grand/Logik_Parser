#use "06.ml";;


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


let print_exp e = Format.print_string ("<<"^string_of_exp 0 e^">>");;


(*Bsp*)
let e = default_parser "(x1 + x2 + x3) * (1 + 2 + 3 * x + y)";;
print_exp e;;

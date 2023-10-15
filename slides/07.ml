#use "06.ml";;


let rec string_of_exp_old e =
  match e with
    Var s -> s
  | Const n -> string_of_int n
  | Add(e1,e2) -> "("^(string_of_exp_old e1)^" + "^(string_of_exp_old e2)^")"
  | Mul(e1,e2) -> "("^(string_of_exp_old e1)^" * "^(string_of_exp_old e2)^")";;


let print_exp_old e = Format.print_string ("<<"^string_of_exp_old e^">>");;


(*Bsp*)

let e = default_parser "(x1 + x2 + x3) * (1 + 2 + 3 * x + y)";;
print_exp_old e;;

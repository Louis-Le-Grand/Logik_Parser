# use "04.ml";;

let rec lexwhile prop inp =
  match inp with
    c::cs when prop c -> let tok,rest = lexwhile prop cs in c^tok,rest
  | _ -> "",inp;;


let rec lex inp =
  match snd(lexwhile space_str inp) with
    [] -> []
  | c::cs -> let prop = if alphanumeric_str(c) then alphanumeric_str
                        else if symbolic_str(c) then symbolic_str
                        else fun c -> false in
              let toktl,rest = lexwhile prop cs in
              (c^toktl)::lex rest;;

(*Bsp*)
lex(explode_str "2*((var_1 + x) + 11)");;
lex(explode_str "if (*p1-- == *p2++) then f() else g()");;
lex(explode_str "Bonn hat die Postleitzahl 53115");;
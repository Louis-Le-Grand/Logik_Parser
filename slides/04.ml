#use "03.ml";;


let matches s = let chars = explode s in fun c -> List.mem c chars;;  


let space = matches " \t\n\r"
  and punctuation = matches "()[]{},"
  and symbolic = matches "~‘!@#$%^&*-+=|\\:;<>.?/"
  and numeric = matches "0123456789"
  and alphanumeric = matches
    "abcdefghijklmnopqrstuvwxyz_’ABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789";;


let space_str inp = matches " \t\n\r" inp.[0]
  and punctuation_str inp = matches "()[]{},;" inp.[0]
  and symbolic_str inp = matches "~‘!@#$%^&*-+=|\\:;<>.?/" inp.[0]
  and numeric_str inp = matches "0123456789" inp.[0]
  and alphanumeric_str inp = matches
    "abcdefghijklmnopqrstuvwxyz_’ABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789" inp.[0];;


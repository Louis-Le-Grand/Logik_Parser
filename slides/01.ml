#use "00.ml";;


type expression =
   Var of string
 | Const of int
 | Add of expression * expression
 | Mul of expression * expression;;

(*Bsp "2*x+y" *)
Add(Mul(Const 2,Var "x"),Var "y");;
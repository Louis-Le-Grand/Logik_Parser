type Term =
  | Var of string
  | Const of string
  | func of string * int * Term list

type Formula =
  | Equal of Term * Term
  | Pred of string * int * Term list
  | Not of Formula
  | And of Formula * Formula
  | Or of Formula * Formula
  | Implies of Formula * Formula
  | Iff of Formula * Formula
  | Forall of string * Formula
  | Exists of string * Formula
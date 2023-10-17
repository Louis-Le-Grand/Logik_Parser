## In this repository

You will find the code and the slides for the parser we will be writing during my Presentation.


## Get the course repository

* Open a terminal (or a git bash on Windows).

* Use `cd` to navigate to a directory where you would like to create the `Logik_Parser` folder.

* Run `git clone https://github.com/Louis-Le-Grand/Logik_Parser.git`.

* Run `cd Logik_Parser`

* Open the Code in your favorite editor (I recommend VSCode).

## Run the code
The Code is written in OCaml. To run the code, you will need to install OCaml and the OCaml Package Manager (opam). You can find the instructions [here](http://www.ocaml.org/).

The code is writen in Version 5.1.0~alpha1 of OCaml. You can check your version by running `ocaml -version` in your terminal. If you have any difiiculties running the code, you may want to install this version.
```bash
    opam update
```
And then
```bash
    eval $(opam env --switch=5.1.0~alpha1)
```
# Logik_Parser
## Types
In Toplevel language, you can define types. For example, you can define a type `Term` and `Formula` as shown in slides\00.ml
### Algebraic Data Types
For this presentation, we will use Algebraic Data Types, which consist of Variballes, Constants and Function Symbols Additions and Multiplications. Defining a type in OCaml is done as follows:
```ocaml
type expression =
  | Var of string
  | Const of string
  | Add of expression * expression
  | Mul of expression * expression
```
## Parsing
### Lexing
The first step of parsing is lexing. Lexing is the process of transforming a string into a list of tokens. For example, the string `x + y` would be transformed into the list `[alphanumeric "x"; space " "; symbolic "+"; space " "; alphanumeric "y"]`. 
### Parsing
The second step of parsing is parsing. Parsing is the process of transforming a list of tokens into a tree. For example, the list `[alphanumeric "x"; space " "; symbolic "+"; space " "; alphanumeric "y"]` would be transformed into the tree `Add (Var "x", Var "y")`.
## Prettyprinting
If you want to return readable output, you can use prettyprinting.
Prettyprinting is the process of transforming a tree into a string. For example, the tree `Add (Var "x", Var "y")` would be transformed into the string `x + y`.
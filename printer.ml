open Ast ;;
open Parser ;;

let string_of_binop : binOp -> string = function
  | Add -> "+"
  | Sub -> "-"
  | Mult -> "*"
  | Div -> "/"
  | And -> "&&"
  | Or -> "||"
  | Eq -> "=="
  | NEq -> "!="
  | Less -> "<"
  | LEq -> "<="
  | Greater -> ">"
  | GEq -> ">="
  ;;

let string_of_unop : unOp -> string = function
  | Not -> "!"
  ;;

let string_of_datatype : datatype -> string = function
  | Int_t -> "int"
  | Float_t -> "float"
  | Bool_t -> "bool"
  | Char_t -> "char"
  | String_t -> "string"
  | Unit_t -> "unit"
  ;;
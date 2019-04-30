open Ast ;;
open Exceptions ;;

(* Functions to get information about / manipulate the AST *)

let string_of_binop : binOp -> string = function
  | Add     -> "+"
  | Sub     -> "-"
  | Mult    -> "*"
  | Div     -> "/"
  | Mod     -> "%"
  | And     -> "&&"
  | Or      -> "||"
  | Xor     -> "^"
  | Eq      -> "=="
  | NEq     -> "!="
  | Less    -> "<"
  | LEq     -> "<="
  | Greater -> ">"
  | GEq     -> ">=" ;;

let string_of_datatype : datatype -> string = function
  | Int_t     -> "int"
  | Float_t   -> "float"
  | Bool_t    -> "bool"
  | Char_t    -> "char"
  | String_t  -> "string"
  | Unit_t    -> "unit" ;;

(* Get expression type *)
let rec get_expr_type : expr -> datatype = function
  | IntLit _          -> Int_t
  | FloatLit _        -> Float_t
  | BoolLit _         -> Bool_t
  | CharLit _         -> Char_t
  | StringLit _       -> String_t
  | BinOp (_, e1, e2) -> get_binop_type e1 e2
  | UnOp (_, e1)      -> get_expr_type e1
  | _                 -> Unit_t

(* Get the type of a binary operation *)
and get_binop_type (e1 : expr) (e2 : expr) : datatype =
  let e1_t : datatype = get_expr_type e1 in
  let e2_t : datatype = get_expr_type e2 in
  match e1_t, e2_t with
  | Int_t, Int_t -> Int_t
  | Float_t, Float_t -> Float_t
  | Bool_t, Bool_t -> Bool_t
  | Char_t, Char_t -> Char_t
  | String_t, _ | _, String_t -> raise (BinaryOperationOnType String_t)
  | Unit_t, _ | _, Unit_t -> raise (BinaryOperationOnType String_t)
  | _ -> raise (BinaryOperationOnDifferentTypes (e1_t, e2_t)) ;;

(* Default error program, which is a call to print an error (this also generates LLVM) *)
let error_program (error : string) : program = 
  Program ([
    FuncDef (Int_t, "main", [], [
      Expr (
        Call ("printf", [
          StringLit error
        ])
      )
    ])
  ]) ;;

let string_of_exception (filename : string) : exn -> string = function
  | BinaryOperationOnDifferentTypes (e1_t, e2_t) -> "Error: File \"" ^ filename ^ "\"" ^ ": Cannot perform binary operation on types " ^ (string_of_datatype e1_t) ^ " and " ^ (string_of_datatype e2_t) ^ "."
  | BinaryOperationOnType data_t -> "Error: File \"" ^ filename ^ "\": Cannot perform binary operation on type" ^ string_of_datatype data_t ^ "."
  | LLVMFunctionNotFound fname -> "Error: File \"" ^ filename ^ "\": Could not find any function with the name \"" ^ fname ^ "\"."
  | SyntaxError (line, err) -> "Syntax error: File \"" ^ filename ^ "\", line " ^ string_of_int line ^ ": " ^ err ^ "."
  | UndefinedId id -> "Error: File \"" ^ filename ^ "\": \"" ^ id ^ "\" is not defined."
  | _ -> "Error: File \"" ^ filename ^ "\"." ;;
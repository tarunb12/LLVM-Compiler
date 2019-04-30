open Ast ;;
open Exceptions ;;

(* Functions to get information about / manipulate the AST *)

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
  | String_t, _ | _, String_t -> raise BinaryOperationOnString
  | Unit_t, _ | _, Unit_t -> raise BinaryOperationOnUnit
  | _ -> raise (BinaryOperationOnDifferentTypes (string_of_datatype e1_t, string_of_datatype e2_t)) ;;

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

let handle_exception (filename : string) (produce_error : string -> unit) : exn -> unit = function
  | SyntaxError (line, err) -> produce_error ("Syntax error: File \"" ^ filename ^ "\", line " ^ string_of_int line ^ ": " ^ err)
  | _ -> produce_error ("Error: File \"" ^ filename ^ "\"") ;;
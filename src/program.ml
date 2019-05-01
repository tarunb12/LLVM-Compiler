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

let string_of_unop : unOp -> string = function
  | Neg -> "-"
  | Not -> "!" ;;

let string_of_datatype : datatype -> string = function
  | Int_t     -> "int"
  | Float_t   -> "float"
  | Bool_t    -> "bool"
  | Char_t    -> "char"
  | String_t  -> "string"
  | Unit_t    -> "unit" ;;

(* Get expression type *)
let rec get_expr_type : expr -> datatype = function
  | IntLit _            -> Int_t
  | FloatLit _          -> Float_t
  | BoolLit _           -> Bool_t
  | CharLit _           -> Char_t
  | StringLit _         -> String_t
  | BinOp (op, e1, e2)  -> get_binop_type op e1 e2
  | UnOp (op, e1)       -> get_expr_type e1
  | _                   -> Unit_t

(* Get the type of a binary operation *)
and get_binop_type (op : binOp) (e1 : expr) (e2 : expr) : datatype =
  let e1_t : datatype = get_expr_type e1 in
  let e2_t : datatype = get_expr_type e2 in
  let invalid_binary_operation = InvalidBinaryOperation (op, e1_t, e2_t) in
  if e1_t <> e2_t then raise invalid_binary_operation
  else
    match op with
    | Add | Sub | Mult | Div | Mod ->
      begin
        match e1_t with
        | Int_t | Float_t | Char_t -> e1_t
        | _ -> raise invalid_binary_operation
      end
    | And | Or | Xor ->
      begin
        match e1_t with
        | Bool_t -> e1_t
        | _ -> raise invalid_binary_operation
      end
    | Less | LEq | Greater | GEq ->
      begin
        match e1_t with
        | Int_t | Float_t | Char_t -> e1_t
        | _ -> raise invalid_binary_operation
      end
    | Eq | NEq ->
      begin
        match e1_t with
        | Unit_t | String_t -> raise invalid_binary_operation
        | _ -> e1_t
      end

and get_unop_type (op : unOp) (e : expr) : datatype =
  let e_t = get_expr_type e in
  let invalid_unary_operation = raise (InvalidUnaryOperation (op, e_t)) in
  match op with
  | Neg ->
    begin
      match e_t with
      | Float_t | Int_t -> e_t
      | _ -> invalid_unary_operation
    end
  | Not ->
    begin
      match e_t with
      | Bool_t -> e_t
      | _ -> invalid_unary_operation
    end ;;

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

let add_period (str : string) : string = str ^ "." ;;

let string_of_exception (filename : string) : exn -> string = function
  | SyntaxError (line, err) -> "Syntax error: File \"" ^ filename ^ "\", line " ^ string_of_int line ^ ": \"" ^ err ^ "\""
  | exn ->
    "Error: File \"" ^ filename ^ "\": " ^
    match exn with
    | InvalidBinaryOperation (op, e1_t, e2_t) -> "Cannot perform the operation \"" ^ string_of_binop op ^ "\" on type " ^ string_of_datatype e1_t ^ " -> " ^ string_of_datatype e2_t
    | InvalidDefinitionType (v, d_t, e_t) ->  "The specified type " ^ string_of_datatype d_t ^ " of the variable \"" ^ v ^ "\" does not match expression of type " ^ string_of_datatype e_t
    | InvalidUnaryOperation (op, e_t) -> "Cannot perform the operation \"" ^ string_of_unop op ^ "\" on type " ^ string_of_datatype e_t
    | LLVMFunctionNotFound fname -> "Could not find any function with the name \"" ^ fname ^ "\""
    | UndefinedId id -> "\"" ^ id ^ "\" is not defined"
    | _ -> "" ;;
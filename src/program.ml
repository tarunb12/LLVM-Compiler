open Ast ;;
open Utils ;;
open Exceptions ;;

(* Functions to get information about / manipulate the AST *)

let binop_type_of_types (op : binOp) (e1_t : datatype) (e2_t : datatype) : datatype =
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

(* Get expression type *)
let rec get_expr_type : expr -> datatype = function
  | IntLit _            -> Int_t
  | FloatLit _          -> Float_t
  | BoolLit _           -> Bool_t
  | CharLit _           -> Char_t
  | StringLit _         -> String_t
  | BinOp (op, e1, e2)  -> get_binop_type op e1 e2
  | UnOp (op, e1)       -> get_unop_type op e1
  | Id _                -> Unit_t
  | Assign _            -> Unit_t
  | Call _              -> Unit_t
  | Noexpr              -> Unit_t

(* Get the type of a binary operation *)
and get_binop_type (op : binOp) (e1 : expr) (e2 : expr) : datatype =
  let e1_t : datatype = get_expr_type e1 in
  let e2_t : datatype = get_expr_type e2 in
  binop_type_of_types op e1_t e2_t

and get_unop_type (op : unOp) (e : expr) : datatype =
  let e_t = get_expr_type e in
  let invalid_unary_operation = InvalidUnaryOperation (op, e_t) in
  match op with
  | Neg ->
    begin
      match e_t with
      | Float_t | Int_t -> e_t
      | _ -> raise invalid_unary_operation
    end
  | Not ->
    begin
      match e_t with
      | Bool_t -> e_t
      | _ -> raise invalid_unary_operation
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
    | FirstPrintArgumentNotString expr -> "Invalid first argument for printf: " ^ string_of_expr expr
    | FunctionWithoutBasicBlock fname -> "The function \"" ^ fname ^ "\" is empty"
    | InvalidBinaryOperation (op, e1_t, e2_t) -> "Cannot perform the operation \"" ^ string_of_binop op ^ "\" on type " ^ string_of_datatype e1_t ^ " -> " ^ string_of_datatype e2_t
    | InvalidConditionType d_t -> "Invalid condition of type " ^ string_of_datatype d_t ^ ", expected bool"
    | InvalidDefinitionType (v, d_t, e_t) ->  "The specified type " ^ string_of_datatype d_t ^ " of the variable \"" ^ v ^ "\" does not match expression of type " ^ string_of_datatype e_t
    | InvalidMainReturnType d_type -> "Invalid return type of \"" ^ string_of_datatype d_type ^ "\" for main method, expected \"int\""
    | InvalidParameterType fname -> "Invalid parameter declaration in the function \"" ^ fname ^ "\""
    | InvalidUnaryOperation (op, e_t) -> "Cannot perform the operation \"" ^ string_of_unop op ^ "\" on type " ^ string_of_datatype e_t
    | LeftHandSideUnassignable expr -> "Cannot assign a value to the left hand side of the expression: " ^ string_of_expr expr
    | LLVMFunctionNotFound fname -> "Could not find any function with the name \"" ^ fname ^ "\""
    | NotImplemented -> "Not implemented"
    | UndefinedId id -> "\"" ^ id ^ "\" is not defined"
    | _ -> Printexc.to_string exn ^ Printexc.get_backtrace () ;;
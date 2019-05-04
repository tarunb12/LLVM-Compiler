open Ast ;;
open Exceptions ;;

let string_of_binop : binOp -> string = function
  | Add     -> "+"
  | Sub     -> "-"
  | Mult    -> "*"
  | Div     -> "/"
  | Mod     -> "%"
  | LShift  -> "<<"
  | RShift  -> ">>"
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

let rec string_of_stmt (indent : int) : statement -> string = 
  let indent_string (indent : int) : string = String.make indent '\t' in function
  | Block stmts -> "{\n" ^ String.concat "\n" (List.map (fun str -> (indent_string indent) ^ str) (List.map (string_of_stmt (indent + 1)) stmts)) ^ "\n}\n"
  | Expr expr -> string_of_expr expr
  | Return expr -> "return " ^ string_of_expr expr
  | VarDef (d_type, vname, expr) -> string_of_datatype d_type ^ " " ^ vname ^ begin match expr with Noexpr -> "" | _ -> " = " ^ string_of_expr expr end
  | FuncDef (d_type, fname, params, stmts) -> string_of_datatype d_type ^ " " ^ fname ^ "(" ^ String.concat ", " (List.map (string_of_stmt indent) params) ^ ") " ^ string_of_stmt indent (Block stmts)
  | If (cond, t_block, f_block) -> "if (" ^ string_of_expr cond ^ ") " ^ string_of_stmt (indent + 1) (t_block) ^ if f_block <> Block([]) then " else " ^ string_of_stmt indent f_block else ""
  | For (v_assign, cond, v_reassign, block) -> "for (" ^ string_of_expr v_assign ^ "; " ^ string_of_expr cond ^ "; " ^ string_of_expr v_reassign ^ string_of_stmt indent block
  | While (cond, block) -> "while (" ^ string_of_expr cond ^ ")" ^ string_of_stmt (indent + 1) block
  | Break -> "break"
  | Continue -> "continue"

and string_of_expr : expr -> string = function
  | Noexpr -> ""
  | IntLit i -> string_of_int i
  | FloatLit f -> string_of_float f
  | BoolLit b -> string_of_bool b
  | CharLit c -> "'" ^ Char.escaped c ^ "'"
  | StringLit s -> "\"" ^ String.escaped s ^ "\""
  | Id id -> id
  | BinOp (op, e1, e2) -> string_of_expr e1 ^ " " ^ string_of_binop op ^ " " ^ string_of_expr e2
  | UnOp (op, e) -> string_of_unop op ^ string_of_expr e
  | Assign (e1, e2) -> string_of_expr e1 ^ " = " ^ string_of_expr e2
  | Call (name, expr_list) ->
    name ^ "(" ^
    match expr_list with
    | [] -> ")"
    | hd :: [] -> string_of_expr hd ^ ")"
    | hd :: tl -> string_of_expr hd ^ ", " ^ String.concat ", " (List.map string_of_expr tl) ^ ")" ;;

let string_of_program : program -> string = function
  | Program stmts -> String.concat "\n\n" (List.map (string_of_stmt 0) stmts) ;;

let string_of_exception (filename : string) : exn -> string = function
  | SyntaxError (line, err) -> "Syntax error: File \"" ^ filename ^ "\", line " ^ string_of_int line ^ ": \"" ^ err ^ "\""
  | exn ->
    "Error: File \"" ^ filename ^ "\": " ^
    match exn with
    | CannotRedefineParameter id -> "Cannot redefine the parameter \"" ^ id ^ "\""
    | FirstPrintArgumentNotString expr -> "Invalid first argument for printf: " ^ string_of_expr expr
    | FunctionWithoutBasicBlock fname -> "The function \"" ^ fname ^ "\" is empty"
    | InvalidBinaryOperation (op, e1_t, e2_t) -> "Cannot perform the operation \"" ^ string_of_binop op ^ "\" on type " ^ string_of_datatype e1_t ^ " -> " ^ string_of_datatype e2_t
    | InvalidConditionType d_t -> "Invalid condition of type " ^ string_of_datatype d_t ^ ", expected bool"
    | InvalidDefinitionType (v, d_t, e_t) ->  "The specified type " ^ string_of_datatype d_t ^ " of the variable \"" ^ v ^ "\" does not match expression of type " ^ string_of_datatype e_t
    | InvalidFunctionReturnType (fname, r_t, d_t, e) -> "Invalid return type specified in the function " ^ fname ^ ": \"" ^ string_of_datatype r_t ^ "\", expected \"" ^ string_of_datatype d_t ^ "\" in \"return " ^ string_of_expr e ^ "\""
    | InvalidFunctionWithoutReturn (fname, r_t) -> "No return expression specified in the function " ^ fname ^ ", expected a return of type \"" ^ string_of_datatype r_t ^ "\""
    | InvalidMainReturnType d_type -> "Invalid return type of \"" ^ string_of_datatype d_type ^ "\" for main method, expected \"int\""
    | InvalidParameterType fname -> "Invalid parameter declaration in the function \"" ^ fname ^ "\""
    | InvalidUnaryOperation (op, e_t) -> "Cannot perform the operation \"" ^ string_of_unop op ^ "\" on type " ^ string_of_datatype e_t
    | LeftHandSideUnassignable expr -> "Cannot assign a value to the left hand side of the expression: " ^ string_of_expr expr
    | LLVMFunctionNotFound fname -> "Could not find any function with the name \"" ^ fname ^ "\""
    | MainMethodNotDefined -> "No entry point found in the file. Define the entry point with the function main defined as follows:\n\nint main() {\n\t...\n\treturn 0\n}\n"
    | MultipleEntryPoints -> "Cannot have multiple entry points within the file"
    | NestedFunctionsNotSupported fname -> "Nested functions are not supported (found the nested function " ^ fname
    | UndefinedId id -> "\"" ^ id ^ "\" is not defined"
    | Parsing.Parse_error -> "Could not parse the provided input file"
    | _ -> Printexc.to_string exn ^ Printexc.get_backtrace () ;;
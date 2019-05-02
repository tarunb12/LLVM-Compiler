open Ast ;;

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

let rec string_of_stmt (indent : int) : statement -> string = 
  let indent_string (indent : int) : string = String.make indent '\t' in function
  | Block stmts -> "{\n" ^ String.concat "\n" (List.map (fun str -> (indent_string indent) ^ str) (List.map (string_of_stmt (indent + 1)) stmts)) ^ "\n}\n"
  | Expr expr -> string_of_expr expr
  | Return expr -> "return " ^ string_of_expr expr
  | VarDef (d_type, vname, expr) -> string_of_datatype d_type ^ " " ^ vname ^ begin match expr with Noexpr -> "" | _ -> " = " ^ string_of_expr expr end
  | VarRedef (vname, expr) -> vname ^ " = " ^ string_of_expr expr
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
  | CharLit c -> Char.escaped c
  | StringLit s -> "\"" ^ s ^ "\""
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
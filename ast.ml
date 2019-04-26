type binOp = 
    | Add | Sub | Mult | Div | And
    | Or | Equal | Neq | Less | Leq
    | Greater | Geq
    ;;

type unOp =
    | Not
    ;;

type datatype =
    | INT | FLOAT | BOOL
    ;;

type expr =
    | Int   of int
    | Float of float
    | Bool  of bool
    | Id    of string
    | UnOp  of unOp
    | BinOp of binOp * expr * expr

and statement =
    | Block     of statement list
    | Expr      of expr
    | VarDef    of datatype * string * expr
    | If        of expr * statement * statement
    | For       of expr * expr * expr * statement
    | While     of expr * statement
    ;;

type program =
    | Program of statement list
    ;;
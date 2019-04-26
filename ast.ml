type binOp = 
    | Add | Sub | Mult | Div | And
    | Or | Eq | NEq | Less | LEq
    | Greater | GEq
    ;;

type unOp =
    | Not
    ;;

type datatype =
    | Int_t | Float_t | Bool_t
    | String_t | Char_t | Unit_t
    ;;

type expr =
    | Int       of int
    | Float     of float
    | Bool      of bool
    | Char      of char
    | String    of string
    | Id        of string
    | UnOp      of unOp * expr
    | BinOp     of binOp * expr * expr
    | Noexpr

type statement =
    | Block     of statement list
    | Expr      of expr
    | Return    of expr
    | VarDef    of datatype * string * expr
    | FuncDef   of datatype * string * statement list * statement list
    | VarDec    of datatype * string
    | If        of expr * statement * statement
    | For       of expr * expr * expr * statement
    | While     of expr * statement
    | Break
    | Continue
    ;;

type program =
    | Program of statement list
    ;;
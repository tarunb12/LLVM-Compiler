type binOp = 
    | Add | Sub | Mult | Div | And
    | Or | Eq | NEq | Less | LEq
    | Greater | GEq
    ;;

type unOp =
    | Not | Neg
    ;;

type datatype =
    | Int_t | Float_t | Bool_t
    | String_t | Char_t | Unit_t
    ;;

type expr =
    | IntLit    of int
    | FloatLit  of float
    | BoolLit   of bool
    | CharLit   of char
    | StringLit of string
    | Id        of string
    | BinOp     of binOp * expr * expr
    | UnOp      of unOp * expr
    | Call      of string * expr list
    | Noexpr
    ;;

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
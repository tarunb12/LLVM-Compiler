open Ast ;;
open Exceptions ;;

let getExprType : expr -> datatype =
  function
    | IntLit _    -> Int_t
    | FloatLit _  -> Float_t
    | BoolLit _   -> Bool_t
    | CharLit _   -> Char_t
    | StringLit _ -> String_t
    | _           -> Unit_t ;;

let execute (program  : program) : unit =
  match program with
  | Program [] -> ()
  | Program stmts -> ()
  ;;
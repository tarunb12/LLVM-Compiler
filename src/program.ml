open Ast ;;
open Exceptions ;;

(* Transform ast, encode types with respected operation ex.binop *)
let get_expr_type : expr -> datatype =
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
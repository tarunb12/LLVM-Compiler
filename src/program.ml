open Ast ;;

let execute (program  : program) : unit =
  match program with
  | Program [] -> ()
  | Program stmts -> ()
  ;;
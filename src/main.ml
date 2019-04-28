open Lexing ;;
open Parser ;;
open Lexer ;;
open Codegen ;;

let filename = Sys.argv.(1) ;;
let out_file = Sys.argv.(2) ;;

let () = open_in filename
  |> from_channel
  |> program token
  |> codegen_ast
  |> print_module out_file
  ;;
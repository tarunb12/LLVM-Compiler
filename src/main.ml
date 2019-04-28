open Lexing ;;
open Parser ;;
open Lexer ;;
open Codegen ;;

let filename = Sys.argv.(1)

(* AST -> sexpr -> SAST ? *)

let () = open_in filename
	|> from_channel
	|> program token
	|> codegen_ast
  |> print_module
	;;
open Lexing ;;
open Parser ;;
open Lexer ;;
open Program ;;

let filename = Sys.argv.(1)

(* AST -> sexpr -> SAST ? *)

let () = open_in filename
	|> from_channel
	|> program token
	|> execute
	;;
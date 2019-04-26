open Lexing
open Parser
open Lexer
open Poly
open Expr

let filename = Sys.argv.(1)

let () = open_in filename
	|> from_channel
	|> main token
	|> print_expr
	|> from_expr
	|> simplify
	|> print_pExp
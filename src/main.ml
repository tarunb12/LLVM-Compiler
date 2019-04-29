open Lexing ;;
open Parser ;;
open Lexer ;;
open Program ;;
open Codegen ;;
open Exceptions ;;

let filename = Sys.argv.(1) ;;
let out_file = Sys.argv.(2) ;;

let () = try open_in filename
  |> from_channel
  |> program token
  |> codegen_ast
  |> print_module out_file
  with
    | SyntaxError (line, char) -> 
      let error = "Syntax error: File \"" ^ filename ^ "\", line " ^ string_of_int line ^ ": character " ^ char in
      Printf.printf "%s\n" error;
      error_program error
        |> codegen_ast
        |> print_module out_file
    | _ ->
      let error = "Error: File \"" ^ filename ^ "\"" in
      Printf.printf "%s\n" error;
      error_program error
        |> codegen_ast
        |> print_module out_file ;;
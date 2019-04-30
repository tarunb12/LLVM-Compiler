open Lexer ;;
open Lexing ;;
open Parser ;;
open Program ;;
open Codegen ;;
open Exceptions ;;

let filename : string = Sys.argv.(1) ;;
let out_file : string = Sys.argv.(2) ;;

let produce_error (error : string) : unit =
  Printf.printf "%s\n" error;
  error_program error
    |> codegen_ast
    |> print_module out_file

let () = try open_in filename
  |> from_channel
  |> program token
  |> codegen_ast
  |> print_module out_file
  with
    | SyntaxError (line, err) -> produce_error ("Syntax error: File \"" ^ filename ^ "\", line " ^ string_of_int line ^ ": " ^ err)
    | _ -> produce_error ("Error: File \"" ^ filename ^ "\"") ;;
open Lexer ;;
open Lexing ;;
open Parser ;;
open Program ;;
open Codegen ;;

let filename : string = Sys.argv.(1) ;;
let out_file : string = Sys.argv.(2) ;;

let produce_error (error : string) : unit = error_program error
  |> codegen_ast
  |> print_module out_file ;;

let () = try open_in filename
  |> from_channel
  |> program token
  |> codegen_ast
  |> print_module out_file
  with exn -> handle_exception filename produce_error exn ;;
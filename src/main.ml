open Lexer ;;
open Lexing ;;
open Parser ;;
open Program ;;
open Codegen ;;

let filename : string = Sys.argv.(1) ;;
let out_file : string = Sys.argv.(2) ;;

let produce_error (exn : exn) : unit = exn
  |> string_of_exception filename
  |> error_program
  |> codegen_ast
  |> print_module out_file ;;

let () = try open_in filename
  |> from_channel
  |> program token
  |> codegen_ast
  |> print_module out_file
  with exn -> produce_error exn ;;
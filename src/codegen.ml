open Ast ;;
open Exceptions ;;

let context     : Llvm.llcontext  = Llvm.global_context () ;;
let the_module  : Llvm.llmodule   = Llvm.create_module context "thx dobra" ;;
let builder     : Llvm.llbuilder  = Llvm.builder context ;;
let i32_t       : Llvm.lltype     = Llvm.i32_type context ;;
let i8_t        : Llvm.lltype     = Llvm.i8_type context ;;
let i1_t        : Llvm.lltype     = Llvm.i1_type context ;;
let float_t     : Llvm.lltype     = Llvm.float_type context ;;
let void_t      : Llvm.lltype     = Llvm.void_type context ;;
let str_t       : Llvm.lltype     = Llvm.pointer_type (Llvm.i8_type context) ;;

let named_values      : (string, Llvm.llvalue) Hashtbl.t = Hashtbl.create 50 ;;
let named_parameters  : (string, Llvm.llvalue) Hashtbl.t = Hashtbl.create 50 ;;

let rec codegen_expr (llbuilder : Llvm.llbuilder) : Llvm.llvalue = function
  | FloatLit flt    -> Llvm.const_float float_t flt
  | IntLit int      -> Llvm.const_int i32_t int
  | BoolLit bool    -> if bool then Llvm.const_int i1_t 1 else Llvm.const_int i1_t 0
  | CharLit char    -> Llvm.const_int i8_t (int_of_char char)
  | _               -> raise NotImplemented

and handle_binop (expr1 : expr) (expr2 : expr) (op : binOp) (llbuilder : Llvm.llbuilder) : Llvm.llvalue =
  let getExprType (expr : expr) : datatype = match expr with
    | IntLit int -> Int_t
    | FloatLit float -> Float_t
    | _ -> raise InvalidType in

  let expr1_t = getExprType expr1 in
  let expr2_t = getExprType expr2 in

  let expr1 = codegen_expr llbuilder expr1 in
  let expr2 = codegen_expr llbuilder expr2 in
  ()
  ;;

let codegen_library_functions () = 
    let printf_t =  Llvm.var_arg_function_type i32_t [| Llvm.pointer_type i8_t |] in
    let _ =         Llvm.declare_function "printf" printf_t the_module in
    let malloc_t =  Llvm.function_type (str_t) [| i32_t |] in
    let _ =         Llvm.declare_function "malloc" malloc_t the_module in
    let open_t =    Llvm.function_type i32_t [| (Llvm.pointer_type i8_t); i32_t |] in 
    let _ =         Llvm.declare_function "open" open_t the_module in
    let close_t =   Llvm.function_type i32_t [| i32_t |] in
    let _ =         Llvm.declare_function "close" close_t the_module in
    let read_t =    Llvm.function_type i32_t [| i32_t; Llvm.pointer_type i8_t; i32_t |] in
    let _ =         Llvm.declare_function "read" read_t the_module in
    let write_t =   Llvm.function_type i32_t [| i32_t; Llvm.pointer_type i8_t; i32_t |] in
    let _ =         Llvm.declare_function "write" write_t the_module in 
    let lseek_t =   Llvm.function_type i32_t [| i32_t; i32_t; i32_t |] in
    let _ =         Llvm.declare_function "lseek" lseek_t the_module in
    let exit_t =    Llvm.function_type void_t [| i32_t |] in
    let _ =         Llvm.declare_function "exit" exit_t the_module in
    let realloc_t = Llvm.function_type str_t [| str_t; i32_t |] in
    let _ =         Llvm.declare_function "realloc" realloc_t the_module in
    let getchar_t = Llvm.function_type (i32_t) [| |] in
    let _ =         Llvm.declare_function "getchar" getchar_t the_module in
    let sizeof_t =  Llvm.function_type (i32_t) [| i32_t |] in
    let _ =         Llvm.declare_function "sizeof" sizeof_t the_module in 
    () ;;


let codegen_ast (_ast : program) =
  (* Reserved functions in llvm *)
  let _ = codegen_library_functions () in
  the_module ;;
  
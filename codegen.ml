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

let rec codegen_expr : Llvm.llvalue = function
  | FloatLit flt    -> Llvm.const_float float_t flt
  | IntLit int      -> Llvm.const_int i32_t int
  | BoolLit bool    -> if bool then Llvm.const_int i1_t 1 else Llvm.const_int i1_t 0
  | CharLit char    -> Llvm.const_int i8_t (int_of_char char)
  | _               -> Llvm.const_float float_t 0

and handle_binop (expr1 : expr) (expr2 : expr) (op : binOp) (llbuilder : Llvm.llbuilder) : Llvm.llvalue =
  let expr1_t : datatype = match expr1 with
    | IntLit int -> Int_t
    | FloatLit float -> Float_t
    | _ -> raise (Inva)
open Llvm
open Ast ;;

let context     = global_context () ;;
let the_module  = create_module context "thx dobra" ;;
let builder     = builder context ;;
let i32_t       = i32_type context ;;
let i8_t        = i8_type context ;;
let i1_t        = i1_type context ;;
let float_t     = float_type context ;;
let void_t      = void_type context ;;
let str_t       = pointer_type (i8_type context) ;;

let named_values : (string, llvalue) Hashtbl.t = Hashtbl.create ()
  ~hashable:String.hashable
  ~size:50 ;;

let named_parameters : (string, llvalue) Hashtbl.t = Hashtbl.create ()
  ~hashable:String.hashable
  ~size:50 ;;

open Ast ;;
open Program ;;
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

(* Function Name -> LLVM Function *)
let llvm_lookup_function (fname : string) : Llvm.llvalue =
  match Llvm.lookup_function fname the_module with
  | None    -> raise (LLVMFunctionNotFound (fname))
  | Some f  -> f ;;

(* Data Type -> LLVM Type *)
let get_lltype : datatype -> Llvm.lltype = function
  | Int_t     -> i32_t
  | Float_t   -> float_t
  | Bool_t    -> i1_t
  | Char_t    -> i8_t
  | String_t  -> str_t
  | Unit_t    -> void_t ;;

(* Statement -> LLVM Statement Execution *)
let rec codegen_stmt (stmt : statement) (llbuilder : Llvm.llbuilder) : Llvm.llvalue =
  match stmt with
  | Block stmt_list                         -> List.hd (List.map (fun stmt -> codegen_stmt stmt llbuilder) stmt_list)
  | Expr expr                               -> codegen_expr llbuilder expr
  | VarDef (data_t, vname, e)               -> codegen_vardef vname data_t e llbuilder
  | _                                       -> raise NotImplemented


(* Expression -> LLVM Expression Evaluation *)
and codegen_expr (llbuilder : Llvm.llbuilder) : expr -> Llvm.llvalue =
  function
    | FloatLit flt          -> Llvm.const_float float_t flt
    | IntLit int            -> Llvm.const_int i32_t int
    | BoolLit bool          -> if bool then Llvm.const_int i1_t 1 else Llvm.const_int i1_t 0
    | CharLit char          -> Llvm.const_int i8_t (int_of_char char)
    | BinOp (op, e1, e2)    -> handle_binop op e1 e2 llbuilder
    | UnOp (op, e)          -> handle_unop op e llbuilder
    | Assign (e1, e2)       -> codegen_assign e1 e2 llbuilder
    | Call (fname, params)  -> codegen_call fname params llbuilder
    | _                     -> raise NotImplemented


(* Binary Expression -> LLVM Value *)
and handle_binop (op : binOp) (e1 : expr) (e2 : expr) (llbuilder : Llvm.llbuilder) : Llvm.llvalue =
  let expr1 : Llvm.llvalue = codegen_expr llbuilder e1 in
  let expr2 : Llvm.llvalue = codegen_expr llbuilder e2 in
  
  let int_ops (op : binOp) (expr1 : Llvm.llvalue) (expr2 : Llvm.llvalue) : Llvm.llvalue =
    match op with
    | Add     -> Llvm.build_add expr1 expr2 "addtmp" llbuilder
    | Sub     -> Llvm.build_sub expr1 expr2 "subtmp" llbuilder
    | Mult    -> Llvm.build_mul expr1 expr2 "multmp" llbuilder
    | Div     -> Llvm.build_sdiv expr1 expr2 "divtmp" llbuilder
    | Mod     -> Llvm.build_srem expr1 expr2 "sremtmp" llbuilder
    | And     -> Llvm.build_and expr1 expr2 "andtmp" llbuilder
    | Or      -> Llvm.build_or expr1 expr2 "ortmp" llbuilder
    | Xor     -> Llvm.build_xor expr1 expr2 "xortmp" llbuilder
    | Eq      -> Llvm.build_icmp Llvm.Icmp.Eq expr1 expr2 "eqtmp" llbuilder
    | NEq     -> Llvm.build_icmp Llvm.Icmp.Ne expr1 expr2 "neqtmp" llbuilder
    | Less    -> Llvm.build_icmp Llvm.Icmp.Slt expr1 expr2 "lesstmp" llbuilder
    | LEq     -> Llvm.build_icmp Llvm.Icmp.Sle expr1 expr2 "leqtmp" llbuilder
    | Greater -> Llvm.build_icmp Llvm.Icmp.Sgt expr1 expr2 "sgttmp" llbuilder
    | GEq     -> Llvm.build_icmp Llvm.Icmp.Sge expr1 expr2 "sgetmp" llbuilder in

  let float_ops (op : binOp) (expr1 : Llvm.llvalue) (expr2 : Llvm.llvalue) : Llvm.llvalue =
    match op with
    | Add     -> Llvm.build_fadd expr1 expr2 "f_addtmp" llbuilder
    | Sub     -> Llvm.build_fsub expr1 expr2 "f_subtmp" llbuilder
    | Mult    -> Llvm.build_fmul expr1 expr2 "f_multmp" llbuilder
    | Div     -> Llvm.build_fdiv expr1 expr2 "f_divtmp" llbuilder
    | Mod     -> Llvm.build_frem expr1 expr2 "f_sremtmp" llbuilder
    | Eq      -> Llvm.build_fcmp Llvm.Fcmp.Oeq expr1 expr2 "f_eqtmp" llbuilder
    | NEq     -> Llvm.build_fcmp Llvm.Fcmp.One expr1 expr2 "f_neqtmp" llbuilder
    | Less    -> Llvm.build_fcmp Llvm.Fcmp.Olt expr1 expr2 "f_lesstmp" llbuilder
    | LEq     -> Llvm.build_fcmp Llvm.Fcmp.Ole expr1 expr2 "f_leqtmp" llbuilder
    | Greater -> Llvm.build_fcmp Llvm.Fcmp.Ogt expr1 expr2 "f_sgttmp" llbuilder
    | GEq     -> Llvm.build_fcmp Llvm.Fcmp.Oge expr1 expr2 "f_sgetmp" llbuilder
    | _       -> raise FloatOpNotSupported in
  
  let type_handler (data_t : datatype) : Llvm.llvalue =
    match data_t with
    | Int_t | Bool_t | Char_t -> int_ops op expr1 expr2
    | Float_t                 -> float_ops op expr1 expr2
    | _                       -> raise BinaryOperationNotSupported in

  let data_t : datatype = get_binop_type e1 e2 in
  type_handler data_t


(* Unary Expression -> LLVM Value *)
and handle_unop (op : unOp) (expr : expr) (llbuilder : Llvm.llbuilder) : Llvm.llvalue =
  let expr_t : datatype = get_expr_type expr in
  
  let expr : Llvm.llvalue = codegen_expr llbuilder expr in

  let un_ops (op : unOp) (expr : Llvm.llvalue) (data_t : datatype) : Llvm.llvalue =
    match op, data_t with
    | Neg, Int_t    -> Llvm.build_neg expr "i_unoptmp" llbuilder
    | Neg, Float_t  -> Llvm.build_fneg expr "f_unoptmp" llbuilder
    | Not, Bool_t   -> Llvm.build_not expr "b_unoptmp" llbuilder
    | _             -> raise InvalidUnOpType in

  let type_handler (data_t : datatype) : Llvm.llvalue =
    match data_t with
    | Int_t | Float_t | Bool_t  -> un_ops op expr data_t
    | _                         -> raise UnOpNotSupported in

  let data_t : datatype =
    match expr_t with
    | Int_t | Float_t | Bool_t  -> expr_t
    | _                         -> raise UnOpNotSupported in

  type_handler data_t


(*  *)
and codegen_assign (expr1 : expr) (expr2 : expr) (llbuilder : Llvm.llbuilder) : Llvm.llvalue =
  let rhs : Llvm.llvalue = codegen_expr llbuilder expr2 in
  let lhs : Llvm.llvalue =
    match expr1 with
    | Id id ->
      begin
        try Hashtbl.find named_parameters id
        with Not_found ->
          try Hashtbl.find named_values id
          with Not_found -> raise (UndefinedId id)
      end
    | _ -> raise LeftHandSideUnassignable in

  ignore (Llvm.build_store rhs lhs llbuilder);
  rhs


(* Variable definition -> LLVM Store Variable, Value *)
and codegen_vardef (vname : string) (data_t : datatype) (expr : expr) (llbuilder : Llvm.llbuilder) : Llvm.llvalue =
  let expr_t = get_expr_type expr in
  match data_t = expr_t with
  | true ->
    begin
      let lltype : Llvm.lltype = get_lltype data_t in
      let malloc : Llvm.llvalue = Llvm.build_malloc lltype vname llbuilder in

      Hashtbl.add named_values vname malloc;

      let lhs : expr = Id vname in
      match expr with
      | Noexpr  -> malloc
      | _       -> codegen_assign lhs expr llbuilder
    end
  | false -> 
    let data_t : string = string_of_datatype data_t in
    let expr_t : string = string_of_datatype expr_t in
    raise (InvalidDefinitionType ("The specified type " ^ data_t ^ " of the variable \"" ^ vname ^ "\" does not match expression of type " ^ expr_t))


(*  *)
and codegen_function_call = Llvm.const_int i32_t 0


(* Function Call -> LLVM Function Lookup/Execution *)
and codegen_call (fname : string) (params : expr list) (llbuilder : Llvm.llbuilder) : Llvm.llvalue =
  match fname with
  | "printf"  -> codegen_printf params llbuilder
  | _         -> codegen_function_call


(*  *)
and codegen_printf (params : expr list) (llbuilder : Llvm.llbuilder) =
  let format_str : expr = List.hd params in
  let format_llstr : Llvm.llvalue =
    match format_str with
    | StringLit str -> Llvm.build_global_stringptr str "fmt" llbuilder
    | _             -> raise FirstPrintArgumentNotString in

  let args : expr list = List.tl params in
  let format_llargs : Llvm.llvalue list = List.map (codegen_expr llbuilder) args in

  let func_llvalue : Llvm.llvalue = llvm_lookup_function "printf" in
  let llargs : Llvm.llvalue array = Array.of_list (format_llstr :: format_llargs) in
  Llvm.build_call func_llvalue llargs "printf" llbuilder ;;

let init_params (f : Llvm.llvalue) (args : statement list) : unit =
  let args = Array.of_list args in
  Array.iteri (fun i element ->
    let param = args.(i) in
    let named_param =
      match param with
      | VarDec (_, name)  -> name
      | _                 -> raise InvalidParameterType in
    Llvm.set_value_name named_param element;
    Hashtbl.add named_parameters named_param element;
  ) (Llvm.params f) ;;

(* Main Function Definition -> LLVM Main Function Store *)
let codegen_main (stmts : statement list) (d_type : datatype) : unit =
  match d_type with
  | Int_t ->
    begin
      Hashtbl.clear named_values;
      Hashtbl.clear named_parameters;

      let ftype : Llvm.lltype = Llvm.function_type i32_t [| i32_t; Llvm.pointer_type str_t |] in
      let f : Llvm.llvalue = Llvm.define_function "main" ftype the_module in
      let llbuilder : Llvm.llbuilder = Llvm.builder_at_end context (Llvm.entry_block f) in
      
      let argc : Llvm.llvalue = Llvm.param f 0 in
      let argv : Llvm.llvalue = Llvm.param f 0 in

      Llvm.set_value_name "argc" argc;
      Llvm.set_value_name "argv" argv;
      Hashtbl.add named_parameters "argc" argc;
      Hashtbl.add named_parameters "argv" argv;
      
      let _ : Llvm.llvalue = codegen_stmt (Block stmts) llbuilder in

      let last_basic_block : Llvm.llbasicblock =
        match Llvm.block_end (llvm_lookup_function "main") with
        | Llvm.After block  -> block
        | _ -> raise (FunctionWithoutBasicBlock "main") in

      match Llvm.instr_end last_basic_block with
      | Llvm.After instr ->
        let op = Llvm.instr_opcode instr in
        if op = Llvm.Opcode.Ret then ()
        else ignore (Llvm.build_ret (Llvm.const_int i32_t 0) llbuilder)
      | Llvm.At_start _ -> ignore (Llvm.build_ret (Llvm.const_int i32_t 0) llbuilder)
    end
  | _ -> raise InvalidMainReturnType ;;


(* Function Definition -> LLVM Function Store *)
let codegen_function (d_type : datatype) (fname : string) (params : statement list) (stmts : statement list) : unit =
  match fname with
  | "main" -> codegen_main stmts d_type
  | _ ->
    Hashtbl.clear named_values;
    Hashtbl.clear named_parameters;

    let f : Llvm.llvalue = llvm_lookup_function fname in
    let llbuilder : Llvm.llbuilder = Llvm.builder_at_end context (Llvm.entry_block f) in

    let () = init_params f params in
    let _ : Llvm.llvalue = codegen_stmt (Block (stmts)) llbuilder in

    let last_basic_block =
      match Llvm.block_end (llvm_lookup_function fname) with
      | Llvm.After block  -> block
      | Llvm.At_start _   -> raise (FunctionWithoutBasicBlock fname) in

    let return_t = Llvm.return_type (Llvm.type_of (llvm_lookup_function fname)) in

    match Llvm.instr_end last_basic_block with
    | Llvm.After instr ->
      let op : Llvm.Opcode.t = Llvm.instr_opcode instr in
      if op = Llvm.Opcode.Ret then ()
      else
        begin
          match return_t = void_t with
          | true  -> ignore (Llvm.build_ret_void llbuilder)
          | false -> ignore (Llvm.build_ret (Llvm.const_int i32_t 0) llbuilder)
        end
    | Llvm.At_start _ ->
      match return_t = void_t with
      | true  -> ignore (Llvm.build_ret_void llbuilder)
      | false -> ignore (Llvm.build_ret (Llvm.const_int i32_t 0) llbuilder) ;;

let codegen_library_functions () =
  let printf_t  : Llvm.lltype   = Llvm.var_arg_function_type i32_t [| Llvm.pointer_type i8_t |] in
  let _         : Llvm.llvalue  = Llvm.declare_function "printf" printf_t the_module in
  let malloc_t  : Llvm.lltype   = Llvm.function_type (str_t) [| i32_t |] in
  let _         : Llvm.llvalue  = Llvm.declare_function "malloc" malloc_t the_module in
  let open_t    : Llvm.lltype   = Llvm.function_type i32_t [| (Llvm.pointer_type i8_t); i32_t |] in
  let _         : Llvm.llvalue  = Llvm.declare_function "open" open_t the_module in
  let close_t   : Llvm.lltype   = Llvm.function_type i32_t [| i32_t |] in
  let _         : Llvm.llvalue  = Llvm.declare_function "close" close_t the_module in
  let read_t    : Llvm.lltype   = Llvm.function_type i32_t [| i32_t; Llvm.pointer_type i8_t; i32_t |] in
  let _         : Llvm.llvalue  = Llvm.declare_function "read" read_t the_module in
  let write_t   : Llvm.lltype   = Llvm.function_type i32_t [| i32_t; Llvm.pointer_type i8_t; i32_t |] in
  let _         : Llvm.llvalue  = Llvm.declare_function "write" write_t the_module in
  let lseek_t   : Llvm.lltype   = Llvm.function_type i32_t [| i32_t; i32_t; i32_t |] in
  let _         : Llvm.llvalue  = Llvm.declare_function "lseek" lseek_t the_module in
  let exit_t    : Llvm.lltype   = Llvm.function_type void_t [| i32_t |] in
  let _         : Llvm.llvalue  = Llvm.declare_function "exit" exit_t the_module in
  let realloc_t : Llvm.lltype   = Llvm.function_type str_t [| str_t; i32_t |] in
  let _         : Llvm.llvalue  = Llvm.declare_function "realloc" realloc_t the_module in
  let getchar_t : Llvm.lltype   = Llvm.function_type (i32_t) [| |] in
  let _         : Llvm.llvalue  = Llvm.declare_function "getchar" getchar_t the_module in
  let sizeof_t  : Llvm.lltype   = Llvm.function_type (i32_t) [| i32_t |] in
  let _sizeof         : Llvm.llvalue  = Llvm.declare_function "sizeof" sizeof_t the_module in
  () ;;


let codegen_ast (ast : program) : Llvm.llmodule =
  (* Reserved functions in LLVM *)
  let () = codegen_library_functions () in
  (* Map statements to LLVM *)
  let _ : unit list = match ast with Program stmts ->
    List.map (fun stmt ->
      match stmt with
      | FuncDef (d_type, fname, params, stmts) -> codegen_function d_type fname params stmts
      | _ -> ignore (codegen_stmt stmt builder)
    ) stmts in
  the_module ;;
  
let print_module (file : string) (m : Llvm.llmodule) : unit =
  Llvm.print_module file m ;;
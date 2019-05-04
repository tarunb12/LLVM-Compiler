open Ast ;;
open Program ;;
open Exceptions ;;

let filename    : string          = Sys.argv.(2) ;;
let context     : Llvm.llcontext  = Llvm.global_context () ;;
let the_module  : Llvm.llmodule   = Llvm.create_module context filename ;;
let builder     : Llvm.llbuilder  = Llvm.builder context ;;
let i32_t       : Llvm.lltype     = Llvm.i32_type context ;;
let i8_t        : Llvm.lltype     = Llvm.i8_type context ;;
let i1_t        : Llvm.lltype     = Llvm.i1_type context ;;
let float_t     : Llvm.lltype     = Llvm.float_type context ;;
let double_t    : Llvm.lltype     = Llvm.double_type context ;;
let void_t      : Llvm.lltype     = Llvm.void_type context ;;
let str_t       : Llvm.lltype     = Llvm.pointer_type (Llvm.i8_type context) ;;

let break_block     : Llvm.llbasicblock ref = ref (Llvm.block_of_value (Llvm.const_int i32_t 0))
let continue_block  : Llvm.llbasicblock ref = ref (Llvm.block_of_value (Llvm.const_int i32_t 0))
let is_loop         : bool ref              = ref false ;;
let main_defined    : bool ref              = ref false ;;

let named_values      : (string, Llvm.llvalue) Hashtbl.t = Hashtbl.create 50 ;;
let named_parameters  : (string, Llvm.llvalue) Hashtbl.t = Hashtbl.create 50 ;;

(* Used for datatype -> lltype, lltype -> datatype *)
let types : (datatype * Llvm.lltype) list = [
  Int_t, i32_t;
  Float_t, float_t;
  Bool_t, i1_t;
  Char_t, i8_t;
  String_t, str_t;
  Unit_t, void_t;
] ;;

(* Function Name -> LLVM Function *)
let llvm_lookup_function (fname : string) : Llvm.llvalue =
  match Llvm.lookup_function fname the_module with
  | None    -> raise (LLVMFunctionNotFound (fname))
  | Some f  -> f ;;

(* Data Type -> LLVM Type *)
let lltype_of_datatype (d_type : datatype) : Llvm.lltype =
  try snd (List.find (fun t -> d_type = fst t) types)
  with Not_found -> void_t ;;

(* LLVM Type -> Data Type *)
let datatype_of_lltype (lltype : Llvm.lltype) : datatype =
  try fst (List.find (fun t -> lltype = snd t) types)
  with Not_found -> Unit_t ;;

(* Statement -> LLVM Statement Execution *)
let rec codegen_stmt ~(llbuilder : Llvm.llbuilder) : statement -> Llvm.llvalue = function
  | Block stmt_list               -> begin try List.hd (List.map (fun stmt -> codegen_stmt stmt ~llbuilder) stmt_list) with Failure _ -> Llvm.const_null i1_t end
  | If (cond, t_stmt, f_stmt)     -> codegen_if cond t_stmt f_stmt ~llbuilder
  | For (init, cond, incr, stmt)  -> codegen_for init cond incr stmt ~llbuilder
  | While (cond, stmt)            -> codegen_while cond stmt ~llbuilder
  | Expr expr                     -> codegen_expr expr ~llbuilder
  | VarDef (d_type, vname, e)     -> codegen_vardef vname d_type e ~llbuilder
  | Return expr                   -> codegen_return expr ~llbuilder
  | Break                         -> codegen_break ~llbuilder
  | Continue                      -> codegen_continue ~llbuilder
  | FuncDef (_, fname, _, _)      -> raise (NestedFunctionsNotSupported fname)

(* Expression -> LLVM Expression Evaluation *)
and codegen_expr ~(llbuilder : Llvm.llbuilder) : expr -> Llvm.llvalue = function
  | FloatLit flt          -> Llvm.const_float float_t flt
  | IntLit int            -> Llvm.const_int i32_t int
  | BoolLit bool          -> if bool then Llvm.const_int i1_t 1 else Llvm.const_int i1_t 0
  | CharLit char          -> Llvm.const_int i8_t (int_of_char char)
  | StringLit str         -> Llvm.build_global_stringptr str "tmp" llbuilder
  | BinOp (op, e1, e2)    -> handle_binop op e1 e2 ~llbuilder
  | UnOp (op, e)          -> handle_unop op e ~llbuilder
  | Assign (e1, e2)       -> codegen_assign e1 e2 ~llbuilder
  | Id id                 -> codegen_id id ~llbuilder
  | Call (fname, params)  -> codegen_call fname params ~llbuilder
  | Noexpr                -> Llvm.build_add (Llvm.const_int i32_t 0) (Llvm.const_int i32_t 0) "nop" llbuilder

(* Binary Expression -> LLVM Value *)
and handle_binop ~(llbuilder : Llvm.llbuilder) (op : binOp) (e1 : expr) (e2 : expr) : Llvm.llvalue =
  let expr1 : Llvm.llvalue = codegen_expr e1 ~llbuilder in
  let expr2 : Llvm.llvalue = codegen_expr e2 ~llbuilder in
  let expr1_t : datatype = datatype_of_lltype (Llvm.type_of expr1) in
  let expr2_t : datatype = datatype_of_lltype (Llvm.type_of expr2) in

  let int_ops (expr1 : Llvm.llvalue) (expr2 : Llvm.llvalue) : binOp -> Llvm.llvalue = function
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

  let float_ops (expr1 : Llvm.llvalue) (expr2 : Llvm.llvalue) : binOp -> Llvm.llvalue = function
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
    | _       -> raise (InvalidBinaryOperation (op, expr1_t, expr2_t)) in
  
  let type_handler (data_t : datatype) : Llvm.llvalue =
    match data_t with
    | Int_t | Bool_t | Char_t -> int_ops expr1 expr2 op
    | Float_t                 -> float_ops expr1 expr2 op
    | _                       -> raise (InvalidBinaryOperation (op, expr1_t, expr2_t)) in

  let data_t : datatype = binop_type_of_types op expr1_t expr2_t in
  type_handler data_t

(* Unary Expression -> LLVM Value *)
and handle_unop ~(llbuilder : Llvm.llbuilder) (op : unOp) (expr : expr) : Llvm.llvalue =
  let expr : Llvm.llvalue = codegen_expr expr ~llbuilder in
  let expr_t : datatype = datatype_of_lltype (Llvm.type_of expr) in

  let un_ops (expr : Llvm.llvalue) : unOp * datatype -> Llvm.llvalue = function
    | Neg, Int_t    -> Llvm.build_neg expr "i_unoptmp" llbuilder
    | Neg, Float_t  -> Llvm.build_fneg expr "f_unoptmp" llbuilder
    | Not, Bool_t   -> Llvm.build_not expr "b_unoptmp" llbuilder
    | _             -> raise (InvalidUnaryOperation (op, expr_t)) in

  let type_handler (data_t : datatype) : Llvm.llvalue =
    match data_t with
    | Int_t | Float_t | Bool_t  -> un_ops expr (op, data_t)
    | _                         -> raise (InvalidUnaryOperation (op, data_t)) in

  type_handler expr_t

(* Expression Assignment -> LLVM Change Value *)
and codegen_assign ~(llbuilder : Llvm.llbuilder) (expr1 : expr) (expr2 : expr) : Llvm.llvalue =
  let rhs : Llvm.llvalue = codegen_expr expr2 ~llbuilder in
  let lhs : Llvm.llvalue =
    match expr1 with
    | Id id ->
      begin
        try Hashtbl.find named_values id
        with Not_found ->
          try ignore (Hashtbl.find named_parameters id); raise (CannotRedefineParameter id);
          with Not_found -> raise (UndefinedId id)
      end
    | _ -> raise (LeftHandSideUnassignable expr1) in

  ignore (Llvm.build_store rhs lhs llbuilder);
  rhs

(* Variable definition -> LLVM Store Variable, Value *)
and codegen_vardef ~(llbuilder : Llvm.llbuilder) (vname : string) (data_t : datatype) (expr : expr) : Llvm.llvalue =
  let expr_val : Llvm.llvalue = codegen_expr expr ~llbuilder in
  let expr_t : datatype = datatype_of_lltype (Llvm.type_of expr_val) in
  match data_t = expr_t || expr_t = Unit_t with
  | true ->
    begin
      let lltype : Llvm.lltype = lltype_of_datatype data_t in
      let malloc : Llvm.llvalue = Llvm.build_malloc lltype vname llbuilder in

      Hashtbl.add named_values vname malloc;

      let lhs : expr = Id vname in
      match expr with
      | Noexpr  -> malloc
      | _       -> codegen_assign lhs expr ~llbuilder
    end
  | false -> raise (InvalidDefinitionType (vname, data_t, expr_t))

(* ID -> Hashtable Lookup -> LLVM Load Instruction *)
and codegen_id ~(llbuilder : Llvm.llbuilder) (id : string) : Llvm.llvalue =
  try Hashtbl.find named_parameters id
  with Not_found ->
    try let value = Hashtbl.find named_values id in
      Llvm.build_load value id llbuilder
    with Not_found -> raise (UndefinedId id)

(* If Statement -> LLVM If *)
and codegen_if ~(llbuilder : Llvm.llbuilder) (cond : expr) (t_stmt : statement) (f_stmt : statement) : Llvm.llvalue =
  let cond_eval : Llvm.llvalue = codegen_expr cond ~llbuilder in
  let cond_t : datatype = datatype_of_lltype (Llvm.type_of cond_eval) in
  match cond_t with
  | Bool_t ->
    begin
      let start_bb : Llvm.llbasicblock = Llvm.insertion_block llbuilder in
      let func : Llvm.llvalue = Llvm.block_parent start_bb in

      let if_bb : Llvm.llbasicblock = Llvm.append_block context "if" func in
      Llvm.position_at_end if_bb llbuilder;

      let _ : Llvm.llvalue = codegen_stmt t_stmt ~llbuilder in
      let new_if_bb : Llvm.llbasicblock = Llvm.insertion_block llbuilder in

      let else_bb : Llvm.llbasicblock = Llvm.append_block context "else" func in
      Llvm.position_at_end else_bb llbuilder;

      let _ : Llvm.llvalue = codegen_stmt f_stmt ~llbuilder in
      let new_else_bb : Llvm.llbasicblock = Llvm.insertion_block llbuilder in

      let merge_bb : Llvm.llbasicblock = Llvm.append_block context "ifcont" func in
      Llvm.position_at_end merge_bb llbuilder;

      let else_bb_val : Llvm.llvalue = Llvm.value_of_block new_else_bb in
      Llvm.position_at_end start_bb llbuilder;

      ignore (Llvm.build_cond_br cond_eval if_bb else_bb llbuilder);
      Llvm.position_at_end new_if_bb llbuilder; ignore (Llvm.build_br merge_bb llbuilder);
      Llvm.position_at_end new_else_bb llbuilder; ignore (Llvm.build_br merge_bb llbuilder);
      Llvm.position_at_end merge_bb llbuilder;
      else_bb_val
    end
  | _ -> raise (InvalidConditionType cond_t)

(* For Statement -> LLVM Loop *)
and codegen_for ~(llbuilder : Llvm.llbuilder) (init : expr) (cond : expr) (incr : expr) (stmt : statement) : Llvm.llvalue =
  let already_in_loop = !is_loop in
  is_loop := true;

  let f : Llvm.llvalue = Llvm.block_parent (Llvm.insertion_block llbuilder) in
  let _ : Llvm.llvalue = codegen_expr init ~llbuilder in

  let loop_bb : Llvm.llbasicblock = Llvm.append_block context "loop" f in
  let incr_bb : Llvm.llbasicblock = Llvm.append_block context "incr" f in
  let cond_bb : Llvm.llbasicblock = Llvm.append_block context "cond" f in
  let after_bb : Llvm.llbasicblock = Llvm.append_block context "after_loop" f in

  let () = if not already_in_loop then
    continue_block := incr_bb;
    break_block := after_bb in
  ignore (Llvm.build_br cond_bb llbuilder);

  Llvm.position_at_end loop_bb llbuilder;
  ignore (codegen_stmt stmt ~llbuilder);

  let bb = Llvm.insertion_block llbuilder in
  Llvm.move_block_after bb incr_bb;
  Llvm.move_block_after incr_bb cond_bb;
  Llvm.move_block_after cond_bb after_bb;
  ignore (Llvm.build_br incr_bb llbuilder);

  Llvm.position_at_end incr_bb llbuilder;

  let _ = codegen_expr incr ~llbuilder in
  ignore(Llvm.build_br cond_bb llbuilder);

  Llvm.position_at_end cond_bb llbuilder;

  let cond_eval : Llvm.llvalue = codegen_expr cond ~llbuilder in
  ignore (Llvm.build_cond_br cond_eval loop_bb after_bb llbuilder);
  Llvm.position_at_end after_bb llbuilder;
  is_loop := already_in_loop;
  Llvm.const_null float_t

(* While Statement -> For Statement -> LLVM Loop *)
and codegen_while ~(llbuilder : Llvm.llbuilder) (cond : expr) (stmt : statement) : Llvm.llvalue =
  let null_expr = IntLit 0 in
  codegen_for null_expr cond null_expr stmt ~llbuilder

(* Return Statement -> LLVM Return Instr *)
and codegen_return ~(llbuilder : Llvm.llbuilder) (expr : expr) : Llvm.llvalue =
  match expr with
  | Noexpr  -> Llvm.build_ret_void llbuilder
  | _       -> Llvm.build_ret (codegen_expr expr ~llbuilder) llbuilder

(* Break Statement -> LLVM Break *)
and codegen_break ~(llbuilder : Llvm.llbuilder) : Llvm.llvalue =
  let b = fun () -> !break_block in
  Llvm.build_br (b ()) llbuilder

(* Continue Statement -> LLVM Continue *)
and codegen_continue ~(llbuilder : Llvm.llbuilder) : Llvm.llvalue =
  let b = fun () -> !continue_block in
  Llvm.build_br (b ()) llbuilder

(* Function Call -> LLVM Branch *)
and codegen_function_call ~(llbuilder : Llvm.llbuilder) (fname : string) (params : expr list) =
  let call_f (f : Llvm.llvalue) : Llvm.llvalue =
    let params = List.map (codegen_expr ~llbuilder) params in
    Llvm.build_call f (Array.of_list params) "tmp" llbuilder in

  let f = llvm_lookup_function fname in
  call_f f

(* Function Call -> LLVM Function Lookup/Execution *)
and codegen_call ~(llbuilder : Llvm.llbuilder) (fname : string) (params : expr list) : Llvm.llvalue =
  match fname with
  | "printf"  -> codegen_printf params ~llbuilder
  | _         -> codegen_function_call fname params ~llbuilder

(* Printf -> LLVM Print Funciton *)
and codegen_printf ~(llbuilder : Llvm.llbuilder) (params : expr list) : Llvm.llvalue =
  let format_str : expr = List.hd params in
  let format_llstr : Llvm.llvalue =
    match format_str with
    | StringLit str -> Llvm.build_global_stringptr str "fmt" llbuilder
    | Id id ->
      begin
        let value : Llvm.llvalue = codegen_id id ~llbuilder in
        match datatype_of_lltype (Llvm.type_of value) with
        | String_t  -> value
        | _         -> raise (FirstPrintArgumentNotString format_str)
      end
    | _  -> raise (FirstPrintArgumentNotString format_str) in

  let args : expr list = List.tl params in
  let format_llargs : Llvm.llvalue list = List.map (fun arg ->
    let arg_eval : Llvm.llvalue = codegen_expr arg ~llbuilder in
    let arg_t : datatype = datatype_of_lltype (Llvm.type_of arg_eval) in
    match arg_t with
    | Float_t -> Llvm.const_fpext arg_eval double_t
    | _       -> arg_eval
  ) args in

  let func_llvalue : Llvm.llvalue = llvm_lookup_function "printf" in
  let llargs : Llvm.llvalue array = Array.of_list (format_llstr :: format_llargs) in
  Llvm.build_call func_llvalue llargs "printf" llbuilder ;;

let rec get_return_stmts (stmts : statement list) : statement list =
  match stmts with
  | [] -> []
  | hd :: tl ->
    match hd with
    | Block stmts -> get_return_stmts stmts @ get_return_stmts tl
    | If (_, t_stmt, f_stmt) -> get_return_stmts ([t_stmt]) @ get_return_stmts ([f_stmt]) @ get_return_stmts tl
    | For (_, _, _, stmt) -> get_return_stmts ([stmt]) @ get_return_stmts tl
    | While (_, stmt) -> get_return_stmts ([stmt]) @ get_return_stmts tl
    | Return expr -> hd :: get_return_stmts tl
    | _ -> get_return_stmts tl ;;

let rec check_valid_return_type (d_type : datatype) (f_type : Llvm.lltype) (fname : string) : expr -> unit = function
  | BinOp (_, e1, e2) -> check_valid_return_type d_type f_type fname e1; check_valid_return_type d_type f_type fname e2
  | UnOp (_, e) -> check_valid_return_type d_type f_type fname e
  | Call (f_name, _) as expr ->
    begin
      match fname = f_name with
      | true  -> ()
      | false ->
        let return_t : datatype = datatype_of_lltype (Llvm.return_type (Llvm.type_of (llvm_lookup_function f_name))) in
        match return_t = d_type with
        | true  -> ()
        | false -> raise (InvalidFunctionReturnType (fname, return_t, d_type, expr))
    end
  | Id _ | Assign _ | Noexpr as expr ->
    begin
      match datatype_of_lltype f_type = Unit_t with
      | true  -> ()
      | false -> raise (InvalidFunctionReturnType (fname, Unit_t, d_type, expr))
    end
  | _ as expr ->
    let expr_t : datatype = get_expr_type expr in
    match expr_t = datatype_of_lltype f_type with
    | true  -> ()
    | false -> raise (InvalidFunctionReturnType (fname, expr_t, d_type, expr)) ;;  

(* Initialization of Function Parameters *)
let init_params (f : Llvm.llvalue) (args : statement list) (fname : string) : unit =
  let args = Array.of_list args in
  Array.iteri (fun i element ->
    let param : statement = args.(i) in
    let named_param =
      match param with
      | VarDef (_, name, _) -> name
      | _                   -> raise (InvalidParameterType fname) in
    Llvm.set_value_name named_param element;
    Hashtbl.add named_parameters named_param element;
  ) (Llvm.params f) ;;

(* Main Function Definition -> LLVM Main Function Store *)
let codegen_main (stmts : statement list) (d_type : datatype) : unit =
  main_defined := true;
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
      
      let _ : Llvm.llvalue = codegen_stmt (Block stmts) ~llbuilder in

      let last_basic_block : Llvm.llbasicblock =
        match Llvm.block_end (llvm_lookup_function "main") with
        | Llvm.After block  -> block
        | _ -> raise (FunctionWithoutBasicBlock "main") in

      let return_t : Llvm.lltype = Llvm.return_type (Llvm.type_of (llvm_lookup_function "main")) in
      if datatype_of_lltype return_t <> datatype_of_lltype i32_t then
        if datatype_of_lltype return_t <> datatype_of_lltype void_t then
          raise (InvalidMainReturnType (datatype_of_lltype return_t));

      match Llvm.instr_end last_basic_block with
      | Llvm.After instr ->
        let op = Llvm.instr_opcode instr in
        if op = Llvm.Opcode.Ret then ()
        else ignore (Llvm.build_ret (Llvm.const_int i32_t 0) llbuilder)
      | Llvm.At_start _ -> ignore (Llvm.build_ret (Llvm.const_int i32_t 0) llbuilder)
    end
  | _ -> raise (InvalidMainReturnType d_type) ;;

(* Function Definition -> LLVM Function Store *)
let codegen_function (d_type : datatype) (fname : string) (params : statement list) (stmts : statement list) : unit =
  match fname with
  | "main" ->
    if !main_defined then raise MultipleEntryPoints
    else codegen_main stmts d_type
  | _ ->
    Hashtbl.clear named_values;
    Hashtbl.clear named_parameters;

    let f : Llvm.llvalue = llvm_lookup_function fname in
    let llbuilder : Llvm.llbuilder = Llvm.builder_at_end context (Llvm.entry_block f) in

    let () = init_params f params fname in
    try
      let _ : Llvm.llvalue = codegen_stmt (Block (stmts)) ~llbuilder in
      let last_basic_block =
        match Llvm.block_end (llvm_lookup_function fname) with
        | Llvm.After block  -> block
        | Llvm.At_start _   -> raise (FunctionWithoutBasicBlock fname) in

      let return_t : Llvm.lltype = Llvm.return_type (Llvm.type_of (llvm_lookup_function fname)) in
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
        | false -> ignore (Llvm.build_ret (Llvm.const_int i32_t 0) llbuilder);
      Llvm_analysis.assert_valid_function f
    with e -> Llvm.delete_function f; raise e;;

(* Define Function -> LLVM Routine *)
let codegen_function_def (d_type : datatype) (fname : string) (params : statement list) (stmts : statement list) : unit =
  match fname with
  | "main" -> ()
  | _ ->
    let is_var_arg : bool ref = ref false in
    let params : Llvm.lltype list = List.rev (List.fold_left (fun expr -> function
      | VarDef (d_type, _, Noexpr) -> lltype_of_datatype d_type :: expr
      | _ -> is_var_arg := true; expr
    ) [] params) in
    let f_type : Llvm.lltype =
      match !is_var_arg with
      | true  -> Llvm.var_arg_function_type (lltype_of_datatype d_type) (Array.of_list params)
      | false -> Llvm.function_type (lltype_of_datatype d_type) (Array.of_list params) in
    let return_t : Llvm.lltype = Llvm.return_type f_type in
    let return_stmts : statement list = get_return_stmts stmts in
    match return_stmts with
    | [] ->
      begin
        match datatype_of_lltype return_t = Unit_t with
        | true  -> ()
        | false -> raise (InvalidFunctionWithoutReturn (fname, d_type))
      end
    | _ ->
      List.iter (fun stmt ->
        match stmt with
        | Return expr -> check_valid_return_type d_type return_t fname expr
        | _ -> ()
      ) return_stmts;
    ignore (Llvm.define_function fname f_type the_module) ;;

(* Built In LLVM Functions *)
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
  let _         : Llvm.llvalue  = Llvm.declare_function "sizeof" sizeof_t the_module in
  () ;;

(* Delete All LLVM Functions *)
let delete_functions (m : Llvm.llmodule) () =
  try
    let main = llvm_lookup_function "main" in
    Llvm.delete_function main;
    Llvm.iter_functions (fun func ->
      Llvm.iter_blocks Llvm.delete_block func;
      Llvm.delete_function func;
    ) m;
    main_defined := false;
  with Invalid_argument _ | LLVMFunctionNotFound _ -> main_defined := false ;;

(* Program -> LLVM Code Generation -> LLVM Module (Code) *)
let codegen_ast (ast : program) : Llvm.llmodule =
  (* Reserved functions in LLVM *)
  let () = codegen_library_functions () in
  (* Map statements to LLVM *)
  let _ : unit list =
    match ast with Program stmts ->
    try
      List.map (fun stmt ->
        match stmt with
        | FuncDef (d_type, fname, params, stmts) ->
          codegen_function_def d_type fname params stmts;
          codegen_function d_type fname params stmts
        | _ -> ignore (codegen_stmt stmt ~llbuilder:builder)
      ) stmts
      with e -> delete_functions the_module (); raise e in
  match !main_defined with
  | true  -> the_module
  | false -> raise MainMethodNotDefined ;;

(* Print Module (Code) -> %.ll *)
let print_module (file : string) (m : Llvm.llmodule) : unit =
  Llvm.print_module file m ;;
open Ast ;;

(* Parser exceptions *)
exception SyntaxError of int * string

(* Type exceptions *)
exception InvalidBinaryOperation of binOp * datatype * datatype
exception InvalidDefinitionType of string * datatype * datatype
exception InvalidMainReturnType of datatype
exception InvalidParameterType of statement
exception InvalidUnaryOperation of unOp * datatype

(* Code generation exceptions *)
exception FirstPrintArgumentNotString of expr
exception FunctionWithoutBasicBlock of string
exception LeftHandSideUnassignable of expr
exception LLVMFunctionNotFound of string
exception UndefinedId of string

(* General exceptions *)
exception NotImplemented
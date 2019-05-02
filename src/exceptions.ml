open Ast ;;

(* Parser exceptions *)
exception SyntaxError of int * string

(* Code generation exceptions *)
exception FirstPrintArgumentNotString of expr
exception FunctionWithoutBasicBlock of string
exception InvalidBinaryOperation of binOp * datatype * datatype
exception InvalidDataType of string
exception InvalidDefinitionType of string * datatype * datatype
exception InvalidMainReturnType of datatype
exception InvalidParameterType
exception InvalidUnaryOperation of unOp * datatype
exception LeftHandSideUnassignable of expr
exception LLVMFunctionNotFound of string
exception NotImplemented
exception UndefinedId of string
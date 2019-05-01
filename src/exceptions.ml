open Ast ;;

(* Parser exceptions *)
exception SyntaxError of int * string

(* Code generation exceptions *)
exception FirstPrintArgumentNotString
exception FloatOpNotSupported
exception FunctionWithoutBasicBlock of string
exception IntOpNotSupported
exception InvalidBinaryOperation of binOp * datatype * datatype
exception InvalidDataType of string
exception InvalidDefinitionType of string
exception InvalidMainReturnType
exception InvalidParameterType
exception InvalidUnaryOperation of unOp * datatype
exception InvalidUnOpType
exception LeftHandSideUnassignable
exception LLVMFunctionNotFound of string
exception NotImplemented
exception UndefinedId of string
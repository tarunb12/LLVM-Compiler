open Ast ;;

(* Parser exceptions *)
exception SyntaxError of int * string

(* Type exceptions *)
exception InvalidBinaryOperation of binOp * datatype * datatype
exception InvalidConditionType of datatype
exception InvalidDefinitionType of string * datatype * datatype
exception InvalidFunctionReturnType of string * datatype * datatype * expr
exception InvalidFunctionWithoutReturn of string * datatype
exception InvalidMainReturnType of datatype
exception InvalidParameterType of string
exception InvalidUnaryOperation of unOp * datatype

(* Code generation exceptions *)
exception CannotRedefineParameter of string
exception FirstPrintArgumentNotString of expr
exception FunctionWithoutBasicBlock of string
exception LeftHandSideUnassignable of expr
exception LLVMFunctionNotFound of string
exception MainMethodNotDefined
exception MultipleEntryPoints
exception NestedFunctionsNotSupported of string
exception UndefinedId of string
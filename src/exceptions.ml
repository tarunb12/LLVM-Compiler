(* Parser exceptions *)
exception SyntaxError of int * string

(* Code generation exceptions *)
exception BinOpNotSupported
exception CannotMixDatatypes
exception FirstPrintArgumentNotString
exception FloatOpNotSupported
exception FunctionNotFound of string
exception FunctionWithoutBasicBlock of string
exception IntOpNotSupported
exception InvalidDataType of string
exception InvalidDefinitionType of string
exception InvalidMainReturnType
exception InvalidParameterType
exception InvalidUnOpType
exception LeftHandSideUnassignable
exception LLVMFunctionNotFound of string
exception NotImplemented
exception UndefinedId of string
exception UnOpNotSupported
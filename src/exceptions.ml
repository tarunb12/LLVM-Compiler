(* Parser exceptions *)
exception SyntaxError of string

(* Code generation exceptions *)
exception NotImplemented
exception IntOpNotSupported
exception FloatOpNotSupported
exception UnOpNotSupported
exception InvalidUnOpType
exception BinOpNotSupported
exception CannotMixDatatypes
exception FunctionNotFound of string
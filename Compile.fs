module NanoML.Compiler.Emitter

open NanoML.Compiler.Ast
open NanoML.VirtualMachine

let rec emit = function
    | Var x -> [ILdVar x]
    | Int i -> [ILdInt i]
    | Float f -> [ILdFloat f]
    | Bool b -> [ILdBool b]
    // TODO: add type checking and generate appropriate "opcodes"
    | Times (e1, e2) -> (emit e1) @ (emit e2) @ [IMulti]
    | Minus (e1, e2) -> (emit e1) @ (emit e2) @ [ISubi]
    | Plus (e1, e2) -> (emit e1) @ (emit e2) @ [IAddi]
    | Divide (e1, e2) -> (emit e1) @ (emit e2) @ [IDivi]
    | Equal (e1, e2) -> (emit e1) @ (emit e2) @ [IEquali]
    | Less (e1, e2) -> (emit e1) @ (emit e2) @ [ILessi]
    | Fun (f, x, _, _, e) -> [ILdClosure (f, x, emit e @ [IPopEnv])]
    | Cond (e1, e2, e3) -> (emit e1) @ [IBranch (emit e2, emit e3)]
    | Apply (e1, e2) -> (emit e1) @ (emit e2) @ [ICall]

module NanoML.Compiler.Emitter
open NanoML.Utils
open NanoML.Compiler.Ast
open NanoML.Compiler.TAst
open NanoML.VirtualMachine

let private times =
    let map = Map.ofList [(TyInt, IMulti); (TyFloat, IMultf)] 
    fun ty -> map.[ty]


let private minus =
    let map = Map.ofList [(TyInt, ISubi); (TyFloat, ISubf)]
    fun ty -> map.[ty]


let private plus = 
    let map = Map.ofList [(TyInt, IAddi); (TyFloat, IAddf)]
    fun ty -> map.[ty]


let private div = 
    let map = Map.ofList [(TyInt, IDivi); (TyFloat, IDivf)]
    fun ty -> map.[ty]


let private eq =
    let map = Map.ofList [(TyInt, IEquali); (TyFloat, IEqualf)]
    fun ty -> map.[ty]


let private less =
    let map = Map.ofList [(TyInt, ILessi); (TyFloat, ILessf)]
    fun ty -> map.[ty]


let rec emit = function
    | TVar (x, _) -> [ILdVar x]
    | TInt i -> [ILdInt i]
    | TFloat f -> [ILdFloat f]
    | TBool b -> [ILdBool b]
    | TTimes (e1, e2, ty) -> (emit e1) @ (emit e2) @ [times ty]
    | TMinus (e1, e2, ty) -> (emit e1) @ (emit e2) @ [minus ty]
    | TPlus (e1, e2, ty) -> (emit e1) @ (emit e2) @ [plus ty]
    | TDivide (e1, e2, ty) -> (emit e1) @ (emit e2) @ [div ty]
    | TEqual (e1, e2) -> (emit e1) @ (emit e2) @ [eq e1.Type]
    | TLess (e1, e2) -> (emit e1) @ (emit e2) @ [less e1.Type]
    | TFun (f, arg, _, _, e, _) -> [ILdClosure (f, arg, emit e @ [IPopEnv])]
    | TCond (e1, e2, e3, _) -> (emit e1) @ [IBranch (emit e2, emit e3)]
    | TApply (e1, e2, _) -> (emit e1) @ (emit e2) @ [ICall]
    | astNode -> failwithf "Not valid node for emitter: %A" astNode

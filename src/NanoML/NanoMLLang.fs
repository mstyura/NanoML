module NanoML.Language.Basics
open NanoML.VirtualMachine
open NanoML.Compiler.Ast

let private addToFloat ctx env =
    let c' = ref Unchecked.defaultof<mvalue>
    let frm = [ILdVar (Name "x"); IConvI2F; IPopEnv]
    let env' = (Name "toFloat", c') :: env
    let c = MClosure (Name "x", frm, env')
    c' := c
    (Name "toFloat", TyFun(TyInt, TyFloat)) :: ctx, env'


let private addToInt ctx env =
    let c' = ref Unchecked.defaultof<mvalue>
    let frm = [ILdVar (Name "x"); IConvF2I; IPopEnv]
    let env' = (Name "toInt", c') :: env
    let c = MClosure (Name "x", frm, env')
    c' := c
    (Name "toInt", TyFun(TyFloat, TyInt)) :: ctx, env'


let private predefined ctx env =
    (ctx, env) ||> addToFloat ||> addToInt


let initialContextAndEnv =
    predefined [] []
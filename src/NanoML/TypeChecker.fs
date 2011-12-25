module NanoML.Compiler.TypeChecker

open Ast
open TAst
open NanoML.Utils

exception TypeError of string


let typeError msg = raise (TypeError msg)


let rec check ctx ty e =
    let ty' : texpr = typeOf ctx e
    if ty'.Type <> ty then
        typeError (sprintf "%s has type %s but is used as if it has type %s" (string e) (string ty') (string ty))

and checkBinOp ctx e1 e2 types =
    match (typeOf ctx e1, typeOf ctx e2) with
    | (texpr1 : texpr, texpr2 : texpr) when texpr1.Type = texpr2.Type ->
        if List.exists ((=) texpr1.Type) types then
            texpr1, texpr2, texpr1.Type
        else
            typeError (sprintf "Operator not defined for type: %O" texpr1.Type)
    | (ty1, ty2) -> typeError (sprintf "Operator can't be applied for different types %s and %s" (string ty1.Type) (string ty2.Type))

and typeOf ctx = function
    | Var x -> match List.assoc x ctx with Some ty -> TVar (x, ty) | _ -> typeError (sprintf "Undefined variable: %O" x)
    | Int i -> TInt i
    | Float f -> TFloat f
    | Bool b -> TBool b
    | Times (e1, e2) -> TTimes (checkBinOp ctx e1 e2 [TyInt; TyFloat])
    | Plus (e1, e2) -> TPlus (checkBinOp ctx e1 e2 [TyInt; TyFloat])
    | Minus (e1, e2) -> TMinus (checkBinOp ctx e1 e2 [TyInt; TyFloat])
    | Divide (e1, e2) -> TDivide (checkBinOp ctx e1 e2 [TyInt; TyFloat])

    | Equal (e1, e2) -> 
        let te1, te2, _ = checkBinOp ctx e1 e2 [TyInt; TyFloat; TyBool] 
        TEqual (te1, te2)

    | Less (e1, e2) -> 
        let te1, te2, _ = checkBinOp ctx e1 e2 [TyInt; TyFloat] 
        TLess (te1, te2)

    | Cond (e1, e2, e3) ->
        check ctx TyBool e1;
        let ty = typeOf ctx e2
        check ctx ty.Type e3 |> ignore; ty

    | Fun (f, x, ty1, ty2, e) ->
        let ctx = (x, ty1) :: (f, TyFun(ty1, ty2)) :: ctx
        check ctx ty2 e
        TFun(f, x, ty1, ty2, typeOf ctx e, TyFun (ty1, ty2))

    | Apply (e1, e2) ->
        match (typeOf ctx e1).Type with
        | TyFun (ty1, ty2) -> check ctx ty1 e2 |> ignore; TApply(typeOf ctx e1, typeOf ctx e2, ty2)
        | ty -> typeError (sprintf "%s has type %s which is not a function and can't be applied" (string e1) (string ty))


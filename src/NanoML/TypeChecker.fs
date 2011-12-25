module NanoML.Compiler.TypeChecker

open Ast
open NanoML.Utils

exception TypeError of string


let typeError msg = raise (TypeError msg)


let rec check ctx ty e =
    let ty' = typeOf ctx e
    if ty' <> ty then
        typeError (sprintf "%s has type %s but is used as if it has type %s" (string e) (string ty') (string ty))

and checkBinOp ctx e1 e2 types =
    match (typeOf ctx e1, typeOf ctx e2) with
    | (ty1, ty2) when ty1 = ty2 ->
        if List.exists ((=) ty1) types then
            ty1
        else
            typeError ("Operator not defined for type" + string ty1)
    | (ty1, ty2) -> typeError (sprintf "Operator can't be applied for different types %s and %s" (string ty1) (string ty2))

and typeOf ctx = function
    | Var x -> match List.assoc x ctx with Some v -> v | _ -> typeError ("undefined variable" + string x)
    | Int _ -> TyInt
    | Float _ -> TyFloat
    | Bool _ -> TyBool
    | Times (e1, e2) -> checkBinOp ctx e1 e2 [TyInt; TyFloat]
    | Plus (e1, e2) -> checkBinOp ctx e1 e2 [TyInt; TyFloat]
    | Minus (e1, e2) -> checkBinOp ctx e1 e2 [TyInt; TyFloat]
    | Divide (e1, e2) -> checkBinOp ctx e1 e2 [TyInt; TyFloat]
    | Equal (e1, e2) -> checkBinOp ctx e1 e2 [TyInt; TyFloat; TyBool] |> ignore; TyBool
    | Less (e1, e2) -> checkBinOp ctx e1 e2 [TyInt; TyFloat] |> ignore; TyBool
    | Cond (e1, e2, e3) ->
        check ctx TyBool e1;
        let ty = typeOf ctx e2
        check ctx ty e3 |> ignore; ty
    | Fun (f, x, ty1, ty2, e) ->
        check ((x, ty1) :: (f, TyFun(ty1, ty2)) :: ctx) ty2 e
        TyFun (ty1, ty2)
    | Apply (e1, e2) ->
        match typeOf ctx e1 with
        | TyFun (ty1, ty2) -> check ctx ty1 e2 |> ignore; ty2
        | ty -> typeError (sprintf "%s has type %s which is not a function and can't be applied" (string e1) (string ty))
    
        

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
    | Int _ -> TInt
    | Float _ -> TFloat
    | Bool _ -> TBool
    | Times (e1, e2) -> checkBinOp ctx e1 e2 [TInt; TFloat]
    | Plus (e1, e2) -> checkBinOp ctx e1 e2 [TInt; TFloat]
    | Minus (e1, e2) -> checkBinOp ctx e1 e2 [TInt; TFloat]
    | Divide (e1, e2) -> checkBinOp ctx e1 e2 [TInt; TFloat]
    | Equal (e1, e2) -> checkBinOp ctx e1 e2 [TInt; TFloat; TBool] |> ignore; TBool
    | Less (e1, e2) -> checkBinOp ctx e1 e2 [TInt; TFloat] |> ignore; TBool
    | Cond (e1, e2, e3) ->
        check ctx TBool e1;
        let ty = typeOf ctx e2
        check ctx ty e3 |> ignore; ty
    | Fun (f, x, ty1, ty2, e) ->
        check ((x, ty1) :: (f, ty2) :: ctx) ty2 e
        TFun (ty1, ty2)
    | Apply (e1, e2) ->
        match typeOf ctx e1 with
        | TFun (ty1, ty2) -> check ctx ty1 e2 |> ignore; ty2
        | ty -> typeError (sprintf "%s has type %s which is not a function and can't be applied" (string e1) (string ty))
    
        

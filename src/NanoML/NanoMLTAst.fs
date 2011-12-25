module NanoML.Compiler.TAst
open NanoML.Compiler.Ast

type texpr =
    | TVar of name * ty
    | TInt of int
    | TFloat of float
    | TBool of bool
    | TTimes of texpr * texpr * ty
    | TPlus of texpr * texpr * ty
    | TMinus of texpr * texpr * ty
    | TDivide of texpr * texpr * ty
    | TEqual of texpr * texpr 
    | TLess of texpr * texpr
    | TCond of texpr * texpr * texpr * ty
    | TFun of name * name * ty * ty * texpr * ty
    | TApply of texpr * texpr * ty
    | TLetIn of name * texpr * texpr * ty
    
    member texpr.Type =
        match texpr with
        | TVar (_, ty) | TTimes (_, _, ty) | TPlus (_, _, ty) | TMinus (_, _, ty)
        | TDivide (_, _, ty) | TCond (_, _, _, ty) | TFun (_, _, _, _, _, ty) 
        | TApply (_, _, ty) | TLetIn (_, _, _, ty) -> ty
        | TInt _ -> TyInt | TFloat _ -> TyFloat 
        | TBool _ | TEqual _ | TLess _ -> TyBool


type ttoplevel_decl =
    | TExpr of texpr
    | TLetBinding of name * texpr




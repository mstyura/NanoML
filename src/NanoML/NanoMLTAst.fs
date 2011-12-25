module NanoML.Compiler.TAst
open NanoML.Compiler.Ast

type texpr =
    | TVar of name * ty
    | TInt of int * ty
    | TFloat of float * ty
    | TBool of bool * ty
    | TTimes of texpr * texpr * ty
    | TPlus of texpr * texpr * ty
    | TMinus of texpr * texpr * ty
    | TDivide of texpr * texpr * ty
    | TEquals of texpr * texpr * ty
    | TLess of texpr * texpr * ty
    | TCond of texpr * texpr * ty
    | TFun of name * name * ty * ty * expr * ty
    | TApply of expr * expr * ty
    
    member texpr.Type() =
        match texpr with
        | TVar (_, ty) | TInt(_, ty) | TFloat (_, ty) | TBool (_, ty)
        | TTimes (_, _, ty) | TPlus (_, _, ty) | TMinus (_, _, ty)
        | TDivide (_, _, ty) | TEquals (_, _, ty) | TLess (_, _, ty) 
        | TCond (_, _, ty) | TFun (_, _, _, _, _, ty) 
        | TApply (_, _, ty) -> ty


type ttoplevel_decl =
    | TExpr of texpr
    | TLetBinding of name * texpr




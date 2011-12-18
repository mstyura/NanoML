module NanoML.Compiler.Ast

type name = Name of string

type ty =
    | TInt
    | TFloat
    | TBool
    | TFun of ty * ty (* function type t1 -> t2 *)

type expr =
    | Var of name
    | Int of int
    | Float of float
    | Bool of bool
    | Times of expr * expr
    | Plus of expr * expr
    | Minus of expr * expr
    | Divide of expr * expr
    | Equal of expr * expr
    | Less of expr * expr
    | Cond of expr * expr * expr
    | Fun of name * name * ty * ty * expr
    | Apply of expr * expr

type toplevel_decl =
    | Expr of expr
    | LetBinding of name * expr
    

module NanoML.Compiler.Ast

open NanoML.Utils

type name = Name of string
    with
        override n.ToString() =
            match n with
            | Name v -> v


type ty =
    | TyInt
    | TyFloat
    | TyBool
    | TyFun of ty * ty (* function type t1 -> t2 *)
    override ty.ToString() =
        let rec toStr precedence ty =
            let n, str =
                match ty with
                | TyInt -> 2, "int"
                | TyFloat -> 2, "float"
                | TyBool -> 2, "bool"
                | TyFun(ty1, ty2) -> 1, toStr 1 ty1 + " -> " + toStr 0 ty2

            if n > precedence then str else "(" + str + ")"
        toStr (-1) ty


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
    | LetIn of name * expr * expr
    | Fun of name * name * ty * ty * expr
    | Apply of expr * expr
    override expr.ToString() =
        let rec toStr precedence e =
            let (m, str) =
                match e with
                | Int n -> 7, string n
                | Float f -> 7, string f
                | Bool b -> 7, string b
                | Var x -> 7, string x
                | Apply (e1, e2) -> 6, toStr 5 e1 + " " + toStr 6 e2
                | Times (e1, e2) -> 5, toStr 4 e1 + " * " + toStr 5 e2
                | Divide (e1, e2) -> 5, toStr 4 e1 + " / " + toStr 5 e2
                | Plus (e1, e2) -> 4, toStr 3 e1 + " + " + toStr 4 e2
                | Minus (e1, e2) -> 4, toStr 3 e1 + " - " + toStr 4 e2
                | Equal (e1, e2) -> 3, toStr 3 e1 + " = " + toStr 3 e2
                | Less (e1, e2) -> 3, toStr 3 e1 + " < " + toStr 3 e2
                | Cond (e1, e2, e3) -> 2, sprintf "if %s then %s else %s" (toStr 2 e1) (toStr 2 e2) (toStr 2 e3)
                | Fun (f, x, ty1, ty2, e) ->
                    (1, sprintf "fun %s (%s : %s) : %s is %s" (string f) (string x) (string ty1) (string ty2) (toStr 0 e))
                | LetIn (name, e1, e2) -> 8, sprintf "let %O = %s in %s" name (toStr 7 e1) (toStr 8 e2)
            if m > precedence then str else "(" + str + ")"
        toStr (-1) expr


type toplevel_decl =
    | Expr of expr
    | LetBinding of name * expr


let rec subst s = function
    | Var x as e -> match List.assoc x s with Some v -> v | _ -> e
    | (Int _ | Bool _ | Float _) as e -> e
    | Times (e1, e2) -> Times(subst s e1, subst s e2)
    | Plus (e1, e2) -> Plus(subst s e1, subst s e2)
    | Minus (e1, e2) -> Minus (subst s e1, subst s e2)
    | Divide (e1, e2) -> Divide(subst s e1, subst s e2)
    | Equal (e1, e2) -> Equal (subst s e1, subst s e2)
    | Less (e1, e2) -> Less (subst s e1, subst s e2)
    | Cond (e1, e2, e3) -> Cond (subst s e1, subst s e2, subst s e3)
    | Fun (f, x, tyIn, tyOut, e) ->
        let s' = s |> List.removeAssoc x |> List.removeAssoc f
        Fun(f, x, tyIn, tyOut, subst s' e)
    | Apply (e1, e2) -> Apply (subst s e1, subst s e2)
    | LetIn (var, e1, e2) -> LetIn(var, e1, subst ((var, e1) :: s) e2)

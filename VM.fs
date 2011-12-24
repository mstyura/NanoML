
module NanoML.VirtualMachine

open NanoML.Utils

exception RuntimeError of string

type name = Compiler.Ast.name

type mvalue =
    | MInt of int
    | MFloat of float
    | MBool of bool
    | MClosure of name * frame * env
    override m.ToString() =
        match m with
        | MInt v -> string v
        | MFloat v -> string v
        | MBool v -> string v
        | MClosure _ -> "<fun>"


and instr =
    | IMulti
    | IMultf
    | ISubi
    | ISubf
    | IAddi
    | IAddf
    | IDivi
    | IDivf
    | IEquali
    | IEqualf
    | ILessi
    | ILessf
    | ILdVar of name // push variable on stack
    | ILdInt of int // push int constant on stack
    | ILdFloat of float // push float constant on stack
    | ILdBool of bool // push booleans constant on stack
    | ILdClosure of name * name * frame // push closure on stack
    | IBranch of frame * frame
    | ICall 
    | IPopEnv


and frame = instr list


and env = (name * ref<mvalue>) list


and stack = mvalue list


let error msg = raise (RuntimeError msg)

let lookup x = function
      env :: _ -> match List.assoc x env with Some v -> v | _ -> error ("unknown" + string x)
    | _ -> error ("unknown" + string x)

let pop = function
      [] -> error "empty stack"
    | v :: s -> (v, s)

let popBool = function
      MBool b :: s -> (b, s)
    | _ -> error "bool expected"

let popApp = function
      v :: MClosure (x, f, e) :: s -> (x, f , e, v, s)
    | _ -> error "value and closure expected"


/// Arithmetical operations.
let mult = function
      MInt x :: MInt y :: s -> MInt (x * y) :: s
    | MFloat x :: MFloat y :: s -> MFloat (x * y) :: s
    | _ -> error "Float and Float or Int and Int expected"

let add = function
      MInt x :: MInt y :: s -> MInt (x + y) :: s
    | MFloat x :: MFloat y :: s -> MFloat (x + y) :: s
    | _ -> error "Float and Float or Int and Int expected"
    
let sub = function
      MInt x :: MInt y :: s -> MInt (y - x) :: s
    | MFloat x :: MFloat y :: s -> MFloat (y - x) :: s
    | _ -> error "Float and Float or Int and Int expected"

let div = function
      MInt x :: MInt y :: s -> MInt (y / x) :: s
    | MFloat x :: MFloat y :: s -> MFloat (y / x) :: s
    | _ -> error "Float and Float or Int and Int expected"   
 
let eq = function
      MInt x :: MInt y :: s -> MBool (y = x) :: s
    | MFloat x :: MFloat y :: s -> MBool (y = x) :: s
    | _ -> error "Float and Float or Int and Int expected"    

let less = function
      MInt x :: MInt y :: s -> MBool (y < x) :: s
    | MFloat x :: MFloat y :: s -> MBool (y < x) :: s
    | _ -> error "Float and Float or Int and Int expected"    


/// execute 
let execute instr frms stck (envs : env list) =
    match instr with
    | IMulti | IMultf -> frms, mult stck, envs
    | IAddi | IAddf -> frms, add stck, envs
    | ISubi | ISubf -> frms, sub stck, envs
    | IDivi | IDivf -> frms, div stck, envs
    | IEquali | IEqualf -> frms, eq stck, envs
    | ILessi | ILessf -> frms, less stck, envs
    | ILdVar x -> frms, (lookup x envs).Value :: stck, envs
    | ILdInt v -> frms, MInt v :: stck, envs
    | ILdFloat v -> frms, MFloat v :: stck, envs
    | ILdBool v -> frms, MBool v :: stck, envs
    | ILdClosure (f, x, frm) ->
        match envs with
        | env :: _ ->
            // TODO: seems like its not compiles
            let c' = ref Unchecked.defaultof<mvalue>
            let c = MClosure (x, frm, (f, c') :: env)
            c' := c
            
            frms, c :: stck, envs
        | [] -> error "no environment for a closure"

    | IBranch (f1, f2) ->
        let b, stck' = popBool stck
        (if b then f1 else f2) :: frms, stck', envs

    | ICall ->
        let x, frm, env, v, stck' = popApp stck
        frm :: frms, stck', ((x, ref v) :: env) :: envs

    | IPopEnv ->
         match envs with
         | [] -> error "non environments to pop"
         | _ :: envs' -> frms, stck, envs'


let run frm env =
    let rec loop = function
          ([], [v], _) -> v
        | ((i :: is) :: frms, stck, envs) -> loop (execute i (is :: frms) stck envs)
        | ([] :: frms, stck, envs) -> loop (frms, stck, envs)
        | _ -> error "Illegal end of program"

    loop ([frm], [], [env])

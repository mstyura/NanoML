open NanoML.Compiler.Ast
open NanoML;
open NanoML.Compiler;

open System.IO

type context = (name * ty) list

type env = (name * VirtualMachine.mvalue) list



let execCmd (ctx, env) = function
    | Expr e ->
        let ty = TypeChecker.typeof ctx e
        let frm = Emitter.emit e
        let v = VirtualMachine.run frm env
        (ctx, env), sprintf "val it : %s = %s" (string ty) (string v)

    | Def (x, e) ->
         let ty = TypeChecker.typeof ctx e
         let frm = Emitter.emit e
         let v = VirtualMachine.run frm env
         ((x, ty) :: ctx, (x, v) :: frm), sprintf "val %s : %s = %s" (string x) (string ty) (string v)


let execCmds ce cmds =
    List.foldLeft (fun ce cmd -> let ce', msg = execCmd ce cmd in printfn "%s" msg; ce') ce cmds


let interactive ctx env =
    printfn "Welcome to NanoML"

    let globaCtx = ref ctx
    let globEnv = ref env

    try
        while true do
            try
                print "NanoML> "
                let str = System.Console.ReadLine()
                let cmds = Parser.toplevel Lexer.token (Lexbuf<_>.FromString str)
                let (ctx, env) = execCmds (!globa_ctx, !globa_env) cmds
                global_ctx := ctx
                global_env := env
            with
                | TypeChecker.TypeError msg -> printfn "Type error: %s" msg
                | Machine.MachineError msg -> printfn "Runtime error: %s" msg
                | e -> printfn "Error: %A" e

    with
        _ -> printfn "Exiting..."

[<EntryPoint>]
let main (args : string array) =
    let nonInteractive = Array.tryFind ((=) "-n") args
    let files = args.[1..]
    try
        let ctx, env =
            List.fold
                (fun ce f ->
                    let text = File.ReadAllText f
                    let lexbuf = Lexbuf<_>.FromString text
                    let cmds = Parser.topLevel Lexer.token lexbuf
                    execCmds ce cmds) ([], []) files
        if nonInteractive.isNone then shell ctx env
        0
    with
        | TypeError msg -> printfn "Type error: %s" msg; 1
        | RuntimeError msg -> printfn "Virtual maching error: %s" msg; 1
        | e -> printfn "Error: %A" e; 1
    

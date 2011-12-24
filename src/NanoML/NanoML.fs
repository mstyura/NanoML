module NanoML.Program

open NanoML.Compiler.Ast
open NanoML;
open NanoML.Compiler;
open Microsoft.FSharp.Text.Lexing

open System.IO

type context = (name * ty) list

type env = (name * VirtualMachine.mvalue) list


type settings = { DumpDeclarations : bool
                  DumpVMCode : bool }


let execCmd (ctx, env) = function
    | Expr e ->
        let ty = TypeChecker.typeOf ctx e
        let frm = Emitter.emit e
        let v = VirtualMachine.run frm env
        (ctx, env), sprintf "val it : %s = %s" (string ty) (string v)

    | LetBinding (x, e) ->
         let ty = TypeChecker.typeOf ctx e
         let frm = Emitter.emit e
         let v = VirtualMachine.run frm env
         ((x, ty) :: ctx, (x, ref v) :: env), sprintf "val %s : %s = %s" (string x) (string ty) (string v)


let execCmds ce cmds =
    List.fold (fun ce cmd -> let ce', msg = execCmd ce cmd in printfn "%s" msg; ce') ce cmds

let dumpDeclarations (settings : settings) (decls : toplevel_decl list) =
    if settings.DumpDeclarations then
        printfn "declarations dump:"
        for decl in decls do 
            printfn "%A" decl
    decls


let interactive (settings : settings) ctx env =
    printfn "Welcome to NanoML"

    let globalCtx = ref ctx
    let globalEnv = ref env

    try
        while true do
            try
                printf "NanoML> "
                let str = System.Console.ReadLine()
                let decls = Parser.toplevel Lexer.token (LexBuffer<_>.FromString str) |> dumpDeclarations settings
                let (ctx, env) = execCmds (!globalCtx, !globalEnv) decls
                globalCtx := ctx
                globalEnv := env
            with
                | TypeChecker.TypeError msg -> printfn "Type error: %s" msg
                | VirtualMachine.RuntimeError msg -> printfn "Runtime error: %s" msg
                | e -> printfn "Error: %A" e

    with
        _ -> printfn "Exiting..."

[<EntryPoint>]
let main (args : string array) =
    let nonInteractive = ref false
    let dumpDecl = ref false
    let dumpVmCode = ref false
    let files = ref []
    ArgParser.Parse 
        ([ArgInfo("-n", ArgType.Set nonInteractive , "Non interactive run")
          ArgInfo("--DumpDecl", ArgType.Set dumpDecl, "Dump delcarations")
          ArgInfo("--DumpVMCode", ArgType.Set dumpVmCode, "Dump virtual machine code")],
         (fun f -> files := f :: !files),
         "Usage: nanoml [-n] [file] ...")
    
    try
        let ctx, env =
            List.fold
                (fun ce f ->
                    let text = File.ReadAllText f
                    let lexbuf = LexBuffer<_>.FromString text
                    let cmds = Parser.toplevel Lexer.token lexbuf
                    execCmds ce cmds) ([], []) !files
        if not !nonInteractive then interactive { DumpDeclarations = !dumpDecl; DumpVMCode = !dumpVmCode } ctx env
        0
    with
        | TypeChecker.TypeError msg -> printfn "Type error: %s" msg; 1
        | VirtualMachine.RuntimeError msg -> printfn "Virtual maching error: %s" msg; 1
        | e -> printfn "Error: %A" e; 1
    

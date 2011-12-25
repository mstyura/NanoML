module NanoML.Program

open NanoML.Compiler.Ast
open NanoML.Compiler.TAst
open NanoML;
open NanoML.Compiler;
open Microsoft.FSharp.Text.Lexing

open System.IO

type context = (name * ty) list

type env = (name * VirtualMachine.mvalue) list


type settings = { DumpDeclarations : bool
                  DumpVMCode : bool
                  DumpTAst : bool }

let inline dumpVmCode (s : settings) (frm : VirtualMachine.frame) =
    if s.DumpVMCode then
        printfn "dump VM code:"
        printfn "%s" (VirtualMachine.frame2string frm)
    frm

let inline dumpTAst (s : settings) (texpr : texpr) = 
    if s.DumpTAst then
        printfn "dump typed abstract syntax tree:"
        printfn "%A" texpr
    texpr
   

let execCmd (s : settings) (ctx, env) = function
    | Expr e ->
        let tast1 = TypeChecker.typeOf ctx e |> dumpTAst s
        let tast = TypeChecker.erasureLetIn ctx tast1 |> dumpTAst s
        let frm = Emitter.emit tast |> dumpVmCode s
        let v = VirtualMachine.run frm env
        (ctx, env), sprintf "val it : %s = %s" (string tast.Type) (string v)

    | LetBinding (x, e) ->
         let tast1 = TypeChecker.typeOf ctx e |> dumpTAst s
         let tast = TypeChecker.erasureLetIn ctx tast1 |> dumpTAst s
         let frm = Emitter.emit tast |> dumpVmCode s
         let v = VirtualMachine.run frm env
         ((x, tast.Type) :: ctx, (x, ref v) :: env), sprintf "val %s : %s = %s" (string x) (string tast.Type) (string v)


let execCmds (s : settings) ce cmds =
    List.fold (fun ce cmd -> let ce', msg = execCmd s ce cmd in printfn "%s" msg; ce') ce cmds

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
                let (ctx, env) = execCmds settings (!globalCtx, !globalEnv) decls
                globalCtx := ctx
                globalEnv := env
                ()
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
    let dumpTAst = ref false
    let files = ref []
    ArgParser.Parse 
        ([ArgInfo("-n", ArgType.Set nonInteractive , "Non interactive run")
          ArgInfo("--DumpDecl", ArgType.Set dumpDecl, "Dump delcarations")
          ArgInfo("--DumpVMCode", ArgType.Set dumpVmCode, "Dump virtual machine code")
          ArgInfo("--DumpTAst", ArgType.Set dumpTAst, "Dump typed AST")],
         (fun f -> files := f :: !files),
         "Usage: nanoml [-n] [file] ...")
    
    try
        let settings = { DumpDeclarations = !dumpDecl; DumpVMCode = !dumpVmCode; DumpTAst = !dumpTAst }
        let ctx, env =
            List.fold
                (fun ce f ->
                    let text = File.ReadAllText f
                    let lexbuf = LexBuffer<_>.FromString text
                    let cmds = Parser.toplevel Lexer.token lexbuf
                    execCmds settings ce cmds) ([], []) !files
        if not !nonInteractive then interactive settings ctx env
        0
    with
        | TypeChecker.TypeError msg -> printfn "Type error: %s" msg; 1
        | VirtualMachine.RuntimeError msg -> printfn "Virtual maching error: %s" msg; 1
        | e -> printfn "Error: %A" e; 1
    

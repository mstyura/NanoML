module NanoML.Program

open NanoML.Compiler.Ast
open NanoML.Compiler.TAst
open NanoML;
open NanoML.Compiler;
open Microsoft.FSharp.Text.Lexing

open System.IO

type context = (name * ty) list

type settings = { DumpDeclarations : bool
                  DumpVMCode : bool
                  DumpTAst : bool
                  DumpContext : bool
                  DumpEnv : bool }

let inline dumpVmCode (s : settings) (frm : VirtualMachine.frame) =
    if s.DumpVMCode then
        printfn "\ndump VM code:"
        printfn "%s" (VirtualMachine.frame2string frm)
    frm

let inline dumpTAst (s : settings) comment (texpr : texpr) = 
    if s.DumpTAst then
        printfn "\n%s:" comment
        printfn "%A" texpr
    texpr

let inline dumpContext (s : settings) (ctx : context) =
    if s.DumpContext then
        printfn "\ndump context:"
        printfn "%A" ctx
    ctx

let inline dumpEnv (s : settings) (env : VirtualMachine.env) =
    if s.DumpEnv then
        printfn "\ndump environment:"
        printfn "%A" env
    env

let execCmd (s : settings) (ctx, env) expr = 
    let name, e = match expr with Expr e -> Name "it", e | LetBinding (x, e) -> x, e
    let tast' = TypeChecker.typify ctx e |> dumpTAst s "dump typed abstract syntax tree"
    let tast = TypeChecker.transform ctx tast' |> dumpTAst s "dump typed and transforment abstract syntax tree"
    let frm = Emitter.emit tast |> dumpVmCode s
    let v = VirtualMachine.run frm env
    ((name, tast.Type) :: ctx, (name, ref v) :: env), sprintf "val %O : %s = %s" name (string tast.Type) (string v)


let execCmds (s : settings) ce cmds =
    List.fold (fun ce cmd -> let ce', msg = execCmd s ce cmd in printfn "%s" msg; ce') ce cmds

let dumpDeclarations (settings : settings) (decls : toplevel_decl list) =
    if settings.DumpDeclarations then
        printfn "\ndeclarations dump:"
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
                let (ctx : context, env : VirtualMachine.env) = execCmds settings (!globalCtx, !globalEnv) decls
                globalCtx := ctx |> dumpContext settings
                globalEnv := env |> dumpEnv settings
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
    let dumpContext = ref false
    let dumpEnv = ref false
    let files = ref []
    ArgParser.Parse 
        ([ArgInfo("-n", ArgType.Set nonInteractive , "Non interactive run")
          ArgInfo("--DumpDecl", ArgType.Set dumpDecl, "Dump delcarations")
          ArgInfo("--DumpVMCode", ArgType.Set dumpVmCode, "Dump virtual machine code")
          ArgInfo("--DumpTAst", ArgType.Set dumpTAst, "Dump typed AST")
          ArgInfo("--DumpCtx", ArgType.Set dumpContext, "Dump context (Attention: printing may cycle when object graph has cycles)")
          ArgInfo("--DumpEnv", ArgType.Set dumpEnv, "Dump environment")],
         (fun f -> files := f :: !files),
         "Usage: nanoml [-n] [other options] [file] ...")
    
    try
        let settings = { DumpDeclarations = !dumpDecl; DumpVMCode = !dumpVmCode; 
                         DumpTAst = !dumpTAst; DumpContext = !dumpContext; DumpEnv = !dumpEnv }
        let (ctx : context), env =
            List.fold
                (fun ce f ->
                    let text = File.ReadAllText f
                    let lexbuf = LexBuffer<_>.FromString text
                    let cmds = Parser.toplevel Lexer.token lexbuf
                    execCmds settings ce cmds) NanoML.Language.Basics.initialContextAndEnv !files
        if not !nonInteractive then interactive settings ctx env
        0
    with
        | TypeChecker.TypeError msg -> printfn "Type error: %s" msg; 1
        | VirtualMachine.RuntimeError msg -> printfn "Virtual maching error: %s" msg; 1
        | e -> printfn "Error: %A" e; 1
    

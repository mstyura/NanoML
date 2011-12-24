call compile-lexer.cmd
call compile-parser.cmd

fsc ../src/NanoML/List.fs ../src/NanoML/NanoMLAst.fs ../src/NanoML/TypeChecker.fs ../src/NanoML/NanoMLParser.fs ../src/NanoML/NanoMLLexer.fs ../src/NanoML/VM.fs ../src/NanoML/Compile.fs ../src/NanoML/NanoML.fs -r FSharp.Powerpack.dll
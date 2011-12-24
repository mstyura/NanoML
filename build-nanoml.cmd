call compile-lexer.cmd
call compile-parser.cmd

fsc List.fs NanoMLAst.fs TypeChecker.fs NanoMLParser.fs NanoMLLexer.fs VM.fs Compile.fs NanoML.fs
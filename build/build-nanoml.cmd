call compile-lexer.cmd
call compile-parser.cmd

set original_dir=%cd%

cd ../src/NanoML

fsc List.fs NanoMLAst.fs NanoMLTAst.fs TypeChecker.fs NanoMLParser.fs NanoMLLexer.fs VM.fs NanoMLLang.fs Compile.fs NanoML.fs -r ../../lib/FSharp.Powerpack.dll

cd %original_dir%
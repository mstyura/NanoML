// Implementation file for parser generated by fsyacc
module NanoML.Compiler.Parser
#nowarn "64";; // turn off warnings that type variables used in production annotations are instantiated to concrete type
open Microsoft.FSharp.Text.Lexing
open Microsoft.FSharp.Text.Parsing.ParseHelpers
# 1 "NanoMLParser.fsy"

open NanoML.Compiler.Ast

# 10 "NanoMLParser.fs"
// This type is the type of tokens accepted by the parser
type token = 
  | EOF
  | SEMICOLON2
  | LET
  | LPAREN
  | RPAREN
  | COLON
  | FUN
  | IS
  | IF
  | THEN
  | ELSE
  | EQUAL
  | LESS
  | PLUS
  | MINUS
  | TIMES
  | DIVIDE
  | TRUE
  | FALSE
  | BOOL of (bool)
  | FLOAT of (float)
  | INT of (int)
  | VAR of (name)
  | TARROW
  | TBOOL
  | TFLOAT
  | TINT
// This type is used to give symbolic names to token indexes, useful for error messages
type tokenId = 
    | TOKEN_EOF
    | TOKEN_SEMICOLON2
    | TOKEN_LET
    | TOKEN_LPAREN
    | TOKEN_RPAREN
    | TOKEN_COLON
    | TOKEN_FUN
    | TOKEN_IS
    | TOKEN_IF
    | TOKEN_THEN
    | TOKEN_ELSE
    | TOKEN_EQUAL
    | TOKEN_LESS
    | TOKEN_PLUS
    | TOKEN_MINUS
    | TOKEN_TIMES
    | TOKEN_DIVIDE
    | TOKEN_TRUE
    | TOKEN_FALSE
    | TOKEN_BOOL
    | TOKEN_FLOAT
    | TOKEN_INT
    | TOKEN_VAR
    | TOKEN_TARROW
    | TOKEN_TBOOL
    | TOKEN_TFLOAT
    | TOKEN_TINT
    | TOKEN_end_of_input
    | TOKEN_error
// This type is used to give symbolic names to token indexes, useful for error messages
type nonTerminalId = 
    | NONTERM__starttoplevel
    | NONTERM_toplevel
    | NONTERM_def
    | NONTERM_expr
    | NONTERM_app
    | NONTERM_non_app
    | NONTERM_arithmetic
    | NONTERM_cond
    | NONTERM_ty

// This function maps tokens to integers indexes
let tagOfToken (t:token) = 
  match t with
  | EOF  -> 0 
  | SEMICOLON2  -> 1 
  | LET  -> 2 
  | LPAREN  -> 3 
  | RPAREN  -> 4 
  | COLON  -> 5 
  | FUN  -> 6 
  | IS  -> 7 
  | IF  -> 8 
  | THEN  -> 9 
  | ELSE  -> 10 
  | EQUAL  -> 11 
  | LESS  -> 12 
  | PLUS  -> 13 
  | MINUS  -> 14 
  | TIMES  -> 15 
  | DIVIDE  -> 16 
  | TRUE  -> 17 
  | FALSE  -> 18 
  | BOOL _ -> 19 
  | FLOAT _ -> 20 
  | INT _ -> 21 
  | VAR _ -> 22 
  | TARROW  -> 23 
  | TBOOL  -> 24 
  | TFLOAT  -> 25 
  | TINT  -> 26 

// This function maps integers indexes to symbolic token ids
let tokenTagToTokenId (tokenIdx:int) = 
  match tokenIdx with
  | 0 -> TOKEN_EOF 
  | 1 -> TOKEN_SEMICOLON2 
  | 2 -> TOKEN_LET 
  | 3 -> TOKEN_LPAREN 
  | 4 -> TOKEN_RPAREN 
  | 5 -> TOKEN_COLON 
  | 6 -> TOKEN_FUN 
  | 7 -> TOKEN_IS 
  | 8 -> TOKEN_IF 
  | 9 -> TOKEN_THEN 
  | 10 -> TOKEN_ELSE 
  | 11 -> TOKEN_EQUAL 
  | 12 -> TOKEN_LESS 
  | 13 -> TOKEN_PLUS 
  | 14 -> TOKEN_MINUS 
  | 15 -> TOKEN_TIMES 
  | 16 -> TOKEN_DIVIDE 
  | 17 -> TOKEN_TRUE 
  | 18 -> TOKEN_FALSE 
  | 19 -> TOKEN_BOOL 
  | 20 -> TOKEN_FLOAT 
  | 21 -> TOKEN_INT 
  | 22 -> TOKEN_VAR 
  | 23 -> TOKEN_TARROW 
  | 24 -> TOKEN_TBOOL 
  | 25 -> TOKEN_TFLOAT 
  | 26 -> TOKEN_TINT 
  | 29 -> TOKEN_end_of_input
  | 27 -> TOKEN_error
  | _ -> failwith "tokenTagToTokenId: bad token"

/// This function maps production indexes returned in syntax errors to strings representing the non terminal that would be produced by that production
let prodIdxToNonTerminal (prodIdx:int) = 
  match prodIdx with
    | 0 -> NONTERM__starttoplevel 
    | 1 -> NONTERM_toplevel 
    | 2 -> NONTERM_toplevel 
    | 3 -> NONTERM_toplevel 
    | 4 -> NONTERM_toplevel 
    | 5 -> NONTERM_toplevel 
    | 6 -> NONTERM_toplevel 
    | 7 -> NONTERM_toplevel 
    | 8 -> NONTERM_def 
    | 9 -> NONTERM_expr 
    | 10 -> NONTERM_expr 
    | 11 -> NONTERM_expr 
    | 12 -> NONTERM_expr 
    | 13 -> NONTERM_expr 
    | 14 -> NONTERM_expr 
    | 15 -> NONTERM_app 
    | 16 -> NONTERM_app 
    | 17 -> NONTERM_non_app 
    | 18 -> NONTERM_non_app 
    | 19 -> NONTERM_non_app 
    | 20 -> NONTERM_non_app 
    | 21 -> NONTERM_non_app 
    | 22 -> NONTERM_arithmetic 
    | 23 -> NONTERM_arithmetic 
    | 24 -> NONTERM_arithmetic 
    | 25 -> NONTERM_arithmetic 
    | 26 -> NONTERM_arithmetic 
    | 27 -> NONTERM_cond 
    | 28 -> NONTERM_cond 
    | 29 -> NONTERM_ty 
    | 30 -> NONTERM_ty 
    | 31 -> NONTERM_ty 
    | 32 -> NONTERM_ty 
    | _ -> failwith "prodIdxToNonTerminal: bad production index"

let _fsyacc_endOfInputTag = 29 
let _fsyacc_tagOfErrorTerminal = 27

// This function gets the name of a token as a string
let token_to_string (t:token) = 
  match t with 
  | EOF  -> "EOF" 
  | SEMICOLON2  -> "SEMICOLON2" 
  | LET  -> "LET" 
  | LPAREN  -> "LPAREN" 
  | RPAREN  -> "RPAREN" 
  | COLON  -> "COLON" 
  | FUN  -> "FUN" 
  | IS  -> "IS" 
  | IF  -> "IF" 
  | THEN  -> "THEN" 
  | ELSE  -> "ELSE" 
  | EQUAL  -> "EQUAL" 
  | LESS  -> "LESS" 
  | PLUS  -> "PLUS" 
  | MINUS  -> "MINUS" 
  | TIMES  -> "TIMES" 
  | DIVIDE  -> "DIVIDE" 
  | TRUE  -> "TRUE" 
  | FALSE  -> "FALSE" 
  | BOOL _ -> "BOOL" 
  | FLOAT _ -> "FLOAT" 
  | INT _ -> "INT" 
  | VAR _ -> "VAR" 
  | TARROW  -> "TARROW" 
  | TBOOL  -> "TBOOL" 
  | TFLOAT  -> "TFLOAT" 
  | TINT  -> "TINT" 

// This function gets the data carried by a token as an object
let _fsyacc_dataOfToken (t:token) = 
  match t with 
  | EOF  -> (null : System.Object) 
  | SEMICOLON2  -> (null : System.Object) 
  | LET  -> (null : System.Object) 
  | LPAREN  -> (null : System.Object) 
  | RPAREN  -> (null : System.Object) 
  | COLON  -> (null : System.Object) 
  | FUN  -> (null : System.Object) 
  | IS  -> (null : System.Object) 
  | IF  -> (null : System.Object) 
  | THEN  -> (null : System.Object) 
  | ELSE  -> (null : System.Object) 
  | EQUAL  -> (null : System.Object) 
  | LESS  -> (null : System.Object) 
  | PLUS  -> (null : System.Object) 
  | MINUS  -> (null : System.Object) 
  | TIMES  -> (null : System.Object) 
  | DIVIDE  -> (null : System.Object) 
  | TRUE  -> (null : System.Object) 
  | FALSE  -> (null : System.Object) 
  | BOOL _fsyacc_x -> Microsoft.FSharp.Core.Operators.box _fsyacc_x 
  | FLOAT _fsyacc_x -> Microsoft.FSharp.Core.Operators.box _fsyacc_x 
  | INT _fsyacc_x -> Microsoft.FSharp.Core.Operators.box _fsyacc_x 
  | VAR _fsyacc_x -> Microsoft.FSharp.Core.Operators.box _fsyacc_x 
  | TARROW  -> (null : System.Object) 
  | TBOOL  -> (null : System.Object) 
  | TFLOAT  -> (null : System.Object) 
  | TINT  -> (null : System.Object) 
let _fsyacc_gotos = [| 0us; 65535us; 3us; 65535us; 0us; 1us; 7us; 11us; 10us; 12us; 3us; 65535us; 0us; 5us; 7us; 5us; 10us; 5us; 15us; 65535us; 0us; 8us; 7us; 8us; 10us; 8us; 15us; 16us; 21us; 22us; 23us; 24us; 25us; 26us; 36us; 37us; 44us; 45us; 55us; 49us; 56us; 50us; 57us; 51us; 58us; 52us; 59us; 53us; 60us; 54us; 15us; 65535us; 0us; 18us; 7us; 18us; 10us; 18us; 15us; 18us; 21us; 18us; 23us; 18us; 25us; 18us; 36us; 18us; 44us; 18us; 55us; 18us; 56us; 18us; 57us; 18us; 58us; 18us; 59us; 18us; 60us; 18us; 17us; 65535us; 0us; 17us; 7us; 17us; 10us; 17us; 15us; 17us; 17us; 39us; 18us; 38us; 21us; 17us; 23us; 17us; 25us; 17us; 36us; 17us; 44us; 17us; 55us; 17us; 56us; 17us; 57us; 17us; 58us; 17us; 59us; 17us; 60us; 17us; 15us; 65535us; 0us; 19us; 7us; 19us; 10us; 19us; 15us; 19us; 21us; 19us; 23us; 19us; 25us; 19us; 36us; 19us; 44us; 19us; 55us; 19us; 56us; 19us; 57us; 19us; 58us; 19us; 59us; 19us; 60us; 19us; 15us; 65535us; 0us; 20us; 7us; 20us; 10us; 20us; 15us; 20us; 21us; 20us; 23us; 20us; 25us; 20us; 36us; 20us; 44us; 20us; 55us; 20us; 56us; 20us; 57us; 20us; 58us; 20us; 59us; 20us; 60us; 20us; 4us; 65535us; 31us; 32us; 34us; 35us; 65us; 63us; 66us; 64us; |]
let _fsyacc_sparseGotoTableRowOffsets = [|0us; 1us; 5us; 9us; 25us; 41us; 59us; 75us; 91us; |]
let _fsyacc_stateToProdIdxsTableElements = [| 1us; 0us; 1us; 0us; 1us; 1us; 2us; 1us; 3us; 2us; 1us; 5us; 3us; 2us; 3us; 6us; 1us; 2us; 2us; 3us; 6us; 9us; 4us; 5us; 7us; 23us; 24us; 25us; 26us; 27us; 28us; 1us; 4us; 2us; 5us; 7us; 1us; 6us; 1us; 7us; 1us; 8us; 1us; 8us; 1us; 8us; 7us; 8us; 23us; 24us; 25us; 26us; 27us; 28us; 2us; 9us; 16us; 2us; 10us; 15us; 1us; 11us; 1us; 12us; 1us; 13us; 7us; 13us; 23us; 24us; 25us; 26us; 27us; 28us; 1us; 13us; 7us; 13us; 23us; 24us; 25us; 26us; 27us; 28us; 1us; 13us; 7us; 13us; 23us; 24us; 25us; 26us; 27us; 28us; 1us; 14us; 1us; 14us; 1us; 14us; 1us; 14us; 1us; 14us; 2us; 14us; 31us; 1us; 14us; 1us; 14us; 2us; 14us; 31us; 1us; 14us; 7us; 14us; 23us; 24us; 25us; 26us; 27us; 28us; 1us; 15us; 1us; 16us; 1us; 17us; 1us; 18us; 1us; 19us; 1us; 20us; 1us; 21us; 7us; 21us; 23us; 24us; 25us; 26us; 27us; 28us; 1us; 21us; 1us; 22us; 1us; 22us; 7us; 23us; 23us; 24us; 25us; 26us; 27us; 28us; 7us; 23us; 24us; 24us; 25us; 26us; 27us; 28us; 7us; 23us; 24us; 25us; 25us; 26us; 27us; 28us; 7us; 23us; 24us; 25us; 26us; 26us; 27us; 28us; 7us; 23us; 24us; 25us; 26us; 27us; 27us; 28us; 7us; 23us; 24us; 25us; 26us; 27us; 28us; 28us; 1us; 23us; 1us; 24us; 1us; 25us; 1us; 26us; 1us; 27us; 1us; 28us; 1us; 29us; 1us; 30us; 2us; 31us; 31us; 2us; 31us; 32us; 1us; 31us; 1us; 32us; 1us; 32us; |]
let _fsyacc_stateToProdIdxsTableRowOffsets = [|0us; 2us; 4us; 6us; 9us; 12us; 16us; 18us; 21us; 31us; 33us; 36us; 38us; 40us; 42us; 44us; 46us; 54us; 57us; 60us; 62us; 64us; 66us; 74us; 76us; 84us; 86us; 94us; 96us; 98us; 100us; 102us; 104us; 107us; 109us; 111us; 114us; 116us; 124us; 126us; 128us; 130us; 132us; 134us; 136us; 138us; 146us; 148us; 150us; 152us; 160us; 168us; 176us; 184us; 192us; 200us; 202us; 204us; 206us; 208us; 210us; 212us; 214us; 216us; 219us; 222us; 224us; 226us; |]
let _fsyacc_action_rows = 68
let _fsyacc_actionTableElements = [|10us; 32768us; 0us; 2us; 2us; 13us; 3us; 44us; 6us; 27us; 8us; 21us; 14us; 47us; 17us; 41us; 18us; 42us; 21us; 43us; 22us; 40us; 0us; 49152us; 0us; 16385us; 0us; 16385us; 0us; 16385us; 2us; 32768us; 0us; 6us; 1us; 7us; 0us; 16386us; 10us; 32768us; 0us; 3us; 2us; 13us; 3us; 44us; 6us; 27us; 8us; 21us; 14us; 47us; 17us; 41us; 18us; 42us; 21us; 43us; 22us; 40us; 8us; 32768us; 0us; 9us; 1us; 10us; 11us; 59us; 12us; 60us; 13us; 55us; 14us; 56us; 15us; 57us; 16us; 58us; 0us; 16388us; 10us; 32768us; 0us; 4us; 2us; 13us; 3us; 44us; 6us; 27us; 8us; 21us; 14us; 47us; 17us; 41us; 18us; 42us; 21us; 43us; 22us; 40us; 0us; 16390us; 0us; 16391us; 1us; 32768us; 22us; 14us; 1us; 32768us; 11us; 15us; 8us; 32768us; 3us; 44us; 6us; 27us; 8us; 21us; 14us; 47us; 17us; 41us; 18us; 42us; 21us; 43us; 22us; 40us; 6us; 16392us; 11us; 59us; 12us; 60us; 13us; 55us; 14us; 56us; 15us; 57us; 16us; 58us; 5us; 16393us; 3us; 44us; 17us; 41us; 18us; 42us; 21us; 43us; 22us; 40us; 5us; 16394us; 3us; 44us; 17us; 41us; 18us; 42us; 21us; 43us; 22us; 40us; 0us; 16395us; 0us; 16396us; 8us; 32768us; 3us; 44us; 6us; 27us; 8us; 21us; 14us; 47us; 17us; 41us; 18us; 42us; 21us; 43us; 22us; 40us; 7us; 32768us; 9us; 23us; 11us; 59us; 12us; 60us; 13us; 55us; 14us; 56us; 15us; 57us; 16us; 58us; 8us; 32768us; 3us; 44us; 6us; 27us; 8us; 21us; 14us; 47us; 17us; 41us; 18us; 42us; 21us; 43us; 22us; 40us; 7us; 32768us; 10us; 25us; 11us; 59us; 12us; 60us; 13us; 55us; 14us; 56us; 15us; 57us; 16us; 58us; 8us; 32768us; 3us; 44us; 6us; 27us; 8us; 21us; 14us; 47us; 17us; 41us; 18us; 42us; 21us; 43us; 22us; 40us; 6us; 16397us; 11us; 59us; 12us; 60us; 13us; 55us; 14us; 56us; 15us; 57us; 16us; 58us; 1us; 32768us; 22us; 28us; 1us; 32768us; 3us; 29us; 1us; 32768us; 22us; 30us; 1us; 32768us; 5us; 31us; 3us; 32768us; 3us; 66us; 24us; 61us; 26us; 62us; 2us; 32768us; 4us; 33us; 23us; 65us; 1us; 32768us; 5us; 34us; 3us; 32768us; 3us; 66us; 24us; 61us; 26us; 62us; 2us; 32768us; 7us; 36us; 23us; 65us; 8us; 32768us; 3us; 44us; 6us; 27us; 8us; 21us; 14us; 47us; 17us; 41us; 18us; 42us; 21us; 43us; 22us; 40us; 6us; 16398us; 11us; 59us; 12us; 60us; 13us; 55us; 14us; 56us; 15us; 57us; 16us; 58us; 0us; 16399us; 0us; 16400us; 0us; 16401us; 0us; 16402us; 0us; 16403us; 0us; 16404us; 8us; 32768us; 3us; 44us; 6us; 27us; 8us; 21us; 14us; 47us; 17us; 41us; 18us; 42us; 21us; 43us; 22us; 40us; 7us; 32768us; 4us; 46us; 11us; 59us; 12us; 60us; 13us; 55us; 14us; 56us; 15us; 57us; 16us; 58us; 0us; 16405us; 1us; 32768us; 21us; 48us; 0us; 16406us; 2us; 16407us; 15us; 57us; 16us; 58us; 2us; 16408us; 15us; 57us; 16us; 58us; 0us; 16409us; 0us; 16410us; 6us; 16411us; 11us; 59us; 12us; 60us; 13us; 55us; 14us; 56us; 15us; 57us; 16us; 58us; 6us; 16412us; 11us; 59us; 12us; 60us; 13us; 55us; 14us; 56us; 15us; 57us; 16us; 58us; 8us; 32768us; 3us; 44us; 6us; 27us; 8us; 21us; 14us; 47us; 17us; 41us; 18us; 42us; 21us; 43us; 22us; 40us; 8us; 32768us; 3us; 44us; 6us; 27us; 8us; 21us; 14us; 47us; 17us; 41us; 18us; 42us; 21us; 43us; 22us; 40us; 8us; 32768us; 3us; 44us; 6us; 27us; 8us; 21us; 14us; 47us; 17us; 41us; 18us; 42us; 21us; 43us; 22us; 40us; 8us; 32768us; 3us; 44us; 6us; 27us; 8us; 21us; 14us; 47us; 17us; 41us; 18us; 42us; 21us; 43us; 22us; 40us; 8us; 32768us; 3us; 44us; 6us; 27us; 8us; 21us; 14us; 47us; 17us; 41us; 18us; 42us; 21us; 43us; 22us; 40us; 8us; 32768us; 3us; 44us; 6us; 27us; 8us; 21us; 14us; 47us; 17us; 41us; 18us; 42us; 21us; 43us; 22us; 40us; 0us; 16413us; 0us; 16414us; 1us; 16415us; 23us; 65us; 2us; 32768us; 4us; 67us; 23us; 65us; 3us; 32768us; 3us; 66us; 24us; 61us; 26us; 62us; 3us; 32768us; 3us; 66us; 24us; 61us; 26us; 62us; 0us; 16416us; |]
let _fsyacc_actionTableRowOffsets = [|0us; 11us; 12us; 13us; 14us; 15us; 18us; 19us; 30us; 39us; 40us; 51us; 52us; 53us; 55us; 57us; 66us; 73us; 79us; 85us; 86us; 87us; 96us; 104us; 113us; 121us; 130us; 137us; 139us; 141us; 143us; 145us; 149us; 152us; 154us; 158us; 161us; 170us; 177us; 178us; 179us; 180us; 181us; 182us; 183us; 192us; 200us; 201us; 203us; 204us; 207us; 210us; 211us; 212us; 219us; 226us; 235us; 244us; 253us; 262us; 271us; 280us; 281us; 282us; 284us; 287us; 291us; 295us; |]
let _fsyacc_reductionSymbolCounts = [|1us; 1us; 2us; 3us; 2us; 3us; 3us; 3us; 4us; 1us; 1us; 1us; 1us; 6us; 11us; 2us; 2us; 1us; 1us; 1us; 1us; 3us; 2us; 3us; 3us; 3us; 3us; 3us; 3us; 1us; 1us; 3us; 3us; |]
let _fsyacc_productionToNonTerminalTable = [|0us; 1us; 1us; 1us; 1us; 1us; 1us; 1us; 2us; 3us; 3us; 3us; 3us; 3us; 3us; 4us; 4us; 5us; 5us; 5us; 5us; 5us; 6us; 6us; 6us; 6us; 6us; 7us; 7us; 8us; 8us; 8us; 8us; |]
let _fsyacc_immediateActions = [|65535us; 49152us; 16385us; 65535us; 65535us; 65535us; 16386us; 65535us; 65535us; 16388us; 65535us; 16390us; 16391us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 16395us; 16396us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 16399us; 16400us; 16401us; 16402us; 16403us; 16404us; 65535us; 65535us; 16405us; 65535us; 16406us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 16413us; 16414us; 65535us; 65535us; 65535us; 65535us; 16416us; |]
let _fsyacc_reductions ()  =    [| 
# 261 "NanoMLParser.fs"
        (fun (parseState : Microsoft.FSharp.Text.Parsing.IParseState) ->
            let _1 = (let data = parseState.GetInput(1) in (Microsoft.FSharp.Core.Operators.unbox data : toplevel_decl list)) in
            Microsoft.FSharp.Core.Operators.box
                (
                   (
                      raise (Microsoft.FSharp.Text.Parsing.Accept(Microsoft.FSharp.Core.Operators.box _1))
                   )
                 : '_starttoplevel));
# 270 "NanoMLParser.fs"
        (fun (parseState : Microsoft.FSharp.Text.Parsing.IParseState) ->
            Microsoft.FSharp.Core.Operators.box
                (
                   (
# 40 "NanoMLParser.fsy"
                                 [] 
                   )
# 40 "NanoMLParser.fsy"
                 : toplevel_decl list));
# 280 "NanoMLParser.fs"
        (fun (parseState : Microsoft.FSharp.Text.Parsing.IParseState) ->
            let _1 = (let data = parseState.GetInput(1) in (Microsoft.FSharp.Core.Operators.unbox data : 'def)) in
            Microsoft.FSharp.Core.Operators.box
                (
                   (
# 41 "NanoMLParser.fsy"
                                     [_1] 
                   )
# 41 "NanoMLParser.fsy"
                 : toplevel_decl list));
# 291 "NanoMLParser.fs"
        (fun (parseState : Microsoft.FSharp.Text.Parsing.IParseState) ->
            let _1 = (let data = parseState.GetInput(1) in (Microsoft.FSharp.Core.Operators.unbox data : 'def)) in
            Microsoft.FSharp.Core.Operators.box
                (
                   (
# 42 "NanoMLParser.fsy"
                                                [_1] 
                   )
# 42 "NanoMLParser.fsy"
                 : toplevel_decl list));
# 302 "NanoMLParser.fs"
        (fun (parseState : Microsoft.FSharp.Text.Parsing.IParseState) ->
            let _1 = (let data = parseState.GetInput(1) in (Microsoft.FSharp.Core.Operators.unbox data : 'expr)) in
            Microsoft.FSharp.Core.Operators.box
                (
                   (
# 43 "NanoMLParser.fsy"
                                      [Expr _1] 
                   )
# 43 "NanoMLParser.fsy"
                 : toplevel_decl list));
# 313 "NanoMLParser.fs"
        (fun (parseState : Microsoft.FSharp.Text.Parsing.IParseState) ->
            let _1 = (let data = parseState.GetInput(1) in (Microsoft.FSharp.Core.Operators.unbox data : 'expr)) in
            Microsoft.FSharp.Core.Operators.box
                (
                   (
# 44 "NanoMLParser.fsy"
                                                 [Expr _1] 
                   )
# 44 "NanoMLParser.fsy"
                 : toplevel_decl list));
# 324 "NanoMLParser.fs"
        (fun (parseState : Microsoft.FSharp.Text.Parsing.IParseState) ->
            let _1 = (let data = parseState.GetInput(1) in (Microsoft.FSharp.Core.Operators.unbox data : 'def)) in
            let _3 = (let data = parseState.GetInput(3) in (Microsoft.FSharp.Core.Operators.unbox data : toplevel_decl list)) in
            Microsoft.FSharp.Core.Operators.box
                (
                   (
# 45 "NanoMLParser.fsy"
                                                     _1 :: _3 
                   )
# 45 "NanoMLParser.fsy"
                 : toplevel_decl list));
# 336 "NanoMLParser.fs"
        (fun (parseState : Microsoft.FSharp.Text.Parsing.IParseState) ->
            let _1 = (let data = parseState.GetInput(1) in (Microsoft.FSharp.Core.Operators.unbox data : 'expr)) in
            let _3 = (let data = parseState.GetInput(3) in (Microsoft.FSharp.Core.Operators.unbox data : toplevel_decl list)) in
            Microsoft.FSharp.Core.Operators.box
                (
                   (
# 46 "NanoMLParser.fsy"
                                                      (Expr _1) :: _3 
                   )
# 46 "NanoMLParser.fsy"
                 : toplevel_decl list));
# 348 "NanoMLParser.fs"
        (fun (parseState : Microsoft.FSharp.Text.Parsing.IParseState) ->
            let _2 = (let data = parseState.GetInput(2) in (Microsoft.FSharp.Core.Operators.unbox data : name)) in
            let _4 = (let data = parseState.GetInput(4) in (Microsoft.FSharp.Core.Operators.unbox data : 'expr)) in
            Microsoft.FSharp.Core.Operators.box
                (
                   (
# 49 "NanoMLParser.fsy"
                                                LetBinding (Name _2, _4) 
                   )
# 49 "NanoMLParser.fsy"
                 : 'def));
# 360 "NanoMLParser.fs"
        (fun (parseState : Microsoft.FSharp.Text.Parsing.IParseState) ->
            let _1 = (let data = parseState.GetInput(1) in (Microsoft.FSharp.Core.Operators.unbox data : 'non_app)) in
            Microsoft.FSharp.Core.Operators.box
                (
                   (
# 53 "NanoMLParser.fsy"
                                     _1 
                   )
# 53 "NanoMLParser.fsy"
                 : 'expr));
# 371 "NanoMLParser.fs"
        (fun (parseState : Microsoft.FSharp.Text.Parsing.IParseState) ->
            let _1 = (let data = parseState.GetInput(1) in (Microsoft.FSharp.Core.Operators.unbox data : 'app)) in
            Microsoft.FSharp.Core.Operators.box
                (
                   (
# 54 "NanoMLParser.fsy"
                                 _1 
                   )
# 54 "NanoMLParser.fsy"
                 : 'expr));
# 382 "NanoMLParser.fs"
        (fun (parseState : Microsoft.FSharp.Text.Parsing.IParseState) ->
            let _1 = (let data = parseState.GetInput(1) in (Microsoft.FSharp.Core.Operators.unbox data : 'arithmetic)) in
            Microsoft.FSharp.Core.Operators.box
                (
                   (
# 55 "NanoMLParser.fsy"
                                        _1 
                   )
# 55 "NanoMLParser.fsy"
                 : 'expr));
# 393 "NanoMLParser.fs"
        (fun (parseState : Microsoft.FSharp.Text.Parsing.IParseState) ->
            let _1 = (let data = parseState.GetInput(1) in (Microsoft.FSharp.Core.Operators.unbox data : 'cond)) in
            Microsoft.FSharp.Core.Operators.box
                (
                   (
# 56 "NanoMLParser.fsy"
                                  _1 
                   )
# 56 "NanoMLParser.fsy"
                 : 'expr));
# 404 "NanoMLParser.fs"
        (fun (parseState : Microsoft.FSharp.Text.Parsing.IParseState) ->
            let _2 = (let data = parseState.GetInput(2) in (Microsoft.FSharp.Core.Operators.unbox data : 'expr)) in
            let _4 = (let data = parseState.GetInput(4) in (Microsoft.FSharp.Core.Operators.unbox data : 'expr)) in
            let _6 = (let data = parseState.GetInput(6) in (Microsoft.FSharp.Core.Operators.unbox data : 'expr)) in
            Microsoft.FSharp.Core.Operators.box
                (
                   (
# 57 "NanoMLParser.fsy"
                                                         Cond (_2, _4, _6) 
                   )
# 57 "NanoMLParser.fsy"
                 : 'expr));
# 417 "NanoMLParser.fs"
        (fun (parseState : Microsoft.FSharp.Text.Parsing.IParseState) ->
            let _2 = (let data = parseState.GetInput(2) in (Microsoft.FSharp.Core.Operators.unbox data : name)) in
            let _4 = (let data = parseState.GetInput(4) in (Microsoft.FSharp.Core.Operators.unbox data : name)) in
            let _6 = (let data = parseState.GetInput(6) in (Microsoft.FSharp.Core.Operators.unbox data : 'ty)) in
            let _9 = (let data = parseState.GetInput(9) in (Microsoft.FSharp.Core.Operators.unbox data : 'ty)) in
            let _11 = (let data = parseState.GetInput(11) in (Microsoft.FSharp.Core.Operators.unbox data : 'expr)) in
            Microsoft.FSharp.Core.Operators.box
                (
                   (
# 58 "NanoMLParser.fsy"
                                                                                 Fun (Name _2, Name _4, _6, _9, _11) 
                   )
# 58 "NanoMLParser.fsy"
                 : 'expr));
# 432 "NanoMLParser.fs"
        (fun (parseState : Microsoft.FSharp.Text.Parsing.IParseState) ->
            let _1 = (let data = parseState.GetInput(1) in (Microsoft.FSharp.Core.Operators.unbox data : 'app)) in
            let _2 = (let data = parseState.GetInput(2) in (Microsoft.FSharp.Core.Operators.unbox data : 'non_app)) in
            Microsoft.FSharp.Core.Operators.box
                (
                   (
# 62 "NanoMLParser.fsy"
                                         Apply (_1, _2) 
                   )
# 62 "NanoMLParser.fsy"
                 : 'app));
# 444 "NanoMLParser.fs"
        (fun (parseState : Microsoft.FSharp.Text.Parsing.IParseState) ->
            let _1 = (let data = parseState.GetInput(1) in (Microsoft.FSharp.Core.Operators.unbox data : 'non_app)) in
            let _2 = (let data = parseState.GetInput(2) in (Microsoft.FSharp.Core.Operators.unbox data : 'non_app)) in
            Microsoft.FSharp.Core.Operators.box
                (
                   (
# 63 "NanoMLParser.fsy"
                                             Apply (_1, _2) 
                   )
# 63 "NanoMLParser.fsy"
                 : 'app));
# 456 "NanoMLParser.fs"
        (fun (parseState : Microsoft.FSharp.Text.Parsing.IParseState) ->
            let _1 = (let data = parseState.GetInput(1) in (Microsoft.FSharp.Core.Operators.unbox data : name)) in
            Microsoft.FSharp.Core.Operators.box
                (
                   (
# 67 "NanoMLParser.fsy"
                                   Var (Name _1) 
                   )
# 67 "NanoMLParser.fsy"
                 : 'non_app));
# 467 "NanoMLParser.fs"
        (fun (parseState : Microsoft.FSharp.Text.Parsing.IParseState) ->
            Microsoft.FSharp.Core.Operators.box
                (
                   (
# 68 "NanoMLParser.fsy"
                                  Bool true 
                   )
# 68 "NanoMLParser.fsy"
                 : 'non_app));
# 477 "NanoMLParser.fs"
        (fun (parseState : Microsoft.FSharp.Text.Parsing.IParseState) ->
            Microsoft.FSharp.Core.Operators.box
                (
                   (
# 69 "NanoMLParser.fsy"
                                   Bool false 
                   )
# 69 "NanoMLParser.fsy"
                 : 'non_app));
# 487 "NanoMLParser.fs"
        (fun (parseState : Microsoft.FSharp.Text.Parsing.IParseState) ->
            let _1 = (let data = parseState.GetInput(1) in (Microsoft.FSharp.Core.Operators.unbox data : int)) in
            Microsoft.FSharp.Core.Operators.box
                (
                   (
# 70 "NanoMLParser.fsy"
                                 Int _1 
                   )
# 70 "NanoMLParser.fsy"
                 : 'non_app));
# 498 "NanoMLParser.fs"
        (fun (parseState : Microsoft.FSharp.Text.Parsing.IParseState) ->
            let _2 = (let data = parseState.GetInput(2) in (Microsoft.FSharp.Core.Operators.unbox data : 'expr)) in
            Microsoft.FSharp.Core.Operators.box
                (
                   (
# 71 "NanoMLParser.fsy"
                                                _2 
                   )
# 71 "NanoMLParser.fsy"
                 : 'non_app));
# 509 "NanoMLParser.fs"
        (fun (parseState : Microsoft.FSharp.Text.Parsing.IParseState) ->
            let _2 = (let data = parseState.GetInput(2) in (Microsoft.FSharp.Core.Operators.unbox data : int)) in
            Microsoft.FSharp.Core.Operators.box
                (
                   (
# 74 "NanoMLParser.fsy"
                                       Int (-_2) 
                   )
# 74 "NanoMLParser.fsy"
                 : 'arithmetic));
# 520 "NanoMLParser.fs"
        (fun (parseState : Microsoft.FSharp.Text.Parsing.IParseState) ->
            let _1 = (let data = parseState.GetInput(1) in (Microsoft.FSharp.Core.Operators.unbox data : 'expr)) in
            let _3 = (let data = parseState.GetInput(3) in (Microsoft.FSharp.Core.Operators.unbox data : 'expr)) in
            Microsoft.FSharp.Core.Operators.box
                (
                   (
# 75 "NanoMLParser.fsy"
                                            Plus (_1, _3) 
                   )
# 75 "NanoMLParser.fsy"
                 : 'arithmetic));
# 532 "NanoMLParser.fs"
        (fun (parseState : Microsoft.FSharp.Text.Parsing.IParseState) ->
            let _1 = (let data = parseState.GetInput(1) in (Microsoft.FSharp.Core.Operators.unbox data : 'expr)) in
            let _3 = (let data = parseState.GetInput(3) in (Microsoft.FSharp.Core.Operators.unbox data : 'expr)) in
            Microsoft.FSharp.Core.Operators.box
                (
                   (
# 76 "NanoMLParser.fsy"
                                             Minus (_1, _3) 
                   )
# 76 "NanoMLParser.fsy"
                 : 'arithmetic));
# 544 "NanoMLParser.fs"
        (fun (parseState : Microsoft.FSharp.Text.Parsing.IParseState) ->
            let _1 = (let data = parseState.GetInput(1) in (Microsoft.FSharp.Core.Operators.unbox data : 'expr)) in
            let _3 = (let data = parseState.GetInput(3) in (Microsoft.FSharp.Core.Operators.unbox data : 'expr)) in
            Microsoft.FSharp.Core.Operators.box
                (
                   (
# 77 "NanoMLParser.fsy"
                                             Times(_1, _3) 
                   )
# 77 "NanoMLParser.fsy"
                 : 'arithmetic));
# 556 "NanoMLParser.fs"
        (fun (parseState : Microsoft.FSharp.Text.Parsing.IParseState) ->
            let _1 = (let data = parseState.GetInput(1) in (Microsoft.FSharp.Core.Operators.unbox data : 'expr)) in
            let _3 = (let data = parseState.GetInput(3) in (Microsoft.FSharp.Core.Operators.unbox data : 'expr)) in
            Microsoft.FSharp.Core.Operators.box
                (
                   (
# 78 "NanoMLParser.fsy"
                                              Divide (_1, _3) 
                   )
# 78 "NanoMLParser.fsy"
                 : 'arithmetic));
# 568 "NanoMLParser.fs"
        (fun (parseState : Microsoft.FSharp.Text.Parsing.IParseState) ->
            let _1 = (let data = parseState.GetInput(1) in (Microsoft.FSharp.Core.Operators.unbox data : 'expr)) in
            let _3 = (let data = parseState.GetInput(3) in (Microsoft.FSharp.Core.Operators.unbox data : 'expr)) in
            Microsoft.FSharp.Core.Operators.box
                (
                   (
# 81 "NanoMLParser.fsy"
                                             Equal (_1, _3) 
                   )
# 81 "NanoMLParser.fsy"
                 : 'cond));
# 580 "NanoMLParser.fs"
        (fun (parseState : Microsoft.FSharp.Text.Parsing.IParseState) ->
            let _1 = (let data = parseState.GetInput(1) in (Microsoft.FSharp.Core.Operators.unbox data : 'expr)) in
            let _3 = (let data = parseState.GetInput(3) in (Microsoft.FSharp.Core.Operators.unbox data : 'expr)) in
            Microsoft.FSharp.Core.Operators.box
                (
                   (
# 82 "NanoMLParser.fsy"
                                            Less (_1, _3) 
                   )
# 82 "NanoMLParser.fsy"
                 : 'cond));
# 592 "NanoMLParser.fs"
        (fun (parseState : Microsoft.FSharp.Text.Parsing.IParseState) ->
            Microsoft.FSharp.Core.Operators.box
                (
                   (
# 85 "NanoMLParser.fsy"
                                   TBool 
                   )
# 85 "NanoMLParser.fsy"
                 : 'ty));
# 602 "NanoMLParser.fs"
        (fun (parseState : Microsoft.FSharp.Text.Parsing.IParseState) ->
            Microsoft.FSharp.Core.Operators.box
                (
                   (
# 86 "NanoMLParser.fsy"
                                  TInt 
                   )
# 86 "NanoMLParser.fsy"
                 : 'ty));
# 612 "NanoMLParser.fs"
        (fun (parseState : Microsoft.FSharp.Text.Parsing.IParseState) ->
            let _1 = (let data = parseState.GetInput(1) in (Microsoft.FSharp.Core.Operators.unbox data : 'ty)) in
            let _3 = (let data = parseState.GetInput(3) in (Microsoft.FSharp.Core.Operators.unbox data : 'ty)) in
            Microsoft.FSharp.Core.Operators.box
                (
                   (
# 87 "NanoMLParser.fsy"
                                          Fun (_1, _3) 
                   )
# 87 "NanoMLParser.fsy"
                 : 'ty));
# 624 "NanoMLParser.fs"
        (fun (parseState : Microsoft.FSharp.Text.Parsing.IParseState) ->
            let _2 = (let data = parseState.GetInput(2) in (Microsoft.FSharp.Core.Operators.unbox data : 'ty)) in
            Microsoft.FSharp.Core.Operators.box
                (
                   (
# 88 "NanoMLParser.fsy"
                                              _2 
                   )
# 88 "NanoMLParser.fsy"
                 : 'ty));
|]
# 636 "NanoMLParser.fs"
let tables () : Microsoft.FSharp.Text.Parsing.Tables<_> = 
  { reductions= _fsyacc_reductions ();
    endOfInputTag = _fsyacc_endOfInputTag;
    tagOfToken = tagOfToken;
    dataOfToken = _fsyacc_dataOfToken; 
    actionTableElements = _fsyacc_actionTableElements;
    actionTableRowOffsets = _fsyacc_actionTableRowOffsets;
    stateToProdIdxsTableElements = _fsyacc_stateToProdIdxsTableElements;
    stateToProdIdxsTableRowOffsets = _fsyacc_stateToProdIdxsTableRowOffsets;
    reductionSymbolCounts = _fsyacc_reductionSymbolCounts;
    immediateActions = _fsyacc_immediateActions;
    gotos = _fsyacc_gotos;
    sparseGotoTableRowOffsets = _fsyacc_sparseGotoTableRowOffsets;
    tagOfErrorTerminal = _fsyacc_tagOfErrorTerminal;
    parseError = (fun (ctxt:Microsoft.FSharp.Text.Parsing.ParseErrorContext<_>) -> 
                              match parse_error_rich with 
                              | Some f -> f ctxt
                              | None -> parse_error ctxt.Message);
    numTerminals = 30;
    productionToNonTerminalTable = _fsyacc_productionToNonTerminalTable  }
let engine lexer lexbuf startState = (tables ()).Interpret(lexer, lexbuf, startState)
let toplevel lexer lexbuf : toplevel_decl list =
    Microsoft.FSharp.Core.Operators.unbox ((tables ()).Interpret(lexer, lexbuf, 0))

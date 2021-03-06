// Signature file for parser generated by fsyacc
module NanoML.Compiler.Parser
type token = 
  | EOF
  | IN
  | END
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
  | STRING of (string)
  | BOOL of (bool)
  | FLOAT of (float)
  | INT of (int)
  | VAR of (string)
  | TARROW
  | TBOOL
  | TFLOAT
  | TINT
type tokenId = 
    | TOKEN_EOF
    | TOKEN_IN
    | TOKEN_END
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
    | TOKEN_STRING
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
type nonTerminalId = 
    | NONTERM__startexpr
    | NONTERM__starttoplevel
    | NONTERM_toplevel
    | NONTERM_def
    | NONTERM_expr
    | NONTERM_app
    | NONTERM_non_app
    | NONTERM_arithmetic
    | NONTERM_cond
    | NONTERM_ty
/// This function maps integers indexes to symbolic token ids
val tagOfToken: token -> int

/// This function maps integers indexes to symbolic token ids
val tokenTagToTokenId: int -> tokenId

/// This function maps production indexes returned in syntax errors to strings representing the non terminal that would be produced by that production
val prodIdxToNonTerminal: int -> nonTerminalId

/// This function gets the name of a token as a string
val token_to_string: token -> string
val expr : (Microsoft.FSharp.Text.Lexing.LexBuffer<'cty> -> token) -> Microsoft.FSharp.Text.Lexing.LexBuffer<'cty> -> (expr) 
val toplevel : (Microsoft.FSharp.Text.Lexing.LexBuffer<'cty> -> token) -> Microsoft.FSharp.Text.Lexing.LexBuffer<'cty> -> (toplevel_decl list) 

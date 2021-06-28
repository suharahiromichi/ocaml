{
open Parser
}

let space = [' ' '\t' '\n' '\r']
let digit = ['0'-'9']
let alpha = ['A'-'Z' 'a'-'z' '_']

rule token = parse
| "let"
    { LET }
| "in"
    { IN }
| "fun"
    { FUN }
| alpha (digit|alpha)*
    { VAR(Lexing.lexeme lexbuf) }
| '-'? digit+
    { CONST(int_of_string (Lexing.lexeme lexbuf)) }
| '('
    { LPAREN }
| ')'
    { RPAREN }
| '+'
    { PLUS }
| '='
    { EQUAL }
| "->"
    { ARROW }
| space+
    { token lexbuf }
      

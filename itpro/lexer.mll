{
open Parser
}

let space = [' ' '\t' '\n' '\r']
let digit = ['0'-'9']
let alpha = ['A'-'Z' 'a'-'z' '_']

rule token = parse
| "while"
    { WHILE }
| "print"
    { PRINT }
| alpha (digit|alpha)*
    (* 以上の予約語にマッチしなければ変数名として処理 *)
    { VAR(Lexing.lexeme lexbuf) }
| '-'? digit+
    { CONST(int_of_string (Lexing.lexeme lexbuf)) }
| space+
    (* 空白をスキップして字句解析を続行 *)
    { token lexbuf }
| '='
    { EQUAL }
| '+'
    { PLUS }
| '('
    { LPAREN }
| ')'
    { RPAREN }
| '>'
    { GREATER }
| '{'
    { LBRACE }
| '}'
    { RBRACE }
| ';'
    { SEMICOLON }
| _
    (* 以上にマッチしない場合はエラーとして例外を発生 *)
    { failwith
        (Printf.sprintf
           "unknown token %s near characters %d-%d"
           (Lexing.lexeme lexbuf)
           (Lexing.lexeme_start lexbuf)
           (Lexing.lexeme_end lexbuf))
    }

(* END *)

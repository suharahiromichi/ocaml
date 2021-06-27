./myc.top <<EOF
let ic = open_in "sum.myc" ;;
open Syntax ;;
let s =
    Parser.statement Lexer.token
      (Lexing.from_channel ic) ;;
EOF

%{
open Syntax
%}

%token <string> VAR /* 変数名 */
%token <int> CONST  /* 整数定数 */
%token EQUAL        /* = */
%token PLUS         /* - */
%token WHILE        /* 予約語「while」 */
%token LPAREN       /* ( */
%token RPAREN       /* ) */
%token GREATER      /* > */
%token LBRACE       /* { */
%token RBRACE       /* } */
%token SEMICOLON    /* ; */
%token PRINT        /* 予約語「print」 */

%type <Syntax.statement> statement
%start statement

%%

statement: /* 一つの文を構文解析するルール */
| VAR EQUAL CONST
    { Const($1, $3) }
| VAR EQUAL VAR PLUS VAR
    { Add($1, $3, $5) }
| WHILE LPAREN VAR GREATER VAR RPAREN statement
    { While($3, $5, $7) }
| LBRACE statement_list RBRACE
    { Seq($2) }
| PRINT VAR
    { Print($2) }
| error /* 以上にマッチしない場合はエラーとして例外を発生 */
    { failwith
        (Printf.sprintf
           "parse error near characters %d-%d"
           (Parsing.symbol_start ())
           (Parsing.symbol_end ())) }

statement_list: /* 文の列を構文解析するルール */
| statement SEMICOLON statement_list    /* 一つの文を，文のリストの先頭に追加 */
    { $1 :: $3 }
| /* 空列 */
    { [] }                              /* 空リストを返す */
                   
/* END */

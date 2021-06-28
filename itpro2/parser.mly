%{
open Syntax
%}

%token <string> VAR
%token <int> CONST
%token LET    /* 予約語「let」 */
%token IN     /* 予約語「in」 */
%token FUN    /* 予約語「fun」 */
%token LPAREN /* 「(」 */
%token RPAREN /* 「)」 */
%token PLUS   /* 「+」 */
%token EQUAL  /* 「=」 */
%token ARROW  /* 「->」 */

%type <Syntax.exp> exp
%start exp

%%

exp:
| CONST
  /* i */
       { Const($1) }
| VAR
  /* x */
       { Var($1) }
| LPAREN exp PLUS exp RPAREN
  /* ( 式1 + 式2 ) */
       { Add($2, $4) }
| LPAREN LET VAR EQUAL exp IN exp RPAREN
  /* ( let x = 式1 in 式2 ) */
       { Let($3, $5, $7) }
| LPAREN FUN VAR ARROW exp RPAREN
  /* ( fun x -> 式 ) */
       { Fun($3, $5) }
| LPAREN exp exp RPAREN
  /* ( 式1 式2 ) */
       { App($2, $3) }

/* END */

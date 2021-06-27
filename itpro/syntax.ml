type var = string                           (* 「変数名」を表す型 *)

type statement =                        (* 「文」を表す代数データ型 *)
  | Const of var * int               (* 「x = i」という形の文を表す *)
  | Add of var * var * var       (* 「x = y + z」という形の文を表す *)
  | While of var * var * statement (* 「while (x > y) 文」という形の文を表す *)
  | Seq of statement list (* 「{ 文1; 文2; …; 文n }」という形の文を表す *)
  | Print of var          (* 「print x」という形の文を表す *)


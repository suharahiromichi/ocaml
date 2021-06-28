type var = string

type exp =                              (* 「式」を表す代数データ型 *)
  | Const of int                        (* 整数定数 *)
  | Var of var                          (* 変数参照 *)
  | Add of exp * exp                    (* 整数加算 *)
  | Let of var * exp * exp              (* let式 *)
  | Fun of var * exp                    (* 関数式 *)
  | App of exp * exp                    (* 関数適用 *)

(* END *)

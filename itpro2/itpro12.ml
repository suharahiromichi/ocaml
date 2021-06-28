(**
第12回 「型推論」の実装法

https://xtech.nikkei.com/it/article/COLUMN/20070717/277580
 *)

#load "syntax.cmo" ;;
#load "parser.cmo" ;;
#load "lexer.cmo" ;;
open Syntax ;;

type typ =                                  (* 型を表すデータ型 *)
  | TInt                                    (* 整数型int *)
  | TFun of typ * typ                       (* 関数型t1 -> t2 *)
  | TVar of typ option ref                  (* 型変数 *)
;;

let rec unify t1 t2 =
  (* t1とt2を組にして両方の形で場合わけ *)
  match (t1, t2) with
  | (TInt, TInt) ->
     ()                     (* 確認が成功したのでダミーの値()を返す *)
  | (TFun(t11, t12), TFun(t21, t22)) ->
     unify t11 t21;                       (* 引数の型が等しいか確認 *)
     unify t12 t22                        (* 返値の型が等しいか確認 *)
  | (TVar(r1), TVar(r2)) when r1 == r2 ->
     ()
  | (TVar(r1), _) when not (occur r1 t2) ->
     (* t1はt2の中に現われない型変数 *)
     (match !r1 with
      | None ->                        (* t1は未定 *)
         r1 := Some(t2)                (* t1にt2を代入 *)
      | Some(t1') ->                   (* すでにt1'が代入されている *)
         unify t1' t2                  (* t1'とt2を等しくする *))
  | (_, TVar(r2)) ->                   (* t2が型変数 *)
      unify t2 t1                      (* t1とt2を入れ替えてunify *)
  | (_, _) ->                          (* 上述以外の場合 *)
     failwith "Type Error"             (* 型エラーを表す例外を発生 *)
;;

let rec infer tenv e =
  match e with
  | Const(_) -> TInt
  | Var(x) -> List.assoc x tenv
  | Add(e1, e2) ->
     unify (infer tenv e1) TInt;
     unify (infer tenv e2) TInt;
     TInt
  | Let(x, e1, e2) ->
     let t = infer tenv e1 in
     let newtenv = (x, t) :: tenv in
     infer newtenv e2
  | Fun(x, e0) ->
     let t1 = TVar(ref None) in
     let newtenv = (x, t1) :: tenv in
     let t2 = infer newtenv e0 in
     TFun(t1, t2)
  | App(e1, e2) ->
     let t1 = infer tenv e1 in
     let t2 = infer tenv e2 in
     let t = TVar(ref None) in
     unify t1 (TFun(t2, t));
     t
;;


let read_and_infer () =
  let e = Parser.exp Lexer.token (Lexing.from_channel stdin) in
  infer [] e
;;


read_and_infer () ;;
(1 + 2)
;;
(* - : typ = TInt *)


read_and_infer () ;;
(fun x -> (x + 3))  
;;
(* - : typ = TFun (TVar {contents = Some TInt}, TInt) *)

read_and_infer () ;;
(let y = (1 + 2) in (fun x -> (x + y)))
;;
(* - : typ = TFun (TVar {contents = Some TInt}, TInt) *)

read_and_infer () ;;
(fun x -> x)
;;
(* - : typ = TFun (TVar {contents = None}, TVar {contents = None}) 変数が残る。 *)

read_and_infer () ;;
(fun f -> (f f))
;;
(* Exception: Failure "Type Error" オーカーチェックによるエラー *)

(* END *)

(**
第15回 型からプログラムを当てる

https://xtech.nikkei.com/it/article/COLUMN/20071005/283903/
 *)

(**
## 「'a * 'b -> 'b * 'a」という型を持つ（もっとも簡単な）関数
 *)
fun (x, y) -> (y, x) ;;           (* - : 'a * 'b -> 'b * 'a = <fun> *)

(**
## 「'a -> 'a -> 'a」という型を持つ（もっとも簡単な）関数
 *)

(fun x y -> x : 'a -> 'a -> 'a) ;;    (* - : 'a -> 'a -> 'a = <fun> *)
(fun x y -> y : 'a -> 'a -> 'a) ;;    (* - : 'a -> 'a -> 'a = <fun> *)
(* おなじことだが *)
fun (x : 'a) (y : 'a) -> x ;;         (* - : 'a -> 'a -> 'a = <fun> *)
fun x y -> if true then x else y ;;   (* - : 'a -> 'a -> 'a = <fun> *)
(* if式のthenとelseの部分は同じ型になることを利用 *)

(**
## type-directed 部分評価
 *)
let counter = ref 0
let gensym () =
  incr counter;
  Printf.sprintf "x%d" !counter

type 'a typ = ('a -> string) * (string -> 'a)

let val2str (t : 'a typ) (v : 'a) : string = fst t v
let str2val (t : 'a typ) (s : string) : 'a = snd t s

let z : string typ = (fun v -> v), (fun s -> s)

let ( ^-> ) (t1 : 'a typ) (t2 : 'b typ) : ('a -> 'b) typ =
  (fun f ->
    let x = gensym () in
    Printf.sprintf
      "(fun %s -> %s)"
      x
      (val2str t2 (f (str2val t1 x)))),
  (fun s ->
    fun x ->
      str2val t2
        (Printf.sprintf
           "(%s %s)"                        (* 第16回で修正 *)
           s
           (val2str t1 x)))

let ( *^ ) (t1 : 'a typ) (t2 : 'b typ) : ('a * 'b) typ =
  (fun (v1, v2) ->
    Printf.sprintf
      "(%s, %s)"
      (val2str t1 v1)
      (val2str t2 v2)),
  (fun s ->
    (str2val t1
       (Printf.sprintf
          "fst %s"
          s),
     str2val t2
       (Printf.sprintf
          "snd %s"
          s)))

(**
## 実行例
 *)
(* 関数fを用意する。 *)
let f (x : 'a) (y : 'a) = x ;;              (* val f : 'a -> 'a -> 'a = <fun> *)

(* f とその型を与える。 *)
val2str (z ^-> z ^-> z) f ;;
(* - : string = "(fun x1 -> (fun x2 -> x1))" *) (* f の定義が復元される。 *)

(**
## 別の例
 *)
let g x y = (y, x) ;;                       (* val g : 'a -> 'b -> 'b * 'a = <fun> *)
val2str (z ^-> z ^-> z *^ z) g ;;
(* - : string = "(fun x3 -> (fun x4 -> (x4, x3)))" *)

(**
## 簡単なコードが生成される例
 *)
let h =
  fun (x, y) ->
  let z = (y, x) in
  (fst z, snd z) ;;           (* val h : 'a * 'b -> 'b * 'a = <fun> *)

val2str (z *^ z ^-> z *^ z) h ;;
(* - : string = "(fun x5 -> (snd x5, fst x5))" *) (* より簡単な関数が得られた。 *)

(* END *)

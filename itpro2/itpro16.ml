(**
第16回 すべてのものは関数である

https://xtech.nikkei.com/it/article/COLUMN/20071101/286199/
 *)

(**
# 論理値と条件式をλ式で表す
 *)

type 'a my_unit = 'a -> 'a ;;
type 'a my_bool = ('a my_unit -> 'a) -> ('a my_unit -> 'a) -> 'a ;;

let e : 'a my_unit = fun x -> x ;;
let my_true : 'a my_bool = fun x -> fun y -> x e ;;
let my_false : 'a my_bool = fun x -> fun y -> y e ;;

(**
「true && false」 を表す。

if true then false else false なので true (λd. false) (λd. false) である。
 *)
let ans1 : 'a my_bool =
  my_true (fun d -> my_false) (fun d -> my_false) ;;
(**
前回のtype-directed部分評価を使う。
 *)
val2str (((z ^-> z) ^-> z) ^-> ((z ^-> z) ^-> z) ^-> z) ans1 ;;
(* - : string = "(fun x6 -> (fun x7 -> (x7 (fun x8 -> x8))))" *)
(**
my_false と同じ式が得られた。
 *)

(**
# 自然数と足し算をλ式で表す
 *)
(* 自然数の型'a natを定義 *)
(* 'aはその自然数に対するループ結果の型 *)

type 'a nat = 'a -> ('a -> 'a) -> 'a ;;

let zero : 'a nat = fun x -> fun f -> x ;;
let succ (e : 'a nat) : 'a nat = fun x -> fun f -> e (f x) f ;;
let add (e1 : 'a nat) (e2 : 'a nat) : 'a nat =
  fun x -> fun f -> e2 (e1 x f) f ;;

(**
1 + 2 = 3
 *)
let ans2 : 'a nat = add (succ zero) (succ (succ zero)) ;;
val2str (z ^-> (z ^-> z) ^-> z) ans2 ;;
(* - : string = "(fun x4 -> (fun x5 -> (x5 (x5 (x5 x4)))))" *)

(**
# 掛け算
 *)
let mul (e1 : 'a nat) (e2 : 'a nat) : 'a nat =
  fun x -> fun f -> e2 x (fun y -> e1 y f) ;;

(**
2 * 2 = 4
 *)
let ans3 : 'a nat = mul (succ (succ zero)) (succ (succ zero)) ;;
val2str (z ^-> (z ^-> z) ^-> z) ans3 ;;
(* - : string = "(fun x6 -> (fun x7 -> (x7 (x7 (x7 (x7 x6))))))" *)

(*
# 引き算と組
 *)

(* 一級多相型を利用するためにレコードを定義 *)
type my_unit = { u : 'a. 'a -> 'a }
type my_bool = { b : 'a. (my_unit -> 'a) -> (my_unit -> 'a) -> 'a }
type nat = { n : 'a. 'a -> ('a -> 'a) -> 'a }
type ('a, 'b) pair = { p : 'c. ('a -> 'b -> 'c) -> 'c }

(* 型を合わせるために，例えば自然数の3だったら
   { n = fun x -> fun f -> f (f (f x)) }のようなレコードとし，
   関数として適用する際に，e.nのように中身を取り出す *)

let e = { u = fun x -> x }
let my_true = { b = fun x -> fun y -> x e }
let my_false = { b = fun x -> fun y -> y e }

(* 先のtrue && falseを書き直した式 *)
let ans1 = my_true.b (fun d -> my_false) (fun d -> my_false)

let zero = { n = fun x -> fun f -> x }
let succ e = { n = fun x -> fun f -> e.n (f x) f }
let add e1 e2 = { n = fun x -> fun f -> e2.n (e1.n x f) f }
let mul e1 e2 = { n = fun x -> fun f -> e2.n x (fun y -> e1.n y f) }
let iszero e = e.n my_true (fun x -> my_false)

(* 先と同じ1+2と2*2の例 *)
let ans2 = add (succ zero) (succ (succ zero))
let ans3 = mul (succ (succ zero)) (succ (succ zero))

let pair e1 e2 = { p = fun f -> f e1 e2 }
let pred e =
  (e.n
     (pair zero zero)
     (fun a -> a.p (fun x -> fun y -> pair (succ x) x))).p
    (fun x -> fun y -> y)
let sub e1 e2 = e2.n e1 pred
;;

(**
5 - 2 = 3
 *)                  
let ans4 =
  sub
    (succ (succ (succ (succ (succ zero)))))
    (succ (succ zero)) ;;

val2str (z ^-> (z ^-> z) ^-> z) ans4.n ;;
(* - : string = "(fun x8 -> (fun x9 -> (x9 (x9 (x9 x8)))))" *)

(**
# 再帰関数

ocaml -rectypes で起動すること。
 *)
let fib =
  fun fib ->
  fun x ->
  if x <= 1 then x else
    fib fib (x - 1) + fib fib (x - 2) ;;
fib fib 10 ;;                               (* - : int = 55 *)

let fib =
  fun fib ->
  fun x ->
  (iszero (sub x (succ zero))).b
    (fun d -> x)
    (fun d ->
      add
        (fib fib (sub x (succ zero)))
        (fib fib (sub x (succ (succ zero))))) ;;

(*
fib 10 = 55
 *)
let ans5 = fib fib
             (succ (succ (succ (succ (succ (succ (succ (succ (succ (succ zero)))))))))) ;;

val2str (z ^-> (z ^-> z) ^-> z) ans5.n ;;
(*
- : string =
"(fun x3 -> (fun x4 ->
(x4 (x4 (x4 (x4 (x4 (x4 (x4 (x4 (x4 (x4
(x4 (x4 (x4 (x4 (x4 (x4 (x4 (x4 (x4 (x4
(x4 (x4 (x4 (x4 (x4 (x4 (x4 (x4 (x4 (x4
(x4 (x4 (x4 (x4 (x4 (x4 (x4 (x4 (x4 (x4
(x4 (x4 (x4 (x4 (x4 (x4 (x4 (x4 (x4 (x4
(x4 (x4 (x4 (x4 (x4 x3)))))
))))))))))))))))))))))))))))))))))))))))))))))))))"
 *)

(* END *)

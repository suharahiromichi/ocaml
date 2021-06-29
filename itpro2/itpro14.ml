(**
第14回 型＝命題，プログラム＝証明

https://xtech.nikkei.com/it/article/COLUMN/20070909/281498/
 *)

(**
## 「A ⇒ BとB ⇒ Cが成り立てば，A ⇒ Cが成り立つ」という証明
 *)
fun (f, g) -> fun x -> g (f x) ;;
(* - : ('a -> 'b) * ('b -> 'c) -> 'a -> 'c = <fun> *)

(**
または を表す型
 *)
type ('a, 'b) or_t = Left of 'a | Right of 'b ;;

(**
## 「A または B ならば B または A」の証明
 *)
fun x ->
match x with
| Left y -> Right y
| Right z -> Left z ;;
(* - : ('a, 'b) or_t -> ('b, 'a) or_t = <fun> *)

(**
真 を表す型
 *)
() ;;                                       (* - : unit = () *)

(**
偽 を表す型
 *)
type void = V of void ;;                   (* type void = V of void *)

(**
否定 を表す型
 *)
type 'a not = N of ('a -> void) ;;          (* type 'a not = N of ('a -> void) *)

(**
## 「￢T ⇒ F」の証明
 *)
fun (N x) -> x ();;
(* - : unit not -> void = <fun> *)

(* 「否定 の 否定 は 肯定」を表す型 *)
let notnot = fun (x : 'a not not) -> (failwith "unimplemented" : 'a) ;;
(* val notnot : 'a not not -> 'a = <fun> *)

(**
￢￢(A∨￢A) を表す型
 *)
let x =
    N (fun (N y) ->
      y (Right (N (fun z ->
                    y (Left z))))) ;;
(* val x : ('a, 'a not) or_t not not = N <fun> *)

(**
## 「A ∨￢A」 の証明

例外が起きているが、型エラーではない。つまり証明になっている。
 *)
notnot x;;
(* Exception: Failure "unimplemented". *)

(* END *)

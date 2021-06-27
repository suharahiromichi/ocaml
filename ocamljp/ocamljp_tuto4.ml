(**
https://ocaml.jp/
 *)
(**
# OCaml入門(4)

## 多相バリアント (polymorphic variant)
 *)

let a = 1;;
let a : int = 1;;


let t = [`A; `B];;          (* AまたはB型からなるリスト型 *)
(* val t : [> `A | `B ] list = [`A; `B] *)
(* > はスーパーセットであることを示す。 *)

let t1 = `C :: t;;                          (* t の型を拡張する。 *)
(* val t1 : [> `A | `B | `C ] list = [`C; `A; `B] *)

let a = [`A 10; `B 1.0];;  (* int型 または float型 からなるリスト型 *)
(* val a : [> `A of int | `B of float ] list = [`A 10; `B 1.] *)

let f = function
  | `A x -> (float_of_int x)
  | `B x -> x
;;
(* val f : [< `A of int | `B of float ] -> float = <fun> *)
(* < サブセットであることを示す。 *)

f (`A 10);;                                 (* - : float = 10 *)
f (`B 1.0);;                                (* - : float = 1. *)

let g = function
  | `A x -> x
  | `B x -> x
;;
(* val g : [< `A of 'a | `B of 'a ] -> 'a = <fun> *)
f (`A 10);;                                 (* - : int = 10 *)
f (`B 1.0);;                                (* - : float = 1. *)

(**
# osiire’s blog

https://osiire.hatenablog.com/entry/20090510/1241957550
 *)
(**
## 多相バリアントを使いこなそう(1)

### 多相バリアントの基本
 *)
type card = Joker | Num of int;;
(* type card = Joker | Num of int *)

type in_data = Str of string | Num of int;;
(* type in_data = Str of string | Num of int *)

let get_number = function    
  | Num i -> i 
  | _ -> failwith "not a number";;          (* 戻り値に影響しない。 *)
(* val get_number : in_data -> int = <fun> *)
(* card 型では使えない。 *)

type card = [ `Jorker | `Num of int ];;     
(* type card = [ `Jorker | `Num of int ] *)

type in_data = [ `Str of string | `Num of int ];; (* 多相バリアント型 *)
(* type in_data = [ `Num of int | `Str of string ] *)

let get_number = function (* 多相バリアント型の引数を受け取る *)
  | `Num i -> i
  | _ -> failwith "not a number";;
(* val get_number : [> `Num of 'a ] -> 'a = <fun> *)

get_number (`Num 3);;         (* - : int = 3 *)
get_number (`Joker);;         (* Exception: Failure "not a number". *)

(**
### 要点

多相バリアント型のタグは特定の多相バリアント型に属さず、同じタグで
あれば複数の多相バリアント型のタグになり得る(多相)という訳です。
 *)

(**
## 多相バリアントを使いこなそう(2)

### 多相バリアント型を組み合わせる
 *)
(*
多相バリアントを使用しない場合

type keyboard_event = KeyPress of char | KeyRelease of char;;
type mouse_event = 
  | MousePress of int * int 
  | MouseRelease of int * int 
  | Click of int * int;;
type event = Key of keyboard_event | Mouse of mouse_event;;
 *)

type keyboard_event = [ `KeyPress of char | `KeyRelease of char ];;
type mouse_event = 
  [ `MousePress of int * int
  | `MouseRelease of int * int
  | `Click of int * int ];;
type event = [ keyboard_event | mouse_event ];; (* 組み合わせる *)
(*
type mouse_event =
[ `Click of int * int
| `KeyPress of char
| `KeyRelease of char
| `MousePress of int * int
| `MouseRelease of int * int ]
 *)

(**
### 操作の再利用性
 *)
(*
多相バリアントを使用しない場合

let get_char = function 
  | KeyPress c -> c
  | KeyRelease c -> c;;
(* val get_char : keyboard_event -> char = <fun> *)
let get_char_from_event = function 
  | Key (KeyPress c) -> c
  | Key (KeyRelease c) -> c
  | _ -> failwith "not a key";;
(* val get_char : keyboard_event -> char = <fun> *)
 *)

let get_char = function                 (* 再利用可能なget_char関数 *)
  | `KeyPress c -> c
  | `KeyRelease c -> c
  | _ -> failwith "not a key";;
(* val get_char : [> `KeyPress of 'a | `KeyRelease of 'a ] -> 'a = <fun> *)
`KeyPress 'u';;       (* - : [> `KeyPress of char ] = `KeyPress 'u' *)
get_char (`KeyPress 'u');;                  (* - : char = 'u' *)
get_char (`Click (42, 24));;     (* Exception: Failure "not a key". *)
(* get_char 1;; *)                     (* コンパイルエラー *)

(**
## 多相バリアントを使いこなそう(3)

### 三つの型
 *)
type t1 = [ `A | `B ];;                        (* 固定 *)
type 'a t2 = 'a constraint 'a = [> `A | `B ];; (* 開いている *)
type 'a t3 = 'a constraint 'a = [< `A | `B ];; (* 閉じている *)

(**

- t1の意味：`Aと`Bというタグを必ず持ち、それ以外のタグは一切持っていない
   多相バリアント型。

- 'a t2の'aの意味：多相バリアント型ならなんでもいいのだけど、列挙されて
   いる`Aと`Bというタグは必ず持っていなければならない型。

- 'a t3の'aの意味：多相バリアント型ならなんでもいいのだけど、列挙されて
   いる`Aと`B以外のタグを持っていてはいけない型。
 *)

let get_a = function
  | `A -> 1;;
(* val get_a : [< `A ] -> int = <fun> *)

let get_a' = function
  | `A -> 1
  | _ -> 0;;
(* val get_a : [> `A ] -> int = <fun> *)

let get_ab = function
  | `A -> 1
  | `B -> 0;;
(* val get_ab : [< `A | `B ] -> int = <fun> *)

let get_ab' = function
  | `A -> 1
  | `B -> 0
  | _ -> -1;;
(*  val get_ab' : [> `A | `B ] -> int = <fun> *)

(* get_a `C;; *)                            (* コンパイルエラー *)
(* get_ab `C;; *)                           (* コンパイルエラー *)
get_ab' `C;;                                (* - : int = -1 *)

(**
### 型の大小関係

閉じた型 ≦ 固定された型 ≦ 開いた型
 *)
(`A : [< `A | `B ] :>  [ `A | `B ]);;       (* 閉じている -> 固定 *)
(* - : [ `A | `B ] = `A *)

(`A : [ `A | `B ] :> [> `A | `B ]);;        (* 固定 -> 開いている *)
(* - : [> `A | `B ] = `A *)


`A;;                                  (* - : [> `A ] = `A *)

(`A :> [< `A]);;                      (* - : [ `A ] = `A *)
(`A :> [`A]);;                        (* - : [ `A ] = `A *)
(`A :> [> `A]);;                      (* - : [> `A ] = `A *)

(`A :> [< `A | `B]);;                 (* - : [< `A | `B > `A ] = `A *)
(`A :> [`A | `B]);;                   (* - : [ `A | `B ] = `A *)
(`A :> [> `A | `B]);;                 (* - : [> `A | `B ] = `A *)

(`A : [< `A | `B ] :>  [ `A | `B ]);;      (* 閉じている -> 固定 *)

(**
## 多相バリアントを使いこなそう(4)

### 場合分け構造の拡張は難しい
 *)

module Card = struct
  type t = [ `Num of int | `Jack | `Queen | `King ]
  let num = function
    | `Num i -> i
    | `Jack -> 11
    | `Queen -> 12
    | `King -> 13
end;;

module CardEx = struct                    (* 拡張Card型 *)
  type t = [ Card.t | `Joker ]            (* 新しい場合分けを増やす *)
  let num = function
    | #Card.t as c -> Card.num c (* #多相バリアント型名で、一気にパターンマッチできる *)
    | `Joker -> 0
  let next = function                       (* 新しい関数 *)
    | `Num i when i < 10 -> `Num (i + 1)
    | `Num _ -> `Jack
    | `Jack -> `Queen
    | `Queen -> `King
    | `King -> `Joker
    | `Joker -> `Num 1
end;;

Card.num `King;;                            (* - : int = 13 *)
CardEx.num `King;;                          (* - : int = 13 *)
CardEx.next `King;; (* - : [> `Jack | `Joker | `King | `Num of int | `Queen ] = `Joker *)

(**
- 静的で安全に
- 既存のコードを改造せず
- Jokerという新しい場合分けを加え、
- nextという新しい操作を加え、
- CardExという新しい構造を定義できています。
 *)

(** ### Expression Problem

   静的な型付けの下で、場合分けのデータ構造に対して、新しい場合分けと
   その場合に対する新しい処理を、元のソースコードに手を加えることなく
   拡張定義することだ。

   ある言語がこのExpression Problemを解決できるかどうかは、その言語の
   表現能力を示す顕著な指標だ。行と列を持つ表を考えると、いうなれば場
   合分けを行、処理を列と捉えることができる。

   関数型言語では、列(処理)を加えることは簡単だけれども、行は(データ型
   宣言により)固定されてしまう。

   オブジェクト指向言語では、新しい行(サブクラス)を加えることは簡単だ
   けれども、列は(メソッド定義により)固定されてしまう。我々は、行と列
   の両方を簡単に加えたいのだ。
*)

  

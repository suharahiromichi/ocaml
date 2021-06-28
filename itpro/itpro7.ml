(**
第7回 「代数データ型」でいろいろなデータを表してみる

https://xtech.nikkei.com/it/article/COLUMN/20070213/261798
 *)

(**
## 型変数を持つ型（多相型）
 *)

type 'a result =
  | Success of 'a
  | Failure of string
;;

Success 123 ;;                      (* - : int result = Success 123 *)
Success 4.5 ;;                    (* - : float result = Success 4.5 *)
Success "hello" ;;           (* - : string result = Success " *)

(**
## リストや木やHTML文書を表してみる

例えば，同じ型の任意個（ただし有限個）の値を並べることができるリストは，
以下のように定義できる。
*)

type 'a list =
  | Nil                  (* 空のリスト *)
  | Cons of 'a * 'a list (* 'a型の要素をリストに追加したもの（consセル） *)
;;

(* 整数1, 2, 3を要素とする，長さ3のリスト *)
Cons(1, Cons(2, Cons(3, Nil))) ;;
(* - : int list = Cons (1, Cons (2, Cons (3, Nil))) *)

(* 文字列"hello"と"world"を要素とする，長さ2のリスト *)
Cons("hello", Cons("world", Nil)) ;;
(* - : string list = Cons ("hello", Cons ("world", Nil)) *)

1 :: 2 :: 3 :: [] ;;
(* - : int list = [1; 2; 3] *)

"hello" :: "world" :: [] ;;
(* - : string list = ["hello"; "world"] *)

(**
２分木の例
 *)

type 'a tree =
  | Leaf of 'a                              (* 'a型の値を持つ葉 *)
  | Node of 'a tree * 'a tree               (* 二つの子を持つ枝 *) ;;

let x = Node(Node(Leaf 1, Leaf 2), Leaf 3) ;;

(* 二分木を受け取って，深さ優先探索でリストにする関数 *)
let rec dfs t =
  match t with
  | Leaf x -> [x]               (* xのみを要素とする，長さ1のリスト *)
  | Node(t1, t2) -> dfs t1 @ dfs t2   (* @はリストとリストの連結 *)
;;

dfs x ;;
(* - : int list = [1; 2; 3] *)

(**
簡単なHTML文書を定義してみよう。
 *)
type html = { head : head; body : block list }
  and head = Title of string
  and block =
    | P of inline list
    | H of int * inline list
    | List of html_list   (* ← OCamlのlistと型名が重複しないように *)
  and inline = string
  and html_list = UL of li list | OL of li list
  and li = LI of flow list
  and flow = BlockFlow of block | InlineFlow of inline
;;

type html = { head : head; body : block list; }
and head = Title of string
and block = P of inline list | H of int * inline list | List of html_list
and inline = string
and html_list = UL of li list | OL of li list
and li = LI of flow list
and flow = BlockFlow of block | InlineFlow of inline
;;

(* 以下のようなHTML文書をイメージ *)
(*
    <html>
      <head><title>ITpro</title></head>
      <body>
        <h1>Greetings</h1>
        <p>hello goodbye</p>
        <ul>
          <li>foo</li>
          <li><p>bar</p></li>
        </ul>
      </body>
    </html>
  *)
(*
{ head = Title "ITpro";
  body = [H(1, ["Greetings"]);
          P ["hello"; "goodbye"];
          List(UL [LI [InlineFlow "foo"];
                   LI [BlockFlow(P ["bar"])]])] }
 *)

(**
## 代数データ型の何が「代数」的なのか？
 *)
(* string + float を定義 *)
type t' = T1 of string | T2 of float ;;

(* int * (string + float) を定義 *)
type t = T of int * t' ;;

(* string * int + float * int を定義 *)
type s = S1 of string * int | S2 of float * int ;;

# (* tからsへの変換関数 *)
let t_to_s x =
  match x with
  | T(i, T1 s) -> S1(s, i)
  | T(i, T2 f) -> S2(f, i) ;;

# (* sからtへの変換関数 *)
let s_to_t y =
  match y with
  | S1(s, i) -> T(i, T1 s)
  | S2(f, i) -> T(i, T2 f) ;;

(* t_to_sをしてからs_to_tをすると元に戻る（逆も同じ） *)
s_to_t (t_to_s (T(123, T2(4.5)))) ;;
(* - : t = T (123, T2 4.5) *)

(**
代数データ型は実際にある種の代数の構造を持っている。
論理学を知っていれば*と+の代わりに∧と∨を，集合論を知っていれば∩と∪を考えてもよい。
*)

(* END *)

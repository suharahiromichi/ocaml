(* 多相バリアント Polymorphic Variant *)

(*
  The Objective Caml system release 3.12
  OCaml.JPによる日本語訳にもとづき、実行できるようにした。
  また、ソースコード単独で理解できるように、型宣言を追加し、改行を増やした。
  説明全体は、@suharahiromichi の個人的なもので、正しさは保証しません。
  
  [>...]を開いた型、[<...]を閉じた型、[...]を固定した型と呼ぶこともある。
  http://d.hatena.ne.jp/osiire/20090514
  しかし、固定した型と閉じた型の違いが分かりにくいから、
  やはり大小関係で考えるのがよいのではないか。
*)

(*
  許されるタグ(tag)の集合の包含関係から、「大きい型」「小さい型」と呼ぶ。
  大きい型に対して、小さい型を与えてもマッチするが、その逆は保証されない。
*)
(*************)
(* 基本 ******)
(*************)
(* `On 、`Off 、`Number が tag-name *)

(* 「>」より大きい型へ、代入できることを、意味する。 *)
let a : [> `Off | `On ] list = [`On; `Off];; (* これはリストである。 *)
let b : [> `Number of int ] = `Number 1;; (* これはリストではない。 *)

(* 「<」はより小さい型を、を引数に取りうることを、意味する。 *)
let f : [< `Number of int | `Off | `On ] -> int =
  function
    | `On -> 1
    | `Off -> 0
    | `Number n -> n + 1
;;

(* [< `Number of int | `Off | `On ] list から int list を求める。*)
List.map f [`On; `Off; `Number 1];;

(* [< `Number of int | `Off | `On ] は、[> `Off | `On ] より大きい。*)
(* f には、より小さい型が与えられている *)
List.map f [`On; `Off];;

(***************)
(* 高度な使い方 *)
(***************)
(* パターンマッチが開いているので、「>」より大きい型を、引数に取りうる。 *)
let f0 : ([> `A | `B | `C | `D ] as 'a) -> 'a =
  function
    | `A -> `C
    | `B -> `D
    | x -> x
;;

(* f12 の &&の型推論によって、タグ`Cは消えてしまう。 *)
let f1 : [< `A of int | `B | `C ] -> bool =
  function
    | `A x -> x = 1
    | `B -> true
    | `C -> false
;;
let f2 : [< `A of string | `B ] -> bool =
  function
    | `A x -> x = "a"
    | `B -> true
;;
let f12 : [< `A of string & int | `B ] -> bool =
  fun x -> f1 x && f2 x
;;

(* *********************************** *)
(* 多相バリアントな型に名前をつけてみる* *)
(* *********************************** *)
(* wlist は vlist より大きい型である。 *)
type 'a vlist = [`Nil | `Cons of 'a * 'a vlist];;
type 'a wlist = [`Nil | `Cons of 'a * 'a wlist | `Snoc of 'a wlist * 'a];;

let rec map f : 'a vlist -> 'b vlist = function
  | `Nil -> `Nil
  | `Cons (a, l) -> `Cons (f a, map f l)
;;

(* 型を変換する関数 *)
(* 小さい型から、大きい型への変換 *)
let wlist_of_vlist : 'a vlist -> 'a wlist =
  fun l -> (l : 'a vlist :> 'a wlist) (* 「:>」は型変換をおこなう。 *)
;;
(* 型から、「>」より大きい型への変換 *)
let open_vlist : 'a vlist -> [> 'a vlist ] =
  fun l -> (l : 'a vlist :> [> 'a vlist]) (* 「:>」は型変換をおこなう。 *)
;;
let g : [< `A | `B | `C ] -> [ `A | `B | `C ] =
  fun x -> (x :> [ `A | `B | `C ])
;;

let a1 : int vlist = `Cons (1, `Nil);;
map (fun x -> x) a1;;
(*
map (fun x -> x) (wlist_of_vlist a1);;  (* mapには、wlist型は大きすぎる。 *)
*)

(* パターンマッチを使って、値を選択的に型変換する関数 *)
let split_cases : [< `Cons of 'a | `Nil | `Snoc of 'b ] ->
  [> `A of [> `Cons of 'a | `Nil ] | `B of [> `Snoc of 'b ] ] =
  function
    | `Nil
    | `Cons _ as x -> `A x              (* x は、`Cons(_,_)全体 *)
    | `Snoc _ as x -> `B x              (* x は、`Snoc(_,_)全体 *)
;;

(*
  バリアントタグを組み合わせた or パターンが alias パターンの中に現れ
  た場合には、その alias には or パターンで列挙されたタグだけを含む型がつ
  けられます。 これによって、関数の漸次定義などの便利なイディオムが実現で
  きます。
*)
let num : 'a -> [> `Num of 'a ] =
  fun x -> `Num x
;;
let eval1 : 'a -> [< `Num of 'b ] -> 'b =
  fun eval ->
    function
      | `Num x -> x
;;
let rec eval : [< `Num of 'a ] -> 'a =
  fun x -> eval1 eval x
;;
let plus : 'a -> 'b -> [> `Plus of 'a * 'b ] =
  fun x y -> `Plus (x, y)
;;
let eval2 : ('a -> int) -> [< `Num of int | `Plus of 'a * 'a ] -> int =
  fun eval ->
    function
      | `Plus (x, y) -> eval x + eval y
      | `Num _ as x -> eval1 eval x
;;
let rec eval : ([< `Num of int | `Plus of 'a * 'a ] as 'a) -> int =
  fun x -> eval2 eval x
;;

(*
  これをもっと便利に使うために、型定義を or パターンの略記として使うこ
  ともできます。 すなわち、 type myvariant = [`Tag1 int | `Tag2 bool] を
  定義すると、 #myvariant というパターンは (`Tag1(_ : int) | `Tag2(_ :
  bool)) と書いたのと等価になります。
*)
type myvariant = [`Tag1 of int | `Tag2 of bool];;

(* このような略記を単独で使うこともできますし、 *)
let f4 : [< `Tag1 of int | `Tag2 of bool | `Tag3 ] -> string =
  function
    | #myvariant -> "myvariant"
    | `Tag3 -> "Tag3"
;;

(* alias と組み合わせることもできます。 *)
let g1 : [< `Tag1 of 'a | `Tag2 of 'b ] -> string =
  function
    | `Tag1 _ -> "Tag1"
    | `Tag2 _ -> "Tag2"
;;

let g : [< `Tag1 of int | `Tag2 of bool | `Tag3 ] -> string =
  function
    | #myvariant as x -> g1 x
    | `Tag3 -> "Tag3"
;;

(*
  多相バリアントの弱点 (引用）

  多相バリアントの威力を見ると、なぜこれで通常のバリアントを置き換えずに、
  新たに追加することにしたのか疑問を覚える人もいるでしょう。

  答えはふたつあります。 ひとつには、多相バリアントは、非常に効率的である
  一方で、最適化のための静的型情報が欠落しているため、通常のバリアントよ
  りもわずかに低速だということがあります。しかしながら、この違いが顕著に
  なるのは巨大なデータ構造を扱った場合だけです。

  より重要なのは、多相バリアントは型安全であるとはいうものの、それはより
  弱い型制約においてのことだということです。 すなわち、通常のバリアントは
  実際に型安全性を保証するだけでなく、宣言された型構成子だけを使い、デー
  タ構造中に存在する構成子はすべて互換性があり、構成子のパラメータにに型
  制約が付いていることを検査しています。

  このことから、多相バリアントを使っている場合には型を明示するようより深
  く注意しなければなりません。 ライブラリを書いている場合には、インタフェー
  スに精確な型を書くことができるので簡単です。 しかし、単純なプログラムに
  は通常のバリアントを使っておいた方がよいでしょう。
*)

(*
(*
  特定のイディオムによって簡単なエラーが見つけにくくなることにも注意して
  ください。 例えば、次のコードは恐らく間違っていますが、コンパイラにはそ
  れを検出する術がありません。
*)
type abc = [`A | `B | `C] ;;
let f : [< `A | `As | `B | `C ] -> string =
  function
    | `As -> "A"
    | #abc -> "other"
;;
let f : abc -> string = f;;

(* このような危険性は定義自体に注釈を付けることで避けることができます。 *)
let f : abc -> string =
  function
    | `As -> "A"                        (* warning 11: this match case is unused. *)
    | #abc -> "other"
;;
*)

(* END *)

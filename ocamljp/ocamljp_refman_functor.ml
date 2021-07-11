(**
https://ocaml.jp/refman/ch02s03.html

https://ocaml.jp/refman/ch02s04.html
 *)

type comparison = Less | Equal | Greater;;

(**
## 基本的な functor ファンクタ
*)

module type ORDERED_TYPE =
  sig
    type t
    val compare: t -> t -> comparison
  end;;

module Set =
  functor (Elt: ORDERED_TYPE) ->
  struct
    type element = Elt.t
    type set = element list
    let empty = []
    let rec add x s =
      match s with
      | [] -> [x]
      | hd::tl ->
         match Elt.compare x hd with
         | Equal   -> s         (* x is already in s *)
         | Less    -> x :: s    (* x is smaller than all elements of s *)
         | Greater -> hd :: add x tl
    let rec member x s =
      match s with
      | [] -> false
      | hd::tl ->
         match Elt.compare x hd with
         | Equal   -> true     (* x belongs to s *)
         | Less    -> false    (* x is smaller than all elements of s *)
         | Greater -> member x tl
  end;;

(**
### 普通に比較する。
 *)
module OrderedString =
  struct
    type t = string
    let compare x y =
      if x = y then Equal else if x < y then Less else Greater
  end;;
module OrderedStringSet = Set(OrderedString);;
OrderedStringSet.member "foo" (OrderedStringSet.add "foo" OrderedStringSet.empty);;

(**
### 小文字で比較する。
 *)
module NoCaseString =
  struct
    type t = string
    let compare s1 s2 =
      OrderedString.compare
        (String.lowercase_ascii s1)
        (String.lowercase_ascii s2)
  end;;
module NoCaseStringSet = Set(NoCaseString);;
NoCaseStringSet.member "FOO" (NoCaseStringSet.add "foo" NoCaseStringSet.empty);;

(**
## functor signature ファンクタ・シグネチャを使う場合
*)

module type SETFUNCTOR =
  functor (Elt : ORDERED_TYPE) ->
  sig
    type element = Elt.t      (* concrete *)
    type set                  (* abstract *)
    val empty : set
    val add : element -> set -> set
    val member : element -> set -> bool
  end;;
module AbstractSet = (Set : SETFUNCTOR);;

(**
### 普通に比較する。
 *)
module OrderedStringSet1 = AbstractSet(OrderedString);;
OrderedStringSet1.member "foo" (OrderedStringSet1.add "foo" OrderedStringSet1.empty);;

(**
### 小文字で比較する。
 *)
module NoCaseStringSet1 = AbstractSet(NoCaseString);;
NoCaseStringSet1.member "FOO" (NoCaseStringSet1.add "foo" NoCaseStringSet1.empty);;

(**
## ファンクタの返すストラクチャのシグネチャに名前を与える。

with type 構文によりシグネチャに型の等価性情報を追加する必要がある。
 *)
module type SET =
  sig
    type element
    type set
    val empty : set
    val add : element -> set -> set
    val member : element -> set -> bool
  end;;

module WrongSet =
  (Set : functor(Elt: ORDERED_TYPE) -> SET);;
module WrongStringSet = WrongSet(OrderedString);;
(* WrongStringSet.add "gee" WrongStringSet.empty;; エラー *)
(**
ここで起きている問題は、 SET が element 型を抽象型として定義しているた
め、ファンクタの返した内容の element 型とファンクタの引数に与えられた
型 t との等価性に関する情報が失われてしまっていることです。 その結果、
WrongStringSet.element と string が同じ型でなくなり、 WrongStringSet
の操作に string が使えなくなってしまっています。

この例からもわかるように、 SET のシグネチャに現われる element と Elt.t
が等価であることを宣言することが必要ですが、 SET が定義される文脈にお
いて Elt が存在しないためこれは実現できません。 この問題を克服するため
Objective Camlでは with type 構文によりシグネチャに型の等価性情報を追
加出来ます。
 *)    

module AbstractSet2 = 
  (Set : functor(Elt: ORDERED_TYPE) -> (SET with type element = Elt.t));;

(**
### 普通に比較する。
 *)
module OrderedStringSet2 = AbstractSet2(OrderedString);;
OrderedStringSet2.member "foo" (OrderedStringSet2.add "foo" OrderedStringSet2.empty);;

(**
### 小文字で比較する。
 *)
module NoCaseStringSet2 = AbstractSet2(NoCaseString);;
NoCaseStringSet2.member "FOO" (NoCaseStringSet2.add "foo" NoCaseStringSet2.empty);;

(*
エラー
NoCaseStringSet.add "FOO" AbstractStringSet.empty;;
 *)    

(**
## Set functor を直接定義してもよい：
 *)
module AbstractSet3(Elt: ORDERED_TYPE) : (SET with type element = Elt.t) =
  struct
    type element = Elt.t
    type set = element list
    let empty = []
    let rec add x s =
      match s with
      | [] -> [x]
      | hd::tl ->
         match Elt.compare x hd with
         | Equal   -> s         (* x is already in s *)
         | Less    -> x :: s    (* x is smaller than all elements of s *)
         | Greater -> hd :: add x tl
    let rec member x s =
      match s with
      | [] -> false
      | hd::tl ->
         match Elt.compare x hd with
         | Equal   -> true     (* x belongs to s *)
         | Less    -> false    (* x is smaller than all elements of s *)
         | Greater -> member x tl
  end;;
 
(**
### 普通に比較する。
 *)
module OrderedStringSet3 = AbstractSet3(OrderedString);;
OrderedStringSet3.member "foo" (OrderedStringSet3.add "foo" OrderedStringSet3.empty);;

(**
### 小文字で比較する。
 *)
module NoCaseStringSet3 = AbstractSet3(NoCaseString);;
NoCaseStringSet3.member "FOO" (NoCaseStringSet3.add "foo" NoCaseStringSet3.empty);;

(* END *)

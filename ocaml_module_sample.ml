(* OCamlのモジュールのサンプル *)

(* one.ml *)
(* ファイルをひとつにまとめる場合
   module Name : sig ... end = struct ... end;;
   シグネチャが要らない場合は、module Name = struct ... end;; *)
module Pool :
sig
  exception Empty
  type 'a pool =
    | None
    | Some of 'a
  val empty : 'a pool
  val set : 'a -> 'a pool
  val get : 'a pool -> 'a
end = struct
  exception Empty
  type 'a pool =
    | None
    | Some of 'a
  let empty = None
  let set x = Some x
  let get = function 
    | None -> raise Empty
    | Some x -> x
end;;
let s1 = Pool.set 1;;
let s2 = Pool.set 2;;
open Pool;;                             (* openはできる *)
let a3 = get s2;;
Printf.printf "%d\n" a3;;
(* end *)


(* ファイルを分割するとき。
   mliファイルつまりシグネチャは必須。 *)
(* ocamlopt pool.mli pool.ml main.ml -o test *)
(* --------------------------------------------- *)
(* pool.mli *)
(* module Name : sig ... end;; *)
module Pool :
sig
  exception Empty
  type 'a pool =
    | None
    | Some of 'a
  val empty : 'a pool
  val set : 'a -> 'a pool
  val get : 'a pool -> 'a
end;;
(* end *)
(* --------------------------------------------- *)
(* pool.ml *)
(* module Name = struct ... end;; *)
module Pool = struct
  exception Empty
  type 'a pool =
    | None
    | Some of 'a
  let empty = None
  let set x = Some x
  let get = function 
    | None -> raise Empty
    | Some x -> x
end;;
(* end *)
(* --------------------------------------------- *)
(* main.ml *)
include Pool;;
let s1 = Pool.set 1;;
let s2 = Pool.set 2;;
open Pool;;                             (* openはできる *)
let a3 = get s2;;
Printf.printf "%d\n" a3;;
(* end *)

(**
https://ocaml.jp/
 *)
(**
# OCaml入門(1)
 *)
let f x = if x > 0 then x else -x;;

(**
## 再帰関数
 *)
let rec factorial x =
  if x <= 0 then 1 else x * (factorial (x - 1));;
factorial 10;;

(**
## 末尾再帰
 *)
let rec f n =
  let rec iter i =
    if i < n then begin
        print_int i;
        iter (i + 1)
      end
  in
  iter 0;;
f 10;;

(**
2種類のsum
 *)
let rec sum n =
    if n <= 0 then
      0
    else
      n + sum (n - 1)
;;
sum 10;;

let sum n =
  let rec iter i s =
    if i <= 0 then
      s
    else
      iter (i - 1) (s + i)
  in
  iter n 0
;;
sum 10;;


(**
# OCaml入門(2)
 *)
(**
## 部分適用
 *)
let f x y = x + y;;
f 1;;
let f1 = f 1 in f1 2;;                      (* - : int = 3 *)

(**
## リストとタプル
 *)
1 :: [2; 3; 4];;
[1; 2; 3] @ [4; 5; 6];;
1, 2, 3, 4;;

(**
## パターンマッチング
 *)

let t = [1; 2; 3; 4; 5];;
match t with
| [] -> 0
| x :: xs -> x;;

let t = (1, 2);;
match t with
  (x, y) -> x + y;;

let rec f x =
  match x with
  | [] -> 0
  | x :: xs -> x + (f xs);;
f [];;                                      (* 0 *)
f [1];;                                     (* 1 *)
f [1; 2];;                                  (* 3 *)
f [1; 2; 3];;                               (* 6 *)

let rec f x =
  match x with
  | [] -> []
  | x :: xs -> (x * x) :: (f xs);;
f [];;                                      (* [] *)
f [1];;                                     (* [1] *)
f [1; 2];;                                  (* [1;4] *)
f [1; 2; 3];;                               (* [1;4;9] *)

(**
## 高階関数
 *)
let rec foldr f v lst =
  match lst with
  | [] -> v
  | x :: xs -> f x (foldr f v xs);;
foldr (fun x y -> x + y) 0 [1; 2; 3; 4; 5; 6; 7; 8; 9; 10];;

(**
# OCaml入門(3)
 *)
(**
## レコード
 *)
type t = {
    name : string;
    zip  : string;
  };;
{ name = "Sakura";
    zip  = "100-1234";
  };;

type 'a t = {
    name : 'a;
  };;                                       (* 'a     t *)
{ name = "mocchi" };;                       (* string t *)
{ name = 23 };;                             (* int    t *)

type t = {
    name : string;
    address : string;
  };;
let s = { name = "Takeo"; address = "Japan"; };;
s.address;;                                 (* Japan *)
s.name;;                                    (* Takeo *)

type t = {
    mutable name : string;
    mutable address : string;
  };;
let s = { name = "Takeo"; address = "Japan"; };;
s.address;;                                 (* Japan *)
s.name;;                                    (* Takeo *)
s.name <- "Takeru";;
s.name;;                                    (* Takeru *)
s;;

(**
## バリアント
 *)
type t =
  | A
  | B
  | C;;

let f x =
    match x with
    | A -> 1
    | B -> 2
    | C -> 3;;
f A;;                                       (* 1 *)
f B;;                                       (* 2 *)
f C;;                                       (* 3 *)

type t =
  | Node of int * int
  | Nil;;

# let f x =
  match x with
  | Node (a, b) -> a + b
  | Nil -> 0;;
f (Node (1, 1));;                           (* 2 *)
f Nil;;                                     (* 0 *)

(**
## 例外
 *)
exception Not_found_x;;                     (* 例外を定義する。 *)
try
  raise Not_found_x                         (* ここで例外を起こす。 *)
with
| Not_found_x -> "Not_found"                (* キャッチする。 *)
| _ -> "Other Exception";;

(**
定義済みの例外
 *)
raise (Failure "test");;
raise Not_found;;
raise (Invalid_argument "test");;

(**
## 参照
 *)
let t = ref 0;;
!t;;                                        (* 0 *)
t := 5;;
!t;;                                        (* 5 *)

(* END *)


(**
第11回 クロージャによる超軽量並行プロセスの簡単実装法

https://xtech.nikkei.com/it/article/COLUMN/20070612/274231/
 *)

type 'a chan =
  | Senders of 'a list
  | Receivers of ('a -> unit) list
;;

let newc () = ref (Senders []);;

(* 通信チャンネルyに値xを送る *)
let send y x =
  match !y with
  | Senders ss ->                     (* 受信プロセスがない *)
     (* キューの後に値を付け加える *)
     y := Senders(ss @ [x])
  | Receivers [] ->                         (* 同上 *)
     y := Senders [x]
  | Receivers(f :: rs) ->      (* 受信プロセスがある *)
     (* 一つ(f)を取り出して残り(rs)は戻す *)
     y := Receivers rs;
     (* 取り出した受信プロセスに値を渡す *)
     f x
;;

(* 通信チャンネルyから値を受信し，関数fを適用する *)
let recv y f =
  match !y with
  | Receivers rs ->       (* 値がない *)
     (* ブロックした受信プロセスをキューに追加 *)
     y := Receivers(rs @ [f])
  | Senders [] ->                           (* 同上 *)
     y := Receivers [f]
  | Senders(x :: ss) ->    (* 値がある *)
     (* 一つだけ(x)を取り出して残り(ss)は戻す *)
     y := Senders ss;
     (* 取り出した値を受信プロセスに渡す *)
     f x
;;
      
(**
新しい通信チャンネルを作る：    let x = newc () in P;;

送信プロセス：  send y x

受信プロセス：  recv y (fun x -> P)

π計算のプロセス生成： P; Q
*)

(**
## 実際に並行プログラミングをしてみる
 *)
let x = newc () in
    send x 3;
    recv x (fun y ->
        Printf.printf "%d\n" y) ;;
(* 3 *)


(**
### 次の例
 *)
(* 新しい通信チャンネルcを作る *)
let c = newc () ;;

(* プロセスrepeat ()を再帰で定義 *)
let rec repeat () =
  (* cから整数iを受信 *)
  recv c (fun i ->
      (* iを画面に表示 *)
      Printf.printf "%d\n" i;
      (* またrepeat ()自身を生成 *)
      repeat ()) ;;

(* 最初のrepeat ()を生成 *)
repeat () ;;

(* cに1を送信 *)
send c 1 ;;

(* cに2を送信 *)
send c 2 ;;

(* 何度でも送信できる *)
send c 3 ;;

(**
### 関数サーバの例
 *)
(* サーバーがリクエストを受け付けるための通信チャンネルservcを作る *)
let servc = newc () ;;

(* サーバー・プロセスserv ()を再帰で定義 *)
let rec serv () =
  (* servcから整数iと，返信のための通信チャンネルrepcの組を受け取る *)
  recv servc (fun (i, repc) ->
      (* repcにiの2乗を返す *)
      send repc (i * i);
      (* serv自身を再び生成 *)
      serv ()) ;;

(* サーバー・プロセスを起動 *)
serv () ;;

(* 返信のためのチャンネルrを作る *)
let r = newc () ;;

(* サーバーに整数123とrを送る *)
send servc (123, r) ;;

(* rから答えの整数jを受け取り表示 *)
recv r (fun j -> Printf.printf "%d\n" j) ;;

(* サーバー・プロセスは何回でも呼び出すことができる *)
send servc (45, r) ;;

recv r (fun j ->  Printf.printf "%d\n" j) ;;

(**
### フィボナッチ数列の例
 *)
(* サーバーがリクエストを受け付けるための通信チャンネルfibcを作る *)
let fibc = newc () ;;

(* フィボナッチ・サーバーfib ()を定義 *)
let rec fib () =
  (* fibcから引数nと，返値を送るための通信チャンネルrepcの組を受け取る *)
  recv fibc (fun (n, repc) ->
      (* またfib ()自身を生成しておく *)
      fib ();
      if n <= 1 then
        (* nが1以下ならnを返信 *)
        send repc n
      else
        (* フィボナッチ・サーバー自身を利用して引数がn-1とn-2の場合の返値を計算 *)
        let repc1 = newc () in
        let repc2 = newc () in
        send fibc (n - 1, repc1);
        send fibc (n - 2, repc2);
        recv repc1 (fun rep1 ->
            recv repc2 (fun rep2 ->
                (* 二つの返値を足してrepcに返信 *)
                send repc (rep1 + rep2)))) ;;

(* フィボナッチ・サーバーを起動 *)
fib () ;;

(* 返値を受け取るための通信チャンネルrを作る *)
let r = newc () ;;

(* 引数とrを送信 *)
send fibc (10, r) ;;

(* rから返値mを受け取って表示 *)
recv r (fun m -> Printf.printf "fib(10) = %d\n" m) ;;
(*
fib(10) = 55
- : unit = ()
 *)

(**
### プロセス・スケジューリングの改良
 *)
let fork f g =
  (* 乱数が0だったらPを先に，1だったらQを先に実行 *)
  if Random.int 2 = 0 then
    (f (); g ())
  else
    (g (); f ())
;;

(*
fork (fun () -> P) (fun () -> Q);;
 *)

(* END *)

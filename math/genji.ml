(**
源氏香の全解、または、集合を分割するプログラム
 *)

(**
集合の分割が妥当かのチェック
 *)
let gchk a =
  let len = Array.length a in
  let rec loop i m =
    if len <= i then true
    else if a.(i) <= m + 1 then
      loop (i + 1) (max a.(i) m)
    else false
  in loop 0 0;;
      
gchk [|0; 0; 0; 0; 0 |];;                   (* すべて同じ。 *)
gchk [|0; 1; 2; 3; 4 |];;                   (* すべて異なる。 *)
gchk [|0; 0; 1; 1; 2 |];;                   (* 3分割する。 *)
gchk [|0; 0; 1; 1; 3 |];;                   (* 不正な例 *)

(**
[|0; 0; 0; 0; 0|] から [|0; 1; 2; 3; 4|] までを生成する。
 *)
let next a' =
  let a = Array.copy a' in                  (* これは重要！！ *)
  let len = Array.length a in
  let rec loop i =                          (* i = (len - 1)..0 *)
    if i = 0 then
      None
    else if a.(i) < i then
      begin
        a.(i) <- a.(i) + 1;
        Some a
      end
    else
      begin
        a.(i) <- 0;
        loop (i - 1)
      end
  in loop (len - 1);;

let a = [|0; 0; 0; 0; 0|];;
next a;;                                    (* [|0; 0; 0; 0; 1|] *)

let a = [|0; 0; 2; 3; 4|];;
next a;;                                    (* [|0; 1; 0; 0; 0|] *)

let a = [|0; 1; 2; 3; 3|];;
next a;;                                    (* [|0; 1; 2; 3; 4|] *)

let a = [|0; 1; 2; 3; 4|];;
next a;;                                    (* None *)

let gen n =
  let a = Array.make n 0 in
  let rec loop a =
    match next a with
    | None -> []
    | Some b -> b :: loop b
  in 
  a :: loop a;;

gen 5;;
List.length (gen 5);;         (* 120 *)
List.nth (gen 5) 119;;        (* [|0; 1; 2; 3; 4|] *)


let rec filter f l =
  match l with
  | [] -> []
  | a :: l' ->
     if f a then a :: filter f l'
     else filter f l';;

filter gchk (gen 5);;
List.length (filter gchk (gen 5));;         (* 52 *)
List.nth (filter gchk (gen 5)) 51;;         (* [|0; 1; 2; 3; 4|] *)

(*
thm([1,2,3,4,5], 帚木).
thm([1,2,3,4,4], 空蝉).
 *)

(* END *)

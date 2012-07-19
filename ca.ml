(*
  Cellular Automata
  Wolfram NKS p.865.
*)

let centerlist n =
  let l = table 0 (n / 2) in
  List.concat [l; [1]; l];;

let rec table x n =
  match n with
    | 0 -> []
    | _ -> x :: table x (n - 1)
;;

(*
  結果は、y と同じ長さとする。
   OCamlの List.map2 の挙動とは異なる。
*)
let rec map3 f x y z =
  match x, y, z with
    | x' :: xs, y' :: ys, z' :: zs -> f x' y' z' :: map3 f xs ys zs
    | [],       y' :: ys, z' :: zs -> f 0  y' z' :: map3 f [] ys zs
    | x' :: xs, y' :: ys, []       -> f x' y' 0  :: map3 f xs ys []
    | [],       y' :: ys, []       -> f 0  y' 0  :: map3 f [] ys []
    | _,       [],        _        -> []
;;

(*
  時刻tでの内部状態              111    110    101    100    011    010    001    000
  時刻t+1での中央のセルの内部状態  0      1      0      1      1      0      1      0
*)
let rule90 x y z =
  match x, y, z with
    | 1, 1, 1 -> 0
    | 1, 1, 0 -> 1
    | 1, 0, 1 -> 0
    | 1, 0, 0 -> 1
    | 0, 1, 1 -> 1
    | 0, 1, 0 -> 0
    | 0, 0, 1 -> 1
    | 0, 0, 0 -> 0
;;

let castep rule l =
  map3 rule (0 :: l) l (List.tl l)
;;

let rec nestlist f l times =
  match times with
    | 0 -> [l]
    | _ -> l :: nestlist f (f l) (times - 1)
;;

let caevolvelist rule init times =
  nestlist rule init times
;;

let ca m n =
  caevolvelist (fun l -> castep rule90 l) (centerlist m) n
;;

(* END *)

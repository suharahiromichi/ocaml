(**
第6回　OCamlの「モジュール・システム」

https://xtech.nikkei.com/it/article/COLUMN/20070116/258746/
 *)

module type Complex =
  sig
    type t                        (* 複素数の型 *)
    val make : float * float -> t (* 実部と虚部から複素数を作って返す *)
    val add : t -> t -> t         (* 複素数の足し算 *)
    val mul : t -> t -> t         (* 複素数の掛け算 *)
    val abs : t -> float          (* 複素数の絶対値 *)
  end;;

(* CartesianはComplexインタフェースを実装する。 *)
module Cartesian : Complex =
  struct
    type t = float * float
    let make (x, y) = (x, y)
    let add (x1, y1) (x2, y2) = (x1 +. x2, y1 +. y2)
    let mul (x1, y1) (x2, y2) =
      (x1 *. x2 -. y1 *. y2,
       x1 *. y2 +. x2 *. y1)
    let abs (x, y) = sqrt (x *. x +. y *. y)
  end;;
(* module Cartesian : Complex *)

(* PolarはComplexインタフェースを実装する。 *)
module Polar : Complex =
  struct
    type t = float * float
    let c2p (x, y) = (sqrt (x *. x +. y *. y), atan2 y x)
    let p2c (r, t) = (r *. cos t, r *. sin t)
    let make (x, y) = c2p (x, y)
    let add p1 p2 =
      let (x1, y1) = p2c p1 in
      let (x2, y2) = p2c p2 in
      c2p (x1 +. x2, y1 +. y2)
    let mul (r1, t1) (r2, t2) = (r1 *. r2, t1 +. t2)
    let abs (r, t) = r
  end;;
(* module Polar : Complex *)

(**
Cartesian のサンプル
 *)
let c1 = Cartesian.make (1.0, 1.0);;       (* c1は複素数1+iを表す。 *)
(* val c1 : Cartesian.t = <abstr> *)
let c2 = Cartesian.mul c1 c1;;    (* c1の2乗をc2とする。 *)
(* val c2 : Cartesian.t = <abstr> *)
Cartesian.abs c2;;                         (* c2の絶対値は2になる。 *)
(* - : float = 2. *)

(**
Polar のサンプル
 *)
let p1 = Polar.make (1.0, 1.0);;      (* p1は複素数1+iを表す。 *)
(* val p1 : Polar.t = <abstr> *)
let p2 = Polar.mul p1 p1;;            (* p1の2乗をp2とする。 *)
(* val p2 : Polar.t = <abstr> *)
Polar.abs p2;;    (* p2の絶対値は2になる。 *)
(* - : float = 2.00000000000000044 （誤差が乗る） *)

(**
ファンクタ
 *)
#load "graphics.cma";;
module Mandelbrot (C : Complex) =
  struct
    let f () =              (* 実際にマンデルブロ集合を描画する関数 *)
      let rec converge n c z =
	if n <= 0 then true else
	  if C.abs z >= 2.0 then false else
	    converge (n - 1) c (C.add c (C.mul z z)) in
      for x = 0 to 299 do
	for y = 0 to 299 do
	  let cx = float x /. 100.0 -. 1.5 in
	  let cy = float y /. 100.0 -. 1.5 in
	  let conv = converge 100 (C.make (cx, cy)) (C.make (0.0, 0.0)) in
	  if conv then Graphics.plot x y;
	done
      done
  end;;
(* module Mandelbrot : functor (C : Complex) -> sig val f : unit -> unit end *)

(* デカルト座標で実行する。 *)
module CartesianMandelbrot = Mandelbrot(Cartesian);;
(* module CartesianMandelbrot : sig val f : unit -> unit end *)

(* 極座標で実行する。 *)
module PolarMandelbrot = Mandelbrot(Polar);;
(* module PolarMandelbrot : sig val f : unit -> unit end *)

(**
実行例
 *)
Graphics.open_graph "";;
CartesianMandelbrot.f ();;              (* デカルト座標で実行する。 *)
Graphics.clear_graph ();;
PolarMandelbrot.f ();;                      (* 極座標で実行する。 *)

(* END *)

(**
第4回 関数型言語とオブジェクト指向，およびOCamlの"O"について

https://xtech.nikkei.com/it/article/COLUMN/20061107/252787/
 *)

(**
## OCamlのオブジェクト
 *)
let hello =
  object (self)
    method get = "Hello\n"
    method print = print_string self#get
  end;;
(* オブジェクトのメソッドを呼び出す。 *)
hello#print;;                               (* Hello *)

let int123 =
  object (self)
    method get = 123
    method print = print_int self#get
  end;;
int123#print;;                              (* 123 *)

(**
## 型推論とオブジェクトの多態性
 *)
(* オブジェクトの多態性 オブジェクトxはどちらでもよい。 *)
let invoke_print x = x#print ;;
invoke_print hello;;                        (* Hello *)
invoke_print int123;;                       (* 123 *)

(**
## 構造的部分型
 *)
type printable = < print : unit >;;
(hello :> printable) ;;         (* printable にアップキャストする。 *)
(* - : printable = <obj> *)
(int123 :> printable);;
(* - : printable = <obj>  *)

(**
## クラスの定義と実装の継承
 *)
class strobj x =
    object (self)
      method get = x
      method print = print_string self#get
    end ;;

(* class に対して new してインスタンスを作る。 *)
let hello = new strobj "Hello\n" ;;

hello#print ;;                              (* Hello *)

(* 以下でも同じことができる。
let new_strobj x =
    object (self)
      method get = x
      method print = print_string self#get
    end ;;
let hello = new_strobj "Hello\n" ;;
hello#print ;;
 *)

(**
class を使えば継承ができる。
 *)
class strobj2 x =
object (self)
  inherit strobj x as super
  method length = String.length self#get
end ;;

let hello2 = new strobj2 "Hello\n" ;;

hello2#length ;;                            (* 6 *)

(**
## 継承≠部分型関係

ここからはやや高度な話題になる。
 *)
class point (pos : int) =
object (self : 'self_type)
  method get_pos = pos
  method is_equal_to (p : 'self_type) =
    (p#get_pos = self#get_pos)
end ;;

class colored_point (pos : int) (color : int) =
object (self : 'self_type)
  inherit point pos as super
  method get_color = color
  method is_equal_to (p : 'self_type) =
    (p#get_pos = self#get_pos) &&
      (p#get_color = self#get_color)
end ;;

let cp = new colored_point 123 255 ;;

(* (cp :> point) ;; *)

let p = new point 456 ;;
let cp = new colored_point 789 255 ;;
(cp :> point)#is_equal_to p ;; (* 実際には型エラーになり実行できない *)

(* END *)

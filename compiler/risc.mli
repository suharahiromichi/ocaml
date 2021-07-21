open Nativeint;;

module Isa : sig
  type ric;;                                (* Register instructions *)
  val to_ric : int -> ric;;
  val from_ric : ric -> int;;
  type mic;;                                (* Memory instructions *)
  val to_mic : int -> mic;;
  val from_mic : mic -> int;;
  type bic;;                                (* Branch instructions *)
  val to_bic : int -> bic;;
  val from_bic : bic -> int;;
  type register;;
  type instr;;
end;;

module Assembler : sig
  val test : unit -> unit;;
end;;

module Emulator : sig
  val ir : nativeint ref;;                      (* instruction register *)
  val pc : nativeint ref;;                      (* program counter *)
  val n : bool ref;;
  val z : bool ref;;
  val r : nativeint array;;                     (* registers *)
  val ra : nativeint ref;;
  val rb : nativeint ref;;
  val rc : nativeint ref;;
  val rh : nativeint ref;;
  val mem : nativeint array;;                   (* memory *)
  
  val inc : nativeint ref -> unit;;
  val exec : unit -> unit;;

  val to_hl : nativeint -> int * int;;

  val print_reg : unit -> unit;;
  val print_mem : unit -> unit;;

  val test : unit -> unit;;
end;;

(* END *)

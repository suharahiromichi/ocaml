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

module Dump : sig
  val to_hl : nativeint -> int * int;;
  val print_mem : nativeint array -> unit;;
end;;

module Program : sig
  type expr;;
  type stat;;
  val src : stat;;
end;;

module Compiler : sig
  val comp : Program.stat -> Isa.instr list;;
end;;

module Assembler : sig
  val asm1 : Isa.instr -> nativeint;;
  val asm : Isa.instr list -> nativeint list;;
  val print1 : Isa.instr -> unit;;
  val print : Isa.instr list -> unit;;
end;;

module Emulator : sig
  val ir : nativeint ref;;                  (* instruction register *)
  val pc : nativeint ref;;                  (* program counter *)
  val n : bool ref;;
  val z : bool ref;;
  val r : nativeint array;;                 (* registers *)
  val ra : nativeint ref;;
  val rb : nativeint ref;;
  val rc : nativeint ref;;
  val rh : nativeint ref;;
  val mem : nativeint array;;               (* memory *)
  val load : nativeint list -> unit;;
  val set : int -> nativeint -> unit;;
  
  val dump : unit -> unit;;
  val reset : unit -> unit;;
  val inc : nativeint ref -> unit;;
  val exec : unit -> unit;;
  
  val print_reg : unit -> unit;;
end;;

(* END *)

open Nativeint;;

module Isa : sig
  type rmb;;
  type opcode;;
  val from_opc : opcode -> int * rmb * bool;;
  val to_opc : int * rmb * bool -> opcode;;
  type register;;
  type instr;;
  type statement;;
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

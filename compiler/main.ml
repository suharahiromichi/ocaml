open Nativeint;;

(*
Risc.Assembler.test ();;
Risc.Emulator.test ();;
 *)

open Risc;;

let test () =
  let obj = Assembler.asm Assembler.src in
  begin
    Dump.print_mem (Array.of_list obj);
    Emulator.load (obj);
    Emulator.set 16 (of_int 2);           (* x *)
    Emulator.set 17 (of_int 3);           (* y *)
    Emulator.set 18 (of_int 4);           (* z *)
    Emulator.set 19 (of_int 5);           (* w *)
    
    Emulator.reset ();
    Emulator.print_reg ();
    Emulator.dump ();
    
    for i = 0 to 9 do
      Emulator.exec ();
      Emulator.print_reg ();
      Emulator.dump ()
    done
  end;;

test ();

(* END *)

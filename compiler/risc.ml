open Nativeint;;

module Isa = struct
  type opc =
    | Imov
    | Ilsl
    | Iasr
    | Iror
    | Iand
    | Iann
    | Iior
    | Ixor
    | Iadd
    | Isub
    | Imul
    | Idiv
    | Inop;;
  
  let to_opc = function
    |  0 -> Imov
    |  1 -> Ilsl
    |  2 -> Iasr
    |  3 -> Iror
    |  4 -> Iand
    |  5 -> Iann
    |  6 -> Iior
    |  7 -> Ixor
    |  8 -> Iadd
    |  9 -> Isub
    | 10 -> Imul
    | 11 -> Idiv
    |  _ -> Inop;;
end;;

module Emulator = struct
  let zero = 0n;;
  
  let ir = ref zero;;
  let pc = ref zero;;
  let n = ref false;;
  let z = ref false;;
  let r = [| zero; zero; zero; zero; zero; zero; zero; zero |];;
  let ra = ref zero;;
  let rb = ref zero;;
  let rc = ref zero;;
  let rh = ref zero;;
  let memsize = 32;;
  let mem = Array.make memsize zero;;
  
  mem.(0) <- add (of_int 0x02345678)  (shift_left (of_int 0x5) 28);;
  mem.(1) <- of_int 0x10;;
  mem.(2) <- of_int 0x20;;
  mem.(3) <- of_int 0x30;;

  let inc x = x := succ !x;;
  let fetch y x = y := mem.(to_int x);;

  let exec () =
    fetch ir !pc;                           (* IR <- M[PC] *)
    inc pc;                                 (* PC <- PC + 1 *)
    let a  = to_int (shift_right_logical !ir 24) land 0xF in
    let b  = to_int (shift_right_logical !ir 20) land 0xF in
    let c  = to_int (logand !ir (of_int 0xF)) in
    let op = to_int (shift_right_logical !ir 16) land 0xF in
    let im = to_int (logand !ir (of_int 0xFFFF)) in
    let p  = to_int (shift_right_logical !ir 31) in
    let q  = to_int (shift_right_logical !ir 30) land 0x1 in
    let v  = to_int (shift_right_logical !ir 28) land 0x1 in
    
    Printf.printf "a=%x\n" a;
    Printf.printf "b=%x\n" b;
    Printf.printf "c=%x\n" c;
    Printf.printf "op=%x\n" op;
    Printf.printf "im=%x\n" im;
    Printf.printf "p=%x\n" p;
    Printf.printf "q=%x\n" q;
    Printf.printf "v=%x\n" v;
    
    if (p = 0) then
      begin
        fetch rb (of_int b);
        
        if (q = 0) then
          fetch rc (of_int c)
        else if (v = 0) then
          rc := of_int im
        else
          rc := add (of_int im) (shift_left (of_int 0xFFFF) 16);
        
        match (Isa.to_opc op) with
        | Isa.Imov -> ()
        | Isa.Ilsl -> ()
        | Isa.Iasr -> ()
        | Isa.Iror -> ()
        | Isa.Iand -> ()
        | Isa.Iann -> ()
        | Isa.Iior -> ()
        | Isa.Ixor -> ()
        | Isa.Iadd -> ()
        | Isa.Isub -> ()
        | Isa.Imul -> ()
        | Isa.Idiv -> ()
        | Isa.Inop -> ()
      end
    else
      if (q = 0) then
        ()
      else
        ()
  ;;
  
  let to_hl i =
    let h = to_int (shift_right_logical i 16) in
    let l = to_int (logand i (of_int 0xFFFF)) in
    (h, l);;

  let to_hex i = 
    match (to_hl i) with
    | (a, b) -> Printf.sprintf "%04X%04X" a b;;
  
  let print_reg () =
    Printf.printf "IR:%s PC:%s N:%B Z:%B\n"
      (to_hex !ir) (to_hex !pc) !n !z;
    Printf.printf "R0:%s R1:%s R2:%s R3:%s\n"
      (to_hex r.(0)) (to_hex r.(1)) (to_hex r.(2)) (to_hex r.(3));
    Printf.printf "R4:%s R5:%s R6:%s R7:%s\n"
      (to_hex r.(4)) (to_hex r.(5)) (to_hex r.(6)) (to_hex r.(7));
    Printf.printf "RA:%s RB:%s RC:%s RH:%s\n"
      (to_hex !ra) (to_hex !rb) (to_hex !rc) (to_hex !rh);;
  
  let print_mem () =
    for i = 0 to memsize - 1 do
      if i mod 8 == 0 then
        Printf.printf "%04X " i;
      Printf.printf "%s " (to_hex mem.(i));
      if (i + 1) mod 8 == 0 then
        Printf.printf "\n";
    done
    
  let test () =
    Printf.printf "%s\n" (to_hex mem.(0));;
  
end;;

(* END *)


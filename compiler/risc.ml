open Nativeint;;

module Isa = struct
  type ric =                               (* Register instructions *)
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
    | Icmp
    | Imul
    | Idiv;;
  
  let to_ric = function
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
    | 11 -> Idiv;;
  
  let from_ric = function
    | Imov ->  0
    | Ilsl ->  1
    | Iasr ->  2
    | Iror ->  3
    | Iand ->  4
    | Iann ->  5
    | Iior ->  6
    | Ixor ->  7
    | Iadd ->  8
    | Isub ->  9
    | Icmp ->  9                            (* SUB *)
    | Imul -> 10
    | Idiv -> 11;;
  
  type mic =
    | Ildw
    | Istw;;
  
  let to_mic = function
    | 0 -> Ildw
    | 1 -> Istw;;

  let from_mic = function
    | Ildw -> 0
    | Istw -> 1;;

  type bic =
    | Ibmi
    | Ibeq
    (*  | Ibcs  *)
    (*  | Ibvs  *)
    (*  | Ibls  *)
    | Iblt
    | Ible
    | Ib  
    | Ibpl
    | Ibne
    (*  | Ibcc  *)
    (*  | Ibvc  *)
    (*  | Ibhi  *)
    | Ibge
    | Ibgt;;
  
  let to_bic = function
    |  0 -> Ibmi
    |  1 -> Ibeq
    (*  | Ibcs  *)
    (*  | Ibvs  *)
    (*  | Ibls  *)
    |  5 -> Iblt
    |  6 -> Ible
    |  7 -> Ib  
    |  8 -> Ibpl
    |  9 -> Ibne
    (*  | Ibcc  *)
    (*  | Ibvc  *)
    (*  | Ibhi  *)
    | 13 -> Ibge
    | 14 -> Ibgt;;
  
  let from_bic = function
    | Ibmi ->  0
    | Ibeq ->  1
    (*  | Ibcs  *)
    (*  | Ibvs  *)
    (*  | Ibls  *)
    | Iblt ->  5
    | Ible ->  6
    | Ib   ->  7
    | Ibpl ->  8
    | Ibne ->  9
    (*  | Ibcc  *)
    (*  | Ibvc  *)
    (*  | Ibhi  *)
    | Ibge -> 13
    | Ibgt -> 14;;
  
  type register =
    | R of int
    | D;;                                   (* dummy *)
  
  type instr =
    | F0' of ric * register * register
    | F0  of ric * register * register * register
    | F1' of ric * register * int
    | F1  of ric * register * register * int
    | F2  of mic * register * register * int
    | F3  of bic * register
    | F3' of bic * int;;
  
  type statement = string * instr;;
end;;

module Asm = struct
  let test () =
      let imov = "label", Isa.F0' (Isa.Imov, Isa.R 0, Isa.R 1) in
      let imov = "label", Isa.F1' (Isa.Imov, Isa.R 0, 1234) in
      let iadd = "label", Isa.F0  (Isa.Iadd, Isa.R 0, Isa.R 1, Isa.R 2) in
      let iadd = "label", Isa.F1  (Isa.Iadd, Isa.R 0, Isa.R 1, 1234) in
      let ildw = "label", Isa.F2  (Isa.Ildw, Isa.R 0, Isa.R 1, 1234) in
      let istw = "label", Isa.F2  (Isa.Istw, Isa.R 0, Isa.R 1, 1234) in
      let ib   = "label", Isa.F3  (Isa.Ib,   Isa.R 0) in
      let ib   = "label", Isa.F3' (Isa.Ib,   1234) in
      ();;
end;;

module Emulator = struct
  let zero = 0n;;
  let lsb  = shift_left (of_int 1) 31;;
  
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
  
  let exec () =
    begin
      ir := mem.(to_int !pc);               (* IR <- M[PC] *)
      inc pc;                               (* PC <- PC + 1 *)
      let a  = to_int (shift_right_logical !ir 24) land 0xF in
      let b  = to_int (shift_right_logical !ir 20) land 0xF in
      let c  = to_int (logand !ir (of_int 0xF)) in
      let op = to_int (shift_right_logical !ir 16) land 0xF in
      let im = to_int (logand !ir (of_int 0xFFFF)) in (* 16bit *)
      let p  = to_int (shift_right_logical !ir 31) land 0x1 in
      let q  = to_int (shift_right_logical !ir 30) land 0x1 in
      let u  = to_int (shift_right_logical !ir 29) land 0x1 in
      let v  = to_int (shift_right_logical !ir 28) land 0x1 in
      let off = to_int (logand !ir (of_int 0xFFFFF)) in (* 20bit *)
      
      Printf.printf "a=%x\n" a;
      Printf.printf "b=%x\n" b;
      Printf.printf "c=%x\n" c;
      Printf.printf "op=%x\n" op;
      Printf.printf "im=%x\n" im;
      Printf.printf "p=%x\n" p;
      Printf.printf "q=%x\n" q;
      Printf.printf "v=%x\n" v;
      
      if (p = 0) then                         (* 0quv *)
        begin
          rb := r.(b);
          if (q = 0) then
            rc := r.(c)
          else if (v = 0) then
            rc := of_int im
          else
            rc := add (of_int im) (shift_left (of_int 0xFFFF) 16);
          
          let tmp = match (Isa.to_ric op) with
            | Isa.Imov ->
               if (u = 0) then
                 !rc
               else
                 !rh
            | Isa.Ilsl -> shift_left !rb (to_int !rc)
            | Isa.Iasr -> shift_right !rb (to_int !rc)
            | Isa.Iror -> zero              (* 未実装 *)
            | Isa.Iand -> logand !rb !rc
            | Isa.Iann -> logand !rb (lognot !rc)
            | Isa.Iior -> logor !rb !rc
            | Isa.Ixor -> logxor !rb !rc
            | Isa.Iadd -> add !rb !rc
            | Isa.Isub -> sub !rb !rc
            | Isa.Imul -> mul !rb !rc
            | Isa.Idiv -> div !rb !rc in
          begin
            r.(a) <- tmp;
            z := tmp = zero;
            n := (logand tmp lsb) = zero
          end
        end
      else
        if (q = 0) then                       (* 10uv *)
          let adr = (to_int r.(b) + off) / 4 in
          if (u = 0) then                     (* LOAD *)
            begin
              let tmp = mem.(adr) in
              r.(a) <- tmp;
              z := tmp = zero;
              n := (logand tmp lsb) = zero
            end
          else                                (* STORE *)
            mem.(adr) <- r.(a)
        else                                  (* 11uv *)
          ()
    end
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


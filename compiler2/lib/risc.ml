open Nativeint;;

module Isa = struct
  exception Not_found;;
  
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
    | 11 -> Idiv
    | _  -> raise Not_found;;
  
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
    | 1 -> Istw
    | _  -> raise Not_found;;
  
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
    | 14 -> Ibgt
    | _  -> raise Not_found;;          
  
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
    | F0 of ric * register * register * register
    | F1 of ric * register * register * int
    | F2 of mic * register * register * int
    | F3 of bic * int
    | FL of bic * int;;              (* call / BL : Branch and Link *)
end;;

module Dump = struct
  let to_hl i =
    let h = to_int (shift_right_logical i 16) in
    let l = to_int (logand i (of_int 0xFFFF)) in
    (h, l);;

  let to_hex i = 
    match (to_hl i) with
    | (a, b) -> Printf.sprintf "%04X%04X" a b;;
  
  let print_mem mem =
    begin
      for i = 0 to Array.length mem - 1 do
        if i mod 8 == 0 then
          Printf.printf "%04X " i;
        Printf.printf "%s " (to_hex mem.(i));
        if (i + 1) mod 8 == 0 then
          Printf.printf "\n";
      done;
      Printf.printf "\n"
    end
end;;

open Isa;;

module Program = struct
  type expr =
    | Cst of int
    | Var of int
    | Add of expr * expr
    (* | Min *)
    | Mul of expr * expr;;

  type stat =
    | Stat of int * expr;;

  let src =
    Stat (4,                                (* u := *)
          (Add                              (* + *)
             ((Mul (Var 0, Var 1)),         (* x * y *)
              (Mul (Var 2, Var 3)))));;     (* z * w *)
end;;

open Program;;

module Compiler = struct
  let sb = R 13;;
  
  (* テスト *)
  let src = [
      F1 (Imov, sb, D, 16);               (*  *)
      F2 (Ildw, R 0, sb,  0);             (* x *)
      F2 (Ildw, R 1, sb,  1);             (* y *)
      F0 (Imul, R 0, R 0, R 1);           (* x := x * y *)
      F2 (Ildw, R 1, sb,  2);             (* z *)
      F2 (Ildw, R 2, sb,  3);             (* w *)
      F0 (Imul, R 1, R 1, R 2);           (* z := z * w *)
      F0 (Iadd, R 0, R 0, R 1);           (* x := x + z *)
      F2 (Istw, R 0, sb,  4)              (* u := x *)
    ];;
  
  let rh = ref 0;;
  
  let inc () =
    let r = !rh in
    begin
      rh := r + 1;
      r
    end;;
  
  let dec () =
    let r = !rh in
    begin
      rh := r - 1;
      r
    end;;
  
  let rec comp_expr = function
    | Cst n -> [F1 (Imov, R (inc ()), D,  n)] (* 仮 *)
    | Var v -> [F2 (Ildw, R (inc ()), sb, v)]
    | Add (e1, e2) ->
       let l1 = comp_expr e1 in
       let l2 = comp_expr e2 in
       let l3 = [F0 (Iadd, R (dec () - 2), R (!rh - 2), R (!rh - 1))] in
       l1 @ l2 @ l3
    | Mul (e1, e2) ->
       let l1 = comp_expr e1 in
       let l2 = comp_expr e2 in
       let l3 = [F0 (Imul, R (dec () - 2), R (!rh - 2), R (!rh - 1))] in
       l1 @ l2 @ l3;;
  
  let comp = function
    | Stat (v, expr) ->
       let l1 = [F1 (Imov, sb, D, 16)] in
       let l2 = comp_expr expr in
       let l3 = [F2 (Istw, R (!rh - 1), sb, v)] in
       l1 @ l2 @ l3;;
(*
  let comp = function
    | _ -> src;;
 *)
end;;

module Assembler = struct
  let build p a b op c =
    add (shift_left (of_int p) 28)
      (add (shift_left (of_int a) 24)
         (add (shift_left (of_int b) 20)
            (add (shift_left (of_int op) 16)
               (of_int c))));;
    
  let asm1 = function
    | F0 (ric, R a, D,   R c) -> build 0b0000 a 0 (from_ric ric) c
    | F0 (ric, R a, R b, R c) -> build 0b0000 a b (from_ric ric) c
    | F1 (ric, R a, D,    im) -> build 0b0100 a 0 (from_ric ric) im
    | F1 (ric, R a, R b,  im) -> build 0b0100 a b (from_ric ric) im
    | F2 (Ildw, R a, R b, off) -> build 0b1000 a b 0 off
    | F2 (Istw, R a, R b, off) -> build 0b1010 a b 0 off
    | F3 (bic, off) ->            build 0b1110 (from_bic bic) 0 0 off
    | FL (bic, off) ->            build 0b1111 (from_bic bic) 0 0 off
    | _ -> zero;;
  
  let asm src = List.map asm1 src;;

  let print1 = function  
    | F0 (ric, R a, D,   R c) ->
       Printf.printf "F0 %d R%d D   R%d\n" (from_ric ric) a c;
    | F0 (ric, R a, R b, R c) ->
       Printf.printf "F0 %d R%d R%d R%d\n" (from_ric ric) a b c;
    | F1 (ric, R a, D,    im) ->
       Printf.printf "F1 %d R%d D   im=%d\n" (from_ric ric) a im;
    | F1 (ric, R a, R b,  im) ->
       Printf.printf "F1 %d R%d R%d im=%d\n" (from_ric ric) a b im;
    | F2 (Ildw, R a, R b, off) ->
       Printf.printf "F2 LDW R%d R%d off=%d\n" a b off
    | F2 (Istw, R a, R b, off) ->
       Printf.printf "F2 STW R%d R%d off=%d\n" a b off
    | F3 (bic, off) ->
       Printf.printf "F2 %d off=%d\n" (from_bic bic) off
    | FL (bic, off) ->
       Printf.printf "F2 %d off=%d\n" (from_bic bic) off
    | _ ->
       Printf.printf "erro\n";;
  
  let print src =
    let _ = List.map print1 src in
    ()
end;;

module Emulator = struct
  exception Not_found;;
  exception Out_of_memory;;
  
  let lsb  = shift_left (of_int 1) 31;;
  
  let ir = ref zero;;
  let pc = ref zero;;
  let n = ref false;;
  let z = ref false;;
  let r = [| zero; zero; zero; zero; zero; zero; zero; zero;
             zero; zero; zero; zero; zero; zero; zero; zero |];;
  let ra = ref zero;;
  let rb = ref zero;;
  let rc = ref zero;;
  let rh = ref zero;;
  let memsize = 32;;
  let mem = Array.make memsize zero;;
  
  let load obj =
    begin
      Array.fill mem 0 memsize zero;
      let obj = Array.of_list obj in
      let len = Array.length obj in
      if (len > memsize) then
        raise Out_of_memory
      else
        Array.blit obj 0 mem 0 len
    end;;

  let set adr dat =
    if (adr >= memsize) then 
      raise Out_of_memory
    else
      mem.(adr) <- dat;;
  
  let dump () =
    Dump.print_mem mem;;
  
  let reset () =
    begin
      ir := zero;
      pc := zero;
      n := false;
      z := false;
      Array.fill r 0 16 zero
    end;;
  
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
      Printf.printf "off=%x\n" off;      
      
      (* pquv = 0quv *)
      if (p = 0) then                      (* Register instructions *)
        begin
          rb := r.(b);
          if (q = 0) then
            rc := r.(c)
          else                              (* if (v = 0) *)
            rc := of_int im;
          
          let tmp = match (Isa.to_ric op) with
            | Isa.Imov -> !rc               (* if (u = 0) *)
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
            | Isa.Idiv -> div !rb !rc
            | _ -> raise Not_found in
          begin
            r.(a) <- tmp;

            z := tmp = zero;
            n := (logand tmp lsb) = zero
          end
        end
      else
        (* pquv = 10uv *)
        if (q = 0) then                     (* Memory instructions *)
          let adr = to_int r.(b) + off in
          if (u = 0) then                   (* LDW *)
            begin
              let tmp = mem.(adr) in
              r.(a) <- tmp;
              z := tmp = zero;
              n := (logand tmp lsb) = zero
            end
          else                              (* STW *)
            mem.(adr) <- r.(a)
      
        (* pquv = 11uv *)
        else                                (* Branch instructions *)
          begin
            if (v = 0) then
              ()                            (* Not Link *)
            else
              r.(15) <- !pc;                (* Link *)
            
            let tmp = match (Isa.to_bic op) with
              | Isa.Ibmi -> !n
              | Isa.Ibeq -> !z
              | Isa.Iblt -> !n
              | Isa.Ible -> !n || !z
              | Isa.Ib   -> true
              | Isa.Ibpl -> not !n
              | Isa.Ibne -> not !z
              | Isa.Ibge -> not !n
              | Isa.Ibgt -> not (!n || !z) in
            if tmp then
              pc := of_int off
            else
              ()
          end
    end;;
  
  let print_reg () =
    Printf.printf "IR:%s PC:%s N:%B Z:%B\n"
      (Dump.to_hex !ir) (Dump.to_hex !pc) !n !z;
    Printf.printf "R0:%s R1:%s R2:%s R3:%s\n"
      (Dump.to_hex r.(0)) (Dump.to_hex r.(1)) (Dump.to_hex r.(2)) (Dump.to_hex r.(3));
    Printf.printf "R4:%s R5:%s R6:%s R7:%s\n"
      (Dump.to_hex r.(4)) (Dump.to_hex r.(5)) (Dump.to_hex r.(6)) (Dump.to_hex r.(7));
    Printf.printf "R8:%s R9:%s R10:%s R11:%s\n"
      (Dump.to_hex r.(8)) (Dump.to_hex r.(9)) (Dump.to_hex r.(10)) (Dump.to_hex r.(11));
    Printf.printf "R12:%s R13:%s R14:%s R15:%s\n"
      (Dump.to_hex r.(12)) (Dump.to_hex r.(13)) (Dump.to_hex r.(14)) (Dump.to_hex r.(15));
    Printf.printf "RA:%s RB:%s RC:%s RH:%s\n"
      (Dump.to_hex !ra) (Dump.to_hex !rb) (Dump.to_hex !rc) (Dump.to_hex !rh);;
end;;

(* END *)

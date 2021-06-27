open Syntax

let table = Hashtbl.create 10

let rec interpret s = match s with
  | Const(x, i) ->
     Hashtbl.replace table x i
  | Add(x, y, z) ->
     Hashtbl.replace table x
       (Hashtbl.find table y +
          Hashtbl.find table z)
  | While(x, y, s2) ->
     if Hashtbl.find table x >
          Hashtbl.find table y then
       (interpret s2;
        interpret s)
  | Seq(ss) ->
     List.iter interpret ss
  | Print(x) ->
     Printf.printf "%d\n"
       (Hashtbl.find table x)

let main =                                (* 必ずunit型の値()になる *)
  let s =
    (* 標準入力を字句解析＆構文解析する *)
    Parser.statement Lexer.token
      (Lexing.from_channel stdin) in
  interpret s

(* END *)
    

open Syntax

let rec compile s = match s with
  | Const(x, i) ->
     Printf.printf "Hashtbl.replace table \"%s\" (%d);\n" x i
  | Add(x, y, z) ->
     Printf.printf "Hashtbl.replace table \"%s\"\n" x;
     Printf.printf "  (Hashtbl.find table \"%s\" +\n" y;
     Printf.printf "     Hashtbl.find table \"%s\");\n" z
  | While(x, y, s2) ->
     Printf.printf "while Hashtbl.find table \"%s\" >\n" x;
     Printf.printf "  Hashtbl.find table \"%s\" do\n" y;
     compile s2;
     Printf.printf "done;\n"
  | Seq(ss) ->
     Printf.printf "(\n";
     List.iter compile ss;
     Printf.printf ")\n"
  | Print(x) ->
     Printf.printf "Printf.printf \"%%d\\n\"\n";
     Printf.printf "  (Hashtbl.find table \"%s\");" x

let main =
  let s =
    Parser.statement Lexer.token
      (Lexing.from_channel stdin) in
  Printf.printf "let table = Hashtbl.create 10 in\n";
  compile s
    
(* END *)

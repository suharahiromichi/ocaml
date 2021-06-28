open Syntax

type value =
  | Int of int
  | Closure of var * exp * env
and env = (var * value) list

let rec eval env e =
  match e with
  | Const(i) ->
     Int(i)
  | Var(x) ->
     List.assoc x env
  | Add(e1, e2) ->
     let v1 = eval env e1 in
     let v2 = eval env e2 in
     (match (v1, v2) with
      | (Int(i1), Int(i2)) -> Int(i1 + i2)
      | _ -> failwith "addition of non-integer")
  | Let(x, e1, e2) ->
     let v = eval env e1 in
     let newenv = (x, v) :: env in
     eval newenv e2
  | Fun(x, e) ->
     Closure(x, e, env)
  | App(e1, e2) ->
     (match eval env e1 with
      | Closure(x, e', oldenv) ->
         let v = eval env e2 in
         let newenv = (x, v) :: oldenv in
         eval newenv e'
      | _ -> failwith "application of non-function")

let main =
  let e =
    Parser.exp Lexer.token
      (Lexing.from_channel stdin) in
  (match eval [] e with
   | Int(i) -> Printf.printf "%d\n" i
   | Closure _ -> Printf.printf "<closure>\n")
    
(* END *)
    

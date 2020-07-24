open Printf
open Core

exception Exit of int

let err s =
  printf "Error: %s.\n" s;
  raise (Exit 1)

let parse s =
  let lexbuf = Lexing.from_string s in
  let ret = 
    try Parser.toplevel Lexer.main lexbuf with Parsing.Parse_error ->
    err "Parse error" in
  Parsing.clear_parser(); ret

let rec printtm t = match t with
    TmTrue -> printf "true"
  | TmFalse -> printf "false"
  | TmZero -> printf "0"
  | TmIf(t1, t2, t3) -> printf "if "; printtm t1; printf " then "; printtm t2; printf " else "; printtm t3
  | TmSucc(t1) -> printf "succ "; printtm t1
  | TmPred(t1) -> printf "pred "; printtm t1
  | TmIsZero(t1) -> printf "iszero "; printtm t1

let main () =
  let ret = parse Sys.argv.(1) in
  printtm(eval ret)

let _ = main()

open Format
open Support
open Core

let parse s =
  let lexbuf = Lexing.from_string s in
  let ret =
    try Parser.toplevel Lexer.main lexbuf with Parsing.Parse_error ->
    error (Lexer.info lexbuf) "Parse Error" in
  Parsing.clear_parser(); ret

let process_command ctx cmd = match cmd with
  | Eval(fi, t) ->
      let t' = eval ctx t in
      printtm ctx t';
      force_newline();
      ctx
  | Bind(fi, x, bind) ->
      pr x; pr " "; prbinding ctx bind; force_newline();
      addbinding ctx x bind

let process_input s ctx = 
  let cmds,_ = parse s ctx in
  let g ctx c =
    open_hvbox 0;
    let ret = process_command ctx c in
    print_flush();
    ret
  in
    List.fold_left g ctx cmds

let main () = let _ = process_input Sys.argv.(1) emptycontext in ()

let _ = main()

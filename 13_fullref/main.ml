open Format
open Support
open Core

let parse s =
  let lexbuf = Lexing.from_string s in
  let ret =
    try Parser.toplevel Lexer.main lexbuf with Parsing.Parse_error ->
    error (Lexer.info lexbuf) "Parse Error" in
  Parsing.clear_parser(); ret

let checkbinding fi ctx b = match b with
    NameBind -> NameBind
  | TyVarBind -> TyVarBind
  | VarBind(tyT) -> VarBind(tyT)
  | TyAbbBind(tyT) -> TyAbbBind(tyT)
  | TmAbbBind(t, None) -> TmAbbBind(t, Some(typeof ctx t))
  | TmAbbBind(t, Some(tyT)) ->
      let tyT' = typeof ctx t in
      if (=) tyT' tyT then TmAbbBind(t, Some(tyT))
      else error fi "Type of binding does not match declared type"

let prbindingty ctx b = match b with
    NameBind -> ()
  | TyVarBind -> ()
  | VarBind(tyT) -> pr ": "; printty ctx tyT
  | TyAbbBind(tyT) -> pr ":: *"
  | TmAbbBind(t, tyT_opt) -> pr ": ";
      (match tyT_opt with
           None -> printty ctx (typeof ctx t)
         | Some(tyT) -> printty ctx tyT)

let process_command (ctx,store) cmd = match cmd with
  | Eval(fi, t) ->
      let tyT = typeof ctx t in
      let t',store = eval ctx store t in
      printtm ctx t';
      pr ": ";
      printty ctx tyT;
      force_newline();
      (ctx,store)
  | Bind(fi, x, bind) ->
      let bind = checkbinding fi ctx bind in
      let bind',store' = evalbinding ctx store bind in
      pr x; pr " "; prbindingty ctx bind'; force_newline();
      addbinding ctx x bind', (shiftstore 1 store')

let process_input s (ctx,store) =
  let cmds,_ = parse s ctx in
  let g (ctx,store) c =
    open_hvbox 0;
    let ret = process_command (ctx,store) c in
    print_flush();
    ret
  in
    List.fold_left g (ctx,store) cmds

let main () = let _ = process_input Sys.argv.(1) (emptycontext, emptystore) in ()

let _ = main()

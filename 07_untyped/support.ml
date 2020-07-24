open Format

type info = FI of string * int * int | UNKNOWN 
type 'a withinfo = {i: info; v: 'a}

let dummyinfo = UNKNOWN
let createInfo f l c = FI(f, l, c)

exception Exit of int

let pr = print_string
let errf f =
  print_flush();
  open_vbox 0;
  open_hvbox 0; f(); print_cut(); close_box(); print_newline();
  raise (Exit 1)
let printInfo = 
  function
    FI(f, l, c) ->
      pr f; pr ":"; print_int l; pr "."; print_int c; pr ":"
  | UNKNOWN ->
      pr "<Unknown file and line>: "
let errfAt fi f = errf(fun() -> printInfo fi; print_space(); f())
let error fi s = errfAt fi (fun() -> pr s; print_newline())

let index2name fi ctx x =
  try
    let (xn, _) = List.nth ctx x in
    xn
  with Failure _ ->
    let msg =
      Printf.sprintf "Variable lookup failure: offset: %d, ctx size: %d" in
    error fi (msg x (List.length ctx))

let rec name2index fi ctx x = match ctx with
    [] -> error fi ("Identifier " ^ x ^ " is unbound")
  | (y, _)::rest -> if y = x then 0
                    else 1 + (name2index fi rest x)

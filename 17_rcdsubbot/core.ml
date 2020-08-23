open Format
open Support

type ty =
    TyArr of ty * ty
  | TyBool
  | TyRecord of (string * ty) list

type term =
    TmVar of info * int * int
  | TmAbs of info * string * ty * term
  | TmApp of info * term * term
  | TmTrue of info
  | TmFalse of info
  | TmIf of info * term * term * term
  | TmRecord of info * (string * term) list
  | TmProj of info * term * string

let tmInfo t = match t with
    TmVar(fi, _, _) -> fi
  | TmAbs(fi, _, _, _) -> fi
  | TmApp(fi, _, _) -> fi
  | TmTrue(fi) -> fi
  | TmFalse(fi) -> fi
  | TmIf(fi, _, _, _) -> fi
  | TmRecord(fi, _) -> fi
  | TmProj(fi, _, _) -> fi

type binding = NameBind | VarBind of ty
type context = (string * binding) list

let emptycontext = []

let ctxlength ctx = List.length ctx
let addbinding ctx x bind = (x, bind)::ctx
let addname ctx x = addbinding ctx x NameBind

let rec isnamebound ctx x = match ctx with
    [] -> false
  | (y, _)::rest ->
      if y = x then true
      else isnamebound rest x

let rec pickfreshname ctx x =
  if isnamebound ctx x then pickfreshname ctx (x^"'")
  else ((x, NameBind)::ctx), x

type command =
  | Eval of info * term
  | Bind of info * string * binding

let rec printty_Type tyT = match tyT with
    tyT -> printty_ArrowType tyT
and printty_ArrowType tyT = match tyT with
    TyArr(tyT1, tyT2) ->
      printty_AType tyT1;
      pr " -> ";
      printty_ArrowType tyT2;
  | tyT -> printty_AType tyT
and printty_AType tyT = match tyT with
    TyRecord(fields) ->
      let pf i (li,tyTi)  =
        if (li <> (string_of_int i)) then (pr li; pr ":");
        printty_Type tyTi
      in let rec p i l = match l with
          [] -> ()
        | [f] -> pf i f
        | f::rest -> pf i f; pr ","; p (i+1) rest
      in pr "{"; p 1 fields; pr "}"
  | TyBool -> pr "Bool"
  | tyT -> pr "("; printty_Type tyT; pr ")"

let printty tyT = printty_Type tyT

let rec printtm ctx t = match t with
    TmAbs(fi, x, tyT1, t1) ->
      let (ctx', x') = pickfreshname ctx x in
      pr "(lambda "; pr x'; pr ":"; printty tyT1; pr ". "; printtm ctx' t1; pr ")"
  | TmApp(fi, t1, t2) ->
      pr "("; printtm ctx t1; pr " "; printtm ctx t2; pr ")"
  | TmVar(fi, x, n) ->
      if ctxlength ctx = n then
        pr (index2name fi ctx x)
      else
        pr "[bad index]"
  | TmTrue(fi) -> pr "true"
  | TmFalse(fi) -> pr "false"
  | TmIf(fi, t1, t2, t3) ->
      pr "if "; printtm ctx t1; pr " then "; printtm ctx t2; pr " else "; printtm ctx t3
  | TmRecord(fi, fields) ->
      let pf i (li,ti) =
        if (li <> (string_of_int i)) then (pr li; pr "=");
        printtm ctx ti
      in let rec p i l = match l with
          [] -> ()
        | [f] -> pf i f
        | f::rest -> pf i f; pr ","; p (i+1) rest
      in pr "{"; p 1 fields; pr "}";
  | TmProj(fi, t1, l) -> printtm ctx t1; pr "."; pr l

let prbinding ctx b = match b with
    NameBind -> () 
  | VarBind(tyT) -> pr ": "; printty tyT

let termShift d t =
  let rec walk c t = match t with
    TmVar(fi, x, n) -> if x >= c then TmVar(fi, x+d, n+d)
                       else TmVar(fi, x, n+d)
  | TmAbs(fi, x, tyT1, t1) -> TmAbs(fi, x, tyT1, walk (c+1) t1)
  | TmApp(fi, t1, t2) -> TmApp(fi, walk c t1, walk c t2)
  | TmTrue(fi) as t -> t
  | TmFalse(fi) as t -> t
  | TmIf(fi, t1, t2, t3) -> TmIf(fi, walk c t1, walk c t2, walk c t3)
  | TmRecord(fi, fields) -> TmRecord(fi, List.map (fun (li, ti) -> (li, walk c ti)) fields)
  | TmProj(fi, t1, l) -> TmProj(fi, walk c t1, l)
  in walk 0 t

let termSubst j s t =
  let rec walk c t = match t with
    TmVar(fi, x, n) -> if x = j+c then termShift c s else TmVar(fi, x, n)
  | TmAbs(fi, x, tyT1, t1) -> TmAbs(fi, x, tyT1, walk (c+1) t1)
  | TmApp(fi, t1, t2) -> TmApp(fi, walk c t1, walk c t2)
  | TmTrue(fi) as t -> t
  | TmFalse(fi) as t -> t
  | TmIf(fi, t1, t2, t3) -> TmIf(fi, walk c t1, walk c t2, walk c t3)
  | TmRecord(fi, fields) -> TmRecord(fi, List.map (fun (li, ti) -> (li, walk c ti)) fields)
  | TmProj(fi, t1, l) -> TmProj(fi, walk c t1, l)
  in walk 0 t

let termSubstTop s t =
  termShift (-1) (termSubst 0 (termShift 1 s) t)

let rec isval ctx t = match t with
    TmTrue(_) -> true
  | TmFalse(_) -> true
  | TmAbs(_, _, _, _) -> true
  | TmRecord(_, fields) -> List.for_all (fun (l, ti) -> isval ctx ti) fields
  | _ -> false

exception NoRuleApplies

let rec eval1 ctx t = match t with
    TmApp(fi, TmAbs(_, x, tyT11, t12), v2) when isval ctx v2 ->
      termSubstTop v2 t12
  | TmApp(fi, v1, t2) when isval ctx v1 ->
      let t2' = eval1 ctx t2 in
      TmApp(fi, v1, t2')
  | TmApp(fi, t1, t2) ->
      let t1' = eval1 ctx t1 in
      TmApp(fi, t1', t2)
  | TmIf(_, TmTrue(_), t2, t3) -> t2
  | TmIf(_, TmFalse(_), t2, t3) -> t3
  | TmIf(fi, t1, t2, t3) ->
      let t1' = eval1 ctx t1 in
      TmIf(fi, t1', t2, t3)
  | TmRecord(fi, fields) ->
      let rec evalfield l = match l with
        [] -> raise NoRuleApplies
      | (l, vi)::rest when isval ctx vi ->
          let rest' = evalfield rest in
          (l, vi)::rest'
      | (l, ti)::rest ->
          let ti' = eval1 ctx ti in
          (l, ti')::rest
      in let fields' = evalfield fields in
      TmRecord(fi, fields')
  | TmProj(fi, (TmRecord(_, fields) as v1), l) when isval ctx v1 ->
      (try List.assoc l fields
       with Not_found -> raise NoRuleApplies)
  | TmProj(fi, t1, l) ->
      let t1' = eval1 ctx t1 in
      TmProj(fi, t1', l)
  | _ ->
      raise NoRuleApplies

let rec eval ctx t =
  try let t' = eval1 ctx t
      in eval ctx t'
  with NoRuleApplies -> t

let getbinding fi ctx i =
  try
    let (_, bind) = List.nth ctx i in
    bind
  with Failure _ ->
    let msg = Printf.sprintf "Variable lookup failure: offset: %d, ctx size: %d" in
    error fi (msg i (List.length ctx))
let getTypeFromContext fi ctx i = match getbinding fi ctx i with
    VarBind(tyT) -> tyT
  | _ -> error fi ("getTypeFromContext: Wrong kind of binding for variable " ^ (index2name fi ctx i))

let rec typeof ctx t = match t with
    TmVar(fi, i, _) -> getTypeFromContext fi ctx i
  | TmAbs(fi, x, tyT1, t2) ->
      let ctx' = addbinding ctx x (VarBind(tyT1)) in
      let tyT2 = typeof ctx' t2 in
      TyArr(tyT1, tyT2)
  | TmApp(fi, t1, t2) ->
      let tyT1 = typeof ctx t1 in
      let tyT2 = typeof ctx t2 in
      (match tyT1 with
          TyArr(tyT11, tyT12) ->
            if (=) tyT2 tyT11 then tyT12
            else error fi "parameter type mismatch"
        | _ -> error fi "arrow type expected")
  | TmTrue(fi) -> TyBool
  | TmFalse(fi) -> TyBool
  | TmIf(fi, t1, t2, t3) ->
      if (=) (typeof ctx t1) TyBool then
        let tyT2 = typeof ctx t2 in
        if (=) tyT2 (typeof ctx t3) then tyT2
        else error fi "arms of conditional have different types"
      else error fi "guard of conditional not a boolean"
  | TmRecord(fi, fields) ->
      let fieldtys = List.map (fun (li, ti) -> (li, typeof ctx ti)) fields in
      TyRecord(fieldtys)
  | TmProj(fi, t1, l) ->
      (match typeof ctx t1 with
          TyRecord(fieldtys) -> 
            (try List.assoc l fieldtys
             with Not_found -> error fi ("label "^l^" not found"))
        | _ -> error fi "Expected record type")

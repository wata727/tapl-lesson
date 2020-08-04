open Format
open Support

type ty =
    TyVar of int * int
  | TyId of string
  | TyArr of ty * ty
  | TyRecord of (string * ty) list
  | TyBool
  | TyString
  | TyFloat
  | TyUnit
  | TyNat

type term =
    TmVar of info * int * int
  | TmAbs of info * string * ty * term
  | TmApp of info * term * term
  | TmLet of info * string * term * term
  | TmTrue of info
  | TmFalse of info
  | TmIf of info * term * term * term
  | TmString of info * string
  | TmFloat of info * float
  | TmTimesfloat of info * term * term
  | TmZero of info
  | TmSucc of info * term
  | TmPred of info * term
  | TmIsZero of info * term
  | TmUnit of info
  | TmAscribe of info * term * ty
  | TmRecord of info * (string * term) list
  | TmProj of info * term * string

let tmInfo t = match t with
    TmVar(fi, _, _) -> fi
  | TmAbs(fi, _, _, _) -> fi
  | TmApp(fi, _, _) -> fi
  | TmLet(fi, _, _, _) -> fi
  | TmTrue(fi) -> fi
  | TmFalse(fi) -> fi
  | TmIf(fi, _, _, _) -> fi
  | TmString(fi, _) -> fi
  | TmFloat(fi, _) -> fi
  | TmTimesfloat(fi, _, _) -> fi
  | TmZero(fi) -> fi
  | TmSucc(fi, _) -> fi
  | TmPred(fi, _) -> fi
  | TmIsZero(fi, _) -> fi
  | TmUnit(fi) -> fi
  | TmAscribe(fi, _, _) -> fi
  | TmRecord(fi, _) -> fi
  | TmProj(fi, _, _) -> fi

type binding =
    NameBind
  | TyVarBind
  | VarBind of ty
  | TyAbbBind of ty
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

let rec printty_Type ctx tyT = match tyT with
    tyT -> printty_ArrowType ctx tyT
and printty_ArrowType ctx tyT = match tyT with
    TyArr(tyT1, tyT2) ->
      printty_AType ctx tyT1;
      pr " -> ";
      printty_ArrowType ctx tyT2;
  | tyT -> printty_AType ctx tyT
and printty_AType ctx tyT = match tyT with
    TyVar(x,n) ->
      if ctxlength ctx = n then
        pr (index2name dummyinfo ctx x)
      else
        pr ("[bad index: " ^ (string_of_int x) ^ "/" ^ (string_of_int n)
          ^ " in {"
          ^ (List.fold_left (fun s (x,_) -> s ^ " " ^ x) "" ctx)
          ^ " }]")
  | TyRecord(fields) ->
      let rec pf (li,tyTi) = printty_Type ctx tyTi
      in let rec p l = match l with
          [] -> ()
        | [f] -> pf f
        | f::rest -> pf f; pr ","; p rest
      in pr "{"; p fields; pr "}"
  | TyId(b) -> pr b
  | TyBool -> pr "Bool"
  | TyString -> pr "String"
  | TyFloat -> pr "Float"
  | TyUnit -> pr "Unit"
  | TyNat -> pr "Nat"
  | tyT -> pr "("; printty_Type ctx tyT; pr ")"

let printty ctx tyT = printty_Type ctx tyT

let rec printtm ctx t = match t with
    TmAbs(fi, x, tyT1, t1) ->
      let (ctx', x') = pickfreshname ctx x in
      pr "(lambda "; pr x'; pr ":"; printty ctx tyT1; pr ". "; printtm ctx' t1; pr ")"
  | TmApp(fi, t1, t2) ->
      pr "("; printtm ctx t1; pr " "; printtm ctx t2; pr ")"
  | TmLet(fi, x, t1, t2) ->
      pr "let "; pr x; pr " = "; printtm ctx t1; pr " in "; printtm (addname ctx x) t2
  | TmVar(fi, x, n) ->
      if ctxlength ctx = n then
        pr (index2name fi ctx x)
      else
        pr "[bad index]"
  | TmTrue(fi) -> pr "true"
  | TmFalse(fi) -> pr "false"
  | TmIf(fi, t1, t2, t3) ->
      pr "if "; printtm ctx t1; pr " then "; printtm ctx t2; pr " else "; printtm ctx t3
  | TmString(fi, s) -> pr ("\"" ^ s ^ "\"")
  | TmFloat(fi, s) -> pr (string_of_float s)
  | TmTimesfloat(fi, t1, t2) -> pr "timesfloat "; printtm ctx t1; pr " "; printtm ctx t2
  | TmZero(fi) -> pr "0"
  | TmSucc(fi, t1) ->
      let rec f n t = match t with
          TmZero(_) -> pr (string_of_int n)
        | TmSucc(_,s) -> f (n+1) s
        | _ -> (pr "(succ "; printtm ctx t1; pr ")")
      in f 1 t1
  | TmPred(fi, t1) -> pr "pred "; printtm ctx t1
  | TmIsZero(fi, t1) -> pr "iszero "; printtm ctx t1
  | TmUnit(fi) -> pr "unit"
  | TmAscribe(fi, t1, tyT1) -> printtm ctx t1; pr " as "; printty ctx tyT1
  | TmRecord(fi, fields) ->
      let rec pf (li,ti) = printtm ctx ti
      in let rec p l = match l with
          [] -> ()
        | [f] -> pf f
        | f::rest -> pf f; pr ","; p rest
      in pr "{"; p fields; pr "}"
  | TmProj(fi, t1, l) -> printtm ctx t1; pr "."; pr l

let prbinding ctx b = match b with
    NameBind -> () 
  | TyVarBind -> ()
  | VarBind(tyT) -> pr ": "; printty ctx tyT
  | TyAbbBind(tyT) -> pr "= "; printty ctx tyT

let typeShiftAbove d c tyT =
  let rec walk c tyT = match tyT with
    TyVar(x, n) -> if x>=c then TyVar(x+d,n+d) else TyVar(x,n+d)
  | TyId(b) as tyT -> tyT
  | TyString -> TyString
  | TyUnit -> TyUnit
  | TyFloat -> TyFloat
  | TyBool -> TyBool
  | TyNat -> TyNat
  | TyArr(tyT1,tyT2) -> TyArr(walk c tyT1,walk c tyT2)
  | TyRecord(fieldtys) -> TyRecord(List.map (fun (li, tyTi) -> (li, walk c tyTi)) fieldtys)
  in walk c tyT

let typeShift d tyT = typeShiftAbove d 0 tyT

let termShift d t =
  let rec walk c t = match t with
    TmVar(fi, x, n) -> if x >= c then TmVar(fi, x+d, n+d)
                       else TmVar(fi, x, n+d)
  | TmAbs(fi, x, tyT1, t1) -> TmAbs(fi, x, tyT1, walk (c+1) t1)
  | TmApp(fi, t1, t2) -> TmApp(fi, walk c t1, walk c t2)
  | TmLet(fi, x, t1, t2) -> TmLet(fi, x, walk c t1, walk (c+1) t2)
  | TmTrue(fi) as t -> t
  | TmFalse(fi) as t -> t
  | TmIf(fi, t1, t2, t3) -> TmIf(fi, walk c t1, walk c t2, walk c t3)
  | TmString _ as t -> t
  | TmFloat _ as t -> t
  | TmTimesfloat(fi, t1, t2) -> TmTimesfloat(fi, walk c t1, walk c t2)
  | TmZero(fi) -> TmZero(fi)
  | TmSucc(fi, t1) -> TmSucc(fi, walk c t1)
  | TmPred(fi, t1) -> TmPred(fi, walk c t1)
  | TmIsZero(fi, t1) -> TmIsZero(fi, walk c t1)
  | TmUnit(fi) as t -> t
  | TmAscribe(fi, t1, tyT1) -> TmAscribe(fi, walk c t1, typeShiftAbove d c tyT1)
  | TmRecord(fi, fields) -> TmRecord(fi, List.map (fun (li, ti) -> (li, walk c ti)) fields)
  | TmProj(fi, t1, l) -> TmProj(fi, walk c t1, l)
  in walk 0 t

let termSubst j s t =
  let rec walk c t = match t with
    TmVar(fi, x, n) -> if x = j+c then termShift c s else TmVar(fi, x, n)
  | TmAbs(fi, x, tyT1, t1) -> TmAbs(fi, x, tyT1, walk (c+1) t1)
  | TmApp(fi, t1, t2) -> TmApp(fi, walk c t1, walk c t2)
  | TmLet(fi, x, t1, t2) -> TmLet(fi, x, walk c t1, walk (c+1) t2)
  | TmTrue(fi) as t -> t
  | TmFalse(fi) as t -> t
  | TmIf(fi, t1, t2, t3) -> TmIf(fi, walk c t1, walk c t2, walk c t3)
  | TmString _ as t -> t
  | TmFloat _ as t -> t
  | TmTimesfloat(fi, t1, t2) -> TmTimesfloat(fi, walk c t1, walk c t2)
  | TmZero(fi) -> TmZero(fi)
  | TmSucc(fi, t1) -> TmSucc(fi, walk c t1)
  | TmPred(fi, t1) -> TmPred(fi, walk c t1)
  | TmIsZero(fi, t1) -> TmIsZero(fi, walk c t1)
  | TmUnit(fi) as t -> t
  | TmAscribe(fi, t1, tyT1) -> TmAscribe(fi, walk c t1, tyT1)
  | TmRecord(fi, fields) -> TmRecord(fi, List.map (fun (li, ti) -> (li, walk c ti)) fields)
  | TmProj(fi, t1, l) -> TmProj(fi, walk c t1, l)
  in walk 0 t

let termSubstTop s t =
  termShift (-1) (termSubst 0 (termShift 1 s) t)

let rec isnumericval ctx t = match t with
    TmZero(_) -> true
  | TmSucc(_, t1) -> isnumericval ctx t1
  | _ -> false

let rec isval ctx t = match t with
    TmTrue(_) -> true
  | TmFalse(_) -> true
  | TmAbs(_, _, _, _) -> true
  | TmRecord(_, fields) -> List.for_all (fun (l, ti) -> isval ctx ti) fields
  | TmString _ -> true
  | TmUnit(_) -> true
  | TmFloat _ -> true
  | t when isnumericval ctx t -> true
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
  | TmLet(fi, x, v1, t2) when isval ctx v1 ->
      termSubstTop v1 t2
  | TmLet(fi, x, t1, t2) ->
      let t1' = eval1 ctx t1 in
      TmLet(fi, x, t1', t2)
  | TmIf(_, TmTrue(_), t2, t3) -> t2
  | TmIf(_, TmFalse(_), t2, t3) -> t3
  | TmIf(fi, t1, t2, t3) ->
      let t1' = eval1 ctx t1 in
      TmIf(fi, t1', t2, t3)
  | TmTimesfloat(fi, TmFloat(_, f1), TmFloat(_, f2)) -> TmFloat(fi, f1 *. f2)
  | TmTimesfloat(fi, (TmFloat(_, f1) as t1), t2) ->
      let t2' = eval1 ctx t2 in
      TmTimesfloat(fi, t1, t2')
  | TmTimesfloat(fi, t1, t2) ->
      let t1' = eval1 ctx t1 in
      TmTimesfloat(fi, t1', t2)
  | TmSucc(fi, t1) ->
      let t1' = eval1 ctx t1 in
      TmSucc(fi, t1')
  | TmPred(_, TmZero(_)) -> TmZero(dummyinfo)
  | TmPred(_, TmSucc(_,nv1)) when (isnumericval ctx nv1) -> nv1
  | TmPred(fi, t1) ->
      let t1' = eval1 ctx t1 in
      TmPred(fi, t1')
  | TmIsZero(fi, TmZero(_)) -> TmTrue(dummyinfo)
  | TmIsZero(fi, TmSucc(_,nv1)) when (isnumericval ctx nv1) -> TmFalse(dummyinfo)
  | TmIsZero(fi, t1) ->
      let t1' = eval1 ctx t1 in
      TmIsZero(fi, t1')
  | TmAscribe(fi, v1, tyT) when isval ctx v1 -> v1
  | TmAscribe(fi, t1, tyT) ->
      let t1' = eval1 ctx t1 in
      TmAscribe(fi, t1', tyT)
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

let bindingshift d bind = match bind with
    NameBind -> NameBind
  | TyVarBind -> TyVarBind
  | VarBind(tyT) -> VarBind(typeShift d tyT)
  | TyAbbBind(tyT) -> TyAbbBind(typeShift d tyT)

let getbinding fi ctx i =
  try
    let (_, bind) = List.nth ctx i in
    bindingshift (i+1) bind
  with Failure _ ->
    let msg = Printf.sprintf "Variable lookup failure: offset: %d, ctx size: %d" in
    error fi (msg i (List.length ctx))
let getTypeFromContext fi ctx i = match getbinding fi ctx i with
    VarBind(tyT) -> tyT
  | _ -> error fi ("getTypeFromContext: Wrong kind of binding for variable " ^ (index2name fi ctx i))

let istyabb ctx i = match getbinding dummyinfo ctx i with
    TyAbbBind(tyT) -> true
  | _ -> false

let gettyabb ctx i = match getbinding dummyinfo ctx i with
    TyAbbBind(tyT) -> tyT
  | _ -> raise NoRuleApplies

let rec computety ctx tyT = match tyT with
    TyVar(i, _) when istyabb ctx i -> gettyabb ctx i
  | _ -> raise NoRuleApplies

let rec simplifyty ctx tyT =
  try
    let tyT' = computety ctx tyT in
    simplifyty ctx tyT'
  with NoRuleApplies -> tyT

let rec typeof ctx t = match t with
    TmVar(fi, i, _) -> getTypeFromContext fi ctx i
  | TmAbs(fi, x, tyT1, t2) ->
      let ctx' = addbinding ctx x (VarBind(tyT1)) in
      let tyT2 = typeof ctx' t2 in
      TyArr(tyT1, tyT2)
  | TmApp(fi, t1, t2) ->
      let tyT1 = typeof ctx t1 in
      let tyT2 = typeof ctx t2 in
      (match simplifyty ctx tyT1 with
          TyArr(tyT11, tyT12) ->
            if (=) tyT2 tyT11 then tyT12
            else error fi "parameter type mismatch"
        | _ -> error fi "arrow type expected")
  | TmLet(fi, x, t1, t2) ->
      let tyT1 = typeof ctx t1 in
      let ctx' = addbinding ctx x (VarBind(tyT1)) in
      typeShift (-1) (typeof ctx' t2)
  | TmTrue(fi) -> TyBool
  | TmFalse(fi) -> TyBool
  | TmIf(fi, t1, t2, t3) ->
      if (=) (typeof ctx t1) TyBool then
        let tyT2 = typeof ctx t2 in
        if (=) tyT2 (typeof ctx t3) then tyT2
        else error fi "arms of conditional have different types"
      else error fi "guard of conditional not a boolean"
  | TmString _ -> TyString
  | TmFloat _ -> TyFloat
  | TmTimesfloat(fi, t1, t2) ->
      if (=) (typeof ctx t1) TyFloat
      && (=) (typeof ctx t2) TyFloat then TyFloat
      else error fi "argument of timesfloat is not a number"
  | TmZero(fi) -> TyNat
  | TmSucc(fi, t1) ->
      if (=) (typeof ctx t1) TyNat then TyNat
      else error fi "argument of succ is not a number"
  | TmPred(fi, t1) ->
      if (=) (typeof ctx t1) TyNat then TyNat
      else error fi "argument of pred is not a number"
  | TmIsZero(fi, t1) ->
      if (=) (typeof ctx t1) TyNat then TyBool
      else error fi "argument of iszero is not a number"
  | TmUnit(fi) -> TyUnit
  | TmAscribe(fi, t1, tyT) ->
      if (=) (simplifyty ctx (typeof ctx t1)) (simplifyty ctx tyT) then tyT
      else error fi "body of as-term does not have the expected type"
  | TmRecord(fi, fields) ->
      let fieldtys = List.map (fun (li, ti) -> (li, typeof ctx ti)) fields in
      TyRecord(fieldtys)
  | TmProj(fi, t1, l) ->
      (match simplifyty ctx (typeof ctx t1) with
          TyRecord(fieldtys) -> 
            (try List.assoc l fieldtys
             with Not_found -> error fi ("label "^l^" not found"))
        | _ -> error fi "Expected record type")

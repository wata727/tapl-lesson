open Format
open Support

type ty =
    TyVar of int * int
  | TyId of string
  | TyArr of ty * ty
  | TyBool
  | TyString
  | TyFloat
  | TyUnit
  | TyNat

type term =
    TmVar of info * int * int
  | TmAbs of info * string * ty * term
  | TmApp of info * term * term
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

let tmInfo t = match t with
    TmVar(fi, _, _) -> fi
  | TmAbs(fi, _, _, _) -> fi
  | TmApp(fi, _, _) -> fi
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

type binding =
    NameBind
  | TyVarBind
  | VarBind of ty
  | TyAddBind of ty
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

let prbinding ctx b = match b with
    NameBind -> () 
  | TyVarBind -> ()
  | VarBind(tyT) -> pr ": "; printty ctx tyT
  | TyAddBind(tyT) -> pr "= "; printty ctx tyT

let termShift d t =
  let rec walk c t = match t with
    TmVar(fi, x, n) -> if x >= c then TmVar(fi, x+d, n+d)
                       else TmVar(fi, x, n+d)
  | TmAbs(fi, x, tyT1, t1) -> TmAbs(fi, x, tyT1, walk (c+1) t1)
  | TmApp(fi, t1, t2) -> TmApp(fi, walk c t1, walk c t2)
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
  in walk 0 t

let termSubst j s t =
  let rec walk c t = match t with
    TmVar(fi, x, n) -> if x = j+c then termShift c s else TmVar(fi, x, n)
  | TmAbs(fi, x, tyT1, t1) -> TmAbs(fi, x, tyT1, walk (c+1) t1)
  | TmApp(fi, t1, t2) -> TmApp(fi, walk c t1, walk c t2)
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

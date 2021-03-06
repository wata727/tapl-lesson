open Format
open Support

type ty =
    TyVar of int * int
  | TyId of string
  | TyArr of ty * ty
  | TyRef of ty
  | TyBool
  | TyUnit
  | TyNat

type term =
    TmVar of info * int * int
  | TmAbs of info * string * ty * term
  | TmApp of info * term * term
  | TmTrue of info
  | TmFalse of info
  | TmIf of info * term * term * term
  | TmZero of info
  | TmSucc of info * term
  | TmPred of info * term
  | TmIsZero of info * term
  | TmUnit of info
  | TmLoc of info * int
  | TmRef of info * term
  | TmDeref of info * term
  | TmAssign of info * term * term

let tmInfo t = match t with
    TmVar(fi, _, _) -> fi
  | TmAbs(fi, _, _, _) -> fi
  | TmApp(fi, _, _) -> fi
  | TmTrue(fi) -> fi
  | TmFalse(fi) -> fi
  | TmIf(fi, _, _, _) -> fi
  | TmZero(fi) -> fi
  | TmSucc(fi, _) -> fi
  | TmPred(fi, _) -> fi
  | TmIsZero(fi, _) -> fi
  | TmUnit(fi) -> fi
  | TmLoc(fi, _) -> fi
  | TmRef(fi, _) -> fi
  | TmDeref(fi, _) -> fi
  | TmAssign(fi, _, _) -> fi

type binding =
    NameBind
  | TyVarBind
  | VarBind of ty
  | TyAbbBind of ty
  | TmAbbBind of term * (ty option)
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
    TyRef(tyT) -> pr "Ref "; printty_AType ctx tyT
  | tyT -> printty_ArrowType ctx tyT
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
  | TmLoc(fi, l) -> pr "<loc #"; print_int l; pr ">"
  | TmRef(fi, t1) -> pr "ref "; printtm ctx t1
  | TmDeref(fi, t1) -> pr "!"; printtm ctx t1
  | TmAssign(fi, t1, t2) -> printtm ctx t1; pr " := "; printtm ctx t2

let prbinding ctx b = match b with
    NameBind -> () 
  | TyVarBind -> ()
  | VarBind(tyT) -> pr ": "; printty ctx tyT
  | TyAbbBind(tyT) -> pr "= "; printty ctx tyT
  | TmAbbBind(t, tyT) -> pr "= "; printtm ctx t

let typeShiftAbove d c tyT =
  let rec walk c tyT = match tyT with
    TyVar(x, n) -> if x>=c then TyVar(x+d,n+d) else TyVar(x,n+d)
  | TyId(b) as tyT -> tyT
  | TyUnit -> TyUnit
  | TyBool -> TyBool
  | TyNat -> TyNat
  | TyArr(tyT1,tyT2) -> TyArr(walk c tyT1,walk c tyT2)
  | TyRef(tyT1) -> TyRef(walk c tyT1)
  in walk c tyT

let typeShift d tyT = typeShiftAbove d 0 tyT

let termShift d t =
  let rec walk c t = match t with
    TmVar(fi, x, n) -> if x >= c then TmVar(fi, x+d, n+d)
                       else TmVar(fi, x, n+d)
  | TmAbs(fi, x, tyT1, t1) -> TmAbs(fi, x, tyT1, walk (c+1) t1)
  | TmApp(fi, t1, t2) -> TmApp(fi, walk c t1, walk c t2)
  | TmTrue(fi) as t -> t
  | TmFalse(fi) as t -> t
  | TmIf(fi, t1, t2, t3) -> TmIf(fi, walk c t1, walk c t2, walk c t3)
  | TmZero(fi) -> TmZero(fi)
  | TmSucc(fi, t1) -> TmSucc(fi, walk c t1)
  | TmPred(fi, t1) -> TmPred(fi, walk c t1)
  | TmIsZero(fi, t1) -> TmIsZero(fi, walk c t1)
  | TmUnit(fi) as t -> t
  | TmLoc(fi, l) as t -> t
  | TmRef(fi, t1) -> TmRef(fi, walk c t1)
  | TmDeref(fi, t1) -> TmDeref(fi, walk c t1)
  | TmAssign(fi, t1, t2) -> TmAssign(fi, walk c t1, walk c t2)
  in walk 0 t

let termSubst j s t =
  let rec walk c t = match t with
    TmVar(fi, x, n) -> if x = j+c then termShift c s else TmVar(fi, x, n)
  | TmAbs(fi, x, tyT1, t1) -> TmAbs(fi, x, tyT1, walk (c+1) t1)
  | TmApp(fi, t1, t2) -> TmApp(fi, walk c t1, walk c t2)
  | TmTrue(fi) as t -> t
  | TmFalse(fi) as t -> t
  | TmIf(fi, t1, t2, t3) -> TmIf(fi, walk c t1, walk c t2, walk c t3)
  | TmZero(fi) -> TmZero(fi)
  | TmSucc(fi, t1) -> TmSucc(fi, walk c t1)
  | TmPred(fi, t1) -> TmPred(fi, walk c t1)
  | TmIsZero(fi, t1) -> TmIsZero(fi, walk c t1)
  | TmUnit(fi) as t -> t
  | TmLoc(fi, l) as t -> t
  | TmRef(fi, t1) -> TmRef(fi, walk c t1)
  | TmDeref(fi, t1) -> TmDeref(fi, walk c t1)
  | TmAssign(fi, t1, t2) -> TmAssign(fi, walk c t1, walk c t2)
  in walk 0 t

let termSubstTop s t =
  termShift (-1) (termSubst 0 (termShift 1 s) t)

let bindingshift d bind = match bind with
    NameBind -> NameBind
  | TyVarBind -> TyVarBind
  | VarBind(tyT) -> VarBind(typeShift d tyT)
  | TyAbbBind(tyT) -> TyAbbBind(typeShift d tyT)
  | TmAbbBind(t, tyT_opt) ->
      let tyT_opt' = match tyT_opt with
                       None -> None
                     | Some(tyT) -> Some(typeShift d tyT) in
      TmAbbBind(termShift d t, tyT_opt')

let getbinding fi ctx i =
  try
    let (_, bind) = List.nth ctx i in
    bindingshift (i+1) bind
  with Failure _ ->
    let msg = Printf.sprintf "Variable lookup failure: offset: %d, ctx size: %d" in
    error fi (msg i (List.length ctx))

let rec isnumericval ctx t = match t with
    TmZero(_) -> true
  | TmSucc(_, t1) -> isnumericval ctx t1
  | _ -> false

let rec isval ctx t = match t with
    TmTrue(_) -> true
  | TmFalse(_) -> true
  | TmAbs(_, _, _, _) -> true
  | TmUnit(_) -> true
  | TmLoc(_, _) -> true
  | t when isnumericval ctx t -> true
  | _ -> false

type store = term list
let emptystore = []
let extendstore store v = (List.length store, List.append store [v])
let lookuploc store l = List.nth store l
let updatestore store n v =
  let rec f s = match s with
      (0, v'::rest) -> v::rest
    | (n, v'::rest) -> v' :: (f (n-1,rest))
    | _ -> error dummyinfo "updatestore: bad index"
  in f (n, store)
let shiftstore i store = List.map (fun t -> termShift i t) store

exception NoRuleApplies

let rec eval1 ctx store t = match t with
    TmApp(fi, TmAbs(_, x, tyT11, t12), v2) when isval ctx v2 ->
      termSubstTop v2 t12, store
  | TmApp(fi, v1, t2) when isval ctx v1 ->
      let t2',store' = eval1 ctx store t2 in
      TmApp(fi, v1, t2'),store'
  | TmApp(fi, t1, t2) ->
      let t1',store' = eval1 ctx store t1 in
      TmApp(fi, t1', t2),store'
  | TmVar(fi, n, _) ->
      (match getbinding fi ctx n with
           TmAbbBind(t, _) -> t,store
         | _ -> raise NoRuleApplies)
  | TmRef(fi, t1) ->
      if not (isval ctx t1) then
        let (t1',store') = eval1 ctx store t1
        in (TmRef(fi,t1'), store')
      else
        let (l,store') = extendstore store t1 in
        (TmLoc(dummyinfo,l), store')
  | TmDeref(fi, t1) ->
      if not (isval ctx t1) then
        let (t1',store') = eval1 ctx store t1
        in (TmDeref(fi, t1'), store')
      else (match t1 with
            TmLoc(_,l) -> (lookuploc store l, store)
          | _ -> raise NoRuleApplies)
  | TmAssign(fi, t1, t2) ->
      if not (isval ctx t1) then
        let (t1',store') = eval1 ctx store t1
        in (TmAssign(fi, t1', t2), store')
      else if not (isval ctx t2) then
        let (t2',store') = eval1 ctx store t2
        in (TmAssign(fi, t1, t2'), store')
      else (match t1 with
            TmLoc(_,l) -> (TmUnit(dummyinfo), updatestore store l t2)
          | _ -> raise NoRuleApplies)
  | TmIf(_, TmTrue(_), t2, t3) -> t2,store
  | TmIf(_, TmFalse(_), t2, t3) -> t3,store
  | TmIf(fi, t1, t2, t3) ->
      let t1',store' = eval1 ctx store t1 in
      TmIf(fi, t1', t2, t3),store'
  | TmSucc(fi, t1) ->
      let t1',store' = eval1 ctx store t1 in
      TmSucc(fi, t1'),store'
  | TmPred(_, TmZero(_)) -> TmZero(dummyinfo),store
  | TmPred(_, TmSucc(_,nv1)) when (isnumericval ctx nv1) -> nv1,store
  | TmPred(fi, t1) ->
      let t1',store' = eval1 ctx store t1 in
      TmPred(fi, t1'),store'
  | TmIsZero(fi, TmZero(_)) -> TmTrue(dummyinfo),store
  | TmIsZero(fi, TmSucc(_,nv1)) when (isnumericval ctx nv1) -> TmFalse(dummyinfo),store
  | TmIsZero(fi, t1) ->
      let t1',store' = eval1 ctx store t1 in
      TmIsZero(fi, t1'),store'
  | _ ->
      raise NoRuleApplies

let rec eval ctx store t =
  try let t',store' = eval1 ctx store t
      in eval ctx store' t'
  with NoRuleApplies -> t,store

let evalbinding ctx store b = match b with
    TmAbbBind(t, tyT) ->
      let t',store' = eval ctx store t in
      TmAbbBind(t', tyT),store'
  | bind -> bind,store

let getTypeFromContext fi ctx i = match getbinding fi ctx i with
    VarBind(tyT) -> tyT
  | TmAbbBind(_, Some(tyT)) -> tyT
  | TmAbbBind(_, None) -> error fi ("No type recorded for variable " ^ (index2name fi ctx i))
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
  | TmTrue(fi) -> TyBool
  | TmFalse(fi) -> TyBool
  | TmIf(fi, t1, t2, t3) ->
      if (=) (typeof ctx t1) TyBool then
        let tyT2 = typeof ctx t2 in
        if (=) tyT2 (typeof ctx t3) then tyT2
        else error fi "arms of conditional have different types"
      else error fi "guard of conditional not a boolean"
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
  | TmLoc(fi, l) -> error fi "locations are not supposed to occur in source programs!"
  | TmRef(fi, t1) -> TyRef(typeof ctx t1)
  | TmDeref(fi, t1) ->
      (match simplifyty ctx (typeof ctx t1) with
          TyRef(tyT1) -> tyT1
        | _ -> error fi "argument of ! is not a Ref or Source")
  | TmAssign(fi, t1, t2) ->
      (match simplifyty ctx (typeof ctx t1) with
          TyRef(tyT1) ->
            if (=) tyT1 (typeof ctx t2) then
              TyUnit
            else
              error fi "arguments of := are incompatible"
        | _ -> error fi "argument of ! is not a Ref")

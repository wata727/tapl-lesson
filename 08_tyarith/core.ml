type ty =
    TyBool
  | TyNat

type term =
    TmTrue
  | TmFalse
  | TmIf of term * term * term
  | TmZero
  | TmSucc of term
  | TmPred of term
  | TmIsZero of term

let rec isnumericval t = match t with
    TmZero -> true
  | TmSucc(t1) -> isnumericval t1
  | _ -> false

let rec isval t = match t with
    TmTrue -> true
  | TmFalse -> true
  | t when isnumericval t -> true
  | _ -> false

exception NoRuleApplies

let rec eval1 t = match t with
    TmIf(TmTrue, t2, t3) ->
      t2
  | TmIf(TmFalse, t2, t3) ->
      t3
  | TmIf(t1, t2, t3) ->
      let t1' = eval1 t1 in
      TmIf(t1', t2, t3)
  | TmSucc(t1) ->
      let t1' = eval1 t1 in
      TmSucc(t1')
  | TmPred(TmZero) ->
      TmZero
  | TmPred(TmSucc(nv1)) when (isnumericval nv1) ->
      nv1
  | TmPred(t1) ->
      let t1' = eval1 t1 in
      TmPred(t1')
  | TmIsZero(TmZero) ->
      TmTrue
  | TmIsZero(TmSucc(nv1)) when (isnumericval nv1) ->
      TmFalse
  | TmIsZero(t1) ->
      let t1' = eval1 t1 in
      TmIsZero(t1')
  | _ ->
      raise NoRuleApplies

let rec eval t =
  try let t' = eval1 t
    in eval t'
  with NoRuleApplies -> t

exception TypeError of string

let rec typeof t = match t with
    TmTrue -> TyBool
  | TmFalse -> TyBool
  | TmIf(t1, t2, t3) ->
      if (=) (typeof t1) TyBool then
        let tyT2 = typeof t2 in
        if (=) tyT2 (typeof t3) then tyT2
        else raise (TypeError "arms of conditional have different types")
      else raise (TypeError "guard of conditional not a boolean")
  | TmZero -> TyNat
  | TmSucc(t1) ->
      if (=) (typeof t1) TyNat then TyNat
      else raise (TypeError "argument of succ is not a number")
  | TmPred(t1) ->
      if (=) (typeof t1) TyNat then TyNat
      else raise (TypeError "argument of pred is not a number")
  | TmIsZero(t1) ->
      if (=) (typeof t1) TyNat then TyBool
      else raise (TypeError "argument of iszero is not a number")

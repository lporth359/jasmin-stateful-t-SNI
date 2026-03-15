open BinNums
open Expr
open Memory_model
open Ssrfun
open Type
open Var0
open Wsize

type fexpr =
| Fconst of coq_Z
| Fvar of var_i
| Fapp1 of sop1 * fexpr
| Fapp2 of sop2 * fexpr * fexpr
| Fif of fexpr * fexpr * fexpr

(** val fconst : wsize -> coq_Z -> fexpr **)

let fconst ws z =
  Fapp1 ((Oword_of_int ws), (Fconst z))

(** val fadd : wsize -> fexpr -> fexpr -> fexpr **)

let fadd ws x x0 =
  Fapp2 ((Oadd (Op_w ws)), x, x0)

(** val faddv : wsize -> var_i -> fexpr -> fexpr **)

let faddv ws x =
  fadd ws (Fvar x)

type rexpr =
| Load of aligned * wsize * fexpr
| Rexpr of fexpr

type lexpr =
| Store of aligned * wsize * fexpr
| LLvar of var_i

(** val fexpr_of_pexpr : pexpr -> fexpr option **)

let rec fexpr_of_pexpr = function
| Pconst z -> Some (Fconst z)
| Pvar g ->
  let { gv = x; gs = gs0 } = g in
  (match gs0 with
   | Slocal -> Some (Fvar x)
   | Sglob -> None)
| Papp1 (op, a) -> Option.map (fun x -> Fapp1 (op, x)) (fexpr_of_pexpr a)
| Papp2 (op, a, b) ->
  Option.bind (fun a0 ->
    Option.map (fun x -> Fapp2 (op, a0, x)) (fexpr_of_pexpr b))
    (fexpr_of_pexpr a)
| Pif (a0, a, b, c) ->
  (match a0 with
   | Coq_abool ->
     Option.bind (fun a1 ->
       Option.bind (fun b0 ->
         Option.map (fun x -> Fif (a1, b0, x)) (fexpr_of_pexpr c))
         (fexpr_of_pexpr b)) (fexpr_of_pexpr a)
   | _ -> None)
| _ -> None

(** val rexpr_of_pexpr : pexpr -> rexpr option **)

let rexpr_of_pexpr e = match e with
| Pload (al, ws, e0) ->
  Option.map (fun x -> Load (al, ws, x)) (fexpr_of_pexpr e0)
| _ -> Option.map (fun x -> Rexpr x) (fexpr_of_pexpr e)

(** val lexpr_of_lval : lval -> lexpr option **)

let lexpr_of_lval = function
| Lvar x -> Some (LLvar x)
| Lmem (al, ws, _, e0) ->
  Option.map (fun x -> Store (al, ws, x)) (fexpr_of_pexpr e0)
| _ -> None

(** val free_vars_rec : SvExtra.Sv.t -> fexpr -> SvExtra.Sv.t **)

let rec free_vars_rec s = function
| Fconst _ -> s
| Fvar x -> SvExtra.Sv.add (Obj.magic x.v_var) s
| Fapp1 (_, f) -> free_vars_rec s f
| Fapp2 (_, f1, f2) -> free_vars_rec (free_vars_rec s f1) f2
| Fif (f1, f2, f3) -> free_vars_rec (free_vars_rec (free_vars_rec s f1) f2) f3

(** val free_vars : fexpr -> SvExtra.Sv.t **)

let free_vars e =
  free_vars_rec SvExtra.Sv.empty e

(** val free_vars_r : rexpr -> SvExtra.Sv.t **)

let free_vars_r = function
| Load (_, _, e) -> free_vars e
| Rexpr e -> free_vars e

(** val rvar : var_i -> rexpr **)

let rvar x =
  Rexpr (Fvar x)

(** val rconst : wsize -> coq_Z -> rexpr **)

let rconst ws z =
  Rexpr (fconst ws z)

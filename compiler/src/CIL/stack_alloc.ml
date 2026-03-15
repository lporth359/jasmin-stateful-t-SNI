open BinInt
open BinNums
open Bool
open Datatypes
open Prelude
open PrimInt63
open Uint0
open Compiler_util
open Eqtype
open Expr
open Gen_map
open Global
open Memory_model
open Pseudo_operator
open Seq
open Slh_lowering
open Slh_ops
open Sopn
open Ssralg
open Ssrbool
open Ssrfun
open Ssrnat
open Stack_alloc_params
open Type
open Utils0
open Var0
open Warray_
open Word0
open Word_ssrZ
open Wsize
open Xseq

type __ = Obj.t
let __ = let rec f _ = Obj.repr f in Obj.repr f

module E =
 struct
  (** val pass : string **)

  let pass =
    "stack allocation"

  (** val stk_error_gen : bool -> var_info -> pp_error -> pp_error_loc **)

  let stk_error_gen internal vi msg =
    { pel_msg = msg; pel_fn = None; pel_fi = None; pel_ii = None; pel_vi =
      (Some vi); pel_pass = (Some pass); pel_internal = internal }

  (** val stk_error : var_i -> pp_error -> pp_error_loc **)

  let stk_error x =
    stk_error_gen false x.v_info

  (** val stk_ierror : var_i -> pp_error -> pp_error_loc **)

  let stk_ierror x =
    stk_error_gen true x.v_info

  (** val stk_ierror_basic : var_i -> string -> pp_error_loc **)

  let stk_ierror_basic x msg =
    stk_ierror x
      (pp_box ((PPEstring
        msg) :: ((pp_nobox ((PPEstring "(") :: ((PPEvar
                   x.v_var) :: ((PPEstring ")") :: [])))) :: [])))

  (** val stk_ierror_basic_lv : lval -> string -> pp_error_loc **)

  let stk_ierror_basic_lv x msg =
    stk_error_gen true (var_info_of_lval x)
      (pp_box ((PPEstring
        msg) :: ((pp_nobox ((PPEstring "(") :: ((PPElval x) :: ((PPEstring
                   ")") :: [])))) :: [])))

  (** val stk_error_no_var_gen : bool -> pp_error -> pp_error_loc **)

  let stk_error_no_var_gen internal msg =
    { pel_msg = msg; pel_fn = None; pel_fi = None; pel_ii = None; pel_vi =
      None; pel_pass = (Some pass); pel_internal = internal }

  (** val stk_error_no_var_box : pp_error -> pp_error_loc **)

  let stk_error_no_var_box =
    stk_error_no_var_gen false

  (** val stk_error_no_var : string -> pp_error_loc **)

  let stk_error_no_var s =
    stk_error_no_var_box (PPEstring s)

  (** val stk_ierror_no_var : string -> pp_error_loc **)

  let stk_ierror_no_var s =
    stk_error_no_var_gen true (PPEstring s)
 end

(** val size_of : atype -> coq_Z **)

let size_of = function
| Coq_aarr (ws, n) -> arr_size ws n
| Coq_aword sz -> wsize_size sz
| _ -> Zpos Coq_xH

type slot = Var.var

type region = { r_slot : slot; r_align : wsize; r_writable : bool }

(** val region_beq : region -> region -> bool **)

let region_beq r1 r2 =
  (&&)
    (eq_op Var.coq_MvMake_var__canonical__eqtype_Equality
      (Obj.magic r1.r_slot) (Obj.magic r2.r_slot))
    ((&&)
      (eq_op wsize_wsize__canonical__eqtype_Equality (Obj.magic r1.r_align)
        (Obj.magic r2.r_align))
      (eq_op coq_Datatypes_bool__canonical__eqtype_Equality
        (Obj.magic r1.r_writable) (Obj.magic r2.r_writable)))

(** val region_same : region -> region -> bool **)

let region_same r1 r2 =
  eq_op Var.coq_MvMake_var__canonical__eqtype_Equality (Obj.magic r1.r_slot)
    (Obj.magic r2.r_slot)

(** val region_axiom : region eq_axiom **)

let region_axiom __top_assumption_ =
  let _evar_0_ = fun xs1 xa1 xw1 __top_assumption_0 ->
    let _evar_0_ = fun xs2 xa2 xw2 ->
      iffP
        ((&&)
          (eq_op Var.coq_MvMake_var__canonical__eqtype_Equality
            (Obj.magic xs1) (Obj.magic xs2))
          ((&&)
            (eq_op wsize_wsize__canonical__eqtype_Equality (Obj.magic xa1)
              (Obj.magic xa2))
            (eq_op coq_Datatypes_bool__canonical__eqtype_Equality
              (Obj.magic xw1) (Obj.magic xw2))))
        (and3P
          (eq_op Var.coq_MvMake_var__canonical__eqtype_Equality
            (Obj.magic xs1) (Obj.magic xs2))
          (eq_op wsize_wsize__canonical__eqtype_Equality (Obj.magic xa1)
            (Obj.magic xa2))
          (eq_op coq_Datatypes_bool__canonical__eqtype_Equality
            (Obj.magic xw1) (Obj.magic xw2)))
    in
    let { r_slot = r_slot0; r_align = r_align0; r_writable = r_writable0 } =
      __top_assumption_0
    in
    _evar_0_ r_slot0 r_align0 r_writable0
  in
  let { r_slot = r_slot0; r_align = r_align0; r_writable = r_writable0 } =
    __top_assumption_
  in
  _evar_0_ r_slot0 r_align0 r_writable0

(** val coq_HB_unnamed_factory_1 : region Coq_hasDecEq.axioms_ **)

let coq_HB_unnamed_factory_1 =
  { Coq_hasDecEq.eq_op = region_beq; Coq_hasDecEq.eqP = region_axiom }

(** val stack_alloc_region__canonical__eqtype_Equality : Equality.coq_type **)

let stack_alloc_region__canonical__eqtype_Equality =
  Obj.magic coq_HB_unnamed_factory_1

module CmpR =
 struct
  (** val t : Equality.coq_type **)

  let t =
    reverse_coercion stack_alloc_region__canonical__eqtype_Equality __

  (** val cmp : Equality.sort -> Equality.sort -> comparison **)

  let cmp r1 r2 =
    match bool_cmp (Obj.magic r1).r_writable (Obj.magic r2).r_writable with
    | Eq ->
      (match wsize_cmp (Obj.magic r1).r_align (Obj.magic r2).r_align with
       | Eq -> Var.var_cmp (Obj.magic r1).r_slot (Obj.magic r2).r_slot
       | x -> x)
    | x -> x
 end

module Mr = Mmake(CmpR)

type sexpr =
| Sconst of coq_Z
| Svar of Var.var
| Sof_int of wsize * sexpr
| Sto_int of signedness * wsize * sexpr
| Sneg of op_kind * sexpr
| Sadd of op_kind * sexpr * sexpr
| Smul of op_kind * sexpr * sexpr
| Ssub of op_kind * sexpr * sexpr

(** val sexpr_beq : sexpr -> sexpr -> bool **)

let rec sexpr_beq e1 e2 =
  match e1 with
  | Sconst n1 ->
    (match e2 with
     | Sconst n2 ->
       eq_op coq_BinNums_Z__canonical__eqtype_Equality (Obj.magic n1)
         (Obj.magic n2)
     | _ -> false)
  | Svar x1 ->
    (match e2 with
     | Svar x2 ->
       eq_op Var.coq_MvMake_var__canonical__eqtype_Equality (Obj.magic x1)
         (Obj.magic x2)
     | _ -> false)
  | Sof_int (ws1, e3) ->
    (match e2 with
     | Sof_int (ws2, e4) ->
       (&&)
         (eq_op wsize_wsize__canonical__eqtype_Equality (Obj.magic ws1)
           (Obj.magic ws2)) (sexpr_beq e3 e4)
     | _ -> false)
  | Sto_int (sg1, ws1, e3) ->
    (match e2 with
     | Sto_int (sg2, ws2, e4) ->
       (&&)
         (eq_op wsize_signedness__canonical__eqtype_Equality (Obj.magic sg1)
           (Obj.magic sg2))
         ((&&)
           (eq_op wsize_wsize__canonical__eqtype_Equality (Obj.magic ws1)
             (Obj.magic ws2)) (sexpr_beq e3 e4))
     | _ -> false)
  | Sneg (opk1, e3) ->
    (match e2 with
     | Sneg (opk2, e4) ->
       (&&)
         (eq_op expr_op_kind__canonical__eqtype_Equality (Obj.magic opk1)
           (Obj.magic opk2)) (sexpr_beq e3 e4)
     | _ -> false)
  | Sadd (opk1, e11, e12) ->
    (match e2 with
     | Sadd (opk2, e21, e22) ->
       (&&)
         (eq_op expr_op_kind__canonical__eqtype_Equality (Obj.magic opk1)
           (Obj.magic opk2)) ((&&) (sexpr_beq e11 e21) (sexpr_beq e12 e22))
     | _ -> false)
  | Smul (opk1, e11, e12) ->
    (match e2 with
     | Smul (opk2, e21, e22) ->
       (&&)
         (eq_op expr_op_kind__canonical__eqtype_Equality (Obj.magic opk1)
           (Obj.magic opk2)) ((&&) (sexpr_beq e11 e21) (sexpr_beq e12 e22))
     | _ -> false)
  | Ssub (opk1, e11, e12) ->
    (match e2 with
     | Ssub (opk2, e21, e22) ->
       (&&)
         (eq_op expr_op_kind__canonical__eqtype_Equality (Obj.magic opk1)
           (Obj.magic opk2)) ((&&) (sexpr_beq e11 e21) (sexpr_beq e12 e22))
     | _ -> false)

(** val sexpr_eq_axiom : sexpr eq_axiom **)

let sexpr_eq_axiom _top_assumption_ =
  let _evar_0_ = fun z1 __top_assumption_ ->
    let _evar_0_ = fun z2 ->
      iffP (eq_op coq_BinNums_Z__canonical__eqtype_Equality z1 z2)
        (eqP coq_BinNums_Z__canonical__eqtype_Equality z1 z2)
    in
    let _evar_0_0 = fun _ -> ReflectF in
    let _evar_0_1 = fun _ _ -> ReflectF in
    let _evar_0_2 = fun _ _ _ -> ReflectF in
    let _evar_0_3 = fun _ _ -> ReflectF in
    let _evar_0_4 = fun _ _ _ -> ReflectF in
    let _evar_0_5 = fun _ _ _ -> ReflectF in
    let _evar_0_6 = fun _ _ _ -> ReflectF in
    (match __top_assumption_ with
     | Sconst z -> Obj.magic _evar_0_ z
     | Svar v -> _evar_0_0 v
     | Sof_int (w, s) -> _evar_0_1 w s
     | Sto_int (s, w, s0) -> _evar_0_2 s w s0
     | Sneg (o, s) -> _evar_0_3 o s
     | Sadd (o, s, s0) -> _evar_0_4 o s s0
     | Smul (o, s, s0) -> _evar_0_5 o s s0
     | Ssub (o, s, s0) -> _evar_0_6 o s s0)
  in
  let _evar_0_0 = fun x1 __top_assumption_ ->
    let _evar_0_0 = fun _ -> ReflectF in
    let _evar_0_1 = fun x2 ->
      iffP (eq_op Var.coq_MvMake_var__canonical__eqtype_Equality x1 x2)
        (eqP Var.coq_MvMake_var__canonical__eqtype_Equality x1 x2)
    in
    let _evar_0_2 = fun _ _ -> ReflectF in
    let _evar_0_3 = fun _ _ _ -> ReflectF in
    let _evar_0_4 = fun _ _ -> ReflectF in
    let _evar_0_5 = fun _ _ _ -> ReflectF in
    let _evar_0_6 = fun _ _ _ -> ReflectF in
    let _evar_0_7 = fun _ _ _ -> ReflectF in
    (match __top_assumption_ with
     | Sconst z -> _evar_0_0 z
     | Svar v -> Obj.magic _evar_0_1 v
     | Sof_int (w, s) -> _evar_0_2 w s
     | Sto_int (s, w, s0) -> _evar_0_3 s w s0
     | Sneg (o, s) -> _evar_0_4 o s
     | Sadd (o, s, s0) -> _evar_0_5 o s s0
     | Smul (o, s, s0) -> _evar_0_6 o s s0
     | Ssub (o, s, s0) -> _evar_0_7 o s s0)
  in
  let _evar_0_1 = fun ws1 e1 _ __top_assumption_ ->
    let _evar_0_1 = fun _ -> ReflectF in
    let _evar_0_2 = fun _ -> ReflectF in
    let _evar_0_3 = fun ws2 e2 ->
      iffP
        ((&&) (eq_op wsize_wsize__canonical__eqtype_Equality ws1 ws2)
          (sexpr_beq e1 e2))
        (andP (eq_op wsize_wsize__canonical__eqtype_Equality ws1 ws2)
          (sexpr_beq e1 e2))
    in
    let _evar_0_4 = fun _ _ _ -> ReflectF in
    let _evar_0_5 = fun _ _ -> ReflectF in
    let _evar_0_6 = fun _ _ _ -> ReflectF in
    let _evar_0_7 = fun _ _ _ -> ReflectF in
    let _evar_0_8 = fun _ _ _ -> ReflectF in
    (match __top_assumption_ with
     | Sconst z -> _evar_0_1 z
     | Svar v -> _evar_0_2 v
     | Sof_int (w, s) -> Obj.magic _evar_0_3 w s
     | Sto_int (s, w, s0) -> _evar_0_4 s w s0
     | Sneg (o, s) -> _evar_0_5 o s
     | Sadd (o, s, s0) -> _evar_0_6 o s s0
     | Smul (o, s, s0) -> _evar_0_7 o s s0
     | Ssub (o, s, s0) -> _evar_0_8 o s s0)
  in
  let _evar_0_2 = fun sg1 ws1 e1 _ __top_assumption_ ->
    let _evar_0_2 = fun _ -> ReflectF in
    let _evar_0_3 = fun _ -> ReflectF in
    let _evar_0_4 = fun _ _ -> ReflectF in
    let _evar_0_5 = fun sg2 ws2 e2 ->
      iffP
        ((&&) (eq_op wsize_signedness__canonical__eqtype_Equality sg1 sg2)
          ((&&) (eq_op wsize_wsize__canonical__eqtype_Equality ws1 ws2)
            (sexpr_beq e1 e2)))
        (and3P (eq_op wsize_signedness__canonical__eqtype_Equality sg1 sg2)
          (eq_op wsize_wsize__canonical__eqtype_Equality ws1 ws2)
          (sexpr_beq e1 e2))
    in
    let _evar_0_6 = fun _ _ -> ReflectF in
    let _evar_0_7 = fun _ _ _ -> ReflectF in
    let _evar_0_8 = fun _ _ _ -> ReflectF in
    let _evar_0_9 = fun _ _ _ -> ReflectF in
    (match __top_assumption_ with
     | Sconst z -> _evar_0_2 z
     | Svar v -> _evar_0_3 v
     | Sof_int (w, s) -> _evar_0_4 w s
     | Sto_int (s, w, s0) -> Obj.magic _evar_0_5 s w s0
     | Sneg (o, s) -> _evar_0_6 o s
     | Sadd (o, s, s0) -> _evar_0_7 o s s0
     | Smul (o, s, s0) -> _evar_0_8 o s s0
     | Ssub (o, s, s0) -> _evar_0_9 o s s0)
  in
  let _evar_0_3 = fun opk1 e1 _ __top_assumption_ ->
    let _evar_0_3 = fun _ -> ReflectF in
    let _evar_0_4 = fun _ -> ReflectF in
    let _evar_0_5 = fun _ _ -> ReflectF in
    let _evar_0_6 = fun _ _ _ -> ReflectF in
    let _evar_0_7 = fun opk2 e2 ->
      iffP
        ((&&) (eq_op expr_op_kind__canonical__eqtype_Equality opk1 opk2)
          (sexpr_beq e1 e2))
        (andP (eq_op expr_op_kind__canonical__eqtype_Equality opk1 opk2)
          (sexpr_beq e1 e2))
    in
    let _evar_0_8 = fun _ _ _ -> ReflectF in
    let _evar_0_9 = fun _ _ _ -> ReflectF in
    let _evar_0_10 = fun _ _ _ -> ReflectF in
    (match __top_assumption_ with
     | Sconst z -> _evar_0_3 z
     | Svar v -> _evar_0_4 v
     | Sof_int (w, s) -> _evar_0_5 w s
     | Sto_int (s, w, s0) -> _evar_0_6 s w s0
     | Sneg (o, s) -> Obj.magic _evar_0_7 o s
     | Sadd (o, s, s0) -> _evar_0_8 o s s0
     | Smul (o, s, s0) -> _evar_0_9 o s s0
     | Ssub (o, s, s0) -> _evar_0_10 o s s0)
  in
  let _evar_0_4 = fun opk1 e11 _ e12 _ __top_assumption_ ->
    let _evar_0_4 = fun _ -> ReflectF in
    let _evar_0_5 = fun _ -> ReflectF in
    let _evar_0_6 = fun _ _ -> ReflectF in
    let _evar_0_7 = fun _ _ _ -> ReflectF in
    let _evar_0_8 = fun _ _ -> ReflectF in
    let _evar_0_9 = fun opk2 e21 e22 ->
      iffP
        ((&&) (eq_op expr_op_kind__canonical__eqtype_Equality opk1 opk2)
          ((&&) (sexpr_beq e11 e21) (sexpr_beq e12 e22)))
        (and3P (eq_op expr_op_kind__canonical__eqtype_Equality opk1 opk2)
          (sexpr_beq e11 e21) (sexpr_beq e12 e22))
    in
    let _evar_0_10 = fun _ _ _ -> ReflectF in
    let _evar_0_11 = fun _ _ _ -> ReflectF in
    (match __top_assumption_ with
     | Sconst z -> _evar_0_4 z
     | Svar v -> _evar_0_5 v
     | Sof_int (w, s) -> _evar_0_6 w s
     | Sto_int (s, w, s0) -> _evar_0_7 s w s0
     | Sneg (o, s) -> _evar_0_8 o s
     | Sadd (o, s, s0) -> Obj.magic _evar_0_9 o s s0
     | Smul (o, s, s0) -> _evar_0_10 o s s0
     | Ssub (o, s, s0) -> _evar_0_11 o s s0)
  in
  let _evar_0_5 = fun opk1 e11 _ e12 _ __top_assumption_ ->
    let _evar_0_5 = fun _ -> ReflectF in
    let _evar_0_6 = fun _ -> ReflectF in
    let _evar_0_7 = fun _ _ -> ReflectF in
    let _evar_0_8 = fun _ _ _ -> ReflectF in
    let _evar_0_9 = fun _ _ -> ReflectF in
    let _evar_0_10 = fun _ _ _ -> ReflectF in
    let _evar_0_11 = fun opk2 e21 e22 ->
      iffP
        ((&&) (eq_op expr_op_kind__canonical__eqtype_Equality opk1 opk2)
          ((&&) (sexpr_beq e11 e21) (sexpr_beq e12 e22)))
        (and3P (eq_op expr_op_kind__canonical__eqtype_Equality opk1 opk2)
          (sexpr_beq e11 e21) (sexpr_beq e12 e22))
    in
    let _evar_0_12 = fun _ _ _ -> ReflectF in
    (match __top_assumption_ with
     | Sconst z -> _evar_0_5 z
     | Svar v -> _evar_0_6 v
     | Sof_int (w, s) -> _evar_0_7 w s
     | Sto_int (s, w, s0) -> _evar_0_8 s w s0
     | Sneg (o, s) -> _evar_0_9 o s
     | Sadd (o, s, s0) -> _evar_0_10 o s s0
     | Smul (o, s, s0) -> Obj.magic _evar_0_11 o s s0
     | Ssub (o, s, s0) -> _evar_0_12 o s s0)
  in
  let _evar_0_6 = fun opk1 e11 _ e12 _ __top_assumption_ ->
    let _evar_0_6 = fun _ -> ReflectF in
    let _evar_0_7 = fun _ -> ReflectF in
    let _evar_0_8 = fun _ _ -> ReflectF in
    let _evar_0_9 = fun _ _ _ -> ReflectF in
    let _evar_0_10 = fun _ _ -> ReflectF in
    let _evar_0_11 = fun _ _ _ -> ReflectF in
    let _evar_0_12 = fun _ _ _ -> ReflectF in
    let _evar_0_13 = fun opk2 e21 e22 ->
      iffP
        ((&&) (eq_op expr_op_kind__canonical__eqtype_Equality opk1 opk2)
          ((&&) (sexpr_beq e11 e21) (sexpr_beq e12 e22)))
        (and3P (eq_op expr_op_kind__canonical__eqtype_Equality opk1 opk2)
          (sexpr_beq e11 e21) (sexpr_beq e12 e22))
    in
    (match __top_assumption_ with
     | Sconst z -> _evar_0_6 z
     | Svar v -> _evar_0_7 v
     | Sof_int (w, s) -> _evar_0_8 w s
     | Sto_int (s, w, s0) -> _evar_0_9 s w s0
     | Sneg (o, s) -> _evar_0_10 o s
     | Sadd (o, s, s0) -> _evar_0_11 o s s0
     | Smul (o, s, s0) -> _evar_0_12 o s s0
     | Ssub (o, s, s0) -> Obj.magic _evar_0_13 o s s0)
  in
  let rec f = function
  | Sconst z -> Obj.magic _evar_0_ z
  | Svar v -> Obj.magic _evar_0_0 v
  | Sof_int (w, s0) -> Obj.magic _evar_0_1 w s0 (f s0)
  | Sto_int (s0, w, s1) -> Obj.magic _evar_0_2 s0 w s1 (f s1)
  | Sneg (o, s0) -> Obj.magic _evar_0_3 o s0 (f s0)
  | Sadd (o, s0, s1) -> Obj.magic _evar_0_4 o s0 (f s0) s1 (f s1)
  | Smul (o, s0, s1) -> Obj.magic _evar_0_5 o s0 (f s0) s1 (f s1)
  | Ssub (o, s0, s1) -> Obj.magic _evar_0_6 o s0 (f s0) s1 (f s1)
  in f _top_assumption_

(** val coq_HB_unnamed_factory_3 : sexpr Coq_hasDecEq.axioms_ **)

let coq_HB_unnamed_factory_3 =
  { Coq_hasDecEq.eq_op = sexpr_beq; Coq_hasDecEq.eqP = sexpr_eq_axiom }

(** val stack_alloc_sexpr__canonical__eqtype_Equality : Equality.coq_type **)

let stack_alloc_sexpr__canonical__eqtype_Equality =
  Obj.magic coq_HB_unnamed_factory_3

(** val is_const : sexpr -> coq_Z option **)

let is_const = function
| Sconst n -> Some n
| _ -> None

type symbolic_slice = { ss_ofs : sexpr; ss_len : sexpr }

(** val symbolic_slice_beq : symbolic_slice -> symbolic_slice -> bool **)

let symbolic_slice_beq s1 s2 =
  (&&)
    (eq_op stack_alloc_sexpr__canonical__eqtype_Equality
      (Obj.magic s1.ss_ofs) (Obj.magic s2.ss_ofs))
    (eq_op stack_alloc_sexpr__canonical__eqtype_Equality
      (Obj.magic s1.ss_len) (Obj.magic s2.ss_len))

(** val symbolic_slice_eq_axiom : symbolic_slice eq_axiom **)

let symbolic_slice_eq_axiom _top_assumption_ =
  let _evar_0_ = fun ofs1 len1 __top_assumption_ ->
    let _evar_0_ = fun ofs2 len2 ->
      iffP
        ((&&) (eq_op stack_alloc_sexpr__canonical__eqtype_Equality ofs1 ofs2)
          (eq_op stack_alloc_sexpr__canonical__eqtype_Equality len1 len2))
        (andP (eq_op stack_alloc_sexpr__canonical__eqtype_Equality ofs1 ofs2)
          (eq_op stack_alloc_sexpr__canonical__eqtype_Equality len1 len2))
    in
    let { ss_ofs = ss_ofs0; ss_len = ss_len0 } = __top_assumption_ in
    Obj.magic _evar_0_ ss_ofs0 ss_len0
  in
  let { ss_ofs = ss_ofs0; ss_len = ss_len0 } = _top_assumption_ in
  Obj.magic _evar_0_ ss_ofs0 ss_len0

(** val coq_HB_unnamed_factory_5 : symbolic_slice Coq_hasDecEq.axioms_ **)

let coq_HB_unnamed_factory_5 =
  { Coq_hasDecEq.eq_op = symbolic_slice_beq; Coq_hasDecEq.eqP =
    symbolic_slice_eq_axiom }

(** val stack_alloc_symbolic_slice__canonical__eqtype_Equality :
    Equality.coq_type **)

let stack_alloc_symbolic_slice__canonical__eqtype_Equality =
  Obj.magic coq_HB_unnamed_factory_5

type symbolic_zone = symbolic_slice list

type sub_region = { sr_region : region; sr_zone : symbolic_zone }

(** val sub_region_beq : sub_region -> sub_region -> bool **)

let sub_region_beq sr1 sr2 =
  (&&)
    (eq_op stack_alloc_region__canonical__eqtype_Equality
      (Obj.magic sr1.sr_region) (Obj.magic sr2.sr_region))
    (eq_op
      (coq_Datatypes_list__canonical__eqtype_Equality
        stack_alloc_symbolic_slice__canonical__eqtype_Equality)
      (Obj.magic sr1.sr_zone) (Obj.magic sr2.sr_zone))

(** val sub_region_eq_axiom : sub_region eq_axiom **)

let sub_region_eq_axiom _top_assumption_ =
  let _evar_0_ = fun r1 z1 __top_assumption_ ->
    let _evar_0_ = fun r2 z2 ->
      iffP
        ((&&) (eq_op stack_alloc_region__canonical__eqtype_Equality r1 r2)
          (eq_op
            (coq_Datatypes_list__canonical__eqtype_Equality
              stack_alloc_symbolic_slice__canonical__eqtype_Equality) z1 z2))
        (andP (eq_op stack_alloc_region__canonical__eqtype_Equality r1 r2)
          (eq_op
            (coq_Datatypes_list__canonical__eqtype_Equality
              stack_alloc_symbolic_slice__canonical__eqtype_Equality) z1 z2))
    in
    let { sr_region = sr_region0; sr_zone = sr_zone0 } = __top_assumption_ in
    Obj.magic _evar_0_ sr_region0 sr_zone0
  in
  let { sr_region = sr_region0; sr_zone = sr_zone0 } = _top_assumption_ in
  Obj.magic _evar_0_ sr_region0 sr_zone0

(** val coq_HB_unnamed_factory_7 : sub_region Coq_hasDecEq.axioms_ **)

let coq_HB_unnamed_factory_7 =
  { Coq_hasDecEq.eq_op = sub_region_beq; Coq_hasDecEq.eqP =
    sub_region_eq_axiom }

(** val stack_alloc_sub_region__canonical__eqtype_Equality :
    Equality.coq_type **)

let stack_alloc_sub_region__canonical__eqtype_Equality =
  Obj.magic coq_HB_unnamed_factory_7

type intervals = symbolic_slice list

(** val symbolic_slice_ble :
    symbolic_slice -> symbolic_slice -> bool option **)

let symbolic_slice_ble s1 s2 =
  match is_const s1.ss_ofs with
  | Some ofs1 ->
    (match is_const s1.ss_len with
     | Some len1 ->
       (match is_const s2.ss_ofs with
        | Some ofs2 -> Some (Z.leb (Z.add ofs1 len1) ofs2)
        | None -> None)
     | None -> None)
  | None -> None

(** val disjoint_slices : symbolic_slice -> symbolic_slice -> bool **)

let disjoint_slices s1 s2 =
  (||) (Ssrfun.Option.default false (symbolic_slice_ble s1 s2))
    (Ssrfun.Option.default false (symbolic_slice_ble s2 s1))

(** val get_sub_interval : intervals -> Equality.sort -> bool **)

let rec get_sub_interval i s =
  match i with
  | [] -> true
  | s' :: i0 ->
    if eq_op stack_alloc_symbolic_slice__canonical__eqtype_Equality s
         (Obj.magic s')
    then false
    else if disjoint_slices (Obj.magic s) s'
         then get_sub_interval i0 s
         else false

(** val add_sub_interval : intervals -> symbolic_slice -> intervals option **)

let rec add_sub_interval i s =
  match i with
  | [] -> Some (s :: [])
  | s' :: i' ->
    if eq_op stack_alloc_symbolic_slice__canonical__eqtype_Equality
         (Obj.magic s) (Obj.magic s')
    then Some i
    else if Ssrfun.Option.default false (symbolic_slice_ble s s')
         then Some (s :: i)
         else if Ssrfun.Option.default false (symbolic_slice_ble s' s)
              then (match add_sub_interval i' s with
                    | Some i'' -> Some (s' :: i'')
                    | None -> None)
              else None

(** val remove_sub_interval : intervals -> Equality.sort -> intervals **)

let rec remove_sub_interval i s =
  match i with
  | [] -> []
  | s' :: i' ->
    if eq_op stack_alloc_symbolic_slice__canonical__eqtype_Equality s
         (Obj.magic s')
    then i'
    else (match is_const (Obj.magic s).ss_ofs with
          | Some ofs ->
            (match is_const (Obj.magic s).ss_len with
             | Some len ->
               (match is_const s'.ss_ofs with
                | Some ofs' ->
                  (match is_const s'.ss_len with
                   | Some len' ->
                     if (&&) (Z.leb ofs ofs')
                          (Z.leb (Z.add ofs' len') (Z.add ofs len))
                     then remove_sub_interval i' s
                     else s' :: (remove_sub_interval i' s)
                   | None -> s' :: (remove_sub_interval i' s))
                | None -> s' :: (remove_sub_interval i' s))
             | None -> s' :: (remove_sub_interval i' s))
          | None -> s' :: (remove_sub_interval i' s))

type status =
| Valid
| Unknown
| Borrowed of intervals

type status_map = status Mvar.t

type region_map = { var_region : sub_region Mvar.t;
                    region_var : status_map Mr.t }

(** val empty_status_map : status Mvar.t **)

let empty_status_map =
  Mvar.empty

(** val empty : region_map **)

let empty =
  { var_region = Mvar.empty; region_var = Mr.empty }

(** val get_sub_region :
    region_map -> var_i -> (pp_error_loc, sub_region) result **)

let get_sub_region rmap x =
  match Mvar.get rmap.var_region (Obj.magic x.v_var) with
  | Some sr -> Ok sr
  | None ->
    Error
      (E.stk_error x
        (pp_box ((PPEstring "no region associated to variable") :: ((PPEvar
          x.v_var) :: []))))

(** val get_status_map : status Mvar.t Mr.t -> region -> status_map **)

let get_status_map rv r =
  Ssrfun.Option.default empty_status_map (Mr.get rv (Obj.magic r))

(** val get_status : status_map -> Equality.sort -> status **)

let get_status sm x =
  Ssrfun.Option.default Unknown (Mvar.get sm x)

(** val get_var_status :
    status Mvar.t Mr.t -> region -> Equality.sort -> status **)

let get_var_status rv r x =
  let sm = get_status_map rv r in get_status sm x

type concrete_slice = { cs_ofs : coq_Z; cs_len : coq_Z }

type ptr_kind_init =
| PIdirect of Var.var * concrete_slice * v_scope
| PIregptr of Var.var
| PIstkptr of Var.var * concrete_slice * Var.var

type ptr_kind =
| Pdirect of Var.var * coq_Z * wsize * concrete_slice * v_scope
| Pregptr of Var.var
| Pstkptr of Var.var * coq_Z * wsize * concrete_slice * Var.var

type vptr_kind =
| VKglob of (coq_Z * wsize)
| VKptr of ptr_kind

type pos_map = { vrip : Var.var; vrsp : Var.var; vxlen : Var.var;
                 globals : (coq_Z * wsize) Mvar.t; locals : ptr_kind Mvar.t;
                 vnew : SvExtra.Sv.t }

type param_info = { pp_ptr : Var.var; pp_writable : bool; pp_align : wsize }

(** val divide_z : coq_Z -> wsize -> bool **)

let divide_z z ws =
  eq_op coq_BinNums_Z__canonical__eqtype_Equality
    (Obj.magic Z.coq_land z (Z.sub (wsize_size ws) (Zpos Coq_xH)))
    (Obj.magic Z0)

(** val divide : sexpr -> wsize -> bool **)

let divide e ws =
  match e with
  | Sconst z -> divide_z z ws
  | Smul (_, e1, e2) ->
    (||) (match is_const e1 with
          | Some z1 -> divide_z z1 ws
          | None -> false)
      (match is_const e2 with
       | Some z2 -> divide_z z2 ws
       | None -> false)
  | _ -> false

(** val divide_zone : symbolic_slice list -> wsize -> bool **)

let rec divide_zone z ws =
  match z with
  | [] -> true
  | s :: z0 -> (&&) (divide s.ss_ofs ws) (divide_zone z0 ws)

(** val check_align :
    Equality.sort -> var_i -> sub_region -> wsize -> (pp_error_loc, unit)
    result **)

let check_align al x sr ws =
  if (||)
       (eq_op memory_model_aligned__canonical__eqtype_Equality al
         (Obj.magic Unaligned)) (cmp_le wsize_cmp ws sr.sr_region.r_align)
  then if (||)
            (eq_op memory_model_aligned__canonical__eqtype_Equality al
              (Obj.magic Unaligned)) (divide_zone sr.sr_zone ws)
       then Ok ()
       else Error
              (E.stk_error x
                (pp_hov ((PPEstring "the access to array") :: ((PPEvar
                  x.v_var) :: ((PPEstring
                  "could not be proved to be aligned;") :: ((PPEstring
                  "if you know what you are doing or want to perform an unaligned access,") :: ((PPEstring
                  "you can use \226\128\156#unaligned\226\128\157") :: [])))))))
  else let s = E.stk_ierror_basic x "unaligned offset" in Error s

(** val check_writable : var_i -> region -> (pp_error_loc, unit) result **)

let check_writable x r =
  if r.r_writable
  then Ok ()
  else Error
         (E.stk_error x
           (pp_box ((PPEstring
             "cannot write to the constant pointer") :: ((PPEvar
             x.v_var) :: ((PPEstring "targetting") :: ((PPEvar
             r.r_slot) :: []))))))

(** val is_valid : status -> bool **)

let is_valid = function
| Valid -> true
| _ -> false

(** val check_valid : var_i -> status -> (pp_error_loc, unit) result **)

let check_valid x status0 =
  if is_valid status0
  then Ok ()
  else Error
         (E.stk_error x
           (pp_box ((PPEstring
             "the region associated to variable") :: ((PPEvar
             x.v_var) :: ((PPEstring "is partial") :: [])))))

(** val split_last :
    symbolic_slice list -> symbolic_slice list * symbolic_slice **)

let rec split_last = function
| [] -> ([], { ss_ofs = (Sconst Z0); ss_len = (Sconst Z0) })
| s :: z0 ->
  (match z0 with
   | [] -> ([], s)
   | _ :: _ -> let (z1, last) = split_last z0 in ((s :: z1), last))

(** val sub_zone_at_ofs :
    symbolic_zone -> sexpr -> sexpr -> symbolic_slice list **)

let sub_zone_at_ofs z ofs len =
  let (z', s) = split_last z in
  (match is_const s.ss_ofs with
   | Some sofs ->
     (match is_const s.ss_len with
      | Some _ ->
        (match is_const ofs with
         | Some ofs0 ->
           (match is_const len with
            | Some len0 ->
              cat z' ({ ss_ofs = (Sconst (Z.add sofs ofs0)); ss_len = (Sconst
                len0) } :: [])
            | None -> cat z ({ ss_ofs = ofs; ss_len = len } :: []))
         | None -> cat z ({ ss_ofs = ofs; ss_len = len } :: []))
      | None -> cat z ({ ss_ofs = ofs; ss_len = len } :: []))
   | None -> cat z ({ ss_ofs = ofs; ss_len = len } :: []))

(** val sub_region_at_ofs : sub_region -> sexpr -> sexpr -> sub_region **)

let sub_region_at_ofs sr ofs len =
  { sr_region = sr.sr_region; sr_zone = (sub_zone_at_ofs sr.sr_zone ofs len) }

(** val get_sub_status : status -> Equality.sort -> bool **)

let get_sub_status status0 s =
  match status0 with
  | Valid -> true
  | Unknown -> false
  | Borrowed i -> get_sub_interval i s

(** val sub_region_status_at_ofs :
    var_i -> sub_region -> status -> Equality.sort -> Equality.sort ->
    sub_region * status **)

let sub_region_status_at_ofs x sr status0 ofs len =
  if (&&)
       (eq_op stack_alloc_sexpr__canonical__eqtype_Equality ofs
         (Obj.magic (Sconst Z0)))
       (eq_op stack_alloc_sexpr__canonical__eqtype_Equality len
         (Obj.magic (Sconst (size_of (Var.vtype x.v_var)))))
  then (sr, status0)
  else let sr0 = sub_region_at_ofs sr (Obj.magic ofs) (Obj.magic len) in
       let status1 =
         if get_sub_status status0
              (Obj.magic { ss_ofs = (Obj.magic ofs); ss_len =
                (Obj.magic len) })
         then Valid
         else Unknown
       in
       (sr0, status1)

(** val get_suffix :
    symbolic_zone -> symbolic_zone -> symbolic_zone option option **)

let rec get_suffix z1 z2 =
  match z1 with
  | [] -> Some (Some z2)
  | s1 :: z3 ->
    (match z2 with
     | [] -> None
     | s2 :: z4 ->
       if eq_op stack_alloc_symbolic_slice__canonical__eqtype_Equality
            (Obj.magic s1) (Obj.magic s2)
       then get_suffix z3 z4
       else if disjoint_slices s1 s2
            then Some None
            else (match z3 with
                  | [] ->
                    (match is_const s1.ss_ofs with
                     | Some ofs1 ->
                       (match is_const s1.ss_len with
                        | Some len1 ->
                          (match is_const s2.ss_ofs with
                           | Some ofs2 ->
                             (match is_const s2.ss_len with
                              | Some len2 ->
                                let m = Z.max ofs1 ofs2 in
                                let ofs = Z.sub m ofs1 in
                                let len =
                                  Z.sub
                                    (Z.min (Z.add ofs1 len1)
                                      (Z.add ofs2 len2)) m
                                in
                                if (&&)
                                     (eq_op
                                       coq_BinNums_Z__canonical__eqtype_Equality
                                       (Obj.magic ofs) (Obj.magic Z0))
                                     (eq_op
                                       coq_BinNums_Z__canonical__eqtype_Equality
                                       (Obj.magic len) (Obj.magic len1))
                                then Some (Some [])
                                else Some (Some ({ ss_ofs = (Sconst ofs);
                                       ss_len = (Sconst len) } :: []))
                              | None -> None)
                           | None -> None)
                        | None -> None)
                     | None -> None)
                  | _ :: _ -> None))

(** val fill_status : status -> symbolic_slice -> status **)

let fill_status status0 s =
  match status0 with
  | Borrowed i ->
    let i0 = remove_sub_interval i (Obj.magic s) in
    (match i0 with
     | [] -> Valid
     | _ :: _ -> Borrowed i0)
  | x -> x

(** val clear_status : status -> symbolic_zone -> status option **)

let clear_status status0 = function
| [] -> None
| s :: _ ->
  (match status0 with
   | Valid -> Some (Borrowed (s :: []))
   | Unknown -> None
   | Borrowed i ->
     (match add_sub_interval i s with
      | Some i0 -> Some (Borrowed i0)
      | None -> None))

(** val clear_status_map_aux :
    region_map -> symbolic_zone -> Equality.sort -> status -> status option **)

let clear_status_map_aux rmap z x status0 =
  match match Mvar.get rmap.var_region x with
        | Some sr -> get_suffix sr.sr_zone z
        | None -> None with
  | Some z0 ->
    (match z0 with
     | Some z1 -> clear_status status0 z1
     | None -> Some status0)
  | None -> None

(** val clear_status_map :
    region_map -> symbolic_zone -> status_map -> status Mvar.t **)

let clear_status_map rmap z sm =
  Mvar.filter_map (clear_status_map_aux rmap z) sm

(** val set_clear_status : region_map -> sub_region -> status_map Mr.Map.t **)

let set_clear_status rmap sr =
  let sm = get_status_map rmap.region_var sr.sr_region in
  let sm0 = clear_status_map rmap sr.sr_zone sm in
  Mr.set rmap.region_var (Obj.magic sr.sr_region) sm0

(** val set_clear_pure : region_map -> sub_region -> region_map **)

let set_clear_pure rmap sr =
  { var_region = rmap.var_region; region_var = (set_clear_status rmap sr) }

(** val set_clear :
    region_map -> var_i -> sub_region -> (pp_error_loc, region_map) result **)

let set_clear rmap x sr =
  match check_writable x sr.sr_region with
  | Ok _ -> Ok (set_clear_pure rmap sr)
  | Error s -> Error s

(** val is_unknown : status -> bool **)

let is_unknown = function
| Unknown -> true
| _ -> false

(** val set_status :
    status Mvar.t -> Equality.sort -> status -> status Mvar.Map.t **)

let set_status sm x status0 =
  if is_unknown status0 then Mvar.remove sm x else Mvar.set sm x status0

(** val set_word_status :
    region_map -> sub_region -> Equality.sort -> status -> status_map Mr.Map.t **)

let set_word_status rmap sr x status0 =
  let sm = get_status_map rmap.region_var sr.sr_region in
  let sm0 = clear_status_map rmap sr.sr_zone sm in
  let sm1 = set_status sm0 x status0 in
  Mr.set rmap.region_var (Obj.magic sr.sr_region) sm1

(** val set_word_pure :
    region_map -> sub_region -> Equality.sort -> status -> region_map **)

let set_word_pure rmap sr x status0 =
  { var_region = rmap.var_region; region_var =
    (set_word_status rmap sr x status0) }

(** val set_word :
    region_map -> Equality.sort -> sub_region -> var_i -> status -> wsize ->
    (pp_error_loc, region_map) result **)

let set_word rmap al sr x status0 ws =
  match check_writable x sr.sr_region with
  | Ok _ ->
    (match check_align al x sr ws with
     | Ok _ -> Ok (set_word_pure rmap sr (Obj.magic x.v_var) status0)
     | Error s -> Error s)
  | Error s -> Error s

(** val set_move_status :
    status Mvar.t Mr.t -> Equality.sort -> region -> status -> status Mvar.t
    Mr.Map.t **)

let set_move_status rv x r status0 =
  let sm = get_status_map rv r in
  let sm0 = set_status sm x status0 in Mr.set rv (Obj.magic r) sm0

(** val set_move :
    region_map -> Equality.sort -> sub_region -> status -> region_map **)

let set_move rmap x sr status0 =
  let rv = set_move_status rmap.region_var x sr.sr_region status0 in
  { var_region = (Mvar.set rmap.var_region x sr); region_var = rv }

(** val insert_status :
    Var.var -> status -> Equality.sort -> Equality.sort -> status -> status **)

let insert_status x status0 ofs len statusy =
  if (&&)
       (eq_op stack_alloc_sexpr__canonical__eqtype_Equality ofs
         (Obj.magic (Sconst Z0)))
       (eq_op stack_alloc_sexpr__canonical__eqtype_Equality len
         (Obj.magic (Sconst (size_of (Var.vtype x)))))
  then statusy
  else let s = { ss_ofs = (Obj.magic ofs); ss_len = (Obj.magic len) } in
       if get_sub_status statusy
            (Obj.magic { ss_ofs = (Sconst Z0); ss_len = (Obj.magic len) })
       then fill_status status0 s
       else Ssrfun.Option.default Unknown (clear_status status0 (s :: []))

(** val set_move_sub :
    region_map -> region -> Equality.sort -> status -> Equality.sort ->
    Equality.sort -> status -> region_map **)

let set_move_sub rmap r x status0 ofs len substatus =
  let rv =
    set_move_status rmap.region_var x r
      (insert_status (Obj.magic x) status0 ofs len substatus)
  in
  { var_region = rmap.var_region; region_var = rv }

(** val zone_of_cs : concrete_slice -> symbolic_zone **)

let zone_of_cs cs =
  { ss_ofs = (Sconst cs.cs_ofs); ss_len = (Sconst cs.cs_len) } :: []

(** val sub_region_stkptr : slot -> wsize -> concrete_slice -> sub_region **)

let sub_region_stkptr s ws cs =
  let r = { r_slot = s; r_align = ws; r_writable = true } in
  let z = zone_of_cs cs in { sr_region = r; sr_zone = z }

(** val set_stack_ptr :
    region_map -> slot -> wsize -> concrete_slice -> Var.var -> region_map **)

let set_stack_ptr rmap s ws cs x' =
  let sr = sub_region_stkptr s ws cs in
  set_word_pure rmap sr (Obj.magic x') Valid

(** val check_stack_ptr :
    status Mvar.t Mr.t -> slot -> wsize -> concrete_slice -> Equality.sort ->
    bool **)

let check_stack_ptr rv s ws cs x' =
  let sr = sub_region_stkptr s ws cs in
  let status0 = get_var_status rv sr.sr_region x' in is_valid status0

(** val sub_region_full : Var.var -> region -> sub_region **)

let sub_region_full x r =
  let z = { ss_ofs = (Sconst Z0); ss_len = (Sconst
    (size_of (Var.vtype x))) } :: []
  in
  { sr_region = r; sr_zone = z }

(** val sub_region_glob : slot -> wsize -> sub_region **)

let sub_region_glob x ws =
  let r = { r_slot = x; r_align = ws; r_writable = false } in
  sub_region_full x r

(** val get_sub_region_status :
    region_map -> var_i -> (pp_error_loc, sub_region * status) result **)

let get_sub_region_status rmap x =
  match get_sub_region rmap x with
  | Ok x0 ->
    let status0 =
      get_var_status rmap.region_var x0.sr_region (Obj.magic x.v_var)
    in
    Ok (x0, status0)
  | Error s -> Error s

(** val get_gsub_region_status :
    region_map -> var_i -> vptr_kind -> (pp_error_loc, sub_region * status)
    result **)

let get_gsub_region_status rmap x = function
| VKglob p ->
  let (_, ws) = p in let sr = sub_region_glob x.v_var ws in Ok (sr, Valid)
| VKptr _ -> get_sub_region_status rmap x

type table = { bindings : sexpr Mvar.t; counter : Uint63.t;
               vars : SvExtra.Sv.t }

(** val table_fresh_var :
    (Var.var -> Uint63.t -> Var.var) -> table -> Var.var -> (pp_error_loc,
    table * sexpr) result **)

let table_fresh_var clone0 t0 x =
  let x' = clone0 x t0.counter in
  if negb (SvExtra.Sv.mem (Obj.magic x') t0.vars)
  then let e = Svar x' in
       let t1 = { bindings = (Mvar.set t0.bindings (Obj.magic x) e);
         counter = (succ t0.counter); vars =
         (SvExtra.Sv.add (Obj.magic x') t0.vars) }
       in
       Ok (t1, e)
  else let s = E.stk_ierror_no_var "table_fresh_var: variable not fresh" in
       Error s

(** val table_get_var :
    (Var.var -> Uint63.t -> Var.var) -> table -> Equality.sort ->
    (pp_error_loc, table * sexpr) result **)

let table_get_var clone0 t0 x =
  match Mvar.get t0.bindings x with
  | Some e -> Ok (t0, e)
  | None -> table_fresh_var clone0 t0 (Obj.magic x)

(** val merge_table : table -> table -> table **)

let merge_table t1 t2 =
  let b =
    Mvar.map2 (fun _ osp1 osp2 ->
      match osp1 with
      | Some sp1 ->
        (match osp2 with
         | Some sp2 ->
           if eq_op stack_alloc_sexpr__canonical__eqtype_Equality
                (Obj.magic sp1) (Obj.magic sp2)
           then osp1
           else None
         | None -> None)
      | None -> None) t1.bindings t2.bindings
  in
  let n = if leb t1.counter t2.counter then t1.counter else t2.counter in
  let vars0 = SvExtra.Sv.inter t1.vars t2.vars in
  { bindings = b; counter = n; vars = vars0 }

(** val symbolic_of_pexpr :
    (Var.var -> Uint63.t -> Var.var) -> table -> pexpr -> (table * sexpr)
    option cexec **)

let rec symbolic_of_pexpr clone0 t0 = function
| Pconst n -> Ok (Some (t0, (Sconst n)))
| Pvar x ->
  if is_lvar x
  then (match table_get_var clone0 t0 (Obj.magic x.gv.v_var) with
        | Ok x0 -> Ok (Some x0)
        | Error s -> Error s)
  else Ok None
| Papp1 (op, e0) ->
  (match op with
   | Oword_of_int ws ->
     let op0 = fun x -> Sof_int (ws, x) in
     (match symbolic_of_pexpr clone0 t0 e0 with
      | Ok x ->
        (match x with
         | Some y -> let (t1, e1) = y in Ok (Some (t1, (op0 e1)))
         | None -> Ok None)
      | Error s -> Error s)
   | Oint_of_word (sg, ws) ->
     let op0 = fun x -> Sto_int (sg, ws, x) in
     (match symbolic_of_pexpr clone0 t0 e0 with
      | Ok x ->
        (match x with
         | Some y -> let (t1, e1) = y in Ok (Some (t1, (op0 e1)))
         | None -> Ok None)
      | Error s -> Error s)
   | Oneg opk ->
     let op0 = fun x -> Sneg (opk, x) in
     (match symbolic_of_pexpr clone0 t0 e0 with
      | Ok x ->
        (match x with
         | Some y -> let (t1, e1) = y in Ok (Some (t1, (op0 e1)))
         | None -> Ok None)
      | Error s -> Error s)
   | _ -> Ok None)
| Papp2 (op, e1, e2) ->
  (match op with
   | Obeq -> Ok None
   | Oand -> Ok None
   | Oor -> Ok None
   | Oadd opk ->
     let op0 = fun x x0 -> Sadd (opk, x, x0) in
     (match symbolic_of_pexpr clone0 t0 e1 with
      | Ok x ->
        (match x with
         | Some y ->
           let (t1, e3) = y in
           (match symbolic_of_pexpr clone0 t1 e2 with
            | Ok x0 ->
              (match x0 with
               | Some y0 -> let (t2, e4) = y0 in Ok (Some (t2, (op0 e3 e4)))
               | None -> Ok None)
            | Error s -> Error s)
         | None -> Ok None)
      | Error s -> Error s)
   | Omul opk ->
     let op0 = fun x x0 -> Smul (opk, x, x0) in
     (match symbolic_of_pexpr clone0 t0 e1 with
      | Ok x ->
        (match x with
         | Some y ->
           let (t1, e3) = y in
           (match symbolic_of_pexpr clone0 t1 e2 with
            | Ok x0 ->
              (match x0 with
               | Some y0 -> let (t2, e4) = y0 in Ok (Some (t2, (op0 e3 e4)))
               | None -> Ok None)
            | Error s -> Error s)
         | None -> Ok None)
      | Error s -> Error s)
   | Osub opk ->
     let op0 = fun x x0 -> Ssub (opk, x, x0) in
     (match symbolic_of_pexpr clone0 t0 e1 with
      | Ok x ->
        (match x with
         | Some y ->
           let (t1, e3) = y in
           (match symbolic_of_pexpr clone0 t1 e2 with
            | Ok x0 ->
              (match x0 with
               | Some y0 -> let (t2, e4) = y0 in Ok (Some (t2, (op0 e3 e4)))
               | None -> Ok None)
            | Error s -> Error s)
         | None -> Ok None)
      | Error s -> Error s)
   | _ -> Ok None)
| _ -> Ok None

(** val get_symbolic_of_pexpr :
    (Var.var -> Uint63.t -> Var.var) -> table -> pexpr -> (pp_error_loc,
    table * sexpr) result **)

let get_symbolic_of_pexpr clone0 t0 e =
  match symbolic_of_pexpr clone0 t0 e with
  | Ok x ->
    let err =
      E.stk_error_no_var_box
        (pp_hov ((PPEstring
          "do not know how to compile this array access into a memory access,") :: ((PPEstring
          "the expression") :: ((PPEexpr e) :: ((PPEstring
          "must contain only the operators \226\128\156+\226\128\157 and \226\128\156*\226\128\157") :: [])))))
    in
    o2r err x
  | Error s -> Error s

(** val remove_binding : table -> Equality.sort -> table **)

let remove_binding table0 x =
  { bindings = (Mvar.remove table0.bindings x); counter = table0.counter;
    vars = table0.vars }

(** val remove_binding_lval : table -> lval -> table **)

let remove_binding_lval table0 = function
| Lvar x -> remove_binding table0 (Obj.magic x.v_var)
| Laset (_, _, _, x, _) -> remove_binding table0 (Obj.magic x.v_var)
| Lasub (_, _, _, x, _) -> remove_binding table0 (Obj.magic x.v_var)
| _ -> table0

(** val remove_binding_lvals : table -> lval list -> table **)

let remove_binding_lvals =
  foldl remove_binding_lval

(** val table_set_var : table -> Equality.sort -> sexpr -> table **)

let table_set_var t0 x e =
  { bindings = (Mvar.set t0.bindings x e); counter = t0.counter; vars =
    t0.vars }

(** val update_table : table -> lval -> sexpr option -> table **)

let update_table table0 lv = function
| Some e ->
  (match lv with
   | Lvar x -> table_set_var table0 (Obj.magic x.v_var) e
   | _ -> table0)
| None -> table0

(** val assert_check : bool -> bool -> 'a1 -> ('a1, unit) result **)

let assert_check check b e =
  if check then if b then Ok () else Error e else Ok ()

(** val clone :
    (v_kind -> Uint63.t -> string -> atype -> Ident.Ident.ident) -> Var.var
    -> Uint63.t -> Var.var **)

let clone fresh_var_ident x n =
  let xn =
    fresh_var_ident (Ident.Ident.id_kind (Var.vname x)) n
      (Ident.Ident.id_name (Var.vname x)) (Var.vtype x)
  in
  { Var.vtype = (Var.vtype x); Var.vname = xn }

(** val mul : coq_PointerData -> pexpr -> pexpr -> pexpr **)

let mul pd x x0 =
  Papp2 ((Omul (Op_w pd)), x, x0)

(** val mk_ofs :
    coq_PointerData -> arr_access -> wsize -> pexpr -> coq_Z -> pexpr **)

let mk_ofs pd aa ws e1 ofs =
  let sz = mk_scale aa ws in
  (match Expr.is_const e1 with
   | Some i -> cast_const pd (Z.add (Z.mul i sz) ofs)
   | None ->
     add pd (mul pd (cast_const pd sz) (cast_ptr pd e1)) (cast_const pd ofs))

(** val mk_ofs_int : arr_access -> wsize -> sexpr -> sexpr **)

let mk_ofs_int aa ws e1 =
  let sz = mk_scale aa ws in
  (match is_const e1 with
   | Some i -> Sconst (Z.mul i sz)
   | None -> Smul (Op_int, (Sconst sz), e1))

(** val get_global :
    pos_map -> var_i -> (pp_error_loc, coq_Z * wsize) result **)

let get_global pmap x =
  match Mvar.get pmap.globals (Obj.magic x.v_var) with
  | Some z -> Ok z
  | None -> Error (E.stk_ierror_basic x "unallocated global variable")

(** val get_local : pos_map -> Var.var -> ptr_kind option **)

let get_local pmap x =
  Mvar.get pmap.locals (Obj.magic x)

(** val get_var_kind :
    pos_map -> gvar -> (pp_error_loc, vptr_kind option) result **)

let get_var_kind pmap x =
  let xv = x.gv in
  if is_glob x
  then (match get_global pmap xv with
        | Ok x0 -> Ok (Some (VKglob x0))
        | Error s -> Error s)
  else Ok (Ssrfun.Option.map (fun x0 -> VKptr x0) (get_local pmap xv.v_var))

(** val check_diff : pos_map -> var_i -> (pp_error_loc, unit) result **)

let check_diff pmap x =
  if SvExtra.Sv.mem (Obj.magic x.v_var) pmap.vnew
  then Error
         (E.stk_ierror_basic x "the code writes to one of the new variables")
  else Ok ()

(** val check_var : pos_map -> var_i -> (pp_error_loc, unit) result **)

let check_var pmap x =
  match get_local pmap x.v_var with
  | Some _ ->
    Error
      (E.stk_error x
        (pp_box ((PPEvar x.v_var) :: ((PPEstring
          "is a stack variable, but a reg variable is expected") :: []))))
  | None -> Ok ()

(** val with_var : var_i -> Var.var -> var_i **)

let with_var xi x =
  { v_var = x; v_info = xi.v_info }

(** val base_ptr : pos_map -> v_scope -> Var.var **)

let base_ptr pmap = function
| Slocal -> pmap.vrsp
| Sglob -> pmap.vrip

(** val addr_from_pk :
    pos_map -> var_i -> ptr_kind -> (pp_error_loc, var_i * coq_Z) result **)

let addr_from_pk pmap x = function
| Pdirect (_, ofs, _, cs, sc) ->
  Ok ((with_var x (base_ptr pmap sc)), (Z.add ofs cs.cs_ofs))
| Pregptr p -> Ok ((with_var x p), Z0)
| Pstkptr (_, _, _, _, _) ->
  Error
    (E.stk_error x
      (pp_box ((PPEvar x.v_var) :: ((PPEstring
        "is a stack ptr, it should not appear in a sub-expression") :: []))))

(** val addr_from_vpk :
    pos_map -> var_i -> vptr_kind -> (pp_error_loc, var_i * coq_Z) result **)

let addr_from_vpk pmap x = function
| VKglob zws -> Ok ((with_var x pmap.vrip), (fst zws))
| VKptr pk -> addr_from_pk pmap x pk

(** val bad_arg_number : pp_error_loc **)

let bad_arg_number =
  E.stk_ierror_no_var "invalid number of args"

(** val not_trivially_incorrect :
    arr_access -> wsize -> pexpr -> coq_Z -> bool **)

let not_trivially_incorrect aa ws ofs len =
  match Expr.is_const ofs with
  | Some i ->
    (&&) (Z.leb Z0 (Z.mul i (mk_scale aa ws)))
      (Z.leb (Z.add (Z.mul i (mk_scale aa ws)) (wsize_size ws)) len)
  | None -> true

(** val alloc_e :
    coq_PointerData -> pos_map -> region_map -> pexpr -> atype ->
    (pp_error_loc, pexpr) result **)

let rec alloc_e pd pmap rmap e ty =
  match e with
  | Pvar x ->
    let xv = x.gv in
    (match get_var_kind pmap x with
     | Ok x0 ->
       (match x0 with
        | Some vpk ->
          (match OtherDefs.is_word_type ty with
           | Some ws ->
             if subatype (Coq_aword ws) (Var.vtype xv.v_var)
             then (match get_gsub_region_status rmap xv vpk with
                   | Ok x1 ->
                     let (sr, status0) = x1 in
                     (match check_valid xv status0 with
                      | Ok _ ->
                        (match check_align (Obj.magic Aligned) xv sr ws with
                         | Ok _ ->
                           (match addr_from_vpk pmap xv vpk with
                            | Ok x2 ->
                              let (p, ofs) = x2 in
                              Ok (Pload (Aligned, ws,
                              (add pd (coq_Plvar p) (cast_const pd ofs))))
                            | Error s -> Error s)
                         | Error s -> Error s)
                      | Error s -> Error s)
                   | Error s -> Error s)
             else Error (E.stk_ierror_basic xv "invalid type for expression")
           | None ->
             Error (E.stk_ierror_basic xv "not a word variable in expression"))
        | None ->
          (match check_diff pmap xv with
           | Ok _ -> Ok e
           | Error s -> Error s))
     | Error s -> Error s)
  | Pget (al, aa, ws, x, e1) ->
    let xv = x.gv in
    if not_trivially_incorrect aa ws e1 (size_of (Var.vtype xv.v_var))
    then (match alloc_e pd pmap rmap e1 Coq_aint with
          | Ok x0 ->
            (match get_var_kind pmap x with
             | Ok x1 ->
               (match x1 with
                | Some vpk ->
                  (match get_gsub_region_status rmap xv vpk with
                   | Ok x2 ->
                     let (sr, status0) = x2 in
                     (match check_valid xv status0 with
                      | Ok _ ->
                        (match check_align (Obj.magic al) xv sr ws with
                         | Ok _ ->
                           (match addr_from_vpk pmap xv vpk with
                            | Ok x3 ->
                              let (p, ofs) = x3 in
                              let ofs0 = mk_ofs pd aa ws x0 ofs in
                              Ok (Pload (al, ws, (add pd (coq_Plvar p) ofs0)))
                            | Error s -> Error s)
                         | Error s -> Error s)
                      | Error s -> Error s)
                   | Error s -> Error s)
                | None ->
                  Error (E.stk_ierror_basic xv "register array remains"))
             | Error s -> Error s)
          | Error s -> Error s)
    else let s = E.stk_error_no_var "this read is trivially out-of-bounds" in
         Error s
  | Psub (_, _, _, x, _) -> Error (E.stk_ierror_basic x.gv "Psub")
  | Pload (al, ws, e1) ->
    (match alloc_e pd pmap rmap e1 (Coq_aword pd) with
     | Ok x -> Ok (Pload (al, ws, x))
     | Error s -> Error s)
  | Papp1 (o, e1) ->
    (match alloc_e pd pmap rmap e1 (fst (type_of_op1 o)) with
     | Ok x -> Ok (Papp1 (o, x))
     | Error s -> Error s)
  | Papp2 (o, e1, e2) ->
    let tys = type_of_op2 o in
    (match alloc_e pd pmap rmap e1 (fst (fst tys)) with
     | Ok x ->
       (match alloc_e pd pmap rmap e2 (snd (fst tys)) with
        | Ok x0 -> Ok (Papp2 (o, x, x0))
        | Error s -> Error s)
     | Error s -> Error s)
  | PappN (o, es) ->
    (match mapM2 bad_arg_number (alloc_e pd pmap rmap) es
             (fst (type_of_opN o)) with
     | Ok x -> Ok (PappN (o, x))
     | Error s -> Error s)
  | Pif (_, e0, e1, e2) ->
    (match alloc_e pd pmap rmap e0 Coq_abool with
     | Ok x ->
       (match alloc_e pd pmap rmap e1 ty with
        | Ok x0 ->
          (match alloc_e pd pmap rmap e2 ty with
           | Ok x1 -> Ok (Pif (ty, x, x0, x1))
           | Error s -> Error s)
        | Error s -> Error s)
     | Error s -> Error s)
  | _ -> Ok e

(** val alloc_es :
    coq_PointerData -> pos_map -> region_map -> pexpr list -> atype list ->
    (pp_error_loc, pexpr list) result **)

let alloc_es pd pmap rmap es ty =
  mapM2 bad_arg_number (alloc_e pd pmap rmap) es ty

(** val sub_region_direct :
    slot -> wsize -> concrete_slice -> Equality.sort -> sub_region **)

let sub_region_direct x align cs sc =
  let r = { r_slot = x; r_align = align; r_writable =
    (negb
      (eq_op expr_v_scope__canonical__eqtype_Equality sc (Obj.magic Sglob))) }
  in
  let z = zone_of_cs cs in { sr_region = r; sr_zone = z }

(** val sub_region_stack : slot -> wsize -> concrete_slice -> sub_region **)

let sub_region_stack x align cs =
  sub_region_direct x align cs (Obj.magic Slocal)

(** val alloc_lval :
    coq_PointerData -> pos_map -> region_map -> lval -> atype ->
    (pp_error_loc, region_map * lval) result **)

let alloc_lval pd pmap rmap r ty =
  match r with
  | Lnone (_, _) -> Ok (rmap, r)
  | Lvar x ->
    (match get_local pmap x.v_var with
     | Some pk ->
       (match OtherDefs.is_word_type (Var.vtype x.v_var) with
        | Some ws ->
          if subatype (Coq_aword ws) ty
          then (match get_sub_region rmap x with
                | Ok x0 ->
                  (match set_word rmap (Obj.magic Aligned) x0 x Valid ws with
                   | Ok x1 ->
                     (match addr_from_pk pmap x pk with
                      | Ok x2 ->
                        let (p, ofs) = x2 in
                        let r0 = Lmem (Aligned, ws, p.v_info,
                          (add pd (coq_Plvar p) (cast_const pd ofs)))
                        in
                        Ok (x1, r0)
                      | Error s -> Error s)
                   | Error s -> Error s)
                | Error s -> Error s)
          else Error (E.stk_ierror_basic x "invalid type for assignment")
        | None ->
          Error (E.stk_ierror_basic x "not a word variable in assignment"))
     | None ->
       (match check_diff pmap x with
        | Ok _ -> Ok (rmap, r)
        | Error s -> Error s))
  | Lmem (al, ws, vi, e1) ->
    (match alloc_e pd pmap rmap e1 (Coq_aword pd) with
     | Ok x -> Ok (rmap, (Lmem (al, ws, vi, x)))
     | Error s -> Error s)
  | Laset (al, aa, ws, x, e1) ->
    if not_trivially_incorrect aa ws e1 (size_of (Var.vtype x.v_var))
    then (match alloc_e pd pmap rmap e1 Coq_aint with
          | Ok x0 ->
            (match get_local pmap x.v_var with
             | Some pk ->
               (match get_sub_region_status rmap x with
                | Ok x1 ->
                  let (sr, status0) = x1 in
                  (match set_word rmap (Obj.magic al) sr x status0 ws with
                   | Ok x2 ->
                     (match addr_from_pk pmap x pk with
                      | Ok x3 ->
                        let (p, ofs) = x3 in
                        let ofs0 = mk_ofs pd aa ws x0 ofs in
                        let r0 = Lmem (al, ws, p.v_info,
                          (add pd (coq_Plvar p) ofs0))
                        in
                        Ok (x2, r0)
                      | Error s -> Error s)
                   | Error s -> Error s)
                | Error s -> Error s)
             | None ->
               (match check_diff pmap x with
                | Ok _ -> Ok (rmap, (Laset (al, aa, ws, x, x0)))
                | Error s -> Error s))
          | Error s -> Error s)
    else let s = E.stk_error_no_var "this write is trivially out-of-bounds" in
         Error s
  | Lasub (_, _, _, x, _) -> Error (E.stk_ierror_basic x "Lasub")

(** val nop : 'a1 asmOp -> 'a1 instr_r **)

let nop asmop =
  Copn ([], AT_none, (sopn_nop asmop), [])

(** val is_nop :
    region_map -> Equality.sort -> sub_region -> slot -> wsize ->
    concrete_slice -> Equality.sort -> bool **)

let is_nop rmap x sry s ws cs f =
  match Mvar.get rmap.var_region x with
  | Some srx ->
    (&&)
      (eq_op stack_alloc_sub_region__canonical__eqtype_Equality
        (Obj.magic srx) (Obj.magic sry))
      (check_stack_ptr rmap.region_var s ws cs f)
  | None -> false

(** val get_addr :
    'a1 asmOp -> 'a1 stack_alloc_params -> var_i -> lval -> assgn_tag ->
    mov_kind -> pexpr -> pexpr -> (pp_error_loc, 'a1 instr_r) result **)

let get_addr _ saparams x dx tag vpk y ofs =
  let oir = saparams.sap_mov_ofs dx tag vpk y ofs in
  (match oir with
   | Some ir -> Ok ir
   | None ->
     let err_pp =
       pp_hov ((PPEstring "cannot compute address") :: ((PPEvar
         x.v_var) :: ((PPEstring
         "the address computation in the right-hand side is too complex,") :: ((PPEstring
         "an intermediate variable might be needed") :: []))))
     in
     Error (E.stk_error x err_pp))

(** val is_stack_ptr :
    vptr_kind -> ((((Var.var * coq_Z) * wsize) * concrete_slice) * Var.var)
    option **)

let is_stack_ptr = function
| VKglob _ -> None
| VKptr p ->
  (match p with
   | Pstkptr (s, ofs, ws, z, f) -> Some ((((s, ofs), ws), z), f)
   | _ -> None)

(** val addr_from_vpk_pexpr :
    coq_PointerData -> pos_map -> status Mvar.t Mr.t -> var_i -> vptr_kind ->
    (pp_error_loc, pexpr * coq_Z) result **)

let addr_from_vpk_pexpr pd pmap rv x vpk =
  match is_stack_ptr vpk with
  | Some p ->
    let (p0, f) = p in
    let (p1, cs) = p0 in
    let (p2, ws) = p1 in
    let (s, ofs) = p2 in
    if check_stack_ptr rv s ws cs (Obj.magic f)
    then let rsp = with_var x pmap.vrsp in
         Ok ((Pload (Aligned, pd,
         (add pd (coq_Plvar rsp) (cast_const pd (Z.add ofs cs.cs_ofs))))), Z0)
    else let s0 =
           E.stk_error x
             (pp_box ((PPEstring "the stack pointer") :: ((PPEvar
               x.v_var) :: ((PPEstring "is no longer valid") :: []))))
         in
         Error s0
  | None ->
    (match addr_from_vpk pmap x vpk with
     | Ok x0 -> Ok ((coq_Plvar (fst x0)), (snd x0))
     | Error s -> Error s)

(** val mk_mov : vptr_kind -> mov_kind **)

let mk_mov = function
| VKglob _ -> MK_LEA
| VKptr p ->
  (match p with
   | Pdirect (_, _, _, _, v0) ->
     (match v0 with
      | Slocal -> MK_MOV
      | Sglob -> MK_LEA)
   | _ -> MK_MOV)

(** val regions_are_not_equal :
    (sub_region -> pp_error) -> string -> var_i -> sub_region -> sub_region
    -> pp_error_loc **)

let regions_are_not_equal pp_sr kind x sry sr =
  E.stk_error x
    (pp_nobox
      ((pp_box ((PPEstring "the assignment to") :: ((PPEstring
         kind) :: ((PPEvar x.v_var) :: ((PPEstring
         "cannot be turned into a nop") :: []))))) :: (PPEbreak :: ((pp_vbox
                                                                    ((PPEstring
                                                                    "source") :: (
                                                                    (pp_nobox
                                                                    ((PPEstring
                                                                    "  ") :: (
                                                                    (pp_sr
                                                                    sry) :: []))) :: ((PPEstring
                                                                    "and destination") :: (
                                                                    (pp_nobox
                                                                    ((PPEstring
                                                                    "  ") :: (
                                                                    (pp_sr sr) :: []))) :: ((PPEstring
                                                                    "regions are not equal") :: [])))))) :: []))))

(** val alloc_array_move :
    coq_PointerData -> 'a1 asmOp -> 'a1 stack_alloc_params -> (v_kind ->
    Uint63.t -> string -> atype -> Ident.Ident.ident) -> (sub_region ->
    pp_error) -> pos_map -> table -> region_map -> lval -> assgn_tag -> pexpr
    -> (pp_error_loc, (table * region_map) * 'a1 instr_r) result **)

let alloc_array_move pd asmop saparams fresh_var_ident pp_sr pmap table0 rmap r tag e =
  match match e with
        | Pconst _ ->
          Error
            (E.stk_ierror_no_var
              "alloc_array_move: variable/subarray expected (y)")
        | Pbool _ ->
          Error
            (E.stk_ierror_no_var
              "alloc_array_move: variable/subarray expected (y)")
        | Parr_init (_, _) ->
          Error
            (E.stk_ierror_no_var
              "alloc_array_move: variable/subarray expected (y)")
        | Pvar y ->
          let yv = y.gv in
          (match get_var_kind pmap y with
           | Ok x ->
             (match x with
              | Some vpk ->
                (match get_gsub_region_status rmap yv vpk with
                 | Ok x0 ->
                   let (sr, status0) = x0 in
                   (match addr_from_vpk_pexpr pd pmap rmap.region_var yv vpk with
                    | Ok x1 ->
                      Ok (((((table0, sr), status0), (mk_mov vpk)),
                        (fst x1)), (cast_const pd (snd x1)))
                    | Error s -> Error s)
                 | Error s -> Error s)
              | None -> Error (E.stk_ierror_basic yv "register array remains"))
           | Error s -> Error s)
        | Psub (aa, ws, len, y, e1) ->
          let yv = y.gv in
          (match get_var_kind pmap y with
           | Ok x ->
             (match x with
              | Some vpk ->
                (match get_gsub_region_status rmap yv vpk with
                 | Ok x0 ->
                   let (sr, status0) = x0 in
                   (match get_symbolic_of_pexpr (clone fresh_var_ident)
                            table0 e1 with
                    | Ok x1 ->
                      let (table1, se1) = x1 in
                      let ofs = mk_ofs_int aa ws se1 in
                      let len0 = Sconst (arr_size ws len) in
                      let (sr0, status1) =
                        sub_region_status_at_ofs yv sr status0
                          (Obj.magic ofs) (Obj.magic len0)
                      in
                      (match addr_from_vpk_pexpr pd pmap rmap.region_var yv
                               vpk with
                       | Ok x2 ->
                         (match alloc_e pd pmap rmap e1 Coq_aint with
                          | Ok x3 ->
                            Ok (((((table1, sr0), status1), (mk_mov vpk)),
                              (fst x2)), (mk_ofs pd aa ws x3 (snd x2)))
                          | Error s -> Error s)
                       | Error s -> Error s)
                    | Error s -> Error s)
                 | Error s -> Error s)
              | None -> Error (E.stk_ierror_basic yv "register array remains"))
           | Error s -> Error s)
        | _ ->
          Error
            (E.stk_ierror_no_var
              "alloc_array_move: variable/subarray expected (y)") with
  | Ok x ->
    let (y, ofs) = x in
    let (y0, ey) = y in
    let (y1, mk) = y0 in
    let (y2, statusy) = y1 in
    let (table1, sry) = y2 in
    (match r with
     | Lvar x0 ->
       (match get_local pmap x0.v_var with
        | Some pk ->
          (match pk with
           | Pdirect (s, _, ws, cs, sc) ->
             let sr = sub_region_direct s ws cs (Obj.magic sc) in
             if eq_op stack_alloc_sub_region__canonical__eqtype_Equality
                  (Obj.magic sry) (Obj.magic sr)
             then let rmap0 = set_move rmap (Obj.magic x0.v_var) sry statusy
                  in
                  Ok ((table1, rmap0), (nop asmop))
             else let s0 = regions_are_not_equal pp_sr "array" x0 sry sr in
                  Error s0
           | Pregptr p ->
             let rmap0 = set_move rmap (Obj.magic x0.v_var) sry statusy in
             (match get_addr asmop saparams x0 (Lvar (with_var x0 p)) tag mk
                      ey ofs with
              | Ok x1 -> Ok ((table1, rmap0), x1)
              | Error s -> Error s)
           | Pstkptr (slot0, ofsx, ws, cs, x') ->
             if is_nop rmap (Obj.magic x0.v_var) sry slot0 ws cs
                  (Obj.magic x')
             then let rmap0 = set_move rmap (Obj.magic x0.v_var) sry statusy
                  in
                  Ok ((table1, rmap0), (nop asmop))
             else let rmap0 = set_move rmap (Obj.magic x0.v_var) sry statusy
                  in
                  let rmap1 = set_stack_ptr rmap0 slot0 ws cs x' in
                  let dx_ofs = cast_const pd (Z.add ofsx cs.cs_ofs) in
                  let rsp = with_var x0 pmap.vrsp in
                  let dx = Lmem (Aligned, pd, rsp.v_info,
                    (add pd (coq_Plvar rsp) dx_ofs))
                  in
                  (match get_addr asmop saparams x0 dx tag mk ey ofs with
                   | Ok x1 -> Ok ((table1, rmap1), x1)
                   | Error s -> Error s))
        | None -> Error (E.stk_ierror_basic x0 "register array remains"))
     | Lasub (aa, ws, len, x0, e0) ->
       (match get_local pmap x0.v_var with
        | Some _ ->
          (match get_sub_region_status rmap x0 with
           | Ok x1 ->
             let (sr, status0) = x1 in
             (match get_symbolic_of_pexpr (clone fresh_var_ident) table1 e0 with
              | Ok x2 ->
                let (table2, e1) = x2 in
                let ofs0 = mk_ofs_int aa ws e1 in
                let len0 = Sconst (arr_size ws len) in
                let (sr', _) =
                  sub_region_status_at_ofs x0 sr status0 (Obj.magic ofs0)
                    (Obj.magic len0)
                in
                if eq_op stack_alloc_sub_region__canonical__eqtype_Equality
                     (Obj.magic sry) (Obj.magic sr')
                then let rmap0 =
                       set_move_sub rmap sr.sr_region (Obj.magic x0.v_var)
                         status0 (Obj.magic ofs0) (Obj.magic len0) statusy
                     in
                     Ok ((table2, rmap0), (nop asmop))
                else let s =
                       regions_are_not_equal pp_sr "sub-array" x0 sry sr'
                     in
                     Error s
              | Error s -> Error s)
           | Error s -> Error s)
        | None -> Error (E.stk_ierror_basic x0 "register array remains"))
     | _ ->
       Error
         (E.stk_ierror_no_var
           "alloc_array_move: variable/subarray expected (x)"))
  | Error s -> Error s

(** val is_protect_ptr_fail :
    'a1 asmOp -> lval list -> 'a1 sopn -> pexpr list ->
    ((lval * pexpr) * pexpr) option **)

let is_protect_ptr_fail _ rs o es =
  match o with
  | Oslh s ->
    (match s with
     | SLHprotect_ptr_fail (_, _) ->
       (match rs with
        | [] -> None
        | r :: l ->
          (match l with
           | [] ->
             (match es with
              | [] -> None
              | e :: l0 ->
                (match l0 with
                 | [] -> None
                 | msf :: l1 ->
                   (match l1 with
                    | [] -> Some ((r, e), msf)
                    | _ :: _ -> None)))
           | _ :: _ -> None))
     | _ -> None)
  | _ -> None

(** val lower_protect_ptr_fail :
    coq_PointerData -> 'a1 asmOp -> 'a1 sh_params -> instr_info -> lval list
    -> assgn_tag -> pexpr list -> 'a1 instr_r cexec **)

let lower_protect_ptr_fail pd asmop shparams ii lvs t0 es =
  lower_slho asmop shparams ii lvs t0 (SLHprotect pd) es

(** val alloc_protect_ptr :
    coq_PointerData -> coq_MSFsize -> 'a1 asmOp -> 'a1 sh_params -> pos_map
    -> region_map -> instr_info -> lval -> assgn_tag -> pexpr -> pexpr ->
    (pp_error_loc, region_map * 'a1 instr_r) result **)

let alloc_protect_ptr pd msfsz asmop shparams pmap rmap ii r t0 e msf =
  match match e with
        | Pconst _ ->
          Error
            (E.stk_ierror_no_var
              "alloc_protect_ptr: variable/subarray expected (y)")
        | Pbool _ ->
          Error
            (E.stk_ierror_no_var
              "alloc_protect_ptr: variable/subarray expected (y)")
        | Parr_init (_, _) ->
          Error
            (E.stk_ierror_no_var
              "alloc_protect_ptr: variable/subarray expected (y)")
        | Pvar y ->
          let yv = y.gv in
          (match get_var_kind pmap y with
           | Ok x ->
             (match x with
              | Some vpk ->
                (match vpk with
                 | VKglob _ ->
                   let s =
                     E.stk_error_no_var
                       "argument of protect_ptr should be a reg ptr"
                   in
                   Error s
                 | VKptr p ->
                   (match p with
                    | Pregptr _ ->
                      (match get_gsub_region_status rmap yv vpk with
                       | Ok x0 ->
                         (match addr_from_vpk_pexpr pd pmap rmap.region_var
                                  yv vpk with
                          | Ok x1 -> let (e0, _) = x1 in Ok (x0, e0)
                          | Error s -> Error s)
                       | Error s -> Error s)
                    | _ ->
                      let s =
                        E.stk_error_no_var
                          "argument of protect_ptr should be a reg ptr"
                      in
                      Error s))
              | None -> Error (E.stk_ierror_basic yv "register array remains"))
           | Error s -> Error s)
        | Psub (_, _, _, _, _) ->
          Error
            (E.stk_error_no_var
              "argument of protect_ptr cannot be a sub array")
        | _ ->
          Error
            (E.stk_ierror_no_var
              "alloc_protect_ptr: variable/subarray expected (y)") with
  | Ok x ->
    let (y, ey) = x in
    let (sry, statusy) = y in
    (match r with
     | Lvar x0 ->
       (match get_local pmap x0.v_var with
        | Some pk ->
          (match pk with
           | Pregptr p ->
             (match add_iinfo ii (alloc_e pd pmap rmap msf (Coq_aword msfsz)) with
              | Ok x1 ->
                let rmap0 = set_move rmap (Obj.magic x0.v_var) sry statusy in
                let dx = Lvar (with_var x0 p) in
                (match lower_protect_ptr_fail pd asmop shparams ii (dx :: [])
                         t0 (ey :: (x1 :: [])) with
                 | Ok x2 -> Ok (rmap0, x2)
                 | Error s -> Error s)
              | Error s -> Error s)
           | _ ->
             Error
               (E.stk_error_no_var
                 "only reg ptr can receive the result of protect_ptr"))
        | None -> Error (E.stk_ierror_basic x0 "register array remains"))
     | Lasub (_, _, _, _, _) ->
       Error (E.stk_error_no_var "cannot assign protect_ptr in a sub array")
     | _ ->
       Error
         (E.stk_ierror_no_var
           "alloc_array_move: variable/subarray expected (x)"))
  | Error s -> Error s

(** val alloc_array_move_init :
    coq_PointerData -> 'a1 asmOp -> 'a1 stack_alloc_params -> (v_kind ->
    Uint63.t -> string -> atype -> Ident.Ident.ident) -> (sub_region ->
    pp_error) -> pos_map -> table -> region_map -> lval -> assgn_tag -> pexpr
    -> (pp_error_loc, (table * region_map) * 'a1 instr_r) result **)

let alloc_array_move_init pd asmop saparams fresh_var_ident pp_sr pmap table0 rmap r tag e =
  if is_array_init e
  then (match r with
        | Lvar x ->
          (match get_sub_region rmap x with
           | Ok x0 ->
             let rmap0 = set_move rmap (Obj.magic x.v_var) x0 Valid in
             Ok ((table0, rmap0), (nop asmop))
           | Error s -> Error s)
        | _ -> Error (E.stk_ierror_no_var "arrayinit of non-variable"))
  else alloc_array_move pd asmop saparams fresh_var_ident pp_sr pmap table0
         rmap r tag e

(** val bad_lval_number : pp_error_loc **)

let bad_lval_number =
  E.stk_ierror_no_var "invalid number of lval"

(** val alloc_lvals :
    coq_PointerData -> pos_map -> region_map -> lval list -> atype list ->
    (pp_error_loc, region_map * lval list) result **)

let alloc_lvals pd pmap rmap rs tys =
  fmapM2 bad_lval_number (alloc_lval pd pmap) rmap rs tys

(** val incl_interval : intervals -> intervals -> bool **)

let incl_interval i1 i2 =
  all (fun s1 ->
    in_mem (Obj.magic s1)
      (mem
        (seq_predType stack_alloc_symbolic_slice__canonical__eqtype_Equality)
        (Obj.magic i2))) i1

(** val incl_status : status -> status -> bool **)

let incl_status status1 status2 =
  match status1 with
  | Valid -> (match status2 with
              | Valid -> true
              | _ -> false)
  | Unknown -> true
  | Borrowed i1 ->
    (match status2 with
     | Valid -> true
     | Unknown -> false
     | Borrowed i2 -> incl_interval i2 i1)

(** val incl_status_map : status_map -> status_map -> bool **)

let incl_status_map sm1 sm2 =
  Mvar.incl (fun _ -> incl_status) sm1 sm2

(** val incl : region_map -> region_map -> bool **)

let incl rmap1 rmap2 =
  (&&)
    (Mvar.incl (fun _ ->
      Obj.magic eq_op stack_alloc_sub_region__canonical__eqtype_Equality)
      rmap1.var_region rmap2.var_region)
    (Mr.incl (fun _ -> incl_status_map) rmap1.region_var rmap2.region_var)

(** val typecheck : sexpr -> (pp_error_loc, atype) result **)

let rec typecheck = function
| Sconst _ -> Ok Coq_aint
| Svar x -> Ok (Var.vtype x)
| Sof_int (ws, e0) ->
  (match typecheck e0 with
   | Ok x ->
     (match x with
      | Coq_aint -> Ok (Coq_aword ws)
      | _ -> Error (E.stk_ierror_no_var "typechecking failed"))
   | Error s -> Error s)
| Sto_int (_, ws, e0) ->
  (match typecheck e0 with
   | Ok x ->
     if subatype (Coq_aword ws) x
     then Ok Coq_aint
     else Error (E.stk_ierror_no_var "typechecking failed")
   | Error s -> Error s)
| Sneg (opk, e0) ->
  let opk_ty = type_of_opk opk in
  (match typecheck e0 with
   | Ok x ->
     if subatype opk_ty x
     then Ok opk_ty
     else Error (E.stk_ierror_no_var "typechecking failed")
   | Error s -> Error s)
| Sadd (opk, e1, e2) ->
  let opk_ty = type_of_opk opk in
  (match typecheck e1 with
   | Ok x ->
     (match typecheck e2 with
      | Ok x0 ->
        if (&&) (subatype opk_ty x) (subatype opk_ty x0)
        then Ok opk_ty
        else Error (E.stk_ierror_no_var "typechecking failed")
      | Error s -> Error s)
   | Error s -> Error s)
| Smul (opk, e1, e2) ->
  let opk_ty = type_of_opk opk in
  (match typecheck e1 with
   | Ok x ->
     (match typecheck e2 with
      | Ok x0 ->
        if (&&) (subatype opk_ty x) (subatype opk_ty x0)
        then Ok opk_ty
        else Error (E.stk_ierror_no_var "typechecking failed")
      | Error s -> Error s)
   | Error s -> Error s)
| Ssub (opk, e1, e2) ->
  let opk_ty = type_of_opk opk in
  (match typecheck e1 with
   | Ok x ->
     (match typecheck e2 with
      | Ok x0 ->
        if (&&) (subatype opk_ty x) (subatype opk_ty x0)
        then Ok opk_ty
        else Error (E.stk_ierror_no_var "typechecking failed")
      | Error s -> Error s)
   | Error s -> Error s)

(** val typecheck_slice : symbolic_slice -> bool **)

let typecheck_slice s =
  match typecheck s.ss_ofs with
  | Ok a ->
    (match a with
     | Coq_aint ->
       (match typecheck s.ss_len with
        | Ok a0 -> (match a0 with
                    | Coq_aint -> true
                    | _ -> false)
        | Error _ -> false)
     | _ -> false)
  | Error _ -> false

(** val merge_interval : intervals -> intervals -> intervals option **)

let merge_interval i1 i2 =
  foldl (fun acc s ->
    match acc with
    | Some acc0 -> add_sub_interval acc0 s
    | None -> None) (Some i2) i1

(** val read_e_rec : SvExtra.Sv.t -> sexpr -> SvExtra.Sv.t **)

let rec read_e_rec s = function
| Sconst _ -> s
| Svar x -> SvExtra.Sv.add (Obj.magic x) s
| Sof_int (_, e0) -> read_e_rec s e0
| Sto_int (_, _, e0) -> read_e_rec s e0
| Sneg (_, e0) -> read_e_rec s e0
| Sadd (_, e1, e2) -> read_e_rec (read_e_rec s e1) e2
| Smul (_, e1, e2) -> read_e_rec (read_e_rec s e1) e2
| Ssub (_, e1, e2) -> read_e_rec (read_e_rec s e1) e2

(** val read_e : sexpr -> SvExtra.Sv.t **)

let read_e =
  read_e_rec SvExtra.Sv.empty

(** val read_slice : symbolic_slice -> SvExtra.Sv.t **)

let read_slice s =
  SvExtra.Sv.union (read_e s.ss_ofs) (read_e s.ss_len)

(** val merge_status :
    SvExtra.Sv.t -> Var.var -> status option -> status option -> status option **)

let merge_status vars0 _ status1 status2 =
  match status1 with
  | Some status3 ->
    (match status2 with
     | Some status4 ->
       (match status3 with
        | Valid ->
          (match status4 with
           | Valid -> Some Valid
           | Unknown -> None
           | Borrowed i ->
             if all (fun s ->
                  (&&) (SvExtra.Sv.subset (read_slice s) vars0)
                    (typecheck_slice s)) i
             then Some (Borrowed i)
             else None)
        | Unknown -> None
        | Borrowed i1 ->
          (match status4 with
           | Valid ->
             if all (fun s ->
                  (&&) (SvExtra.Sv.subset (read_slice s) vars0)
                    (typecheck_slice s)) i1
             then Some (Borrowed i1)
             else None
           | Unknown -> None
           | Borrowed i2 ->
             (match merge_interval i1 i2 with
              | Some i ->
                if all (fun s ->
                     (&&) (SvExtra.Sv.subset (read_slice s) vars0)
                       (typecheck_slice s)) i
                then Some (Borrowed i)
                else None
              | None -> None)))
     | None -> None)
  | None -> None

(** val merge_status_map :
    SvExtra.Sv.t -> region -> status_map option -> status_map option ->
    status Mvar.t option **)

let merge_status_map vars0 _ bm1 bm2 =
  match bm1 with
  | Some bm3 ->
    (match bm2 with
     | Some bm4 ->
       let bm = Mvar.map2 (Obj.magic merge_status vars0) bm3 bm4 in
       if Mvar.is_empty bm then None else Some bm
     | None -> None)
  | None -> None

(** val merge : SvExtra.Sv.t -> region_map -> region_map -> region_map **)

let merge vars0 rmap1 rmap2 =
  { var_region =
    (Mvar.map2 (fun _ osr1 osr2 ->
      match osr1 with
      | Some sr1 ->
        (match osr2 with
         | Some sr2 ->
           if eq_op stack_alloc_sub_region__canonical__eqtype_Equality
                (Obj.magic sr1) (Obj.magic sr2)
           then osr1
           else None
         | None -> None)
      | None -> None) rmap1.var_region rmap2.var_region); region_var =
    (Mr.map2 (Obj.magic merge_status_map vars0) rmap1.region_var
      rmap2.region_var) }

(** val incl_table : table -> table -> bool **)

let incl_table table1 table2 =
  (&&)
    (Mvar.incl (fun _ ->
      Obj.magic eq_op stack_alloc_sexpr__canonical__eqtype_Equality)
      table1.bindings table2.bindings)
    ((&&) (leb table1.counter table2.counter)
      (SvExtra.Sv.subset table1.vars table2.vars))

(** val loop2 :
    'a1 asmOp -> instr_info -> (table -> region_map ->
    (((table * region_map) * (table * region_map)) * ((pexpr * 'a1 instr list
    list) * 'a1 instr list list)) cexec) -> nat -> table -> region_map ->
    (pp_error_loc, (table * region_map) * ((pexpr * 'a1 instr list
    list) * 'a1 instr list list)) result **)

let rec loop2 asmop ii check_c2 n table0 rmap =
  match n with
  | O -> Error (pp_at_ii ii (E.stk_ierror_no_var "loop2"))
  | S n0 ->
    (match check_c2 table0 rmap with
     | Ok x ->
       let (y, c) = x in
       let (y0, y1) = y in
       let (table2, rmap2) = y1 in
       if (&&) (incl_table table0 table2) (incl rmap rmap2)
       then Ok (y0, c)
       else let table1 = merge_table table0 table2 in
            let rmap0 = merge table1.vars rmap rmap2 in
            loop2 asmop ii check_c2 n0 table1 rmap0
     | Error s -> Error s)

type stk_alloc_oracle_t = { sao_align : wsize; sao_size : coq_Z;
                            sao_ioff : coq_Z; sao_extra_size : coq_Z;
                            sao_max_size : coq_Z; sao_max_call_depth : 
                            coq_Z; sao_params : param_info option list;
                            sao_return : nat option list;
                            sao_slots : ((Var.var * wsize) * coq_Z) list;
                            sao_alloc : (Var.var * ptr_kind_init) list;
                            sao_to_save : (Var.var * coq_Z) list;
                            sao_rsp : saved_stack;
                            sao_return_address : return_address_location }

(** val sao_frame_size : stk_alloc_oracle_t -> coq_Z **)

let sao_frame_size sao =
  if is_RAnone sao.sao_return_address
  then Z.add sao.sao_size sao.sao_extra_size
  else round_ws sao.sao_align (Z.add sao.sao_size sao.sao_extra_size)

(** val get_Pvar : pexpr -> (pp_error_loc, gvar) result **)

let get_Pvar = function
| Pvar x -> Ok x
| _ -> Error (E.stk_ierror_no_var "get_Pvar: variable expected")

(** val alloc_call_arg_aux :
    pos_map -> region_map -> region_map -> param_info option -> pexpr ->
    (pp_error_loc, region_map * ((bool * sub_region) option * pexpr)) result **)

let alloc_call_arg_aux pmap rmap0 rmap sao_param e =
  match get_Pvar e with
  | Ok x ->
    if negb (is_glob x)
    then let xv = x.gv in
         (match sao_param with
          | Some pi ->
            (match get_local pmap xv.v_var with
             | Some p0 ->
               (match p0 with
                | Pregptr p ->
                  (match get_sub_region_status rmap0 xv with
                   | Ok x0 ->
                     let (sr, status0) = x0 in
                     (match check_valid xv status0 with
                      | Ok _ ->
                        (match check_align (Obj.magic Aligned) xv sr
                                 pi.pp_align with
                         | Ok _ ->
                           (match if pi.pp_writable
                                  then set_clear rmap xv sr
                                  else Ok rmap with
                            | Ok x1 ->
                              Ok (x1, ((Some (pi.pp_writable, sr)), (Pvar
                                (mk_lvar (with_var xv p)))))
                            | Error s -> Error s)
                         | Error s -> Error s)
                      | Error s -> Error s)
                   | Error s -> Error s)
                | _ ->
                  Error
                    (E.stk_ierror_basic xv "the argument should be a reg ptr"))
             | None ->
               Error
                 (E.stk_ierror_basic xv "the argument should be a reg ptr"))
          | None ->
            (match get_local pmap xv.v_var with
             | Some _ -> Error (E.stk_ierror_basic xv "argument not a reg")
             | None ->
               (match check_diff pmap xv with
                | Ok _ -> Ok (rmap, (None, (Pvar x)))
                | Error s -> Error s)))
    else let s =
           E.stk_ierror_basic x.gv "global variable in argument of a call"
         in
         Error s
  | Error s -> Error s

(** val alloc_call_args_aux :
    pos_map -> region_map -> param_info option list -> pexpr list ->
    (pp_error_loc, region_map * ((bool * sub_region) option * pexpr) list)
    result **)

let alloc_call_args_aux pmap rmap sao_params0 es =
  fmapM2 (E.stk_ierror_no_var "bad params info")
    (alloc_call_arg_aux pmap rmap) rmap sao_params0 es

(** val disjoint_zones : Equality.sort list -> Equality.sort list -> bool **)

let rec disjoint_zones z1 z2 =
  match z1 with
  | [] -> false
  | s1 :: z3 ->
    (match z2 with
     | [] -> false
     | s2 :: z4 ->
       if eq_op stack_alloc_symbolic_slice__canonical__eqtype_Equality s1 s2
       then disjoint_zones z3 z4
       else disjoint_slices (Obj.magic s1) (Obj.magic s2))

(** val disj_sub_regions : sub_region -> sub_region -> bool **)

let disj_sub_regions sr1 sr2 =
  (||) (negb (region_same sr1.sr_region sr2.sr_region))
    (disjoint_zones (Obj.magic sr1.sr_zone) (Obj.magic sr2.sr_zone))

(** val check_all_disj :
    sub_region list -> sub_region list -> ((bool * sub_region)
    option * pexpr) list -> bool **)

let rec check_all_disj notwritables writables = function
| [] -> true
| p :: srs0 ->
  let (o, _) = p in
  (match o with
   | Some p1 ->
     let (writable, sr) = p1 in
     if all (disj_sub_regions sr) writables
     then if writable
          then if all (disj_sub_regions sr) notwritables
               then check_all_disj notwritables (sr :: writables) srs0
               else false
          else check_all_disj (sr :: notwritables) writables srs0
     else false
   | None -> check_all_disj notwritables writables srs0)

(** val alloc_call_args :
    pos_map -> region_map -> funname -> param_info option list -> pexpr list
    -> (pp_error_loc, region_map * ((bool * sub_region) option * pexpr) list)
    result **)

let alloc_call_args pmap rmap fn sao_params0 es =
  match alloc_call_args_aux pmap rmap sao_params0 es with
  | Ok x ->
    if check_all_disj [] [] (snd x)
    then Ok x
    else let s =
           E.stk_error_no_var_box
             (pp_hov ((PPEstring
               "in the call to function") :: ((pp_nobox ((PPEfunname
                                                fn) :: ((PPEstring
                                                ",") :: []))) :: ((PPEstring
               "some writable reg ptr are not disjoint") :: []))))
         in
         Error s
  | Error s -> Error s

(** val check_lval_reg_call :
    pos_map -> lval -> (pp_error_loc, unit) result **)

let check_lval_reg_call pmap r = match r with
| Lnone (_, _) -> Ok ()
| Lvar x ->
  (match get_local pmap x.v_var with
   | Some _ ->
     Error (E.stk_ierror_basic x "call result should be stored in reg")
   | None -> (match check_diff pmap x with
              | Ok _ -> Ok ()
              | Error s -> Error s))
| Lmem (_, _, _, _) ->
  Error (E.stk_ierror_basic_lv r "call result should be stored in reg")
| Laset (_, _, _, _, _) ->
  Error (E.stk_ierror_basic_lv r "array assignement in lval of a call")
| Lasub (_, _, _, _, _) ->
  Error (E.stk_ierror_basic_lv r "sub-array assignement in lval of a call")

(** val get_regptr : pos_map -> var_i -> (pp_error_loc, var_i) result **)

let get_regptr pmap x =
  match get_local pmap x.v_var with
  | Some p0 ->
    (match p0 with
     | Pregptr p -> Ok (with_var x p)
     | _ ->
       Error
         (E.stk_ierror x
           (pp_box ((PPEstring "variable") :: ((PPEvar
             x.v_var) :: ((PPEstring "should be a reg ptr") :: []))))))
  | None ->
    Error
      (E.stk_ierror x
        (pp_box ((PPEstring "variable") :: ((PPEvar x.v_var) :: ((PPEstring
          "should be a reg ptr") :: [])))))

(** val alloc_lval_call :
    coq_PointerData -> pos_map -> ((bool * sub_region) option * pexpr) list
    -> region_map -> lval -> nat option -> (pp_error_loc, region_map * lval)
    result **)

let alloc_lval_call pd pmap srs rmap r = function
| Some i0 ->
  let (o, _) = nth (None, (Pconst Z0)) srs i0 in
  (match o with
   | Some p0 ->
     let (_, sr) = p0 in
     (match r with
      | Lnone (i1, _) -> Ok (rmap, (Lnone (i1, (Coq_aword pd))))
      | Lvar x ->
        (match get_regptr pmap x with
         | Ok x0 ->
           let rmap0 = set_move rmap (Obj.magic x.v_var) sr Valid in
           Ok (rmap0, (Lvar x0))
         | Error s -> Error s)
      | Lmem (_, _, _, _) ->
        Error (E.stk_ierror_basic_lv r "call result should be stored in reg")
      | Laset (_, _, _, _, _) ->
        Error (E.stk_ierror_basic_lv r "array assignement in lval of a call")
      | Lasub (_, _, _, _, _) ->
        Error
          (E.stk_ierror_basic_lv r "sub-array assignement in lval of a call"))
   | None -> Error (E.stk_ierror_no_var "alloc_lval_call"))
| None ->
  (match check_lval_reg_call pmap r with
   | Ok _ -> Ok (rmap, r)
   | Error s -> Error s)

(** val alloc_call_res :
    coq_PointerData -> pos_map -> region_map -> ((bool * sub_region)
    option * pexpr) list -> nat option list -> lval list -> (pp_error_loc,
    region_map * lval list) result **)

let alloc_call_res pd pmap rmap srs ret_pos rs =
  fmapM2 bad_lval_number (alloc_lval_call pd pmap srs) rmap rs ret_pos

(** val alloc_call :
    bool -> coq_PointerData -> 'a1 asmOp -> pos_map -> (funname ->
    stk_alloc_oracle_t) -> stk_alloc_oracle_t -> region_map -> lval list ->
    funname -> pexpr list -> (pp_error_loc, region_map * 'a1 instr_r) result **)

let alloc_call check pd _ pmap local_alloc sao_caller rmap rs fn es =
  let sao_callee = local_alloc fn in
  (match alloc_call_args pmap rmap fn sao_callee.sao_params es with
   | Ok x ->
     let (rmap0, es0) = x in
     (match alloc_call_res pd pmap rmap0 es0 sao_callee.sao_return rs with
      | Ok x0 ->
        (match assert_check check
                 (negb (is_RAnone sao_callee.sao_return_address))
                 (E.stk_ierror_no_var "cannot call export function") with
         | Ok _ ->
           (match let local_size = sao_frame_size sao_caller in
                  assert_check check
                    (Z.leb (Z.add local_size sao_callee.sao_max_size)
                      sao_caller.sao_max_size)
                    (E.stk_ierror_no_var "error in max size computation") with
            | Ok _ ->
              (match assert_check check
                       (cmp_le wsize_cmp sao_callee.sao_align
                         sao_caller.sao_align)
                       (E.stk_ierror_no_var "non aligned function call") with
               | Ok _ ->
                 let es1 = map snd es0 in
                 Ok ((fst x0), (Ccall ((snd x0), fn, es1)))
               | Error s -> Error s)
            | Error s -> Error s)
         | Error s -> Error s)
      | Error s -> Error s)
   | Error s -> Error s)

(** val alloc_syscall :
    coq_PointerData -> 'a1 asmOp -> 'a1 stack_alloc_params -> pos_map ->
    instr_info -> region_map -> lval list ->
    (Wsize.wsize * BinNums.positive) Syscall_t.syscall_t -> pexpr list ->
    (pp_error_loc, region_map * 'a1 instr list) result **)

let alloc_syscall pd _ saparams pmap ii rmap rs o es =
  add_iinfo ii
    (let Syscall_t.RandomBytes (ws, n) = o in
     let len = arr_size ws n in
     if Z.ltb len (wbase pd)
     then (match rs with
           | [] ->
             Error (E.stk_ierror_no_var "randombytes: invalid args or result")
           | y :: l ->
             (match y with
              | Lvar x ->
                (match l with
                 | [] ->
                   (match es with
                    | [] ->
                      Error
                        (E.stk_ierror_no_var
                          "randombytes: invalid args or result")
                    | y0 :: l0 ->
                      (match y0 with
                       | Pvar xe ->
                         (match l0 with
                          | [] ->
                            let xe0 = xe.gv in
                            let xlen = with_var xe0 pmap.vxlen in
                            (match get_regptr pmap xe0 with
                             | Ok x0 ->
                               (match get_regptr pmap x with
                                | Ok x1 ->
                                  (match get_sub_region rmap xe0 with
                                   | Ok x2 ->
                                     (match set_clear rmap xe0 x2 with
                                      | Ok x3 ->
                                        let rmap0 =
                                          set_move x3 (Obj.magic x.v_var) x2
                                            Valid
                                        in
                                        Ok (rmap0, ((MkI (ii,
                                        (saparams.sap_immediate xlen len))) :: ((MkI
                                        (ii, (Csyscall (((Lvar x1) :: []), o,
                                        ((coq_Plvar x0) :: ((coq_Plvar xlen) :: [])))))) :: [])))
                                      | Error s -> Error s)
                                   | Error s -> Error s)
                                | Error s -> Error s)
                             | Error s -> Error s)
                          | _ :: _ ->
                            Error
                              (E.stk_ierror_no_var
                                "randombytes: invalid args or result"))
                       | _ ->
                         Error
                           (E.stk_ierror_no_var
                             "randombytes: invalid args or result")))
                 | _ :: _ ->
                   Error
                     (E.stk_ierror_no_var
                       "randombytes: invalid args or result"))
              | _ ->
                Error
                  (E.stk_ierror_no_var "randombytes: invalid args or result")))
     else let s =
            E.stk_error_no_var "randombytes: the requested size is too large"
          in
          Error s)

(** val is_swap_array : 'a1 asmOp -> 'a1 sopn -> bool **)

let is_swap_array _ = function
| Opseudo_op p -> (match p with
                   | Oswap ty -> OtherDefs.is_aarr ty
                   | _ -> false)
| _ -> false

(** val alloc_array_swap :
    'a1 asmOp -> 'a1 stack_alloc_params -> pos_map -> region_map -> lval list
    -> assgn_tag -> pexpr list -> (pp_error_loc, region_map * 'a1 instr_r)
    result **)

let alloc_array_swap _ saparams pmap rmap rs t0 es =
  match rs with
  | [] ->
    Error
      (E.stk_ierror_no_var
        "swap: invalid args or result, only reg ptr are accepted")
  | y0 :: l ->
    (match y0 with
     | Lvar x ->
       (match l with
        | [] ->
          Error
            (E.stk_ierror_no_var
              "swap: invalid args or result, only reg ptr are accepted")
        | l0 :: l1 ->
          (match l0 with
           | Lvar y ->
             (match l1 with
              | [] ->
                (match es with
                 | [] ->
                   Error
                     (E.stk_ierror_no_var
                       "swap: invalid args or result, only reg ptr are accepted")
                 | y1 :: l2 ->
                   (match y1 with
                    | Pvar z' ->
                      (match l2 with
                       | [] ->
                         Error
                           (E.stk_ierror_no_var
                             "swap: invalid args or result, only reg ptr are accepted")
                       | p :: l3 ->
                         (match p with
                          | Pvar w' ->
                            (match l3 with
                             | [] ->
                               let z = z'.gv in
                               (match get_regptr pmap z with
                                | Ok x0 ->
                                  (match get_sub_region_status rmap z with
                                   | Ok x1 ->
                                     let (srz, statusz) = x1 in
                                     let w = w'.gv in
                                     (match get_regptr pmap w with
                                      | Ok x2 ->
                                        (match get_sub_region_status rmap w with
                                         | Ok x3 ->
                                           let (srw, statusw) = x3 in
                                           let rmap0 =
                                             set_move rmap
                                               (Obj.magic x.v_var) srw statusw
                                           in
                                           let rmap1 =
                                             set_move rmap0
                                               (Obj.magic y.v_var) srz statusz
                                           in
                                           (match get_regptr pmap x with
                                            | Ok x4 ->
                                              (match get_regptr pmap y with
                                               | Ok x5 ->
                                                 if (&&) (is_lvar z')
                                                      (is_lvar w')
                                                 then Ok (rmap1,
                                                        (saparams.sap_swap t0
                                                          x4 x5 x0 x2))
                                                 else let s =
                                                        E.stk_ierror_no_var
                                                          "global reg ptr ..."
                                                      in
                                                      Error s
                                               | Error s -> Error s)
                                            | Error s -> Error s)
                                         | Error s -> Error s)
                                      | Error s -> Error s)
                                   | Error s -> Error s)
                                | Error s -> Error s)
                             | _ :: _ ->
                               Error
                                 (E.stk_ierror_no_var
                                   "swap: invalid args or result, only reg ptr are accepted"))
                          | _ ->
                            Error
                              (E.stk_ierror_no_var
                                "swap: invalid args or result, only reg ptr are accepted")))
                    | _ ->
                      Error
                        (E.stk_ierror_no_var
                          "swap: invalid args or result, only reg ptr are accepted")))
              | _ :: _ ->
                Error
                  (E.stk_ierror_no_var
                    "swap: invalid args or result, only reg ptr are accepted"))
           | _ ->
             Error
               (E.stk_ierror_no_var
                 "swap: invalid args or result, only reg ptr are accepted")))
     | _ ->
       Error
         (E.stk_ierror_no_var
           "swap: invalid args or result, only reg ptr are accepted"))

(** val is_declassify_array : 'a1 asmOp -> 'a1 sopn -> bool **)

let is_declassify_array _ = function
| Opseudo_op p ->
  (match p with
   | Odeclassify ty -> OtherDefs.is_aarr ty
   | _ -> false)
| _ -> false

(** val alloc_declassify_array :
    coq_PointerData -> 'a1 asmOp -> pos_map -> region_map -> pexpr list ->
    (pp_error_loc, 'a1 instr_r) result **)

let alloc_declassify_array pd _ pmap rmap = function
| [] -> Error (E.stk_ierror_no_var "declassify: invalid args")
| y :: l ->
  (match y with
   | Pvar x ->
     (match l with
      | [] ->
        if is_lvar x
        then let xv = x.gv in
             (match get_sub_region_status rmap xv with
              | Ok x0 ->
                let (_, status0) = x0 in
                (match check_valid xv status0 with
                 | Ok _ ->
                   (match get_local pmap xv.v_var with
                    | Some pk ->
                      (match addr_from_pk pmap xv pk with
                       | Ok x1 ->
                         let (p, ofs) = x1 in
                         let e = add pd (coq_Plvar p) (cast_const pd ofs) in
                         let len = Z.to_pos (size_of (Var.vtype xv.v_var)) in
                         Ok (Copn ([], AT_keep, (Opseudo_op (Odeclassify_mem
                         len)), (e :: [])))
                       | Error s -> Error s)
                    | None ->
                      Error (E.stk_ierror_basic xv "register array remains"))
                 | Error s -> Error s)
              | Error s -> Error s)
        else let s = E.stk_ierror_no_var "global reg ptr ..." in Error s
      | _ :: _ -> Error (E.stk_ierror_no_var "declassify: invalid args"))
   | _ -> Error (E.stk_ierror_no_var "declassify: invalid args"))

(** val alloc_i :
    bool -> coq_PointerData -> coq_MSFsize -> 'a1 asmOp -> 'a1 sh_params ->
    'a1 stack_alloc_params -> ('a1 asm_op_t -> bool) -> (v_kind -> Uint63.t
    -> string -> atype -> Ident.Ident.ident) -> (sub_region -> pp_error) ->
    pos_map -> (funname -> stk_alloc_oracle_t) -> 'a1 _uprog ->
    stk_alloc_oracle_t -> (table * region_map) -> 'a1 instr ->
    ((table * region_map) * 'a1 instr list) cexec **)

let rec alloc_i check pd msfsz asmop shparams saparams is_move_op fresh_var_ident pp_sr pmap local_alloc p sao trmap i =
  let (table0, rmap) = trmap in
  let MkI (ii, ir) = i in
  (match ir with
   | Cassgn (r, t0, ty, e) ->
     if OtherDefs.is_aarr ty
     then (match add_iinfo ii
                   (alloc_array_move_init pd asmop saparams fresh_var_ident
                     pp_sr pmap table0 rmap r t0 e) with
           | Ok x ->
             let (y, ir0) = x in
             let (table1, rmap0) = y in
             let table2 = remove_binding_lval table1 r in
             Ok ((table2, rmap0), ((MkI (ii, ir0)) :: []))
           | Error s -> Error s)
     else (match symbolic_of_pexpr (clone fresh_var_ident) table0 e with
           | Ok x ->
             (match x with
              | Some y ->
                let (table1, e0) = y in
                let oe = Some e0 in
                let table2 = remove_binding_lval table1 r in
                let table3 = update_table table2 r oe in
                (match add_iinfo ii (alloc_e pd pmap rmap e ty) with
                 | Ok x0 ->
                   (match add_iinfo ii (alloc_lval pd pmap rmap r ty) with
                    | Ok x1 ->
                      Ok ((table3, (fst x1)), ((MkI (ii, (Cassgn ((snd x1),
                        t0, ty, x0)))) :: []))
                    | Error s -> Error s)
                 | Error s -> Error s)
              | None ->
                let oe = None in
                let table1 = remove_binding_lval table0 r in
                let table2 = update_table table1 r oe in
                (match add_iinfo ii (alloc_e pd pmap rmap e ty) with
                 | Ok x0 ->
                   (match add_iinfo ii (alloc_lval pd pmap rmap r ty) with
                    | Ok x1 ->
                      Ok ((table2, (fst x1)), ((MkI (ii, (Cassgn ((snd x1),
                        t0, ty, x0)))) :: []))
                    | Error s -> Error s)
                 | Error s -> Error s))
           | Error s -> Error s)
   | Copn (rs, t0, o, e) ->
     (match is_protect_ptr_fail asmop rs o e with
      | Some p0 ->
        let (p1, msf) = p0 in
        let (r, e0) = p1 in
        let table1 = remove_binding_lval table0 r in
        (match alloc_protect_ptr pd msfsz asmop shparams pmap rmap ii r t0 e0
                 msf with
         | Ok x -> Ok ((table1, (fst x)), ((MkI (ii, (snd x))) :: []))
         | Error s -> Error s)
      | None ->
        if is_swap_array asmop o
        then let table1 = remove_binding_lvals table0 rs in
             (match add_iinfo ii
                      (alloc_array_swap asmop saparams pmap rmap rs t0 e) with
              | Ok x -> Ok ((table1, (fst x)), ((MkI (ii, (snd x))) :: []))
              | Error s -> Error s)
        else if is_declassify_array asmop o
             then (match alloc_declassify_array pd asmop pmap rmap e with
                   | Ok x -> Ok ((table0, rmap), ((MkI (ii, x)) :: []))
                   | Error s -> Error s)
             else (match match rs with
                         | [] -> Ok (remove_binding_lvals table0 rs)
                         | r :: l ->
                           (match l with
                            | [] ->
                              (match o with
                               | Oasm op ->
                                 (match e with
                                  | [] -> Ok (remove_binding_lvals table0 rs)
                                  | e0 :: l0 ->
                                    (match l0 with
                                     | [] ->
                                       if is_move_op op
                                       then (match symbolic_of_pexpr
                                                     (clone fresh_var_ident)
                                                     table0 e0 with
                                             | Ok x ->
                                               (match x with
                                                | Some y ->
                                                  let (table1, e1) = y in
                                                  let oe = Some e1 in
                                                  let table2 =
                                                    remove_binding_lval
                                                      table1 r
                                                  in
                                                  Ok
                                                  (update_table table2 r oe)
                                                | None ->
                                                  let oe = None in
                                                  let table1 =
                                                    remove_binding_lval
                                                      table0 r
                                                  in
                                                  Ok
                                                  (update_table table1 r oe))
                                             | Error s -> Error s)
                                       else Ok
                                              (remove_binding_lvals table0 rs)
                                     | _ :: _ ->
                                       Ok (remove_binding_lvals table0 rs)))
                               | _ -> Ok (remove_binding_lvals table0 rs))
                            | _ :: _ -> Ok (remove_binding_lvals table0 rs)) with
                   | Ok x ->
                     (match add_iinfo ii
                              (alloc_es pd pmap rmap e
                                (sopn_tin pd msfsz asmop o)) with
                      | Ok x0 ->
                        (match add_iinfo ii
                                 (alloc_lvals pd pmap rmap rs
                                   (sopn_tout pd msfsz asmop o)) with
                         | Ok x1 ->
                           Ok ((x, (fst x1)), ((MkI (ii, (Copn ((snd x1), t0,
                             o, x0)))) :: []))
                         | Error s -> Error s)
                      | Error s -> Error s)
                   | Error s -> Error s))
   | Csyscall (rs, o, es) ->
     let table1 = remove_binding_lvals table0 rs in
     (match alloc_syscall pd asmop saparams pmap ii rmap rs o es with
      | Ok x -> let (rmap0, c) = x in Ok ((table1, rmap0), c)
      | Error s -> Error s)
   | Cif (e, c1, c2) ->
     (match add_iinfo ii (alloc_e pd pmap rmap e Coq_abool) with
      | Ok x ->
        (match fmapM
                 (alloc_i check pd msfsz asmop shparams saparams is_move_op
                   fresh_var_ident pp_sr pmap local_alloc p sao) (table0,
                 rmap) c1 with
         | Ok x0 ->
           let (y, c3) = x0 in
           let (table1, rmap1) = y in
           (match fmapM
                    (alloc_i check pd msfsz asmop shparams saparams
                      is_move_op fresh_var_ident pp_sr pmap local_alloc p sao)
                    (table0, rmap) c2 with
            | Ok x1 ->
              let (y0, c4) = x1 in
              let (table2, rmap2) = y0 in
              let table3 = merge_table table1 table2 in
              let rmap0 = merge table3.vars rmap1 rmap2 in
              Ok ((table3, rmap0), ((MkI (ii, (Cif (x, (flatten c3),
              (flatten c4))))) :: []))
            | Error s -> Error s)
         | Error s -> Error s)
      | Error s -> Error s)
   | Cfor (_, _, _) ->
     Error (pp_at_ii ii (E.stk_ierror_no_var "don't deal with for loop"))
   | Cwhile (a, c1, e, info, c2) ->
     let check_c = fun table1 rmap0 ->
       match fmapM
               (alloc_i check pd msfsz asmop shparams saparams is_move_op
                 fresh_var_ident pp_sr pmap local_alloc p sao) (table1,
               rmap0) c1 with
       | Ok x ->
         let (y, c3) = x in
         let (table2, rmap1) = y in
         (match add_iinfo ii (alloc_e pd pmap rmap1 e Coq_abool) with
          | Ok x0 ->
            (match fmapM
                     (alloc_i check pd msfsz asmop shparams saparams
                       is_move_op fresh_var_ident pp_sr pmap local_alloc p
                       sao) (table2, rmap1) c2 with
             | Ok x1 ->
               let (y0, c4) = x1 in Ok (((table2, rmap1), y0), ((x0, c3), c4))
             | Error s -> Error s)
          | Error s -> Error s)
       | Error s -> Error s
     in
     (match loop2 asmop ii check_c Loop.nb table0 rmap with
      | Ok x ->
        let (y, y0) = x in
        let (y1, c3) = y0 in
        let (e0, c4) = y1 in
        Ok (y, ((MkI (ii, (Cwhile (a, (flatten c4), e0, info,
        (flatten c3))))) :: []))
      | Error s -> Error s)
   | Ccall (rs, fn, es) ->
     (match get_fundef p.p_funcs fn with
      | Some _ ->
        let table1 = remove_binding_lvals table0 rs in
        (match add_iinfo ii
                 (alloc_call check pd asmop pmap local_alloc sao rmap rs fn
                   es) with
         | Ok x -> Ok ((table1, (fst x)), ((MkI (ii, (snd x))) :: []))
         | Error s -> Error s)
      | None ->
        let s =
          pp_at_ii ii (E.stk_ierror_no_var "call to a undefined function")
        in
        Error s))

(** val init_stack_layout :
    (coq_Z * wsize) Mvar.t -> stk_alloc_oracle_t -> (pp_error_loc,
    (coq_Z * wsize) Mvar.t) result **)

let init_stack_layout mglob sao =
  let add0 = fun xsr slp ->
    let (stack, p) = slp in
    let (p0, ofs) = xsr in
    let (x, ws) = p0 in
    (match Mvar.get stack x with
     | Some _ -> Error (E.stk_ierror_no_var "duplicate stack region")
     | None ->
       (match Mvar.get mglob x with
        | Some _ ->
          Error (E.stk_ierror_no_var "a region is both glob and stack")
        | None ->
          if cmp_le Z.compare p ofs
          then let len = size_of (Var.vtype (Obj.magic x)) in
               if cmp_le wsize_cmp ws sao.sao_align
               then if eq_op coq_BinNums_Z__canonical__eqtype_Equality
                         (Obj.magic Z.coq_land ofs
                           (Z.sub (wsize_size ws) (Zpos Coq_xH)))
                         (Obj.magic Z0)
                    then let stack0 = Mvar.set stack x (ofs, ws) in
                         Ok (stack0, (Z.add ofs len))
                    else Error
                           (E.stk_ierror_no_var "bad stack region alignment")
               else Error (E.stk_ierror_no_var "bad stack alignment")
          else Error (E.stk_ierror_no_var "stack region overlap")))
  in
  if Z.leb Z0 sao.sao_ioff
  then (match foldM (Obj.magic add0) (Mvar.empty, sao.sao_ioff) sao.sao_slots with
        | Ok x ->
          let (stack, size0) = x in
          if cmp_le Z.compare size0 sao.sao_size
          then Ok stack
          else Error (E.stk_ierror_no_var "stack size")
        | Error s -> Error s)
  else let s = E.stk_ierror_no_var "negative initial stack offset" in Error s

(** val add_alloc :
    coq_PointerData -> (coq_Z * wsize) Mvar.t -> (coq_Z * wsize) Mvar.t ->
    (Var.var * ptr_kind_init) -> ((ptr_kind
    Mvar.t * region_map) * SvExtra.Sv.t) -> (pp_error_loc, (ptr_kind
    Mvar.Map.t * region_map) * SvExtra.Sv.t) result **)

let add_alloc pd globals0 stack xpk = function
| (p, sv) ->
  let (locals0, rmap) = p in
  let (x, pk) = xpk in
  if SvExtra.Sv.mem (Obj.magic x) sv
  then Error (E.stk_ierror_no_var "invalid reg pointer")
  else (match Mvar.get locals0 (Obj.magic x) with
        | Some _ ->
          Error
            (E.stk_ierror_no_var
              "the oracle returned two results for the same var")
        | None ->
          (match match pk with
                 | PIdirect (x', cs, sc) ->
                   let vars0 =
                     match sc with
                     | Slocal -> stack
                     | Sglob -> globals0
                   in
                   (match Mvar.get vars0 (Obj.magic x') with
                    | Some y ->
                      let (ofs', ws') = y in
                      if (&&)
                           (cmp_le Z.compare (size_of (Var.vtype x))
                             cs.cs_len)
                           ((&&) (cmp_le Z.compare Z0 cs.cs_ofs)
                             (cmp_le Z.compare (Z.add cs.cs_ofs cs.cs_len)
                               (size_of (Var.vtype x'))))
                      then let rmap0 =
                             match sc with
                             | Slocal ->
                               let sr = sub_region_stack x' ws' cs in
                               set_move rmap (Obj.magic x) sr Valid
                             | Sglob -> rmap
                           in
                           Ok ((sv, (Pdirect (x', ofs', ws', cs, sc))), rmap0)
                      else Error (E.stk_ierror_no_var "invalid slot")
                    | None -> Error (E.stk_ierror_no_var "unknown region"))
                 | PIregptr p0 ->
                   if negb (OtherDefs.is_aarr (Var.vtype x))
                   then Error
                          (E.stk_ierror_no_var
                            "a reg ptr variable must be an array")
                   else if SvExtra.Sv.mem (Obj.magic p0) sv
                        then Error
                               (E.stk_ierror_no_var
                                 "invalid reg pointer already exists")
                        else (match Mvar.get locals0 (Obj.magic p0) with
                              | Some _ ->
                                Error
                                  (E.stk_ierror_no_var
                                    "a pointer is equal to a program var")
                              | None ->
                                if negb
                                     (convertible (Var.vtype p0) (Coq_aword
                                       pd))
                                then Error
                                       (E.stk_ierror_no_var
                                         "invalid pointer type")
                                else Ok (((SvExtra.Sv.add (Obj.magic p0) sv),
                                       (Pregptr p0)), rmap))
                 | PIstkptr (x', cs, xp) ->
                   if negb (OtherDefs.is_aarr (Var.vtype x))
                   then Error
                          (E.stk_ierror_no_var
                            "a stk ptr variable must be an array")
                   else (match Mvar.get stack (Obj.magic x') with
                         | Some p0 ->
                           let (ofs', ws') = p0 in
                           if SvExtra.Sv.mem (Obj.magic xp) sv
                           then Error
                                  (E.stk_ierror_no_var
                                    "invalid stk ptr (not unique)")
                           else if eq_op
                                     Var.coq_MvMake_var__canonical__eqtype_Equality
                                     (Obj.magic xp) (Obj.magic x)
                                then Error
                                       (E.stk_ierror_no_var
                                         "a pseudo-var is equal to a program var")
                                else (match Mvar.get locals0 (Obj.magic xp) with
                                      | Some _ ->
                                        Error
                                          (E.stk_ierror_no_var
                                            "a pseudo-var is equal to a program var")
                                      | None ->
                                        if (&&) (cmp_le wsize_cmp pd ws')
                                             ((&&)
                                               (cmp_le Z.compare Z0 cs.cs_ofs)
                                               ((&&)
                                                 (eq_op
                                                   coq_BinNums_Z__canonical__eqtype_Equality
                                                   (Obj.magic Z.coq_land
                                                     cs.cs_ofs
                                                     (Z.sub (wsize_size pd)
                                                       (Zpos Coq_xH)))
                                                   (Obj.magic Z0))
                                                 ((&&)
                                                   (cmp_le Z.compare
                                                     (wsize_size pd)
                                                     cs.cs_len)
                                                   (cmp_le Z.compare
                                                     (Z.add cs.cs_ofs
                                                       cs.cs_len)
                                                     (size_of (Var.vtype x'))))))
                                        then Ok
                                               (((SvExtra.Sv.add
                                                   (Obj.magic xp) sv),
                                               (Pstkptr (x', ofs', ws', cs,
                                               xp))), rmap)
                                        else Error
                                               (E.stk_ierror_no_var
                                                 "invalid ptr kind"))
                         | None ->
                           Error (E.stk_ierror_no_var "unknown stack region")) with
           | Ok x0 ->
             let (y, rmap0) = x0 in
             let (sv0, pk0) = y in
             let locals1 = Mvar.set locals0 (Obj.magic x) pk0 in
             Ok ((locals1, rmap0), sv0)
           | Error s -> Error s))

(** val init_local_map :
    coq_PointerData -> SvExtra.Sv.elt -> SvExtra.Sv.elt -> SvExtra.Sv.elt ->
    (coq_Z * wsize) Mvar.t -> (coq_Z * wsize) Mvar.t -> stk_alloc_oracle_t ->
    (pp_error_loc, (ptr_kind Mvar.t * region_map) * SvExtra.Sv.t) result **)

let init_local_map pd vrip0 vrsp0 vxlen0 globals0 stack sao =
  if negb (eq_op CmpVar.t vxlen0 vrip0)
  then if negb (eq_op CmpVar.t vxlen0 vrsp0)
       then let sv =
              SvExtra.Sv.add vxlen0
                (SvExtra.Sv.add vrip0 (SvExtra.Sv.add vrsp0 SvExtra.Sv.empty))
            in
            (match foldM (add_alloc pd globals0 stack) ((Mvar.empty, empty),
                     sv) sao.sao_alloc with
             | Ok x -> Ok x
             | Error s -> Error s)
       else let s = E.stk_ierror_no_var "two fresh variables are equal" in
            Error s
  else let s = E.stk_ierror_no_var "two fresh variables are equal" in Error s

(** val check_result :
    pos_map -> region_map -> Equality.sort option list -> var_i list -> nat
    option -> var_i -> (pp_error_loc, var_i) result **)

let check_result pmap rmap paramsi params oi x =
  match oi with
  | Some i ->
    (match nth None paramsi i with
     | Some sr_param ->
       if convertible (Var.vtype x.v_var) (Var.vtype (nth x params i).v_var)
       then (match get_sub_region_status rmap x with
             | Ok x0 ->
               let (sr, status0) = x0 in
               (match check_valid x status0 with
                | Ok _ ->
                  if eq_op stack_alloc_sub_region__canonical__eqtype_Equality
                       sr_param (Obj.magic sr)
                  then (match get_regptr pmap x with
                        | Ok x1 -> Ok x1
                        | Error s -> Error s)
                  else let s =
                         E.stk_error x
                           (pp_box ((PPEstring
                             "the returned pointer") :: ((pp_nobox
                                                           ((PPEstring
                                                           "(") :: ((PPEvar
                                                           x.v_var) :: ((PPEstring
                                                           ")") :: [])))) :: ((PPEstring
                             "is one of the parameters,") :: ((PPEstring
                             "but it does not point to the region it pointed to initially") :: [])))))
                       in
                       Error s
                | Error s -> Error s)
             | Error s -> Error s)
       else let s =
              E.stk_ierror_no_var
                "reg ptr in result not corresponding to a parameter"
            in
            Error s
     | None -> Error (E.stk_ierror_no_var "invalid function info"))
  | None ->
    (match check_var pmap x with
     | Ok _ ->
       (match check_diff pmap x with
        | Ok _ -> Ok x
        | Error s -> Error s)
     | Error s -> Error s)

(** val check_all_writable_regions_returned :
    sub_region option list -> nat option list -> bool **)

let check_all_writable_regions_returned paramsi ret_pos =
  all2 (fun i osr ->
    match osr with
    | Some sr ->
      if sr.sr_region.r_writable
      then in_mem (Obj.magic (Some i))
             (mem
               (seq_predType
                 (coq_Datatypes_option__canonical__eqtype_Equality
                   coq_Datatypes_nat__canonical__eqtype_Equality))
               (Obj.magic ret_pos))
      else true
    | None -> true) (iota O (size paramsi)) paramsi

(** val check_results :
    pos_map -> region_map -> Equality.sort option list -> var_i list -> nat
    option list -> var_i list -> (pp_error_loc, var_i list) result **)

let check_results pmap rmap paramsi params ret_pos res =
  if check_all_writable_regions_returned (Obj.magic paramsi) ret_pos
  then mapM2 (E.stk_ierror_no_var "invalid function info")
         (check_result pmap rmap paramsi params) ret_pos res
  else let s = E.stk_ierror_no_var "a writable region is not returned" in
       Error s

(** val init_param :
    coq_PointerData -> (coq_Z * wsize) Mvar.t -> (coq_Z * wsize) Mvar.t ->
    ((SvExtra.Sv.t * ptr_kind Mvar.t) * region_map) -> param_info option ->
    var_i -> (pp_error_loc, ((SvExtra.Sv.t * ptr_kind
    Mvar.Map.t) * region_map) * (sub_region option * var_i)) result **)

let init_param pd mglob stack accu pi x =
  let (y, rmap) = accu in
  let (disj, lmap) = y in
  if negb (SvExtra.Sv.mem (Obj.magic x.v_var) disj)
  then (match Mvar.get lmap (Obj.magic x.v_var) with
        | Some _ ->
          Error
            (E.stk_ierror_no_var
              "a stack variable also occurs as a parameter")
        | None ->
          (match pi with
           | Some pi0 ->
             if convertible (Var.vtype pi0.pp_ptr) (Coq_aword pd)
             then if negb (SvExtra.Sv.mem (Obj.magic pi0.pp_ptr) disj)
                  then if OtherDefs.is_aarr (Var.vtype x.v_var)
                       then (match Mvar.get lmap (Obj.magic pi0.pp_ptr) with
                             | Some _ ->
                               Error
                                 (E.stk_ierror_no_var
                                   "a pointer is equal to a local var")
                             | None ->
                               (match Mvar.get mglob (Obj.magic x.v_var) with
                                | Some _ ->
                                  Error
                                    (E.stk_ierror_no_var
                                      "a region is both glob and param")
                                | None ->
                                  (match Mvar.get stack (Obj.magic x.v_var) with
                                   | Some _ ->
                                     Error
                                       (E.stk_ierror_no_var
                                         "a region is both stack and param")
                                   | None ->
                                     let r = { r_slot = x.v_var; r_align =
                                       pi0.pp_align; r_writable =
                                       pi0.pp_writable }
                                     in
                                     let sr = sub_region_full x.v_var r in
                                     Ok
                                     ((((SvExtra.Sv.add
                                          (Obj.magic pi0.pp_ptr) disj),
                                     (Mvar.set lmap (Obj.magic x.v_var)
                                       (Pregptr pi0.pp_ptr))),
                                     (set_move rmap (Obj.magic x.v_var) sr
                                       Valid)), ((Some sr),
                                     (with_var x pi0.pp_ptr))))))
                       else let s = E.stk_ierror_no_var "bad reg ptr type" in
                            Error s
                  else let s = E.stk_ierror_no_var "duplicate region" in
                       Error s
             else let s = E.stk_ierror_no_var "bad ptr type" in Error s
           | None -> Ok (accu, (None, x))))
  else let s = E.stk_ierror_no_var "a parameter already exists" in Error s

(** val init_params :
    coq_PointerData -> (coq_Z * wsize) Mvar.t -> (coq_Z * wsize) Mvar.t ->
    SvExtra.Sv.t -> ptr_kind Mvar.t -> region_map -> param_info option list
    -> var_i list -> (pp_error_loc, ((SvExtra.Sv.t * ptr_kind
    Mvar.t) * region_map) * (sub_region option * var_i) list) result **)

let init_params pd mglob stack disj lmap rmap sao_params0 params =
  fmapM2 (E.stk_ierror_no_var "invalid function info")
    (init_param pd mglob stack) ((disj, lmap), rmap) sao_params0 params

(** val fresh_reg :
    (v_kind -> Uint63.t -> string -> atype -> Ident.Ident.ident) -> string ->
    atype -> Ident.Ident.ident **)

let fresh_reg fresh_var_ident =
  fresh_var_ident (Reg (Normal, Direct)) (Uint63.of_int (0))

(** val alloc_fd_aux :
    bool -> coq_PointerData -> coq_MSFsize -> 'a1 asmOp -> 'a1 sh_params ->
    'a1 stack_alloc_params -> ('a1 asm_op_t -> bool) -> (v_kind -> Uint63.t
    -> string -> atype -> Ident.Ident.ident) -> (sub_region -> pp_error) ->
    'a1 _uprog -> sprog_extra -> (coq_Z * wsize) Mvar.t -> (funname ->
    stk_alloc_oracle_t) -> stk_alloc_oracle_t -> ('a1, unit) _fundef -> 'a1
    _ufundef cexec **)

let alloc_fd_aux check pd msfsz asmop shparams saparams is_move_op fresh_var_ident pp_sr p p_extra0 mglob local_alloc sao fd =
  let vrip0 = { Var.vtype = (Coq_aword pd); Var.vname = p_extra0.sp_rip } in
  let vrsp0 = { Var.vtype = (Coq_aword pd); Var.vname = p_extra0.sp_rsp } in
  let vxlen0 = { Var.vtype = (Coq_aword pd); Var.vname =
    (fresh_reg fresh_var_ident "__len__" (Coq_aword pd)) }
  in
  (match init_stack_layout mglob sao with
   | Ok x ->
     (match init_local_map pd (Obj.magic vrip0) (Obj.magic vrsp0)
              (Obj.magic vxlen0) mglob x sao with
      | Ok x0 ->
        let (y, disj) = x0 in
        let (locals0, rmap) = y in
        let table0 = { bindings = Mvar.empty; counter = (Uint63.of_int (0));
          vars = SvExtra.Sv.empty }
        in
        (match init_params pd mglob x disj locals0 rmap sao.sao_params
                 fd.f_params with
         | Ok x1 ->
           let (y0, alloc_params) = x1 in
           let (y1, rmap0) = y0 in
           let (sv, lmap) = y1 in
           let paramsi = map fst alloc_params in
           let params = map snd alloc_params in
           let pmap = { vrip = vrip0; vrsp = vrsp0; vxlen = vxlen0; globals =
             mglob; locals = lmap; vnew = sv }
           in
           if Z.leb Z0 sao.sao_extra_size
           then (match let local_size = sao_frame_size sao in
                       assert_check check (Z.leb local_size sao.sao_max_size)
                         (E.stk_ierror_no_var "sao_max_size too small") with
                 | Ok _ ->
                   (match fmapM
                            (alloc_i check pd msfsz asmop shparams saparams
                              is_move_op fresh_var_ident pp_sr pmap
                              local_alloc p sao) (table0, rmap0) fd.f_body with
                    | Ok x2 ->
                      let (y2, body) = x2 in
                      let (_, rmap1) = y2 in
                      (match check_results pmap rmap1 (Obj.magic paramsi)
                               fd.f_params sao.sao_return fd.f_res with
                       | Ok x3 ->
                         Ok { f_info = fd.f_info; f_tyin =
                           (map2 (fun o ty ->
                             match o with
                             | Some _ -> Coq_aword pd
                             | None -> ty) sao.sao_params fd.f_tyin);
                           f_params = params; f_body = (flatten body);
                           f_tyout =
                           (map2 (fun o ty ->
                             match o with
                             | Some _ -> Coq_aword pd
                             | None -> ty) sao.sao_return fd.f_tyout);
                           f_res = x3; f_extra = fd.f_extra }
                       | Error s -> Error s)
                    | Error s -> Error s)
                 | Error s -> Error s)
           else let s = E.stk_ierror_no_var "negative extra size" in Error s
         | Error s -> Error s)
      | Error s -> Error s)
   | Error s -> Error s)



  

(** val alloc_fd :
    bool -> coq_PointerData -> coq_MSFsize -> 'a1 asmOp -> 'a1 sh_params ->
    'a1 stack_alloc_params -> ('a1 asm_op_t -> bool) -> (v_kind -> Uint63.t
    -> string -> atype -> Ident.Ident.ident) -> (sub_region -> pp_error) ->
    'a1 _uprog -> sprog_extra -> (coq_Z * wsize) Mvar.t -> (funname ->
    stk_alloc_oracle_t) -> funname -> ('a1, unit) _fundef -> (pp_error_loc,
    'a1 sfundef) result **)

let alloc_fd check pd msfsz asmop shparams saparams is_move_op fresh_var_ident pp_sr p p_extra0 mglob local_alloc fn fd =
  match alloc_fd_aux check pd msfsz asmop shparams saparams is_move_op
          fresh_var_ident pp_sr p p_extra0 mglob local_alloc (local_alloc fn)
          fd with
  | Ok x ->
    let f_extra0 = { sf_align = (local_alloc fn).sao_align; sf_stk_sz =
      (local_alloc fn).sao_size; sf_stk_ioff = (local_alloc fn).sao_ioff;
      sf_stk_extra_sz = (local_alloc fn).sao_extra_size; sf_stk_max =
      (local_alloc fn).sao_max_size; sf_max_call_depth =
      (local_alloc fn).sao_max_call_depth; sf_to_save =
      (local_alloc fn).sao_to_save; sf_save_stack = (local_alloc fn).sao_rsp;
      sf_return_address = (local_alloc fn).sao_return_address;
      sf_align_args =
      (map (Ssrfun.Option.apply (fun p0 -> p0.pp_align) U8)
        (local_alloc fn).sao_params);
        (* NEW *)
     (* sf_mask_layout = build_mask_layout (local_alloc fn).sao_slots;*)  sf_random_layout = Expr.RandomnessMap.empty; sf_masking_layout = Expr.ShareMap.empty; sf_output_layout = Expr.ShareMap.empty;}
    in
    Ok (swith_extra pd asmop pd (Obj.magic x) (Obj.magic f_extra0))
  | Error s -> Error s

(** val ptake :
    positive -> 'a1 list -> 'a1 list -> ('a1 list * 'a1 list) option **)

let rec ptake p r l =
  match p with
  | Coq_xI p0 ->
    (match l with
     | [] -> None
     | x :: l0 ->
       (match ptake p0 (x :: r) l0 with
        | Some y -> let (r0, l1) = y in ptake p0 r0 l1
        | None -> None))
  | Coq_xO p0 ->
    (match ptake p0 r l with
     | Some p1 -> let (r0, l0) = p1 in ptake p0 r0 l0
     | None -> None)
  | Coq_xH -> (match l with
               | [] -> None
               | x :: l0 -> Some ((x :: r), l0))

(** val ztake : coq_Z -> 'a1 list -> ('a1 list * 'a1 list) option **)

let ztake z l =
  match z with
  | Z0 -> Some ([], l)
  | Zpos p ->
    (match ptake p [] l with
     | Some p0 -> let (r, l0) = p0 in Some ((rev r), l0)
     | None -> None)
  | Zneg _ -> None

(** val check_glob :
    GRing.ComRing.sort list -> glob_value -> (pp_error_loc, unit) result **)

let check_glob data = function
| Gword (ws, w) ->
  if eq_op
       (GRing.ComRing.Exports.coq_GRing_ComRing__to__eqtype_Equality
         (word ws)) (LE.decode ws data) w
  then Ok ()
  else Error (E.stk_ierror_no_var "bad decode")
| Garr (p, t0) ->
  (match foldM (fun wd i ->
           match CoreMem.read coq_BinNums_Z__canonical__eqtype_Equality
                   WArray.coq_PointerZ (WArray.array_CM p) t0 Aligned
                   (Obj.magic i) U8 with
           | Ok w ->
             if eq_op
                  (GRing.ComRing.Exports.coq_GRing_ComRing__to__eqtype_Equality
                    (word U8)) wd w
             then Ok (Z.add i (Zpos Coq_xH))
             else Error (E.stk_ierror_no_var "bad decode array eq")
           | Error _ -> Error (E.stk_ierror_no_var "bad decode array len"))
           Z0 data with
   | Ok _ -> Ok ()
   | Error s -> Error s)

(** val size_glob : glob_value -> coq_Z **)

let size_glob = function
| Gword (ws, _) -> wsize_size ws
| Garr (p, _) -> Zpos p

(** val init_map :
    ((Var.var * wsize) * coq_Z) list -> GRing.ComRing.sort list -> glob_decl
    list -> (coq_Z * wsize) Mvar.t cexec **)

let init_map l data gd =
  let add0 = fun vp globals0 ->
    let (p0, p) = vp in
    let (v, ws) = p0 in
    let (p1, data0) = globals0 in
    let (mvar, pos) = p1 in
    if Z.leb pos p
    then if eq_op coq_BinNums_Z__canonical__eqtype_Equality
              (Obj.magic Z.coq_land p (Z.sub (wsize_size ws) (Zpos Coq_xH)))
              (Obj.magic Z0)
         then let s = size_of (Var.vtype v) in
              (match ztake (Z.sub p pos) data0 with
               | Some p2 ->
                 let (_, data1) = p2 in
                 (match ztake s data1 with
                  | Some p3 ->
                    let (vdata, data2) = p3 in
                    (match assoc
                             Var.coq_MvMake_var__canonical__eqtype_Equality
                             (Obj.magic gd) (Obj.magic v) with
                     | Some gv0 ->
                       if eq_op coq_BinNums_Z__canonical__eqtype_Equality
                            (Obj.magic s) (Obj.magic size_glob gv0)
                       then (match check_glob vdata gv0 with
                             | Ok _ ->
                               Ok (((Mvar.set mvar (Obj.magic v) (p, ws)),
                                 (Z.add p s)), data2)
                             | Error s0 -> Error s0)
                       else let s0 = E.stk_ierror_no_var "bad size" in
                            Error s0
                     | None -> Error (E.stk_ierror_no_var "unknown var"))
                  | None -> Error (E.stk_ierror_no_var "bad data 2"))
               | None -> Error (E.stk_ierror_no_var "bad data 1"))
         else Error (E.stk_ierror_no_var "bad global alignment")
    else Error (E.stk_ierror_no_var "global overlap")
  in
  (match foldM add0 ((Mvar.empty, Z0), data) l with
   | Ok x ->
     let (y, _) = x in
     let (mvar, _) = y in
     if SvExtra.Sv.subset (SvExtra.sv_of_list (Obj.magic fst) gd)
          (SvExtra.sv_of_list (fun x0 -> fst (fst (Obj.magic x0))) l)
     then Ok mvar
     else let s = E.stk_ierror_no_var "missing globals" in Error s
   | Error s -> Error s)

(** val alloc_prog :
    bool -> coq_PointerData -> coq_MSFsize -> 'a1 asmOp -> 'a1 sh_params ->
    'a1 stack_alloc_params -> ('a1 asm_op_t -> bool) -> (v_kind -> Uint63.t
    -> string -> atype -> Ident.Ident.ident) -> (sub_region -> pp_error) ->
    Ident.Ident.ident -> Ident.Ident.ident -> GRing.ComRing.sort list ->
    ((Var.var * wsize) * coq_Z) list -> (funname -> stk_alloc_oracle_t) ->
    'a1 _uprog -> 'a1 _sprog cexec **)

let alloc_prog check pd msfsz asmop shparams saparams is_move_op fresh_var_ident pp_sr rip rsp global_data global_alloc local_alloc p =
  match init_map global_alloc global_data p.p_globs with
  | Ok x ->
    let p_extra0 = { sp_rsp = rsp; sp_rip = rip; sp_globs = global_data;
      sp_glob_names = global_alloc }
    in
    if negb (eq_op Ident.ident_eqType (Obj.magic rip) (Obj.magic rsp))
    then (match map_cfprog_name_gen (fun x0 -> x0.f_info)
                  (alloc_fd check pd msfsz asmop shparams saparams is_move_op
                    fresh_var_ident pp_sr p p_extra0 x local_alloc) p.p_funcs with
          | Ok x0 ->
            Ok { p_funcs = (Obj.magic x0); p_globs = []; p_extra = p_extra0 }
          | Error s -> Error s)
    else let s = E.stk_ierror_no_var "rip and rsp clash" in Error s
  | Error s -> Error s

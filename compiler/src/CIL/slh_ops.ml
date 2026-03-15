open BinNums
open Bool
open Eqb_core_defs
open Eqtype
open Utils0
open Wsize

type __ = Obj.t

type slh_op =
| SLHinit
| SLHupdate
| SLHmove
| SLHprotect of wsize
| SLHprotect_ptr of wsize * positive
| SLHprotect_ptr_fail of wsize * positive

(** val slh_op_tag : slh_op -> positive **)

let slh_op_tag = function
| SLHinit -> Coq_xH
| SLHupdate -> Coq_xO Coq_xH
| SLHmove -> Coq_xI Coq_xH
| SLHprotect _ -> Coq_xO (Coq_xO Coq_xH)
| SLHprotect_ptr (_, _) -> Coq_xI (Coq_xO Coq_xH)
| SLHprotect_ptr_fail (_, _) -> Coq_xO (Coq_xI Coq_xH)

type box_slh_op_SLHinit =
| Box_slh_op_SLHinit

type box_slh_op_SLHprotect_ptr = { coq_Box_slh_op_SLHprotect_ptr_0 : 
                                   wsize;
                                   coq_Box_slh_op_SLHprotect_ptr_1 : 
                                   positive }

type slh_op_fields_t = __

(** val slh_op_fields : slh_op -> slh_op_fields_t **)

let slh_op_fields = function
| SLHprotect h -> Obj.magic h
| SLHprotect_ptr (h, h0) ->
  Obj.magic { coq_Box_slh_op_SLHprotect_ptr_0 = h;
    coq_Box_slh_op_SLHprotect_ptr_1 = h0 }
| SLHprotect_ptr_fail (h, h0) ->
  Obj.magic { coq_Box_slh_op_SLHprotect_ptr_0 = h;
    coq_Box_slh_op_SLHprotect_ptr_1 = h0 }
| _ -> Obj.magic Box_slh_op_SLHinit

(** val slh_op_eqb_fields :
    (slh_op -> slh_op -> bool) -> positive -> slh_op_fields_t ->
    slh_op_fields_t -> bool **)

let slh_op_eqb_fields _ x x0 x1 =
  match x with
  | Coq_xI x2 ->
    (match x2 with
     | Coq_xO _ ->
       let { coq_Box_slh_op_SLHprotect_ptr_0 = box_slh_op_SLHprotect_ptr_0;
         coq_Box_slh_op_SLHprotect_ptr_1 = box_slh_op_SLHprotect_ptr_1 } =
         Obj.magic x0
       in
       let { coq_Box_slh_op_SLHprotect_ptr_0 = box_slh_op_SLHprotect_ptr_2;
         coq_Box_slh_op_SLHprotect_ptr_1 = box_slh_op_SLHprotect_ptr_3 } =
         Obj.magic x1
       in
       (&&)
         (wsize_eqb box_slh_op_SLHprotect_ptr_0 box_slh_op_SLHprotect_ptr_2)
         ((&&)
           (positive_eqb box_slh_op_SLHprotect_ptr_1
             box_slh_op_SLHprotect_ptr_3) true)
     | _ -> true)
  | Coq_xO x2 ->
    (match x2 with
     | Coq_xI _ ->
       let { coq_Box_slh_op_SLHprotect_ptr_0 = box_slh_op_SLHprotect_ptr_0;
         coq_Box_slh_op_SLHprotect_ptr_1 = box_slh_op_SLHprotect_ptr_1 } =
         Obj.magic x0
       in
       let { coq_Box_slh_op_SLHprotect_ptr_0 = box_slh_op_SLHprotect_ptr_2;
         coq_Box_slh_op_SLHprotect_ptr_1 = box_slh_op_SLHprotect_ptr_3 } =
         Obj.magic x1
       in
       (&&)
         (wsize_eqb box_slh_op_SLHprotect_ptr_0 box_slh_op_SLHprotect_ptr_2)
         ((&&)
           (positive_eqb box_slh_op_SLHprotect_ptr_1
             box_slh_op_SLHprotect_ptr_3) true)
     | Coq_xO _ -> (&&) (wsize_eqb (Obj.magic x0) (Obj.magic x1)) true
     | Coq_xH -> true)
  | Coq_xH -> true

(** val slh_op_eqb : slh_op -> slh_op -> bool **)

let slh_op_eqb x1 x2 =
  match x1 with
  | SLHprotect h ->
    eqb_body slh_op_tag slh_op_fields
      (Obj.magic slh_op_eqb_fields (fun _ _ -> true))
      (slh_op_tag (SLHprotect h)) h x2
  | SLHprotect_ptr (h, h0) ->
    eqb_body slh_op_tag slh_op_fields
      (Obj.magic slh_op_eqb_fields (fun _ _ -> true))
      (slh_op_tag (SLHprotect_ptr (h, h0)))
      { coq_Box_slh_op_SLHprotect_ptr_0 = h;
      coq_Box_slh_op_SLHprotect_ptr_1 = h0 } x2
  | SLHprotect_ptr_fail (h, h0) ->
    eqb_body slh_op_tag slh_op_fields
      (Obj.magic slh_op_eqb_fields (fun _ _ -> true))
      (slh_op_tag (SLHprotect_ptr_fail (h, h0)))
      { coq_Box_slh_op_SLHprotect_ptr_0 = h;
      coq_Box_slh_op_SLHprotect_ptr_1 = h0 } x2
  | x ->
    eqb_body slh_op_tag slh_op_fields
      (Obj.magic slh_op_eqb_fields (fun _ _ -> true)) (slh_op_tag x)
      Box_slh_op_SLHinit x2

(** val slh_op_eqb_OK : slh_op -> slh_op -> reflect **)

let slh_op_eqb_OK =
  iffP2 slh_op_eqb

(** val coq_HB_unnamed_factory_1 : slh_op Coq_hasDecEq.axioms_ **)

let coq_HB_unnamed_factory_1 =
  { Coq_hasDecEq.eq_op = slh_op_eqb; Coq_hasDecEq.eqP = slh_op_eqb_OK }

(** val slh_ops_slh_op__canonical__eqtype_Equality : Equality.coq_type **)

let slh_ops_slh_op__canonical__eqtype_Equality =
  Obj.magic coq_HB_unnamed_factory_1

(** val is_protect_ptr : slh_op -> (wsize * positive) option **)

let is_protect_ptr = function
| SLHprotect_ptr (ws, p) -> Some (ws, p)
| _ -> None

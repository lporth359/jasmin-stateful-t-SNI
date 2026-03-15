open BinNums
open Bool
open Eqb_core_defs
open Eqtype

type __ = Obj.t

type shift_kind =
| SLSL
| SLSR
| SASR
| SROR

type is_shift_kind =
| Coq_is_SLSL
| Coq_is_SLSR
| Coq_is_SASR
| Coq_is_SROR

(** val shift_kind_tag : shift_kind -> positive **)

let shift_kind_tag = function
| SLSL -> Coq_xH
| SLSR -> Coq_xO Coq_xH
| SASR -> Coq_xI Coq_xH
| SROR -> Coq_xO (Coq_xO Coq_xH)

(** val is_shift_kind_inhab : shift_kind -> is_shift_kind **)

let is_shift_kind_inhab = function
| SLSL -> Coq_is_SLSL
| SLSR -> Coq_is_SLSR
| SASR -> Coq_is_SASR
| SROR -> Coq_is_SROR

type box_shift_kind_SLSL =
| Box_shift_kind_SLSL

type shift_kind_fields_t = __

(** val shift_kind_fields : shift_kind -> shift_kind_fields_t **)

let shift_kind_fields _ =
  Obj.magic Box_shift_kind_SLSL

(** val shift_kind_eqb_fields :
    (shift_kind -> shift_kind -> bool) -> positive -> shift_kind_fields_t ->
    shift_kind_fields_t -> bool **)

let shift_kind_eqb_fields _ _ _ _ =
  true

(** val shift_kind_eqb : shift_kind -> shift_kind -> bool **)

let shift_kind_eqb x1 x2 =
  eqb_body shift_kind_tag shift_kind_fields
    (Obj.magic shift_kind_eqb_fields (fun _ _ -> true)) (shift_kind_tag x1)
    Box_shift_kind_SLSL x2

(** val shift_kind_eqb_OK : shift_kind -> shift_kind -> reflect **)

let shift_kind_eqb_OK =
  iffP2 shift_kind_eqb

(** val coq_HB_unnamed_factory_1 : shift_kind Coq_hasDecEq.axioms_ **)

let coq_HB_unnamed_factory_1 =
  { Coq_hasDecEq.eq_op = shift_kind_eqb; Coq_hasDecEq.eqP =
    shift_kind_eqb_OK }

(** val shift_kind_shift_kind__canonical__eqtype_Equality :
    Equality.coq_type **)

let shift_kind_shift_kind__canonical__eqtype_Equality =
  Obj.magic coq_HB_unnamed_factory_1

(** val shift_amount_bounds : shift_kind -> coq_Z * coq_Z **)

let shift_amount_bounds = function
| SLSL -> (Z0, (Zpos (Coq_xI (Coq_xI (Coq_xI (Coq_xI Coq_xH))))))
| SROR -> ((Zpos Coq_xH), (Zpos (Coq_xI (Coq_xI (Coq_xI (Coq_xI Coq_xH))))))
| _ ->
  ((Zpos Coq_xH), (Zpos (Coq_xO (Coq_xO (Coq_xO (Coq_xO (Coq_xO Coq_xH)))))))

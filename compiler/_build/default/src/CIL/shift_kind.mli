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

val shift_kind_tag : shift_kind -> positive

val is_shift_kind_inhab : shift_kind -> is_shift_kind

type box_shift_kind_SLSL =
| Box_shift_kind_SLSL

type shift_kind_fields_t = __

val shift_kind_fields : shift_kind -> shift_kind_fields_t

val shift_kind_eqb_fields :
  (shift_kind -> shift_kind -> bool) -> positive -> shift_kind_fields_t ->
  shift_kind_fields_t -> bool

val shift_kind_eqb : shift_kind -> shift_kind -> bool

val shift_kind_eqb_OK : shift_kind -> shift_kind -> reflect

val coq_HB_unnamed_factory_1 : shift_kind Coq_hasDecEq.axioms_

val shift_kind_shift_kind__canonical__eqtype_Equality : Equality.coq_type

val shift_amount_bounds : shift_kind -> coq_Z * coq_Z

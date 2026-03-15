open BinNums
open Bool
open Datatypes
open Eqb_core_defs
open Eqtype
open Utils0

type __ = Obj.t

type wsize =
| U8
| U16
| U32
| U64
| U128
| U256

type is_wsize =
| Coq_is_U8
| Coq_is_U16
| Coq_is_U32
| Coq_is_U64
| Coq_is_U128
| Coq_is_U256

val wsize_tag : wsize -> positive

val is_wsize_inhab : wsize -> is_wsize

type box_wsize_U8 =
| Box_wsize_U8

type wsize_fields_t = __

val wsize_fields : wsize -> wsize_fields_t

val wsize_eqb_fields :
  (wsize -> wsize -> bool) -> positive -> wsize_fields_t -> wsize_fields_t ->
  bool

val wsize_eqb : wsize -> wsize -> bool

val wsize_eqb_OK : wsize -> wsize -> reflect

val wsize_size : wsize -> coq_Z

type velem =
| VE8
| VE16
| VE32
| VE64

type is_velem =
| Coq_is_VE8
| Coq_is_VE16
| Coq_is_VE32
| Coq_is_VE64

val velem_tag : velem -> positive

val is_velem_inhab : velem -> is_velem

type box_velem_VE8 =
| Box_velem_VE8

type velem_fields_t = __

val velem_fields : velem -> velem_fields_t

val velem_eqb_fields :
  (velem -> velem -> bool) -> positive -> velem_fields_t -> velem_fields_t ->
  bool

val velem_eqb : velem -> velem -> bool

val wsize_of_velem : velem -> wsize

type pelem =
| PE1
| PE2
| PE4
| PE8
| PE16
| PE32
| PE64
| PE128

type is_pelem =
| Coq_is_PE1
| Coq_is_PE2
| Coq_is_PE4
| Coq_is_PE8
| Coq_is_PE16
| Coq_is_PE32
| Coq_is_PE64
| Coq_is_PE128

val pelem_tag : pelem -> positive

val is_pelem_inhab : pelem -> is_pelem

type box_pelem_PE1 =
| Box_pelem_PE1

type pelem_fields_t = __

val pelem_fields : pelem -> pelem_fields_t

val pelem_eqb_fields :
  (pelem -> pelem -> bool) -> positive -> pelem_fields_t -> pelem_fields_t ->
  bool

val pelem_eqb : pelem -> pelem -> bool

type signedness =
| Signed
| Unsigned

type is_signedness =
| Coq_is_Signed
| Coq_is_Unsigned

val signedness_tag : signedness -> positive

val is_signedness_inhab : signedness -> is_signedness

type box_signedness_Signed =
| Box_signedness_Signed

type signedness_fields_t = __

val signedness_fields : signedness -> signedness_fields_t

val signedness_eqb_fields :
  (signedness -> signedness -> bool) -> positive -> signedness_fields_t ->
  signedness_fields_t -> bool

val signedness_eqb : signedness -> signedness -> bool

val signedness_eqb_OK : signedness -> signedness -> reflect

val coq_HB_unnamed_factory_1 : signedness Coq_hasDecEq.axioms_

val wsize_signedness__canonical__eqtype_Equality : Equality.coq_type

val coq_HB_unnamed_factory_3 : wsize Coq_hasDecEq.axioms_

val wsize_wsize__canonical__eqtype_Equality : Equality.coq_type

val wsize_eq_dec : wsize -> wsize -> bool

val wsizes : wsize list

val wsize_cmp : wsize -> wsize -> comparison

val size_8_16 : wsize -> bool

val size_8_32 : wsize -> bool

val size_8_64 : wsize -> bool

val size_16_32 : wsize -> bool

val size_16_64 : wsize -> bool

val size_32_64 : wsize -> bool

val size_64_128 : wsize -> bool

val size_128_256 : wsize -> bool

val string_of_wsize : wsize -> string

val string_of_ve_sz : velem -> wsize -> string

val pp_s : string -> unit -> string

val pp_sz : string -> wsize -> unit -> string

val pp_ve_sz : string -> velem -> wsize -> unit -> string

val pp_ve_sz_ve_sz :
  string -> velem -> wsize -> velem -> wsize -> unit -> string

val pp_sz_sz : string -> bool -> wsize -> wsize -> unit -> string

type reg_kind =
| Normal
| Extra

type is_reg_kind =
| Coq_is_Normal
| Coq_is_Extra

val reg_kind_tag : reg_kind -> positive

val is_reg_kind_inhab : reg_kind -> is_reg_kind

type box_reg_kind_Normal =
| Box_reg_kind_Normal

type reg_kind_fields_t = __

val reg_kind_fields : reg_kind -> reg_kind_fields_t

val reg_kind_eqb_fields :
  (reg_kind -> reg_kind -> bool) -> positive -> reg_kind_fields_t ->
  reg_kind_fields_t -> bool

val reg_kind_eqb : reg_kind -> reg_kind -> bool

type writable =
| Constant
| Writable

type reference =
| Direct
| Pointer of writable

type v_kind =
| Const
| Stack of reference
| Reg of (reg_kind * reference)
| Inline
| Global

type safe_cond =
| NotZero of wsize * nat
| X86Division of wsize * signedness
| InRangeMod32 of wsize * coq_Z * coq_Z * nat
| ULt of wsize * nat * coq_Z
| UGe of wsize * coq_Z * nat
| UaddLe of wsize * nat * nat * coq_Z
| AllInit of wsize * positive * nat
| ScFalse

type coq_PointerData =
  wsize
  (* singleton inductive, whose constructor was Build_PointerData *)

type coq_MSFsize =
  wsize
  (* singleton inductive, whose constructor was Build_MSFsize *)

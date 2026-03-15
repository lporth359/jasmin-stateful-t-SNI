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

(** val wsize_tag : wsize -> positive **)

let wsize_tag = function
| U8 -> Coq_xH
| U16 -> Coq_xO Coq_xH
| U32 -> Coq_xI Coq_xH
| U64 -> Coq_xO (Coq_xO Coq_xH)
| U128 -> Coq_xI (Coq_xO Coq_xH)
| U256 -> Coq_xO (Coq_xI Coq_xH)

(** val is_wsize_inhab : wsize -> is_wsize **)

let is_wsize_inhab = function
| U8 -> Coq_is_U8
| U16 -> Coq_is_U16
| U32 -> Coq_is_U32
| U64 -> Coq_is_U64
| U128 -> Coq_is_U128
| U256 -> Coq_is_U256

type box_wsize_U8 =
| Box_wsize_U8

type wsize_fields_t = __

(** val wsize_fields : wsize -> wsize_fields_t **)

let wsize_fields _ =
  Obj.magic Box_wsize_U8

(** val wsize_eqb_fields :
    (wsize -> wsize -> bool) -> positive -> wsize_fields_t -> wsize_fields_t
    -> bool **)

let wsize_eqb_fields _ _ _ _ =
  true

(** val wsize_eqb : wsize -> wsize -> bool **)

let wsize_eqb x1 x2 =
  eqb_body wsize_tag wsize_fields
    (Obj.magic wsize_eqb_fields (fun _ _ -> true)) (wsize_tag x1)
    Box_wsize_U8 x2

(** val wsize_eqb_OK : wsize -> wsize -> reflect **)

let wsize_eqb_OK =
  iffP2 wsize_eqb

(** val wsize_size : wsize -> coq_Z **)

let wsize_size sz =
  Zpos
    (match sz with
     | U8 -> Coq_xH
     | U16 -> Coq_xO Coq_xH
     | U32 -> Coq_xO (Coq_xO Coq_xH)
     | U64 -> Coq_xO (Coq_xO (Coq_xO Coq_xH))
     | U128 -> Coq_xO (Coq_xO (Coq_xO (Coq_xO Coq_xH)))
     | U256 -> Coq_xO (Coq_xO (Coq_xO (Coq_xO (Coq_xO Coq_xH)))))

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

(** val velem_tag : velem -> positive **)

let velem_tag = function
| VE8 -> Coq_xH
| VE16 -> Coq_xO Coq_xH
| VE32 -> Coq_xI Coq_xH
| VE64 -> Coq_xO (Coq_xO Coq_xH)

(** val is_velem_inhab : velem -> is_velem **)

let is_velem_inhab = function
| VE8 -> Coq_is_VE8
| VE16 -> Coq_is_VE16
| VE32 -> Coq_is_VE32
| VE64 -> Coq_is_VE64

type box_velem_VE8 =
| Box_velem_VE8

type velem_fields_t = __

(** val velem_fields : velem -> velem_fields_t **)

let velem_fields _ =
  Obj.magic Box_velem_VE8

(** val velem_eqb_fields :
    (velem -> velem -> bool) -> positive -> velem_fields_t -> velem_fields_t
    -> bool **)

let velem_eqb_fields _ _ _ _ =
  true

(** val velem_eqb : velem -> velem -> bool **)

let velem_eqb x1 x2 =
  eqb_body velem_tag velem_fields
    (Obj.magic velem_eqb_fields (fun _ _ -> true)) (velem_tag x1)
    Box_velem_VE8 x2

(** val wsize_of_velem : velem -> wsize **)

let wsize_of_velem = function
| VE8 -> U8
| VE16 -> U16
| VE32 -> U32
| VE64 -> U64

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

(** val pelem_tag : pelem -> positive **)

let pelem_tag = function
| PE1 -> Coq_xH
| PE2 -> Coq_xO Coq_xH
| PE4 -> Coq_xI Coq_xH
| PE8 -> Coq_xO (Coq_xO Coq_xH)
| PE16 -> Coq_xI (Coq_xO Coq_xH)
| PE32 -> Coq_xO (Coq_xI Coq_xH)
| PE64 -> Coq_xI (Coq_xI Coq_xH)
| PE128 -> Coq_xO (Coq_xO (Coq_xO Coq_xH))

(** val is_pelem_inhab : pelem -> is_pelem **)

let is_pelem_inhab = function
| PE1 -> Coq_is_PE1
| PE2 -> Coq_is_PE2
| PE4 -> Coq_is_PE4
| PE8 -> Coq_is_PE8
| PE16 -> Coq_is_PE16
| PE32 -> Coq_is_PE32
| PE64 -> Coq_is_PE64
| PE128 -> Coq_is_PE128

type box_pelem_PE1 =
| Box_pelem_PE1

type pelem_fields_t = __

(** val pelem_fields : pelem -> pelem_fields_t **)

let pelem_fields _ =
  Obj.magic Box_pelem_PE1

(** val pelem_eqb_fields :
    (pelem -> pelem -> bool) -> positive -> pelem_fields_t -> pelem_fields_t
    -> bool **)

let pelem_eqb_fields _ _ _ _ =
  true

(** val pelem_eqb : pelem -> pelem -> bool **)

let pelem_eqb x1 x2 =
  eqb_body pelem_tag pelem_fields
    (Obj.magic pelem_eqb_fields (fun _ _ -> true)) (pelem_tag x1)
    Box_pelem_PE1 x2

type signedness =
| Signed
| Unsigned

type is_signedness =
| Coq_is_Signed
| Coq_is_Unsigned

(** val signedness_tag : signedness -> positive **)

let signedness_tag = function
| Signed -> Coq_xH
| Unsigned -> Coq_xO Coq_xH

(** val is_signedness_inhab : signedness -> is_signedness **)

let is_signedness_inhab = function
| Signed -> Coq_is_Signed
| Unsigned -> Coq_is_Unsigned

type box_signedness_Signed =
| Box_signedness_Signed

type signedness_fields_t = __

(** val signedness_fields : signedness -> signedness_fields_t **)

let signedness_fields _ =
  Obj.magic Box_signedness_Signed

(** val signedness_eqb_fields :
    (signedness -> signedness -> bool) -> positive -> signedness_fields_t ->
    signedness_fields_t -> bool **)

let signedness_eqb_fields _ _ _ _ =
  true

(** val signedness_eqb : signedness -> signedness -> bool **)

let signedness_eqb x1 x2 =
  eqb_body signedness_tag signedness_fields
    (Obj.magic signedness_eqb_fields (fun _ _ -> true)) (signedness_tag x1)
    Box_signedness_Signed x2

(** val signedness_eqb_OK : signedness -> signedness -> reflect **)

let signedness_eqb_OK =
  iffP2 signedness_eqb

(** val coq_HB_unnamed_factory_1 : signedness Coq_hasDecEq.axioms_ **)

let coq_HB_unnamed_factory_1 =
  { Coq_hasDecEq.eq_op = signedness_eqb; Coq_hasDecEq.eqP =
    signedness_eqb_OK }

(** val wsize_signedness__canonical__eqtype_Equality : Equality.coq_type **)

let wsize_signedness__canonical__eqtype_Equality =
  Obj.magic coq_HB_unnamed_factory_1

(** val coq_HB_unnamed_factory_3 : wsize Coq_hasDecEq.axioms_ **)

let coq_HB_unnamed_factory_3 =
  { Coq_hasDecEq.eq_op = wsize_eqb; Coq_hasDecEq.eqP = wsize_eqb_OK }

(** val wsize_wsize__canonical__eqtype_Equality : Equality.coq_type **)

let wsize_wsize__canonical__eqtype_Equality =
  Obj.magic coq_HB_unnamed_factory_3

(** val wsize_eq_dec : wsize -> wsize -> bool **)

let wsize_eq_dec ws1 ws2 =
  reflect_dec (wsize_eqb ws1 ws2) (wsize_eqb_OK ws1 ws2)

(** val wsizes : wsize list **)

let wsizes =
  U8 :: (U16 :: (U32 :: (U64 :: (U128 :: (U256 :: [])))))

(** val wsize_cmp : wsize -> wsize -> comparison **)

let wsize_cmp s s' =
  match s with
  | U8 -> (match s' with
           | U8 -> Eq
           | _ -> Lt)
  | U16 -> (match s' with
            | U8 -> Gt
            | U16 -> Eq
            | _ -> Lt)
  | U32 -> (match s' with
            | U8 -> Gt
            | U16 -> Gt
            | U32 -> Eq
            | _ -> Lt)
  | U64 -> (match s' with
            | U64 -> Eq
            | U128 -> Lt
            | U256 -> Lt
            | _ -> Gt)
  | U128 -> (match s' with
             | U128 -> Eq
             | U256 -> Lt
             | _ -> Gt)
  | U256 -> (match s' with
             | U256 -> Eq
             | _ -> Gt)

(** val size_8_16 : wsize -> bool **)

let size_8_16 sz =
  cmp_le wsize_cmp sz U16

(** val size_8_32 : wsize -> bool **)

let size_8_32 sz =
  cmp_le wsize_cmp sz U32

(** val size_8_64 : wsize -> bool **)

let size_8_64 sz =
  cmp_le wsize_cmp sz U64

(** val size_16_32 : wsize -> bool **)

let size_16_32 sz =
  (&&) (cmp_le wsize_cmp U16 sz) (cmp_le wsize_cmp sz U32)

(** val size_16_64 : wsize -> bool **)

let size_16_64 sz =
  (&&) (cmp_le wsize_cmp U16 sz) (cmp_le wsize_cmp sz U64)

(** val size_32_64 : wsize -> bool **)

let size_32_64 sz =
  (&&) (cmp_le wsize_cmp U32 sz) (cmp_le wsize_cmp sz U64)

(** val size_64_128 : wsize -> bool **)

let size_64_128 sz =
  (&&) (cmp_le wsize_cmp U64 sz) (cmp_le wsize_cmp sz U128)

(** val size_128_256 : wsize -> bool **)

let size_128_256 sz =
  (&&) (cmp_le wsize_cmp U128 sz) (cmp_le wsize_cmp sz U256)

(** val string_of_wsize : wsize -> string **)

let string_of_wsize = function
| U8 -> "8"
| U16 -> "16"
| U32 -> "32"
| U64 -> "64"
| U128 -> "128"
| U256 -> "256"

(** val string_of_ve_sz : velem -> wsize -> string **)

let string_of_ve_sz ve sz =
  match ve with
  | VE8 ->
    (match sz with
     | U8 -> "ERROR: please repport"
     | U16 -> "2u8"
     | U32 -> "4u8"
     | U64 -> "8u8"
     | U128 -> "16u8"
     | U256 -> "32u8")
  | VE16 ->
    (match sz with
     | U32 -> "2u16"
     | U64 -> "4u16"
     | U128 -> "8u16"
     | U256 -> "16u16"
     | _ -> "ERROR: please repport")
  | VE32 ->
    (match sz with
     | U64 -> "2u32"
     | U128 -> "4u32"
     | U256 -> "8u32"
     | _ -> "ERROR: please repport")
  | VE64 ->
    (match sz with
     | U64 -> "1u64"
     | U128 -> "2u64"
     | U256 -> "4u64"
     | _ -> "ERROR: please repport")

(** val pp_s : string -> unit -> string **)

let pp_s s _ =
  s

(** val pp_sz : string -> wsize -> unit -> string **)

let pp_sz s sz _ =
  (^) s ((^) "_" (string_of_wsize sz))

(** val pp_ve_sz : string -> velem -> wsize -> unit -> string **)

let pp_ve_sz s ve sz _ =
  (^) s ((^) "_" (string_of_ve_sz ve sz))

(** val pp_ve_sz_ve_sz :
    string -> velem -> wsize -> velem -> wsize -> unit -> string **)

let pp_ve_sz_ve_sz s ve sz ve' sz' _ =
  (^) s
    ((^) "_"
      ((^) (string_of_ve_sz ve sz) ((^) "_" (string_of_ve_sz ve' sz'))))

(** val pp_sz_sz : string -> bool -> wsize -> wsize -> unit -> string **)

let pp_sz_sz s sign sz sz' _ =
  (^) s
    ((^) "_u"
      ((^) (string_of_wsize sz)
        ((^) (if sign then "s" else "u") (string_of_wsize sz'))))

type reg_kind =
| Normal
| Extra

type is_reg_kind =
| Coq_is_Normal
| Coq_is_Extra

(** val reg_kind_tag : reg_kind -> positive **)

let reg_kind_tag = function
| Normal -> Coq_xH
| Extra -> Coq_xO Coq_xH

(** val is_reg_kind_inhab : reg_kind -> is_reg_kind **)

let is_reg_kind_inhab = function
| Normal -> Coq_is_Normal
| Extra -> Coq_is_Extra

type box_reg_kind_Normal =
| Box_reg_kind_Normal

type reg_kind_fields_t = __

(** val reg_kind_fields : reg_kind -> reg_kind_fields_t **)

let reg_kind_fields _ =
  Obj.magic Box_reg_kind_Normal

(** val reg_kind_eqb_fields :
    (reg_kind -> reg_kind -> bool) -> positive -> reg_kind_fields_t ->
    reg_kind_fields_t -> bool **)

let reg_kind_eqb_fields _ _ _ _ =
  true

(** val reg_kind_eqb : reg_kind -> reg_kind -> bool **)

let reg_kind_eqb x1 x2 =
  eqb_body reg_kind_tag reg_kind_fields
    (Obj.magic reg_kind_eqb_fields (fun _ _ -> true)) (reg_kind_tag x1)
    Box_reg_kind_Normal x2

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

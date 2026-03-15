open BinInt
open BinNums
open Eqb_core_defs
open Eqtype
open Param1
open Param1_trivial
open Ssralg
open Std
open Word0
open Word_ssrZ
open Wsize

type __ = Obj.t

type expand_immediate_kind =
| EI_none
| EI_byte
| EI_pattern
| EI_shift

(** val z_to_bytes : coq_Z -> ((coq_Z * coq_Z) * coq_Z) * coq_Z **)

let z_to_bytes n =
  let (n0, b0) =
    Z.div_eucl n (Zpos (Coq_xO (Coq_xO (Coq_xO (Coq_xO (Coq_xO (Coq_xO
      (Coq_xO (Coq_xO Coq_xH)))))))))
  in
  let (n1, b1) =
    Z.div_eucl n0 (Zpos (Coq_xO (Coq_xO (Coq_xO (Coq_xO (Coq_xO (Coq_xO
      (Coq_xO (Coq_xO Coq_xH)))))))))
  in
  let (n2, b2) =
    Z.div_eucl n1 (Zpos (Coq_xO (Coq_xO (Coq_xO (Coq_xO (Coq_xO (Coq_xO
      (Coq_xO (Coq_xO Coq_xH)))))))))
  in
  let b3 =
    Z.modulo n2 (Zpos (Coq_xO (Coq_xO (Coq_xO (Coq_xO (Coq_xO (Coq_xO (Coq_xO
      (Coq_xO Coq_xH)))))))))
  in
  (((b3, b2), b1), b0)

(** val is_ei_pattern : coq_Z -> bool **)

let is_ei_pattern n =
  let (p, b0) = z_to_bytes n in
  let (p0, b1) = p in
  let (b3, b2) = p0 in
  (||)
    ((&&)
      (eq_op coq_BinNums_Z__canonical__eqtype_Equality (Obj.magic b3)
        (Obj.magic b0))
      ((&&)
        (eq_op coq_BinNums_Z__canonical__eqtype_Equality (Obj.magic b2)
          (Obj.magic b0))
        (eq_op coq_BinNums_Z__canonical__eqtype_Equality (Obj.magic b1)
          (Obj.magic b0))))
    ((||)
      ((&&)
        (eq_op coq_BinNums_Z__canonical__eqtype_Equality (Obj.magic b3)
          (Obj.magic Z0))
        ((&&)
          (eq_op coq_BinNums_Z__canonical__eqtype_Equality (Obj.magic b2)
            (Obj.magic b0))
          (eq_op coq_BinNums_Z__canonical__eqtype_Equality (Obj.magic b1)
            (Obj.magic Z0))))
      ((&&)
        (eq_op coq_BinNums_Z__canonical__eqtype_Equality (Obj.magic b3)
          (Obj.magic b1))
        ((&&)
          (eq_op coq_BinNums_Z__canonical__eqtype_Equality (Obj.magic b2)
            (Obj.magic Z0))
          (eq_op coq_BinNums_Z__canonical__eqtype_Equality (Obj.magic b0)
            (Obj.magic Z0)))))

(** val is_ei_shift : coq_Z -> bool **)

let is_ei_shift n =
  let byte_end = Z.sub (Z.log2 n) (Zpos (Coq_xI (Coq_xI Coq_xH))) in
  eq_op coq_BinNums_Z__canonical__eqtype_Equality
    (Obj.magic Z.rem n (Z.pow (Zpos (Coq_xO Coq_xH)) byte_end)) (Obj.magic Z0)

(** val ei_kind : coq_Z -> expand_immediate_kind **)

let ei_kind n =
  if (&&) (Z.leb Z0 n)
       (Z.ltb n (Zpos (Coq_xO (Coq_xO (Coq_xO (Coq_xO (Coq_xO (Coq_xO (Coq_xO
         (Coq_xO Coq_xH))))))))))
  then EI_byte
  else if is_ei_pattern n
       then EI_pattern
       else if is_ei_shift n then EI_shift else EI_none

type wencoding =
| WE_allowed of bool
| W12_encoding
| W16_encoding

type is_wencoding =
| Coq_is_WE_allowed of bool * Param1.Coq_exports.is_bool
| Coq_is_W12_encoding
| Coq_is_W16_encoding

(** val wencoding_tag : wencoding -> positive **)

let wencoding_tag = function
| WE_allowed _ -> Coq_xH
| W12_encoding -> Coq_xO Coq_xH
| W16_encoding -> Coq_xI Coq_xH

(** val is_wencoding_inhab : wencoding -> is_wencoding **)

let is_wencoding_inhab = function
| WE_allowed h -> Coq_is_WE_allowed (h, (Coq_exports.is_bool_inhab h))
| W12_encoding -> Coq_is_W12_encoding
| W16_encoding -> Coq_is_W16_encoding

type box_wencoding_W12_encoding =
| Box_wencoding_W12_encoding

type wencoding_fields_t = __

(** val wencoding_fields : wencoding -> wencoding_fields_t **)

let wencoding_fields = function
| WE_allowed h -> Obj.magic h
| _ -> Obj.magic Box_wencoding_W12_encoding

(** val wencoding_eqb_fields :
    (wencoding -> wencoding -> bool) -> positive -> wencoding_fields_t ->
    wencoding_fields_t -> bool **)

let wencoding_eqb_fields _ x a b =
  match x with
  | Coq_xH -> (&&) (Prelude.bool_eqb (Obj.magic a) (Obj.magic b)) true
  | _ -> true

(** val wencoding_eqb : wencoding -> wencoding -> bool **)

let wencoding_eqb x1 x2 =
  match x1 with
  | WE_allowed h ->
    eqb_body wencoding_tag wencoding_fields
      (Obj.magic wencoding_eqb_fields (fun _ _ -> true))
      (wencoding_tag (WE_allowed h)) h x2
  | x ->
    eqb_body wencoding_tag wencoding_fields
      (Obj.magic wencoding_eqb_fields (fun _ _ -> true)) (wencoding_tag x)
      Box_wencoding_W12_encoding x2

type expected_wencoding = { on_shift : wencoding; on_none : wencoding }

type is_expected_wencoding =
| Coq_is_Build_expected_wencoding of wencoding * is_wencoding * wencoding
   * is_wencoding

(** val expected_wencoding_tag : expected_wencoding -> positive **)

let expected_wencoding_tag _ =
  Coq_xH

(** val is_expected_wencoding_inhab :
    expected_wencoding -> is_expected_wencoding **)

let is_expected_wencoding_inhab x =
  let { on_shift = on_shift0; on_none = on_none0 } = x in
  Coq_is_Build_expected_wencoding (on_shift0, (is_wencoding_inhab on_shift0),
  on_none0, (is_wencoding_inhab on_none0))

type box_expected_wencoding_Build_expected_wencoding = { coq_Box_expected_wencoding_Build_expected_wencoding_0 : 
                                                         wencoding;
                                                         coq_Box_expected_wencoding_Build_expected_wencoding_1 : 
                                                         wencoding }

type expected_wencoding_fields_t =
  box_expected_wencoding_Build_expected_wencoding

(** val expected_wencoding_fields :
    expected_wencoding -> expected_wencoding_fields_t **)

let expected_wencoding_fields i =
  let { on_shift = on_shift0; on_none = on_none0 } = i in
  { coq_Box_expected_wencoding_Build_expected_wencoding_0 = on_shift0;
  coq_Box_expected_wencoding_Build_expected_wencoding_1 = on_none0 }

(** val expected_wencoding_eqb_fields :
    (expected_wencoding -> expected_wencoding -> bool) -> positive ->
    box_expected_wencoding_Build_expected_wencoding ->
    box_expected_wencoding_Build_expected_wencoding -> bool **)

let expected_wencoding_eqb_fields _ _ a b =
  let { coq_Box_expected_wencoding_Build_expected_wencoding_0 =
    box_expected_wencoding_Build_expected_wencoding_0;
    coq_Box_expected_wencoding_Build_expected_wencoding_1 =
    box_expected_wencoding_Build_expected_wencoding_1 } = a
  in
  let { coq_Box_expected_wencoding_Build_expected_wencoding_0 =
    box_expected_wencoding_Build_expected_wencoding_2;
    coq_Box_expected_wencoding_Build_expected_wencoding_1 =
    box_expected_wencoding_Build_expected_wencoding_3 } = b
  in
  (&&)
    (wencoding_eqb box_expected_wencoding_Build_expected_wencoding_0
      box_expected_wencoding_Build_expected_wencoding_2)
    ((&&)
      (wencoding_eqb box_expected_wencoding_Build_expected_wencoding_1
        box_expected_wencoding_Build_expected_wencoding_3) true)

(** val expected_wencoding_eqb :
    expected_wencoding -> expected_wencoding -> bool **)

let expected_wencoding_eqb x1 x2 =
  let { on_shift = on_shift0; on_none = on_none0 } = x1 in
  eqb_body expected_wencoding_tag expected_wencoding_fields
    (expected_wencoding_eqb_fields (fun _ _ -> true))
    (expected_wencoding_tag { on_shift = on_shift0; on_none = on_none0 })
    { coq_Box_expected_wencoding_Build_expected_wencoding_0 = on_shift0;
    coq_Box_expected_wencoding_Build_expected_wencoding_1 = on_none0 } x2

(** val is_w12_encoding : coq_Z -> bool **)

let is_w12_encoding z =
  Z.ltb z
    (Z.pow (Zpos (Coq_xO Coq_xH)) (Zpos (Coq_xO (Coq_xO (Coq_xI Coq_xH)))))

(** val is_w16_encoding : coq_Z -> bool **)

let is_w16_encoding z =
  Z.ltb z
    (Z.pow (Zpos (Coq_xO Coq_xH)) (Zpos (Coq_xO (Coq_xO (Coq_xO (Coq_xO
      Coq_xH))))))

(** val check_wencoding : wencoding -> coq_Z -> bool **)

let check_wencoding we z =
  match we with
  | WE_allowed b -> b
  | W12_encoding -> is_w12_encoding z
  | W16_encoding -> is_w16_encoding z

(** val check_ei_kind :
    expected_wencoding -> wsize -> GRing.ComRing.sort -> bool **)

let check_ei_kind ewe sz w =
  let z = wunsigned sz w in
  (match ei_kind z with
   | EI_none -> check_wencoding ewe.on_none z
   | EI_shift -> check_wencoding ewe.on_shift z
   | _ -> true)

(** val string_of_ew : wencoding -> string **)

let string_of_ew = function
| WE_allowed b -> if b then "allowed" else "not allowed"
| W12_encoding -> "W12"
| W16_encoding -> "W16"

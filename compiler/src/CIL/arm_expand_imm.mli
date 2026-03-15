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

val z_to_bytes : coq_Z -> ((coq_Z * coq_Z) * coq_Z) * coq_Z

val is_ei_pattern : coq_Z -> bool

val is_ei_shift : coq_Z -> bool

val ei_kind : coq_Z -> expand_immediate_kind

type wencoding =
| WE_allowed of bool
| W12_encoding
| W16_encoding

type is_wencoding =
| Coq_is_WE_allowed of bool * Param1.Coq_exports.is_bool
| Coq_is_W12_encoding
| Coq_is_W16_encoding

val wencoding_tag : wencoding -> positive

val is_wencoding_inhab : wencoding -> is_wencoding

type box_wencoding_W12_encoding =
| Box_wencoding_W12_encoding

type wencoding_fields_t = __

val wencoding_fields : wencoding -> wencoding_fields_t

val wencoding_eqb_fields :
  (wencoding -> wencoding -> bool) -> positive -> wencoding_fields_t ->
  wencoding_fields_t -> bool

val wencoding_eqb : wencoding -> wencoding -> bool

type expected_wencoding = { on_shift : wencoding; on_none : wencoding }

type is_expected_wencoding =
| Coq_is_Build_expected_wencoding of wencoding * is_wencoding * wencoding
   * is_wencoding

val expected_wencoding_tag : expected_wencoding -> positive

val is_expected_wencoding_inhab : expected_wencoding -> is_expected_wencoding

type box_expected_wencoding_Build_expected_wencoding = { coq_Box_expected_wencoding_Build_expected_wencoding_0 : 
                                                         wencoding;
                                                         coq_Box_expected_wencoding_Build_expected_wencoding_1 : 
                                                         wencoding }

type expected_wencoding_fields_t =
  box_expected_wencoding_Build_expected_wencoding

val expected_wencoding_fields :
  expected_wencoding -> expected_wencoding_fields_t

val expected_wencoding_eqb_fields :
  (expected_wencoding -> expected_wencoding -> bool) -> positive ->
  box_expected_wencoding_Build_expected_wencoding ->
  box_expected_wencoding_Build_expected_wencoding -> bool

val expected_wencoding_eqb : expected_wencoding -> expected_wencoding -> bool

val is_w12_encoding : coq_Z -> bool

val is_w16_encoding : coq_Z -> bool

val check_wencoding : wencoding -> coq_Z -> bool

val check_ei_kind : expected_wencoding -> wsize -> GRing.ComRing.sort -> bool

val string_of_ew : wencoding -> string

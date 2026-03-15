open BinInt
open BinNums
open Bool
open Datatypes
open PeanoNat
open Arch_decl
open Arch_utils
open Arm_decl
open Arm_expand_imm
open EqbOK
open Eqb_core_defs
open Eqtype
open Fintype
open Memory_model
open Param1
open Param1_trivial
open Sem_type
open Seq
open Shift_kind
open Sopn
open Ssralg
open Ssrbool
open Ssrnat
open Std
open Type
open Utils0
open Word0
open Word_ssrZ
open Wsize
open Xseq

type __ = Obj.t
let __ = let rec f _ = Obj.repr f in Obj.repr f

module E =
 struct
  (** val no_semantics : error **)

  let no_semantics =
    ErrSemUndef
 end

type arm_options = { set_flags : bool; is_conditional : bool;
                     has_shift : shift_kind option }

(** val set_flags : arm_options -> bool **)

let set_flags record =
  record.set_flags

(** val is_conditional : arm_options -> bool **)

let is_conditional record =
  record.is_conditional

(** val has_shift : arm_options -> shift_kind option **)

let has_shift record =
  record.has_shift

type is_arm_options =
| Coq_is_Build_arm_options of bool * Param1.Coq_exports.is_bool * bool
   * Param1.Coq_exports.is_bool * shift_kind option
   * (shift_kind, is_shift_kind) Prelude.is_option

(** val is_arm_options_rect :
    (bool -> Param1.Coq_exports.is_bool -> bool -> Param1.Coq_exports.is_bool
    -> shift_kind option -> (shift_kind, is_shift_kind) Prelude.is_option ->
    'a1) -> arm_options -> is_arm_options -> 'a1 **)

let is_arm_options_rect f _ = function
| Coq_is_Build_arm_options (set_flags0, pset_flags, is_conditional0,
                            pis_conditional, has_shift0, phas_shift) ->
  f set_flags0 pset_flags is_conditional0 pis_conditional has_shift0
    phas_shift

(** val is_arm_options_rec :
    (bool -> Param1.Coq_exports.is_bool -> bool -> Param1.Coq_exports.is_bool
    -> shift_kind option -> (shift_kind, is_shift_kind) Prelude.is_option ->
    'a1) -> arm_options -> is_arm_options -> 'a1 **)

let is_arm_options_rec f _ = function
| Coq_is_Build_arm_options (set_flags0, pset_flags, is_conditional0,
                            pis_conditional, has_shift0, phas_shift) ->
  f set_flags0 pset_flags is_conditional0 pis_conditional has_shift0
    phas_shift

(** val arm_options_tag : arm_options -> positive **)

let arm_options_tag _ =
  Coq_xH

(** val is_arm_options_inhab : arm_options -> is_arm_options **)

let is_arm_options_inhab x =
  let { set_flags = set_flags0; is_conditional = is_conditional0; has_shift =
    has_shift0 } = x
  in
  Coq_is_Build_arm_options (set_flags0,
  (Coq_exports.is_bool_inhab set_flags0), is_conditional0,
  (Coq_exports.is_bool_inhab is_conditional0), has_shift0,
  (Prelude.is_option_inhab is_shift_kind_inhab has_shift0))

(** val is_arm_options_functor :
    arm_options -> is_arm_options -> is_arm_options **)

let rec is_arm_options_functor _ x =
  x

type box_arm_options_Build_arm_options = { coq_Box_arm_options_Build_arm_options_0 : 
                                           bool;
                                           coq_Box_arm_options_Build_arm_options_1 : 
                                           bool;
                                           coq_Box_arm_options_Build_arm_options_2 : 
                                           shift_kind option }

(** val coq_Box_arm_options_Build_arm_options_0 :
    box_arm_options_Build_arm_options -> bool **)

let coq_Box_arm_options_Build_arm_options_0 record =
  record.coq_Box_arm_options_Build_arm_options_0

(** val coq_Box_arm_options_Build_arm_options_1 :
    box_arm_options_Build_arm_options -> bool **)

let coq_Box_arm_options_Build_arm_options_1 record =
  record.coq_Box_arm_options_Build_arm_options_1

(** val coq_Box_arm_options_Build_arm_options_2 :
    box_arm_options_Build_arm_options -> shift_kind option **)

let coq_Box_arm_options_Build_arm_options_2 record =
  record.coq_Box_arm_options_Build_arm_options_2

type arm_options_fields_t = box_arm_options_Build_arm_options

(** val arm_options_fields : arm_options -> arm_options_fields_t **)

let arm_options_fields i =
  let { set_flags = set_flags0; is_conditional = is_conditional0; has_shift =
    has_shift0 } = i
  in
  { coq_Box_arm_options_Build_arm_options_0 = set_flags0;
  coq_Box_arm_options_Build_arm_options_1 = is_conditional0;
  coq_Box_arm_options_Build_arm_options_2 = has_shift0 }

(** val arm_options_construct :
    positive -> box_arm_options_Build_arm_options -> arm_options option **)

let arm_options_construct _ b =
  let { coq_Box_arm_options_Build_arm_options_0 =
    box_arm_options_Build_arm_options_0;
    coq_Box_arm_options_Build_arm_options_1 =
    box_arm_options_Build_arm_options_1;
    coq_Box_arm_options_Build_arm_options_2 =
    box_arm_options_Build_arm_options_2 } = b
  in
  Some { set_flags = box_arm_options_Build_arm_options_0; is_conditional =
  box_arm_options_Build_arm_options_1; has_shift =
  box_arm_options_Build_arm_options_2 }

(** val arm_options_induction :
    (bool -> Param1.Coq_exports.is_bool -> bool -> Param1.Coq_exports.is_bool
    -> shift_kind option -> (shift_kind, is_shift_kind) Prelude.is_option ->
    'a1) -> arm_options -> is_arm_options -> 'a1 **)

let arm_options_induction his_Build_arm_options _ = function
| Coq_is_Build_arm_options (set_flags0, pset_flags, is_conditional0,
                            pis_conditional, has_shift0, phas_shift) ->
  his_Build_arm_options set_flags0 pset_flags is_conditional0 pis_conditional
    has_shift0 phas_shift

(** val arm_options_eqb_fields :
    (arm_options -> arm_options -> bool) -> positive ->
    box_arm_options_Build_arm_options -> box_arm_options_Build_arm_options ->
    bool **)

let arm_options_eqb_fields _ _ a b =
  let { coq_Box_arm_options_Build_arm_options_0 =
    box_arm_options_Build_arm_options_0;
    coq_Box_arm_options_Build_arm_options_1 =
    box_arm_options_Build_arm_options_1;
    coq_Box_arm_options_Build_arm_options_2 =
    box_arm_options_Build_arm_options_2 } = a
  in
  let { coq_Box_arm_options_Build_arm_options_0 =
    box_arm_options_Build_arm_options_3;
    coq_Box_arm_options_Build_arm_options_1 =
    box_arm_options_Build_arm_options_4;
    coq_Box_arm_options_Build_arm_options_2 =
    box_arm_options_Build_arm_options_5 } = b
  in
  (&&)
    (Prelude.bool_eqb box_arm_options_Build_arm_options_0
      box_arm_options_Build_arm_options_3)
    ((&&)
      (Prelude.bool_eqb box_arm_options_Build_arm_options_1
        box_arm_options_Build_arm_options_4)
      ((&&)
        (Prelude.option_eqb shift_kind_eqb
          box_arm_options_Build_arm_options_2
          box_arm_options_Build_arm_options_5) true))

(** val arm_options_eqb : arm_options -> arm_options -> bool **)

let arm_options_eqb x1 x2 =
  let { set_flags = set_flags0; is_conditional = is_conditional0; has_shift =
    has_shift0 } = x1
  in
  eqb_body arm_options_tag arm_options_fields
    (arm_options_eqb_fields (fun _ _ -> true))
    (arm_options_tag { set_flags = set_flags0; is_conditional =
      is_conditional0; has_shift = has_shift0 })
    { coq_Box_arm_options_Build_arm_options_0 = set_flags0;
    coq_Box_arm_options_Build_arm_options_1 = is_conditional0;
    coq_Box_arm_options_Build_arm_options_2 = has_shift0 } x2

(** val arm_options_eqb_OK : arm_options -> arm_options -> reflect **)

let arm_options_eqb_OK =
  iffP2 arm_options_eqb

(** val arm_options_eqb_OK_sumbool : arm_options -> arm_options -> bool **)

let arm_options_eqb_OK_sumbool =
  reflect_dec arm_options_eqb arm_options_eqb_OK

(** val eqTC_arm_options : arm_options eqTypeC **)

let eqTC_arm_options =
  { beq = arm_options_eqb; ceqP = arm_options_eqb_OK }

(** val arm_options_eqType : Equality.coq_type **)

let arm_options_eqType =
  ceqT_eqType eqTC_arm_options

(** val default_opts : arm_options **)

let default_opts =
  { set_flags = false; is_conditional = false; has_shift = None }

(** val set_is_conditional : arm_options -> arm_options **)

let set_is_conditional ao =
  { set_flags = ao.set_flags; is_conditional = true; has_shift =
    ao.has_shift }

(** val unset_is_conditional : arm_options -> arm_options **)

let unset_is_conditional ao =
  { set_flags = ao.set_flags; is_conditional = false; has_shift =
    ao.has_shift }

type halfword =
| HWB
| HWT

(** val halfword_rect : 'a1 -> 'a1 -> halfword -> 'a1 **)

let halfword_rect f f0 = function
| HWB -> f
| HWT -> f0

(** val halfword_rec : 'a1 -> 'a1 -> halfword -> 'a1 **)

let halfword_rec f f0 = function
| HWB -> f
| HWT -> f0

type is_halfword =
| Coq_is_HWB
| Coq_is_HWT

(** val is_halfword_rect : 'a1 -> 'a1 -> halfword -> is_halfword -> 'a1 **)

let is_halfword_rect f f0 _ = function
| Coq_is_HWB -> f
| Coq_is_HWT -> f0

(** val is_halfword_rec : 'a1 -> 'a1 -> halfword -> is_halfword -> 'a1 **)

let is_halfword_rec f f0 _ = function
| Coq_is_HWB -> f
| Coq_is_HWT -> f0

(** val halfword_tag : halfword -> positive **)

let halfword_tag = function
| HWB -> Coq_xH
| HWT -> Coq_xO Coq_xH

(** val is_halfword_inhab : halfword -> is_halfword **)

let is_halfword_inhab = function
| HWB -> Coq_is_HWB
| HWT -> Coq_is_HWT

(** val is_halfword_functor : halfword -> is_halfword -> is_halfword **)

let rec is_halfword_functor _ x =
  x

type box_halfword_HWB =
| Box_halfword_HWB

type halfword_fields_t = __

(** val halfword_fields : halfword -> halfword_fields_t **)

let halfword_fields _ =
  Obj.magic Box_halfword_HWB

(** val halfword_construct :
    positive -> halfword_fields_t -> halfword option **)

let halfword_construct p _ =
  match p with
  | Coq_xI _ -> None
  | Coq_xO _ -> Some HWT
  | Coq_xH -> Some HWB

(** val halfword_induction : 'a1 -> 'a1 -> halfword -> is_halfword -> 'a1 **)

let halfword_induction his_HWB his_HWT _ = function
| Coq_is_HWB -> his_HWB
| Coq_is_HWT -> his_HWT

(** val halfword_eqb_fields :
    (halfword -> halfword -> bool) -> positive -> halfword_fields_t ->
    halfword_fields_t -> bool **)

let halfword_eqb_fields _ _ _ _ =
  true

(** val halfword_eqb : halfword -> halfword -> bool **)

let halfword_eqb x1 x2 =
  eqb_body halfword_tag halfword_fields
    (Obj.magic halfword_eqb_fields (fun _ _ -> true)) (halfword_tag x1)
    Box_halfword_HWB x2

(** val halfword_eqb_OK : halfword -> halfword -> reflect **)

let halfword_eqb_OK =
  iffP2 halfword_eqb

(** val halfword_eqb_OK_sumbool : halfword -> halfword -> bool **)

let halfword_eqb_OK_sumbool =
  reflect_dec halfword_eqb halfword_eqb_OK

type arm_mnemonic =
| ADD
| ADC
| MUL
| MLA
| MLS
| SDIV
| SUB
| SBC
| RSB
| UDIV
| UMULL
| UMAAL
| UMLAL
| SMULL
| SMLAL
| SMMUL
| SMMULR
| SMUL_hw of halfword * halfword
| SMLA_hw of halfword * halfword
| SMULW_hw of halfword
| AND
| BFC
| BFI
| BIC
| EOR
| MVN
| ORR
| ASR
| LSL
| LSR
| ROR
| REV
| REV16
| REVSH
| ADR
| MOV
| MOVT
| UBFX
| UXTB
| UXTH
| SBFX
| CLZ
| CMP
| TST
| CMN
| LDR
| LDRB
| LDRH
| LDRSB
| LDRSH
| STR
| STRB
| STRH

(** val arm_mnemonic_rect :
    'a1 -> 'a1 -> 'a1 -> 'a1 -> 'a1 -> 'a1 -> 'a1 -> 'a1 -> 'a1 -> 'a1 -> 'a1
    -> 'a1 -> 'a1 -> 'a1 -> 'a1 -> 'a1 -> 'a1 -> (halfword -> halfword ->
    'a1) -> (halfword -> halfword -> 'a1) -> (halfword -> 'a1) -> 'a1 -> 'a1
    -> 'a1 -> 'a1 -> 'a1 -> 'a1 -> 'a1 -> 'a1 -> 'a1 -> 'a1 -> 'a1 -> 'a1 ->
    'a1 -> 'a1 -> 'a1 -> 'a1 -> 'a1 -> 'a1 -> 'a1 -> 'a1 -> 'a1 -> 'a1 -> 'a1
    -> 'a1 -> 'a1 -> 'a1 -> 'a1 -> 'a1 -> 'a1 -> 'a1 -> 'a1 -> 'a1 -> 'a1 ->
    arm_mnemonic -> 'a1 **)

let arm_mnemonic_rect f f0 f1 f2 f3 f4 f5 f6 f7 f8 f9 f10 f11 f12 f13 f14 f15 f16 f17 f18 f19 f20 f21 f22 f23 f24 f25 f26 f27 f28 f29 f30 f31 f32 f33 f34 f35 f36 f37 f38 f39 f40 f41 f42 f43 f44 f45 f46 f47 f48 f49 f50 f51 = function
| ADD -> f
| ADC -> f0
| MUL -> f1
| MLA -> f2
| MLS -> f3
| SDIV -> f4
| SUB -> f5
| SBC -> f6
| RSB -> f7
| UDIV -> f8
| UMULL -> f9
| UMAAL -> f10
| UMLAL -> f11
| SMULL -> f12
| SMLAL -> f13
| SMMUL -> f14
| SMMULR -> f15
| SMUL_hw (h, h0) -> f16 h h0
| SMLA_hw (h, h0) -> f17 h h0
| SMULW_hw h -> f18 h
| AND -> f19
| BFC -> f20
| BFI -> f21
| BIC -> f22
| EOR -> f23
| MVN -> f24
| ORR -> f25
| ASR -> f26
| LSL -> f27
| LSR -> f28
| ROR -> f29
| REV -> f30
| REV16 -> f31
| REVSH -> f32
| ADR -> f33
| MOV -> f34
| MOVT -> f35
| UBFX -> f36
| UXTB -> f37
| UXTH -> f38
| SBFX -> f39
| CLZ -> f40
| CMP -> f41
| TST -> f42
| CMN -> f43
| LDR -> f44
| LDRB -> f45
| LDRH -> f46
| LDRSB -> f47
| LDRSH -> f48
| STR -> f49
| STRB -> f50
| STRH -> f51

(** val arm_mnemonic_rec :
    'a1 -> 'a1 -> 'a1 -> 'a1 -> 'a1 -> 'a1 -> 'a1 -> 'a1 -> 'a1 -> 'a1 -> 'a1
    -> 'a1 -> 'a1 -> 'a1 -> 'a1 -> 'a1 -> 'a1 -> (halfword -> halfword ->
    'a1) -> (halfword -> halfword -> 'a1) -> (halfword -> 'a1) -> 'a1 -> 'a1
    -> 'a1 -> 'a1 -> 'a1 -> 'a1 -> 'a1 -> 'a1 -> 'a1 -> 'a1 -> 'a1 -> 'a1 ->
    'a1 -> 'a1 -> 'a1 -> 'a1 -> 'a1 -> 'a1 -> 'a1 -> 'a1 -> 'a1 -> 'a1 -> 'a1
    -> 'a1 -> 'a1 -> 'a1 -> 'a1 -> 'a1 -> 'a1 -> 'a1 -> 'a1 -> 'a1 -> 'a1 ->
    arm_mnemonic -> 'a1 **)

let arm_mnemonic_rec f f0 f1 f2 f3 f4 f5 f6 f7 f8 f9 f10 f11 f12 f13 f14 f15 f16 f17 f18 f19 f20 f21 f22 f23 f24 f25 f26 f27 f28 f29 f30 f31 f32 f33 f34 f35 f36 f37 f38 f39 f40 f41 f42 f43 f44 f45 f46 f47 f48 f49 f50 f51 = function
| ADD -> f
| ADC -> f0
| MUL -> f1
| MLA -> f2
| MLS -> f3
| SDIV -> f4
| SUB -> f5
| SBC -> f6
| RSB -> f7
| UDIV -> f8
| UMULL -> f9
| UMAAL -> f10
| UMLAL -> f11
| SMULL -> f12
| SMLAL -> f13
| SMMUL -> f14
| SMMULR -> f15
| SMUL_hw (h, h0) -> f16 h h0
| SMLA_hw (h, h0) -> f17 h h0
| SMULW_hw h -> f18 h
| AND -> f19
| BFC -> f20
| BFI -> f21
| BIC -> f22
| EOR -> f23
| MVN -> f24
| ORR -> f25
| ASR -> f26
| LSL -> f27
| LSR -> f28
| ROR -> f29
| REV -> f30
| REV16 -> f31
| REVSH -> f32
| ADR -> f33
| MOV -> f34
| MOVT -> f35
| UBFX -> f36
| UXTB -> f37
| UXTH -> f38
| SBFX -> f39
| CLZ -> f40
| CMP -> f41
| TST -> f42
| CMN -> f43
| LDR -> f44
| LDRB -> f45
| LDRH -> f46
| LDRSB -> f47
| LDRSH -> f48
| STR -> f49
| STRB -> f50
| STRH -> f51

type is_arm_mnemonic =
| Coq_is_ADD
| Coq_is_ADC
| Coq_is_MUL
| Coq_is_MLA
| Coq_is_MLS
| Coq_is_SDIV
| Coq_is_SUB
| Coq_is_SBC
| Coq_is_RSB
| Coq_is_UDIV
| Coq_is_UMULL
| Coq_is_UMAAL
| Coq_is_UMLAL
| Coq_is_SMULL
| Coq_is_SMLAL
| Coq_is_SMMUL
| Coq_is_SMMULR
| Coq_is_SMUL_hw of halfword * is_halfword * halfword * is_halfword
| Coq_is_SMLA_hw of halfword * is_halfword * halfword * is_halfword
| Coq_is_SMULW_hw of halfword * is_halfword
| Coq_is_AND
| Coq_is_BFC
| Coq_is_BFI
| Coq_is_BIC
| Coq_is_EOR
| Coq_is_MVN
| Coq_is_ORR
| Coq_is_ASR
| Coq_is_LSL
| Coq_is_LSR
| Coq_is_ROR
| Coq_is_REV
| Coq_is_REV16
| Coq_is_REVSH
| Coq_is_ADR
| Coq_is_MOV
| Coq_is_MOVT
| Coq_is_UBFX
| Coq_is_UXTB
| Coq_is_UXTH
| Coq_is_SBFX
| Coq_is_CLZ
| Coq_is_CMP
| Coq_is_TST
| Coq_is_CMN
| Coq_is_LDR
| Coq_is_LDRB
| Coq_is_LDRH
| Coq_is_LDRSB
| Coq_is_LDRSH
| Coq_is_STR
| Coq_is_STRB
| Coq_is_STRH

(** val is_arm_mnemonic_rect :
    'a1 -> 'a1 -> 'a1 -> 'a1 -> 'a1 -> 'a1 -> 'a1 -> 'a1 -> 'a1 -> 'a1 -> 'a1
    -> 'a1 -> 'a1 -> 'a1 -> 'a1 -> 'a1 -> 'a1 -> (halfword -> is_halfword ->
    halfword -> is_halfword -> 'a1) -> (halfword -> is_halfword -> halfword
    -> is_halfword -> 'a1) -> (halfword -> is_halfword -> 'a1) -> 'a1 -> 'a1
    -> 'a1 -> 'a1 -> 'a1 -> 'a1 -> 'a1 -> 'a1 -> 'a1 -> 'a1 -> 'a1 -> 'a1 ->
    'a1 -> 'a1 -> 'a1 -> 'a1 -> 'a1 -> 'a1 -> 'a1 -> 'a1 -> 'a1 -> 'a1 -> 'a1
    -> 'a1 -> 'a1 -> 'a1 -> 'a1 -> 'a1 -> 'a1 -> 'a1 -> 'a1 -> 'a1 -> 'a1 ->
    arm_mnemonic -> is_arm_mnemonic -> 'a1 **)

let is_arm_mnemonic_rect f f0 f1 f2 f3 f4 f5 f6 f7 f8 f9 f10 f11 f12 f13 f14 f15 f16 f17 f18 f19 f20 f21 f22 f23 f24 f25 f26 f27 f28 f29 f30 f31 f32 f33 f34 f35 f36 f37 f38 f39 f40 f41 f42 f43 f44 f45 f46 f47 f48 f49 f50 f51 _ = function
| Coq_is_ADD -> f
| Coq_is_ADC -> f0
| Coq_is_MUL -> f1
| Coq_is_MLA -> f2
| Coq_is_MLS -> f3
| Coq_is_SDIV -> f4
| Coq_is_SUB -> f5
| Coq_is_SBC -> f6
| Coq_is_RSB -> f7
| Coq_is_UDIV -> f8
| Coq_is_UMULL -> f9
| Coq_is_UMAAL -> f10
| Coq_is_UMLAL -> f11
| Coq_is_SMULL -> f12
| Coq_is_SMLAL -> f13
| Coq_is_SMMUL -> f14
| Coq_is_SMMULR -> f15
| Coq_is_SMUL_hw (h, p_, h0, p_0) -> f16 h p_ h0 p_0
| Coq_is_SMLA_hw (h, p_, h0, p_0) -> f17 h p_ h0 p_0
| Coq_is_SMULW_hw (h, p_) -> f18 h p_
| Coq_is_AND -> f19
| Coq_is_BFC -> f20
| Coq_is_BFI -> f21
| Coq_is_BIC -> f22
| Coq_is_EOR -> f23
| Coq_is_MVN -> f24
| Coq_is_ORR -> f25
| Coq_is_ASR -> f26
| Coq_is_LSL -> f27
| Coq_is_LSR -> f28
| Coq_is_ROR -> f29
| Coq_is_REV -> f30
| Coq_is_REV16 -> f31
| Coq_is_REVSH -> f32
| Coq_is_ADR -> f33
| Coq_is_MOV -> f34
| Coq_is_MOVT -> f35
| Coq_is_UBFX -> f36
| Coq_is_UXTB -> f37
| Coq_is_UXTH -> f38
| Coq_is_SBFX -> f39
| Coq_is_CLZ -> f40
| Coq_is_CMP -> f41
| Coq_is_TST -> f42
| Coq_is_CMN -> f43
| Coq_is_LDR -> f44
| Coq_is_LDRB -> f45
| Coq_is_LDRH -> f46
| Coq_is_LDRSB -> f47
| Coq_is_LDRSH -> f48
| Coq_is_STR -> f49
| Coq_is_STRB -> f50
| Coq_is_STRH -> f51

(** val is_arm_mnemonic_rec :
    'a1 -> 'a1 -> 'a1 -> 'a1 -> 'a1 -> 'a1 -> 'a1 -> 'a1 -> 'a1 -> 'a1 -> 'a1
    -> 'a1 -> 'a1 -> 'a1 -> 'a1 -> 'a1 -> 'a1 -> (halfword -> is_halfword ->
    halfword -> is_halfword -> 'a1) -> (halfword -> is_halfword -> halfword
    -> is_halfword -> 'a1) -> (halfword -> is_halfword -> 'a1) -> 'a1 -> 'a1
    -> 'a1 -> 'a1 -> 'a1 -> 'a1 -> 'a1 -> 'a1 -> 'a1 -> 'a1 -> 'a1 -> 'a1 ->
    'a1 -> 'a1 -> 'a1 -> 'a1 -> 'a1 -> 'a1 -> 'a1 -> 'a1 -> 'a1 -> 'a1 -> 'a1
    -> 'a1 -> 'a1 -> 'a1 -> 'a1 -> 'a1 -> 'a1 -> 'a1 -> 'a1 -> 'a1 -> 'a1 ->
    arm_mnemonic -> is_arm_mnemonic -> 'a1 **)

let is_arm_mnemonic_rec f f0 f1 f2 f3 f4 f5 f6 f7 f8 f9 f10 f11 f12 f13 f14 f15 f16 f17 f18 f19 f20 f21 f22 f23 f24 f25 f26 f27 f28 f29 f30 f31 f32 f33 f34 f35 f36 f37 f38 f39 f40 f41 f42 f43 f44 f45 f46 f47 f48 f49 f50 f51 _ = function
| Coq_is_ADD -> f
| Coq_is_ADC -> f0
| Coq_is_MUL -> f1
| Coq_is_MLA -> f2
| Coq_is_MLS -> f3
| Coq_is_SDIV -> f4
| Coq_is_SUB -> f5
| Coq_is_SBC -> f6
| Coq_is_RSB -> f7
| Coq_is_UDIV -> f8
| Coq_is_UMULL -> f9
| Coq_is_UMAAL -> f10
| Coq_is_UMLAL -> f11
| Coq_is_SMULL -> f12
| Coq_is_SMLAL -> f13
| Coq_is_SMMUL -> f14
| Coq_is_SMMULR -> f15
| Coq_is_SMUL_hw (h, p_, h0, p_0) -> f16 h p_ h0 p_0
| Coq_is_SMLA_hw (h, p_, h0, p_0) -> f17 h p_ h0 p_0
| Coq_is_SMULW_hw (h, p_) -> f18 h p_
| Coq_is_AND -> f19
| Coq_is_BFC -> f20
| Coq_is_BFI -> f21
| Coq_is_BIC -> f22
| Coq_is_EOR -> f23
| Coq_is_MVN -> f24
| Coq_is_ORR -> f25
| Coq_is_ASR -> f26
| Coq_is_LSL -> f27
| Coq_is_LSR -> f28
| Coq_is_ROR -> f29
| Coq_is_REV -> f30
| Coq_is_REV16 -> f31
| Coq_is_REVSH -> f32
| Coq_is_ADR -> f33
| Coq_is_MOV -> f34
| Coq_is_MOVT -> f35
| Coq_is_UBFX -> f36
| Coq_is_UXTB -> f37
| Coq_is_UXTH -> f38
| Coq_is_SBFX -> f39
| Coq_is_CLZ -> f40
| Coq_is_CMP -> f41
| Coq_is_TST -> f42
| Coq_is_CMN -> f43
| Coq_is_LDR -> f44
| Coq_is_LDRB -> f45
| Coq_is_LDRH -> f46
| Coq_is_LDRSB -> f47
| Coq_is_LDRSH -> f48
| Coq_is_STR -> f49
| Coq_is_STRB -> f50
| Coq_is_STRH -> f51

(** val arm_mnemonic_tag : arm_mnemonic -> positive **)

let arm_mnemonic_tag = function
| ADD -> Coq_xH
| ADC -> Coq_xO Coq_xH
| MUL -> Coq_xI Coq_xH
| MLA -> Coq_xO (Coq_xO Coq_xH)
| MLS -> Coq_xI (Coq_xO Coq_xH)
| SDIV -> Coq_xO (Coq_xI Coq_xH)
| SUB -> Coq_xI (Coq_xI Coq_xH)
| SBC -> Coq_xO (Coq_xO (Coq_xO Coq_xH))
| RSB -> Coq_xI (Coq_xO (Coq_xO Coq_xH))
| UDIV -> Coq_xO (Coq_xI (Coq_xO Coq_xH))
| UMULL -> Coq_xI (Coq_xI (Coq_xO Coq_xH))
| UMAAL -> Coq_xO (Coq_xO (Coq_xI Coq_xH))
| UMLAL -> Coq_xI (Coq_xO (Coq_xI Coq_xH))
| SMULL -> Coq_xO (Coq_xI (Coq_xI Coq_xH))
| SMLAL -> Coq_xI (Coq_xI (Coq_xI Coq_xH))
| SMMUL -> Coq_xO (Coq_xO (Coq_xO (Coq_xO Coq_xH)))
| SMMULR -> Coq_xI (Coq_xO (Coq_xO (Coq_xO Coq_xH)))
| SMUL_hw (_, _) -> Coq_xO (Coq_xI (Coq_xO (Coq_xO Coq_xH)))
| SMLA_hw (_, _) -> Coq_xI (Coq_xI (Coq_xO (Coq_xO Coq_xH)))
| SMULW_hw _ -> Coq_xO (Coq_xO (Coq_xI (Coq_xO Coq_xH)))
| AND -> Coq_xI (Coq_xO (Coq_xI (Coq_xO Coq_xH)))
| BFC -> Coq_xO (Coq_xI (Coq_xI (Coq_xO Coq_xH)))
| BFI -> Coq_xI (Coq_xI (Coq_xI (Coq_xO Coq_xH)))
| BIC -> Coq_xO (Coq_xO (Coq_xO (Coq_xI Coq_xH)))
| EOR -> Coq_xI (Coq_xO (Coq_xO (Coq_xI Coq_xH)))
| MVN -> Coq_xO (Coq_xI (Coq_xO (Coq_xI Coq_xH)))
| ORR -> Coq_xI (Coq_xI (Coq_xO (Coq_xI Coq_xH)))
| ASR -> Coq_xO (Coq_xO (Coq_xI (Coq_xI Coq_xH)))
| LSL -> Coq_xI (Coq_xO (Coq_xI (Coq_xI Coq_xH)))
| LSR -> Coq_xO (Coq_xI (Coq_xI (Coq_xI Coq_xH)))
| ROR -> Coq_xI (Coq_xI (Coq_xI (Coq_xI Coq_xH)))
| REV -> Coq_xO (Coq_xO (Coq_xO (Coq_xO (Coq_xO Coq_xH))))
| REV16 -> Coq_xI (Coq_xO (Coq_xO (Coq_xO (Coq_xO Coq_xH))))
| REVSH -> Coq_xO (Coq_xI (Coq_xO (Coq_xO (Coq_xO Coq_xH))))
| ADR -> Coq_xI (Coq_xI (Coq_xO (Coq_xO (Coq_xO Coq_xH))))
| MOV -> Coq_xO (Coq_xO (Coq_xI (Coq_xO (Coq_xO Coq_xH))))
| MOVT -> Coq_xI (Coq_xO (Coq_xI (Coq_xO (Coq_xO Coq_xH))))
| UBFX -> Coq_xO (Coq_xI (Coq_xI (Coq_xO (Coq_xO Coq_xH))))
| UXTB -> Coq_xI (Coq_xI (Coq_xI (Coq_xO (Coq_xO Coq_xH))))
| UXTH -> Coq_xO (Coq_xO (Coq_xO (Coq_xI (Coq_xO Coq_xH))))
| SBFX -> Coq_xI (Coq_xO (Coq_xO (Coq_xI (Coq_xO Coq_xH))))
| CLZ -> Coq_xO (Coq_xI (Coq_xO (Coq_xI (Coq_xO Coq_xH))))
| CMP -> Coq_xI (Coq_xI (Coq_xO (Coq_xI (Coq_xO Coq_xH))))
| TST -> Coq_xO (Coq_xO (Coq_xI (Coq_xI (Coq_xO Coq_xH))))
| CMN -> Coq_xI (Coq_xO (Coq_xI (Coq_xI (Coq_xO Coq_xH))))
| LDR -> Coq_xO (Coq_xI (Coq_xI (Coq_xI (Coq_xO Coq_xH))))
| LDRB -> Coq_xI (Coq_xI (Coq_xI (Coq_xI (Coq_xO Coq_xH))))
| LDRH -> Coq_xO (Coq_xO (Coq_xO (Coq_xO (Coq_xI Coq_xH))))
| LDRSB -> Coq_xI (Coq_xO (Coq_xO (Coq_xO (Coq_xI Coq_xH))))
| LDRSH -> Coq_xO (Coq_xI (Coq_xO (Coq_xO (Coq_xI Coq_xH))))
| STR -> Coq_xI (Coq_xI (Coq_xO (Coq_xO (Coq_xI Coq_xH))))
| STRB -> Coq_xO (Coq_xO (Coq_xI (Coq_xO (Coq_xI Coq_xH))))
| STRH -> Coq_xI (Coq_xO (Coq_xI (Coq_xO (Coq_xI Coq_xH))))

(** val is_arm_mnemonic_inhab : arm_mnemonic -> is_arm_mnemonic **)

let is_arm_mnemonic_inhab = function
| ADD -> Coq_is_ADD
| ADC -> Coq_is_ADC
| MUL -> Coq_is_MUL
| MLA -> Coq_is_MLA
| MLS -> Coq_is_MLS
| SDIV -> Coq_is_SDIV
| SUB -> Coq_is_SUB
| SBC -> Coq_is_SBC
| RSB -> Coq_is_RSB
| UDIV -> Coq_is_UDIV
| UMULL -> Coq_is_UMULL
| UMAAL -> Coq_is_UMAAL
| UMLAL -> Coq_is_UMLAL
| SMULL -> Coq_is_SMULL
| SMLAL -> Coq_is_SMLAL
| SMMUL -> Coq_is_SMMUL
| SMMULR -> Coq_is_SMMULR
| SMUL_hw (h, h0) ->
  Coq_is_SMUL_hw (h, (is_halfword_inhab h), h0, (is_halfword_inhab h0))
| SMLA_hw (h, h0) ->
  Coq_is_SMLA_hw (h, (is_halfword_inhab h), h0, (is_halfword_inhab h0))
| SMULW_hw h -> Coq_is_SMULW_hw (h, (is_halfword_inhab h))
| AND -> Coq_is_AND
| BFC -> Coq_is_BFC
| BFI -> Coq_is_BFI
| BIC -> Coq_is_BIC
| EOR -> Coq_is_EOR
| MVN -> Coq_is_MVN
| ORR -> Coq_is_ORR
| ASR -> Coq_is_ASR
| LSL -> Coq_is_LSL
| LSR -> Coq_is_LSR
| ROR -> Coq_is_ROR
| REV -> Coq_is_REV
| REV16 -> Coq_is_REV16
| REVSH -> Coq_is_REVSH
| ADR -> Coq_is_ADR
| MOV -> Coq_is_MOV
| MOVT -> Coq_is_MOVT
| UBFX -> Coq_is_UBFX
| UXTB -> Coq_is_UXTB
| UXTH -> Coq_is_UXTH
| SBFX -> Coq_is_SBFX
| CLZ -> Coq_is_CLZ
| CMP -> Coq_is_CMP
| TST -> Coq_is_TST
| CMN -> Coq_is_CMN
| LDR -> Coq_is_LDR
| LDRB -> Coq_is_LDRB
| LDRH -> Coq_is_LDRH
| LDRSB -> Coq_is_LDRSB
| LDRSH -> Coq_is_LDRSH
| STR -> Coq_is_STR
| STRB -> Coq_is_STRB
| STRH -> Coq_is_STRH

(** val is_arm_mnemonic_functor :
    arm_mnemonic -> is_arm_mnemonic -> is_arm_mnemonic **)

let rec is_arm_mnemonic_functor _ x =
  x

type box_arm_mnemonic_ADD =
| Box_arm_mnemonic_ADD

type box_arm_mnemonic_SMUL_hw = { coq_Box_arm_mnemonic_SMUL_hw_0 : halfword;
                                  coq_Box_arm_mnemonic_SMUL_hw_1 : halfword }

(** val coq_Box_arm_mnemonic_SMUL_hw_0 :
    box_arm_mnemonic_SMUL_hw -> halfword **)

let coq_Box_arm_mnemonic_SMUL_hw_0 record =
  record.coq_Box_arm_mnemonic_SMUL_hw_0

(** val coq_Box_arm_mnemonic_SMUL_hw_1 :
    box_arm_mnemonic_SMUL_hw -> halfword **)

let coq_Box_arm_mnemonic_SMUL_hw_1 record =
  record.coq_Box_arm_mnemonic_SMUL_hw_1

type box_arm_mnemonic_SMULW_hw =
  halfword
  (* singleton inductive, whose constructor was Box_arm_mnemonic_SMULW_hw *)

(** val coq_Box_arm_mnemonic_SMULW_hw_0 :
    box_arm_mnemonic_SMULW_hw -> halfword **)

let coq_Box_arm_mnemonic_SMULW_hw_0 record =
  record

type arm_mnemonic_fields_t = __

(** val arm_mnemonic_fields : arm_mnemonic -> arm_mnemonic_fields_t **)

let arm_mnemonic_fields = function
| SMUL_hw (h, h0) ->
  Obj.magic { coq_Box_arm_mnemonic_SMUL_hw_0 = h;
    coq_Box_arm_mnemonic_SMUL_hw_1 = h0 }
| SMLA_hw (h, h0) ->
  Obj.magic { coq_Box_arm_mnemonic_SMUL_hw_0 = h;
    coq_Box_arm_mnemonic_SMUL_hw_1 = h0 }
| SMULW_hw h -> Obj.magic h
| _ -> Obj.magic Box_arm_mnemonic_ADD

(** val arm_mnemonic_construct :
    positive -> arm_mnemonic_fields_t -> arm_mnemonic option **)

let arm_mnemonic_construct p x =
  match p with
  | Coq_xI x0 ->
    (match x0 with
     | Coq_xI x1 ->
       (match x1 with
        | Coq_xI x2 ->
          (match x2 with
           | Coq_xI x3 ->
             (match x3 with
              | Coq_xI _ -> None
              | Coq_xO _ -> Some LDRB
              | Coq_xH -> Some ROR)
           | Coq_xO x3 ->
             (match x3 with
              | Coq_xI _ -> None
              | Coq_xO _ -> Some UXTB
              | Coq_xH -> Some BFI)
           | Coq_xH -> Some SMLAL)
        | Coq_xO x2 ->
          (match x2 with
           | Coq_xI x3 ->
             (match x3 with
              | Coq_xI _ -> None
              | Coq_xO _ -> Some CMP
              | Coq_xH -> Some ORR)
           | Coq_xO x3 ->
             (match x3 with
              | Coq_xI _ -> Some STR
              | Coq_xO _ -> Some ADR
              | Coq_xH ->
                let { coq_Box_arm_mnemonic_SMUL_hw_0 =
                  box_arm_mnemonic_SMUL_hw_0;
                  coq_Box_arm_mnemonic_SMUL_hw_1 =
                  box_arm_mnemonic_SMUL_hw_1 } = Obj.magic x
                in
                Some (SMLA_hw (box_arm_mnemonic_SMUL_hw_0,
                box_arm_mnemonic_SMUL_hw_1)))
           | Coq_xH -> Some UMULL)
        | Coq_xH -> Some SUB)
     | Coq_xO x1 ->
       (match x1 with
        | Coq_xI x2 ->
          (match x2 with
           | Coq_xI x3 ->
             (match x3 with
              | Coq_xI _ -> None
              | Coq_xO _ -> Some CMN
              | Coq_xH -> Some LSL)
           | Coq_xO x3 ->
             (match x3 with
              | Coq_xI _ -> Some STRH
              | Coq_xO _ -> Some MOVT
              | Coq_xH -> Some AND)
           | Coq_xH -> Some UMLAL)
        | Coq_xO x2 ->
          (match x2 with
           | Coq_xI x3 ->
             (match x3 with
              | Coq_xI _ -> None
              | Coq_xO _ -> Some SBFX
              | Coq_xH -> Some EOR)
           | Coq_xO x3 ->
             (match x3 with
              | Coq_xI _ -> Some LDRSB
              | Coq_xO _ -> Some REV16
              | Coq_xH -> Some SMMULR)
           | Coq_xH -> Some RSB)
        | Coq_xH -> Some MLS)
     | Coq_xH -> Some MUL)
  | Coq_xO x0 ->
    (match x0 with
     | Coq_xI x1 ->
       (match x1 with
        | Coq_xI x2 ->
          (match x2 with
           | Coq_xI x3 ->
             (match x3 with
              | Coq_xI _ -> None
              | Coq_xO _ -> Some LDR
              | Coq_xH -> Some LSR)
           | Coq_xO x3 ->
             (match x3 with
              | Coq_xI _ -> None
              | Coq_xO _ -> Some UBFX
              | Coq_xH -> Some BFC)
           | Coq_xH -> Some SMULL)
        | Coq_xO x2 ->
          (match x2 with
           | Coq_xI x3 ->
             (match x3 with
              | Coq_xI _ -> None
              | Coq_xO _ -> Some CLZ
              | Coq_xH -> Some MVN)
           | Coq_xO x3 ->
             (match x3 with
              | Coq_xI _ -> Some LDRSH
              | Coq_xO _ -> Some REVSH
              | Coq_xH ->
                let { coq_Box_arm_mnemonic_SMUL_hw_0 =
                  box_arm_mnemonic_SMUL_hw_0;
                  coq_Box_arm_mnemonic_SMUL_hw_1 =
                  box_arm_mnemonic_SMUL_hw_1 } = Obj.magic x
                in
                Some (SMUL_hw (box_arm_mnemonic_SMUL_hw_0,
                box_arm_mnemonic_SMUL_hw_1)))
           | Coq_xH -> Some UDIV)
        | Coq_xH -> Some SDIV)
     | Coq_xO x1 ->
       (match x1 with
        | Coq_xI x2 ->
          (match x2 with
           | Coq_xI x3 ->
             (match x3 with
              | Coq_xI _ -> None
              | Coq_xO _ -> Some TST
              | Coq_xH -> Some ASR)
           | Coq_xO x3 ->
             (match x3 with
              | Coq_xI _ -> Some STRB
              | Coq_xO _ -> Some MOV
              | Coq_xH -> Some (SMULW_hw (Obj.magic x)))
           | Coq_xH -> Some UMAAL)
        | Coq_xO x2 ->
          (match x2 with
           | Coq_xI x3 ->
             (match x3 with
              | Coq_xI _ -> None
              | Coq_xO _ -> Some UXTH
              | Coq_xH -> Some BIC)
           | Coq_xO x3 ->
             (match x3 with
              | Coq_xI _ -> Some LDRH
              | Coq_xO _ -> Some REV
              | Coq_xH -> Some SMMUL)
           | Coq_xH -> Some SBC)
        | Coq_xH -> Some MLA)
     | Coq_xH -> Some ADC)
  | Coq_xH -> Some ADD

(** val arm_mnemonic_induction :
    'a1 -> 'a1 -> 'a1 -> 'a1 -> 'a1 -> 'a1 -> 'a1 -> 'a1 -> 'a1 -> 'a1 -> 'a1
    -> 'a1 -> 'a1 -> 'a1 -> 'a1 -> 'a1 -> 'a1 -> (halfword -> is_halfword ->
    halfword -> is_halfword -> 'a1) -> (halfword -> is_halfword -> halfword
    -> is_halfword -> 'a1) -> (halfword -> is_halfword -> 'a1) -> 'a1 -> 'a1
    -> 'a1 -> 'a1 -> 'a1 -> 'a1 -> 'a1 -> 'a1 -> 'a1 -> 'a1 -> 'a1 -> 'a1 ->
    'a1 -> 'a1 -> 'a1 -> 'a1 -> 'a1 -> 'a1 -> 'a1 -> 'a1 -> 'a1 -> 'a1 -> 'a1
    -> 'a1 -> 'a1 -> 'a1 -> 'a1 -> 'a1 -> 'a1 -> 'a1 -> 'a1 -> 'a1 -> 'a1 ->
    arm_mnemonic -> is_arm_mnemonic -> 'a1 **)

let arm_mnemonic_induction his_ADD his_ADC his_MUL his_MLA his_MLS his_SDIV his_SUB his_SBC his_RSB his_UDIV his_UMULL his_UMAAL his_UMLAL his_SMULL his_SMLAL his_SMMUL his_SMMULR his_SMUL_hw his_SMLA_hw his_SMULW_hw his_AND his_BFC his_BFI his_BIC his_EOR his_MVN his_ORR his_ASR his_LSL his_LSR his_ROR his_REV his_REV16 his_REVSH his_ADR his_MOV his_MOVT his_UBFX his_UXTB his_UXTH his_SBFX his_CLZ his_CMP his_TST his_CMN his_LDR his_LDRB his_LDRH his_LDRSB his_LDRSH his_STR his_STRB his_STRH _ = function
| Coq_is_ADD -> his_ADD
| Coq_is_ADC -> his_ADC
| Coq_is_MUL -> his_MUL
| Coq_is_MLA -> his_MLA
| Coq_is_MLS -> his_MLS
| Coq_is_SDIV -> his_SDIV
| Coq_is_SUB -> his_SUB
| Coq_is_SBC -> his_SBC
| Coq_is_RSB -> his_RSB
| Coq_is_UDIV -> his_UDIV
| Coq_is_UMULL -> his_UMULL
| Coq_is_UMAAL -> his_UMAAL
| Coq_is_UMLAL -> his_UMLAL
| Coq_is_SMULL -> his_SMULL
| Coq_is_SMLAL -> his_SMLAL
| Coq_is_SMMUL -> his_SMMUL
| Coq_is_SMMULR -> his_SMMULR
| Coq_is_SMUL_hw (x0, p_, x1, p_0) -> his_SMUL_hw x0 p_ x1 p_0
| Coq_is_SMLA_hw (x0, p_, x1, p_0) -> his_SMLA_hw x0 p_ x1 p_0
| Coq_is_SMULW_hw (x0, p_) -> his_SMULW_hw x0 p_
| Coq_is_AND -> his_AND
| Coq_is_BFC -> his_BFC
| Coq_is_BFI -> his_BFI
| Coq_is_BIC -> his_BIC
| Coq_is_EOR -> his_EOR
| Coq_is_MVN -> his_MVN
| Coq_is_ORR -> his_ORR
| Coq_is_ASR -> his_ASR
| Coq_is_LSL -> his_LSL
| Coq_is_LSR -> his_LSR
| Coq_is_ROR -> his_ROR
| Coq_is_REV -> his_REV
| Coq_is_REV16 -> his_REV16
| Coq_is_REVSH -> his_REVSH
| Coq_is_ADR -> his_ADR
| Coq_is_MOV -> his_MOV
| Coq_is_MOVT -> his_MOVT
| Coq_is_UBFX -> his_UBFX
| Coq_is_UXTB -> his_UXTB
| Coq_is_UXTH -> his_UXTH
| Coq_is_SBFX -> his_SBFX
| Coq_is_CLZ -> his_CLZ
| Coq_is_CMP -> his_CMP
| Coq_is_TST -> his_TST
| Coq_is_CMN -> his_CMN
| Coq_is_LDR -> his_LDR
| Coq_is_LDRB -> his_LDRB
| Coq_is_LDRH -> his_LDRH
| Coq_is_LDRSB -> his_LDRSB
| Coq_is_LDRSH -> his_LDRSH
| Coq_is_STR -> his_STR
| Coq_is_STRB -> his_STRB
| Coq_is_STRH -> his_STRH

(** val arm_mnemonic_eqb_fields :
    (arm_mnemonic -> arm_mnemonic -> bool) -> positive ->
    arm_mnemonic_fields_t -> arm_mnemonic_fields_t -> bool **)

let arm_mnemonic_eqb_fields _ x x0 x1 =
  match x with
  | Coq_xI x2 ->
    (match x2 with
     | Coq_xI x3 ->
       (match x3 with
        | Coq_xO x4 ->
          (match x4 with
           | Coq_xO x5 ->
             (match x5 with
              | Coq_xH ->
                let { coq_Box_arm_mnemonic_SMUL_hw_0 =
                  box_arm_mnemonic_SMUL_hw_0;
                  coq_Box_arm_mnemonic_SMUL_hw_1 =
                  box_arm_mnemonic_SMUL_hw_1 } = Obj.magic x0
                in
                let { coq_Box_arm_mnemonic_SMUL_hw_0 =
                  box_arm_mnemonic_SMUL_hw_2;
                  coq_Box_arm_mnemonic_SMUL_hw_1 =
                  box_arm_mnemonic_SMUL_hw_3 } = Obj.magic x1
                in
                (&&)
                  (halfword_eqb box_arm_mnemonic_SMUL_hw_0
                    box_arm_mnemonic_SMUL_hw_2)
                  ((&&)
                    (halfword_eqb box_arm_mnemonic_SMUL_hw_1
                      box_arm_mnemonic_SMUL_hw_3) true)
              | _ -> true)
           | _ -> true)
        | _ -> true)
     | _ -> true)
  | Coq_xO x2 ->
    (match x2 with
     | Coq_xI x3 ->
       (match x3 with
        | Coq_xO x4 ->
          (match x4 with
           | Coq_xO x5 ->
             (match x5 with
              | Coq_xH ->
                let { coq_Box_arm_mnemonic_SMUL_hw_0 =
                  box_arm_mnemonic_SMUL_hw_0;
                  coq_Box_arm_mnemonic_SMUL_hw_1 =
                  box_arm_mnemonic_SMUL_hw_1 } = Obj.magic x0
                in
                let { coq_Box_arm_mnemonic_SMUL_hw_0 =
                  box_arm_mnemonic_SMUL_hw_2;
                  coq_Box_arm_mnemonic_SMUL_hw_1 =
                  box_arm_mnemonic_SMUL_hw_3 } = Obj.magic x1
                in
                (&&)
                  (halfword_eqb box_arm_mnemonic_SMUL_hw_0
                    box_arm_mnemonic_SMUL_hw_2)
                  ((&&)
                    (halfword_eqb box_arm_mnemonic_SMUL_hw_1
                      box_arm_mnemonic_SMUL_hw_3) true)
              | _ -> true)
           | _ -> true)
        | _ -> true)
     | Coq_xO x3 ->
       (match x3 with
        | Coq_xI x4 ->
          (match x4 with
           | Coq_xO x5 ->
             (match x5 with
              | Coq_xH ->
                (&&) (halfword_eqb (Obj.magic x0) (Obj.magic x1)) true
              | _ -> true)
           | _ -> true)
        | _ -> true)
     | Coq_xH -> true)
  | Coq_xH -> true

(** val arm_mnemonic_eqb : arm_mnemonic -> arm_mnemonic -> bool **)

let arm_mnemonic_eqb x1 x2 =
  match x1 with
  | SMUL_hw (h, h0) ->
    eqb_body arm_mnemonic_tag arm_mnemonic_fields
      (Obj.magic arm_mnemonic_eqb_fields (fun _ _ -> true))
      (arm_mnemonic_tag (SMUL_hw (h, h0))) { coq_Box_arm_mnemonic_SMUL_hw_0 =
      h; coq_Box_arm_mnemonic_SMUL_hw_1 = h0 } x2
  | SMLA_hw (h, h0) ->
    eqb_body arm_mnemonic_tag arm_mnemonic_fields
      (Obj.magic arm_mnemonic_eqb_fields (fun _ _ -> true))
      (arm_mnemonic_tag (SMLA_hw (h, h0))) { coq_Box_arm_mnemonic_SMUL_hw_0 =
      h; coq_Box_arm_mnemonic_SMUL_hw_1 = h0 } x2
  | SMULW_hw h ->
    eqb_body arm_mnemonic_tag arm_mnemonic_fields
      (Obj.magic arm_mnemonic_eqb_fields (fun _ _ -> true))
      (arm_mnemonic_tag (SMULW_hw h)) h x2
  | x ->
    eqb_body arm_mnemonic_tag arm_mnemonic_fields
      (Obj.magic arm_mnemonic_eqb_fields (fun _ _ -> true))
      (arm_mnemonic_tag x) Box_arm_mnemonic_ADD x2

(** val arm_mnemonic_eqb_OK : arm_mnemonic -> arm_mnemonic -> reflect **)

let arm_mnemonic_eqb_OK =
  iffP2 arm_mnemonic_eqb

(** val arm_mnemonic_eqb_OK_sumbool : arm_mnemonic -> arm_mnemonic -> bool **)

let arm_mnemonic_eqb_OK_sumbool =
  reflect_dec arm_mnemonic_eqb arm_mnemonic_eqb_OK

(** val eqTC_arm_mnemonic : arm_mnemonic eqTypeC **)

let eqTC_arm_mnemonic =
  { beq = arm_mnemonic_eqb; ceqP = arm_mnemonic_eqb_OK }

(** val arm_mnemonic_eqType : Equality.coq_type **)

let arm_mnemonic_eqType =
  ceqT_eqType eqTC_arm_mnemonic

(** val arm_mnemonics : arm_mnemonic list **)

let arm_mnemonics =
  ADD :: (ADC :: (MUL :: (MLA :: (MLS :: (SDIV :: (SUB :: (SBC :: (RSB :: (UDIV :: (UMULL :: (UMAAL :: (UMLAL :: (SMULL :: (SMLAL :: (SMMUL :: (SMMULR :: ((SMUL_hw
    (HWB, HWB)) :: ((SMUL_hw (HWB, HWT)) :: ((SMUL_hw (HWT,
    HWB)) :: ((SMUL_hw (HWT, HWT)) :: ((SMLA_hw (HWB, HWB)) :: ((SMLA_hw
    (HWB, HWT)) :: ((SMLA_hw (HWT, HWB)) :: ((SMLA_hw (HWT,
    HWT)) :: ((SMULW_hw HWB) :: ((SMULW_hw
    HWT) :: (AND :: (BFC :: (BFI :: (BIC :: (EOR :: (MVN :: (ORR :: (ASR :: (LSL :: (LSR :: (ROR :: (REV :: (REV16 :: (REVSH :: (ADR :: (MOV :: (MOVT :: (UBFX :: (UXTB :: (UXTH :: (SBFX :: (CLZ :: (CMP :: (TST :: (CMN :: (LDR :: (LDRB :: (LDRH :: (LDRSB :: (LDRSH :: (STR :: (STRB :: (STRH :: [])))))))))))))))))))))))))))))))))))))))))))))))))))))))))))

(** val finTC_arm_mnemonic : arm_mnemonic finTypeC **)

let finTC_arm_mnemonic =
  { _eqC = eqTC_arm_mnemonic; cenum = arm_mnemonics }

(** val arm_mnemonic_finType : Finite.coq_type **)

let arm_mnemonic_finType =
  cfinT_finType finTC_arm_mnemonic

(** val set_flags_mnemonics : arm_mnemonic list **)

let set_flags_mnemonics =
  ADD :: (ADC :: (MUL :: (SUB :: (SBC :: (RSB :: (AND :: (BIC :: (EOR :: (MVN :: (ORR :: (ASR :: (LSL :: (LSR :: (ROR :: (MOV :: [])))))))))))))))

(** val has_shift_mnemonics : arm_mnemonic list **)

let has_shift_mnemonics =
  ADD :: (ADC :: (SUB :: (SBC :: (RSB :: (AND :: (BIC :: (EOR :: (MVN :: (ORR :: (CMP :: (TST :: (CMN :: []))))))))))))

(** val condition_mnemonics : arm_mnemonic list **)

let condition_mnemonics =
  CMP :: (TST :: [])

(** val always_has_shift_mnemonics : (arm_mnemonic * shift_kind) list **)

let always_has_shift_mnemonics =
  (UXTB, SROR) :: ((UXTH, SROR) :: [])

(** val wsize_uload_mn : (wsize * arm_mnemonic) list **)

let wsize_uload_mn =
  (U8, LDRB) :: ((U16, LDRH) :: ((U32, LDR) :: []))

(** val uload_mn_of_wsize : wsize -> arm_mnemonic option **)

let uload_mn_of_wsize ws =
  assoc wsize_wsize__canonical__eqtype_Equality (Obj.magic wsize_uload_mn)
    (Obj.magic ws)

(** val wsize_of_uload_mn : arm_mnemonic -> wsize option **)

let wsize_of_uload_mn mn =
  assoc arm_mnemonic_eqType
    (map (fun x -> ((snd (Obj.magic x)), (fst x))) wsize_uload_mn)
    (Obj.magic mn)

(** val wsize_sload_mn : (wsize * arm_mnemonic) list **)

let wsize_sload_mn =
  (U8, LDRSB) :: ((U16, LDRSH) :: [])

(** val sload_mn_of_wsize : wsize -> arm_mnemonic option **)

let sload_mn_of_wsize ws =
  assoc wsize_wsize__canonical__eqtype_Equality (Obj.magic wsize_sload_mn)
    (Obj.magic ws)

(** val wsize_of_sload_mn : arm_mnemonic -> wsize option **)

let wsize_of_sload_mn mn =
  assoc arm_mnemonic_eqType
    (map (fun x -> ((snd (Obj.magic x)), (fst x))) wsize_sload_mn)
    (Obj.magic mn)

(** val wsize_of_load_mn : arm_mnemonic -> wsize option **)

let wsize_of_load_mn mn =
  match wsize_of_uload_mn mn with
  | Some ws -> Some ws
  | None -> wsize_of_sload_mn mn

(** val wsize_store_mn : (wsize * arm_mnemonic) list **)

let wsize_store_mn =
  (U8, STRB) :: ((U16, STRH) :: ((U32, STR) :: []))

(** val store_mn_of_wsize : wsize -> arm_mnemonic option **)

let store_mn_of_wsize ws =
  assoc wsize_wsize__canonical__eqtype_Equality (Obj.magic wsize_store_mn)
    (Obj.magic ws)

(** val wsize_of_store_mn : arm_mnemonic -> wsize option **)

let wsize_of_store_mn mn =
  assoc arm_mnemonic_eqType
    (map (fun x -> ((snd (Obj.magic x)), (fst x))) wsize_store_mn)
    (Obj.magic mn)

(** val string_of_hw : halfword -> string **)

let string_of_hw = function
| HWB -> "B"
| HWT -> "T"

(** val string_of_arm_mnemonic : arm_mnemonic -> string **)

let string_of_arm_mnemonic mn =
  let with_hw = fun s hw -> (^) s (string_of_hw hw) in
  (match mn with
   | ADD -> "ADD"
   | ADC -> "ADC"
   | MUL -> "MUL"
   | MLA -> "MLA"
   | MLS -> "MLS"
   | SDIV -> "SDIV"
   | SUB -> "SUB"
   | SBC -> "SBC"
   | RSB -> "RSB"
   | UDIV -> "UDIV"
   | UMULL -> "UMULL"
   | UMAAL -> "UMAAL"
   | UMLAL -> "UMLAL"
   | SMULL -> "SMULL"
   | SMLAL -> "SMLAL"
   | SMMUL -> "SMMUL"
   | SMMULR -> "SMMULR"
   | SMUL_hw (hw0, hw1) -> with_hw (with_hw "SMUL" hw0) hw1
   | SMLA_hw (hw0, hw1) -> with_hw (with_hw "SMLA" hw0) hw1
   | SMULW_hw hw -> with_hw "SMULW" hw
   | AND -> "AND"
   | BFC -> "BFC"
   | BFI -> "BFI"
   | BIC -> "BIC"
   | EOR -> "EOR"
   | MVN -> "MVN"
   | ORR -> "ORR"
   | ASR -> "ASR"
   | LSL -> "LSL"
   | LSR -> "LSR"
   | ROR -> "ROR"
   | REV -> "REV"
   | REV16 -> "REV16"
   | REVSH -> "REVSH"
   | ADR -> "ADR"
   | MOV -> "MOV"
   | MOVT -> "MOVT"
   | UBFX -> "UBFX"
   | UXTB -> "UXTB"
   | UXTH -> "UXTH"
   | SBFX -> "SBFX"
   | CLZ -> "CLZ"
   | CMP -> "CMP"
   | TST -> "TST"
   | CMN -> "CMN"
   | LDR -> "LDR"
   | LDRB -> "LDRB"
   | LDRH -> "LDRH"
   | LDRSB -> "LDRSB"
   | LDRSH -> "LDRSH"
   | STR -> "STR"
   | STRB -> "STRB"
   | STRH -> "STRH")

type arm_op =
| ARM_op of arm_mnemonic * arm_options

(** val arm_op_rect :
    (arm_mnemonic -> arm_options -> 'a1) -> arm_op -> 'a1 **)

let arm_op_rect f = function
| ARM_op (a0, a1) -> f a0 a1

(** val arm_op_rec : (arm_mnemonic -> arm_options -> 'a1) -> arm_op -> 'a1 **)

let arm_op_rec f = function
| ARM_op (a0, a1) -> f a0 a1

type is_arm_op =
| Coq_is_ARM_op of arm_mnemonic * is_arm_mnemonic * arm_options
   * is_arm_options

(** val is_arm_op_rect :
    (arm_mnemonic -> is_arm_mnemonic -> arm_options -> is_arm_options -> 'a1)
    -> arm_op -> is_arm_op -> 'a1 **)

let is_arm_op_rect f _ = function
| Coq_is_ARM_op (a, p_, a0, p_0) -> f a p_ a0 p_0

(** val is_arm_op_rec :
    (arm_mnemonic -> is_arm_mnemonic -> arm_options -> is_arm_options -> 'a1)
    -> arm_op -> is_arm_op -> 'a1 **)

let is_arm_op_rec f _ = function
| Coq_is_ARM_op (a, p_, a0, p_0) -> f a p_ a0 p_0

(** val arm_op_tag : arm_op -> positive **)

let arm_op_tag _ =
  Coq_xH

(** val is_arm_op_inhab : arm_op -> is_arm_op **)

let is_arm_op_inhab = function
| ARM_op (h, h0) ->
  Coq_is_ARM_op (h, (is_arm_mnemonic_inhab h), h0, (is_arm_options_inhab h0))

(** val is_arm_op_functor : arm_op -> is_arm_op -> is_arm_op **)

let rec is_arm_op_functor _ x =
  x

type box_arm_op_ARM_op = { coq_Box_arm_op_ARM_op_0 : arm_mnemonic;
                           coq_Box_arm_op_ARM_op_1 : arm_options }

(** val coq_Box_arm_op_ARM_op_0 : box_arm_op_ARM_op -> arm_mnemonic **)

let coq_Box_arm_op_ARM_op_0 record =
  record.coq_Box_arm_op_ARM_op_0

(** val coq_Box_arm_op_ARM_op_1 : box_arm_op_ARM_op -> arm_options **)

let coq_Box_arm_op_ARM_op_1 record =
  record.coq_Box_arm_op_ARM_op_1

type arm_op_fields_t = box_arm_op_ARM_op

(** val arm_op_fields : arm_op -> arm_op_fields_t **)

let arm_op_fields = function
| ARM_op (h, h0) ->
  { coq_Box_arm_op_ARM_op_0 = h; coq_Box_arm_op_ARM_op_1 = h0 }

(** val arm_op_construct : positive -> box_arm_op_ARM_op -> arm_op option **)

let arm_op_construct _ b =
  let { coq_Box_arm_op_ARM_op_0 = box_arm_op_ARM_op_0;
    coq_Box_arm_op_ARM_op_1 = box_arm_op_ARM_op_1 } = b
  in
  Some (ARM_op (box_arm_op_ARM_op_0, box_arm_op_ARM_op_1))

(** val arm_op_induction :
    (arm_mnemonic -> is_arm_mnemonic -> arm_options -> is_arm_options -> 'a1)
    -> arm_op -> is_arm_op -> 'a1 **)

let arm_op_induction his_ARM_op _ = function
| Coq_is_ARM_op (x0, p_, x1, p_0) -> his_ARM_op x0 p_ x1 p_0

(** val arm_op_eqb_fields :
    (arm_op -> arm_op -> bool) -> positive -> box_arm_op_ARM_op ->
    box_arm_op_ARM_op -> bool **)

let arm_op_eqb_fields _ _ a b =
  let { coq_Box_arm_op_ARM_op_0 = box_arm_op_ARM_op_0;
    coq_Box_arm_op_ARM_op_1 = box_arm_op_ARM_op_1 } = a
  in
  let { coq_Box_arm_op_ARM_op_0 = box_arm_op_ARM_op_2;
    coq_Box_arm_op_ARM_op_1 = box_arm_op_ARM_op_3 } = b
  in
  (&&) (arm_mnemonic_eqb box_arm_op_ARM_op_0 box_arm_op_ARM_op_2)
    ((&&) (arm_options_eqb box_arm_op_ARM_op_1 box_arm_op_ARM_op_3) true)

(** val arm_op_eqb : arm_op -> arm_op -> bool **)

let arm_op_eqb x1 x2 =
  let ARM_op (h, h0) = x1 in
  eqb_body arm_op_tag arm_op_fields (arm_op_eqb_fields (fun _ _ -> true))
    (arm_op_tag (ARM_op (h, h0))) { coq_Box_arm_op_ARM_op_0 = h;
    coq_Box_arm_op_ARM_op_1 = h0 } x2

(** val arm_op_eqb_OK : arm_op -> arm_op -> reflect **)

let arm_op_eqb_OK =
  iffP2 arm_op_eqb

(** val arm_op_eqb_OK_sumbool : arm_op -> arm_op -> bool **)

let arm_op_eqb_OK_sumbool =
  reflect_dec arm_op_eqb arm_op_eqb_OK

(** val eqTC_arm_op : arm_op eqTypeC **)

let eqTC_arm_op =
  { beq = arm_op_eqb; ceqP = arm_op_eqb_OK }

(** val arm_op_eqType : Equality.coq_type **)

let arm_op_eqType =
  ceqT_eqType eqTC_arm_op

(** val ad_nz :
    (register, empty, empty, rflag, condt) Arch_decl.arg_desc list **)

let ad_nz =
  map (coq_F arm_decl) (NF :: (ZF :: []))

(** val ad_nzc :
    (register, empty, empty, rflag, condt) Arch_decl.arg_desc list **)

let ad_nzc =
  map (coq_F arm_decl) (NF :: (ZF :: (CF :: [])))

(** val ad_nzcv :
    (register, empty, empty, rflag, condt) Arch_decl.arg_desc list **)

let ad_nzcv =
  map (coq_F arm_decl) (NF :: (ZF :: (CF :: (VF :: []))))

(** val coq_NF_of_word : wsize -> GRing.ComRing.sort -> bool **)

let coq_NF_of_word =
  msb

(** val coq_ZF_of_word : wsize -> GRing.ComRing.sort -> bool **)

let coq_ZF_of_word ws w =
  eq_op
    (GRing.ComRing.Exports.coq_GRing_ComRing__to__eqtype_Equality (word ws))
    w
    (GRing.zero
      (GRing.ComRing.Exports.coq_GRing_ComRing__to__GRing_Nmodule (word ws)))

(** val nzcv_of_aluop :
    wsize -> GRing.ComRing.sort -> coq_Z -> coq_Z -> sem_tuple **)

let nzcv_of_aluop ws res res_unsigned res_signed =
  Obj.magic ((Some (coq_NF_of_word ws res)), ((Some (coq_ZF_of_word ws res)),
    ((Some
    (negb
      (eq_op coq_BinNums_Z__canonical__eqtype_Equality
        (Obj.magic wunsigned ws res) (Obj.magic res_unsigned)))), (Some
    (negb
      (eq_op coq_BinNums_Z__canonical__eqtype_Equality
        (Obj.magic wsigned ws res) (Obj.magic res_signed)))))))

(** val nzcv_w_of_aluop :
    wsize -> GRing.ComRing.sort -> coq_Z -> coq_Z -> ltuple **)

let nzcv_w_of_aluop ws w wun wsi =
  merge_tuple
    (map (Obj.magic __)
      (map eval_ltype
        (Coq_lbool :: (Coq_lbool :: (Coq_lbool :: (Coq_lbool :: []))))))
    (map (Obj.magic __) (map eval_ltype ((Coq_lword ws) :: [])))
    (nzcv_of_aluop ws w wun wsi) w

(** val drop_nz :
    (register, empty, empty, rflag, condt) instr_desc_t -> (register, empty,
    empty, rflag, condt) instr_desc_t **)

let drop_nz =
  idt_drop2 arm_decl

(** val drop_nzc :
    (register, empty, empty, rflag, condt) instr_desc_t -> (register, empty,
    empty, rflag, condt) instr_desc_t **)

let drop_nzc =
  idt_drop3 arm_decl

(** val drop_nzcv :
    (register, empty, empty, rflag, condt) instr_desc_t -> (register, empty,
    empty, rflag, condt) instr_desc_t **)

let drop_nzcv =
  idt_drop4 arm_decl

(** val add_arguments :
    ltype list -> ltype list -> 'a1 sem_prod sem_prod -> 'a1 sem_prod **)

let add_arguments _ _ f =
  f

(** val mk_semi_cond :
    ltype list -> ltype list -> sem_tuple exec sem_prod -> sem_tuple exec
    sem_prod **)

let mk_semi_cond tin tout semi =
  let f0 = fun res cond ->
    if cond
    then sem_prod_const (map eval_ltype tout) res
    else sem_prod_ok (map eval_ltype tout)
           (sem_prod_tuple (map eval_ltype tout))
  in
  let f1 = sem_prod_app (map eval_ltype tin) semi f0 in
  add_arguments tin (Coq_lbool :: tout) f1

(** val mk_cond :
    (register, empty, empty, rflag, condt) instr_desc_t -> (register, empty,
    empty, rflag, condt) instr_desc_t **)

let mk_cond idt =
  { id_valid = idt.id_valid; id_msb_flag = MSB_MERGE; id_tin =
    (cat idt.id_tin (Coq_lbool :: idt.id_tout)); id_in =
    (cat idt.id_in ((coq_Ea arm_decl idt.id_nargs) :: idt.id_out)); id_tout =
    idt.id_tout; id_out = idt.id_out; id_semi =
    (mk_semi_cond idt.id_tin idt.id_tout idt.id_semi); id_args_kinds =
    (map (fun x -> cat x ((CAcond :: []) :: [])) idt.id_args_kinds);
    id_nargs = (S idt.id_nargs); id_str_jas = idt.id_str_jas; id_safe =
    idt.id_safe; id_pp_asm = idt.id_pp_asm }

(** val mk_semi1_shifted :
    shift_kind -> 'a1 exec sem_prod -> 'a1 exec sem_prod **)

let mk_semi1_shifted sk semi =
  Obj.magic (fun wn shift_amount ->
    let sham = wunsigned U8 shift_amount in
    Obj.magic semi (shift_op sk arm_decl.reg_size wn sham))

(** val mk_semi2_2_shifted :
    ltype -> shift_kind -> 'a1 exec sem_prod -> 'a1 exec sem_prod **)

let mk_semi2_2_shifted _ sk semi =
  Obj.magic (fun x wm shift_amount ->
    let sham = wunsigned U8 shift_amount in
    Obj.magic semi x (shift_op sk arm_decl.reg_size wm sham))

(** val mk_semi3_2_shifted :
    ltype -> ltype -> shift_kind -> 'a1 exec sem_prod -> 'a1 exec sem_prod **)

let mk_semi3_2_shifted _ _ sk semi =
  Obj.magic (fun x wm y shift_amount ->
    let sham = wunsigned U8 shift_amount in
    Obj.magic semi x (shift_op sk arm_decl.reg_size wm sham) y)

(** val mk_shifted :
    shift_kind -> (register, empty, empty, rflag, condt) instr_desc_t ->
    sem_tuple exec sem_prod -> (register, empty, empty, rflag, condt)
    instr_desc_t **)

let mk_shifted sk idt semi' =
  { id_valid = idt.id_valid; id_msb_flag = MSB_MERGE; id_tin =
    (cat idt.id_tin ((Coq_lword U8) :: [])); id_in =
    (cat idt.id_in ((coq_Ea arm_decl idt.id_nargs) :: [])); id_tout =
    idt.id_tout; id_out = idt.id_out; id_semi = semi'; id_args_kinds =
    (map (fun x ->
      cat x (((CAimm ((CAimmC_arm_shift_amout sk), U8)) :: []) :: []))
      idt.id_args_kinds); id_nargs = (S idt.id_nargs); id_str_jas =
    idt.id_str_jas; id_safe = idt.id_safe; id_pp_asm = idt.id_pp_asm }

(** val ak_reg_reg_imm_ : expected_wencoding -> arg_kind list list list **)

let ak_reg_reg_imm_ ew =
  ((CAreg :: []) :: ((CAreg :: []) :: (((CAimm ((CAimmC_arm_wencoding ew),
    arm_decl.reg_size)) :: []) :: []))) :: []

(** val ak_reg_reg_imm_shift :
    wsize -> shift_kind -> arg_kind list list list **)

let ak_reg_reg_imm_shift ws sk =
  ((CAreg :: []) :: ((CAreg :: []) :: (((CAimm ((CAimmC_arm_shift_amout sk),
    ws)) :: []) :: []))) :: []

(** val ak_reg_reg_reg_or_imm_ : expected_wencoding -> args_kinds list **)

let ak_reg_reg_reg_or_imm_ ew =
  cat ak_reg_reg_reg (ak_reg_reg_imm_ ew)

(** val ak_reg_reg_reg_or_imm :
    arm_options -> expected_wencoding -> i_args_kinds **)

let ak_reg_reg_reg_or_imm opts ew =
  if isSome opts.has_shift then ak_reg_reg_reg else ak_reg_reg_reg_or_imm_ ew

(** val ak_reg_imm_ : expected_wencoding -> arg_kind list list list **)

let ak_reg_imm_ ew =
  ((CAreg :: []) :: (((CAimm ((CAimmC_arm_wencoding ew),
    arm_decl.reg_size)) :: []) :: [])) :: []

(** val ak_reg_reg_or_imm_ : expected_wencoding -> args_kinds list **)

let ak_reg_reg_or_imm_ ew =
  cat ak_reg_reg (ak_reg_imm_ ew)

(** val ak_reg_reg_or_imm :
    arm_options -> expected_wencoding -> i_args_kinds **)

let ak_reg_reg_or_imm opts ew =
  if isSome opts.has_shift then ak_reg_reg else ak_reg_reg_or_imm_ ew

(** val chk_imm_accept_shift : expected_wencoding **)

let chk_imm_accept_shift =
  { on_shift = (WE_allowed true); on_none = (WE_allowed false) }

(** val chk_imm_accept_shift_w12 : arm_options -> expected_wencoding **)

let chk_imm_accept_shift_w12 opts =
  { on_shift = (WE_allowed true); on_none =
    (if opts.set_flags then WE_allowed false else W12_encoding) }

(** val chk_imm_w16_encoding : bool -> expected_wencoding **)

let chk_imm_w16_encoding opts =
  let allowed = if opts then WE_allowed false else W16_encoding in
  { on_shift = allowed; on_none = allowed }

(** val chk_imm_reject_shift : expected_wencoding **)

let chk_imm_reject_shift =
  { on_shift = (WE_allowed false); on_none = (WE_allowed false) }

(** val pp_arm_op :
    arm_mnemonic -> arm_options -> (register, empty, empty, rflag, condt)
    asm_arg list -> (register, empty, empty, rflag, condt) pp_asm_op **)

let pp_arm_op mn _ args =
  { pp_aop_name = (string_of_arm_mnemonic mn); pp_aop_ext = PP_name;
    pp_aop_args = (map (fun a -> (arm_decl.reg_size, a)) args) }

(** val arm_ADD_semi : sem_tuple -> sem_tuple -> sem_tuple **)

let arm_ADD_semi wn wm =
  nzcv_w_of_aluop arm_decl.reg_size
    (GRing.add
      (GRing.ComRing.Exports.coq_GRing_ComRing__to__GRing_Nmodule
        (word arm_decl.reg_size)) wn wm)
    (Z.add (wunsigned arm_decl.reg_size wn) (wunsigned arm_decl.reg_size wm))
    (Z.add (wsigned arm_decl.reg_size wn) (wsigned arm_decl.reg_size wm))

(** val arm_ADD_instr :
    arm_options -> (register, empty, empty, rflag, condt) instr_desc_t **)

let arm_ADD_instr opts =
  let string_of_arm_mnemonic0 = fun mn ->
    (^) (string_of_arm_mnemonic mn)
      ((^) (if opts.set_flags then "S" else "")
        (if opts.is_conditional then "cc" else ""))
  in
  let mn = ADD in
  let tin = (lreg arm_decl) :: ((lreg arm_decl) :: []) in
  let x = { id_valid = true; id_msb_flag = MSB_MERGE; id_tin = tin; id_in =
    ((coq_Ea arm_decl (S O)) :: ((coq_Ea arm_decl (S (S O))) :: []));
    id_tout =
    (cat (Coq_lbool :: (Coq_lbool :: (Coq_lbool :: (Coq_lbool :: []))))
      ((lreg arm_decl) :: [])); id_out =
    (cat ad_nzcv ((coq_Ea arm_decl O) :: [])); id_semi =
    (sem_prod_ok (map eval_ltype tin) (Obj.magic arm_ADD_semi));
    id_args_kinds =
    (ak_reg_reg_reg_or_imm opts (chk_imm_accept_shift_w12 opts)); id_nargs =
    (S (S (S O))); id_str_jas = (pp_s (string_of_arm_mnemonic0 mn));
    id_safe = []; id_pp_asm = (pp_arm_op mn opts) }
  in
  let x0 =
    match opts.has_shift with
    | Some sk ->
      mk_shifted sk x (mk_semi2_2_shifted (lreg arm_decl) sk x.id_semi)
    | None -> x
  in
  if opts.set_flags then x0 else drop_nzcv x0

(** val arm_ADC_semi : sem_tuple -> sem_tuple -> bool -> sem_tuple **)

let arm_ADC_semi wn wm cf =
  let c = Z.b2z cf in
  nzcv_w_of_aluop arm_decl.reg_size
    (GRing.add
      (GRing.ComRing.Exports.coq_GRing_ComRing__to__GRing_Nmodule
        (word arm_decl.reg_size))
      (GRing.add
        (GRing.ComRing.Exports.coq_GRing_ComRing__to__GRing_Nmodule
          (word arm_decl.reg_size)) wn wm) (wrepr arm_decl.reg_size c))
    (Z.add
      (Z.add (wunsigned arm_decl.reg_size wn)
        (wunsigned arm_decl.reg_size wm)) c)
    (Z.add
      (Z.add (wsigned arm_decl.reg_size wn) (wsigned arm_decl.reg_size wm)) c)

(** val arm_ADC_instr :
    arm_options -> (register, empty, empty, rflag, condt) instr_desc_t **)

let arm_ADC_instr opts =
  let string_of_arm_mnemonic0 = fun mn ->
    (^) (string_of_arm_mnemonic mn)
      ((^) (if opts.set_flags then "S" else "")
        (if opts.is_conditional then "cc" else ""))
  in
  let mn = ADC in
  let tin = (lreg arm_decl) :: ((lreg arm_decl) :: (Coq_lbool :: [])) in
  let x = { id_valid = true; id_msb_flag = MSB_MERGE; id_tin = tin; id_in =
    ((coq_Ea arm_decl (S O)) :: ((coq_Ea arm_decl (S (S O))) :: ((coq_F
                                                                   arm_decl
                                                                   CF) :: [])));
    id_tout =
    (cat (Coq_lbool :: (Coq_lbool :: (Coq_lbool :: (Coq_lbool :: []))))
      ((lreg arm_decl) :: [])); id_out =
    (cat ad_nzcv ((coq_Ea arm_decl O) :: [])); id_semi =
    (sem_prod_ok (map eval_ltype tin) (Obj.magic arm_ADC_semi));
    id_args_kinds = (ak_reg_reg_reg_or_imm opts chk_imm_accept_shift);
    id_nargs = (S (S (S O))); id_str_jas =
    (pp_s (string_of_arm_mnemonic0 mn)); id_safe = []; id_pp_asm =
    (pp_arm_op mn opts) }
  in
  let x0 =
    match opts.has_shift with
    | Some sk ->
      mk_shifted sk x
        (mk_semi3_2_shifted (lreg arm_decl) Coq_lbool sk x.id_semi)
    | None -> x
  in
  if opts.set_flags then x0 else drop_nzcv x0

(** val arm_MUL_semi : sem_tuple -> sem_tuple -> sem_tuple **)

let arm_MUL_semi wn wm =
  let res =
    GRing.mul
      (GRing.ComRing.Exports.coq_GRing_ComRing__to__GRing_SemiRing
        (word arm_decl.reg_size)) wn wm
  in
  Obj.magic ((Some (coq_NF_of_word arm_decl.reg_size res)), ((Some
    (coq_ZF_of_word arm_decl.reg_size res)), res))

(** val arm_high_registers : register list **)

let arm_high_registers =
  R08 :: (R09 :: (R10 :: (R11 :: (R12 :: (LR :: [])))))

(** val arm_MUL_instr :
    arm_options -> (register, empty, empty, rflag, condt) instr_desc_t **)

let arm_MUL_instr opts =
  let string_of_arm_mnemonic0 = fun mn ->
    (^) (string_of_arm_mnemonic mn)
      ((^) (if opts.set_flags then "S" else "")
        (if opts.is_conditional then "cc" else ""))
  in
  let mn = MUL in
  let tin = (lreg arm_decl) :: ((lreg arm_decl) :: []) in
  let iop = fun n ->
    if opts.set_flags
    then Arch_decl.ADExplicit ((AK_mem Aligned), n, (Arch_decl.ACR_subset
           arm_high_registers))
    else coq_Ea arm_decl (S n)
  in
  let x = { id_valid = true; id_msb_flag = MSB_MERGE; id_tin = tin; id_in =
    ((iop O) :: ((iop (S O)) :: [])); id_tout =
    (cat (Coq_lbool :: (Coq_lbool :: [])) ((lreg arm_decl) :: [])); id_out =
    (cat ad_nz ((coq_Ea arm_decl O) :: [])); id_semi =
    (sem_prod_ok (map eval_ltype tin) (Obj.magic arm_MUL_semi));
    id_args_kinds = (if opts.set_flags then ak_reg_reg else ak_reg_reg_reg);
    id_nargs = (if opts.set_flags then S (S O) else S (S (S O)));
    id_str_jas = (pp_s (string_of_arm_mnemonic0 mn)); id_safe = [];
    id_pp_asm = (pp_arm_op mn opts) }
  in
  if opts.set_flags then x else drop_nz x

(** val arm_MLA_semi : sem_tuple -> sem_tuple -> sem_tuple -> sem_tuple **)

let arm_MLA_semi wn wm wa =
  GRing.add
    (GRing.SemiRing.Exports.coq_GRing_SemiRing__to__GRing_Nmodule
      (GRing.ComRing.Exports.coq_GRing_ComRing__to__GRing_SemiRing
        (word arm_decl.reg_size)))
    (GRing.mul
      (GRing.ComRing.Exports.coq_GRing_ComRing__to__GRing_SemiRing
        (word arm_decl.reg_size)) wn wm) wa

(** val arm_MLA_instr :
    arm_options -> (register, empty, empty, rflag, condt) instr_desc_t **)

let arm_MLA_instr opts =
  let string_of_arm_mnemonic0 = fun mn ->
    (^) (string_of_arm_mnemonic mn)
      ((^) (if opts.set_flags then "S" else "")
        (if opts.is_conditional then "cc" else ""))
  in
  let mn = MLA in
  let tin = (lreg arm_decl) :: ((lreg arm_decl) :: ((lreg arm_decl) :: [])) in
  { id_valid = true; id_msb_flag = MSB_MERGE; id_tin = tin; id_in =
  ((coq_Ea arm_decl (S O)) :: ((coq_Ea arm_decl (S (S O))) :: ((coq_Ea
                                                                 arm_decl (S
                                                                 (S (S O)))) :: [])));
  id_tout = ((lreg arm_decl) :: []); id_out = ((coq_Ea arm_decl O) :: []);
  id_semi = (sem_prod_ok (map eval_ltype tin) (Obj.magic arm_MLA_semi));
  id_args_kinds = ak_reg_reg_reg_reg; id_nargs = (S (S (S (S O))));
  id_str_jas = (pp_s (string_of_arm_mnemonic0 mn)); id_safe = []; id_pp_asm =
  (pp_arm_op mn opts) }

(** val arm_MLS_semi : sem_tuple -> sem_tuple -> sem_tuple -> sem_tuple **)

let arm_MLS_semi wn wm wa =
  GRing.add
    (GRing.ComRing.Exports.coq_GRing_ComRing__to__GRing_Nmodule
      (word arm_decl.reg_size)) wa
    (GRing.opp
      (GRing.Ring.Exports.join_GRing_Ring_between_GRing_SemiRing_and_GRing_Zmodule
        (GRing.ComRing.Exports.coq_GRing_ComRing__to__GRing_Ring
          (word arm_decl.reg_size)))
      (GRing.mul
        (GRing.ComRing.Exports.coq_GRing_ComRing__to__GRing_SemiRing
          (word arm_decl.reg_size)) wn wm))

(** val arm_MLS_instr :
    arm_options -> (register, empty, empty, rflag, condt) instr_desc_t **)

let arm_MLS_instr opts =
  let string_of_arm_mnemonic0 = fun mn ->
    (^) (string_of_arm_mnemonic mn)
      ((^) (if opts.set_flags then "S" else "")
        (if opts.is_conditional then "cc" else ""))
  in
  let mn = MLS in
  let tin = (lreg arm_decl) :: ((lreg arm_decl) :: ((lreg arm_decl) :: [])) in
  { id_valid = true; id_msb_flag = MSB_MERGE; id_tin = tin; id_in =
  ((coq_Ea arm_decl (S O)) :: ((coq_Ea arm_decl (S (S O))) :: ((coq_Ea
                                                                 arm_decl (S
                                                                 (S (S O)))) :: [])));
  id_tout = ((lreg arm_decl) :: []); id_out = ((coq_Ea arm_decl O) :: []);
  id_semi = (sem_prod_ok (map eval_ltype tin) (Obj.magic arm_MLS_semi));
  id_args_kinds = ak_reg_reg_reg_reg; id_nargs = (S (S (S (S O))));
  id_str_jas = (pp_s (string_of_arm_mnemonic0 mn)); id_safe = []; id_pp_asm =
  (pp_arm_op mn opts) }

(** val arm_SDIV_semi : sem_tuple -> sem_tuple -> sem_tuple **)

let arm_SDIV_semi wn wm =
  wdivi arm_decl.reg_size wn wm

(** val arm_SDIV_instr :
    arm_options -> (register, empty, empty, rflag, condt) instr_desc_t **)

let arm_SDIV_instr opts =
  let string_of_arm_mnemonic0 = fun mn ->
    (^) (string_of_arm_mnemonic mn)
      ((^) (if opts.set_flags then "S" else "")
        (if opts.is_conditional then "cc" else ""))
  in
  let mn = SDIV in
  let tin = (lreg arm_decl) :: ((lreg arm_decl) :: []) in
  { id_valid = true; id_msb_flag = MSB_MERGE; id_tin = tin; id_in =
  ((coq_Ea arm_decl (S O)) :: ((coq_Ea arm_decl (S (S O))) :: [])); id_tout =
  ((lreg arm_decl) :: []); id_out = ((coq_Ea arm_decl O) :: []); id_semi =
  (sem_prod_ok (map eval_ltype tin) (Obj.magic arm_SDIV_semi));
  id_args_kinds = ak_reg_reg_reg; id_nargs = (S (S (S O))); id_str_jas =
  (pp_s (string_of_arm_mnemonic0 mn)); id_safe = []; id_pp_asm =
  (pp_arm_op mn opts) }

(** val arm_SUB_semi : sem_tuple -> sem_tuple -> sem_tuple **)

let arm_SUB_semi wn wm =
  let wmnot = wnot arm_decl.reg_size wm in
  nzcv_w_of_aluop arm_decl.reg_size
    (GRing.add
      (GRing.ComRing.Exports.coq_GRing_ComRing__to__GRing_Nmodule
        (word arm_decl.reg_size))
      (GRing.add
        (GRing.ComRing.Exports.coq_GRing_ComRing__to__GRing_Nmodule
          (word arm_decl.reg_size)) wn wmnot)
      (GRing.one
        (GRing.ComRing.Exports.coq_GRing_ComRing__to__GRing_SemiRing
          (word arm_decl.reg_size))))
    (Z.add
      (Z.add (wunsigned arm_decl.reg_size wn)
        (wunsigned arm_decl.reg_size wmnot)) (Zpos Coq_xH))
    (Z.add
      (Z.add (wsigned arm_decl.reg_size wn) (wsigned arm_decl.reg_size wmnot))
      (Zpos Coq_xH))

(** val arm_SUB_instr :
    arm_options -> (register, empty, empty, rflag, condt) instr_desc_t **)

let arm_SUB_instr opts =
  let string_of_arm_mnemonic0 = fun mn ->
    (^) (string_of_arm_mnemonic mn)
      ((^) (if opts.set_flags then "S" else "")
        (if opts.is_conditional then "cc" else ""))
  in
  let mn = SUB in
  let tin = (lreg arm_decl) :: ((lreg arm_decl) :: []) in
  let x = { id_valid = true; id_msb_flag = MSB_MERGE; id_tin = tin; id_in =
    ((coq_Ea arm_decl (S O)) :: ((coq_Ea arm_decl (S (S O))) :: []));
    id_tout =
    (cat (Coq_lbool :: (Coq_lbool :: (Coq_lbool :: (Coq_lbool :: []))))
      ((lreg arm_decl) :: [])); id_out =
    (cat ad_nzcv ((coq_Ea arm_decl O) :: [])); id_semi =
    (sem_prod_ok (map eval_ltype tin) (Obj.magic arm_SUB_semi));
    id_args_kinds =
    (ak_reg_reg_reg_or_imm opts (chk_imm_accept_shift_w12 opts)); id_nargs =
    (S (S (S O))); id_str_jas = (pp_s (string_of_arm_mnemonic0 mn));
    id_safe = []; id_pp_asm = (pp_arm_op mn opts) }
  in
  let x0 =
    match opts.has_shift with
    | Some sk ->
      mk_shifted sk x (mk_semi2_2_shifted (lreg arm_decl) sk x.id_semi)
    | None -> x
  in
  if opts.set_flags then x0 else drop_nzcv x0

(** val arm_SBC_semi : sem_tuple -> sem_tuple -> bool -> sem_tuple **)

let arm_SBC_semi wn wm cf =
  arm_ADC_semi wn (wnot arm_decl.reg_size wm) cf

(** val arm_SBC_instr :
    arm_options -> (register, empty, empty, rflag, condt) instr_desc_t **)

let arm_SBC_instr opts =
  let string_of_arm_mnemonic0 = fun mn ->
    (^) (string_of_arm_mnemonic mn)
      ((^) (if opts.set_flags then "S" else "")
        (if opts.is_conditional then "cc" else ""))
  in
  let mn = SBC in
  let tin = (lreg arm_decl) :: ((lreg arm_decl) :: (Coq_lbool :: [])) in
  let x = { id_valid = true; id_msb_flag = MSB_MERGE; id_tin = tin; id_in =
    ((coq_Ea arm_decl (S O)) :: ((coq_Ea arm_decl (S (S O))) :: ((coq_F
                                                                   arm_decl
                                                                   CF) :: [])));
    id_tout =
    (cat (Coq_lbool :: (Coq_lbool :: (Coq_lbool :: (Coq_lbool :: []))))
      ((lreg arm_decl) :: [])); id_out =
    (cat ad_nzcv ((coq_Ea arm_decl O) :: [])); id_semi =
    (sem_prod_ok (map eval_ltype tin) (Obj.magic arm_SBC_semi));
    id_args_kinds = (ak_reg_reg_reg_or_imm opts chk_imm_accept_shift);
    id_nargs = (S (S (S O))); id_str_jas =
    (pp_s (string_of_arm_mnemonic0 mn)); id_safe = []; id_pp_asm =
    (pp_arm_op mn opts) }
  in
  let x0 =
    match opts.has_shift with
    | Some sk ->
      mk_shifted sk x
        (mk_semi3_2_shifted (lreg arm_decl) Coq_lbool sk x.id_semi)
    | None -> x
  in
  if opts.set_flags then x0 else drop_nzcv x0

(** val arm_RSB_instr :
    arm_options -> (register, empty, empty, rflag, condt) instr_desc_t **)

let arm_RSB_instr opts =
  let string_of_arm_mnemonic0 = fun mn ->
    (^) (string_of_arm_mnemonic mn)
      ((^) (if opts.set_flags then "S" else "")
        (if opts.is_conditional then "cc" else ""))
  in
  let mn = RSB in
  let tin = (lreg arm_decl) :: ((lreg arm_decl) :: []) in
  let arm_RSB_semi = fun wn wm -> arm_SUB_semi wm wn in
  let x = { id_valid = true; id_msb_flag = MSB_MERGE; id_tin = tin; id_in =
    ((coq_Ea arm_decl (S O)) :: ((coq_Ea arm_decl (S (S O))) :: []));
    id_tout =
    (cat (Coq_lbool :: (Coq_lbool :: (Coq_lbool :: (Coq_lbool :: []))))
      ((lreg arm_decl) :: [])); id_out =
    (cat ad_nzcv ((coq_Ea arm_decl O) :: [])); id_semi =
    (sem_prod_ok (map eval_ltype tin) (Obj.magic arm_RSB_semi));
    id_args_kinds = (ak_reg_reg_reg_or_imm opts chk_imm_accept_shift);
    id_nargs = (S (S (S O))); id_str_jas =
    (pp_s (string_of_arm_mnemonic0 mn)); id_safe = []; id_pp_asm =
    (pp_arm_op mn opts) }
  in
  let x0 =
    match opts.has_shift with
    | Some sk ->
      mk_shifted sk x (mk_semi2_2_shifted (lreg arm_decl) sk x.id_semi)
    | None -> x
  in
  if opts.set_flags then x0 else drop_nzcv x0

(** val arm_UDIV_semi : sem_tuple -> sem_tuple -> sem_tuple **)

let arm_UDIV_semi wn wm =
  wdiv arm_decl.reg_size wn wm

(** val arm_UDIV_instr :
    arm_options -> (register, empty, empty, rflag, condt) instr_desc_t **)

let arm_UDIV_instr opts =
  let string_of_arm_mnemonic0 = fun mn ->
    (^) (string_of_arm_mnemonic mn)
      ((^) (if opts.set_flags then "S" else "")
        (if opts.is_conditional then "cc" else ""))
  in
  let mn = UDIV in
  let tin = (lreg arm_decl) :: ((lreg arm_decl) :: []) in
  { id_valid = true; id_msb_flag = MSB_MERGE; id_tin = tin; id_in =
  ((coq_Ea arm_decl (S O)) :: ((coq_Ea arm_decl (S (S O))) :: [])); id_tout =
  ((lreg arm_decl) :: []); id_out = ((coq_Ea arm_decl O) :: []); id_semi =
  (sem_prod_ok (map eval_ltype tin) (Obj.magic arm_UDIV_semi));
  id_args_kinds = ak_reg_reg_reg; id_nargs = (S (S (S O))); id_str_jas =
  (pp_s (string_of_arm_mnemonic0 mn)); id_safe = []; id_pp_asm =
  (pp_arm_op mn opts) }

(** val arm_UMULL_semi : sem_tuple -> sem_tuple -> sem_tuple **)

let arm_UMULL_semi wn wm =
  let (hi, lo) = wumul arm_decl.reg_size wn wm in Obj.magic (lo, hi)

(** val arm_UMULL_instr :
    arm_options -> (register, empty, empty, rflag, condt) instr_desc_t **)

let arm_UMULL_instr opts =
  let string_of_arm_mnemonic0 = fun mn ->
    (^) (string_of_arm_mnemonic mn)
      ((^) (if opts.set_flags then "S" else "")
        (if opts.is_conditional then "cc" else ""))
  in
  let mn = UMULL in
  let tin = (lreg arm_decl) :: ((lreg arm_decl) :: []) in
  { id_valid = true; id_msb_flag = MSB_MERGE; id_tin = tin; id_in =
  ((coq_Ea arm_decl (S (S O))) :: ((coq_Ea arm_decl (S (S (S O)))) :: []));
  id_tout = ((lreg arm_decl) :: ((lreg arm_decl) :: [])); id_out =
  ((coq_Ea arm_decl O) :: ((coq_Ea arm_decl (S O)) :: [])); id_semi =
  (sem_prod_ok (map eval_ltype tin) (Obj.magic arm_UMULL_semi));
  id_args_kinds = ak_reg_reg_reg_reg; id_nargs = (S (S (S (S O))));
  id_str_jas = (pp_s (string_of_arm_mnemonic0 mn)); id_safe = []; id_pp_asm =
  (pp_arm_op mn opts) }

(** val arm_UMAAL_semi :
    sem_tuple -> sem_tuple -> sem_tuple -> sem_tuple -> sem_tuple **)

let arm_UMAAL_semi wa wb wn wm =
  let r =
    Z.add
      (Z.add (wunsigned arm_decl.reg_size wa)
        (wunsigned arm_decl.reg_size wb))
      (Z.mul (wunsigned arm_decl.reg_size wn)
        (wunsigned arm_decl.reg_size wm))
  in
  Obj.magic ((wrepr arm_decl.reg_size r), (high_bits arm_decl.reg_size r))

(** val arm_UMAAL_instr :
    arm_options -> (register, empty, empty, rflag, condt) instr_desc_t **)

let arm_UMAAL_instr opts =
  let string_of_arm_mnemonic0 = fun mn ->
    (^) (string_of_arm_mnemonic mn)
      ((^) (if opts.set_flags then "S" else "")
        (if opts.is_conditional then "cc" else ""))
  in
  let mn = UMAAL in
  let tin =
    (lreg arm_decl) :: ((lreg arm_decl) :: ((lreg arm_decl) :: ((lreg
                                                                  arm_decl) :: [])))
  in
  { id_valid = true; id_msb_flag = MSB_MERGE; id_tin = tin; id_in =
  ((coq_Ea arm_decl O) :: ((coq_Ea arm_decl (S O)) :: ((coq_Ea arm_decl (S (S
                                                         O))) :: ((coq_Ea
                                                                    arm_decl
                                                                    (S (S (S
                                                                    O)))) :: []))));
  id_tout = ((lreg arm_decl) :: ((lreg arm_decl) :: [])); id_out =
  ((coq_Ea arm_decl O) :: ((coq_Ea arm_decl (S O)) :: [])); id_semi =
  (sem_prod_ok (map eval_ltype tin) (Obj.magic arm_UMAAL_semi));
  id_args_kinds = ak_reg_reg_reg_reg; id_nargs = (S (S (S (S O))));
  id_str_jas = (pp_s (string_of_arm_mnemonic0 mn)); id_safe = []; id_pp_asm =
  (pp_arm_op mn opts) }

(** val arm_UMLAL_semi :
    sem_tuple -> sem_tuple -> sem_tuple -> sem_tuple -> sem_tuple **)

let arm_UMLAL_semi dlo dhi wn wm =
  let (hi, lo) = wumul arm_decl.reg_size wn wm in
  Obj.magic wdaddu arm_decl.reg_size dhi dlo hi lo

(** val arm_UMLAL_instr :
    arm_options -> (register, empty, empty, rflag, condt) instr_desc_t **)

let arm_UMLAL_instr opts =
  let string_of_arm_mnemonic0 = fun mn ->
    (^) (string_of_arm_mnemonic mn)
      ((^) (if opts.set_flags then "S" else "")
        (if opts.is_conditional then "cc" else ""))
  in
  let mn = UMLAL in
  let tin =
    (lreg arm_decl) :: ((lreg arm_decl) :: ((lreg arm_decl) :: ((lreg
                                                                  arm_decl) :: [])))
  in
  { id_valid = true; id_msb_flag = MSB_MERGE; id_tin = tin; id_in =
  ((coq_Ea arm_decl O) :: ((coq_Ea arm_decl (S O)) :: ((coq_Ea arm_decl (S (S
                                                         O))) :: ((coq_Ea
                                                                    arm_decl
                                                                    (S (S (S
                                                                    O)))) :: []))));
  id_tout = ((lreg arm_decl) :: ((lreg arm_decl) :: [])); id_out =
  ((coq_Ea arm_decl O) :: ((coq_Ea arm_decl (S O)) :: [])); id_semi =
  (sem_prod_ok (map eval_ltype tin) (Obj.magic arm_UMLAL_semi));
  id_args_kinds = ak_reg_reg_reg_reg; id_nargs = (S (S (S (S O))));
  id_str_jas = (pp_s (string_of_arm_mnemonic0 mn)); id_safe = []; id_pp_asm =
  (pp_arm_op mn opts) }

(** val arm_SMULL_semi : sem_tuple -> sem_tuple -> sem_tuple **)

let arm_SMULL_semi wn wm =
  let (hi, lo) = wsmul arm_decl.reg_size wn wm in Obj.magic (lo, hi)

(** val arm_SMULL_instr :
    arm_options -> (register, empty, empty, rflag, condt) instr_desc_t **)

let arm_SMULL_instr opts =
  let string_of_arm_mnemonic0 = fun mn ->
    (^) (string_of_arm_mnemonic mn)
      ((^) (if opts.set_flags then "S" else "")
        (if opts.is_conditional then "cc" else ""))
  in
  let mn = SMULL in
  let tin = (lreg arm_decl) :: ((lreg arm_decl) :: []) in
  { id_valid = true; id_msb_flag = MSB_MERGE; id_tin = tin; id_in =
  ((coq_Ea arm_decl (S (S O))) :: ((coq_Ea arm_decl (S (S (S O)))) :: []));
  id_tout = ((lreg arm_decl) :: ((lreg arm_decl) :: [])); id_out =
  ((coq_Ea arm_decl O) :: ((coq_Ea arm_decl (S O)) :: [])); id_semi =
  (sem_prod_ok (map eval_ltype tin) (Obj.magic arm_SMULL_semi));
  id_args_kinds = ak_reg_reg_reg_reg; id_nargs = (S (S (S (S O))));
  id_str_jas = (pp_s (string_of_arm_mnemonic0 mn)); id_safe = []; id_pp_asm =
  (pp_arm_op mn opts) }

(** val arm_SMLAL_semi :
    sem_tuple -> sem_tuple -> sem_tuple -> sem_tuple -> sem_tuple **)

let arm_SMLAL_semi dlo dhi wn wm =
  let (hi, lo) = wsmul arm_decl.reg_size wn wm in
  Obj.magic wdadds arm_decl.reg_size dhi dlo hi lo

(** val arm_SMLAL_instr :
    arm_options -> (register, empty, empty, rflag, condt) instr_desc_t **)

let arm_SMLAL_instr opts =
  let string_of_arm_mnemonic0 = fun mn ->
    (^) (string_of_arm_mnemonic mn)
      ((^) (if opts.set_flags then "S" else "")
        (if opts.is_conditional then "cc" else ""))
  in
  let mn = SMLAL in
  let tin =
    (lreg arm_decl) :: ((lreg arm_decl) :: ((lreg arm_decl) :: ((lreg
                                                                  arm_decl) :: [])))
  in
  { id_valid = true; id_msb_flag = MSB_MERGE; id_tin = tin; id_in =
  ((coq_Ea arm_decl O) :: ((coq_Ea arm_decl (S O)) :: ((coq_Ea arm_decl (S (S
                                                         O))) :: ((coq_Ea
                                                                    arm_decl
                                                                    (S (S (S
                                                                    O)))) :: []))));
  id_tout = ((lreg arm_decl) :: ((lreg arm_decl) :: [])); id_out =
  ((coq_Ea arm_decl O) :: ((coq_Ea arm_decl (S O)) :: [])); id_semi =
  (sem_prod_ok (map eval_ltype tin) (Obj.magic arm_SMLAL_semi));
  id_args_kinds = ak_reg_reg_reg_reg; id_nargs = (S (S (S (S O))));
  id_str_jas = (pp_s (string_of_arm_mnemonic0 mn)); id_safe = []; id_pp_asm =
  (pp_arm_op mn opts) }

(** val arm_SMMUL_semi : sem_tuple -> sem_tuple -> sem_tuple **)

let arm_SMMUL_semi wn wm =
  wmulhs arm_decl.reg_size wn wm

(** val arm_SMMUL_instr :
    arm_options -> (register, empty, empty, rflag, condt) instr_desc_t **)

let arm_SMMUL_instr opts =
  let string_of_arm_mnemonic0 = fun mn ->
    (^) (string_of_arm_mnemonic mn)
      ((^) (if opts.set_flags then "S" else "")
        (if opts.is_conditional then "cc" else ""))
  in
  let mn = SMMUL in
  let tin = (lreg arm_decl) :: ((lreg arm_decl) :: []) in
  { id_valid = true; id_msb_flag = MSB_MERGE; id_tin = tin; id_in =
  ((coq_Ea arm_decl (S O)) :: ((coq_Ea arm_decl (S (S O))) :: [])); id_tout =
  ((lreg arm_decl) :: []); id_out = ((coq_Ea arm_decl O) :: []); id_semi =
  (sem_prod_ok (map eval_ltype tin) (Obj.magic arm_SMMUL_semi));
  id_args_kinds = ak_reg_reg_reg; id_nargs = (S (S (S O))); id_str_jas =
  (pp_s (string_of_arm_mnemonic0 mn)); id_safe = []; id_pp_asm =
  (pp_arm_op mn opts) }

(** val arm_SMMULR_semi : sem_tuple -> sem_tuple -> sem_tuple **)

let arm_SMMULR_semi wn wm =
  high_bits arm_decl.reg_size
    (Z.add
      (Z.mul (wsigned arm_decl.reg_size wn) (wsigned arm_decl.reg_size wm))
      (Zpos (Coq_xO (Coq_xO (Coq_xO (Coq_xO (Coq_xO (Coq_xO (Coq_xO (Coq_xO
      (Coq_xO (Coq_xO (Coq_xO (Coq_xO (Coq_xO (Coq_xO (Coq_xO (Coq_xO (Coq_xO
      (Coq_xO (Coq_xO (Coq_xO (Coq_xO (Coq_xO (Coq_xO (Coq_xO (Coq_xO (Coq_xO
      (Coq_xO (Coq_xO (Coq_xO (Coq_xO (Coq_xO
      Coq_xH)))))))))))))))))))))))))))))))))

(** val arm_SMMULR_instr :
    arm_options -> (register, empty, empty, rflag, condt) instr_desc_t **)

let arm_SMMULR_instr opts =
  let string_of_arm_mnemonic0 = fun mn ->
    (^) (string_of_arm_mnemonic mn)
      ((^) (if opts.set_flags then "S" else "")
        (if opts.is_conditional then "cc" else ""))
  in
  let mn = SMMULR in
  let tin = (lreg arm_decl) :: ((lreg arm_decl) :: []) in
  { id_valid = true; id_msb_flag = MSB_MERGE; id_tin = tin; id_in =
  ((coq_Ea arm_decl (S O)) :: ((coq_Ea arm_decl (S (S O))) :: [])); id_tout =
  ((lreg arm_decl) :: []); id_out = ((coq_Ea arm_decl O) :: []); id_semi =
  (sem_prod_ok (map eval_ltype tin) (Obj.magic arm_SMMULR_semi));
  id_args_kinds = ak_reg_reg_reg; id_nargs = (S (S (S O))); id_str_jas =
  (pp_s (string_of_arm_mnemonic0 mn)); id_safe = []; id_pp_asm =
  (pp_arm_op mn opts) }

(** val get_hw :
    halfword -> (register, empty, empty, rflag, condt) wreg ->
    GRing.ComRing.sort **)

let get_hw hw x =
  match split_vec arm_decl.reg_size (S (S (S (S (S (S (S (S (S (S (S (S (S (S
          (S (S O)))))))))))))))) x with
  | [] ->
    GRing.zero
      (GRing.ComRing.Exports.coq_GRing_ComRing__to__GRing_Nmodule (word U16))
  | lo :: l ->
    (match l with
     | [] ->
       GRing.zero
         (GRing.ComRing.Exports.coq_GRing_ComRing__to__GRing_Nmodule
           (word U16))
     | hi :: l0 ->
       (match l0 with
        | [] -> (match hw with
                 | HWB -> Obj.magic lo
                 | HWT -> Obj.magic hi)
        | _ :: _ ->
          GRing.zero
            (GRing.ComRing.Exports.coq_GRing_ComRing__to__GRing_Nmodule
              (word U16))))

(** val arm_smul_hw_semi :
    halfword -> halfword -> (register, empty, empty, rflag, condt) wreg ->
    (register, empty, empty, rflag, condt) wreg -> (register, empty, empty,
    rflag, condt) wreg **)

let arm_smul_hw_semi hwn hwm wn wm =
  let n = get_hw hwn wn in
  let m = get_hw hwm wm in
  let r = Z.mul (wsigned U16 n) (wsigned U16 m) in wrepr U32 r

(** val arm_smul_hw_instr :
    arm_options -> halfword -> halfword -> (register, empty, empty, rflag,
    condt) instr_desc_t **)

let arm_smul_hw_instr opts =
  let string_of_arm_mnemonic0 = fun mn ->
    (^) (string_of_arm_mnemonic mn)
      ((^) (if opts.set_flags then "S" else "")
        (if opts.is_conditional then "cc" else ""))
  in
  (fun hwn hwm ->
  let mn = SMUL_hw (hwn, hwm) in
  let tin = (lreg arm_decl) :: ((lreg arm_decl) :: []) in
  let semi = arm_smul_hw_semi hwn hwm in
  { id_valid = true; id_msb_flag = MSB_MERGE; id_tin = tin; id_in =
  ((coq_Ea arm_decl (S O)) :: ((coq_Ea arm_decl (S (S O))) :: [])); id_tout =
  ((lreg arm_decl) :: []); id_out = ((coq_Ea arm_decl O) :: []); id_semi =
  (sem_prod_ok (map eval_ltype tin) (Obj.magic semi)); id_args_kinds =
  ak_reg_reg_reg; id_nargs = (S (S (S O))); id_str_jas =
  (pp_s (string_of_arm_mnemonic0 mn)); id_safe = []; id_pp_asm =
  (pp_arm_op mn opts) })

(** val arm_smla_hw_semi :
    halfword -> halfword -> (register, empty, empty, rflag, condt) wreg ->
    (register, empty, empty, rflag, condt) wreg -> (register, empty, empty,
    rflag, condt) wreg -> (register, empty, empty, rflag, condt) wreg **)

let arm_smla_hw_semi hwn hwm wn wm acc =
  let n = get_hw hwn wn in
  let m = get_hw hwm wm in
  let r =
    Z.add (Z.mul (wsigned U16 n) (wsigned U16 m))
      (wsigned arm_decl.reg_size acc)
  in
  wrepr U32 r

(** val arm_smla_hw_instr :
    arm_options -> halfword -> halfword -> (register, empty, empty, rflag,
    condt) instr_desc_t **)

let arm_smla_hw_instr opts =
  let string_of_arm_mnemonic0 = fun mn ->
    (^) (string_of_arm_mnemonic mn)
      ((^) (if opts.set_flags then "S" else "")
        (if opts.is_conditional then "cc" else ""))
  in
  (fun hwn hwm ->
  let mn = SMLA_hw (hwn, hwm) in
  let tin = (lreg arm_decl) :: ((lreg arm_decl) :: ((lreg arm_decl) :: [])) in
  let semi = arm_smla_hw_semi hwn hwm in
  { id_valid = true; id_msb_flag = MSB_MERGE; id_tin = tin; id_in =
  ((coq_Ea arm_decl (S O)) :: ((coq_Ea arm_decl (S (S O))) :: ((coq_Ea
                                                                 arm_decl (S
                                                                 (S (S O)))) :: [])));
  id_tout = ((lreg arm_decl) :: []); id_out = ((coq_Ea arm_decl O) :: []);
  id_semi = (sem_prod_ok (map eval_ltype tin) (Obj.magic semi));
  id_args_kinds = ak_reg_reg_reg_reg; id_nargs = (S (S (S (S O))));
  id_str_jas = (pp_s (string_of_arm_mnemonic0 mn)); id_safe = []; id_pp_asm =
  (pp_arm_op mn opts) })

(** val arm_smulw_hw_semi :
    halfword -> (register, empty, empty, rflag, condt) wreg -> (register,
    empty, empty, rflag, condt) wreg -> (register, empty, empty, rflag,
    condt) wreg **)

let arm_smulw_hw_semi hw wn wm =
  let m = get_hw hw wm in
  let res = Z.mul (wsigned arm_decl.reg_size wn) (wsigned U16 m) in
  let w = wrepr U64 res in
  winit U32 (fun i ->
    wbit_n U64 w
      (addn i (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S
        O))))))))))))))))))

(** val arm_smulw_hw_instr :
    arm_options -> halfword -> (register, empty, empty, rflag, condt)
    instr_desc_t **)

let arm_smulw_hw_instr opts =
  let string_of_arm_mnemonic0 = fun mn ->
    (^) (string_of_arm_mnemonic mn)
      ((^) (if opts.set_flags then "S" else "")
        (if opts.is_conditional then "cc" else ""))
  in
  (fun hw ->
  let mn = SMULW_hw hw in
  let tin = (lreg arm_decl) :: ((lreg arm_decl) :: []) in
  let semi = arm_smulw_hw_semi hw in
  { id_valid = true; id_msb_flag = MSB_MERGE; id_tin = tin; id_in =
  ((coq_Ea arm_decl (S O)) :: ((coq_Ea arm_decl (S (S O))) :: [])); id_tout =
  ((lreg arm_decl) :: []); id_out = ((coq_Ea arm_decl O) :: []); id_semi =
  (sem_prod_ok (map eval_ltype tin) (Obj.magic semi)); id_args_kinds =
  ak_reg_reg_reg; id_nargs = (S (S (S O))); id_str_jas =
  (pp_s (string_of_arm_mnemonic0 mn)); id_safe = []; id_pp_asm =
  (pp_arm_op mn opts) })

(** val arm_bitwise_semi :
    wsize -> (GRing.ComRing.sort -> GRing.ComRing.sort) ->
    (GRing.ComRing.sort -> GRing.ComRing.sort) -> (GRing.ComRing.sort ->
    GRing.ComRing.sort -> GRing.ComRing.sort) -> sem_tuple -> sem_tuple ->
    sem_tuple **)

let arm_bitwise_semi ws op0 op1 op wn wm =
  let res = op (op0 wn) (op1 wm) in
  Obj.magic ((Some (coq_NF_of_word ws res)), ((Some (coq_ZF_of_word ws res)),
    (None, res)))

(** val arm_AND_instr :
    arm_options -> (register, empty, empty, rflag, condt) instr_desc_t **)

let arm_AND_instr opts =
  let string_of_arm_mnemonic0 = fun mn ->
    (^) (string_of_arm_mnemonic mn)
      ((^) (if opts.set_flags then "S" else "")
        (if opts.is_conditional then "cc" else ""))
  in
  let mn = AND in
  let tin = (lreg arm_decl) :: ((lreg arm_decl) :: []) in
  let semi =
    arm_bitwise_semi arm_decl.reg_size (fun x -> x) (fun x -> x)
      (wand arm_decl.reg_size)
  in
  let x = { id_valid = true; id_msb_flag = MSB_MERGE; id_tin =
    ((lreg arm_decl) :: ((lreg arm_decl) :: [])); id_in =
    ((coq_Ea arm_decl (S O)) :: ((coq_Ea arm_decl (S (S O))) :: []));
    id_tout =
    (cat (Coq_lbool :: (Coq_lbool :: (Coq_lbool :: [])))
      ((lreg arm_decl) :: [])); id_out =
    (cat ad_nzc ((coq_Ea arm_decl O) :: [])); id_semi =
    (sem_prod_ok (map eval_ltype tin) (Obj.magic semi)); id_args_kinds =
    (ak_reg_reg_reg_or_imm opts chk_imm_reject_shift); id_nargs = (S (S (S
    O))); id_str_jas = (pp_s (string_of_arm_mnemonic0 mn)); id_safe = [];
    id_pp_asm = (pp_arm_op mn opts) }
  in
  let x0 =
    match opts.has_shift with
    | Some sk ->
      mk_shifted sk x (mk_semi2_2_shifted (lreg arm_decl) sk x.id_semi)
    | None -> x
  in
  if opts.set_flags then x0 else drop_nzc x0

(** val arm_BFC_semi :
    (register, empty, empty, rflag, condt) wreg -> GRing.ComRing.sort ->
    GRing.ComRing.sort -> (register, empty, empty, rflag, condt) wreg exec **)

let arm_BFC_semi x lsb width =
  let lsbit = wunsigned U8 lsb in
  let nbits = wunsigned U8 width in
  if Z.ltb lsbit (Zpos (Coq_xO (Coq_xO (Coq_xO (Coq_xO (Coq_xO Coq_xH))))))
  then if Z.leb (Zpos Coq_xH) nbits
       then if Z.leb nbits
                 (Z.sub (Zpos (Coq_xO (Coq_xO (Coq_xO (Coq_xO (Coq_xO
                   Coq_xH)))))) lsbit)
            then let msbit = Z.sub (Z.add lsbit nbits) (Zpos Coq_xH) in
                 let mk = fun i ->
                   if (&&) (Nat.leb (Z.to_nat lsbit) i)
                        (Nat.leb i (Z.to_nat msbit))
                   then false
                   else wbit_n arm_decl.reg_size x i
                 in
                 Ok (winit arm_decl.reg_size mk)
            else Error E.no_semantics
       else Error E.no_semantics
  else Error E.no_semantics

(** val arm_BFC_semi_sc : safe_cond list **)

let arm_BFC_semi_sc =
  (ULt (U8, (S O), (Zpos (Coq_xO (Coq_xO (Coq_xO (Coq_xO (Coq_xO
    Coq_xH)))))))) :: ((UGe (U8, (Zpos Coq_xH), (S (S O)))) :: ((UaddLe (U8,
    (S (S O)), (S O), (Zpos (Coq_xO (Coq_xO (Coq_xO (Coq_xO (Coq_xO
    Coq_xH)))))))) :: []))

(** val arm_BFC_instr :
    arm_options -> (register, empty, empty, rflag, condt) instr_desc_t **)

let arm_BFC_instr opts =
  let string_of_arm_mnemonic0 = fun mn ->
    (^) (string_of_arm_mnemonic mn)
      ((^) (if opts.set_flags then "S" else "")
        (if opts.is_conditional then "cc" else ""))
  in
  let mn = BFC in
  { id_valid = true; id_msb_flag = MSB_MERGE; id_tin =
  ((lreg arm_decl) :: ((Coq_lword U8) :: ((Coq_lword U8) :: []))); id_in =
  ((coq_Ea arm_decl O) :: ((coq_Ea arm_decl (S O)) :: ((coq_Ea arm_decl (S (S
                                                         O))) :: [])));
  id_tout = ((lreg arm_decl) :: []); id_out = ((coq_Ea arm_decl O) :: []);
  id_semi = (Obj.magic arm_BFC_semi); id_args_kinds = ak_reg_imm8_imm8;
  id_nargs = (S (S (S O))); id_str_jas = (pp_s (string_of_arm_mnemonic0 mn));
  id_safe = arm_BFC_semi_sc; id_pp_asm = (pp_arm_op mn opts) }

(** val arm_BFI_semi :
    (register, empty, empty, rflag, condt) wreg -> (register, empty, empty,
    rflag, condt) wreg -> GRing.ComRing.sort -> GRing.ComRing.sort ->
    (register, empty, empty, rflag, condt) wreg exec **)

let arm_BFI_semi x y lsb width =
  let lsbit = wunsigned U8 lsb in
  let nbits = wunsigned U8 width in
  if Z.ltb lsbit (Zpos (Coq_xO (Coq_xO (Coq_xO (Coq_xO (Coq_xO Coq_xH))))))
  then if Z.leb (Zpos Coq_xH) nbits
       then if Z.leb nbits
                 (Z.sub (Zpos (Coq_xO (Coq_xO (Coq_xO (Coq_xO (Coq_xO
                   Coq_xH)))))) lsbit)
            then let msbit = Z.sub (Z.add lsbit nbits) (Zpos Coq_xH) in
                 let mk = fun i ->
                   if (&&) (Nat.leb (Z.to_nat lsbit) i)
                        (Nat.leb i (Z.to_nat msbit))
                   then wbit_n arm_decl.reg_size y (subn i (Z.to_nat lsbit))
                   else wbit_n arm_decl.reg_size x i
                 in
                 Ok (winit arm_decl.reg_size mk)
            else Error E.no_semantics
       else Error E.no_semantics
  else Error E.no_semantics

(** val arm_BFI_semi_sc : safe_cond list **)

let arm_BFI_semi_sc =
  (ULt (U8, (S (S O)), (Zpos (Coq_xO (Coq_xO (Coq_xO (Coq_xO (Coq_xO
    Coq_xH)))))))) :: ((UGe (U8, (Zpos Coq_xH), (S (S (S O))))) :: ((UaddLe
    (U8, (S (S (S O))), (S (S O)), (Zpos (Coq_xO (Coq_xO (Coq_xO (Coq_xO
    (Coq_xO Coq_xH)))))))) :: []))

(** val arm_BFI_instr :
    arm_options -> (register, empty, empty, rflag, condt) instr_desc_t **)

let arm_BFI_instr opts =
  let string_of_arm_mnemonic0 = fun mn ->
    (^) (string_of_arm_mnemonic mn)
      ((^) (if opts.set_flags then "S" else "")
        (if opts.is_conditional then "cc" else ""))
  in
  let mn = BFI in
  { id_valid = true; id_msb_flag = MSB_MERGE; id_tin =
  ((lreg arm_decl) :: ((lreg arm_decl) :: ((Coq_lword U8) :: ((Coq_lword
  U8) :: [])))); id_in =
  ((coq_Ea arm_decl O) :: ((coq_Ea arm_decl (S O)) :: ((coq_Ea arm_decl (S (S
                                                         O))) :: ((coq_Ea
                                                                    arm_decl
                                                                    (S (S (S
                                                                    O)))) :: []))));
  id_tout = ((lreg arm_decl) :: []); id_out = ((coq_Ea arm_decl O) :: []);
  id_semi = (Obj.magic arm_BFI_semi); id_args_kinds = ak_reg_reg_imm8_imm8;
  id_nargs = (S (S (S (S O)))); id_str_jas =
  (pp_s (string_of_arm_mnemonic0 mn)); id_safe = arm_BFI_semi_sc; id_pp_asm =
  (pp_arm_op mn opts) }

(** val arm_BIC_instr :
    arm_options -> (register, empty, empty, rflag, condt) instr_desc_t **)

let arm_BIC_instr opts =
  let string_of_arm_mnemonic0 = fun mn ->
    (^) (string_of_arm_mnemonic mn)
      ((^) (if opts.set_flags then "S" else "")
        (if opts.is_conditional then "cc" else ""))
  in
  let mn = BIC in
  let tin = (lreg arm_decl) :: ((lreg arm_decl) :: []) in
  let semi =
    arm_bitwise_semi arm_decl.reg_size (fun x -> x) (wnot arm_decl.reg_size)
      (wand arm_decl.reg_size)
  in
  let x = { id_valid = true; id_msb_flag = MSB_MERGE; id_tin = tin; id_in =
    ((coq_Ea arm_decl (S O)) :: ((coq_Ea arm_decl (S (S O))) :: []));
    id_tout =
    (cat (Coq_lbool :: (Coq_lbool :: (Coq_lbool :: [])))
      ((lreg arm_decl) :: [])); id_out =
    (cat ad_nzc ((coq_Ea arm_decl O) :: [])); id_semi =
    (sem_prod_ok (map eval_ltype tin) (Obj.magic semi)); id_args_kinds =
    (ak_reg_reg_reg_or_imm opts chk_imm_reject_shift); id_nargs = (S (S (S
    O))); id_str_jas = (pp_s (string_of_arm_mnemonic0 mn)); id_safe = [];
    id_pp_asm = (pp_arm_op mn opts) }
  in
  let x0 =
    match opts.has_shift with
    | Some sk ->
      mk_shifted sk x (mk_semi2_2_shifted (lreg arm_decl) sk x.id_semi)
    | None -> x
  in
  if opts.set_flags then x0 else drop_nzc x0

(** val arm_EOR_instr :
    arm_options -> (register, empty, empty, rflag, condt) instr_desc_t **)

let arm_EOR_instr opts =
  let string_of_arm_mnemonic0 = fun mn ->
    (^) (string_of_arm_mnemonic mn)
      ((^) (if opts.set_flags then "S" else "")
        (if opts.is_conditional then "cc" else ""))
  in
  let mn = EOR in
  let tin = (lreg arm_decl) :: ((lreg arm_decl) :: []) in
  let semi =
    arm_bitwise_semi arm_decl.reg_size (fun x -> x) (fun x -> x)
      (wxor arm_decl.reg_size)
  in
  let x = { id_valid = true; id_msb_flag = MSB_MERGE; id_tin = tin; id_in =
    ((coq_Ea arm_decl (S O)) :: ((coq_Ea arm_decl (S (S O))) :: []));
    id_tout =
    (cat (Coq_lbool :: (Coq_lbool :: (Coq_lbool :: [])))
      ((lreg arm_decl) :: [])); id_out =
    (cat ad_nzc ((coq_Ea arm_decl O) :: [])); id_semi =
    (sem_prod_ok (map eval_ltype tin) (Obj.magic semi)); id_args_kinds =
    (ak_reg_reg_reg_or_imm opts chk_imm_reject_shift); id_nargs = (S (S (S
    O))); id_str_jas = (pp_s (string_of_arm_mnemonic0 mn)); id_safe = [];
    id_pp_asm = (pp_arm_op mn opts) }
  in
  let x0 =
    match opts.has_shift with
    | Some sk ->
      mk_shifted sk x (mk_semi2_2_shifted (lreg arm_decl) sk x.id_semi)
    | None -> x
  in
  if opts.set_flags then x0 else drop_nzc x0

(** val arm_MVN_semi : sem_tuple -> sem_tuple **)

let arm_MVN_semi wn =
  let res = wnot arm_decl.reg_size wn in
  Obj.magic ((Some (coq_NF_of_word arm_decl.reg_size res)), ((Some
    (coq_ZF_of_word arm_decl.reg_size res)), (None, res)))

(** val arm_MVN_instr :
    arm_options -> (register, empty, empty, rflag, condt) instr_desc_t **)

let arm_MVN_instr opts =
  let string_of_arm_mnemonic0 = fun mn ->
    (^) (string_of_arm_mnemonic mn)
      ((^) (if opts.set_flags then "S" else "")
        (if opts.is_conditional then "cc" else ""))
  in
  let mn = MVN in
  let tin = (lreg arm_decl) :: [] in
  let x = { id_valid = true; id_msb_flag = MSB_MERGE; id_tin = tin; id_in =
    ((coq_Ea arm_decl (S O)) :: []); id_tout =
    (cat (Coq_lbool :: (Coq_lbool :: (Coq_lbool :: [])))
      ((lreg arm_decl) :: [])); id_out =
    (cat ad_nzc ((coq_Ea arm_decl O) :: [])); id_semi =
    (sem_prod_ok (map eval_ltype tin) (Obj.magic arm_MVN_semi));
    id_args_kinds = (ak_reg_reg_or_imm opts chk_imm_reject_shift); id_nargs =
    (S (S O)); id_str_jas = (pp_s (string_of_arm_mnemonic0 mn)); id_safe =
    []; id_pp_asm = (pp_arm_op mn opts) }
  in
  let x0 =
    match opts.has_shift with
    | Some sk -> mk_shifted sk x (mk_semi1_shifted sk x.id_semi)
    | None -> x
  in
  if opts.set_flags then x0 else drop_nzc x0

(** val arm_ORR_instr :
    arm_options -> (register, empty, empty, rflag, condt) instr_desc_t **)

let arm_ORR_instr opts =
  let string_of_arm_mnemonic0 = fun mn ->
    (^) (string_of_arm_mnemonic mn)
      ((^) (if opts.set_flags then "S" else "")
        (if opts.is_conditional then "cc" else ""))
  in
  let mn = ORR in
  let tin = (lreg arm_decl) :: ((lreg arm_decl) :: []) in
  let semi =
    arm_bitwise_semi arm_decl.reg_size (fun x -> x) (fun x -> x)
      (wor arm_decl.reg_size)
  in
  let x = { id_valid = true; id_msb_flag = MSB_MERGE; id_tin = tin; id_in =
    ((coq_Ea arm_decl (S O)) :: ((coq_Ea arm_decl (S (S O))) :: []));
    id_tout =
    (cat (Coq_lbool :: (Coq_lbool :: (Coq_lbool :: [])))
      ((lreg arm_decl) :: [])); id_out =
    (cat ad_nzc ((coq_Ea arm_decl O) :: [])); id_semi =
    (sem_prod_ok (map eval_ltype tin) (Obj.magic semi)); id_args_kinds =
    (ak_reg_reg_reg_or_imm opts chk_imm_reject_shift); id_nargs = (S (S (S
    O))); id_str_jas = (pp_s (string_of_arm_mnemonic0 mn)); id_safe = [];
    id_pp_asm = (pp_arm_op mn opts) }
  in
  let x0 =
    match opts.has_shift with
    | Some sk ->
      mk_shifted sk x (mk_semi2_2_shifted (lreg arm_decl) sk x.id_semi)
    | None -> x
  in
  if opts.set_flags then x0 else drop_nzc x0

(** val arm_shift_semi :
    (sem_tuple -> coq_Z -> sem_tuple) -> (sem_tuple -> coq_Z -> bool) ->
    sem_tuple -> GRing.ComRing.sort -> sem_tuple **)

let arm_shift_semi op op_c wn wsham =
  let sham = wunsigned U8 wsham in
  let res = op wn sham in
  if eq_op coq_BinNums_Z__canonical__eqtype_Equality (Obj.magic sham)
       (Obj.magic Z0)
  then Obj.magic (None, (None, (None, res)))
  else Obj.magic ((Some (coq_NF_of_word arm_decl.reg_size res)), ((Some
         (coq_ZF_of_word arm_decl.reg_size res)), ((Some (op_c wn sham)),
         res)))

(** val arm_ASR_C : sem_tuple -> coq_Z -> bool **)

let arm_ASR_C wn shift =
  if Z.leb (Zpos (Coq_xO (Coq_xO (Coq_xO (Coq_xO (Coq_xO Coq_xH)))))) shift
  then msb arm_decl.reg_size wn
  else wbit_n arm_decl.reg_size wn (Z.to_nat (Z.sub shift (Zpos Coq_xH)))

(** val arm_ASR_semi : sem_tuple -> GRing.ComRing.sort -> sem_tuple **)

let arm_ASR_semi wn wsham =
  arm_shift_semi (wsar arm_decl.reg_size) arm_ASR_C wn wsham

(** val arm_ASR_instr :
    arm_options -> (register, empty, empty, rflag, condt) instr_desc_t **)

let arm_ASR_instr opts =
  let string_of_arm_mnemonic0 = fun mn ->
    (^) (string_of_arm_mnemonic mn)
      ((^) (if opts.set_flags then "S" else "")
        (if opts.is_conditional then "cc" else ""))
  in
  let mn = ASR in
  let tin = (lreg arm_decl) :: ((Coq_lword U8) :: []) in
  let x = { id_valid = true; id_msb_flag = MSB_MERGE; id_tin = tin; id_in =
    ((coq_Ea arm_decl (S O)) :: ((coq_Ea arm_decl (S (S O))) :: []));
    id_tout =
    (cat (Coq_lbool :: (Coq_lbool :: (Coq_lbool :: [])))
      ((lreg arm_decl) :: [])); id_out =
    (cat ad_nzc ((coq_Ea arm_decl O) :: [])); id_semi =
    (sem_prod_ok (map eval_ltype tin) (Obj.magic arm_ASR_semi));
    id_args_kinds = (cat ak_reg_reg_reg (ak_reg_reg_imm_shift U8 SASR));
    id_nargs = (S (S (S O))); id_str_jas =
    (pp_s (string_of_arm_mnemonic0 mn)); id_safe = []; id_pp_asm =
    (pp_arm_op mn opts) }
  in
  if opts.set_flags then x else drop_nzc x

(** val arm_LSL_C : sem_tuple -> coq_Z -> bool **)

let arm_LSL_C wn shift =
  if Z.leb shift (Zpos (Coq_xO (Coq_xO (Coq_xO (Coq_xO (Coq_xO Coq_xH))))))
  then wbit_n arm_decl.reg_size wn
         (subn (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S
           (S (S (S (S (S (S (S (S (S (S (S O))))))))))))))))))))))))))))))))
           (Z.to_nat shift))
  else false

(** val arm_LSL_semi : sem_tuple -> GRing.ComRing.sort -> sem_tuple **)

let arm_LSL_semi wn wsham =
  arm_shift_semi (wshl arm_decl.reg_size) arm_LSL_C wn wsham

(** val arm_LSL_instr :
    arm_options -> (register, empty, empty, rflag, condt) instr_desc_t **)

let arm_LSL_instr opts =
  let string_of_arm_mnemonic0 = fun mn ->
    (^) (string_of_arm_mnemonic mn)
      ((^) (if opts.set_flags then "S" else "")
        (if opts.is_conditional then "cc" else ""))
  in
  let mn = LSL in
  let tin = (lreg arm_decl) :: ((Coq_lword U8) :: []) in
  let x = { id_valid = true; id_msb_flag = MSB_MERGE; id_tin = tin; id_in =
    ((coq_Ea arm_decl (S O)) :: ((coq_Ea arm_decl (S (S O))) :: []));
    id_tout =
    (cat (Coq_lbool :: (Coq_lbool :: (Coq_lbool :: [])))
      ((lreg arm_decl) :: [])); id_out =
    (cat ad_nzc ((coq_Ea arm_decl O) :: [])); id_semi =
    (sem_prod_ok (map eval_ltype tin) (Obj.magic arm_LSL_semi));
    id_args_kinds = (cat ak_reg_reg_reg (ak_reg_reg_imm_shift U8 SLSL));
    id_nargs = (S (S (S O))); id_str_jas =
    (pp_s (string_of_arm_mnemonic0 mn)); id_safe = []; id_pp_asm =
    (pp_arm_op mn opts) }
  in
  if opts.set_flags then x else drop_nzc x

(** val arm_LSR_C : sem_tuple -> coq_Z -> bool **)

let arm_LSR_C wn shift =
  if Z.ltb (Zpos (Coq_xO (Coq_xO (Coq_xO (Coq_xO (Coq_xO Coq_xH)))))) shift
  then false
  else wbit_n arm_decl.reg_size wn (Z.to_nat (Z.sub shift (Zpos Coq_xH)))

(** val arm_LSR_semi : sem_tuple -> GRing.ComRing.sort -> sem_tuple **)

let arm_LSR_semi wn wsham =
  arm_shift_semi (wshr arm_decl.reg_size) arm_LSR_C wn wsham

(** val arm_LSR_instr :
    arm_options -> (register, empty, empty, rflag, condt) instr_desc_t **)

let arm_LSR_instr opts =
  let string_of_arm_mnemonic0 = fun mn ->
    (^) (string_of_arm_mnemonic mn)
      ((^) (if opts.set_flags then "S" else "")
        (if opts.is_conditional then "cc" else ""))
  in
  let mn = LSR in
  let tin = (lreg arm_decl) :: ((Coq_lword U8) :: []) in
  let x = { id_valid = true; id_msb_flag = MSB_MERGE; id_tin = tin; id_in =
    ((coq_Ea arm_decl (S O)) :: ((coq_Ea arm_decl (S (S O))) :: []));
    id_tout =
    (cat (Coq_lbool :: (Coq_lbool :: (Coq_lbool :: [])))
      ((lreg arm_decl) :: [])); id_out =
    (cat ad_nzc ((coq_Ea arm_decl O) :: [])); id_semi =
    (sem_prod_ok (map eval_ltype tin) (Obj.magic arm_LSR_semi));
    id_args_kinds = (cat ak_reg_reg_reg (ak_reg_reg_imm_shift U8 SLSR));
    id_nargs = (S (S (S O))); id_str_jas =
    (pp_s (string_of_arm_mnemonic0 mn)); id_safe = []; id_pp_asm =
    (pp_arm_op mn opts) }
  in
  if opts.set_flags then x else drop_nzc x

(** val arm_ROR_C : sem_tuple -> coq_Z -> bool **)

let arm_ROR_C wn shift =
  let res = wror arm_decl.reg_size wn shift in msb arm_decl.reg_size res

(** val arm_ROR_semi : sem_tuple -> GRing.ComRing.sort -> sem_tuple **)

let arm_ROR_semi wn wsham =
  arm_shift_semi (wror arm_decl.reg_size) arm_ROR_C wn wsham

(** val arm_ROR_instr :
    arm_options -> (register, empty, empty, rflag, condt) instr_desc_t **)

let arm_ROR_instr opts =
  let string_of_arm_mnemonic0 = fun mn ->
    (^) (string_of_arm_mnemonic mn)
      ((^) (if opts.set_flags then "S" else "")
        (if opts.is_conditional then "cc" else ""))
  in
  let mn = ROR in
  let tin = (lreg arm_decl) :: ((Coq_lword U8) :: []) in
  let x = { id_valid = true; id_msb_flag = MSB_MERGE; id_tin = tin; id_in =
    ((coq_Ea arm_decl (S O)) :: ((coq_Ea arm_decl (S (S O))) :: []));
    id_tout =
    (cat (Coq_lbool :: (Coq_lbool :: (Coq_lbool :: [])))
      ((lreg arm_decl) :: [])); id_out =
    (cat ad_nzc ((coq_Ea arm_decl O) :: [])); id_semi =
    (sem_prod_ok (map eval_ltype tin) (Obj.magic arm_ROR_semi));
    id_args_kinds = (cat ak_reg_reg_reg (ak_reg_reg_imm_shift U8 SROR));
    id_nargs = (S (S (S O))); id_str_jas =
    (pp_s (string_of_arm_mnemonic0 mn)); id_safe = []; id_pp_asm =
    (pp_arm_op mn opts) }
  in
  if opts.set_flags then x else drop_nzc x

(** val mk_rev_instr :
    arm_options -> arm_mnemonic -> sem_tuple sem_prod -> (register, empty,
    empty, rflag, condt) instr_desc_t **)

let mk_rev_instr opts =
  let string_of_arm_mnemonic0 = fun mn ->
    (^) (string_of_arm_mnemonic mn)
      ((^) (if opts.set_flags then "S" else "")
        (if opts.is_conditional then "cc" else ""))
  in
  (fun mn semi ->
  let tin = (lreg arm_decl) :: [] in
  { id_valid = true; id_msb_flag = MSB_MERGE; id_tin = tin; id_in =
  ((coq_Ea arm_decl (S O)) :: []); id_tout = ((lreg arm_decl) :: []);
  id_out = ((coq_Ea arm_decl O) :: []); id_semi =
  (sem_prod_ok (map eval_ltype tin) semi); id_args_kinds = ak_reg_reg;
  id_nargs = (S (S O)); id_str_jas = (pp_s (string_of_arm_mnemonic0 mn));
  id_safe = []; id_pp_asm = (pp_arm_op mn opts) })

(** val arm_REV_semi : sem_tuple -> sem_tuple **)

let arm_REV_semi w =
  wbswap arm_decl.reg_size w

(** val arm_REV16_semi : sem_tuple -> sem_tuple **)

let arm_REV16_semi w =
  lift1_vec U16 (wbswap U16) U32 w

(** val arm_REVSH_semi : sem_tuple -> sem_tuple **)

let arm_REVSH_semi w =
  sign_extend U32 U16 (wbswap U16 (zero_extend U16 arm_decl.reg_size w))

(** val arm_REV_instr :
    arm_options -> (register, empty, empty, rflag, condt) instr_desc_t **)

let arm_REV_instr opts =
  mk_rev_instr opts REV (Obj.magic arm_REV_semi)

(** val arm_REV16_instr :
    arm_options -> (register, empty, empty, rflag, condt) instr_desc_t **)

let arm_REV16_instr opts =
  mk_rev_instr opts REV16 (Obj.magic arm_REV16_semi)

(** val arm_REVSH_instr :
    arm_options -> (register, empty, empty, rflag, condt) instr_desc_t **)

let arm_REVSH_instr opts =
  mk_rev_instr opts REVSH (Obj.magic arm_REVSH_semi)

(** val arm_ADR_semi : sem_tuple -> sem_tuple **)

let arm_ADR_semi wn =
  wn

(** val arm_ADR_instr :
    arm_options -> (register, empty, empty, rflag, condt) instr_desc_t **)

let arm_ADR_instr opts =
  let string_of_arm_mnemonic0 = fun mn ->
    (^) (string_of_arm_mnemonic mn)
      ((^) (if opts.set_flags then "S" else "")
        (if opts.is_conditional then "cc" else ""))
  in
  let mn = ADR in
  let tin = (lreg arm_decl) :: [] in
  { id_valid = true; id_msb_flag = MSB_MERGE; id_tin = tin; id_in =
  ((coq_Ec arm_decl (S O)) :: []); id_tout = ((lreg arm_decl) :: []);
  id_out = ((coq_Ea arm_decl O) :: []); id_semi =
  (sem_prod_ok (map eval_ltype tin) (Obj.magic arm_ADR_semi));
  id_args_kinds = ak_reg_addr; id_nargs = (S (S O)); id_str_jas =
  (pp_s (string_of_arm_mnemonic0 mn)); id_safe = []; id_pp_asm =
  (pp_arm_op mn opts) }

(** val arm_MOV_semi : sem_tuple -> sem_tuple **)

let arm_MOV_semi wn =
  Obj.magic ((Some (coq_NF_of_word arm_decl.reg_size wn)), ((Some
    (coq_ZF_of_word arm_decl.reg_size wn)), (None, wn)))

(** val arm_MOV_instr :
    arm_options -> (register, empty, empty, rflag, condt) instr_desc_t **)

let arm_MOV_instr opts =
  let string_of_arm_mnemonic0 = fun mn ->
    (^) (string_of_arm_mnemonic mn)
      ((^) (if opts.set_flags then "S" else "")
        (if opts.is_conditional then "cc" else ""))
  in
  let mn = MOV in
  let tin = (lreg arm_decl) :: [] in
  let x = { id_valid = true; id_msb_flag = MSB_MERGE; id_tin = tin; id_in =
    ((coq_Ea arm_decl (S O)) :: []); id_tout =
    (cat (Coq_lbool :: (Coq_lbool :: (Coq_lbool :: [])))
      ((lreg arm_decl) :: [])); id_out =
    (cat ad_nzc ((coq_Ea arm_decl O) :: [])); id_semi =
    (sem_prod_ok (map eval_ltype tin) (Obj.magic arm_MOV_semi));
    id_args_kinds =
    (cat ak_reg_reg (ak_reg_imm_ (chk_imm_w16_encoding opts.set_flags)));
    id_nargs = (S (S O)); id_str_jas = (pp_s (string_of_arm_mnemonic0 mn));
    id_safe = []; id_pp_asm = (pp_arm_op mn opts) }
  in
  if opts.set_flags then x else drop_nzc x

(** val arm_MOVT_semi : sem_tuple -> GRing.ComRing.sort -> sem_tuple **)

let arm_MOVT_semi wn wm =
  let hi =
    wshl arm_decl.reg_size (zero_extend arm_decl.reg_size U16 wm) (Zpos
      (Coq_xO (Coq_xO (Coq_xO (Coq_xO Coq_xH)))))
  in
  let mask = zero_extend arm_decl.reg_size U16 (wrepr U16 (Zneg Coq_xH)) in
  wor arm_decl.reg_size hi (wand arm_decl.reg_size wn mask)

(** val arm_MOVT_instr :
    arm_options -> (register, empty, empty, rflag, condt) instr_desc_t **)

let arm_MOVT_instr opts =
  let string_of_arm_mnemonic0 = fun mn ->
    (^) (string_of_arm_mnemonic mn)
      ((^) (if opts.set_flags then "S" else "")
        (if opts.is_conditional then "cc" else ""))
  in
  let mn = MOVT in
  let tin = (lreg arm_decl) :: ((Coq_lword U16) :: []) in
  { id_valid = true; id_msb_flag = MSB_MERGE; id_tin = tin; id_in =
  ((coq_Ea arm_decl O) :: ((coq_Ea arm_decl (S O)) :: [])); id_tout =
  ((lreg arm_decl) :: []); id_out = ((coq_Ea arm_decl O) :: []); id_semi =
  (sem_prod_ok (map eval_ltype tin) (Obj.magic arm_MOVT_semi));
  id_args_kinds =
  (((CAreg :: []) :: (((coq_CAimm_sz U16) :: []) :: [])) :: []); id_nargs =
  (S (S O)); id_str_jas = (pp_s (string_of_arm_mnemonic0 mn)); id_safe = [];
  id_pp_asm = (pp_arm_op mn opts) }

(** val bit_field_extract_semi :
    ((register, empty, empty, rflag, condt) wreg -> coq_Z -> (register,
    empty, empty, rflag, condt) wreg) -> (register, empty, empty, rflag,
    condt) wreg -> GRing.ComRing.sort -> GRing.ComRing.sort -> (register,
    empty, empty, rflag, condt) wreg exec **)

let bit_field_extract_semi shr wn widx wwidth =
  let idx = wunsigned U8 widx in
  let width = wunsigned U8 wwidth in
  if (&&) (Z.leb (Zpos Coq_xH) width)
       (Z.ltb width
         (Z.sub (Zpos (Coq_xI (Coq_xO (Coq_xO (Coq_xO (Coq_xO Coq_xH))))))
           idx))
  then Ok
         (shr
           (wshl arm_decl.reg_size wn
             (Z.sub
               (Z.sub (Zpos (Coq_xO (Coq_xO (Coq_xO (Coq_xO (Coq_xO
                 Coq_xH)))))) width) idx))
           (Z.sub (Zpos (Coq_xO (Coq_xO (Coq_xO (Coq_xO (Coq_xO Coq_xH))))))
             width))
  else Error E.no_semantics

(** val bit_field_extract_semi_sc : safe_cond list **)

let bit_field_extract_semi_sc =
  (UGe (U8, (Zpos Coq_xH), (S (S O)))) :: ((UaddLe (U8, (S (S O)), (S O),
    (Zpos (Coq_xO (Coq_xO (Coq_xO (Coq_xO (Coq_xO Coq_xH)))))))) :: [])

(** val ak_reg_reg_imm_imm_extr : arg_kind list list list **)

let ak_reg_reg_imm_imm_extr =
  ((CAreg :: []) :: ((CAreg :: []) :: (((CAimm ((CAimmC_arm_shift_amout
    SLSL), U8)) :: []) :: (((coq_CAimm_sz U8) :: []) :: [])))) :: []

(** val arm_UBFX_instr :
    arm_options -> (register, empty, empty, rflag, condt) instr_desc_t **)

let arm_UBFX_instr opts =
  let string_of_arm_mnemonic0 = fun mn ->
    (^) (string_of_arm_mnemonic mn)
      ((^) (if opts.set_flags then "S" else "")
        (if opts.is_conditional then "cc" else ""))
  in
  let mn = UBFX in
  let sh = wshr arm_decl.reg_size in
  { id_valid = true; id_msb_flag = MSB_MERGE; id_tin =
  ((lreg arm_decl) :: ((Coq_lword U8) :: ((Coq_lword U8) :: []))); id_in =
  ((coq_Ea arm_decl (S O)) :: ((coq_Ea arm_decl (S (S O))) :: ((coq_Ea
                                                                 arm_decl (S
                                                                 (S (S O)))) :: [])));
  id_tout = ((lreg arm_decl) :: []); id_out = ((coq_Ea arm_decl O) :: []);
  id_semi = (Obj.magic bit_field_extract_semi sh); id_args_kinds =
  ak_reg_reg_imm_imm_extr; id_nargs = (S (S (S (S O)))); id_str_jas =
  (pp_s (string_of_arm_mnemonic0 mn)); id_safe = bit_field_extract_semi_sc;
  id_pp_asm = (pp_arm_op mn opts) }

(** val extend_bits_semi :
    coq_Z -> (register, empty, empty, rflag, condt) wreg ->
    GRing.ComRing.sort -> (register, empty, empty, rflag, condt) wreg **)

let extend_bits_semi len wn wroram =
  let mask =
    wrepr arm_decl.reg_size
      (Z.sub (Z.pow (Zpos (Coq_xO Coq_xH)) len) (Zpos Coq_xH))
  in
  let roram = wunsigned U8 wroram in
  wand arm_decl.reg_size mask (wror arm_decl.reg_size wn roram)

(** val ak_reg_reg_imm8_0_8_16_24 : arg_kind list list list **)

let ak_reg_reg_imm8_0_8_16_24 =
  ((CAreg :: []) :: ((CAreg :: []) :: (((CAimm (CAimmC_arm_0_8_16_24,
    U8)) :: []) :: []))) :: []

(** val arm_UXTB_instr :
    arm_options -> (register, empty, empty, rflag, condt) instr_desc_t **)

let arm_UXTB_instr opts =
  let string_of_arm_mnemonic0 = fun mn ->
    (^) (string_of_arm_mnemonic mn)
      ((^) (if opts.set_flags then "S" else "")
        (if opts.is_conditional then "cc" else ""))
  in
  let mn = UXTB in
  let tin = (lreg arm_decl) :: ((Coq_lword U8) :: []) in
  let semi = extend_bits_semi (Zpos (Coq_xO (Coq_xO (Coq_xO Coq_xH)))) in
  { id_valid = true; id_msb_flag = MSB_MERGE; id_tin = tin; id_in =
  ((coq_Ea arm_decl (S O)) :: ((coq_Ea arm_decl (S (S O))) :: [])); id_tout =
  ((lreg arm_decl) :: []); id_out = ((coq_Ea arm_decl O) :: []); id_semi =
  (sem_prod_ok (map eval_ltype tin) (Obj.magic semi)); id_args_kinds =
  ak_reg_reg_imm8_0_8_16_24; id_nargs = (S (S (S O))); id_str_jas =
  (pp_s (string_of_arm_mnemonic0 mn)); id_safe = []; id_pp_asm =
  (pp_arm_op mn opts) }

(** val arm_UXTH_instr :
    arm_options -> (register, empty, empty, rflag, condt) instr_desc_t **)

let arm_UXTH_instr opts =
  let string_of_arm_mnemonic0 = fun mn ->
    (^) (string_of_arm_mnemonic mn)
      ((^) (if opts.set_flags then "S" else "")
        (if opts.is_conditional then "cc" else ""))
  in
  let mn = UXTH in
  let tin = (lreg arm_decl) :: ((Coq_lword U8) :: []) in
  let semi =
    extend_bits_semi (Zpos (Coq_xO (Coq_xO (Coq_xO (Coq_xO Coq_xH)))))
  in
  { id_valid = true; id_msb_flag = MSB_MERGE; id_tin = tin; id_in =
  ((coq_Ea arm_decl (S O)) :: ((coq_Ea arm_decl (S (S O))) :: [])); id_tout =
  ((lreg arm_decl) :: []); id_out = ((coq_Ea arm_decl O) :: []); id_semi =
  (sem_prod_ok (map eval_ltype tin) (Obj.magic semi)); id_args_kinds =
  ak_reg_reg_imm8_0_8_16_24; id_nargs = (S (S (S O))); id_str_jas =
  (pp_s (string_of_arm_mnemonic0 mn)); id_safe = []; id_pp_asm =
  (pp_arm_op mn opts) }

(** val arm_SBFX_instr :
    arm_options -> (register, empty, empty, rflag, condt) instr_desc_t **)

let arm_SBFX_instr opts =
  let string_of_arm_mnemonic0 = fun mn ->
    (^) (string_of_arm_mnemonic mn)
      ((^) (if opts.set_flags then "S" else "")
        (if opts.is_conditional then "cc" else ""))
  in
  let mn = SBFX in
  let sh = wsar arm_decl.reg_size in
  { id_valid = true; id_msb_flag = MSB_MERGE; id_tin =
  ((lreg arm_decl) :: ((Coq_lword U8) :: ((Coq_lword U8) :: []))); id_in =
  ((coq_Ea arm_decl (S O)) :: ((coq_Ea arm_decl (S (S O))) :: ((coq_Ea
                                                                 arm_decl (S
                                                                 (S (S O)))) :: [])));
  id_tout = ((lreg arm_decl) :: []); id_out = ((coq_Ea arm_decl O) :: []);
  id_semi = (Obj.magic bit_field_extract_semi sh); id_args_kinds =
  ak_reg_reg_imm_imm_extr; id_nargs = (S (S (S (S O)))); id_str_jas =
  (pp_s (string_of_arm_mnemonic0 mn)); id_safe = bit_field_extract_semi_sc;
  id_pp_asm = (pp_arm_op mn opts) }

(** val arm_CMP_semi : sem_tuple -> sem_tuple -> sem_tuple **)

let arm_CMP_semi wn wm =
  let wmnot = wnot arm_decl.reg_size wm in
  nzcv_of_aluop arm_decl.reg_size
    (GRing.add
      (GRing.ComRing.Exports.coq_GRing_ComRing__to__GRing_Nmodule
        (word arm_decl.reg_size))
      (GRing.add
        (GRing.ComRing.Exports.coq_GRing_ComRing__to__GRing_Nmodule
          (word arm_decl.reg_size)) wn wmnot)
      (GRing.one
        (GRing.ComRing.Exports.coq_GRing_ComRing__to__GRing_SemiRing
          (word arm_decl.reg_size))))
    (Z.add
      (Z.add (wunsigned arm_decl.reg_size wn)
        (wunsigned arm_decl.reg_size wmnot)) (Zpos Coq_xH))
    (Z.add
      (Z.add (wsigned arm_decl.reg_size wn) (wsigned arm_decl.reg_size wmnot))
      (Zpos Coq_xH))

(** val arm_CMP_instr :
    arm_options -> (register, empty, empty, rflag, condt) instr_desc_t **)

let arm_CMP_instr opts =
  let string_of_arm_mnemonic0 = fun mn ->
    (^) (string_of_arm_mnemonic mn)
      ((^) (if opts.set_flags then "S" else "")
        (if opts.is_conditional then "cc" else ""))
  in
  let mn = CMP in
  let tin = (lreg arm_decl) :: ((lreg arm_decl) :: []) in
  let x = { id_valid = true; id_msb_flag = MSB_MERGE; id_tin = tin; id_in =
    ((coq_Ea arm_decl O) :: ((coq_Ea arm_decl (S O)) :: [])); id_tout =
    (Coq_lbool :: (Coq_lbool :: (Coq_lbool :: (Coq_lbool :: [])))); id_out =
    ad_nzcv; id_semi =
    (sem_prod_ok (map eval_ltype tin) (Obj.magic arm_CMP_semi));
    id_args_kinds = (ak_reg_reg_or_imm opts chk_imm_accept_shift); id_nargs =
    (S (S O)); id_str_jas = (pp_s (string_of_arm_mnemonic0 mn)); id_safe =
    []; id_pp_asm = (pp_arm_op mn opts) }
  in
  (match opts.has_shift with
   | Some sk ->
     mk_shifted sk x (mk_semi2_2_shifted (lreg arm_decl) sk x.id_semi)
   | None -> x)

(** val arm_TST_semi : sem_tuple -> sem_tuple -> sem_tuple **)

let arm_TST_semi wn wm =
  let res = wand arm_decl.reg_size wn wm in
  Obj.magic ((Some (coq_NF_of_word arm_decl.reg_size res)), ((Some
    (coq_ZF_of_word arm_decl.reg_size res)), (Some false)))

(** val arm_TST_instr :
    arm_options -> (register, empty, empty, rflag, condt) instr_desc_t **)

let arm_TST_instr opts =
  let string_of_arm_mnemonic0 = fun mn ->
    (^) (string_of_arm_mnemonic mn)
      ((^) (if opts.set_flags then "S" else "")
        (if opts.is_conditional then "cc" else ""))
  in
  let mn = TST in
  let tin = (lreg arm_decl) :: ((lreg arm_decl) :: []) in
  let x = { id_valid = true; id_msb_flag = MSB_MERGE; id_tin = tin; id_in =
    ((coq_Ea arm_decl O) :: ((coq_Ea arm_decl (S O)) :: [])); id_tout =
    (Coq_lbool :: (Coq_lbool :: (Coq_lbool :: []))); id_out = ad_nzc;
    id_semi = (sem_prod_ok (map eval_ltype tin) (Obj.magic arm_TST_semi));
    id_args_kinds = (ak_reg_reg_or_imm opts chk_imm_reject_shift); id_nargs =
    (S (S O)); id_str_jas = (pp_s (string_of_arm_mnemonic0 mn)); id_safe =
    []; id_pp_asm = (pp_arm_op mn opts) }
  in
  (match opts.has_shift with
   | Some sk ->
     mk_shifted sk x (mk_semi2_2_shifted (lreg arm_decl) sk x.id_semi)
   | None -> x)

(** val arm_CMN_instr :
    arm_options -> (register, empty, empty, rflag, condt) instr_desc_t **)

let arm_CMN_instr opts =
  let string_of_arm_mnemonic0 = fun mn ->
    (^) (string_of_arm_mnemonic mn)
      ((^) (if opts.set_flags then "S" else "")
        (if opts.is_conditional then "cc" else ""))
  in
  let mn = CMN in
  let tin = (lreg arm_decl) :: ((lreg arm_decl) :: []) in
  let semi = fun wn wm ->
    rtuple_drop5th (eval_ltype Coq_lbool) (eval_ltype Coq_lbool)
      (eval_ltype Coq_lbool) (eval_ltype Coq_lbool)
      (eval_ltype (lreg arm_decl)) (arm_ADD_semi wn wm)
  in
  let x = { id_valid = true; id_msb_flag = MSB_MERGE; id_tin = tin; id_in =
    ((coq_Ea arm_decl O) :: ((coq_Ea arm_decl (S O)) :: [])); id_tout =
    (Coq_lbool :: (Coq_lbool :: (Coq_lbool :: (Coq_lbool :: [])))); id_out =
    ad_nzcv; id_semi = (sem_prod_ok (map eval_ltype tin) (Obj.magic semi));
    id_args_kinds = (ak_reg_reg_or_imm opts chk_imm_accept_shift); id_nargs =
    (S (S O)); id_str_jas = (pp_s (string_of_arm_mnemonic0 mn)); id_safe =
    []; id_pp_asm = (pp_arm_op mn opts) }
  in
  (match opts.has_shift with
   | Some sk ->
     mk_shifted sk x (mk_semi2_2_shifted (lreg arm_decl) sk x.id_semi)
   | None -> x)

(** val arm_extend_semi :
    wsize -> bool -> wsize -> GRing.ComRing.sort -> GRing.ComRing.sort **)

let arm_extend_semi ws sign ws' wn =
  if sign then sign_extend ws' ws wn else zero_extend ws' ws wn

(** val arm_load_instr :
    arm_options -> arm_mnemonic -> (register, empty, empty, rflag, condt)
    instr_desc_t **)

let arm_load_instr opts =
  let string_of_arm_mnemonic0 = fun mn ->
    (^) (string_of_arm_mnemonic mn)
      ((^) (if opts.set_flags then "S" else "")
        (if opts.is_conditional then "cc" else ""))
  in
  (fun mn ->
  let ws = match wsize_of_load_mn mn with
           | Some ws' -> ws'
           | None -> U32 in
  let tin = (Coq_lword ws) :: [] in
  let semi =
    arm_extend_semi ws (isSome (wsize_of_sload_mn mn)) arm_decl.reg_size
  in
  { id_valid = true; id_msb_flag = MSB_MERGE; id_tin = tin; id_in =
  ((coq_Eu arm_decl (S O)) :: []); id_tout = ((lreg arm_decl) :: []);
  id_out = ((coq_Ea arm_decl O) :: []); id_semi =
  (sem_prod_ok (map eval_ltype tin) (Obj.magic semi)); id_args_kinds =
  ak_reg_addr; id_nargs = (S (S O)); id_str_jas =
  (pp_s (string_of_arm_mnemonic0 mn)); id_safe = []; id_pp_asm =
  (pp_arm_op mn opts) })

(** val arm_store_instr :
    arm_options -> arm_mnemonic -> (register, empty, empty, rflag, condt)
    instr_desc_t **)

let arm_store_instr opts =
  let string_of_arm_mnemonic0 = fun mn ->
    (^) (string_of_arm_mnemonic mn)
      ((^) (if opts.set_flags then "S" else "")
        (if opts.is_conditional then "cc" else ""))
  in
  (fun mn ->
  let ws = match wsize_of_store_mn mn with
           | Some ws' -> ws'
           | None -> U32 in
  let tin = (Coq_lword ws) :: [] in
  let semi = arm_extend_semi ws false ws in
  { id_valid = true; id_msb_flag = MSB_MERGE; id_tin = tin; id_in =
  ((coq_Ea arm_decl O) :: []); id_tout = ((Coq_lword ws) :: []); id_out =
  ((coq_Eu arm_decl (S O)) :: []); id_semi =
  (sem_prod_ok (map eval_ltype tin) (Obj.magic semi)); id_args_kinds =
  ak_reg_addr; id_nargs = (S (S O)); id_str_jas =
  (pp_s (string_of_arm_mnemonic0 mn)); id_safe = []; id_pp_asm =
  (pp_arm_op mn opts) })

(** val arm_CLZ_instr :
    arm_options -> (register, empty, empty, rflag, condt) instr_desc_t **)

let arm_CLZ_instr opts =
  let string_of_arm_mnemonic0 = fun mn ->
    (^) (string_of_arm_mnemonic mn)
      ((^) (if opts.set_flags then "S" else "")
        (if opts.is_conditional then "cc" else ""))
  in
  let mn = CLZ in
  let tin = (lreg arm_decl) :: [] in
  let semi = fun z -> leading_zero arm_decl.reg_size z in
  { id_valid = true; id_msb_flag = MSB_MERGE; id_tin = tin; id_in =
  ((coq_Ea arm_decl (S O)) :: []); id_tout = ((lreg arm_decl) :: []);
  id_out = ((coq_Ea arm_decl O) :: []); id_semi =
  (sem_prod_ok (map eval_ltype tin) (Obj.magic semi)); id_args_kinds =
  ak_reg_reg; id_nargs = (S (S O)); id_str_jas =
  (pp_s (string_of_arm_mnemonic0 mn)); id_safe = []; id_pp_asm =
  (pp_arm_op mn opts) }

(** val mn_desc :
    arm_options -> arm_mnemonic -> (register, empty, empty, rflag, condt)
    instr_desc_t **)

let mn_desc opts = function
| ADD -> arm_ADD_instr opts
| ADC -> arm_ADC_instr opts
| MUL -> arm_MUL_instr opts
| MLA -> arm_MLA_instr opts
| MLS -> arm_MLS_instr opts
| SDIV -> arm_SDIV_instr opts
| SUB -> arm_SUB_instr opts
| SBC -> arm_SBC_instr opts
| RSB -> arm_RSB_instr opts
| UDIV -> arm_UDIV_instr opts
| UMULL -> arm_UMULL_instr opts
| UMAAL -> arm_UMAAL_instr opts
| UMLAL -> arm_UMLAL_instr opts
| SMULL -> arm_SMULL_instr opts
| SMLAL -> arm_SMLAL_instr opts
| SMMUL -> arm_SMMUL_instr opts
| SMMULR -> arm_SMMULR_instr opts
| SMUL_hw (hw0, hw1) -> arm_smul_hw_instr opts hw0 hw1
| SMLA_hw (hw0, hw1) -> arm_smla_hw_instr opts hw0 hw1
| SMULW_hw hw -> arm_smulw_hw_instr opts hw
| AND -> arm_AND_instr opts
| BFC -> arm_BFC_instr opts
| BFI -> arm_BFI_instr opts
| BIC -> arm_BIC_instr opts
| EOR -> arm_EOR_instr opts
| MVN -> arm_MVN_instr opts
| ORR -> arm_ORR_instr opts
| ASR -> arm_ASR_instr opts
| LSL -> arm_LSL_instr opts
| LSR -> arm_LSR_instr opts
| ROR -> arm_ROR_instr opts
| REV -> arm_REV_instr opts
| REV16 -> arm_REV16_instr opts
| REVSH -> arm_REVSH_instr opts
| ADR -> arm_ADR_instr opts
| MOV -> arm_MOV_instr opts
| MOVT -> arm_MOVT_instr opts
| UBFX -> arm_UBFX_instr opts
| UXTB -> arm_UXTB_instr opts
| UXTH -> arm_UXTH_instr opts
| SBFX -> arm_SBFX_instr opts
| CLZ -> arm_CLZ_instr opts
| CMP -> arm_CMP_instr opts
| TST -> arm_TST_instr opts
| CMN -> arm_CMN_instr opts
| STR -> arm_store_instr opts STR
| STRB -> arm_store_instr opts STRB
| STRH -> arm_store_instr opts STRH
| x -> arm_load_instr opts x

(** val arm_instr_desc :
    arm_op -> (register, empty, empty, rflag, condt) instr_desc_t **)

let arm_instr_desc = function
| ARM_op (mn, opts) ->
  let x = mn_desc opts mn in if opts.is_conditional then mk_cond x else x

(** val arm_prim_string : (string * arm_op prim_constructor) list **)

let arm_prim_string =
  ("ADD", (PrimARM (fun sf ic -> Ok (ARM_op (ADD, { set_flags = sf;
    is_conditional = ic; has_shift = None }))))) :: (("ADC", (PrimARM
    (fun sf ic -> Ok (ARM_op (ADC, { set_flags = sf; is_conditional = ic;
    has_shift = None }))))) :: (("MUL", (PrimARM (fun sf ic -> Ok (ARM_op
    (MUL, { set_flags = sf; is_conditional = ic; has_shift =
    None }))))) :: (("MLA", (PrimARM (fun sf ic ->
    if sf
    then let s = "this mnemonic cannot set flags" in Error s
    else Ok (ARM_op (MLA, { set_flags = sf; is_conditional = ic; has_shift =
           None }))))) :: (("MLS", (PrimARM (fun sf ic ->
    if sf
    then let s = "this mnemonic cannot set flags" in Error s
    else Ok (ARM_op (MLS, { set_flags = sf; is_conditional = ic; has_shift =
           None }))))) :: (("SDIV", (PrimARM (fun sf ic ->
    if sf
    then let s = "this mnemonic cannot set flags" in Error s
    else Ok (ARM_op (SDIV, { set_flags = sf; is_conditional = ic; has_shift =
           None }))))) :: (("SUB", (PrimARM (fun sf ic -> Ok (ARM_op (SUB,
    { set_flags = sf; is_conditional = ic; has_shift =
    None }))))) :: (("SBC", (PrimARM (fun sf ic -> Ok (ARM_op (SBC,
    { set_flags = sf; is_conditional = ic; has_shift =
    None }))))) :: (("RSB", (PrimARM (fun sf ic -> Ok (ARM_op (RSB,
    { set_flags = sf; is_conditional = ic; has_shift =
    None }))))) :: (("UDIV", (PrimARM (fun sf ic ->
    if sf
    then let s = "this mnemonic cannot set flags" in Error s
    else Ok (ARM_op (UDIV, { set_flags = sf; is_conditional = ic; has_shift =
           None }))))) :: (("UMULL", (PrimARM (fun sf ic ->
    if sf
    then let s = "this mnemonic cannot set flags" in Error s
    else Ok (ARM_op (UMULL, { set_flags = sf; is_conditional = ic;
           has_shift = None }))))) :: (("UMAAL", (PrimARM (fun sf ic ->
    if sf
    then let s = "this mnemonic cannot set flags" in Error s
    else Ok (ARM_op (UMAAL, { set_flags = sf; is_conditional = ic;
           has_shift = None }))))) :: (("UMLAL", (PrimARM (fun sf ic ->
    if sf
    then let s = "this mnemonic cannot set flags" in Error s
    else Ok (ARM_op (UMLAL, { set_flags = sf; is_conditional = ic;
           has_shift = None }))))) :: (("SMULL", (PrimARM (fun sf ic ->
    if sf
    then let s = "this mnemonic cannot set flags" in Error s
    else Ok (ARM_op (SMULL, { set_flags = sf; is_conditional = ic;
           has_shift = None }))))) :: (("SMLAL", (PrimARM (fun sf ic ->
    if sf
    then let s = "this mnemonic cannot set flags" in Error s
    else Ok (ARM_op (SMLAL, { set_flags = sf; is_conditional = ic;
           has_shift = None }))))) :: (("SMMUL", (PrimARM (fun sf ic ->
    if sf
    then let s = "this mnemonic cannot set flags" in Error s
    else Ok (ARM_op (SMMUL, { set_flags = sf; is_conditional = ic;
           has_shift = None }))))) :: (("SMMULR", (PrimARM (fun sf ic ->
    if sf
    then let s = "this mnemonic cannot set flags" in Error s
    else Ok (ARM_op (SMMULR, { set_flags = sf; is_conditional = ic;
           has_shift = None }))))) :: (("SMULBB", (PrimARM (fun sf ic ->
    if sf
    then let s = "this mnemonic cannot set flags" in Error s
    else Ok (ARM_op ((SMUL_hw (HWB, HWB)), { set_flags = sf; is_conditional =
           ic; has_shift = None }))))) :: (("SMULBT", (PrimARM (fun sf ic ->
    if sf
    then let s = "this mnemonic cannot set flags" in Error s
    else Ok (ARM_op ((SMUL_hw (HWB, HWT)), { set_flags = sf; is_conditional =
           ic; has_shift = None }))))) :: (("SMULTB", (PrimARM (fun sf ic ->
    if sf
    then let s = "this mnemonic cannot set flags" in Error s
    else Ok (ARM_op ((SMUL_hw (HWT, HWB)), { set_flags = sf; is_conditional =
           ic; has_shift = None }))))) :: (("SMULTT", (PrimARM (fun sf ic ->
    if sf
    then let s = "this mnemonic cannot set flags" in Error s
    else Ok (ARM_op ((SMUL_hw (HWT, HWT)), { set_flags = sf; is_conditional =
           ic; has_shift = None }))))) :: (("SMLABB", (PrimARM (fun sf ic ->
    if sf
    then let s = "this mnemonic cannot set flags" in Error s
    else Ok (ARM_op ((SMLA_hw (HWB, HWB)), { set_flags = sf; is_conditional =
           ic; has_shift = None }))))) :: (("SMLABT", (PrimARM (fun sf ic ->
    if sf
    then let s = "this mnemonic cannot set flags" in Error s
    else Ok (ARM_op ((SMLA_hw (HWB, HWT)), { set_flags = sf; is_conditional =
           ic; has_shift = None }))))) :: (("SMLATB", (PrimARM (fun sf ic ->
    if sf
    then let s = "this mnemonic cannot set flags" in Error s
    else Ok (ARM_op ((SMLA_hw (HWT, HWB)), { set_flags = sf; is_conditional =
           ic; has_shift = None }))))) :: (("SMLATT", (PrimARM (fun sf ic ->
    if sf
    then let s = "this mnemonic cannot set flags" in Error s
    else Ok (ARM_op ((SMLA_hw (HWT, HWT)), { set_flags = sf; is_conditional =
           ic; has_shift = None }))))) :: (("SMULWB", (PrimARM (fun sf ic ->
    if sf
    then let s = "this mnemonic cannot set flags" in Error s
    else Ok (ARM_op ((SMULW_hw HWB), { set_flags = sf; is_conditional = ic;
           has_shift = None }))))) :: (("SMULWT", (PrimARM (fun sf ic ->
    if sf
    then let s = "this mnemonic cannot set flags" in Error s
    else Ok (ARM_op ((SMULW_hw HWT), { set_flags = sf; is_conditional = ic;
           has_shift = None }))))) :: (("AND", (PrimARM (fun sf ic -> Ok
    (ARM_op (AND, { set_flags = sf; is_conditional = ic; has_shift =
    None }))))) :: (("BFC", (PrimARM (fun sf ic ->
    if sf
    then let s = "this mnemonic cannot set flags" in Error s
    else Ok (ARM_op (BFC, { set_flags = sf; is_conditional = ic; has_shift =
           None }))))) :: (("BFI", (PrimARM (fun sf ic ->
    if sf
    then let s = "this mnemonic cannot set flags" in Error s
    else Ok (ARM_op (BFI, { set_flags = sf; is_conditional = ic; has_shift =
           None }))))) :: (("BIC", (PrimARM (fun sf ic -> Ok (ARM_op (BIC,
    { set_flags = sf; is_conditional = ic; has_shift =
    None }))))) :: (("EOR", (PrimARM (fun sf ic -> Ok (ARM_op (EOR,
    { set_flags = sf; is_conditional = ic; has_shift =
    None }))))) :: (("MVN", (PrimARM (fun sf ic -> Ok (ARM_op (MVN,
    { set_flags = sf; is_conditional = ic; has_shift =
    None }))))) :: (("ORR", (PrimARM (fun sf ic -> Ok (ARM_op (ORR,
    { set_flags = sf; is_conditional = ic; has_shift =
    None }))))) :: (("ASR", (PrimARM (fun sf ic -> Ok (ARM_op (ASR,
    { set_flags = sf; is_conditional = ic; has_shift =
    None }))))) :: (("LSL", (PrimARM (fun sf ic -> Ok (ARM_op (LSL,
    { set_flags = sf; is_conditional = ic; has_shift =
    None }))))) :: (("LSR", (PrimARM (fun sf ic -> Ok (ARM_op (LSR,
    { set_flags = sf; is_conditional = ic; has_shift =
    None }))))) :: (("ROR", (PrimARM (fun sf ic -> Ok (ARM_op (ROR,
    { set_flags = sf; is_conditional = ic; has_shift =
    None }))))) :: (("REV", (PrimARM (fun sf ic ->
    if sf
    then let s = "this mnemonic cannot set flags" in Error s
    else Ok (ARM_op (REV, { set_flags = sf; is_conditional = ic; has_shift =
           None }))))) :: (("REV16", (PrimARM (fun sf ic ->
    if sf
    then let s = "this mnemonic cannot set flags" in Error s
    else Ok (ARM_op (REV16, { set_flags = sf; is_conditional = ic;
           has_shift = None }))))) :: (("REVSH", (PrimARM (fun sf ic ->
    if sf
    then let s = "this mnemonic cannot set flags" in Error s
    else Ok (ARM_op (REVSH, { set_flags = sf; is_conditional = ic;
           has_shift = None }))))) :: (("ADR", (PrimARM (fun sf ic ->
    if sf
    then let s = "this mnemonic cannot set flags" in Error s
    else Ok (ARM_op (ADR, { set_flags = sf; is_conditional = ic; has_shift =
           None }))))) :: (("MOV", (PrimARM (fun sf ic -> Ok (ARM_op (MOV,
    { set_flags = sf; is_conditional = ic; has_shift =
    None }))))) :: (("MOVT", (PrimARM (fun sf ic ->
    if sf
    then let s = "this mnemonic cannot set flags" in Error s
    else Ok (ARM_op (MOVT, { set_flags = sf; is_conditional = ic; has_shift =
           None }))))) :: (("UBFX", (PrimARM (fun sf ic ->
    if sf
    then let s = "this mnemonic cannot set flags" in Error s
    else Ok (ARM_op (UBFX, { set_flags = sf; is_conditional = ic; has_shift =
           None }))))) :: (("UXTB", (PrimARM (fun sf ic ->
    if sf
    then let s = "this mnemonic cannot set flags" in Error s
    else Ok (ARM_op (UXTB, { set_flags = sf; is_conditional = ic; has_shift =
           (Some SROR) }))))) :: (("UXTH", (PrimARM (fun sf ic ->
    if sf
    then let s = "this mnemonic cannot set flags" in Error s
    else Ok (ARM_op (UXTH, { set_flags = sf; is_conditional = ic; has_shift =
           (Some SROR) }))))) :: (("SBFX", (PrimARM (fun sf ic ->
    if sf
    then let s = "this mnemonic cannot set flags" in Error s
    else Ok (ARM_op (SBFX, { set_flags = sf; is_conditional = ic; has_shift =
           None }))))) :: (("CLZ", (PrimARM (fun sf ic ->
    if sf
    then let s = "this mnemonic cannot set flags" in Error s
    else Ok (ARM_op (CLZ, { set_flags = sf; is_conditional = ic; has_shift =
           None }))))) :: (("CMP", (PrimARM (fun sf ic ->
    if sf
    then let s = "this mnemonic cannot set flags" in Error s
    else Ok (ARM_op (CMP, { set_flags = sf; is_conditional = ic; has_shift =
           None }))))) :: (("TST", (PrimARM (fun sf ic ->
    if sf
    then let s = "this mnemonic cannot set flags" in Error s
    else Ok (ARM_op (TST, { set_flags = sf; is_conditional = ic; has_shift =
           None }))))) :: (("CMN", (PrimARM (fun sf ic ->
    if sf
    then let s = "this mnemonic cannot set flags" in Error s
    else Ok (ARM_op (CMN, { set_flags = sf; is_conditional = ic; has_shift =
           None }))))) :: (("LDR", (PrimARM (fun sf ic ->
    if sf
    then let s = "this mnemonic cannot set flags" in Error s
    else Ok (ARM_op (LDR, { set_flags = sf; is_conditional = ic; has_shift =
           None }))))) :: (("LDRB", (PrimARM (fun sf ic ->
    if sf
    then let s = "this mnemonic cannot set flags" in Error s
    else Ok (ARM_op (LDRB, { set_flags = sf; is_conditional = ic; has_shift =
           None }))))) :: (("LDRH", (PrimARM (fun sf ic ->
    if sf
    then let s = "this mnemonic cannot set flags" in Error s
    else Ok (ARM_op (LDRH, { set_flags = sf; is_conditional = ic; has_shift =
           None }))))) :: (("LDRSB", (PrimARM (fun sf ic ->
    if sf
    then let s = "this mnemonic cannot set flags" in Error s
    else Ok (ARM_op (LDRSB, { set_flags = sf; is_conditional = ic;
           has_shift = None }))))) :: (("LDRSH", (PrimARM (fun sf ic ->
    if sf
    then let s = "this mnemonic cannot set flags" in Error s
    else Ok (ARM_op (LDRSH, { set_flags = sf; is_conditional = ic;
           has_shift = None }))))) :: (("STR", (PrimARM (fun sf ic ->
    if sf
    then let s = "this mnemonic cannot set flags" in Error s
    else Ok (ARM_op (STR, { set_flags = sf; is_conditional = ic; has_shift =
           None }))))) :: (("STRB", (PrimARM (fun sf ic ->
    if sf
    then let s = "this mnemonic cannot set flags" in Error s
    else Ok (ARM_op (STRB, { set_flags = sf; is_conditional = ic; has_shift =
           None }))))) :: (("STRH", (PrimARM (fun sf ic ->
    if sf
    then let s = "this mnemonic cannot set flags" in Error s
    else Ok (ARM_op (STRH, { set_flags = sf; is_conditional = ic; has_shift =
           None }))))) :: [])))))))))))))))))))))))))))))))))))))))))))))))))))))))))))

(** val arm_op_decl :
    (register, empty, empty, rflag, condt, arm_op) asm_op_decl **)

let arm_op_decl =
  { Arch_decl._eqT = eqTC_arm_op; instr_desc_op = arm_instr_desc;
    Arch_decl.prim_string = arm_prim_string }

type arm_prog = (register, empty, empty, rflag, condt, arm_op) asm_prog

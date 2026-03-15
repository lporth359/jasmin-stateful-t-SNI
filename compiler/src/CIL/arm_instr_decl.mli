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

module E :
 sig
  val no_semantics : error
 end

type arm_options = { set_flags : bool; is_conditional : bool;
                     has_shift : shift_kind option }

val set_flags : arm_options -> bool

val is_conditional : arm_options -> bool

val has_shift : arm_options -> shift_kind option

type is_arm_options =
| Coq_is_Build_arm_options of bool * Param1.Coq_exports.is_bool * bool
   * Param1.Coq_exports.is_bool * shift_kind option
   * (shift_kind, is_shift_kind) Prelude.is_option

val is_arm_options_rect :
  (bool -> Param1.Coq_exports.is_bool -> bool -> Param1.Coq_exports.is_bool
  -> shift_kind option -> (shift_kind, is_shift_kind) Prelude.is_option ->
  'a1) -> arm_options -> is_arm_options -> 'a1

val is_arm_options_rec :
  (bool -> Param1.Coq_exports.is_bool -> bool -> Param1.Coq_exports.is_bool
  -> shift_kind option -> (shift_kind, is_shift_kind) Prelude.is_option ->
  'a1) -> arm_options -> is_arm_options -> 'a1

val arm_options_tag : arm_options -> positive

val is_arm_options_inhab : arm_options -> is_arm_options

val is_arm_options_functor : arm_options -> is_arm_options -> is_arm_options

type box_arm_options_Build_arm_options = { coq_Box_arm_options_Build_arm_options_0 : 
                                           bool;
                                           coq_Box_arm_options_Build_arm_options_1 : 
                                           bool;
                                           coq_Box_arm_options_Build_arm_options_2 : 
                                           shift_kind option }

val coq_Box_arm_options_Build_arm_options_0 :
  box_arm_options_Build_arm_options -> bool

val coq_Box_arm_options_Build_arm_options_1 :
  box_arm_options_Build_arm_options -> bool

val coq_Box_arm_options_Build_arm_options_2 :
  box_arm_options_Build_arm_options -> shift_kind option

type arm_options_fields_t = box_arm_options_Build_arm_options

val arm_options_fields : arm_options -> arm_options_fields_t

val arm_options_construct :
  positive -> box_arm_options_Build_arm_options -> arm_options option

val arm_options_induction :
  (bool -> Param1.Coq_exports.is_bool -> bool -> Param1.Coq_exports.is_bool
  -> shift_kind option -> (shift_kind, is_shift_kind) Prelude.is_option ->
  'a1) -> arm_options -> is_arm_options -> 'a1

val arm_options_eqb_fields :
  (arm_options -> arm_options -> bool) -> positive ->
  box_arm_options_Build_arm_options -> box_arm_options_Build_arm_options ->
  bool

val arm_options_eqb : arm_options -> arm_options -> bool

val arm_options_eqb_OK : arm_options -> arm_options -> reflect

val arm_options_eqb_OK_sumbool : arm_options -> arm_options -> bool

val eqTC_arm_options : arm_options eqTypeC

val arm_options_eqType : Equality.coq_type

val default_opts : arm_options

val set_is_conditional : arm_options -> arm_options

val unset_is_conditional : arm_options -> arm_options

type halfword =
| HWB
| HWT

val halfword_rect : 'a1 -> 'a1 -> halfword -> 'a1

val halfword_rec : 'a1 -> 'a1 -> halfword -> 'a1

type is_halfword =
| Coq_is_HWB
| Coq_is_HWT

val is_halfword_rect : 'a1 -> 'a1 -> halfword -> is_halfword -> 'a1

val is_halfword_rec : 'a1 -> 'a1 -> halfword -> is_halfword -> 'a1

val halfword_tag : halfword -> positive

val is_halfword_inhab : halfword -> is_halfword

val is_halfword_functor : halfword -> is_halfword -> is_halfword

type box_halfword_HWB =
| Box_halfword_HWB

type halfword_fields_t = __

val halfword_fields : halfword -> halfword_fields_t

val halfword_construct : positive -> halfword_fields_t -> halfword option

val halfword_induction : 'a1 -> 'a1 -> halfword -> is_halfword -> 'a1

val halfword_eqb_fields :
  (halfword -> halfword -> bool) -> positive -> halfword_fields_t ->
  halfword_fields_t -> bool

val halfword_eqb : halfword -> halfword -> bool

val halfword_eqb_OK : halfword -> halfword -> reflect

val halfword_eqb_OK_sumbool : halfword -> halfword -> bool

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

val arm_mnemonic_rect :
  'a1 -> 'a1 -> 'a1 -> 'a1 -> 'a1 -> 'a1 -> 'a1 -> 'a1 -> 'a1 -> 'a1 -> 'a1
  -> 'a1 -> 'a1 -> 'a1 -> 'a1 -> 'a1 -> 'a1 -> (halfword -> halfword -> 'a1)
  -> (halfword -> halfword -> 'a1) -> (halfword -> 'a1) -> 'a1 -> 'a1 -> 'a1
  -> 'a1 -> 'a1 -> 'a1 -> 'a1 -> 'a1 -> 'a1 -> 'a1 -> 'a1 -> 'a1 -> 'a1 ->
  'a1 -> 'a1 -> 'a1 -> 'a1 -> 'a1 -> 'a1 -> 'a1 -> 'a1 -> 'a1 -> 'a1 -> 'a1
  -> 'a1 -> 'a1 -> 'a1 -> 'a1 -> 'a1 -> 'a1 -> 'a1 -> 'a1 -> 'a1 ->
  arm_mnemonic -> 'a1

val arm_mnemonic_rec :
  'a1 -> 'a1 -> 'a1 -> 'a1 -> 'a1 -> 'a1 -> 'a1 -> 'a1 -> 'a1 -> 'a1 -> 'a1
  -> 'a1 -> 'a1 -> 'a1 -> 'a1 -> 'a1 -> 'a1 -> (halfword -> halfword -> 'a1)
  -> (halfword -> halfword -> 'a1) -> (halfword -> 'a1) -> 'a1 -> 'a1 -> 'a1
  -> 'a1 -> 'a1 -> 'a1 -> 'a1 -> 'a1 -> 'a1 -> 'a1 -> 'a1 -> 'a1 -> 'a1 ->
  'a1 -> 'a1 -> 'a1 -> 'a1 -> 'a1 -> 'a1 -> 'a1 -> 'a1 -> 'a1 -> 'a1 -> 'a1
  -> 'a1 -> 'a1 -> 'a1 -> 'a1 -> 'a1 -> 'a1 -> 'a1 -> 'a1 -> 'a1 ->
  arm_mnemonic -> 'a1

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

val is_arm_mnemonic_rect :
  'a1 -> 'a1 -> 'a1 -> 'a1 -> 'a1 -> 'a1 -> 'a1 -> 'a1 -> 'a1 -> 'a1 -> 'a1
  -> 'a1 -> 'a1 -> 'a1 -> 'a1 -> 'a1 -> 'a1 -> (halfword -> is_halfword ->
  halfword -> is_halfword -> 'a1) -> (halfword -> is_halfword -> halfword ->
  is_halfword -> 'a1) -> (halfword -> is_halfword -> 'a1) -> 'a1 -> 'a1 ->
  'a1 -> 'a1 -> 'a1 -> 'a1 -> 'a1 -> 'a1 -> 'a1 -> 'a1 -> 'a1 -> 'a1 -> 'a1
  -> 'a1 -> 'a1 -> 'a1 -> 'a1 -> 'a1 -> 'a1 -> 'a1 -> 'a1 -> 'a1 -> 'a1 ->
  'a1 -> 'a1 -> 'a1 -> 'a1 -> 'a1 -> 'a1 -> 'a1 -> 'a1 -> 'a1 -> 'a1 ->
  arm_mnemonic -> is_arm_mnemonic -> 'a1

val is_arm_mnemonic_rec :
  'a1 -> 'a1 -> 'a1 -> 'a1 -> 'a1 -> 'a1 -> 'a1 -> 'a1 -> 'a1 -> 'a1 -> 'a1
  -> 'a1 -> 'a1 -> 'a1 -> 'a1 -> 'a1 -> 'a1 -> (halfword -> is_halfword ->
  halfword -> is_halfword -> 'a1) -> (halfword -> is_halfword -> halfword ->
  is_halfword -> 'a1) -> (halfword -> is_halfword -> 'a1) -> 'a1 -> 'a1 ->
  'a1 -> 'a1 -> 'a1 -> 'a1 -> 'a1 -> 'a1 -> 'a1 -> 'a1 -> 'a1 -> 'a1 -> 'a1
  -> 'a1 -> 'a1 -> 'a1 -> 'a1 -> 'a1 -> 'a1 -> 'a1 -> 'a1 -> 'a1 -> 'a1 ->
  'a1 -> 'a1 -> 'a1 -> 'a1 -> 'a1 -> 'a1 -> 'a1 -> 'a1 -> 'a1 -> 'a1 ->
  arm_mnemonic -> is_arm_mnemonic -> 'a1

val arm_mnemonic_tag : arm_mnemonic -> positive

val is_arm_mnemonic_inhab : arm_mnemonic -> is_arm_mnemonic

val is_arm_mnemonic_functor :
  arm_mnemonic -> is_arm_mnemonic -> is_arm_mnemonic

type box_arm_mnemonic_ADD =
| Box_arm_mnemonic_ADD

type box_arm_mnemonic_SMUL_hw = { coq_Box_arm_mnemonic_SMUL_hw_0 : halfword;
                                  coq_Box_arm_mnemonic_SMUL_hw_1 : halfword }

val coq_Box_arm_mnemonic_SMUL_hw_0 : box_arm_mnemonic_SMUL_hw -> halfword

val coq_Box_arm_mnemonic_SMUL_hw_1 : box_arm_mnemonic_SMUL_hw -> halfword

type box_arm_mnemonic_SMULW_hw =
  halfword
  (* singleton inductive, whose constructor was Box_arm_mnemonic_SMULW_hw *)

val coq_Box_arm_mnemonic_SMULW_hw_0 : box_arm_mnemonic_SMULW_hw -> halfword

type arm_mnemonic_fields_t = __

val arm_mnemonic_fields : arm_mnemonic -> arm_mnemonic_fields_t

val arm_mnemonic_construct :
  positive -> arm_mnemonic_fields_t -> arm_mnemonic option

val arm_mnemonic_induction :
  'a1 -> 'a1 -> 'a1 -> 'a1 -> 'a1 -> 'a1 -> 'a1 -> 'a1 -> 'a1 -> 'a1 -> 'a1
  -> 'a1 -> 'a1 -> 'a1 -> 'a1 -> 'a1 -> 'a1 -> (halfword -> is_halfword ->
  halfword -> is_halfword -> 'a1) -> (halfword -> is_halfword -> halfword ->
  is_halfword -> 'a1) -> (halfword -> is_halfword -> 'a1) -> 'a1 -> 'a1 ->
  'a1 -> 'a1 -> 'a1 -> 'a1 -> 'a1 -> 'a1 -> 'a1 -> 'a1 -> 'a1 -> 'a1 -> 'a1
  -> 'a1 -> 'a1 -> 'a1 -> 'a1 -> 'a1 -> 'a1 -> 'a1 -> 'a1 -> 'a1 -> 'a1 ->
  'a1 -> 'a1 -> 'a1 -> 'a1 -> 'a1 -> 'a1 -> 'a1 -> 'a1 -> 'a1 -> 'a1 ->
  arm_mnemonic -> is_arm_mnemonic -> 'a1

val arm_mnemonic_eqb_fields :
  (arm_mnemonic -> arm_mnemonic -> bool) -> positive -> arm_mnemonic_fields_t
  -> arm_mnemonic_fields_t -> bool

val arm_mnemonic_eqb : arm_mnemonic -> arm_mnemonic -> bool

val arm_mnemonic_eqb_OK : arm_mnemonic -> arm_mnemonic -> reflect

val arm_mnemonic_eqb_OK_sumbool : arm_mnemonic -> arm_mnemonic -> bool

val eqTC_arm_mnemonic : arm_mnemonic eqTypeC

val arm_mnemonic_eqType : Equality.coq_type

val arm_mnemonics : arm_mnemonic list

val finTC_arm_mnemonic : arm_mnemonic finTypeC

val arm_mnemonic_finType : Finite.coq_type

val set_flags_mnemonics : arm_mnemonic list

val has_shift_mnemonics : arm_mnemonic list

val condition_mnemonics : arm_mnemonic list

val always_has_shift_mnemonics : (arm_mnemonic * shift_kind) list

val wsize_uload_mn : (wsize * arm_mnemonic) list

val uload_mn_of_wsize : wsize -> arm_mnemonic option

val wsize_of_uload_mn : arm_mnemonic -> wsize option

val wsize_sload_mn : (wsize * arm_mnemonic) list

val sload_mn_of_wsize : wsize -> arm_mnemonic option

val wsize_of_sload_mn : arm_mnemonic -> wsize option

val wsize_of_load_mn : arm_mnemonic -> wsize option

val wsize_store_mn : (wsize * arm_mnemonic) list

val store_mn_of_wsize : wsize -> arm_mnemonic option

val wsize_of_store_mn : arm_mnemonic -> wsize option

val string_of_hw : halfword -> string

val string_of_arm_mnemonic : arm_mnemonic -> string

type arm_op =
| ARM_op of arm_mnemonic * arm_options

val arm_op_rect : (arm_mnemonic -> arm_options -> 'a1) -> arm_op -> 'a1

val arm_op_rec : (arm_mnemonic -> arm_options -> 'a1) -> arm_op -> 'a1

type is_arm_op =
| Coq_is_ARM_op of arm_mnemonic * is_arm_mnemonic * arm_options
   * is_arm_options

val is_arm_op_rect :
  (arm_mnemonic -> is_arm_mnemonic -> arm_options -> is_arm_options -> 'a1)
  -> arm_op -> is_arm_op -> 'a1

val is_arm_op_rec :
  (arm_mnemonic -> is_arm_mnemonic -> arm_options -> is_arm_options -> 'a1)
  -> arm_op -> is_arm_op -> 'a1

val arm_op_tag : arm_op -> positive

val is_arm_op_inhab : arm_op -> is_arm_op

val is_arm_op_functor : arm_op -> is_arm_op -> is_arm_op

type box_arm_op_ARM_op = { coq_Box_arm_op_ARM_op_0 : arm_mnemonic;
                           coq_Box_arm_op_ARM_op_1 : arm_options }

val coq_Box_arm_op_ARM_op_0 : box_arm_op_ARM_op -> arm_mnemonic

val coq_Box_arm_op_ARM_op_1 : box_arm_op_ARM_op -> arm_options

type arm_op_fields_t = box_arm_op_ARM_op

val arm_op_fields : arm_op -> arm_op_fields_t

val arm_op_construct : positive -> box_arm_op_ARM_op -> arm_op option

val arm_op_induction :
  (arm_mnemonic -> is_arm_mnemonic -> arm_options -> is_arm_options -> 'a1)
  -> arm_op -> is_arm_op -> 'a1

val arm_op_eqb_fields :
  (arm_op -> arm_op -> bool) -> positive -> box_arm_op_ARM_op ->
  box_arm_op_ARM_op -> bool

val arm_op_eqb : arm_op -> arm_op -> bool

val arm_op_eqb_OK : arm_op -> arm_op -> reflect

val arm_op_eqb_OK_sumbool : arm_op -> arm_op -> bool

val eqTC_arm_op : arm_op eqTypeC

val arm_op_eqType : Equality.coq_type

val ad_nz : (register, empty, empty, rflag, condt) Arch_decl.arg_desc list

val ad_nzc : (register, empty, empty, rflag, condt) Arch_decl.arg_desc list

val ad_nzcv : (register, empty, empty, rflag, condt) Arch_decl.arg_desc list

val coq_NF_of_word : wsize -> GRing.ComRing.sort -> bool

val coq_ZF_of_word : wsize -> GRing.ComRing.sort -> bool

val nzcv_of_aluop : wsize -> GRing.ComRing.sort -> coq_Z -> coq_Z -> sem_tuple

val nzcv_w_of_aluop : wsize -> GRing.ComRing.sort -> coq_Z -> coq_Z -> ltuple

val drop_nz :
  (register, empty, empty, rflag, condt) instr_desc_t -> (register, empty,
  empty, rflag, condt) instr_desc_t

val drop_nzc :
  (register, empty, empty, rflag, condt) instr_desc_t -> (register, empty,
  empty, rflag, condt) instr_desc_t

val drop_nzcv :
  (register, empty, empty, rflag, condt) instr_desc_t -> (register, empty,
  empty, rflag, condt) instr_desc_t

val add_arguments :
  ltype list -> ltype list -> 'a1 sem_prod sem_prod -> 'a1 sem_prod

val mk_semi_cond :
  ltype list -> ltype list -> sem_tuple exec sem_prod -> sem_tuple exec
  sem_prod

val mk_cond :
  (register, empty, empty, rflag, condt) instr_desc_t -> (register, empty,
  empty, rflag, condt) instr_desc_t

val mk_semi1_shifted : shift_kind -> 'a1 exec sem_prod -> 'a1 exec sem_prod

val mk_semi2_2_shifted :
  ltype -> shift_kind -> 'a1 exec sem_prod -> 'a1 exec sem_prod

val mk_semi3_2_shifted :
  ltype -> ltype -> shift_kind -> 'a1 exec sem_prod -> 'a1 exec sem_prod

val mk_shifted :
  shift_kind -> (register, empty, empty, rflag, condt) instr_desc_t ->
  sem_tuple exec sem_prod -> (register, empty, empty, rflag, condt)
  instr_desc_t

val ak_reg_reg_imm_ : expected_wencoding -> arg_kind list list list

val ak_reg_reg_imm_shift : wsize -> shift_kind -> arg_kind list list list

val ak_reg_reg_reg_or_imm_ : expected_wencoding -> args_kinds list

val ak_reg_reg_reg_or_imm : arm_options -> expected_wencoding -> i_args_kinds

val ak_reg_imm_ : expected_wencoding -> arg_kind list list list

val ak_reg_reg_or_imm_ : expected_wencoding -> args_kinds list

val ak_reg_reg_or_imm : arm_options -> expected_wencoding -> i_args_kinds

val chk_imm_accept_shift : expected_wencoding

val chk_imm_accept_shift_w12 : arm_options -> expected_wencoding

val chk_imm_w16_encoding : bool -> expected_wencoding

val chk_imm_reject_shift : expected_wencoding

val pp_arm_op :
  arm_mnemonic -> arm_options -> (register, empty, empty, rflag, condt)
  asm_arg list -> (register, empty, empty, rflag, condt) pp_asm_op

val arm_ADD_semi : sem_tuple -> sem_tuple -> sem_tuple

val arm_ADD_instr :
  arm_options -> (register, empty, empty, rflag, condt) instr_desc_t

val arm_ADC_semi : sem_tuple -> sem_tuple -> bool -> sem_tuple

val arm_ADC_instr :
  arm_options -> (register, empty, empty, rflag, condt) instr_desc_t

val arm_MUL_semi : sem_tuple -> sem_tuple -> sem_tuple

val arm_high_registers : register list

val arm_MUL_instr :
  arm_options -> (register, empty, empty, rflag, condt) instr_desc_t

val arm_MLA_semi : sem_tuple -> sem_tuple -> sem_tuple -> sem_tuple

val arm_MLA_instr :
  arm_options -> (register, empty, empty, rflag, condt) instr_desc_t

val arm_MLS_semi : sem_tuple -> sem_tuple -> sem_tuple -> sem_tuple

val arm_MLS_instr :
  arm_options -> (register, empty, empty, rflag, condt) instr_desc_t

val arm_SDIV_semi : sem_tuple -> sem_tuple -> sem_tuple

val arm_SDIV_instr :
  arm_options -> (register, empty, empty, rflag, condt) instr_desc_t

val arm_SUB_semi : sem_tuple -> sem_tuple -> sem_tuple

val arm_SUB_instr :
  arm_options -> (register, empty, empty, rflag, condt) instr_desc_t

val arm_SBC_semi : sem_tuple -> sem_tuple -> bool -> sem_tuple

val arm_SBC_instr :
  arm_options -> (register, empty, empty, rflag, condt) instr_desc_t

val arm_RSB_instr :
  arm_options -> (register, empty, empty, rflag, condt) instr_desc_t

val arm_UDIV_semi : sem_tuple -> sem_tuple -> sem_tuple

val arm_UDIV_instr :
  arm_options -> (register, empty, empty, rflag, condt) instr_desc_t

val arm_UMULL_semi : sem_tuple -> sem_tuple -> sem_tuple

val arm_UMULL_instr :
  arm_options -> (register, empty, empty, rflag, condt) instr_desc_t

val arm_UMAAL_semi :
  sem_tuple -> sem_tuple -> sem_tuple -> sem_tuple -> sem_tuple

val arm_UMAAL_instr :
  arm_options -> (register, empty, empty, rflag, condt) instr_desc_t

val arm_UMLAL_semi :
  sem_tuple -> sem_tuple -> sem_tuple -> sem_tuple -> sem_tuple

val arm_UMLAL_instr :
  arm_options -> (register, empty, empty, rflag, condt) instr_desc_t

val arm_SMULL_semi : sem_tuple -> sem_tuple -> sem_tuple

val arm_SMULL_instr :
  arm_options -> (register, empty, empty, rflag, condt) instr_desc_t

val arm_SMLAL_semi :
  sem_tuple -> sem_tuple -> sem_tuple -> sem_tuple -> sem_tuple

val arm_SMLAL_instr :
  arm_options -> (register, empty, empty, rflag, condt) instr_desc_t

val arm_SMMUL_semi : sem_tuple -> sem_tuple -> sem_tuple

val arm_SMMUL_instr :
  arm_options -> (register, empty, empty, rflag, condt) instr_desc_t

val arm_SMMULR_semi : sem_tuple -> sem_tuple -> sem_tuple

val arm_SMMULR_instr :
  arm_options -> (register, empty, empty, rflag, condt) instr_desc_t

val get_hw :
  halfword -> (register, empty, empty, rflag, condt) wreg ->
  GRing.ComRing.sort

val arm_smul_hw_semi :
  halfword -> halfword -> (register, empty, empty, rflag, condt) wreg ->
  (register, empty, empty, rflag, condt) wreg -> (register, empty, empty,
  rflag, condt) wreg

val arm_smul_hw_instr :
  arm_options -> halfword -> halfword -> (register, empty, empty, rflag,
  condt) instr_desc_t

val arm_smla_hw_semi :
  halfword -> halfword -> (register, empty, empty, rflag, condt) wreg ->
  (register, empty, empty, rflag, condt) wreg -> (register, empty, empty,
  rflag, condt) wreg -> (register, empty, empty, rflag, condt) wreg

val arm_smla_hw_instr :
  arm_options -> halfword -> halfword -> (register, empty, empty, rflag,
  condt) instr_desc_t

val arm_smulw_hw_semi :
  halfword -> (register, empty, empty, rflag, condt) wreg -> (register,
  empty, empty, rflag, condt) wreg -> (register, empty, empty, rflag, condt)
  wreg

val arm_smulw_hw_instr :
  arm_options -> halfword -> (register, empty, empty, rflag, condt)
  instr_desc_t

val arm_bitwise_semi :
  wsize -> (GRing.ComRing.sort -> GRing.ComRing.sort) -> (GRing.ComRing.sort
  -> GRing.ComRing.sort) -> (GRing.ComRing.sort -> GRing.ComRing.sort ->
  GRing.ComRing.sort) -> sem_tuple -> sem_tuple -> sem_tuple

val arm_AND_instr :
  arm_options -> (register, empty, empty, rflag, condt) instr_desc_t

val arm_BFC_semi :
  (register, empty, empty, rflag, condt) wreg -> GRing.ComRing.sort ->
  GRing.ComRing.sort -> (register, empty, empty, rflag, condt) wreg exec

val arm_BFC_semi_sc : safe_cond list

val arm_BFC_instr :
  arm_options -> (register, empty, empty, rflag, condt) instr_desc_t

val arm_BFI_semi :
  (register, empty, empty, rflag, condt) wreg -> (register, empty, empty,
  rflag, condt) wreg -> GRing.ComRing.sort -> GRing.ComRing.sort ->
  (register, empty, empty, rflag, condt) wreg exec

val arm_BFI_semi_sc : safe_cond list

val arm_BFI_instr :
  arm_options -> (register, empty, empty, rflag, condt) instr_desc_t

val arm_BIC_instr :
  arm_options -> (register, empty, empty, rflag, condt) instr_desc_t

val arm_EOR_instr :
  arm_options -> (register, empty, empty, rflag, condt) instr_desc_t

val arm_MVN_semi : sem_tuple -> sem_tuple

val arm_MVN_instr :
  arm_options -> (register, empty, empty, rflag, condt) instr_desc_t

val arm_ORR_instr :
  arm_options -> (register, empty, empty, rflag, condt) instr_desc_t

val arm_shift_semi :
  (sem_tuple -> coq_Z -> sem_tuple) -> (sem_tuple -> coq_Z -> bool) ->
  sem_tuple -> GRing.ComRing.sort -> sem_tuple

val arm_ASR_C : sem_tuple -> coq_Z -> bool

val arm_ASR_semi : sem_tuple -> GRing.ComRing.sort -> sem_tuple

val arm_ASR_instr :
  arm_options -> (register, empty, empty, rflag, condt) instr_desc_t

val arm_LSL_C : sem_tuple -> coq_Z -> bool

val arm_LSL_semi : sem_tuple -> GRing.ComRing.sort -> sem_tuple

val arm_LSL_instr :
  arm_options -> (register, empty, empty, rflag, condt) instr_desc_t

val arm_LSR_C : sem_tuple -> coq_Z -> bool

val arm_LSR_semi : sem_tuple -> GRing.ComRing.sort -> sem_tuple

val arm_LSR_instr :
  arm_options -> (register, empty, empty, rflag, condt) instr_desc_t

val arm_ROR_C : sem_tuple -> coq_Z -> bool

val arm_ROR_semi : sem_tuple -> GRing.ComRing.sort -> sem_tuple

val arm_ROR_instr :
  arm_options -> (register, empty, empty, rflag, condt) instr_desc_t

val mk_rev_instr :
  arm_options -> arm_mnemonic -> sem_tuple sem_prod -> (register, empty,
  empty, rflag, condt) instr_desc_t

val arm_REV_semi : sem_tuple -> sem_tuple

val arm_REV16_semi : sem_tuple -> sem_tuple

val arm_REVSH_semi : sem_tuple -> sem_tuple

val arm_REV_instr :
  arm_options -> (register, empty, empty, rflag, condt) instr_desc_t

val arm_REV16_instr :
  arm_options -> (register, empty, empty, rflag, condt) instr_desc_t

val arm_REVSH_instr :
  arm_options -> (register, empty, empty, rflag, condt) instr_desc_t

val arm_ADR_semi : sem_tuple -> sem_tuple

val arm_ADR_instr :
  arm_options -> (register, empty, empty, rflag, condt) instr_desc_t

val arm_MOV_semi : sem_tuple -> sem_tuple

val arm_MOV_instr :
  arm_options -> (register, empty, empty, rflag, condt) instr_desc_t

val arm_MOVT_semi : sem_tuple -> GRing.ComRing.sort -> sem_tuple

val arm_MOVT_instr :
  arm_options -> (register, empty, empty, rflag, condt) instr_desc_t

val bit_field_extract_semi :
  ((register, empty, empty, rflag, condt) wreg -> coq_Z -> (register, empty,
  empty, rflag, condt) wreg) -> (register, empty, empty, rflag, condt) wreg
  -> GRing.ComRing.sort -> GRing.ComRing.sort -> (register, empty, empty,
  rflag, condt) wreg exec

val bit_field_extract_semi_sc : safe_cond list

val ak_reg_reg_imm_imm_extr : arg_kind list list list

val arm_UBFX_instr :
  arm_options -> (register, empty, empty, rflag, condt) instr_desc_t

val extend_bits_semi :
  coq_Z -> (register, empty, empty, rflag, condt) wreg -> GRing.ComRing.sort
  -> (register, empty, empty, rflag, condt) wreg

val ak_reg_reg_imm8_0_8_16_24 : arg_kind list list list

val arm_UXTB_instr :
  arm_options -> (register, empty, empty, rflag, condt) instr_desc_t

val arm_UXTH_instr :
  arm_options -> (register, empty, empty, rflag, condt) instr_desc_t

val arm_SBFX_instr :
  arm_options -> (register, empty, empty, rflag, condt) instr_desc_t

val arm_CMP_semi : sem_tuple -> sem_tuple -> sem_tuple

val arm_CMP_instr :
  arm_options -> (register, empty, empty, rflag, condt) instr_desc_t

val arm_TST_semi : sem_tuple -> sem_tuple -> sem_tuple

val arm_TST_instr :
  arm_options -> (register, empty, empty, rflag, condt) instr_desc_t

val arm_CMN_instr :
  arm_options -> (register, empty, empty, rflag, condt) instr_desc_t

val arm_extend_semi :
  wsize -> bool -> wsize -> GRing.ComRing.sort -> GRing.ComRing.sort

val arm_load_instr :
  arm_options -> arm_mnemonic -> (register, empty, empty, rflag, condt)
  instr_desc_t

val arm_store_instr :
  arm_options -> arm_mnemonic -> (register, empty, empty, rflag, condt)
  instr_desc_t

val arm_CLZ_instr :
  arm_options -> (register, empty, empty, rflag, condt) instr_desc_t

val mn_desc :
  arm_options -> arm_mnemonic -> (register, empty, empty, rflag, condt)
  instr_desc_t

val arm_instr_desc :
  arm_op -> (register, empty, empty, rflag, condt) instr_desc_t

val arm_prim_string : (string * arm_op prim_constructor) list

val arm_op_decl : (register, empty, empty, rflag, condt, arm_op) asm_op_decl

type arm_prog = (register, empty, empty, rflag, condt, arm_op) asm_prog

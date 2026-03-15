open BinInt
open BinNums
open Bool
open Arch_decl
open Arch_utils
open Arm_expand_imm
open EqbOK
open Eqb_core_defs
open Eqtype
open Expr
open Fintype
open Flag_combination
open Seq
open Shift_kind
open Ssralg
open Ssrbool
open Type
open Utils0
open Word0
open Word_ssrZ
open Wsize

type __ = Obj.t

val arm_reg_size : wsize

val arm_xreg_size : wsize

type register =
| R00
| R01
| R02
| R03
| R04
| R05
| R06
| R07
| R08
| R09
| R10
| R11
| R12
| LR
| SP

val register_rect :
  'a1 -> 'a1 -> 'a1 -> 'a1 -> 'a1 -> 'a1 -> 'a1 -> 'a1 -> 'a1 -> 'a1 -> 'a1
  -> 'a1 -> 'a1 -> 'a1 -> 'a1 -> register -> 'a1

val register_rec :
  'a1 -> 'a1 -> 'a1 -> 'a1 -> 'a1 -> 'a1 -> 'a1 -> 'a1 -> 'a1 -> 'a1 -> 'a1
  -> 'a1 -> 'a1 -> 'a1 -> 'a1 -> register -> 'a1

type is_register =
| Coq_is_R00
| Coq_is_R01
| Coq_is_R02
| Coq_is_R03
| Coq_is_R04
| Coq_is_R05
| Coq_is_R06
| Coq_is_R07
| Coq_is_R08
| Coq_is_R09
| Coq_is_R10
| Coq_is_R11
| Coq_is_R12
| Coq_is_LR
| Coq_is_SP

val is_register_rect :
  'a1 -> 'a1 -> 'a1 -> 'a1 -> 'a1 -> 'a1 -> 'a1 -> 'a1 -> 'a1 -> 'a1 -> 'a1
  -> 'a1 -> 'a1 -> 'a1 -> 'a1 -> register -> is_register -> 'a1

val is_register_rec :
  'a1 -> 'a1 -> 'a1 -> 'a1 -> 'a1 -> 'a1 -> 'a1 -> 'a1 -> 'a1 -> 'a1 -> 'a1
  -> 'a1 -> 'a1 -> 'a1 -> 'a1 -> register -> is_register -> 'a1

val register_tag : register -> positive

val is_register_inhab : register -> is_register

val is_register_functor : register -> is_register -> is_register

type box_register_R00 =
| Box_register_R00

type register_fields_t = __

val register_fields : register -> register_fields_t

val register_construct : positive -> register_fields_t -> register option

val register_induction :
  'a1 -> 'a1 -> 'a1 -> 'a1 -> 'a1 -> 'a1 -> 'a1 -> 'a1 -> 'a1 -> 'a1 -> 'a1
  -> 'a1 -> 'a1 -> 'a1 -> 'a1 -> register -> is_register -> 'a1

val register_eqb_fields :
  (register -> register -> bool) -> positive -> register_fields_t ->
  register_fields_t -> bool

val register_eqb : register -> register -> bool

val register_eqb_OK : register -> register -> reflect

val register_eqb_OK_sumbool : register -> register -> bool

val eqTC_register : register eqTypeC

val arm_register_eqType : Equality.coq_type

val registers : register list

val finTC_register : register finTypeC

val register_finType : Finite.coq_type

val register_to_string : register -> string

val reg_toS : register coq_ToString

type rflag =
| NF
| ZF
| CF
| VF

val rflag_rect : 'a1 -> 'a1 -> 'a1 -> 'a1 -> rflag -> 'a1

val rflag_rec : 'a1 -> 'a1 -> 'a1 -> 'a1 -> rflag -> 'a1

type is_rflag =
| Coq_is_NF
| Coq_is_ZF
| Coq_is_CF
| Coq_is_VF

val is_rflag_rect : 'a1 -> 'a1 -> 'a1 -> 'a1 -> rflag -> is_rflag -> 'a1

val is_rflag_rec : 'a1 -> 'a1 -> 'a1 -> 'a1 -> rflag -> is_rflag -> 'a1

val rflag_tag : rflag -> positive

val is_rflag_inhab : rflag -> is_rflag

val is_rflag_functor : rflag -> is_rflag -> is_rflag

type box_rflag_NF =
| Box_rflag_NF

type rflag_fields_t = __

val rflag_fields : rflag -> rflag_fields_t

val rflag_construct : positive -> rflag_fields_t -> rflag option

val rflag_induction : 'a1 -> 'a1 -> 'a1 -> 'a1 -> rflag -> is_rflag -> 'a1

val rflag_eqb_fields :
  (rflag -> rflag -> bool) -> positive -> rflag_fields_t -> rflag_fields_t ->
  bool

val rflag_eqb : rflag -> rflag -> bool

val rflag_eqb_OK : rflag -> rflag -> reflect

val rflag_eqb_OK_sumbool : rflag -> rflag -> bool

val eqTC_rflag : rflag eqTypeC

val rflag_eqType : Equality.coq_type

val rflags : rflag list

val finTC_rflag : rflag finTypeC

val rflag_finType : Finite.coq_type

val flag_to_string : rflag -> string

val rflag_toS : rflag coq_ToString

type condt =
| EQ_ct
| NE_ct
| CS_ct
| CC_ct
| MI_ct
| PL_ct
| VS_ct
| VC_ct
| HI_ct
| LS_ct
| GE_ct
| LT_ct
| GT_ct
| LE_ct

val condt_rect :
  'a1 -> 'a1 -> 'a1 -> 'a1 -> 'a1 -> 'a1 -> 'a1 -> 'a1 -> 'a1 -> 'a1 -> 'a1
  -> 'a1 -> 'a1 -> 'a1 -> condt -> 'a1

val condt_rec :
  'a1 -> 'a1 -> 'a1 -> 'a1 -> 'a1 -> 'a1 -> 'a1 -> 'a1 -> 'a1 -> 'a1 -> 'a1
  -> 'a1 -> 'a1 -> 'a1 -> condt -> 'a1

type is_condt =
| Coq_is_EQ_ct
| Coq_is_NE_ct
| Coq_is_CS_ct
| Coq_is_CC_ct
| Coq_is_MI_ct
| Coq_is_PL_ct
| Coq_is_VS_ct
| Coq_is_VC_ct
| Coq_is_HI_ct
| Coq_is_LS_ct
| Coq_is_GE_ct
| Coq_is_LT_ct
| Coq_is_GT_ct
| Coq_is_LE_ct

val is_condt_rect :
  'a1 -> 'a1 -> 'a1 -> 'a1 -> 'a1 -> 'a1 -> 'a1 -> 'a1 -> 'a1 -> 'a1 -> 'a1
  -> 'a1 -> 'a1 -> 'a1 -> condt -> is_condt -> 'a1

val is_condt_rec :
  'a1 -> 'a1 -> 'a1 -> 'a1 -> 'a1 -> 'a1 -> 'a1 -> 'a1 -> 'a1 -> 'a1 -> 'a1
  -> 'a1 -> 'a1 -> 'a1 -> condt -> is_condt -> 'a1

val condt_tag : condt -> positive

val is_condt_inhab : condt -> is_condt

val is_condt_functor : condt -> is_condt -> is_condt

type box_condt_EQ_ct =
| Box_condt_EQ_ct

type condt_fields_t = __

val condt_fields : condt -> condt_fields_t

val condt_construct : positive -> condt_fields_t -> condt option

val condt_induction :
  'a1 -> 'a1 -> 'a1 -> 'a1 -> 'a1 -> 'a1 -> 'a1 -> 'a1 -> 'a1 -> 'a1 -> 'a1
  -> 'a1 -> 'a1 -> 'a1 -> condt -> is_condt -> 'a1

val condt_eqb_fields :
  (condt -> condt -> bool) -> positive -> condt_fields_t -> condt_fields_t ->
  bool

val condt_eqb : condt -> condt -> bool

val condt_eqb_OK : condt -> condt -> reflect

val condt_eqb_OK_sumbool : condt -> condt -> bool

val eqTC_condt : condt eqTypeC

val condt_eqType : Equality.coq_type

val condts : condt list

val finTC_condt : condt finTypeC

val condt_finType : Finite.coq_type

val string_of_condt : condt -> string

val eqTC_shift_kind : shift_kind eqTypeC

val shift_kind_eqType : Equality.coq_type

val shift_kinds : shift_kind list

val string_of_shift_kind : shift_kind -> string

val check_shift_amount : shift_kind -> coq_Z -> bool

val shift_op :
  shift_kind -> wsize -> GRing.ComRing.sort -> coq_Z -> GRing.ComRing.sort

val shift_of_sop2 : wsize -> sop2 -> shift_kind option

val arm_fc_of_cfc : combine_flags_core -> flag_combination

val arm_fcp : coq_FlagCombinationParams

val arm_check_CAimm : caimm_checker_s -> wsize -> GRing.ComRing.sort -> bool

val arm_decl : (register, empty, empty, rflag, condt) arch_decl

val arm_linux_call_conv :
  (register, empty, empty, rflag, condt) calling_convention

val is_expandable : coq_Z -> bool

val is_expandable_or_shift : coq_Z -> bool

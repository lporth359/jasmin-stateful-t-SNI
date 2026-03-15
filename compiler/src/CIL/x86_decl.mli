open BinNums
open Bool
open Arch_decl
open EqbOK
open Eqb_core_defs
open Eqtype
open Fintype
open Flag_combination
open Seq
open Ssralg
open Utils0
open Wsize

type __ = Obj.t

val x86_reg_size : wsize

val x86_xreg_size : wsize

type register =
| RAX
| RCX
| RDX
| RBX
| RSP
| RBP
| RSI
| RDI
| R8
| R9
| R10
| R11
| R12
| R13
| R14
| R15

val register_rect :
  'a1 -> 'a1 -> 'a1 -> 'a1 -> 'a1 -> 'a1 -> 'a1 -> 'a1 -> 'a1 -> 'a1 -> 'a1
  -> 'a1 -> 'a1 -> 'a1 -> 'a1 -> 'a1 -> register -> 'a1

val register_rec :
  'a1 -> 'a1 -> 'a1 -> 'a1 -> 'a1 -> 'a1 -> 'a1 -> 'a1 -> 'a1 -> 'a1 -> 'a1
  -> 'a1 -> 'a1 -> 'a1 -> 'a1 -> 'a1 -> register -> 'a1

type is_register =
| Coq_is_RAX
| Coq_is_RCX
| Coq_is_RDX
| Coq_is_RBX
| Coq_is_RSP
| Coq_is_RBP
| Coq_is_RSI
| Coq_is_RDI
| Coq_is_R8
| Coq_is_R9
| Coq_is_R10
| Coq_is_R11
| Coq_is_R12
| Coq_is_R13
| Coq_is_R14
| Coq_is_R15

val is_register_rect :
  'a1 -> 'a1 -> 'a1 -> 'a1 -> 'a1 -> 'a1 -> 'a1 -> 'a1 -> 'a1 -> 'a1 -> 'a1
  -> 'a1 -> 'a1 -> 'a1 -> 'a1 -> 'a1 -> register -> is_register -> 'a1

val is_register_rec :
  'a1 -> 'a1 -> 'a1 -> 'a1 -> 'a1 -> 'a1 -> 'a1 -> 'a1 -> 'a1 -> 'a1 -> 'a1
  -> 'a1 -> 'a1 -> 'a1 -> 'a1 -> 'a1 -> register -> is_register -> 'a1

val register_tag : register -> positive

val is_register_inhab : register -> is_register

val is_register_functor : register -> is_register -> is_register

type box_register_RAX =
| Box_register_RAX

type register_fields_t = __

val register_fields : register -> register_fields_t

val register_construct : positive -> register_fields_t -> register option

val register_induction :
  'a1 -> 'a1 -> 'a1 -> 'a1 -> 'a1 -> 'a1 -> 'a1 -> 'a1 -> 'a1 -> 'a1 -> 'a1
  -> 'a1 -> 'a1 -> 'a1 -> 'a1 -> 'a1 -> register -> is_register -> 'a1

val register_eqb_fields :
  (register -> register -> bool) -> positive -> register_fields_t ->
  register_fields_t -> bool

val register_eqb : register -> register -> bool

val register_eqb_OK : register -> register -> reflect

val register_eqb_OK_sumbool : register -> register -> bool

type register_ext =
| MM0
| MM1
| MM2
| MM3
| MM4
| MM5
| MM6
| MM7

val register_ext_rect :
  'a1 -> 'a1 -> 'a1 -> 'a1 -> 'a1 -> 'a1 -> 'a1 -> 'a1 -> register_ext -> 'a1

val register_ext_rec :
  'a1 -> 'a1 -> 'a1 -> 'a1 -> 'a1 -> 'a1 -> 'a1 -> 'a1 -> register_ext -> 'a1

type is_register_ext =
| Coq_is_MM0
| Coq_is_MM1
| Coq_is_MM2
| Coq_is_MM3
| Coq_is_MM4
| Coq_is_MM5
| Coq_is_MM6
| Coq_is_MM7

val is_register_ext_rect :
  'a1 -> 'a1 -> 'a1 -> 'a1 -> 'a1 -> 'a1 -> 'a1 -> 'a1 -> register_ext ->
  is_register_ext -> 'a1

val is_register_ext_rec :
  'a1 -> 'a1 -> 'a1 -> 'a1 -> 'a1 -> 'a1 -> 'a1 -> 'a1 -> register_ext ->
  is_register_ext -> 'a1

val register_ext_tag : register_ext -> positive

val is_register_ext_inhab : register_ext -> is_register_ext

val is_register_ext_functor :
  register_ext -> is_register_ext -> is_register_ext

type box_register_ext_MM0 =
| Box_register_ext_MM0

type register_ext_fields_t = __

val register_ext_fields : register_ext -> register_ext_fields_t

val register_ext_construct :
  positive -> register_ext_fields_t -> register_ext option

val register_ext_induction :
  'a1 -> 'a1 -> 'a1 -> 'a1 -> 'a1 -> 'a1 -> 'a1 -> 'a1 -> register_ext ->
  is_register_ext -> 'a1

val register_ext_eqb_fields :
  (register_ext -> register_ext -> bool) -> positive -> register_ext_fields_t
  -> register_ext_fields_t -> bool

val register_ext_eqb : register_ext -> register_ext -> bool

val register_ext_eqb_OK : register_ext -> register_ext -> reflect

val register_ext_eqb_OK_sumbool : register_ext -> register_ext -> bool

type xmm_register =
| XMM0
| XMM1
| XMM2
| XMM3
| XMM4
| XMM5
| XMM6
| XMM7
| XMM8
| XMM9
| XMM10
| XMM11
| XMM12
| XMM13
| XMM14
| XMM15

val xmm_register_rect :
  'a1 -> 'a1 -> 'a1 -> 'a1 -> 'a1 -> 'a1 -> 'a1 -> 'a1 -> 'a1 -> 'a1 -> 'a1
  -> 'a1 -> 'a1 -> 'a1 -> 'a1 -> 'a1 -> xmm_register -> 'a1

val xmm_register_rec :
  'a1 -> 'a1 -> 'a1 -> 'a1 -> 'a1 -> 'a1 -> 'a1 -> 'a1 -> 'a1 -> 'a1 -> 'a1
  -> 'a1 -> 'a1 -> 'a1 -> 'a1 -> 'a1 -> xmm_register -> 'a1

type is_xmm_register =
| Coq_is_XMM0
| Coq_is_XMM1
| Coq_is_XMM2
| Coq_is_XMM3
| Coq_is_XMM4
| Coq_is_XMM5
| Coq_is_XMM6
| Coq_is_XMM7
| Coq_is_XMM8
| Coq_is_XMM9
| Coq_is_XMM10
| Coq_is_XMM11
| Coq_is_XMM12
| Coq_is_XMM13
| Coq_is_XMM14
| Coq_is_XMM15

val is_xmm_register_rect :
  'a1 -> 'a1 -> 'a1 -> 'a1 -> 'a1 -> 'a1 -> 'a1 -> 'a1 -> 'a1 -> 'a1 -> 'a1
  -> 'a1 -> 'a1 -> 'a1 -> 'a1 -> 'a1 -> xmm_register -> is_xmm_register -> 'a1

val is_xmm_register_rec :
  'a1 -> 'a1 -> 'a1 -> 'a1 -> 'a1 -> 'a1 -> 'a1 -> 'a1 -> 'a1 -> 'a1 -> 'a1
  -> 'a1 -> 'a1 -> 'a1 -> 'a1 -> 'a1 -> xmm_register -> is_xmm_register -> 'a1

val xmm_register_tag : xmm_register -> positive

val is_xmm_register_inhab : xmm_register -> is_xmm_register

val is_xmm_register_functor :
  xmm_register -> is_xmm_register -> is_xmm_register

type box_xmm_register_XMM0 =
| Box_xmm_register_XMM0

type xmm_register_fields_t = __

val xmm_register_fields : xmm_register -> xmm_register_fields_t

val xmm_register_construct :
  positive -> xmm_register_fields_t -> xmm_register option

val xmm_register_induction :
  'a1 -> 'a1 -> 'a1 -> 'a1 -> 'a1 -> 'a1 -> 'a1 -> 'a1 -> 'a1 -> 'a1 -> 'a1
  -> 'a1 -> 'a1 -> 'a1 -> 'a1 -> 'a1 -> xmm_register -> is_xmm_register -> 'a1

val xmm_register_eqb_fields :
  (xmm_register -> xmm_register -> bool) -> positive -> xmm_register_fields_t
  -> xmm_register_fields_t -> bool

val xmm_register_eqb : xmm_register -> xmm_register -> bool

val xmm_register_eqb_OK : xmm_register -> xmm_register -> reflect

val xmm_register_eqb_OK_sumbool : xmm_register -> xmm_register -> bool

type rflag =
| CF
| PF
| ZF
| SF
| OF

val rflag_rect : 'a1 -> 'a1 -> 'a1 -> 'a1 -> 'a1 -> rflag -> 'a1

val rflag_rec : 'a1 -> 'a1 -> 'a1 -> 'a1 -> 'a1 -> rflag -> 'a1

type is_rflag =
| Coq_is_CF
| Coq_is_PF
| Coq_is_ZF
| Coq_is_SF
| Coq_is_OF

val is_rflag_rect :
  'a1 -> 'a1 -> 'a1 -> 'a1 -> 'a1 -> rflag -> is_rflag -> 'a1

val is_rflag_rec : 'a1 -> 'a1 -> 'a1 -> 'a1 -> 'a1 -> rflag -> is_rflag -> 'a1

val rflag_tag : rflag -> positive

val is_rflag_inhab : rflag -> is_rflag

val is_rflag_functor : rflag -> is_rflag -> is_rflag

type box_rflag_CF =
| Box_rflag_CF

type rflag_fields_t = __

val rflag_fields : rflag -> rflag_fields_t

val rflag_construct : positive -> rflag_fields_t -> rflag option

val rflag_induction :
  'a1 -> 'a1 -> 'a1 -> 'a1 -> 'a1 -> rflag -> is_rflag -> 'a1

val rflag_eqb_fields :
  (rflag -> rflag -> bool) -> positive -> rflag_fields_t -> rflag_fields_t ->
  bool

val rflag_eqb : rflag -> rflag -> bool

val rflag_eqb_OK : rflag -> rflag -> reflect

val rflag_eqb_OK_sumbool : rflag -> rflag -> bool

type condt =
| O_ct
| NO_ct
| B_ct
| NB_ct
| E_ct
| NE_ct
| BE_ct
| NBE_ct
| S_ct
| NS_ct
| P_ct
| NP_ct
| L_ct
| NL_ct
| LE_ct
| NLE_ct

val condt_rect :
  'a1 -> 'a1 -> 'a1 -> 'a1 -> 'a1 -> 'a1 -> 'a1 -> 'a1 -> 'a1 -> 'a1 -> 'a1
  -> 'a1 -> 'a1 -> 'a1 -> 'a1 -> 'a1 -> condt -> 'a1

val condt_rec :
  'a1 -> 'a1 -> 'a1 -> 'a1 -> 'a1 -> 'a1 -> 'a1 -> 'a1 -> 'a1 -> 'a1 -> 'a1
  -> 'a1 -> 'a1 -> 'a1 -> 'a1 -> 'a1 -> condt -> 'a1

type is_condt =
| Coq_is_O_ct
| Coq_is_NO_ct
| Coq_is_B_ct
| Coq_is_NB_ct
| Coq_is_E_ct
| Coq_is_NE_ct
| Coq_is_BE_ct
| Coq_is_NBE_ct
| Coq_is_S_ct
| Coq_is_NS_ct
| Coq_is_P_ct
| Coq_is_NP_ct
| Coq_is_L_ct
| Coq_is_NL_ct
| Coq_is_LE_ct
| Coq_is_NLE_ct

val is_condt_rect :
  'a1 -> 'a1 -> 'a1 -> 'a1 -> 'a1 -> 'a1 -> 'a1 -> 'a1 -> 'a1 -> 'a1 -> 'a1
  -> 'a1 -> 'a1 -> 'a1 -> 'a1 -> 'a1 -> condt -> is_condt -> 'a1

val is_condt_rec :
  'a1 -> 'a1 -> 'a1 -> 'a1 -> 'a1 -> 'a1 -> 'a1 -> 'a1 -> 'a1 -> 'a1 -> 'a1
  -> 'a1 -> 'a1 -> 'a1 -> 'a1 -> 'a1 -> condt -> is_condt -> 'a1

val condt_tag : condt -> positive

val is_condt_inhab : condt -> is_condt

val is_condt_functor : condt -> is_condt -> is_condt

type box_condt_O_ct =
| Box_condt_O_ct

type condt_fields_t = __

val condt_fields : condt -> condt_fields_t

val condt_construct : positive -> condt_fields_t -> condt option

val condt_induction :
  'a1 -> 'a1 -> 'a1 -> 'a1 -> 'a1 -> 'a1 -> 'a1 -> 'a1 -> 'a1 -> 'a1 -> 'a1
  -> 'a1 -> 'a1 -> 'a1 -> 'a1 -> 'a1 -> condt -> is_condt -> 'a1

val condt_eqb_fields :
  (condt -> condt -> bool) -> positive -> condt_fields_t -> condt_fields_t ->
  bool

val condt_eqb : condt -> condt -> bool

val condt_eqb_OK : condt -> condt -> reflect

val condt_eqb_OK_sumbool : condt -> condt -> bool

val coq_HB_unnamed_factory_1 : register Coq_hasDecEq.axioms_

val x86_decl_register__canonical__eqtype_Equality : Equality.coq_type

val coq_HB_unnamed_factory_3 : register_ext Coq_hasDecEq.axioms_

val x86_decl_register_ext__canonical__eqtype_Equality : Equality.coq_type

val coq_HB_unnamed_factory_5 : xmm_register Coq_hasDecEq.axioms_

val x86_decl_xmm_register__canonical__eqtype_Equality : Equality.coq_type

val coq_HB_unnamed_factory_7 : rflag Coq_hasDecEq.axioms_

val x86_decl_rflag__canonical__eqtype_Equality : Equality.coq_type

val coq_HB_unnamed_factory_9 : condt Coq_hasDecEq.axioms_

val x86_decl_condt__canonical__eqtype_Equality : Equality.coq_type

val registers : register list

val coq_HB_unnamed_factory_11 : register Choice.Countable.axioms_

val choice_Countable__to__choice_hasChoice :
  register Choice.Coq_hasChoice.axioms_

val choice_Countable__to__choice_Choice_isCountable :
  register Choice.Choice_isCountable.axioms_

val coq_HB_unnamed_mixin_15 : register Choice.Coq_hasChoice.axioms_

val x86_decl_register__canonical__choice_Choice : Choice.Choice.coq_type

val coq_HB_unnamed_mixin_16 : register Choice.Choice_isCountable.axioms_

val x86_decl_register__canonical__choice_Countable : Choice.Countable.coq_type

val coq_HB_unnamed_factory_17 : register Coq_isFinite.axioms_

val x86_decl_register__canonical__fintype_Finite : Finite.coq_type

val regxs : register_ext list

val coq_HB_unnamed_factory_19 : register_ext Choice.Countable.axioms_

val choice_Countable__to__choice_hasChoice__21 :
  register_ext Choice.Coq_hasChoice.axioms_

val choice_Countable__to__choice_Choice_isCountable__24 :
  register_ext Choice.Choice_isCountable.axioms_

val coq_HB_unnamed_mixin_25 : register_ext Choice.Coq_hasChoice.axioms_

val x86_decl_register_ext__canonical__choice_Choice : Choice.Choice.coq_type

val coq_HB_unnamed_mixin_26 : register_ext Choice.Choice_isCountable.axioms_

val x86_decl_register_ext__canonical__choice_Countable :
  Choice.Countable.coq_type

val coq_HB_unnamed_factory_27 : register_ext Coq_isFinite.axioms_

val x86_decl_register_ext__canonical__fintype_Finite : Finite.coq_type

val xmm_registers : xmm_register list

val coq_HB_unnamed_factory_29 : xmm_register Choice.Countable.axioms_

val choice_Countable__to__choice_hasChoice__31 :
  xmm_register Choice.Coq_hasChoice.axioms_

val choice_Countable__to__choice_Choice_isCountable__34 :
  xmm_register Choice.Choice_isCountable.axioms_

val coq_HB_unnamed_mixin_35 : xmm_register Choice.Coq_hasChoice.axioms_

val x86_decl_xmm_register__canonical__choice_Choice : Choice.Choice.coq_type

val coq_HB_unnamed_mixin_36 : xmm_register Choice.Choice_isCountable.axioms_

val x86_decl_xmm_register__canonical__choice_Countable :
  Choice.Countable.coq_type

val coq_HB_unnamed_factory_37 : xmm_register Coq_isFinite.axioms_

val x86_decl_xmm_register__canonical__fintype_Finite : Finite.coq_type

val rflags : rflag list

val coq_HB_unnamed_factory_39 : rflag Choice.Countable.axioms_

val choice_Countable__to__choice_hasChoice__41 :
  rflag Choice.Coq_hasChoice.axioms_

val choice_Countable__to__choice_Choice_isCountable__44 :
  rflag Choice.Choice_isCountable.axioms_

val coq_HB_unnamed_mixin_45 : rflag Choice.Coq_hasChoice.axioms_

val x86_decl_rflag__canonical__choice_Choice : Choice.Choice.coq_type

val coq_HB_unnamed_mixin_46 : rflag Choice.Choice_isCountable.axioms_

val x86_decl_rflag__canonical__choice_Countable : Choice.Countable.coq_type

val coq_HB_unnamed_factory_47 : rflag Coq_isFinite.axioms_

val x86_decl_rflag__canonical__fintype_Finite : Finite.coq_type

val eqTC_register : register eqTypeC

val finC_register : register finTypeC

val register_to_string : register -> string

val x86_reg_toS : register coq_ToString

val eqTC_regx : register_ext eqTypeC

val finC_regx : register_ext finTypeC

val regx_to_string : register_ext -> string

val x86_regx_toS : register_ext coq_ToString

val eqTC_xmm_register : xmm_register eqTypeC

val finC_xmm_register : xmm_register finTypeC

val xreg_to_string : xmm_register -> string

val x86_xreg_toS : xmm_register coq_ToString

val eqTC_rflag : rflag eqTypeC

val finC_rflag : rflag finTypeC

val rflag_to_string : rflag -> string

val x86_rflag_toS : rflag coq_ToString

val eqC_condt : condt eqTypeC

val x86_fc_of_cfc : combine_flags_core -> flag_combination

val x86_fcp : coq_FlagCombinationParams

val x86_check_CAimm : caimm_checker_s -> wsize -> GRing.ComRing.sort -> bool

val x86_decl : (register, register_ext, xmm_register, rflag, condt) arch_decl

val x86_linux_call_conv :
  (register, register_ext, xmm_register, rflag, condt) calling_convention

val x86_windows_call_conv :
  (register, register_ext, xmm_register, rflag, condt) calling_convention

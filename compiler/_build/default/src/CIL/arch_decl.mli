open BinNums
open Bool
open Datatypes
open Prelude
open Arm_expand_imm
open EqbOK
open Eqb_core_defs
open Eqtype
open Expr
open Flag_combination
open Label
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
open Type
open Utils0
open Var0
open Word0
open Word_ssrZ
open Wsize

type __ = Obj.t

type 't coq_ToString = { category : string; _finC : 't finTypeC;
                         to_string : ('t -> string) }

val category : ltype -> 'a1 coq_ToString -> string

val _finC : ltype -> 'a1 coq_ToString -> 'a1 finTypeC

val to_string : ltype -> 'a1 coq_ToString -> 'a1 -> string

val rtype : ltype -> 'a1 coq_ToString -> ltype

type caimm_checker_s =
| CAimmC_none
| CAimmC_arm_shift_amout of shift_kind
| CAimmC_arm_wencoding of expected_wencoding
| CAimmC_arm_0_8_16_24
| CAimmC_riscv_12bits_signed
| CAimmC_riscv_5bits_unsigned

val caimm_checker_s_rect :
  'a1 -> (shift_kind -> 'a1) -> (expected_wencoding -> 'a1) -> 'a1 -> 'a1 ->
  'a1 -> caimm_checker_s -> 'a1

val caimm_checker_s_rec :
  'a1 -> (shift_kind -> 'a1) -> (expected_wencoding -> 'a1) -> 'a1 -> 'a1 ->
  'a1 -> caimm_checker_s -> 'a1

type is_caimm_checker_s =
| Coq_is_CAimmC_none
| Coq_is_CAimmC_arm_shift_amout of shift_kind * is_shift_kind
| Coq_is_CAimmC_arm_wencoding of expected_wencoding * is_expected_wencoding
| Coq_is_CAimmC_arm_0_8_16_24
| Coq_is_CAimmC_riscv_12bits_signed
| Coq_is_CAimmC_riscv_5bits_unsigned

val is_caimm_checker_s_rect :
  'a1 -> (shift_kind -> is_shift_kind -> 'a1) -> (expected_wencoding ->
  is_expected_wencoding -> 'a1) -> 'a1 -> 'a1 -> 'a1 -> caimm_checker_s ->
  is_caimm_checker_s -> 'a1

val is_caimm_checker_s_rec :
  'a1 -> (shift_kind -> is_shift_kind -> 'a1) -> (expected_wencoding ->
  is_expected_wencoding -> 'a1) -> 'a1 -> 'a1 -> 'a1 -> caimm_checker_s ->
  is_caimm_checker_s -> 'a1

val caimm_checker_s_tag : caimm_checker_s -> positive

val is_caimm_checker_s_inhab : caimm_checker_s -> is_caimm_checker_s

val is_caimm_checker_s_functor :
  caimm_checker_s -> is_caimm_checker_s -> is_caimm_checker_s

type box_caimm_checker_s_CAimmC_none =
| Box_caimm_checker_s_CAimmC_none

type box_caimm_checker_s_CAimmC_arm_shift_amout =
  shift_kind
  (* singleton inductive, whose constructor was Box_caimm_checker_s_CAimmC_arm_shift_amout *)

val coq_Box_caimm_checker_s_CAimmC_arm_shift_amout_0 :
  box_caimm_checker_s_CAimmC_arm_shift_amout -> shift_kind

type box_caimm_checker_s_CAimmC_arm_wencoding =
  expected_wencoding
  (* singleton inductive, whose constructor was Box_caimm_checker_s_CAimmC_arm_wencoding *)

val coq_Box_caimm_checker_s_CAimmC_arm_wencoding_0 :
  box_caimm_checker_s_CAimmC_arm_wencoding -> expected_wencoding

type caimm_checker_s_fields_t = __

val caimm_checker_s_fields : caimm_checker_s -> caimm_checker_s_fields_t

val caimm_checker_s_construct :
  positive -> caimm_checker_s_fields_t -> caimm_checker_s option

val caimm_checker_s_induction :
  'a1 -> (shift_kind -> is_shift_kind -> 'a1) -> (expected_wencoding ->
  is_expected_wencoding -> 'a1) -> 'a1 -> 'a1 -> 'a1 -> caimm_checker_s ->
  is_caimm_checker_s -> 'a1

val caimm_checker_s_eqb_fields :
  (caimm_checker_s -> caimm_checker_s -> bool) -> positive ->
  caimm_checker_s_fields_t -> caimm_checker_s_fields_t -> bool

val caimm_checker_s_eqb : caimm_checker_s -> caimm_checker_s -> bool

val caimm_checker_s_eqb_OK : caimm_checker_s -> caimm_checker_s -> reflect

val caimm_checker_s_eqb_OK_sumbool :
  caimm_checker_s -> caimm_checker_s -> bool

val coq_HB_unnamed_factory_1 : caimm_checker_s Coq_hasDecEq.axioms_

val arch_decl_caimm_checker_s__canonical__eqtype_Equality : Equality.coq_type

type ('reg, 'regx, 'xreg, 'rflag, 'cond) arch_decl = { reg_size : wsize;
                                                       xreg_size : wsize;
                                                       cond_eqC : 'cond
                                                                  eqTypeC;
                                                       toS_r : 'reg
                                                               coq_ToString;
                                                       toS_rx : 'regx
                                                                coq_ToString;
                                                       toS_x : 'xreg
                                                               coq_ToString;
                                                       toS_f : 'rflag
                                                               coq_ToString;
                                                       ad_rsp : 'reg;
                                                       ad_fcp : coq_FlagCombinationParams;
                                                       check_CAimm : 
                                                       (caimm_checker_s ->
                                                       wsize ->
                                                       GRing.ComRing.sort ->
                                                       bool) }

val reg_size : ('a1, 'a2, 'a3, 'a4, 'a5) arch_decl -> wsize

val xreg_size : ('a1, 'a2, 'a3, 'a4, 'a5) arch_decl -> wsize

val cond_eqC : ('a1, 'a2, 'a3, 'a4, 'a5) arch_decl -> 'a5 eqTypeC

val toS_r : ('a1, 'a2, 'a3, 'a4, 'a5) arch_decl -> 'a1 coq_ToString

val toS_rx : ('a1, 'a2, 'a3, 'a4, 'a5) arch_decl -> 'a2 coq_ToString

val toS_x : ('a1, 'a2, 'a3, 'a4, 'a5) arch_decl -> 'a3 coq_ToString

val toS_f : ('a1, 'a2, 'a3, 'a4, 'a5) arch_decl -> 'a4 coq_ToString

val ad_rsp : ('a1, 'a2, 'a3, 'a4, 'a5) arch_decl -> 'a1

val ad_fcp : ('a1, 'a2, 'a3, 'a4, 'a5) arch_decl -> coq_FlagCombinationParams

val check_CAimm :
  ('a1, 'a2, 'a3, 'a4, 'a5) arch_decl -> caimm_checker_s -> wsize ->
  GRing.ComRing.sort -> bool

val arch_pd : ('a1, 'a2, 'a3, 'a4, 'a5) arch_decl -> coq_PointerData

val arch_msfsz : ('a1, 'a2, 'a3, 'a4, 'a5) arch_decl -> coq_MSFsize

val mk_ptr :
  ('a1, 'a2, 'a3, 'a4, 'a5) arch_decl -> Ident.Ident.ident -> Var.var

type ('reg, 'regx, 'xreg, 'rflag, 'cond) reg_t = 'reg

type ('reg, 'regx, 'xreg, 'rflag, 'cond) regx_t = 'regx

type ('reg, 'regx, 'xreg, 'rflag, 'cond) xreg_t = 'xreg

type ('reg, 'regx, 'xreg, 'rflag, 'cond) rflag_t = 'rflag

type ('reg, 'regx, 'xreg, 'rflag, 'cond) cond_t = 'cond

val lreg : ('a1, 'a2, 'a3, 'a4, 'a5) arch_decl -> ltype

type ('reg, 'regx, 'xreg, 'rflag, 'cond) wreg = sem_t

val lxreg : ('a1, 'a2, 'a3, 'a4, 'a5) arch_decl -> ltype

type ('reg, 'regx, 'xreg, 'rflag, 'cond) wxreg = sem_t

type ('reg, 'regx, 'xreg, 'rflag, 'cond) reg_address = { ad_disp : GRing.ComRing.sort;
                                                         ad_base : ('reg,
                                                                   'regx,
                                                                   'xreg,
                                                                   'rflag,
                                                                   'cond)
                                                                   reg_t
                                                                   option;
                                                         ad_scale : nat;
                                                         ad_offset : 
                                                         ('reg, 'regx, 'xreg,
                                                         'rflag, 'cond) reg_t
                                                         option }

val ad_disp :
  ('a1, 'a2, 'a3, 'a4, 'a5) arch_decl -> ('a1, 'a2, 'a3, 'a4, 'a5)
  reg_address -> GRing.ComRing.sort

val ad_base :
  ('a1, 'a2, 'a3, 'a4, 'a5) arch_decl -> ('a1, 'a2, 'a3, 'a4, 'a5)
  reg_address -> ('a1, 'a2, 'a3, 'a4, 'a5) reg_t option

val ad_scale :
  ('a1, 'a2, 'a3, 'a4, 'a5) arch_decl -> ('a1, 'a2, 'a3, 'a4, 'a5)
  reg_address -> nat

val ad_offset :
  ('a1, 'a2, 'a3, 'a4, 'a5) arch_decl -> ('a1, 'a2, 'a3, 'a4, 'a5)
  reg_address -> ('a1, 'a2, 'a3, 'a4, 'a5) reg_t option

type ('reg, 'regx, 'xreg, 'rflag, 'cond) address =
| Areg of ('reg, 'regx, 'xreg, 'rflag, 'cond) reg_address
| Arip of GRing.ComRing.sort

val oeq_reg :
  ('a1, 'a2, 'a3, 'a4, 'a5) arch_decl -> ('a1, 'a2, 'a3, 'a4, 'a5) reg_t
  option -> ('a1, 'a2, 'a3, 'a4, 'a5) reg_t option -> bool

val reg_address_beq :
  ('a1, 'a2, 'a3, 'a4, 'a5) arch_decl -> ('a1, 'a2, 'a3, 'a4, 'a5)
  reg_address -> ('a1, 'a2, 'a3, 'a4, 'a5) reg_address -> bool

val reg_address_eq_axiom :
  ('a1, 'a2, 'a3, 'a4, 'a5) arch_decl -> ('a1, 'a2, 'a3, 'a4, 'a5)
  reg_address eq_axiom

val coq_HB_unnamed_factory_3 :
  ('a1, 'a2, 'a3, 'a4, 'a5) arch_decl -> ('a1, 'a2, 'a3, 'a4, 'a5)
  reg_address Coq_hasDecEq.axioms_

val arch_decl_reg_address__canonical__eqtype_Equality :
  ('a1, 'a2, 'a3, 'a4, 'a5) arch_decl -> Equality.coq_type

val address_beq :
  ('a1, 'a2, 'a3, 'a4, 'a5) arch_decl -> ('a1, 'a2, 'a3, 'a4, 'a5) address ->
  ('a1, 'a2, 'a3, 'a4, 'a5) address -> bool

val address_eq_axiom :
  ('a1, 'a2, 'a3, 'a4, 'a5) arch_decl -> ('a1, 'a2, 'a3, 'a4, 'a5) address
  eq_axiom

val coq_HB_unnamed_factory_5 :
  ('a1, 'a2, 'a3, 'a4, 'a5) arch_decl -> ('a1, 'a2, 'a3, 'a4, 'a5) address
  Coq_hasDecEq.axioms_

val arch_decl_address__canonical__eqtype_Equality :
  ('a1, 'a2, 'a3, 'a4, 'a5) arch_decl -> Equality.coq_type

type ('reg, 'regx, 'xreg, 'rflag, 'cond) asm_arg =
| Condt of ('reg, 'regx, 'xreg, 'rflag, 'cond) cond_t
| Imm of wsize * GRing.ComRing.sort
| Reg of ('reg, 'regx, 'xreg, 'rflag, 'cond) reg_t
| Regx of ('reg, 'regx, 'xreg, 'rflag, 'cond) regx_t
| Addr of ('reg, 'regx, 'xreg, 'rflag, 'cond) address
| XReg of ('reg, 'regx, 'xreg, 'rflag, 'cond) xreg_t

type ('reg, 'regx, 'xreg, 'rflag, 'cond) asm_args =
  ('reg, 'regx, 'xreg, 'rflag, 'cond) asm_arg list

val is_Condt :
  ('a1, 'a2, 'a3, 'a4, 'a5) arch_decl -> ('a1, 'a2, 'a3, 'a4, 'a5) asm_arg ->
  ('a1, 'a2, 'a3, 'a4, 'a5) cond_t option

val asm_arg_beq :
  ('a1, 'a2, 'a3, 'a4, 'a5) arch_decl -> ('a1, 'a2, 'a3, 'a4, 'a5) asm_arg ->
  ('a1, 'a2, 'a3, 'a4, 'a5) asm_arg -> bool

val asm_arg_eq_axiom :
  ('a1, 'a2, 'a3, 'a4, 'a5) arch_decl -> ('a1, 'a2, 'a3, 'a4, 'a5) asm_arg
  eq_axiom

val coq_HB_unnamed_factory_7 :
  ('a1, 'a2, 'a3, 'a4, 'a5) arch_decl -> ('a1, 'a2, 'a3, 'a4, 'a5) asm_arg
  Coq_hasDecEq.axioms_

val arch_decl_asm_arg__canonical__eqtype_Equality :
  ('a1, 'a2, 'a3, 'a4, 'a5) arch_decl -> Equality.coq_type

type msb_flag =
| MSB_CLEAR
| MSB_MERGE

val msb_flag_rect : 'a1 -> 'a1 -> msb_flag -> 'a1

val msb_flag_rec : 'a1 -> 'a1 -> msb_flag -> 'a1

type is_msb_flag =
| Coq_is_MSB_CLEAR
| Coq_is_MSB_MERGE

val is_msb_flag_rect : 'a1 -> 'a1 -> msb_flag -> is_msb_flag -> 'a1

val is_msb_flag_rec : 'a1 -> 'a1 -> msb_flag -> is_msb_flag -> 'a1

val msb_flag_tag : msb_flag -> positive

val is_msb_flag_inhab : msb_flag -> is_msb_flag

val is_msb_flag_functor : msb_flag -> is_msb_flag -> is_msb_flag

type box_msb_flag_MSB_CLEAR =
| Box_msb_flag_MSB_CLEAR

type msb_flag_fields_t = __

val msb_flag_fields : msb_flag -> msb_flag_fields_t

val msb_flag_construct : positive -> msb_flag_fields_t -> msb_flag option

val msb_flag_induction : 'a1 -> 'a1 -> msb_flag -> is_msb_flag -> 'a1

val msb_flag_eqb_fields :
  (msb_flag -> msb_flag -> bool) -> positive -> msb_flag_fields_t ->
  msb_flag_fields_t -> bool

val msb_flag_eqb : msb_flag -> msb_flag -> bool

val msb_flag_eqb_OK : msb_flag -> msb_flag -> reflect

val msb_flag_eqb_OK_sumbool : msb_flag -> msb_flag -> bool

val coq_HB_unnamed_factory_9 : msb_flag Coq_hasDecEq.axioms_

val arch_decl_msb_flag__canonical__eqtype_Equality : Equality.coq_type

type ('reg, 'regx, 'xreg, 'rflag, 'cond) implicit_arg =
| IArflag of ('reg, 'regx, 'xreg, 'rflag, 'cond) rflag_t
| IAreg of ('reg, 'regx, 'xreg, 'rflag, 'cond) reg_t

val implicit_arg_beq :
  ('a1, 'a2, 'a3, 'a4, 'a5) arch_decl -> ('a1, 'a2, 'a3, 'a4, 'a5)
  implicit_arg -> ('a1, 'a2, 'a3, 'a4, 'a5) implicit_arg -> bool

val implicit_arg_eq_axiom :
  ('a1, 'a2, 'a3, 'a4, 'a5) arch_decl -> ('a1, 'a2, 'a3, 'a4, 'a5)
  implicit_arg eq_axiom

val coq_HB_unnamed_factory_11 :
  ('a1, 'a2, 'a3, 'a4, 'a5) arch_decl -> ('a1, 'a2, 'a3, 'a4, 'a5)
  implicit_arg Coq_hasDecEq.axioms_

val arch_decl_implicit_arg__canonical__eqtype_Equality :
  ('a1, 'a2, 'a3, 'a4, 'a5) arch_decl -> Equality.coq_type

type addr_kind =
| AK_compute
| AK_mem of aligned

type ('reg, 'regx, 'xreg, 'rflag, 'cond) arg_constrained_register =
| ACR_any
| ACR_exact of ('reg, 'regx, 'xreg, 'rflag, 'cond) reg_t
| ACR_vector of ('reg, 'regx, 'xreg, 'rflag, 'cond) xreg_t
| ACR_subset of ('reg, 'regx, 'xreg, 'rflag, 'cond) reg_t list

type ('reg, 'regx, 'xreg, 'rflag, 'cond) arg_desc =
| ADImplicit of ('reg, 'regx, 'xreg, 'rflag, 'cond) implicit_arg
| ADExplicit of addr_kind * nat
   * ('reg, 'regx, 'xreg, 'rflag, 'cond) arg_constrained_register

val coq_F :
  ('a1, 'a2, 'a3, 'a4, 'a5) arch_decl -> ('a1, 'a2, 'a3, 'a4, 'a5) rflag_t ->
  ('a1, 'a2, 'a3, 'a4, 'a5) arg_desc

val coq_R :
  ('a1, 'a2, 'a3, 'a4, 'a5) arch_decl -> ('a1, 'a2, 'a3, 'a4, 'a5) reg_t ->
  ('a1, 'a2, 'a3, 'a4, 'a5) arg_desc

val coq_Ea :
  ('a1, 'a2, 'a3, 'a4, 'a5) arch_decl -> nat -> ('a1, 'a2, 'a3, 'a4, 'a5)
  arg_desc

val coq_Eu :
  ('a1, 'a2, 'a3, 'a4, 'a5) arch_decl -> nat -> ('a1, 'a2, 'a3, 'a4, 'a5)
  arg_desc

val coq_Ec :
  ('a1, 'a2, 'a3, 'a4, 'a5) arch_decl -> nat -> ('a1, 'a2, 'a3, 'a4, 'a5)
  arg_desc

val coq_Ef :
  ('a1, 'a2, 'a3, 'a4, 'a5) arch_decl -> nat -> ('a1, 'a2, 'a3, 'a4, 'a5)
  reg_t -> ('a1, 'a2, 'a3, 'a4, 'a5) arg_desc

val check_oreg :
  ('a1, 'a2, 'a3, 'a4, 'a5) arch_decl -> ('a1, 'a2, 'a3, 'a4, 'a5)
  arg_constrained_register -> ('a1, 'a2, 'a3, 'a4, 'a5) asm_arg -> bool

type arg_kind =
| CAcond
| CAreg
| CAregx
| CAxmm
| CAmem of bool
| CAimm of caimm_checker_s * wsize

val arg_kind_rect :
  'a1 -> 'a1 -> 'a1 -> 'a1 -> (bool -> 'a1) -> (caimm_checker_s -> wsize ->
  'a1) -> arg_kind -> 'a1

val arg_kind_rec :
  'a1 -> 'a1 -> 'a1 -> 'a1 -> (bool -> 'a1) -> (caimm_checker_s -> wsize ->
  'a1) -> arg_kind -> 'a1

type is_arg_kind =
| Coq_is_CAcond
| Coq_is_CAreg
| Coq_is_CAregx
| Coq_is_CAxmm
| Coq_is_CAmem of bool * Param1.Coq_exports.is_bool
| Coq_is_CAimm of caimm_checker_s * is_caimm_checker_s * wsize * is_wsize

val is_arg_kind_rect :
  'a1 -> 'a1 -> 'a1 -> 'a1 -> (bool -> Param1.Coq_exports.is_bool -> 'a1) ->
  (caimm_checker_s -> is_caimm_checker_s -> wsize -> is_wsize -> 'a1) ->
  arg_kind -> is_arg_kind -> 'a1

val is_arg_kind_rec :
  'a1 -> 'a1 -> 'a1 -> 'a1 -> (bool -> Param1.Coq_exports.is_bool -> 'a1) ->
  (caimm_checker_s -> is_caimm_checker_s -> wsize -> is_wsize -> 'a1) ->
  arg_kind -> is_arg_kind -> 'a1

val arg_kind_tag : arg_kind -> positive

val is_arg_kind_inhab : arg_kind -> is_arg_kind

val is_arg_kind_functor : arg_kind -> is_arg_kind -> is_arg_kind

type box_arg_kind_CAcond =
| Box_arg_kind_CAcond

type box_arg_kind_CAmem =
  bool
  (* singleton inductive, whose constructor was Box_arg_kind_CAmem *)

val coq_Box_arg_kind_CAmem_0 : box_arg_kind_CAmem -> bool

type box_arg_kind_CAimm = { coq_Box_arg_kind_CAimm_0 : caimm_checker_s;
                            coq_Box_arg_kind_CAimm_1 : wsize }

val coq_Box_arg_kind_CAimm_0 : box_arg_kind_CAimm -> caimm_checker_s

val coq_Box_arg_kind_CAimm_1 : box_arg_kind_CAimm -> wsize

type arg_kind_fields_t = __

val arg_kind_fields : arg_kind -> arg_kind_fields_t

val arg_kind_construct : positive -> arg_kind_fields_t -> arg_kind option

val arg_kind_induction :
  'a1 -> 'a1 -> 'a1 -> 'a1 -> (bool -> Param1.Coq_exports.is_bool -> 'a1) ->
  (caimm_checker_s -> is_caimm_checker_s -> wsize -> is_wsize -> 'a1) ->
  arg_kind -> is_arg_kind -> 'a1

val arg_kind_eqb_fields :
  (arg_kind -> arg_kind -> bool) -> positive -> arg_kind_fields_t ->
  arg_kind_fields_t -> bool

val arg_kind_eqb : arg_kind -> arg_kind -> bool

val arg_kind_eqb_OK : arg_kind -> arg_kind -> reflect

val arg_kind_eqb_OK_sumbool : arg_kind -> arg_kind -> bool

val coq_HB_unnamed_factory_13 : arg_kind Coq_hasDecEq.axioms_

val arch_decl_arg_kind__canonical__eqtype_Equality : Equality.coq_type

type arg_kinds = arg_kind list

type args_kinds = arg_kinds list

type i_args_kinds = args_kinds list

val check_arg_kind :
  ('a1, 'a2, 'a3, 'a4, 'a5) arch_decl -> ('a1, 'a2, 'a3, 'a4, 'a5) asm_arg ->
  arg_kind -> bool

val check_arg_kinds :
  ('a1, 'a2, 'a3, 'a4, 'a5) arch_decl -> ('a1, 'a2, 'a3, 'a4, 'a5) asm_arg ->
  arg_kinds -> bool

val check_args_kinds :
  ('a1, 'a2, 'a3, 'a4, 'a5) arch_decl -> ('a1, 'a2, 'a3, 'a4, 'a5) asm_args
  -> args_kinds -> bool

val check_i_args_kinds :
  ('a1, 'a2, 'a3, 'a4, 'a5) arch_decl -> i_args_kinds -> ('a1, 'a2, 'a3, 'a4,
  'a5) asm_args -> bool

val check_arg_dest :
  ('a1, 'a2, 'a3, 'a4, 'a5) arch_decl -> ('a1, 'a2, 'a3, 'a4, 'a5) arg_desc
  -> ltype -> bool

type ('reg, 'regx, 'xreg, 'rflag, 'cond) pp_asm_op_ext =
| PP_error
| PP_name
| PP_iname of wsize
| PP_iname2 of string * wsize * wsize
| PP_viname of velem * bool
| PP_viname2 of velem * velem
| PP_ct of ('reg, 'regx, 'xreg, 'rflag, 'cond) asm_arg

type ('reg, 'regx, 'xreg, 'rflag, 'cond) pp_asm_op = { pp_aop_name : 
                                                       string;
                                                       pp_aop_ext : ('reg,
                                                                    'regx,
                                                                    'xreg,
                                                                    'rflag,
                                                                    'cond)
                                                                    pp_asm_op_ext;
                                                       pp_aop_args : 
                                                       (wsize * ('reg, 'regx,
                                                       'xreg, 'rflag, 'cond)
                                                       asm_arg) list }

val pp_aop_name :
  ('a1, 'a2, 'a3, 'a4, 'a5) arch_decl -> ('a1, 'a2, 'a3, 'a4, 'a5) pp_asm_op
  -> string

val pp_aop_ext :
  ('a1, 'a2, 'a3, 'a4, 'a5) arch_decl -> ('a1, 'a2, 'a3, 'a4, 'a5) pp_asm_op
  -> ('a1, 'a2, 'a3, 'a4, 'a5) pp_asm_op_ext

val pp_aop_args :
  ('a1, 'a2, 'a3, 'a4, 'a5) arch_decl -> ('a1, 'a2, 'a3, 'a4, 'a5) pp_asm_op
  -> (wsize * ('a1, 'a2, 'a3, 'a4, 'a5) asm_arg) list

type ('reg, 'regx, 'xreg, 'rflag, 'cond) instr_desc_t = { id_valid : 
                                                          bool;
                                                          id_msb_flag : 
                                                          msb_flag;
                                                          id_tin : ltype list;
                                                          id_in : ('reg,
                                                                  'regx,
                                                                  'xreg,
                                                                  'rflag,
                                                                  'cond)
                                                                  arg_desc
                                                                  list;
                                                          id_tout : ltype list;
                                                          id_out : ('reg,
                                                                   'regx,
                                                                   'xreg,
                                                                   'rflag,
                                                                   'cond)
                                                                   arg_desc
                                                                   list;
                                                          id_semi : sem_tuple
                                                                    exec
                                                                    sem_prod;
                                                          id_args_kinds : 
                                                          i_args_kinds;
                                                          id_nargs : 
                                                          nat;
                                                          id_str_jas : 
                                                          (unit -> string);
                                                          id_safe : safe_cond
                                                                    list;
                                                          id_pp_asm : 
                                                          (('reg, 'regx,
                                                          'xreg, 'rflag,
                                                          'cond) asm_args ->
                                                          ('reg, 'regx,
                                                          'xreg, 'rflag,
                                                          'cond) pp_asm_op) }

val id_valid :
  ('a1, 'a2, 'a3, 'a4, 'a5) arch_decl -> ('a1, 'a2, 'a3, 'a4, 'a5)
  instr_desc_t -> bool

val id_msb_flag :
  ('a1, 'a2, 'a3, 'a4, 'a5) arch_decl -> ('a1, 'a2, 'a3, 'a4, 'a5)
  instr_desc_t -> msb_flag

val id_tin :
  ('a1, 'a2, 'a3, 'a4, 'a5) arch_decl -> ('a1, 'a2, 'a3, 'a4, 'a5)
  instr_desc_t -> ltype list

val id_in :
  ('a1, 'a2, 'a3, 'a4, 'a5) arch_decl -> ('a1, 'a2, 'a3, 'a4, 'a5)
  instr_desc_t -> ('a1, 'a2, 'a3, 'a4, 'a5) arg_desc list

val id_tout :
  ('a1, 'a2, 'a3, 'a4, 'a5) arch_decl -> ('a1, 'a2, 'a3, 'a4, 'a5)
  instr_desc_t -> ltype list

val id_out :
  ('a1, 'a2, 'a3, 'a4, 'a5) arch_decl -> ('a1, 'a2, 'a3, 'a4, 'a5)
  instr_desc_t -> ('a1, 'a2, 'a3, 'a4, 'a5) arg_desc list

val id_semi :
  ('a1, 'a2, 'a3, 'a4, 'a5) arch_decl -> ('a1, 'a2, 'a3, 'a4, 'a5)
  instr_desc_t -> sem_tuple exec sem_prod

val id_args_kinds :
  ('a1, 'a2, 'a3, 'a4, 'a5) arch_decl -> ('a1, 'a2, 'a3, 'a4, 'a5)
  instr_desc_t -> i_args_kinds

val id_nargs :
  ('a1, 'a2, 'a3, 'a4, 'a5) arch_decl -> ('a1, 'a2, 'a3, 'a4, 'a5)
  instr_desc_t -> nat

val id_str_jas :
  ('a1, 'a2, 'a3, 'a4, 'a5) arch_decl -> ('a1, 'a2, 'a3, 'a4, 'a5)
  instr_desc_t -> unit -> string

val id_safe :
  ('a1, 'a2, 'a3, 'a4, 'a5) arch_decl -> ('a1, 'a2, 'a3, 'a4, 'a5)
  instr_desc_t -> safe_cond list

val id_pp_asm :
  ('a1, 'a2, 'a3, 'a4, 'a5) arch_decl -> ('a1, 'a2, 'a3, 'a4, 'a5)
  instr_desc_t -> ('a1, 'a2, 'a3, 'a4, 'a5) asm_args -> ('a1, 'a2, 'a3, 'a4,
  'a5) pp_asm_op

type ('reg, 'regx, 'xreg, 'rflag, 'cond, 'asm_op) asm_op_decl = { _eqT : 
                                                                  'asm_op
                                                                  eqTypeC;
                                                                  instr_desc_op : 
                                                                  ('asm_op ->
                                                                  ('reg,
                                                                  'regx,
                                                                  'xreg,
                                                                  'rflag,
                                                                  'cond)
                                                                  instr_desc_t);
                                                                  prim_string : 
                                                                  (string * 'asm_op
                                                                  prim_constructor)
                                                                  list }

val _eqT :
  ('a1, 'a2, 'a3, 'a4, 'a5) arch_decl -> ('a1, 'a2, 'a3, 'a4, 'a5, 'a6)
  asm_op_decl -> 'a6 eqTypeC

val instr_desc_op :
  ('a1, 'a2, 'a3, 'a4, 'a5) arch_decl -> ('a1, 'a2, 'a3, 'a4, 'a5, 'a6)
  asm_op_decl -> 'a6 -> ('a1, 'a2, 'a3, 'a4, 'a5) instr_desc_t

val prim_string :
  ('a1, 'a2, 'a3, 'a4, 'a5) arch_decl -> ('a1, 'a2, 'a3, 'a4, 'a5, 'a6)
  asm_op_decl -> (string * 'a6 prim_constructor) list

type ('reg, 'regx, 'xreg, 'rflag, 'cond, 'asm_op) asm_op_t' = 'asm_op

type ('reg, 'regx, 'xreg, 'rflag, 'cond, 'asm_op) asm_op_msb_t =
  wsize option * 'asm_op

val extend_size : wsize -> ltype -> ltype

val wextend_size : wsize -> ltype -> sem_ot -> sem_ot

val extend_tuple : wsize -> ltype list -> sem_tuple -> sem_tuple

val apply_lprod : ('a1 -> 'a2) -> __ list -> 'a1 lprod -> 'a2 lprod

val is_not_CAmem : arg_kind -> bool

val exclude_mem_args_kinds :
  ('a1, 'a2, 'a3, 'a4, 'a5) arch_decl -> ('a1, 'a2, 'a3, 'a4, 'a5) arg_desc
  -> args_kinds -> args_kinds

val exclude_mem_i_args_kinds :
  ('a1, 'a2, 'a3, 'a4, 'a5) arch_decl -> ('a1, 'a2, 'a3, 'a4, 'a5) arg_desc
  -> i_args_kinds -> i_args_kinds

val exclude_mem_aux :
  ('a1, 'a2, 'a3, 'a4, 'a5) arch_decl -> i_args_kinds -> ('a1, 'a2, 'a3, 'a4,
  'a5) arg_desc list -> i_args_kinds

val is_nil : 'a1 list -> bool

val exclude_mem :
  ('a1, 'a2, 'a3, 'a4, 'a5) arch_decl -> i_args_kinds -> ('a1, 'a2, 'a3, 'a4,
  'a5) arg_desc list -> i_args_kinds

val extend_sem :
  ltype list -> ltype list -> wsize -> sem_tuple exec sem_prod -> sem_tuple
  exec sem_prod

val instr_desc :
  ('a1, 'a2, 'a3, 'a4, 'a5) arch_decl -> ('a1, 'a2, 'a3, 'a4, 'a5, 'a6)
  asm_op_decl -> ('a1, 'a2, 'a3, 'a4, 'a5, 'a6) asm_op_msb_t -> ('a1, 'a2,
  'a3, 'a4, 'a5) instr_desc_t

type ('reg, 'regx, 'xreg, 'rflag, 'cond, 'asm_op) asm_i_r =
| ALIGN
| LABEL of label_kind * label
| STORELABEL of ('reg, 'regx, 'xreg, 'rflag, 'cond) reg_t * label
| JMP of remote_label
| JMPI of ('reg, 'regx, 'xreg, 'rflag, 'cond) asm_arg
| Jcc of label * ('reg, 'regx, 'xreg, 'rflag, 'cond) cond_t
| JAL of ('reg, 'regx, 'xreg, 'rflag, 'cond) reg_t * remote_label
| CALL of remote_label
| POPPC
| AsmOp of ('reg, 'regx, 'xreg, 'rflag, 'cond, 'asm_op) asm_op_t'
   * ('reg, 'regx, 'xreg, 'rflag, 'cond) asm_args
| SysCall of (Wsize.wsize * BinNums.positive) Syscall_t.syscall_t

type ('reg, 'regx, 'xreg, 'rflag, 'cond, 'asm_op) asm_i = { asmi_ii : 
                                                            instr_info;
                                                            asmi_i : 
                                                            ('reg, 'regx,
                                                            'xreg, 'rflag,
                                                            'cond, 'asm_op)
                                                            asm_i_r }

val asmi_ii :
  ('a1, 'a2, 'a3, 'a4, 'a5) arch_decl -> ('a1, 'a2, 'a3, 'a4, 'a5, 'a6)
  asm_op_decl -> ('a1, 'a2, 'a3, 'a4, 'a5, 'a6) asm_i -> instr_info

val asmi_i :
  ('a1, 'a2, 'a3, 'a4, 'a5) arch_decl -> ('a1, 'a2, 'a3, 'a4, 'a5, 'a6)
  asm_op_decl -> ('a1, 'a2, 'a3, 'a4, 'a5, 'a6) asm_i -> ('a1, 'a2, 'a3, 'a4,
  'a5, 'a6) asm_i_r

type ('reg, 'regx, 'xreg, 'rflag, 'cond, 'asm_op) asm_code =
  ('reg, 'regx, 'xreg, 'rflag, 'cond, 'asm_op) asm_i list

type ('reg, 'regx, 'xreg, 'rflag, 'cond) asm_typed_reg =
| ARReg of ('reg, 'regx, 'xreg, 'rflag, 'cond) reg_t
| ARegX of ('reg, 'regx, 'xreg, 'rflag, 'cond) regx_t
| AXReg of ('reg, 'regx, 'xreg, 'rflag, 'cond) xreg_t
| ABReg of ('reg, 'regx, 'xreg, 'rflag, 'cond) rflag_t

val asm_typed_reg_beq :
  ('a1, 'a2, 'a3, 'a4, 'a5) arch_decl -> ('a1, 'a2, 'a3, 'a4, 'a5)
  asm_typed_reg -> ('a1, 'a2, 'a3, 'a4, 'a5) asm_typed_reg -> bool

val asm_typed_reg_eq_axiom :
  ('a1, 'a2, 'a3, 'a4, 'a5) arch_decl -> ('a1, 'a2, 'a3, 'a4, 'a5)
  asm_typed_reg eq_axiom

val coq_HB_unnamed_factory_15 :
  ('a1, 'a2, 'a3, 'a4, 'a5) arch_decl -> ('a1, 'a2, 'a3, 'a4, 'a5)
  asm_typed_reg Coq_hasDecEq.axioms_

val arch_decl_asm_typed_reg__canonical__eqtype_Equality :
  ('a1, 'a2, 'a3, 'a4, 'a5) arch_decl -> Equality.coq_type

type ('reg, 'regx, 'xreg, 'rflag, 'cond, 'asm_op) asm_fundef = { asm_fd_align : 
                                                                 wsize;
                                                                 asm_fd_arg : 
                                                                 ('reg,
                                                                 'regx,
                                                                 'xreg,
                                                                 'rflag,
                                                                 'cond)
                                                                 asm_typed_reg
                                                                 list;
                                                                 asm_fd_body : 
                                                                 ('reg,
                                                                 'regx,
                                                                 'xreg,
                                                                 'rflag,
                                                                 'cond,
                                                                 'asm_op)
                                                                 asm_code;
                                                                 asm_fd_res : 
                                                                 ('reg,
                                                                 'regx,
                                                                 'xreg,
                                                                 'rflag,
                                                                 'cond)
                                                                 asm_typed_reg
                                                                 list;
                                                                 asm_fd_export : 
                                                                 bool;
                                                                 asm_fd_total_stack : 
                                                                 coq_Z;
                                                                 asm_fd_align_args : 
                                                                 wsize list }

val asm_fd_align :
  ('a1, 'a2, 'a3, 'a4, 'a5) arch_decl -> ('a1, 'a2, 'a3, 'a4, 'a5, 'a6)
  asm_op_decl -> ('a1, 'a2, 'a3, 'a4, 'a5, 'a6) asm_fundef -> wsize

val asm_fd_arg :
  ('a1, 'a2, 'a3, 'a4, 'a5) arch_decl -> ('a1, 'a2, 'a3, 'a4, 'a5, 'a6)
  asm_op_decl -> ('a1, 'a2, 'a3, 'a4, 'a5, 'a6) asm_fundef -> ('a1, 'a2, 'a3,
  'a4, 'a5) asm_typed_reg list

val asm_fd_body :
  ('a1, 'a2, 'a3, 'a4, 'a5) arch_decl -> ('a1, 'a2, 'a3, 'a4, 'a5, 'a6)
  asm_op_decl -> ('a1, 'a2, 'a3, 'a4, 'a5, 'a6) asm_fundef -> ('a1, 'a2, 'a3,
  'a4, 'a5, 'a6) asm_code

val asm_fd_res :
  ('a1, 'a2, 'a3, 'a4, 'a5) arch_decl -> ('a1, 'a2, 'a3, 'a4, 'a5, 'a6)
  asm_op_decl -> ('a1, 'a2, 'a3, 'a4, 'a5, 'a6) asm_fundef -> ('a1, 'a2, 'a3,
  'a4, 'a5) asm_typed_reg list

val asm_fd_export :
  ('a1, 'a2, 'a3, 'a4, 'a5) arch_decl -> ('a1, 'a2, 'a3, 'a4, 'a5, 'a6)
  asm_op_decl -> ('a1, 'a2, 'a3, 'a4, 'a5, 'a6) asm_fundef -> bool

val asm_fd_total_stack :
  ('a1, 'a2, 'a3, 'a4, 'a5) arch_decl -> ('a1, 'a2, 'a3, 'a4, 'a5, 'a6)
  asm_op_decl -> ('a1, 'a2, 'a3, 'a4, 'a5, 'a6) asm_fundef -> coq_Z

val asm_fd_align_args :
  ('a1, 'a2, 'a3, 'a4, 'a5) arch_decl -> ('a1, 'a2, 'a3, 'a4, 'a5, 'a6)
  asm_op_decl -> ('a1, 'a2, 'a3, 'a4, 'a5, 'a6) asm_fundef -> wsize list

type ('reg, 'regx, 'xreg, 'rflag, 'cond, 'asm_op) asm_prog = { asm_globs : 
                                                               GRing.ComRing.sort
                                                               list;
                                                               asm_glob_names : 
                                                               ((Var.var * wsize) * coq_Z)
                                                               list;
                                                               asm_funcs : 
                                                               (funname * ('reg,
                                                               'regx, 'xreg,
                                                               'rflag, 'cond,
                                                               'asm_op)
                                                               asm_fundef)
                                                               list }

val asm_globs :
  ('a1, 'a2, 'a3, 'a4, 'a5) arch_decl -> ('a1, 'a2, 'a3, 'a4, 'a5, 'a6)
  asm_op_decl -> ('a1, 'a2, 'a3, 'a4, 'a5, 'a6) asm_prog ->
  GRing.ComRing.sort list

val asm_glob_names :
  ('a1, 'a2, 'a3, 'a4, 'a5) arch_decl -> ('a1, 'a2, 'a3, 'a4, 'a5, 'a6)
  asm_op_decl -> ('a1, 'a2, 'a3, 'a4, 'a5, 'a6) asm_prog ->
  ((Var.var * wsize) * coq_Z) list

val asm_funcs :
  ('a1, 'a2, 'a3, 'a4, 'a5) arch_decl -> ('a1, 'a2, 'a3, 'a4, 'a5, 'a6)
  asm_op_decl -> ('a1, 'a2, 'a3, 'a4, 'a5, 'a6) asm_prog -> (funname * ('a1,
  'a2, 'a3, 'a4, 'a5, 'a6) asm_fundef) list

val is_ABReg :
  ('a1, 'a2, 'a3, 'a4, 'a5) arch_decl -> ('a1, 'a2, 'a3, 'a4, 'a5)
  asm_typed_reg -> bool

type ('reg, 'regx, 'xreg, 'rflag, 'cond) calling_convention = { callee_saved : 
                                                                ('reg, 'regx,
                                                                'xreg,
                                                                'rflag,
                                                                'cond)
                                                                asm_typed_reg
                                                                list;
                                                                call_reg_args : 
                                                                ('reg, 'regx,
                                                                'xreg,
                                                                'rflag,
                                                                'cond) reg_t
                                                                list;
                                                                call_xreg_args : 
                                                                ('reg, 'regx,
                                                                'xreg,
                                                                'rflag,
                                                                'cond) xreg_t
                                                                list;
                                                                call_reg_ret : 
                                                                ('reg, 'regx,
                                                                'xreg,
                                                                'rflag,
                                                                'cond) reg_t
                                                                list;
                                                                call_xreg_ret : 
                                                                ('reg, 'regx,
                                                                'xreg,
                                                                'rflag,
                                                                'cond) xreg_t
                                                                list }

val callee_saved :
  ('a1, 'a2, 'a3, 'a4, 'a5) arch_decl -> ('a1, 'a2, 'a3, 'a4, 'a5)
  calling_convention -> ('a1, 'a2, 'a3, 'a4, 'a5) asm_typed_reg list

val call_reg_args :
  ('a1, 'a2, 'a3, 'a4, 'a5) arch_decl -> ('a1, 'a2, 'a3, 'a4, 'a5)
  calling_convention -> ('a1, 'a2, 'a3, 'a4, 'a5) reg_t list

val call_xreg_args :
  ('a1, 'a2, 'a3, 'a4, 'a5) arch_decl -> ('a1, 'a2, 'a3, 'a4, 'a5)
  calling_convention -> ('a1, 'a2, 'a3, 'a4, 'a5) xreg_t list

val call_reg_ret :
  ('a1, 'a2, 'a3, 'a4, 'a5) arch_decl -> ('a1, 'a2, 'a3, 'a4, 'a5)
  calling_convention -> ('a1, 'a2, 'a3, 'a4, 'a5) reg_t list

val call_xreg_ret :
  ('a1, 'a2, 'a3, 'a4, 'a5) arch_decl -> ('a1, 'a2, 'a3, 'a4, 'a5)
  calling_convention -> ('a1, 'a2, 'a3, 'a4, 'a5) xreg_t list

val get_ARReg :
  ('a1, 'a2, 'a3, 'a4, 'a5) arch_decl -> ('a1, 'a2, 'a3, 'a4, 'a5)
  asm_typed_reg -> ('a1, 'a2, 'a3, 'a4, 'a5) reg_t option

val get_ARegX :
  ('a1, 'a2, 'a3, 'a4, 'a5) arch_decl -> ('a1, 'a2, 'a3, 'a4, 'a5)
  asm_typed_reg -> ('a1, 'a2, 'a3, 'a4, 'a5) regx_t option

val get_AXReg :
  ('a1, 'a2, 'a3, 'a4, 'a5) arch_decl -> ('a1, 'a2, 'a3, 'a4, 'a5)
  asm_typed_reg -> ('a1, 'a2, 'a3, 'a4, 'a5) xreg_t option

val check_list :
  ('a1, 'a2, 'a3, 'a4, 'a5) arch_decl -> 'a6 eqTypeC -> (('a1, 'a2, 'a3, 'a4,
  'a5) asm_typed_reg -> 'a6 option) -> ('a1, 'a2, 'a3, 'a4, 'a5)
  asm_typed_reg list -> 'a6 list -> bool

val check_call_conv :
  ('a1, 'a2, 'a3, 'a4, 'a5) arch_decl -> ('a1, 'a2, 'a3, 'a4, 'a5, 'a6)
  asm_op_decl -> ('a1, 'a2, 'a3, 'a4, 'a5) calling_convention -> ('a1, 'a2,
  'a3, 'a4, 'a5, 'a6) asm_fundef -> bool

val registers :
  ('a1, 'a2, 'a3, 'a4, 'a5) arch_decl -> ('a1, 'a2, 'a3, 'a4, 'a5) reg_t list

val registerxs :
  ('a1, 'a2, 'a3, 'a4, 'a5) arch_decl -> ('a1, 'a2, 'a3, 'a4, 'a5) regx_t list

val xregisters :
  ('a1, 'a2, 'a3, 'a4, 'a5) arch_decl -> ('a1, 'a2, 'a3, 'a4, 'a5) xreg_t list

val rflags :
  ('a1, 'a2, 'a3, 'a4, 'a5) arch_decl -> ('a1, 'a2, 'a3, 'a4, 'a5) rflag_t
  list

type rflagv =
| Def of bool
| Undef

val rflagv_rect : (bool -> 'a1) -> 'a1 -> rflagv -> 'a1

val rflagv_rec : (bool -> 'a1) -> 'a1 -> rflagv -> 'a1

type is_rflagv =
| Coq_is_Def of bool * Param1.Coq_exports.is_bool
| Coq_is_Undef

val is_rflagv_rect :
  (bool -> Param1.Coq_exports.is_bool -> 'a1) -> 'a1 -> rflagv -> is_rflagv
  -> 'a1

val is_rflagv_rec :
  (bool -> Param1.Coq_exports.is_bool -> 'a1) -> 'a1 -> rflagv -> is_rflagv
  -> 'a1

val rflagv_tag : rflagv -> positive

val is_rflagv_inhab : rflagv -> is_rflagv

val is_rflagv_functor : rflagv -> is_rflagv -> is_rflagv

type box_rflagv_Def =
  bool
  (* singleton inductive, whose constructor was Box_rflagv_Def *)

val coq_Box_rflagv_Def_0 : box_rflagv_Def -> bool

type box_rflagv_Undef =
| Box_rflagv_Undef

type rflagv_fields_t = __

val rflagv_fields : rflagv -> rflagv_fields_t

val rflagv_construct : positive -> rflagv_fields_t -> rflagv option

val rflagv_induction :
  (bool -> Param1.Coq_exports.is_bool -> 'a1) -> 'a1 -> rflagv -> is_rflagv
  -> 'a1

val rflagv_eqb_fields :
  (rflagv -> rflagv -> bool) -> positive -> rflagv_fields_t ->
  rflagv_fields_t -> bool

val rflagv_eqb : rflagv -> rflagv -> bool

val rflagv_eqb_OK : rflagv -> rflagv -> reflect

val rflagv_eqb_OK_sumbool : rflagv -> rflagv -> bool

val coq_HB_unnamed_factory_17 : rflagv Coq_hasDecEq.axioms_

val arch_decl_rflagv__canonical__eqtype_Equality : Equality.coq_type

type ('reg, 'regx, 'xreg, 'rflag, 'cond, 'asm_op) asm = { _arch_decl : 
                                                          ('reg, 'regx,
                                                          'xreg, 'rflag,
                                                          'cond) arch_decl;
                                                          _asm_op_decl : 
                                                          ('reg, 'regx,
                                                          'xreg, 'rflag,
                                                          'cond, 'asm_op)
                                                          asm_op_decl;
                                                          eval_cond : 
                                                          ((('reg, 'regx,
                                                          'xreg, 'rflag,
                                                          'cond) reg_t ->
                                                          GRing.ComRing.sort)
                                                          -> (('reg, 'regx,
                                                          'xreg, 'rflag,
                                                          'cond) rflag_t ->
                                                          bool exec) ->
                                                          ('reg, 'regx,
                                                          'xreg, 'rflag,
                                                          'cond) cond_t ->
                                                          bool exec) }

val _arch_decl :
  ('a1, 'a2, 'a3, 'a4, 'a5, 'a6) asm -> ('a1, 'a2, 'a3, 'a4, 'a5) arch_decl

val _asm_op_decl :
  ('a1, 'a2, 'a3, 'a4, 'a5, 'a6) asm -> ('a1, 'a2, 'a3, 'a4, 'a5, 'a6)
  asm_op_decl

val eval_cond :
  ('a1, 'a2, 'a3, 'a4, 'a5, 'a6) asm -> (('a1, 'a2, 'a3, 'a4, 'a5) reg_t ->
  GRing.ComRing.sort) -> (('a1, 'a2, 'a3, 'a4, 'a5) rflag_t -> bool exec) ->
  ('a1, 'a2, 'a3, 'a4, 'a5) cond_t -> bool exec

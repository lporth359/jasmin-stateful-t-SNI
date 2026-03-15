open BinInt
open BinNums
open Bool
open Arch_decl
open Arch_utils
open EqbOK
open Eqb_core_defs
open Eqtype
open Fintype
open Flag_combination
open Seq
open Ssralg
open Std
open Type
open Utils0
open Word0
open Wsize

type __ = Obj.t

val riscv_reg_size : wsize

val riscv_xreg_size : wsize

type register =
| RA
| SP
| X5
| X6
| X7
| X8
| X9
| X10
| X11
| X12
| X13
| X14
| X15
| X16
| X17
| X18
| X19
| X20
| X21
| X22
| X23
| X24
| X25
| X26
| X27
| X28
| X29
| X30
| X31

val register_rect :
  'a1 -> 'a1 -> 'a1 -> 'a1 -> 'a1 -> 'a1 -> 'a1 -> 'a1 -> 'a1 -> 'a1 -> 'a1
  -> 'a1 -> 'a1 -> 'a1 -> 'a1 -> 'a1 -> 'a1 -> 'a1 -> 'a1 -> 'a1 -> 'a1 ->
  'a1 -> 'a1 -> 'a1 -> 'a1 -> 'a1 -> 'a1 -> 'a1 -> 'a1 -> register -> 'a1

val register_rec :
  'a1 -> 'a1 -> 'a1 -> 'a1 -> 'a1 -> 'a1 -> 'a1 -> 'a1 -> 'a1 -> 'a1 -> 'a1
  -> 'a1 -> 'a1 -> 'a1 -> 'a1 -> 'a1 -> 'a1 -> 'a1 -> 'a1 -> 'a1 -> 'a1 ->
  'a1 -> 'a1 -> 'a1 -> 'a1 -> 'a1 -> 'a1 -> 'a1 -> 'a1 -> register -> 'a1

type is_register =
| Coq_is_RA
| Coq_is_SP
| Coq_is_X5
| Coq_is_X6
| Coq_is_X7
| Coq_is_X8
| Coq_is_X9
| Coq_is_X10
| Coq_is_X11
| Coq_is_X12
| Coq_is_X13
| Coq_is_X14
| Coq_is_X15
| Coq_is_X16
| Coq_is_X17
| Coq_is_X18
| Coq_is_X19
| Coq_is_X20
| Coq_is_X21
| Coq_is_X22
| Coq_is_X23
| Coq_is_X24
| Coq_is_X25
| Coq_is_X26
| Coq_is_X27
| Coq_is_X28
| Coq_is_X29
| Coq_is_X30
| Coq_is_X31

val is_register_rect :
  'a1 -> 'a1 -> 'a1 -> 'a1 -> 'a1 -> 'a1 -> 'a1 -> 'a1 -> 'a1 -> 'a1 -> 'a1
  -> 'a1 -> 'a1 -> 'a1 -> 'a1 -> 'a1 -> 'a1 -> 'a1 -> 'a1 -> 'a1 -> 'a1 ->
  'a1 -> 'a1 -> 'a1 -> 'a1 -> 'a1 -> 'a1 -> 'a1 -> 'a1 -> register ->
  is_register -> 'a1

val is_register_rec :
  'a1 -> 'a1 -> 'a1 -> 'a1 -> 'a1 -> 'a1 -> 'a1 -> 'a1 -> 'a1 -> 'a1 -> 'a1
  -> 'a1 -> 'a1 -> 'a1 -> 'a1 -> 'a1 -> 'a1 -> 'a1 -> 'a1 -> 'a1 -> 'a1 ->
  'a1 -> 'a1 -> 'a1 -> 'a1 -> 'a1 -> 'a1 -> 'a1 -> 'a1 -> register ->
  is_register -> 'a1

val register_tag : register -> positive

val is_register_inhab : register -> is_register

val is_register_functor : register -> is_register -> is_register

type box_register_RA =
| Box_register_RA

type register_fields_t = __

val register_fields : register -> register_fields_t

val register_construct : positive -> register_fields_t -> register option

val register_induction :
  'a1 -> 'a1 -> 'a1 -> 'a1 -> 'a1 -> 'a1 -> 'a1 -> 'a1 -> 'a1 -> 'a1 -> 'a1
  -> 'a1 -> 'a1 -> 'a1 -> 'a1 -> 'a1 -> 'a1 -> 'a1 -> 'a1 -> 'a1 -> 'a1 ->
  'a1 -> 'a1 -> 'a1 -> 'a1 -> 'a1 -> 'a1 -> 'a1 -> 'a1 -> register ->
  is_register -> 'a1

val register_eqb_fields :
  (register -> register -> bool) -> positive -> register_fields_t ->
  register_fields_t -> bool

val register_eqb : register -> register -> bool

val register_eqb_OK : register -> register -> reflect

val register_eqb_OK_sumbool : register -> register -> bool

val eqTC_register : register eqTypeC

val riscv_register_eqType : Equality.coq_type

val registers : register list

val finTC_register : register finTypeC

val register_finType : Finite.coq_type

val register_to_string : register -> string

val reg_toS : register coq_ToString

type condition_kind =
| EQ
| NE
| LT of signedness
| GE of signedness

val condition_kind_rect :
  'a1 -> 'a1 -> (signedness -> 'a1) -> (signedness -> 'a1) -> condition_kind
  -> 'a1

val condition_kind_rec :
  'a1 -> 'a1 -> (signedness -> 'a1) -> (signedness -> 'a1) -> condition_kind
  -> 'a1

type is_condition_kind =
| Coq_is_EQ
| Coq_is_NE
| Coq_is_LT of signedness * is_signedness
| Coq_is_GE of signedness * is_signedness

val is_condition_kind_rect :
  'a1 -> 'a1 -> (signedness -> is_signedness -> 'a1) -> (signedness ->
  is_signedness -> 'a1) -> condition_kind -> is_condition_kind -> 'a1

val is_condition_kind_rec :
  'a1 -> 'a1 -> (signedness -> is_signedness -> 'a1) -> (signedness ->
  is_signedness -> 'a1) -> condition_kind -> is_condition_kind -> 'a1

val condition_kind_tag : condition_kind -> positive

val is_condition_kind_inhab : condition_kind -> is_condition_kind

val is_condition_kind_functor :
  condition_kind -> is_condition_kind -> is_condition_kind

type box_condition_kind_EQ =
| Box_condition_kind_EQ

type box_condition_kind_LT =
  signedness
  (* singleton inductive, whose constructor was Box_condition_kind_LT *)

val coq_Box_condition_kind_LT_0 : box_condition_kind_LT -> signedness

type condition_kind_fields_t = __

val condition_kind_fields : condition_kind -> condition_kind_fields_t

val condition_kind_construct :
  positive -> condition_kind_fields_t -> condition_kind option

val condition_kind_induction :
  'a1 -> 'a1 -> (signedness -> is_signedness -> 'a1) -> (signedness ->
  is_signedness -> 'a1) -> condition_kind -> is_condition_kind -> 'a1

val condition_kind_eqb_fields :
  (condition_kind -> condition_kind -> bool) -> positive ->
  condition_kind_fields_t -> condition_kind_fields_t -> bool

val condition_kind_eqb : condition_kind -> condition_kind -> bool

val condition_kind_eqb_OK : condition_kind -> condition_kind -> reflect

val condition_kind_eqb_OK_sumbool : condition_kind -> condition_kind -> bool

type condt = { cond_kind : condition_kind; cond_fst : register option;
               cond_snd : register option }

val cond_kind : condt -> condition_kind

val cond_fst : condt -> register option

val cond_snd : condt -> register option

type is_condt =
| Coq_is_Build_condt of condition_kind * is_condition_kind * register option
   * (register, is_register) Prelude.is_option * register option
   * (register, is_register) Prelude.is_option

val is_condt_rect :
  (condition_kind -> is_condition_kind -> register option -> (register,
  is_register) Prelude.is_option -> register option -> (register,
  is_register) Prelude.is_option -> 'a1) -> condt -> is_condt -> 'a1

val is_condt_rec :
  (condition_kind -> is_condition_kind -> register option -> (register,
  is_register) Prelude.is_option -> register option -> (register,
  is_register) Prelude.is_option -> 'a1) -> condt -> is_condt -> 'a1

val condt_tag : condt -> positive

val is_condt_inhab : condt -> is_condt

val is_condt_functor : condt -> is_condt -> is_condt

type box_condt_Build_condt = { coq_Box_condt_Build_condt_0 : condition_kind;
                               coq_Box_condt_Build_condt_1 : register option;
                               coq_Box_condt_Build_condt_2 : register option }

val coq_Box_condt_Build_condt_0 : box_condt_Build_condt -> condition_kind

val coq_Box_condt_Build_condt_1 : box_condt_Build_condt -> register option

val coq_Box_condt_Build_condt_2 : box_condt_Build_condt -> register option

type condt_fields_t = box_condt_Build_condt

val condt_fields : condt -> condt_fields_t

val condt_construct : positive -> box_condt_Build_condt -> condt option

val condt_induction :
  (condition_kind -> is_condition_kind -> register option -> (register,
  is_register) Prelude.is_option -> register option -> (register,
  is_register) Prelude.is_option -> 'a1) -> condt -> is_condt -> 'a1

val condt_eqb_fields :
  (condt -> condt -> bool) -> positive -> box_condt_Build_condt ->
  box_condt_Build_condt -> bool

val condt_eqb : condt -> condt -> bool

val condt_eqb_OK : condt -> condt -> reflect

val condt_eqb_OK_sumbool : condt -> condt -> bool

val eqTC_condt : condt eqTypeC

val condt_eqType : Equality.coq_type

val riscv_fc_of_cfc : combine_flags_core -> flag_combination

val riscv_fcp : coq_FlagCombinationParams

val riscv_check_CAimm : caimm_checker_s -> wsize -> GRing.ComRing.sort -> bool

val riscv_decl : (register, empty, empty, empty, condt) arch_decl

val riscv_linux_call_conv :
  (register, empty, empty, empty, condt) calling_convention

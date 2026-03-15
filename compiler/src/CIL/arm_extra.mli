open BinNums
open Bool
open Datatypes
open Arch_decl
open Arch_extra
open Arch_utils
open Arm
open Arm_decl
open Arm_instr_decl
open Arm_params_core
open Compiler_util
open EqbOK
open Eqb_core_defs
open Eqtype
open Expr
open Fexpr
open Sem_type
open Seq
open Sopn
open Ssralg
open Type
open Utils0
open Var0
open Word0
open Wsize

type __ = Obj.t

type arm_extra_op =
| Oarm_swap of wsize
| Oarm_add_large_imm
| Osmart_li of wsize
| Osmart_li_cc of wsize

val arm_extra_op_rect :
  (wsize -> 'a1) -> 'a1 -> (wsize -> 'a1) -> (wsize -> 'a1) -> arm_extra_op
  -> 'a1

val arm_extra_op_rec :
  (wsize -> 'a1) -> 'a1 -> (wsize -> 'a1) -> (wsize -> 'a1) -> arm_extra_op
  -> 'a1

type is_arm_extra_op =
| Coq_is_Oarm_swap of wsize * is_wsize
| Coq_is_Oarm_add_large_imm
| Coq_is_Osmart_li of wsize * is_wsize
| Coq_is_Osmart_li_cc of wsize * is_wsize

val is_arm_extra_op_rect :
  (wsize -> is_wsize -> 'a1) -> 'a1 -> (wsize -> is_wsize -> 'a1) -> (wsize
  -> is_wsize -> 'a1) -> arm_extra_op -> is_arm_extra_op -> 'a1

val is_arm_extra_op_rec :
  (wsize -> is_wsize -> 'a1) -> 'a1 -> (wsize -> is_wsize -> 'a1) -> (wsize
  -> is_wsize -> 'a1) -> arm_extra_op -> is_arm_extra_op -> 'a1

val arm_extra_op_tag : arm_extra_op -> positive

val is_arm_extra_op_inhab : arm_extra_op -> is_arm_extra_op

val is_arm_extra_op_functor :
  arm_extra_op -> is_arm_extra_op -> is_arm_extra_op

type box_arm_extra_op_Oarm_swap =
  wsize
  (* singleton inductive, whose constructor was Box_arm_extra_op_Oarm_swap *)

val coq_Box_arm_extra_op_Oarm_swap_0 : box_arm_extra_op_Oarm_swap -> wsize

type box_arm_extra_op_Oarm_add_large_imm =
| Box_arm_extra_op_Oarm_add_large_imm

type arm_extra_op_fields_t = __

val arm_extra_op_fields : arm_extra_op -> arm_extra_op_fields_t

val arm_extra_op_construct :
  positive -> arm_extra_op_fields_t -> arm_extra_op option

val arm_extra_op_induction :
  (wsize -> is_wsize -> 'a1) -> 'a1 -> (wsize -> is_wsize -> 'a1) -> (wsize
  -> is_wsize -> 'a1) -> arm_extra_op -> is_arm_extra_op -> 'a1

val arm_extra_op_eqb_fields :
  (arm_extra_op -> arm_extra_op -> bool) -> positive -> arm_extra_op_fields_t
  -> arm_extra_op_fields_t -> bool

val arm_extra_op_eqb : arm_extra_op -> arm_extra_op -> bool

val arm_extra_op_eqb_OK : arm_extra_op -> arm_extra_op -> reflect

val arm_extra_op_eqb_OK_sumbool : arm_extra_op -> arm_extra_op -> bool

val coq_HB_unnamed_factory_1 : arm_extra_op Coq_hasDecEq.axioms_

val arm_extra_arm_extra_op__canonical__eqtype_Equality : Equality.coq_type

val eqTC_arm_extra_op : arm_extra_op eqTypeC

val coq_Oarm_add_large_imm_instr : instruction_desc

val smart_li_instr : wsize -> instruction_desc

val smart_li_instr_cc : wsize -> instruction_desc

val get_instr_desc : arm_extra_op -> instruction_desc

val arm_extra_op_decl : arm_extra_op asmOp

module E :
 sig
  val pass_name : string

  val internal_error : instr_info -> string -> pp_error_loc

  val error : instr_info -> string -> pp_error_loc

  val li_condition_modified : instr_info -> pp_error_loc
 end

val asm_args_of_opn_args :
  ARMFopn_core.opn_args list -> (((register, empty, empty, rflag, condt,
  arm_op) asm_op_msb_t * lexpr list) * rexpr list) list

val uncons : instr_info -> 'a1 list -> ('a1 * 'a1 list) cexec

val uncons_LLvar : instr_info -> lexpr list -> (var_i * lexpr list) cexec

val uncons_rvar : instr_info -> rexpr list -> (var_i * rexpr list) cexec

val uncons_wconst : instr_info -> rexpr list -> (coq_Z * rexpr list) cexec

val smart_li_args :
  instr_info -> wsize -> lexpr list -> rexpr list -> (pp_error_loc,
  (var_i * coq_Z) * rexpr list) result

val assemble_smart_li :
  instr_info -> wsize -> lexpr list -> rexpr list -> (pp_error_loc,
  (((register, empty, empty, rflag, condt, arm_op) asm_op_msb_t * lexpr
  list) * rexpr list) list) result

val assemble_smart_li_cc :
  instr_info -> wsize -> lexpr list -> rexpr list -> (((register, empty,
  empty, rflag, condt, arm_op) asm_op_msb_t * lexpr list) * rexpr list) list
  cexec

val assemble_extra :
  instr_info -> arm_extra_op -> lexpr list -> rexpr list -> (((register,
  empty, empty, rflag, condt, arm_op) asm_op_msb_t * lexpr list) * rexpr
  list) list cexec

val arm_extra :
  (register, empty, empty, rflag, condt) arch_toIdent -> (register, empty,
  empty, rflag, condt, arm_op, arm_extra_op) asm_extra

type arm_extended_op =
  (register, empty, empty, rflag, condt, arm_op, arm_extra_op) extended_op

val coq_Oarm :
  (register, empty, empty, rflag, condt) arch_toIdent -> arm_op ->
  arm_extended_op sopn

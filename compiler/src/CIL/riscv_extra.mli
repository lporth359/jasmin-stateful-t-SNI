open BinNums
open Bool
open Datatypes
open Arch_decl
open Arch_extra
open Arch_utils
open Compiler_util
open EqbOK
open Eqb_core_defs
open Eqtype
open Expr
open Fexpr
open Riscv
open Riscv_decl
open Riscv_instr_decl
open Riscv_params_core
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

type riscv_extra_op =
| SWAP of wsize
| Oriscv_add_large_imm

val riscv_extra_op_rect : (wsize -> 'a1) -> 'a1 -> riscv_extra_op -> 'a1

val riscv_extra_op_rec : (wsize -> 'a1) -> 'a1 -> riscv_extra_op -> 'a1

type is_riscv_extra_op =
| Coq_is_SWAP of wsize * is_wsize
| Coq_is_Oriscv_add_large_imm

val is_riscv_extra_op_rect :
  (wsize -> is_wsize -> 'a1) -> 'a1 -> riscv_extra_op -> is_riscv_extra_op ->
  'a1

val is_riscv_extra_op_rec :
  (wsize -> is_wsize -> 'a1) -> 'a1 -> riscv_extra_op -> is_riscv_extra_op ->
  'a1

val riscv_extra_op_tag : riscv_extra_op -> positive

val is_riscv_extra_op_inhab : riscv_extra_op -> is_riscv_extra_op

val is_riscv_extra_op_functor :
  riscv_extra_op -> is_riscv_extra_op -> is_riscv_extra_op

type box_riscv_extra_op_SWAP =
  wsize
  (* singleton inductive, whose constructor was Box_riscv_extra_op_SWAP *)

val coq_Box_riscv_extra_op_SWAP_0 : box_riscv_extra_op_SWAP -> wsize

type box_riscv_extra_op_Oriscv_add_large_imm =
| Box_riscv_extra_op_Oriscv_add_large_imm

type riscv_extra_op_fields_t = __

val riscv_extra_op_fields : riscv_extra_op -> riscv_extra_op_fields_t

val riscv_extra_op_construct :
  positive -> riscv_extra_op_fields_t -> riscv_extra_op option

val riscv_extra_op_induction :
  (wsize -> is_wsize -> 'a1) -> 'a1 -> riscv_extra_op -> is_riscv_extra_op ->
  'a1

val riscv_extra_op_eqb_fields :
  (riscv_extra_op -> riscv_extra_op -> bool) -> positive ->
  riscv_extra_op_fields_t -> riscv_extra_op_fields_t -> bool

val riscv_extra_op_eqb : riscv_extra_op -> riscv_extra_op -> bool

val riscv_extra_op_eqb_OK : riscv_extra_op -> riscv_extra_op -> reflect

val riscv_extra_op_eqb_OK_sumbool : riscv_extra_op -> riscv_extra_op -> bool

val coq_HB_unnamed_factory_1 : riscv_extra_op Coq_hasDecEq.axioms_

val riscv_extra_riscv_extra_op__canonical__eqtype_Equality : Equality.coq_type

val eqTC_riscv_extra_op : riscv_extra_op eqTypeC

val coq_Oriscv_add_large_imm_instr : instruction_desc

val get_instr_desc : riscv_extra_op -> instruction_desc

val riscv_extra_op_decl : riscv_extra_op asmOp

module E :
 sig
  val pass_name : string

  val internal_error : instr_info -> string -> pp_error_loc

  val error : instr_info -> string -> pp_error_loc
 end

val asm_args_of_opn_args :
  RISCVFopn_core.opn_args list -> (((register, empty, empty, empty, condt,
  riscv_op) asm_op_msb_t * lexpr list) * rexpr list) list

val assemble_extra :
  instr_info -> riscv_extra_op -> lexpr list -> rexpr list -> (((register,
  empty, empty, empty, condt, riscv_op) asm_op_msb_t * lexpr list) * rexpr
  list) list cexec

val riscv_extra :
  (register, empty, empty, empty, condt) arch_toIdent -> (register, empty,
  empty, empty, condt, riscv_op, riscv_extra_op) asm_extra

type riscv_extended_op =
  (register, empty, empty, empty, condt, riscv_op, riscv_extra_op) extended_op

val coq_Oriscv :
  (register, empty, empty, empty, condt) arch_toIdent -> riscv_op ->
  riscv_extended_op sopn

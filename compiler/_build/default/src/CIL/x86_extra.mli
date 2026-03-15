open BinNums
open Bool
open Datatypes
open Arch_decl
open Arch_extra
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
open X86
open X86_decl
open X86_instr_decl

type __ = Obj.t

module E :
 sig
  val pass_name : string

  val error : instr_info -> string -> pp_error_loc

  val se_update_arguments : instr_info -> pp_error_loc

  val se_protect_arguments : instr_info -> pp_error_loc

  val se_protect_ptr : instr_info -> pp_error_loc
 end

type x86_extra_op =
| Oset0 of wsize
| Oconcat128
| Ox86MOVZX32
| Ox86MULX of wsize
| Ox86MULX_hi of wsize
| Ox86SLHinit
| Ox86SLHupdate
| Ox86SLHmove
| Ox86SLHprotect of reg_kind * wsize

val x86_extra_op_rect :
  (wsize -> 'a1) -> 'a1 -> 'a1 -> (wsize -> 'a1) -> (wsize -> 'a1) -> 'a1 ->
  'a1 -> 'a1 -> (reg_kind -> wsize -> 'a1) -> x86_extra_op -> 'a1

val x86_extra_op_rec :
  (wsize -> 'a1) -> 'a1 -> 'a1 -> (wsize -> 'a1) -> (wsize -> 'a1) -> 'a1 ->
  'a1 -> 'a1 -> (reg_kind -> wsize -> 'a1) -> x86_extra_op -> 'a1

type is_x86_extra_op =
| Coq_is_Oset0 of wsize * is_wsize
| Coq_is_Oconcat128
| Coq_is_Ox86MOVZX32
| Coq_is_Ox86MULX of wsize * is_wsize
| Coq_is_Ox86MULX_hi of wsize * is_wsize
| Coq_is_Ox86SLHinit
| Coq_is_Ox86SLHupdate
| Coq_is_Ox86SLHmove
| Coq_is_Ox86SLHprotect of reg_kind * is_reg_kind * wsize * is_wsize

val is_x86_extra_op_rect :
  (wsize -> is_wsize -> 'a1) -> 'a1 -> 'a1 -> (wsize -> is_wsize -> 'a1) ->
  (wsize -> is_wsize -> 'a1) -> 'a1 -> 'a1 -> 'a1 -> (reg_kind -> is_reg_kind
  -> wsize -> is_wsize -> 'a1) -> x86_extra_op -> is_x86_extra_op -> 'a1

val is_x86_extra_op_rec :
  (wsize -> is_wsize -> 'a1) -> 'a1 -> 'a1 -> (wsize -> is_wsize -> 'a1) ->
  (wsize -> is_wsize -> 'a1) -> 'a1 -> 'a1 -> 'a1 -> (reg_kind -> is_reg_kind
  -> wsize -> is_wsize -> 'a1) -> x86_extra_op -> is_x86_extra_op -> 'a1

val x86_extra_op_tag : x86_extra_op -> positive

val is_x86_extra_op_inhab : x86_extra_op -> is_x86_extra_op

val is_x86_extra_op_functor :
  x86_extra_op -> is_x86_extra_op -> is_x86_extra_op

type box_x86_extra_op_Oset0 =
  wsize
  (* singleton inductive, whose constructor was Box_x86_extra_op_Oset0 *)

val coq_Box_x86_extra_op_Oset0_0 : box_x86_extra_op_Oset0 -> wsize

type box_x86_extra_op_Oconcat128 =
| Box_x86_extra_op_Oconcat128

type box_x86_extra_op_Ox86SLHprotect = { coq_Box_x86_extra_op_Ox86SLHprotect_0 : 
                                         reg_kind;
                                         coq_Box_x86_extra_op_Ox86SLHprotect_1 : 
                                         wsize }

val coq_Box_x86_extra_op_Ox86SLHprotect_0 :
  box_x86_extra_op_Ox86SLHprotect -> reg_kind

val coq_Box_x86_extra_op_Ox86SLHprotect_1 :
  box_x86_extra_op_Ox86SLHprotect -> wsize

type x86_extra_op_fields_t = __

val x86_extra_op_fields : x86_extra_op -> x86_extra_op_fields_t

val x86_extra_op_construct :
  positive -> x86_extra_op_fields_t -> x86_extra_op option

val x86_extra_op_induction :
  (wsize -> is_wsize -> 'a1) -> 'a1 -> 'a1 -> (wsize -> is_wsize -> 'a1) ->
  (wsize -> is_wsize -> 'a1) -> 'a1 -> 'a1 -> 'a1 -> (reg_kind -> is_reg_kind
  -> wsize -> is_wsize -> 'a1) -> x86_extra_op -> is_x86_extra_op -> 'a1

val x86_extra_op_eqb_fields :
  (x86_extra_op -> x86_extra_op -> bool) -> positive -> x86_extra_op_fields_t
  -> x86_extra_op_fields_t -> bool

val x86_extra_op_eqb : x86_extra_op -> x86_extra_op -> bool

val x86_extra_op_eqb_OK : x86_extra_op -> x86_extra_op -> reflect

val x86_extra_op_eqb_OK_sumbool : x86_extra_op -> x86_extra_op -> bool

val coq_HB_unnamed_factory_1 : x86_extra_op Coq_hasDecEq.axioms_

val x86_extra_x86_extra_op__canonical__eqtype_Equality : Equality.coq_type

val coq_Oset0_instr :
  (register, register_ext, xmm_register, rflag, condt) arch_toIdent -> wsize
  -> instruction_desc

val coq_Oconcat128_instr : instruction_desc

val coq_Ox86MOVZX32_instr : instruction_desc

val x86_MULX : wsize -> GRing.ComRing.sort -> GRing.ComRing.sort -> sem_tuple

val coq_Ox86MULX_instr :
  (register, register_ext, xmm_register, rflag, condt) arch_toIdent -> wsize
  -> instruction_desc

val x86_MULX_hi :
  wsize -> GRing.ComRing.sort -> GRing.ComRing.sort -> sem_tuple

val coq_Ox86MULX_hi_instr :
  (register, register_ext, xmm_register, rflag, condt) arch_toIdent -> wsize
  -> instruction_desc

val coq_Ox86SLHinit_str : string

val coq_Ox86SLHinit_instr : instruction_desc

val x86_se_update_sem :
  bool -> GRing.ComRing.sort -> GRing.ComRing.sort * GRing.ComRing.sort

val coq_Ox86SLHupdate_str : string

val coq_Ox86SLHupdate_instr : instruction_desc

val coq_Ox86SLHmove_str : string

val coq_Ox86SLHmove_instr : instruction_desc

val se_protect_small_sem :
  wsize -> GRing.ComRing.sort -> GRing.ComRing.sort -> sem_tuple

val se_protect_mmx_sem :
  wsize -> GRing.ComRing.sort -> GRing.ComRing.sort -> GRing.ComRing.sort

val se_protect_large_sem :
  wsize -> GRing.ComRing.sort -> GRing.ComRing.sort ->
  GRing.ComRing.sort * GRing.ComRing.sort

val coq_Ox86SLHprotect_str : string

val coq_Ox86SLHprotect_instr :
  (register, register_ext, xmm_register, rflag, condt) arch_toIdent ->
  reg_kind -> wsize -> instruction_desc

val get_instr_desc :
  (register, register_ext, xmm_register, rflag, condt) arch_toIdent ->
  x86_extra_op -> instruction_desc

val prim_string : (string * x86_extra_op prim_constructor) list

val re_i : wsize -> coq_Z -> rexpr

val re8_0 : rexpr

val re8_1 : rexpr

val assemble_slh_init :
  lexpr list -> (((register, register_ext, xmm_register, rflag, condt,
  x86_op) asm_op_msb_t * lexpr list) * rexpr list) list cexec

val assemble_slh_update :
  instr_info -> lexpr list -> rexpr list -> (((register, register_ext,
  xmm_register, rflag, condt, x86_op) asm_op_msb_t * lexpr list) * rexpr
  list) list cexec

val assemble_slh_protect :
  instr_info -> reg_kind -> wsize -> lexpr list -> rexpr list -> (((register,
  register_ext, xmm_register, rflag, condt, x86_op) asm_op_msb_t * lexpr
  list) * rexpr list) list cexec

val assemble_slh_move :
  lexpr list -> rexpr list -> (((register, register_ext, xmm_register, rflag,
  condt, x86_op) asm_op_msb_t * lexpr list) * rexpr list) list cexec

val assemble_extra :
  instr_info -> x86_extra_op -> lexpr list -> rexpr list -> (((register,
  register_ext, xmm_register, rflag, condt, x86_op) asm_op_msb_t * lexpr
  list) * rexpr list) list cexec

val eqC_x86_extra_op : x86_extra_op eqTypeC

val x86_extra_op_decl :
  (register, register_ext, xmm_register, rflag, condt) arch_toIdent ->
  x86_extra_op asmOp

val x86_extra :
  (register, register_ext, xmm_register, rflag, condt) arch_toIdent ->
  (register, register_ext, xmm_register, rflag, condt, x86_op, x86_extra_op)
  asm_extra

type x86_extended_op =
  (register, register_ext, xmm_register, rflag, condt, x86_op, x86_extra_op)
  extended_op

val coq_Ox86 :
  (register, register_ext, xmm_register, rflag, condt) arch_toIdent -> x86_op
  -> x86_extended_op sopn

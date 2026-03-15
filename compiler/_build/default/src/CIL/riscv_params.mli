open BinNums
open Datatypes
open Arch_decl
open Arch_extra
open Arch_params
open Arch_utils
open Asm_gen
open Compiler_util
open Eqtype
open Expr
open Fexpr
open Lea
open Linearization
open Memory_model
open Riscv_decl
open Riscv_extra
open Riscv_instr_decl
open Riscv_lower_addressing
open Riscv_lowering
open Riscv_params_common
open Riscv_params_core
open Riscv_stack_zeroization
open Seq
open Slh_lowering
open Sopn
open Ssrbool
open Ssrnat
open Stack_alloc_params
open Stack_zeroization
open Type
open Utils0
open Var0
open Word_ssrZ
open Wsize

val riscv_mov_ofs :
  (register, empty, empty, empty, condt) arch_toIdent -> lval -> assgn_tag ->
  mov_kind -> pexpr -> pexpr -> (register, empty, empty, empty, condt,
  riscv_op, riscv_extra_op) extended_op instr_r option

val riscv_immediate :
  (register, empty, empty, empty, condt) arch_toIdent -> var_i -> coq_Z ->
  riscv_extended_op instr_r

val riscv_swap :
  (register, empty, empty, empty, condt) arch_toIdent -> assgn_tag -> var_i
  -> var_i -> var_i -> var_i -> (register, empty, empty, empty, condt,
  riscv_op, riscv_extra_op) extended_op instr_r

val riscv_saparams :
  (register, empty, empty, empty, condt) arch_toIdent -> (register, empty,
  empty, empty, condt, riscv_op, riscv_extra_op) extended_op
  stack_alloc_params

val riscv_allocate_stack_frame :
  (register, empty, empty, empty, condt) arch_toIdent -> var_i -> var_i
  option -> coq_Z -> ((lexpr list * (register, empty, empty, empty, condt,
  riscv_op, riscv_extra_op) extended_op sopn) * rexpr list) list

val riscv_free_stack_frame :
  (register, empty, empty, empty, condt) arch_toIdent -> var_i -> var_i
  option -> coq_Z -> ((lexpr list * (register, empty, empty, empty, condt,
  riscv_op, riscv_extra_op) extended_op sopn) * rexpr list) list

val riscv_set_up_sp_register :
  (register, empty, empty, empty, condt) arch_toIdent -> var_i -> coq_Z ->
  wsize -> var_i -> var_i -> ((lexpr list * (register, empty, empty, empty,
  condt, riscv_op, riscv_extra_op) extended_op sopn) * rexpr list) list

val riscv_tmp :
  (register, empty, empty, empty, condt) arch_toIdent -> Ident.Ident.ident

val riscv_tmp2 :
  (register, empty, empty, empty, condt) arch_toIdent -> Ident.Ident.ident

val riscv_lmove :
  (register, empty, empty, empty, condt) arch_toIdent -> var_i -> var_i ->
  (lexpr list * riscv_extended_op sopn) * rexpr list

val riscv_check_ws : Equality.sort -> bool

val riscv_lstore :
  (register, empty, empty, empty, condt) arch_toIdent -> var_i -> coq_Z ->
  var_i -> (lexpr list * riscv_extended_op sopn) * rexpr list

val riscv_lload :
  (register, empty, empty, empty, condt) arch_toIdent -> var_i -> var_i ->
  coq_Z -> (lexpr list * riscv_extended_op sopn) * rexpr list

val riscv_liparams :
  (register, empty, empty, empty, condt) arch_toIdent -> (register, empty,
  empty, empty, condt, riscv_op, riscv_extra_op) extended_op
  linearization_params

val riscv_loparams :
  (register, empty, empty, empty, condt) arch_toIdent -> ((register, empty,
  empty, empty, condt, riscv_op, riscv_extra_op) extended_op,
  lowering_options) lowering_params

val riscv_shparams :
  (register, empty, empty, empty, condt) arch_toIdent -> (register, empty,
  empty, empty, condt, riscv_op, riscv_extra_op) extended_op sh_params

val condt_not : condt -> condt

val assemble_cond_arg :
  (register, empty, empty, empty, condt) arch_toIdent -> instr_info -> fexpr
  -> register option cexec

val assemble_cond_app2 : sop2 -> (condition_kind * bool) option

val assemble_cond :
  (register, empty, empty, empty, condt) arch_toIdent -> instr_info -> fexpr
  -> condt cexec

val is_valid_address :
  (register, empty, empty, empty, condt) reg_address -> bool

val riscv_agparams :
  (register, empty, empty, empty, condt) arch_toIdent -> (register, empty,
  empty, empty, condt, riscv_op, riscv_extra_op) asm_gen_params

val riscv_szparams :
  (register, empty, empty, empty, condt) arch_toIdent -> (register, empty,
  empty, empty, condt, riscv_op, riscv_extra_op) extended_op
  stack_zeroization_params

val riscv_laparams :
  (register, empty, empty, empty, condt) arch_toIdent -> (register, empty,
  empty, empty, condt, riscv_op, riscv_extra_op) lower_addressing_params

val riscv_is_move_op :
  (register, empty, empty, empty, condt) arch_toIdent -> (register, empty,
  empty, empty, condt, riscv_op, riscv_extra_op) extended_op asm_op_t -> bool

val riscv_params :
  (register, empty, empty, empty, condt) arch_toIdent -> (register, empty,
  empty, empty, condt, riscv_op, riscv_extra_op, lowering_options)
  architecture_params

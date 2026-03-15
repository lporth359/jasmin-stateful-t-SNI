open BinInt
open BinNums
open Datatypes
open Arch_decl
open Arch_extra
open Arch_params
open Asm_gen
open Compiler_util
open Eqtype
open Expr
open Fexpr
open Linearization
open Memory_model
open Seq
open Slh_lowering
open Slh_ops
open Sopn
open Stack_alloc_params
open Stack_zeroization
open Type
open Utils0
open Var0
open Word_ssrZ
open Wsize
open X86_decl
open X86_extra
open X86_instr_decl
open X86_lowering
open X86_stack_zeroization

val x86_op_align :
  (register, register_ext, xmm_register, rflag, condt) arch_toIdent -> var_i
  -> wsize -> wsize -> (lexpr list * (register, register_ext, xmm_register,
  rflag, condt, x86_op, x86_extra_op) extended_op sopn) * rexpr list

val lea_ptr :
  (register, register_ext, xmm_register, rflag, condt) arch_toIdent -> lval
  -> pexpr -> assgn_tag -> pexpr -> (register, register_ext, xmm_register,
  rflag, condt, x86_op, x86_extra_op) extended_op instr_r

val x86_mov_ofs :
  (register, register_ext, xmm_register, rflag, condt) arch_toIdent -> lval
  -> assgn_tag -> mov_kind -> pexpr -> pexpr -> (register, register_ext,
  xmm_register, rflag, condt, x86_op, x86_extra_op) extended_op instr_r option

val x86_immediate :
  (register, register_ext, xmm_register, rflag, condt) arch_toIdent -> var_i
  -> coq_Z -> x86_extended_op instr_r

val x86_swap :
  (register, register_ext, xmm_register, rflag, condt) arch_toIdent ->
  assgn_tag -> var_i -> var_i -> var_i -> var_i -> x86_extended_op instr_r

val x86_saparams :
  (register, register_ext, xmm_register, rflag, condt) arch_toIdent ->
  (register, register_ext, xmm_register, rflag, condt, x86_op, x86_extra_op)
  extended_op stack_alloc_params

val x86_allocate_stack_frame :
  (register, register_ext, xmm_register, rflag, condt) arch_toIdent -> var_i
  -> var_i option -> coq_Z -> ((lexpr list * x86_extended_op sopn) * rexpr
  list) list

val x86_free_stack_frame :
  (register, register_ext, xmm_register, rflag, condt) arch_toIdent -> var_i
  -> var_i option -> coq_Z -> ((lexpr list * x86_extended_op sopn) * rexpr
  list) list

val x86_lassign :
  (register, register_ext, xmm_register, rflag, condt) arch_toIdent -> lexpr
  -> wsize -> rexpr -> (lexpr list * x86_extended_op sopn) * rexpr list

val x86_set_up_sp_register :
  (register, register_ext, xmm_register, rflag, condt) arch_toIdent -> var_i
  -> coq_Z -> wsize -> var_i -> var_i -> ((lexpr list * (register,
  register_ext, xmm_register, rflag, condt, x86_op, x86_extra_op) extended_op
  sopn) * rexpr list) list

val x86_lmove :
  (register, register_ext, xmm_register, rflag, condt) arch_toIdent -> var_i
  -> var_i -> (lexpr list * x86_extended_op sopn) * rexpr list

val x86_check_ws : wsize -> bool

val x86_lstore :
  (register, register_ext, xmm_register, rflag, condt) arch_toIdent -> var_i
  -> coq_Z -> var_i -> (lexpr list * x86_extended_op sopn) * rexpr list

val x86_lload :
  (register, register_ext, xmm_register, rflag, condt) arch_toIdent -> var_i
  -> var_i -> coq_Z -> (lexpr list * x86_extended_op sopn) * rexpr list

val x86_tmp :
  (register, register_ext, xmm_register, rflag, condt) arch_toIdent ->
  Ident.Ident.ident

val x86_liparams :
  (register, register_ext, xmm_register, rflag, condt) arch_toIdent ->
  (register, register_ext, xmm_register, rflag, condt, x86_op, x86_extra_op)
  extended_op linearization_params

val x86_loparams :
  (register, register_ext, xmm_register, rflag, condt) arch_toIdent ->
  ((register, register_ext, xmm_register, rflag, condt, x86_op, x86_extra_op)
  extended_op, lowering_options) lowering_params

val lflags : lval list

val is_mmx_protect : wsize -> lval list -> bool

val x86_sh_lower :
  (register, register_ext, xmm_register, rflag, condt) arch_toIdent -> lval
  list -> slh_op -> pexpr list -> ((lval list * (register, register_ext,
  xmm_register, rflag, condt, x86_op, x86_extra_op) extended_op sopn) * pexpr
  list) option

val x86_shparams :
  (register, register_ext, xmm_register, rflag, condt) arch_toIdent ->
  (register, register_ext, xmm_register, rflag, condt, x86_op, x86_extra_op)
  extended_op sh_params

val not_condt : condt -> condt

val or_condt : instr_info -> fexpr -> condt -> condt -> condt cexec

val and_condt :
  instr_info -> fexpr -> condt -> condt -> (pp_error_loc, condt) result

val of_var_e_bool :
  (register, register_ext, xmm_register, rflag, condt) arch_toIdent ->
  instr_info -> var_i -> rflag cexec

val assemble_cond_r :
  (register, register_ext, xmm_register, rflag, condt) arch_toIdent ->
  instr_info -> fexpr -> condt cexec

val assemble_cond :
  (register, register_ext, xmm_register, rflag, condt) arch_toIdent ->
  instr_info -> fexpr -> condt cexec

val is_valid_address :
  (register, register_ext, xmm_register, rflag, condt) reg_address -> bool

val x86_agparams :
  (register, register_ext, xmm_register, rflag, condt) arch_toIdent ->
  (register, register_ext, xmm_register, rflag, condt, x86_op, x86_extra_op)
  asm_gen_params

val x86_szparams :
  (register, register_ext, xmm_register, rflag, condt) arch_toIdent ->
  (register, register_ext, xmm_register, rflag, condt, x86_op, x86_extra_op)
  extended_op stack_zeroization_params

val x86_is_move_op :
  (register, register_ext, xmm_register, rflag, condt) arch_toIdent ->
  (register, register_ext, xmm_register, rflag, condt, x86_op, x86_extra_op)
  extended_op asm_op_t -> bool

val x86_params :
  (register, register_ext, xmm_register, rflag, condt) arch_toIdent ->
  (register, register_ext, xmm_register, rflag, condt, x86_op, x86_extra_op,
  lowering_options) architecture_params

open BinNums
open Datatypes
open Arch_decl
open Arch_extra
open Arch_utils
open Arm_decl
open Arm_extra
open Arm_instr_decl
open Eqtype
open Expr
open Lowering
open Pseudo_operator
open Seq
open Shift_kind
open Sopn
open Ssralg
open Ssrbool
open Type
open Utils0
open Var0
open Word0
open Wsize

val fv_NF : fresh_vars -> Ident.Ident.ident

val fv_ZF : fresh_vars -> Ident.Ident.ident

val fv_CF : fresh_vars -> Ident.Ident.ident

val fv_VF : fresh_vars -> Ident.Ident.ident

val all_fresh_vars : fresh_vars -> Ident.Ident.ident list

val fvNF : fresh_vars -> Var.var

val fvZF : fresh_vars -> Var.var

val fvCF : fresh_vars -> Var.var

val fvVF : fresh_vars -> Var.var

val fresh_flags : fresh_vars -> Var.var list

val fvars : fresh_vars -> SvExtra.Sv.t

type low_expr =
  ((register, empty, empty, rflag, condt, arm_op, arm_extra_op) extended_op
  sopn * pexpr list) option

val le_skip : (register, empty, empty, rflag, condt) arch_toIdent -> low_expr

val le_issue_sopn :
  (register, empty, empty, rflag, condt) arch_toIdent -> (register, empty,
  empty, rflag, condt, arm_op, arm_extra_op) extended_op sopn -> pexpr list
  -> low_expr

val le_issue_aop :
  (register, empty, empty, rflag, condt) arch_toIdent -> arm_op -> pexpr list
  -> low_expr

val le_issue_opts :
  (register, empty, empty, rflag, condt) arch_toIdent -> arm_mnemonic ->
  arm_options -> pexpr list -> low_expr

val le_issue :
  (register, empty, empty, rflag, condt) arch_toIdent -> arm_mnemonic ->
  pexpr list -> low_expr

val no_pre :
  (register, empty, empty, rflag, condt) arch_toIdent -> low_expr ->
  (((register, empty, empty, rflag, condt, arm_op, arm_extra_op) extended_op
  instr_r list * (register, empty, empty, rflag, condt, arm_op, arm_extra_op)
  extended_op sopn) * pexpr list) option

val chk_ws_reg : wsize -> unit option

val flags_of_mn : fresh_vars -> arm_mnemonic -> Var.var list

val lflags_of_mn : fresh_vars -> var_info -> arm_mnemonic -> lval list

val lower_TST : pexpr -> pexpr -> pexpr list option

val lower_condition_Papp2 :
  fresh_vars -> var_info -> sop2 -> pexpr -> pexpr ->
  ((arm_mnemonic * pexpr) * pexpr list) option

val lower_condition_pexpr :
  (register, empty, empty, rflag, condt) arch_toIdent -> fresh_vars ->
  var_info -> pexpr -> (((lval list * (register, empty, empty, rflag, condt,
  arm_op, arm_extra_op) extended_op sopn) * pexpr list) * pexpr) option

val lower_condition :
  (register, empty, empty, rflag, condt) arch_toIdent -> fresh_vars ->
  var_info -> pexpr -> (register, empty, empty, rflag, condt, arm_op,
  arm_extra_op) extended_op instr_r list * pexpr

val get_arg_shift :
  wsize -> pexpr list -> ((pexpr * shift_kind) * pexpr) option

val arg_shift : arm_mnemonic -> wsize -> pexpr list -> arm_op * pexpr list

val lower_Pvar :
  (register, empty, empty, rflag, condt) arch_toIdent -> wsize -> gvar ->
  low_expr

val lower_load :
  (register, empty, empty, rflag, condt) arch_toIdent -> wsize -> pexpr ->
  low_expr

val mov_imm_op :
  (register, empty, empty, rflag, condt) arch_toIdent -> pexpr -> (register,
  empty, empty, rflag, condt, arm_op, arm_extra_op) extended_op sopn

val lower_Papp1 :
  (register, empty, empty, rflag, condt) arch_toIdent -> wsize -> sop1 ->
  pexpr -> low_expr

val is_mul : pexpr -> (pexpr * pexpr) option

val is_rsb : wsize -> pexpr -> pexpr -> bool

val lower_Papp2_op :
  wsize -> sop2 -> pexpr -> pexpr -> ((arm_mnemonic * pexpr) * pexpr list)
  option

val lower_Papp2 :
  (register, empty, empty, rflag, condt) arch_toIdent -> wsize -> sop2 ->
  pexpr -> pexpr -> low_expr

val lower_pexpr_aux :
  (register, empty, empty, rflag, condt) arch_toIdent -> wsize -> pexpr ->
  low_expr

val sopn_set_is_conditional :
  (register, empty, empty, rflag, condt) arch_toIdent -> (register, empty,
  empty, rflag, condt, arm_op, arm_extra_op) extended_op sopn -> (register,
  empty, empty, rflag, condt, arm_op, arm_extra_op) extended_op sopn option

val lower_pexpr :
  (register, empty, empty, rflag, condt) arch_toIdent -> fresh_vars ->
  var_info -> wsize -> pexpr -> (((register, empty, empty, rflag, condt,
  arm_op, arm_extra_op) extended_op instr_r list * (register, empty, empty,
  rflag, condt, arm_op, arm_extra_op) extended_op sopn) * pexpr list) option

val lower_store : wsize -> pexpr -> (arm_op * pexpr list) option

val lower_cassgn_word :
  (register, empty, empty, rflag, condt) arch_toIdent -> fresh_vars -> lval
  -> wsize -> pexpr -> ((register, empty, empty, rflag, condt, arm_op,
  arm_extra_op) extended_op instr_r list * ((lval list * (register, empty,
  empty, rflag, condt, arm_op, arm_extra_op) extended_op sopn) * pexpr list))
  option

val lower_cassgn_bool :
  (register, empty, empty, rflag, condt) arch_toIdent -> fresh_vars -> lval
  -> assgn_tag -> pexpr -> (register, empty, empty, rflag, condt, arm_op,
  arm_extra_op) extended_op instr_r list option

val lower_add_carry :
  (register, empty, empty, rflag, condt) arch_toIdent -> lval list -> pexpr
  list -> ((lval list * (register, empty, empty, rflag, condt, arm_op,
  arm_extra_op) extended_op sopn) * pexpr list) option

val lower_mulu :
  (register, empty, empty, rflag, condt) arch_toIdent -> lval list -> pexpr
  list -> ((lval list * (register, empty, empty, rflag, condt, arm_op,
  arm_extra_op) extended_op sopn) * pexpr list) option

val with_shift : arm_options -> shift_kind -> arm_options

val lower_base_op :
  (register, empty, empty, rflag, condt) arch_toIdent -> lval list -> arm_op
  -> pexpr list -> ((lval list * (register, empty, empty, rflag, condt,
  arm_op, arm_extra_op) extended_op sopn) * pexpr list) option

val lower_swap :
  (register, empty, empty, rflag, condt) arch_toIdent -> atype -> lval list
  -> pexpr list -> ((lval list * (register, empty, empty, rflag, condt,
  arm_op, arm_extra_op) extended_op sopn) * pexpr list) option

val lower_pseudo_operator :
  (register, empty, empty, rflag, condt) arch_toIdent -> lval list ->
  pseudo_operator -> pexpr list -> ((lval list * (register, empty, empty,
  rflag, condt, arm_op, arm_extra_op) extended_op sopn) * pexpr list) option

val lower_copn :
  (register, empty, empty, rflag, condt) arch_toIdent -> lval list ->
  (register, empty, empty, rflag, condt, arm_op, arm_extra_op) extended_op
  sopn -> pexpr list -> ((lval list * (register, empty, empty, rflag, condt,
  arm_op, arm_extra_op) extended_op sopn) * pexpr list) option

type lowering_options = unit

val lower_i :
  (register, empty, empty, rflag, condt) arch_toIdent -> fresh_vars ->
  (register, empty, empty, rflag, condt, arm_op, arm_extra_op) extended_op
  instr -> (register, empty, empty, rflag, condt, arm_op, arm_extra_op)
  extended_op instr list

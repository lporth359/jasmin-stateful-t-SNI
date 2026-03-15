open BinNums
open Datatypes
open Compiler_util
open Eqtype
open Expr
open Pseudo_operator
open Seq
open Sopn
open Ssrfun
open Ssrnat
open Syscall
open Type
open Utils0
open Var0
open Word0
open Word_ssrZ
open Wsize

module E :
 sig
  val pass : string

  val ierror_s : string -> pp_error_loc

  val ierror : pp_error -> pp_error_loc

  val ierror_e : pexpr -> pp_error_loc

  val ierror_lv : lval -> pp_error_loc
 end

val is_wi1 : sop1 -> (signedness * wiop1) option

val is_wi2 : sop2 -> (signedness * wiop2) option

val wi2i_op2 : sop2 -> sop2

val esubtype : positive extended_type -> positive extended_type -> bool

val wi2i_op1_e : sop1 -> pexpr -> pexpr

val wi2i_op2_e : sop2 -> pexpr -> pexpr -> pexpr

val to_etype : signedness option -> atype -> positive extended_type

val sign_of_var :
  (Var.var -> (signedness * Var.var) option) -> Var.var -> signedness option

val etype_of_var :
  (Var.var -> (signedness * Var.var) option) -> Var.var -> positive
  extended_type

val sign_of_gvar :
  (Var.var -> (signedness * Var.var) option) -> gvar -> signedness option

val etype_of_gvar :
  (Var.var -> (signedness * Var.var) option) -> gvar -> positive extended_type

val sign_of_etype : positive extended_type -> signedness option

val etype_of_expr :
  (Var.var -> (signedness * Var.var) option) -> pexpr -> positive
  extended_type

val sign_of_expr :
  (Var.var -> (signedness * Var.var) option) -> pexpr -> signedness option

val wi2i_var :
  (Var.var -> (signedness * Var.var) option) -> Var.var -> Var.var

val in_FV_var : SvExtra.Sv.t -> Var.var -> bool

val wi2i_vari :
  (Var.var -> (signedness * Var.var) option) -> SvExtra.Sv.t -> var_i ->
  (pp_error_loc, var_i) result

val wi2i_gvar :
  (Var.var -> (signedness * Var.var) option) -> SvExtra.Sv.t -> gvar ->
  (pp_error_loc, gvar) result

val wi2i_type : signedness option -> atype -> atype

val wi2i_e :
  (Var.var -> (signedness * Var.var) option) -> SvExtra.Sv.t -> pexpr ->
  pexpr cexec

val wi2i_lvar :
  (Var.var -> (signedness * Var.var) option) -> SvExtra.Sv.t -> positive
  extended_type -> var_i -> var_i cexec

val wi2i_lv :
  (Var.var -> (signedness * Var.var) option) -> SvExtra.Sv.t -> positive
  extended_type -> lval -> lval cexec

val get_sig :
  (funname -> (positive extended_type list * positive extended_type list)
  option) -> funname -> (pp_error_loc, positive extended_type list * positive
  extended_type list) result

val wi2i_ir :
  'a1 asmOp -> coq_PointerData -> coq_MSFsize -> (Var.var ->
  (signedness * Var.var) option) -> SvExtra.Sv.t -> (funname -> (positive
  extended_type list * positive extended_type list) option) -> 'a1 instr_r ->
  'a1 instr_r cexec

val wi2i_i :
  'a1 asmOp -> coq_PointerData -> coq_MSFsize -> (Var.var ->
  (signedness * Var.var) option) -> SvExtra.Sv.t -> (funname -> (positive
  extended_type list * positive extended_type list) option) -> 'a1 instr ->
  'a1 instr cexec

val wi2i_fun :
  'a1 asmOp -> coq_PointerData -> coq_MSFsize -> (Var.var ->
  (signedness * Var.var) option) -> SvExtra.Sv.t -> (funname -> (positive
  extended_type list * positive extended_type list) option) -> funname -> 'a1
  fundef -> ('a1, extra_fun_t) _fundef cexec

val build_sig :
  'a1 asmOp -> (Var.var -> (signedness * Var.var) option) -> (funname * 'a1
  fundef) -> funname * (positive extended_type list * positive extended_type
  list)

val build_info :
  (Var.var -> (signedness * Var.var) option) -> SvExtra.Sv.t ->
  (pp_error_loc, Equality.sort -> (signedness * Var.var) option) result

val wi2i_prog :
  'a1 asmOp -> coq_PointerData -> coq_MSFsize -> (Var.var ->
  (signedness * Var.var) option) -> 'a1 _uprog -> 'a1 _uprog cexec

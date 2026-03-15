open BinNums
open Datatypes
open Compiler_util
open Constant_prop
open Expr
open Flag_combination
open Seq
open Slh_ops
open Sopn
open Type
open Utils0
open Var0
open Wsize

module E :
 sig
  val pass : string

  val pp_user_error :
    instr_info option -> var_info option -> pp_error -> pp_error_loc

  val cond_not_found : instr_info -> pexpr option -> pexpr -> pp_error_loc

  val lvar_variable : instr_info -> pp_error_loc

  val expr_variable : instr_info -> pexpr -> pp_error_loc

  val msf_not_found_r : var_i -> SvExtra.Sv.t -> pp_error_loc

  val msf_not_found : instr_info -> var_i -> SvExtra.Sv.t -> pp_error_loc

  val invalid_nb_args : pp_error_loc

  val invalid_nb_lvals : pp_error_loc

  val cond_uses_mem : instr_info -> pexpr -> pp_error_loc

  val lowering_failed : instr_info -> pp_error_loc

  val invalid_type_for_msf : instr_info -> pp_error_loc
 end

module Env :
 sig
  type t = { cond : pexpr option; msf_vars : SvExtra.Sv.t }

  val cond : t -> pexpr option

  val msf_vars : t -> SvExtra.Sv.t

  val restrict_cond : pexpr option -> SvExtra.Sv.t -> pexpr option

  val empty : t

  val initial : Var.var option -> t

  val update_cond : coq_FlagCombinationParams -> t -> pexpr -> t

  val meet : t -> t -> t

  val le : t -> t -> bool

  val is_msf_var : t -> Var.var -> bool

  val is_cond : coq_FlagCombinationParams -> t -> pexpr -> bool

  val after_SLHmove : t -> Var.var option -> t

  val after_assign_var : t -> Var.var -> t

  val after_assign_vars : t -> SvExtra.Sv.t -> t
 end

val check_e_msf : instr_info -> Env.t -> pexpr -> (pp_error_loc, unit) result

val check_lv : instr_info -> lval -> (pp_error_loc, Var.var option) result

val check_lv_msf :
  coq_MSFsize -> instr_info -> lval -> (pp_error_loc, Var.var option) result

val check_slho :
  coq_MSFsize -> coq_FlagCombinationParams -> instr_info -> lval list ->
  slh_op -> pexpr list -> Env.t -> Env.t cexec

type slh_t =
| Slh_None
| Slh_msf

val check_f_arg :
  instr_info -> Env.t -> pexpr -> slh_t -> (pp_error_loc, unit) result

val check_f_args :
  instr_info -> Env.t -> pexpr list -> slh_t list -> (pp_error_loc, unit
  list) result

val check_f_lv :
  coq_MSFsize -> instr_info -> Env.t -> lval -> slh_t -> (pp_error_loc,
  Env.t) result

val check_f_lvs :
  coq_MSFsize -> instr_info -> Env.t -> lval list -> slh_t list ->
  (pp_error_loc, Env.t) result

val init_fun_env :
  coq_MSFsize -> Env.t -> var_i list -> atype list -> slh_t list ->
  (pp_error_loc, Env.t) result

val check_res :
  coq_MSFsize -> Env.t -> var_i list -> atype list -> slh_t list ->
  (pp_error_loc, unit) result

val check_for :
  instr_info -> Var.var -> (Env.t -> Env.t cexec) -> nat -> Env.t -> Env.t
  cexec

val check_while :
  coq_FlagCombinationParams -> instr_info -> pexpr -> (Env.t -> Env.t cexec)
  -> (Env.t -> Env.t cexec) -> nat -> Env.t -> Env.t cexec

type 'asm_op sh_params =
  lval list -> slh_op -> pexpr list -> ((lval list * 'asm_op sopn) * pexpr
  list) option
  (* singleton inductive, whose constructor was Build_sh_params *)

val check_i :
  'a1 asmOp -> coq_MSFsize -> coq_FlagCombinationParams -> (funname -> slh_t
  list * slh_t list) -> 'a1 instr -> Env.t -> Env.t cexec

val check_cmd :
  'a1 asmOp -> coq_MSFsize -> coq_FlagCombinationParams -> (funname -> slh_t
  list * slh_t list) -> Env.t -> 'a1 instr list -> Env.t cexec

val check_fd :
  'a1 asmOp -> coq_MSFsize -> coq_FlagCombinationParams -> progT -> (funname
  -> slh_t list * slh_t list) -> funname -> 'a1 fundef -> unit cexec

val lower_slho :
  'a1 asmOp -> 'a1 sh_params -> instr_info -> lval list -> assgn_tag ->
  slh_op -> pexpr list -> 'a1 instr_r cexec

val lower_i : 'a1 asmOp -> 'a1 sh_params -> 'a1 instr -> 'a1 instr cexec

val lower_cmd :
  'a1 asmOp -> 'a1 sh_params -> 'a1 instr list -> 'a1 instr list cexec

val lower_fd :
  'a1 asmOp -> coq_MSFsize -> coq_FlagCombinationParams -> progT -> 'a1
  sh_params -> (funname -> slh_t list * slh_t list) -> funname -> 'a1 fundef
  -> (pp_error_loc, ('a1, extra_fun_t) _fundef) result

val is_slh_none : slh_t -> bool

val lower_slh_prog :
  'a1 asmOp -> coq_MSFsize -> coq_FlagCombinationParams -> progT -> 'a1
  sh_params -> (funname -> slh_t list * slh_t list) -> funname list -> 'a1
  prog -> 'a1 prog cexec

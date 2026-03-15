open Datatypes
open Uint0
open Compiler_util
open Eqtype
open Expr
open Pseudo_operator
open Seq
open Sopn
open Ssrnat
open Syscall
open Type
open Utils0
open Var0

module E :
 sig
  val pass : string

  val make_ref_error : instr_info -> string -> pp_error_loc
 end

val with_id :
  (instr_info -> Uint63.t -> string -> atype -> Ident.Ident.ident) ->
  var_info -> instr_info -> Uint63.t -> string -> atype -> var_i

val is_reg_ptr_expr :
  (instr_info -> Uint63.t -> string -> atype -> Ident.Ident.ident) -> bool ->
  instr_info -> Uint63.t -> string -> atype -> pexpr -> var_i option

val is_reg_ptr_lval :
  (instr_info -> Uint63.t -> string -> atype -> Ident.Ident.ident) -> bool ->
  instr_info -> Uint63.t -> string -> atype -> lval -> var_i option

val make_prologue :
  'a1 asmOp -> (instr_info -> Uint63.t -> string -> atype ->
  Ident.Ident.ident) -> instr_info -> SvExtra.Sv.t -> Uint63.t ->
  ((bool * string) * atype) list -> pexpr list -> (pp_error_loc, 'a1 instr
  list * pexpr list) result

type pseudo_instr =
| PI_lv of lval
| PI_i of lval * atype * var_i

val make_pseudo_epilogue :
  (instr_info -> Uint63.t -> string -> atype -> Ident.Ident.ident) ->
  instr_info -> SvExtra.Sv.t -> Uint63.t -> ((bool * string) * atype) list ->
  lval list -> (pp_error_loc, pseudo_instr list) result

val mk_ep_i : 'a1 asmOp -> instr_info -> lval -> atype -> var_i -> 'a1 instr

val wf_lv : lval -> bool

val swapable :
  'a1 asmOp -> instr_info -> pseudo_instr list -> (pp_error_loc, lval
  list * 'a1 instr list) result

val make_epilogue :
  'a1 asmOp -> (instr_info -> Uint63.t -> string -> atype ->
  Ident.Ident.ident) -> instr_info -> SvExtra.Sv.t ->
  ((bool * string) * atype) list -> lval list -> (pp_error_loc, lval
  list * 'a1 instr list) result

val update_c :
  'a1 asmOp -> ('a1 instr -> 'a1 instr list cexec) -> 'a1 instr list ->
  (pp_error_loc, 'a1 instr list) result

val mk_info : var_i -> atype -> (bool * string) * atype

val get_sig :
  'a1 asmOp -> 'a1 uprog -> instr_info -> funname -> (pp_error_loc,
  ((bool * string) * atype) list * ((bool * string) * atype) list) result

val get_syscall_sig :
  (Wsize.wsize * BinNums.positive) Syscall_t.syscall_t ->
  ((bool * string) * atype) list * ((bool * string) * atype) list

val is_swap_op : 'a1 asmOp -> 'a1 sopn -> atype option

val update_i :
  'a1 asmOp -> (instr_info -> Uint63.t -> string -> atype ->
  Ident.Ident.ident) -> 'a1 uprog -> SvExtra.Sv.t -> 'a1 instr -> 'a1 instr
  list cexec

val update_fd :
  'a1 asmOp -> (instr_info -> Uint63.t -> string -> atype ->
  Ident.Ident.ident) -> 'a1 uprog -> 'a1 ufundef -> (pp_error_loc, ('a1,
  extra_fun_t) _fundef) result

val makereference_prog :
  'a1 asmOp -> (instr_info -> Uint63.t -> string -> atype ->
  Ident.Ident.ident) -> 'a1 uprog -> 'a1 uprog cexec

open Datatypes
open Compiler_util
open Expr
open Sopn
open Utils0
open Var0

module E :
 sig
  val pass : string

  val for_loop_remains : pp_error_loc

  val inline_instr_remains : pp_error_loc
 end

val check_no_for_loop_cmd :
  'a1 asmOp -> ('a1 instr -> unit cexec) -> 'a1 instr list -> (pp_error_loc,
  unit) result

val check_no_for_loop_instr : 'a1 asmOp -> 'a1 instr -> unit cexec

val check_no_for_loop_fd : 'a1 asmOp -> (funname * 'a1 ufundef) -> unit cexec

val check_no_for_loop : 'a1 asmOp -> 'a1 uprog -> (pp_error_loc, unit) result

val check_no_inline_instr_cmd :
  'a1 asmOp -> ('a1 instr -> unit cexec) -> 'a1 instr list -> (pp_error_loc,
  unit) result

val check_no_inline_instr_instr : 'a1 asmOp -> 'a1 instr -> unit cexec

val check_no_inline_instr_fd :
  'a1 asmOp -> (funname * 'a1 ufundef) -> unit cexec

val check_no_inline_instr :
  'a1 asmOp -> 'a1 uprog -> (pp_error_loc, unit) result

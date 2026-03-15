open Allocation
open Compiler_util
open Expr
open Sem_type
open Seq
open Sopn
open Utils0
open Var0
open Wsize

val wi2w_wiop1 : signedness -> wiop1 -> pexpr -> pexpr

val wi2w_op1 : sop1 -> pexpr -> pexpr

val wi2w_wiop2 : signedness -> wsize -> wiop2 -> sop2

val wi2w_op2 : sop2 -> sop2

val wi2w_e : pexpr -> pexpr

val wi2w_lv : lval -> lval

val wi2w_i : 'a1 asmOp -> 'a1 instr -> 'a1 instr

val wi2w_fun : 'a1 asmOp -> ('a1, 'a2) _fundef -> ('a1, 'a2) _fundef

val wi2w_prog_internal : 'a1 asmOp -> progT -> 'a1 prog -> 'a1 prog

val wi2w_prog :
  'a1 asmOp -> coq_WithSubWord -> (funname -> 'a1 fundef -> 'a1 fundef) ->
  ('a1 fun_decl -> instr_info -> SvExtra.Sv.t) -> 'a1 prog -> (pp_error_loc,
  'a1 prog) result

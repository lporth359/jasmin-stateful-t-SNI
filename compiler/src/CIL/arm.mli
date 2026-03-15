open Datatypes
open Arch_decl
open Arch_utils
open Arm_decl
open Arm_instr_decl
open Eqtype
open Utils0

val arm_eval_cond :
  (rflag -> (error, bool) result) -> condt -> (error, bool) result

val arm : (register, empty, empty, rflag, condt, arm_op) asm

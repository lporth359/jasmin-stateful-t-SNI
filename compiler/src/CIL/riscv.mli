open BinNums
open Datatypes
open Arch_decl
open Arch_utils
open Eqtype
open Riscv_decl
open Riscv_instr_decl
open Ssralg
open Utils0
open Word0

val sem_cond_arg :
  (register -> GRing.ComRing.sort) -> register option -> GRing.ComRing.sort

val sem_cond_kind :
  condition_kind -> GRing.ComRing.sort -> GRing.ComRing.sort -> bool

val riscv_eval_cond :
  (register -> GRing.ComRing.sort) -> condt -> (error, bool) result

val riscv : (register, empty, empty, empty, condt, riscv_op) asm

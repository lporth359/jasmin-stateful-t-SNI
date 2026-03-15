open BinInt
open Datatypes
open Arch_decl
open Arch_extra
open Arch_utils
open Compiler_util
open Eqtype
open Expr
open Lea
open Memory_model
open Riscv_decl
open Riscv_extra
open Riscv_instr_decl
open Seq
open Ssrfun
open Type
open Utils0
open Var0
open Word0
open Wsize

module E :
 sig
  val pass_name : string

  val error : string -> pp_error_loc
 end

val is_one_Lmem : lval list -> (((aligned * wsize) * var_info) * pexpr) option

val is_one_Pload : pexpr list -> ((aligned * wsize) * pexpr) option

val compute_addr :
  (register, empty, empty, empty, condt) arch_toIdent -> var_i -> pexpr ->
  (riscv_extended_op instr_r list * pexpr) option

val lower_addressing_i :
  (register, empty, empty, empty, condt) arch_toIdent -> var_i -> (register,
  empty, empty, empty, condt, riscv_op, riscv_extra_op) extended_op instr ->
  (register, empty, empty, empty, condt, riscv_op, riscv_extra_op)
  extended_op instr list

val lower_addressing_c :
  (register, empty, empty, empty, condt) arch_toIdent -> var_i -> (register,
  empty, empty, empty, condt, riscv_op, riscv_extra_op) extended_op instr
  list -> (register, empty, empty, empty, condt, riscv_op, riscv_extra_op)
  extended_op instr list

val lower_addressing_fd :
  (register, empty, empty, empty, condt) arch_toIdent -> progT -> var_i ->
  (register, empty, empty, empty, condt, riscv_op, riscv_extra_op)
  extended_op fundef -> (pp_error_loc, ((register, empty, empty, empty,
  condt, riscv_op, riscv_extra_op) extended_op, extra_fun_t) _fundef) result

val lower_addressing_prog :
  (register, empty, empty, empty, condt) arch_toIdent -> progT -> (string ->
  atype -> Ident.Ident.ident) -> (register, empty, empty, empty, condt,
  riscv_op, riscv_extra_op) extended_op prog -> (register, empty, empty,
  empty, condt, riscv_op, riscv_extra_op) extended_op prog cexec

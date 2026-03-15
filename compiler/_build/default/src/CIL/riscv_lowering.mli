open BinInt
open BinNums
open Datatypes
open Arch_decl
open Arch_extra
open Arch_utils
open Eqtype
open Expr
open Lowering
open Pseudo_operator
open Riscv_decl
open Riscv_extra
open Riscv_instr_decl
open Riscv_params_core
open Seq
open Sopn
open Ssralg
open Ssrbool
open Type
open Utils0
open Var0
open Word0
open Wsize

val chk_ws_reg : wsize -> unit option

val check_shift_amount : pexpr -> pexpr option

val lower_Papp1 :
  (register, empty, empty, empty, condt) arch_toIdent -> wsize -> sop1 ->
  pexpr -> (riscv_extended_op * pexpr list) option

val decide_op_reg_imm :
  (register, empty, empty, empty, condt) arch_toIdent -> wsize -> pexpr ->
  pexpr -> riscv_extended_op -> riscv_extended_op ->
  (riscv_extended_op * pexpr list) option

val insert_minus : pexpr -> pexpr option

val decide_op_reg_imm_neg :
  (register, empty, empty, empty, condt) arch_toIdent -> wsize -> pexpr ->
  pexpr -> riscv_extended_op -> riscv_extended_op ->
  (riscv_extended_op * pexpr list) option

val lower_Papp2 :
  (register, empty, empty, empty, condt) arch_toIdent -> wsize -> sop2 ->
  pexpr -> pexpr -> (riscv_extended_op * pexpr list) option

val lower_load :
  (register, empty, empty, empty, condt) arch_toIdent -> wsize -> pexpr ->
  (riscv_extended_op * pexpr list) option

val lower_Pvar :
  (register, empty, empty, empty, condt) arch_toIdent -> wsize -> gvar ->
  (riscv_extended_op * pexpr list) option

val lower_cassgn :
  (register, empty, empty, empty, condt) arch_toIdent -> lval -> wsize ->
  pexpr -> ((lval list * (register, empty, empty, empty, condt, riscv_op,
  riscv_extra_op) extended_op sopn) * pexpr list) option

val lower_swap :
  (register, empty, empty, empty, condt) arch_toIdent -> atype -> lval list
  -> pexpr list -> ((lval list * (register, empty, empty, empty, condt,
  riscv_op, riscv_extra_op) extended_op sopn) * pexpr list) list option

val lower_mulu :
  (register, empty, empty, empty, condt) arch_toIdent -> lval list -> pexpr
  list -> ((lval list * (register, empty, empty, empty, condt, riscv_op,
  riscv_extra_op) extended_op sopn) * pexpr list) list option

val lower_pseudo_operator :
  (register, empty, empty, empty, condt) arch_toIdent -> lval list ->
  pseudo_operator -> pexpr list -> ((lval list * (register, empty, empty,
  empty, condt, riscv_op, riscv_extra_op) extended_op sopn) * pexpr list)
  list option

val lower_copn :
  (register, empty, empty, empty, condt) arch_toIdent -> lval list ->
  (register, empty, empty, empty, condt, riscv_op, riscv_extra_op)
  extended_op sopn -> pexpr list -> ((lval list * (register, empty, empty,
  empty, condt, riscv_op, riscv_extra_op) extended_op sopn) * pexpr list)
  list option

type lowering_options = unit

val lower_i :
  (register, empty, empty, empty, condt) arch_toIdent -> (register, empty,
  empty, empty, condt, riscv_op, riscv_extra_op) extended_op instr ->
  (register, empty, empty, empty, condt, riscv_op, riscv_extra_op)
  extended_op instr list

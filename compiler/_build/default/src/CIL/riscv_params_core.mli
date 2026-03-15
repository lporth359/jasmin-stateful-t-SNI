open BinInt
open BinNums
open Arch_decl
open Eqtype
open Expr
open Fexpr
open Riscv_decl
open Riscv_instr_decl
open Var0

val is_arith_small : coq_Z -> bool

val is_arith_small_neg : coq_Z -> bool

module RISCVFopn_core :
 sig
  type opn_args = (lexpr list * riscv_op) * rexpr list

  val op_gen : riscv_op -> var_i -> rexpr list -> opn_args

  val op_un_reg : riscv_op -> var_i -> var_i -> opn_args

  val op_un_imm : riscv_op -> var_i -> coq_Z -> opn_args

  val op_bin_reg : riscv_op -> var_i -> var_i -> var_i -> opn_args

  val op_bin_imm : riscv_op -> var_i -> var_i -> coq_Z -> opn_args

  val neg_op_bin_imm : riscv_op -> var_i -> var_i -> coq_Z -> opn_args

  val mov : var_i -> var_i -> opn_args

  val add : var_i -> var_i -> var_i -> opn_args

  val sub : var_i -> var_i -> var_i -> opn_args

  val li : var_i -> coq_Z -> opn_args

  val addi : var_i -> var_i -> coq_Z -> opn_args

  val subi : var_i -> var_i -> coq_Z -> opn_args

  val andi : var_i -> var_i -> coq_Z -> opn_args

  val smart_mov : var_i -> var_i -> opn_args list

  val gen_smart_opi :
    (var_i -> var_i -> var_i -> opn_args) -> (var_i -> var_i -> coq_Z ->
    opn_args) -> (coq_Z -> bool) -> coq_Z option -> var_i -> var_i -> var_i
    -> coq_Z -> opn_args list

  val smart_addi : var_i -> var_i -> coq_Z -> opn_args list

  val smart_subi : var_i -> var_i -> coq_Z -> opn_args list

  val gen_smart_opi_tmp :
    (coq_Z -> bool) -> (var_i -> var_i -> var_i -> opn_args) -> (var_i ->
    var_i -> coq_Z -> opn_args) -> var_i -> var_i -> coq_Z -> opn_args list

  val smart_addi_tmp : var_i -> var_i -> coq_Z -> opn_args list

  val smart_subi_tmp : var_i -> var_i -> coq_Z -> opn_args list
 end

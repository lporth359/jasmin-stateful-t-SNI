open BinInt
open BinNums
open Datatypes
open Arch_decl
open Arm_decl
open Arm_expand_imm
open Arm_instr_decl
open Eqtype
open Expr
open Fexpr
open Seq
open Var0
open Word0
open Wsize

val is_arith_small : coq_Z -> bool

val coq_Z_mod_lnot : coq_Z -> wsize -> coq_Z

module ARMFopn_core :
 sig
  type opn_args = (lexpr list * arm_op) * rexpr list

  val mov : var_i -> var_i -> opn_args

  val add : var_i -> var_i -> var_i -> opn_args

  val sub : var_i -> var_i -> var_i -> opn_args

  val movi : var_i -> coq_Z -> opn_args

  val mvni : var_i -> coq_Z -> opn_args

  val movt : var_i -> coq_Z -> opn_args

  val addi : var_i -> var_i -> coq_Z -> opn_args

  val subi : var_i -> var_i -> coq_Z -> opn_args

  val bici : var_i -> var_i -> coq_Z -> opn_args

  val li : var_i -> coq_Z -> opn_args list

  val smart_mov : var_i -> var_i -> opn_args list

  val gen_smart_opi :
    (var_i -> var_i -> var_i -> opn_args) -> (var_i -> var_i -> coq_Z ->
    opn_args) -> (coq_Z -> bool) -> coq_Z option -> var_i -> var_i -> var_i
    -> coq_Z -> opn_args list

  val smart_addi : var_i -> var_i -> coq_Z -> opn_args list

  val smart_subi : var_i -> var_i -> coq_Z -> opn_args list

  val gen_smart_opi_tmp :
    (var_i -> var_i -> var_i -> opn_args) -> (var_i -> var_i -> coq_Z ->
    opn_args) -> var_i -> var_i -> coq_Z -> opn_args list

  val smart_addi_tmp : var_i -> var_i -> coq_Z -> opn_args list

  val smart_subi_tmp : var_i -> var_i -> coq_Z -> opn_args list
 end

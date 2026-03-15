open BinInt
open BinNums
open Arch_decl
open Arch_extra
open Arch_utils
open Arm_decl
open Arm_extra
open Arm_instr_decl
open Arm_params_core
open Expr
open Fexpr
open Seq
open Sopn
open Wsize

module ARMFopn :
 sig
  val to_opn :
    (register, empty, empty, rflag, condt) arch_toIdent -> ((lexpr
    list * arm_op) * rexpr list) -> (lexpr list * arm_extended_op
    sopn) * rexpr list

  val mov :
    (register, empty, empty, rflag, condt) arch_toIdent -> var_i -> var_i ->
    (lexpr list * arm_extended_op sopn) * rexpr list

  val sub :
    (register, empty, empty, rflag, condt) arch_toIdent -> var_i -> var_i ->
    var_i -> (lexpr list * arm_extended_op sopn) * rexpr list

  val movi :
    (register, empty, empty, rflag, condt) arch_toIdent -> var_i -> coq_Z ->
    (lexpr list * arm_extended_op sopn) * rexpr list

  val addi :
    (register, empty, empty, rflag, condt) arch_toIdent -> var_i -> var_i ->
    coq_Z -> (lexpr list * arm_extended_op sopn) * rexpr list

  val subi :
    (register, empty, empty, rflag, condt) arch_toIdent -> var_i -> var_i ->
    coq_Z -> (lexpr list * arm_extended_op sopn) * rexpr list

  val bici :
    (register, empty, empty, rflag, condt) arch_toIdent -> var_i -> var_i ->
    coq_Z -> (lexpr list * arm_extended_op sopn) * rexpr list

  val align :
    (register, empty, empty, rflag, condt) arch_toIdent -> var_i -> var_i ->
    wsize -> (lexpr list * arm_extended_op sopn) * rexpr list

  val li :
    (register, empty, empty, rflag, condt) arch_toIdent -> var_i -> coq_Z ->
    (lexpr list * (register, empty, empty, rflag, condt, arm_op,
    arm_extra_op) extended_op sopn) * rexpr list

  val smart_addi :
    (register, empty, empty, rflag, condt) arch_toIdent -> var_i -> var_i ->
    coq_Z -> ((lexpr list * arm_extended_op sopn) * rexpr list) list

  val smart_subi :
    (register, empty, empty, rflag, condt) arch_toIdent -> var_i -> var_i ->
    coq_Z -> ((lexpr list * arm_extended_op sopn) * rexpr list) list

  val smart_addi_tmp :
    (register, empty, empty, rflag, condt) arch_toIdent -> var_i -> var_i ->
    coq_Z -> ((lexpr list * arm_extended_op sopn) * rexpr list) list

  val smart_subi_tmp :
    (register, empty, empty, rflag, condt) arch_toIdent -> var_i -> var_i ->
    coq_Z -> ((lexpr list * arm_extended_op sopn) * rexpr list) list
 end

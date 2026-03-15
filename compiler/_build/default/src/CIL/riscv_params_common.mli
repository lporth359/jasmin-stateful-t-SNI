open BinInt
open BinNums
open Arch_extra
open Arch_utils
open Expr
open Fexpr
open Riscv_decl
open Riscv_extra
open Riscv_instr_decl
open Riscv_params_core
open Seq
open Sopn
open Wsize

module RISCVFopn :
 sig
  val to_opn :
    (register, empty, empty, empty, condt) arch_toIdent -> ((lexpr
    list * riscv_op) * rexpr list) -> (lexpr list * (register, empty, empty,
    empty, condt, riscv_op, riscv_extra_op) extended_op sopn) * rexpr list

  val mov :
    (register, empty, empty, empty, condt) arch_toIdent -> var_i -> var_i ->
    (lexpr list * (register, empty, empty, empty, condt, riscv_op,
    riscv_extra_op) extended_op sopn) * rexpr list

  val add :
    (register, empty, empty, empty, condt) arch_toIdent -> var_i -> var_i ->
    var_i -> (lexpr list * (register, empty, empty, empty, condt, riscv_op,
    riscv_extra_op) extended_op sopn) * rexpr list

  val sub :
    (register, empty, empty, empty, condt) arch_toIdent -> var_i -> var_i ->
    var_i -> (lexpr list * (register, empty, empty, empty, condt, riscv_op,
    riscv_extra_op) extended_op sopn) * rexpr list

  val li :
    (register, empty, empty, empty, condt) arch_toIdent -> var_i -> coq_Z ->
    (lexpr list * (register, empty, empty, empty, condt, riscv_op,
    riscv_extra_op) extended_op sopn) * rexpr list

  val addi :
    (register, empty, empty, empty, condt) arch_toIdent -> var_i -> var_i ->
    coq_Z -> (lexpr list * (register, empty, empty, empty, condt, riscv_op,
    riscv_extra_op) extended_op sopn) * rexpr list

  val subi :
    (register, empty, empty, empty, condt) arch_toIdent -> var_i -> var_i ->
    coq_Z -> (lexpr list * (register, empty, empty, empty, condt, riscv_op,
    riscv_extra_op) extended_op sopn) * rexpr list

  val andi :
    (register, empty, empty, empty, condt) arch_toIdent -> var_i -> var_i ->
    coq_Z -> (lexpr list * (register, empty, empty, empty, condt, riscv_op,
    riscv_extra_op) extended_op sopn) * rexpr list

  val align :
    (register, empty, empty, empty, condt) arch_toIdent -> var_i -> var_i ->
    wsize -> (lexpr list * (register, empty, empty, empty, condt, riscv_op,
    riscv_extra_op) extended_op sopn) * rexpr list

  val smart_addi :
    (register, empty, empty, empty, condt) arch_toIdent -> var_i -> var_i ->
    coq_Z -> ((lexpr list * (register, empty, empty, empty, condt, riscv_op,
    riscv_extra_op) extended_op sopn) * rexpr list) list

  val smart_subi :
    (register, empty, empty, empty, condt) arch_toIdent -> var_i -> var_i ->
    coq_Z -> ((lexpr list * (register, empty, empty, empty, condt, riscv_op,
    riscv_extra_op) extended_op sopn) * rexpr list) list

  val smart_addi_tmp :
    (register, empty, empty, empty, condt) arch_toIdent -> var_i -> var_i ->
    coq_Z -> ((lexpr list * (register, empty, empty, empty, condt, riscv_op,
    riscv_extra_op) extended_op sopn) * rexpr list) list

  val smart_subi_tmp :
    (register, empty, empty, empty, condt) arch_toIdent -> var_i -> var_i ->
    coq_Z -> ((lexpr list * (register, empty, empty, empty, condt, riscv_op,
    riscv_extra_op) extended_op sopn) * rexpr list) list
 end

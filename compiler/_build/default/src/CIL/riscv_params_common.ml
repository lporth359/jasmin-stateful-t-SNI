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

module RISCVFopn =
 struct
  (** val to_opn :
      (register, empty, empty, empty, condt) arch_toIdent -> ((lexpr
      list * riscv_op) * rexpr list) -> (lexpr list * (register, empty,
      empty, empty, condt, riscv_op, riscv_extra_op) extended_op
      sopn) * rexpr list **)

  let to_opn _ = function
  | (y, e) -> let (d, o) = y in ((d, (Oasm (BaseOp (None, o)))), e)

  (** val mov :
      (register, empty, empty, empty, condt) arch_toIdent -> var_i -> var_i
      -> (lexpr list * (register, empty, empty, empty, condt, riscv_op,
      riscv_extra_op) extended_op sopn) * rexpr list **)

  let mov atoI x y =
    to_opn atoI (RISCVFopn_core.mov x y)

  (** val add :
      (register, empty, empty, empty, condt) arch_toIdent -> var_i -> var_i
      -> var_i -> (lexpr list * (register, empty, empty, empty, condt,
      riscv_op, riscv_extra_op) extended_op sopn) * rexpr list **)

  let add atoI x y z =
    to_opn atoI (RISCVFopn_core.add x y z)

  (** val sub :
      (register, empty, empty, empty, condt) arch_toIdent -> var_i -> var_i
      -> var_i -> (lexpr list * (register, empty, empty, empty, condt,
      riscv_op, riscv_extra_op) extended_op sopn) * rexpr list **)

  let sub atoI x y z =
    to_opn atoI (RISCVFopn_core.sub x y z)

  (** val li :
      (register, empty, empty, empty, condt) arch_toIdent -> var_i -> coq_Z
      -> (lexpr list * (register, empty, empty, empty, condt, riscv_op,
      riscv_extra_op) extended_op sopn) * rexpr list **)

  let li atoI x imm =
    to_opn atoI (RISCVFopn_core.li x imm)

  (** val addi :
      (register, empty, empty, empty, condt) arch_toIdent -> var_i -> var_i
      -> coq_Z -> (lexpr list * (register, empty, empty, empty, condt,
      riscv_op, riscv_extra_op) extended_op sopn) * rexpr list **)

  let addi atoI x y imm =
    to_opn atoI (RISCVFopn_core.addi x y imm)

  (** val subi :
      (register, empty, empty, empty, condt) arch_toIdent -> var_i -> var_i
      -> coq_Z -> (lexpr list * (register, empty, empty, empty, condt,
      riscv_op, riscv_extra_op) extended_op sopn) * rexpr list **)

  let subi atoI x y imm =
    to_opn atoI (RISCVFopn_core.subi x y imm)

  (** val andi :
      (register, empty, empty, empty, condt) arch_toIdent -> var_i -> var_i
      -> coq_Z -> (lexpr list * (register, empty, empty, empty, condt,
      riscv_op, riscv_extra_op) extended_op sopn) * rexpr list **)

  let andi atoI x y imm =
    to_opn atoI (RISCVFopn_core.andi x y imm)

  (** val align :
      (register, empty, empty, empty, condt) arch_toIdent -> var_i -> var_i
      -> wsize -> (lexpr list * (register, empty, empty, empty, condt,
      riscv_op, riscv_extra_op) extended_op sopn) * rexpr list **)

  let align atoI x y al =
    andi atoI x y (Z.opp (wsize_size al))

  (** val smart_addi :
      (register, empty, empty, empty, condt) arch_toIdent -> var_i -> var_i
      -> coq_Z -> ((lexpr list * (register, empty, empty, empty, condt,
      riscv_op, riscv_extra_op) extended_op sopn) * rexpr list) list **)

  let smart_addi atoI x y imm =
    map (to_opn atoI) (RISCVFopn_core.smart_addi x y imm)

  (** val smart_subi :
      (register, empty, empty, empty, condt) arch_toIdent -> var_i -> var_i
      -> coq_Z -> ((lexpr list * (register, empty, empty, empty, condt,
      riscv_op, riscv_extra_op) extended_op sopn) * rexpr list) list **)

  let smart_subi atoI x y imm =
    map (to_opn atoI) (RISCVFopn_core.smart_subi x y imm)

  (** val smart_addi_tmp :
      (register, empty, empty, empty, condt) arch_toIdent -> var_i -> var_i
      -> coq_Z -> ((lexpr list * (register, empty, empty, empty, condt,
      riscv_op, riscv_extra_op) extended_op sopn) * rexpr list) list **)

  let smart_addi_tmp atoI x tmp imm =
    map (to_opn atoI) (RISCVFopn_core.smart_addi_tmp x tmp imm)

  (** val smart_subi_tmp :
      (register, empty, empty, empty, condt) arch_toIdent -> var_i -> var_i
      -> coq_Z -> ((lexpr list * (register, empty, empty, empty, condt,
      riscv_op, riscv_extra_op) extended_op sopn) * rexpr list) list **)

  let smart_subi_tmp atoI x tmp imm =
    map (to_opn atoI) (RISCVFopn_core.smart_subi_tmp x tmp imm)
 end

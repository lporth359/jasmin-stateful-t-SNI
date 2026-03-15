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

module ARMFopn =
 struct
  (** val to_opn :
      (register, empty, empty, rflag, condt) arch_toIdent -> ((lexpr
      list * arm_op) * rexpr list) -> (lexpr list * arm_extended_op
      sopn) * rexpr list **)

  let to_opn atoI = function
  | (y, e) -> let (d, o) = y in ((d, (coq_Oarm atoI o)), e)

  (** val mov :
      (register, empty, empty, rflag, condt) arch_toIdent -> var_i -> var_i
      -> (lexpr list * arm_extended_op sopn) * rexpr list **)

  let mov atoI x y =
    to_opn atoI (ARMFopn_core.mov x y)

  (** val sub :
      (register, empty, empty, rflag, condt) arch_toIdent -> var_i -> var_i
      -> var_i -> (lexpr list * arm_extended_op sopn) * rexpr list **)

  let sub atoI x y z =
    to_opn atoI (ARMFopn_core.sub x y z)

  (** val movi :
      (register, empty, empty, rflag, condt) arch_toIdent -> var_i -> coq_Z
      -> (lexpr list * arm_extended_op sopn) * rexpr list **)

  let movi atoI x imm =
    to_opn atoI (ARMFopn_core.movi x imm)

  (** val addi :
      (register, empty, empty, rflag, condt) arch_toIdent -> var_i -> var_i
      -> coq_Z -> (lexpr list * arm_extended_op sopn) * rexpr list **)

  let addi atoI x y imm =
    to_opn atoI (ARMFopn_core.addi x y imm)

  (** val subi :
      (register, empty, empty, rflag, condt) arch_toIdent -> var_i -> var_i
      -> coq_Z -> (lexpr list * arm_extended_op sopn) * rexpr list **)

  let subi atoI x y imm =
    to_opn atoI (ARMFopn_core.subi x y imm)

  (** val bici :
      (register, empty, empty, rflag, condt) arch_toIdent -> var_i -> var_i
      -> coq_Z -> (lexpr list * arm_extended_op sopn) * rexpr list **)

  let bici atoI x y imm =
    to_opn atoI (ARMFopn_core.bici x y imm)

  (** val align :
      (register, empty, empty, rflag, condt) arch_toIdent -> var_i -> var_i
      -> wsize -> (lexpr list * arm_extended_op sopn) * rexpr list **)

  let align atoI x y al =
    bici atoI x y (Z.sub (wsize_size al) (Zpos Coq_xH))

  (** val li :
      (register, empty, empty, rflag, condt) arch_toIdent -> var_i -> coq_Z
      -> (lexpr list * (register, empty, empty, rflag, condt, arm_op,
      arm_extra_op) extended_op sopn) * rexpr list **)

  let li _ x imm =
    let op = Oasm (ExtOp (Osmart_li arm_decl.reg_size)) in
    ((((LLvar x) :: []), op), ((rconst arm_decl.reg_size imm) :: []))

  (** val smart_addi :
      (register, empty, empty, rflag, condt) arch_toIdent -> var_i -> var_i
      -> coq_Z -> ((lexpr list * arm_extended_op sopn) * rexpr list) list **)

  let smart_addi atoI x y imm =
    map (to_opn atoI) (ARMFopn_core.smart_addi x y imm)

  (** val smart_subi :
      (register, empty, empty, rflag, condt) arch_toIdent -> var_i -> var_i
      -> coq_Z -> ((lexpr list * arm_extended_op sopn) * rexpr list) list **)

  let smart_subi atoI x y imm =
    map (to_opn atoI) (ARMFopn_core.smart_subi x y imm)

  (** val smart_addi_tmp :
      (register, empty, empty, rflag, condt) arch_toIdent -> var_i -> var_i
      -> coq_Z -> ((lexpr list * arm_extended_op sopn) * rexpr list) list **)

  let smart_addi_tmp atoI x tmp imm =
    map (to_opn atoI) (ARMFopn_core.smart_addi_tmp x tmp imm)

  (** val smart_subi_tmp :
      (register, empty, empty, rflag, condt) arch_toIdent -> var_i -> var_i
      -> coq_Z -> ((lexpr list * arm_extended_op sopn) * rexpr list) list **)

  let smart_subi_tmp atoI x tmp imm =
    map (to_opn atoI) (ARMFopn_core.smart_subi_tmp x tmp imm)
 end

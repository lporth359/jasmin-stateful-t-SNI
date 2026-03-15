open BinInt
open BinNums
open Arch_decl
open Eqtype
open Expr
open Fexpr
open Riscv_decl
open Riscv_instr_decl
open Var0

(** val is_arith_small : coq_Z -> bool **)

let is_arith_small imm =
  (&&)
    (Z.leb
      (Z.opp
        (Z.pow (Zpos (Coq_xO Coq_xH)) (Zpos (Coq_xI (Coq_xI (Coq_xO
          Coq_xH)))))) imm)
    (Z.ltb imm
      (Z.pow (Zpos (Coq_xO Coq_xH)) (Zpos (Coq_xI (Coq_xI (Coq_xO Coq_xH))))))

(** val is_arith_small_neg : coq_Z -> bool **)

let is_arith_small_neg imm =
  is_arith_small (Z.opp imm)

module RISCVFopn_core =
 struct
  type opn_args = (lexpr list * riscv_op) * rexpr list

  (** val op_gen : riscv_op -> var_i -> rexpr list -> opn_args **)

  let op_gen mn x res =
    ((((LLvar x) :: []), mn), res)

  (** val op_un_reg : riscv_op -> var_i -> var_i -> opn_args **)

  let op_un_reg mn x y =
    op_gen mn x ((rvar y) :: [])

  (** val op_un_imm : riscv_op -> var_i -> coq_Z -> opn_args **)

  let op_un_imm mn x imm =
    op_gen mn x ((rconst riscv_decl.reg_size imm) :: [])

  (** val op_bin_reg : riscv_op -> var_i -> var_i -> var_i -> opn_args **)

  let op_bin_reg mn x y z =
    op_gen mn x ((rvar y) :: ((rvar z) :: []))

  (** val op_bin_imm : riscv_op -> var_i -> var_i -> coq_Z -> opn_args **)

  let op_bin_imm mn x y imm =
    op_gen mn x ((rvar y) :: ((rconst riscv_decl.reg_size imm) :: []))

  (** val neg_op_bin_imm : riscv_op -> var_i -> var_i -> coq_Z -> opn_args **)

  let neg_op_bin_imm mn x y imm =
    op_gen mn x ((rvar y) :: ((rconst riscv_decl.reg_size (Z.opp imm)) :: []))

  (** val mov : var_i -> var_i -> opn_args **)

  let mov =
    op_un_reg MV

  (** val add : var_i -> var_i -> var_i -> opn_args **)

  let add =
    op_bin_reg ADD

  (** val sub : var_i -> var_i -> var_i -> opn_args **)

  let sub =
    op_bin_reg SUB

  (** val li : var_i -> coq_Z -> opn_args **)

  let li =
    op_un_imm LI

  (** val addi : var_i -> var_i -> coq_Z -> opn_args **)

  let addi =
    op_bin_imm ADDI

  (** val subi : var_i -> var_i -> coq_Z -> opn_args **)

  let subi =
    neg_op_bin_imm ADDI

  (** val andi : var_i -> var_i -> coq_Z -> opn_args **)

  let andi =
    op_bin_imm ANDI

  (** val smart_mov : var_i -> var_i -> opn_args list **)

  let smart_mov x y =
    if eq_op Var.coq_MvMake_var__canonical__eqtype_Equality
         (Obj.magic x.v_var) (Obj.magic y.v_var)
    then []
    else (mov x y) :: []

  (** val gen_smart_opi :
      (var_i -> var_i -> var_i -> opn_args) -> (var_i -> var_i -> coq_Z ->
      opn_args) -> (coq_Z -> bool) -> coq_Z option -> var_i -> var_i -> var_i
      -> coq_Z -> opn_args list **)

  let gen_smart_opi on_reg on_imm is_small neutral tmp x y imm =
    let is_mov = match neutral with
                 | Some n -> Z.eqb imm n
                 | None -> false in
    if is_mov
    then smart_mov x y
    else if is_small imm
         then (on_imm x y imm) :: []
         else (li tmp imm) :: ((on_reg x y tmp) :: [])

  (** val smart_addi : var_i -> var_i -> coq_Z -> opn_args list **)

  let smart_addi x y =
    gen_smart_opi add addi is_arith_small (Some Z0) x x y

  (** val smart_subi : var_i -> var_i -> coq_Z -> opn_args list **)

  let smart_subi x y imm =
    gen_smart_opi sub subi is_arith_small_neg (Some Z0) x x y imm

  (** val gen_smart_opi_tmp :
      (coq_Z -> bool) -> (var_i -> var_i -> var_i -> opn_args) -> (var_i ->
      var_i -> coq_Z -> opn_args) -> var_i -> var_i -> coq_Z -> opn_args list **)

  let gen_smart_opi_tmp is_arith_small0 on_reg on_imm x tmp imm =
    gen_smart_opi on_reg on_imm is_arith_small0 (Some Z0) tmp x x imm

  (** val smart_addi_tmp : var_i -> var_i -> coq_Z -> opn_args list **)

  let smart_addi_tmp x tmp imm =
    gen_smart_opi_tmp is_arith_small add addi x tmp imm

  (** val smart_subi_tmp : var_i -> var_i -> coq_Z -> opn_args list **)

  let smart_subi_tmp x tmp imm =
    gen_smart_opi_tmp is_arith_small_neg sub subi x tmp imm
 end

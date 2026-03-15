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

(** val is_arith_small : coq_Z -> bool **)

let is_arith_small imm =
  (||) (is_expandable_or_shift imm) (is_w12_encoding imm)

(** val coq_Z_mod_lnot : coq_Z -> wsize -> coq_Z **)

let coq_Z_mod_lnot z ws =
  let m = wbase ws in Z.modulo (Z.lnot (Z.modulo z m)) m

module ARMFopn_core =
 struct
  type opn_args = (lexpr list * arm_op) * rexpr list

  (** val mov : var_i -> var_i -> opn_args **)

  let mov =
    let op_gen = fun mn x res -> ((((LLvar x) :: []), (ARM_op (mn,
      default_opts))), res)
    in
    let mn = MOV in (fun x y -> op_gen mn x ((rvar y) :: []))

  (** val add : var_i -> var_i -> var_i -> opn_args **)

  let add =
    let op_gen = fun mn x res -> ((((LLvar x) :: []), (ARM_op (mn,
      default_opts))), res)
    in
    let mn = ADD in (fun x y z -> op_gen mn x ((rvar y) :: ((rvar z) :: [])))

  (** val sub : var_i -> var_i -> var_i -> opn_args **)

  let sub =
    let op_gen = fun mn x res -> ((((LLvar x) :: []), (ARM_op (mn,
      default_opts))), res)
    in
    let mn = SUB in (fun x y z -> op_gen mn x ((rvar y) :: ((rvar z) :: [])))

  (** val movi : var_i -> coq_Z -> opn_args **)

  let movi =
    let op_gen = fun mn x res -> ((((LLvar x) :: []), (ARM_op (mn,
      default_opts))), res)
    in
    let mn = MOV in
    (fun x imm -> op_gen mn x ((rconst arm_decl.reg_size imm) :: []))

  (** val mvni : var_i -> coq_Z -> opn_args **)

  let mvni =
    let op_gen = fun mn x res -> ((((LLvar x) :: []), (ARM_op (mn,
      default_opts))), res)
    in
    let mn = MVN in
    (fun x imm -> op_gen mn x ((rconst arm_decl.reg_size imm) :: []))

  (** val movt : var_i -> coq_Z -> opn_args **)

  let movt =
    let op_gen = fun mn x res -> ((((LLvar x) :: []), (ARM_op (mn,
      default_opts))), res)
    in
    (fun x imm -> op_gen MOVT x ((rvar x) :: ((rconst U16 imm) :: [])))

  (** val addi : var_i -> var_i -> coq_Z -> opn_args **)

  let addi =
    let op_gen = fun mn x res -> ((((LLvar x) :: []), (ARM_op (mn,
      default_opts))), res)
    in
    let mn = ADD in
    (fun x y imm ->
    op_gen mn x ((rvar y) :: ((rconst arm_decl.reg_size imm) :: [])))

  (** val subi : var_i -> var_i -> coq_Z -> opn_args **)

  let subi =
    let op_gen = fun mn x res -> ((((LLvar x) :: []), (ARM_op (mn,
      default_opts))), res)
    in
    let mn = SUB in
    (fun x y imm ->
    op_gen mn x ((rvar y) :: ((rconst arm_decl.reg_size imm) :: [])))

  (** val bici : var_i -> var_i -> coq_Z -> opn_args **)

  let bici =
    let op_gen = fun mn x res -> ((((LLvar x) :: []), (ARM_op (mn,
      default_opts))), res)
    in
    let mn = BIC in
    (fun x y imm ->
    op_gen mn x ((rvar y) :: ((rconst arm_decl.reg_size imm) :: [])))

  (** val li : var_i -> coq_Z -> opn_args list **)

  let li x imm =
    if (||) (is_expandable imm) (is_w16_encoding imm)
    then (movi x imm) :: []
    else let nimm = coq_Z_mod_lnot imm arm_decl.reg_size in
         if is_expandable nimm
         then (mvni x nimm) :: []
         else let p = Z.div_eucl imm (wbase U16) in
              (movi x (snd p)) :: ((movt x (fst p)) :: [])

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
         else rcons (li tmp imm) (on_reg x y tmp)

  (** val smart_addi : var_i -> var_i -> coq_Z -> opn_args list **)

  let smart_addi x y =
    gen_smart_opi add addi is_arith_small (Some Z0) x x y

  (** val smart_subi : var_i -> var_i -> coq_Z -> opn_args list **)

  let smart_subi x y imm =
    gen_smart_opi sub subi is_arith_small (Some Z0) x x y imm

  (** val gen_smart_opi_tmp :
      (var_i -> var_i -> var_i -> opn_args) -> (var_i -> var_i -> coq_Z ->
      opn_args) -> var_i -> var_i -> coq_Z -> opn_args list **)

  let gen_smart_opi_tmp on_reg on_imm x tmp imm =
    gen_smart_opi on_reg on_imm is_arith_small (Some Z0) tmp x x imm

  (** val smart_addi_tmp : var_i -> var_i -> coq_Z -> opn_args list **)

  let smart_addi_tmp x tmp imm =
    gen_smart_opi_tmp add addi x tmp imm

  (** val smart_subi_tmp : var_i -> var_i -> coq_Z -> opn_args list **)

  let smart_subi_tmp x tmp imm =
    gen_smart_opi_tmp sub subi x tmp imm
 end

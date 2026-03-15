open BinInt
open BinNums
open Arch_decl
open Arch_extra
open Arch_utils
open Compiler_util
open Expr
open Fexpr
open Label
open Linear
open Memory_model
open Riscv_decl
open Riscv_extra
open Riscv_instr_decl
open Riscv_params_common
open Seq
open Stack_zero_strategy
open Type
open Utils0
open Var0
open Wsize

(** val sz_init :
    (register, empty, empty, empty, condt) arch_toIdent -> var_i -> wsize ->
    coq_Z -> (register, empty, empty, empty, condt, riscv_op, riscv_extra_op)
    extended_op lcmd **)

let sz_init atoI vrsp alignment stk_max =
  let vsaved_sp =
    mk_var_i
      (to_var (Coq_lword riscv_decl.reg_size) riscv_decl.toS_r atoI.toI_r X5)
  in
  let voff =
    mk_var_i
      (to_var (Coq_lword riscv_decl.reg_size) riscv_decl.toS_r atoI.toI_r X6)
  in
  let vzero =
    mk_var_i
      (to_var (Coq_lword riscv_decl.reg_size) riscv_decl.toS_r atoI.toI_r X7)
  in
  let args =
    (RISCVFopn.mov atoI vsaved_sp vrsp) :: ((RISCVFopn.li atoI voff stk_max) :: (
    (RISCVFopn.align atoI vzero vsaved_sp alignment) :: ((RISCVFopn.mov atoI
                                                           vrsp vzero) :: (
    (RISCVFopn.sub atoI vrsp vrsp voff) :: ((RISCVFopn.li atoI vzero Z0) :: [])))))
  in
  map (li_of_fopn_args (asm_opI (riscv_extra atoI)) dummy_instr_info) args

(** val store_zero :
    (register, empty, empty, empty, condt) arch_toIdent -> wsize -> var_i ->
    coq_Z -> (register, empty, empty, empty, condt, riscv_op, riscv_extra_op)
    extended_op linstr_r **)

let store_zero atoI ws =
  let vzero =
    mk_var_i
      (to_var (Coq_lword riscv_decl.reg_size) riscv_decl.toS_r atoI.toI_r X7)
  in
  (fun v off ->
  let current = Store (Aligned, ws,
    (faddv (arch_pd riscv_decl) v (fconst riscv_decl.reg_size off)))
  in
  Lopn ((current :: []), (coq_Oriscv atoI (STORE ws)), ((rvar vzero) :: [])))

(** val sz_loop :
    (register, empty, empty, empty, condt) arch_toIdent -> var_i -> label ->
    wsize -> (register, empty, empty, empty, condt, riscv_op, riscv_extra_op)
    extended_op lcmd **)

let sz_loop atoI vrsp lbl ws =
  let voff =
    mk_var_i
      (to_var (Coq_lword riscv_decl.reg_size) riscv_decl.toS_r atoI.toI_r X6)
  in
  let vtemp =
    mk_var_i
      (to_var (Coq_lword riscv_decl.reg_size) riscv_decl.toS_r atoI.toI_r X12)
  in
  let dec_off =
    let (p, e) = RISCVFopn.subi atoI voff voff (wsize_size ws) in
    let (r, op) = p in Lopn (r, op, e)
  in
  let compute_address =
    let (p, e) = RISCVFopn.add atoI vtemp vrsp voff in
    let (r, op) = p in Lopn (r, op, e)
  in
  let irs = (Llabel (InternalLabel,
    lbl)) :: (dec_off :: (compute_address :: ((store_zero atoI ws vtemp Z0) :: ((Lcond
    ((Fapp2 ((Oneq (Op_w U32)), (Fvar voff),
    (fconst riscv_decl.reg_size Z0))), lbl)) :: []))))
  in
  map (fun x -> { li_ii = dummy_instr_info; li_i = x }) irs

(** val restore_sp :
    (register, empty, empty, empty, condt) arch_toIdent -> var_i ->
    (register, empty, empty, empty, condt, riscv_op, riscv_extra_op)
    extended_op linstr list **)

let restore_sp atoI vrsp =
  let vsaved_sp =
    mk_var_i
      (to_var (Coq_lword riscv_decl.reg_size) riscv_decl.toS_r atoI.toI_r X5)
  in
  (li_of_fopn_args (asm_opI (riscv_extra atoI)) dummy_instr_info
    (RISCVFopn.mov atoI vrsp vsaved_sp)) :: []

(** val stack_zero_loop :
    (register, empty, empty, empty, condt) arch_toIdent -> var_i -> label ->
    wsize -> wsize -> coq_Z -> (register, empty, empty, empty, condt,
    riscv_op, riscv_extra_op) extended_op lcmd **)

let stack_zero_loop atoI vrsp lbl alignment ws stk_max =
  cat (sz_init atoI vrsp alignment stk_max)
    (cat (sz_loop atoI vrsp lbl ws) (restore_sp atoI vrsp))

(** val stack_zero_loop_vars :
    (register, empty, empty, empty, condt) arch_toIdent -> SvExtra.Sv.t **)

let stack_zero_loop_vars atoI =
  let vsaved_sp =
    mk_var_i
      (to_var (Coq_lword riscv_decl.reg_size) riscv_decl.toS_r atoI.toI_r X5)
  in
  let voff =
    mk_var_i
      (to_var (Coq_lword riscv_decl.reg_size) riscv_decl.toS_r atoI.toI_r X6)
  in
  let vzero =
    mk_var_i
      (to_var (Coq_lword riscv_decl.reg_size) riscv_decl.toS_r atoI.toI_r X7)
  in
  let vtemp =
    mk_var_i
      (to_var (Coq_lword riscv_decl.reg_size) riscv_decl.toS_r atoI.toI_r X12)
  in
  SvExtra.sv_of_list (Obj.magic (fun v -> v.v_var))
    (vsaved_sp :: (voff :: (vzero :: (vtemp :: []))))

(** val sz_unrolled :
    (register, empty, empty, empty, condt) arch_toIdent -> var_i -> wsize ->
    coq_Z -> (register, empty, empty, empty, condt, riscv_op, riscv_extra_op)
    extended_op lcmd **)

let sz_unrolled atoI vrsp ws stk_max =
  let rn = rev (ziota Z0 (Z.div stk_max (wsize_size ws))) in
  map (fun off -> { li_ii = dummy_instr_info; li_i =
    (store_zero atoI ws vrsp (Z.mul off (wsize_size ws))) }) rn

(** val stack_zero_unrolled :
    (register, empty, empty, empty, condt) arch_toIdent -> var_i -> wsize ->
    wsize -> coq_Z -> (register, empty, empty, empty, condt, riscv_op,
    riscv_extra_op) extended_op lcmd **)

let stack_zero_unrolled atoI vrsp alignment ws stk_max =
  cat (sz_init atoI vrsp alignment stk_max)
    (cat (sz_unrolled atoI vrsp ws stk_max) (restore_sp atoI vrsp))

(** val stack_zero_unrolled_vars :
    (register, empty, empty, empty, condt) arch_toIdent -> SvExtra.Sv.t **)

let stack_zero_unrolled_vars atoI =
  let vsaved_sp =
    mk_var_i
      (to_var (Coq_lword riscv_decl.reg_size) riscv_decl.toS_r atoI.toI_r X5)
  in
  let voff =
    mk_var_i
      (to_var (Coq_lword riscv_decl.reg_size) riscv_decl.toS_r atoI.toI_r X6)
  in
  let vzero =
    mk_var_i
      (to_var (Coq_lword riscv_decl.reg_size) riscv_decl.toS_r atoI.toI_r X7)
  in
  let vtemp =
    mk_var_i
      (to_var (Coq_lword riscv_decl.reg_size) riscv_decl.toS_r atoI.toI_r X12)
  in
  SvExtra.sv_of_list (Obj.magic (fun v -> v.v_var))
    (vsaved_sp :: (voff :: (vzero :: (vtemp :: []))))

(** val stack_zeroization_cmd :
    (register, empty, empty, empty, condt) arch_toIdent ->
    stack_zero_strategy -> Ident.Ident.ident -> label -> wsize -> wsize ->
    coq_Z -> ((register, empty, empty, empty, condt, riscv_op,
    riscv_extra_op) extended_op lcmd * SvExtra.Sv.t) cexec **)

let stack_zeroization_cmd atoI szs rspn lbl ws_align ws stk_max =
  let err = fun msg -> { pel_msg = (PPEstring msg); pel_fn = None; pel_fi =
    None; pel_ii = None; pel_vi = None; pel_pass = (Some
    "stack zeroization"); pel_internal = false }
  in
  let err_size = err "Stack zeroization size not supported in RISC-V" in
  if cmp_le wsize_cmp ws U32
  then let rsp =
         mk_var_i { Var.vtype = (Coq_aword (arch_pd riscv_decl)); Var.vname =
           rspn }
       in
       (match szs with
        | SZSloop ->
          Ok ((stack_zero_loop atoI rsp lbl ws_align ws stk_max),
            (stack_zero_loop_vars atoI))
        | SZSloopSCT ->
          let err_sct =
            err "Strategy \"loop with SCT\" is not supported in RISC-V"
          in
          Error err_sct
        | SZSunrolled ->
          Ok ((stack_zero_unrolled atoI rsp ws_align ws stk_max),
            (stack_zero_unrolled_vars atoI)))
  else Error err_size

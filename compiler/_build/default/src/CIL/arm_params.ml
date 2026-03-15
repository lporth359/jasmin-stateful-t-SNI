open BinInt
open BinNums
open Datatypes
open Arch_decl
open Arch_extra
open Arch_params
open Arch_utils
open Arm_decl
open Arm_extra
open Arm_instr_decl
open Arm_lowering
open Arm_params_common
open Arm_params_core
open Arm_stack_zeroization
open Asm_gen
open Compiler_util
open Eqtype
open Expr
open Fexpr
open Lea
open Linearization
open Lowering
open Memory_model
open Seq
open Shift_kind
open Slh_lowering
open Sopn
open Ssralg
open Ssrbool
open Ssrfun
open Ssrnat
open Stack_alloc_params
open Stack_zeroization
open Type
open Utils0
open Var0
open Word0
open Word_ssrZ
open Wsize

(** val arm_mov_ofs :
    (register, empty, empty, rflag, condt) arch_toIdent -> lval -> assgn_tag
    -> mov_kind -> pexpr -> pexpr -> (register, empty, empty, rflag, condt,
    arm_op, arm_extra_op) extended_op instr_r option **)

let arm_mov_ofs atoI x tag movk y ofs =
  let mk = fun oa ->
    let (op, args) = oa in
    Some (Copn ((x :: []), tag, (coq_Oarm atoI (ARM_op (op, default_opts))),
    args))
  in
  (match movk with
   | MK_LEA ->
     mk (ADR,
       ((if is_zero (Obj.magic arch_pd arm_decl) ofs
         then y
         else add (arch_pd arm_decl) y ofs) :: []))
   | MK_MOV ->
     (match x with
      | Lvar _ ->
        if is_Pload y
        then if is_zero (Obj.magic arch_pd arm_decl) ofs
             then mk (LDR, (y :: []))
             else None
        else (match mk_lea (arch_pd arm_decl) (add (arch_pd arm_decl) y ofs) with
              | Some lea ->
                (match lea.lea_base with
                 | Some base ->
                   (match lea.lea_offset with
                    | Some off ->
                      if eq_op coq_BinNums_Z__canonical__eqtype_Equality
                           (Obj.magic lea.lea_disp) (Obj.magic Z0)
                      then (match Ssrfun.Option.map Z.of_nat
                                    (shift_of_scale lea.lea_scale) with
                            | Some scale ->
                              if eq_op
                                   coq_BinNums_Z__canonical__eqtype_Equality
                                   (Obj.magic scale) (Obj.magic Z0)
                              then mk (ADD,
                                     ((coq_Plvar base) :: ((coq_Plvar off) :: [])))
                              else let opts = { set_flags = false;
                                     is_conditional = false; has_shift =
                                     (Some SLSL) }
                                   in
                                   Some (Copn ((x :: []), tag,
                                   (coq_Oarm atoI (ARM_op (ADD, opts))),
                                   ((coq_Plvar base) :: ((coq_Plvar off) :: (
                                   (eword_of_int U8 scale) :: [])))))
                            | None -> None)
                      else None
                    | None ->
                      if eq_op coq_BinNums_Z__canonical__eqtype_Equality
                           (Obj.magic lea.lea_disp) (Obj.magic Z0)
                      then mk (MOV, ((coq_Plvar base) :: []))
                      else if is_arith_small lea.lea_disp
                           then mk (ADD,
                                  ((coq_Plvar base) :: ((cast_const
                                                          (arch_pd arm_decl)
                                                          lea.lea_disp) :: [])))
                           else Some (Copn ((x :: []), tag, (Oasm (ExtOp
                                  Oarm_add_large_imm)),
                                  ((coq_Plvar base) :: ((cast_const
                                                          (arch_pd arm_decl)
                                                          lea.lea_disp) :: [])))))
                 | None -> None)
              | None -> None)
      | Lmem (_, _, _, _) ->
        if is_zero (Obj.magic arch_pd arm_decl) ofs
        then mk (STR, (y :: []))
        else None
      | _ -> None))

(** val arm_immediate :
    (register, empty, empty, rflag, condt) arch_toIdent -> var_i -> coq_Z ->
    arm_extended_op instr_r **)

let arm_immediate atoI x z =
  Copn (((Lvar x) :: []), AT_none,
    (coq_Oarm atoI (ARM_op (MOV, default_opts))),
    ((cast_const (arch_pd arm_decl) z) :: []))

(** val arm_swap :
    (register, empty, empty, rflag, condt) arch_toIdent -> assgn_tag -> var_i
    -> var_i -> var_i -> var_i -> (register, empty, empty, rflag, condt,
    arm_op, arm_extra_op) extended_op instr_r **)

let arm_swap _ t x y z w =
  Copn (((Lvar x) :: ((Lvar y) :: [])), t, (Oasm (ExtOp (Oarm_swap
    arm_decl.reg_size))), ((coq_Plvar z) :: ((coq_Plvar w) :: [])))

(** val arm_saparams :
    (register, empty, empty, rflag, condt) arch_toIdent -> (register, empty,
    empty, rflag, condt, arm_op, arm_extra_op) extended_op stack_alloc_params **)

let arm_saparams atoI =
  { sap_mov_ofs = (arm_mov_ofs atoI); sap_immediate = (arm_immediate atoI);
    sap_swap = (arm_swap atoI) }

(** val arm_allocate_stack_frame :
    (register, empty, empty, rflag, condt) arch_toIdent -> var_i -> var_i
    option -> coq_Z -> ((lexpr list * arm_extended_op sopn) * rexpr list) list **)

let arm_allocate_stack_frame atoI rspi tmp sz =
  match tmp with
  | Some aux -> ARMFopn.smart_subi_tmp atoI rspi aux sz
  | None -> (ARMFopn.subi atoI rspi rspi sz) :: []

(** val arm_free_stack_frame :
    (register, empty, empty, rflag, condt) arch_toIdent -> var_i -> var_i
    option -> coq_Z -> ((lexpr list * arm_extended_op sopn) * rexpr list) list **)

let arm_free_stack_frame atoI rspi tmp sz =
  match tmp with
  | Some aux -> ARMFopn.smart_addi_tmp atoI rspi aux sz
  | None -> (ARMFopn.addi atoI rspi rspi sz) :: []

(** val arm_set_up_sp_register :
    (register, empty, empty, rflag, condt) arch_toIdent -> var_i -> coq_Z ->
    wsize -> var_i -> var_i -> ((lexpr list * (register, empty, empty, rflag,
    condt, arm_op, arm_extra_op) extended_op sopn) * rexpr list) list **)

let arm_set_up_sp_register atoI rspi sf_sz al r tmp =
  let load_imm = ARMFopn.smart_subi atoI tmp rspi sf_sz in
  let i0 = ARMFopn.align atoI tmp tmp al in
  let i1 = ARMFopn.mov atoI r rspi in
  let i2 = ARMFopn.mov atoI rspi tmp in
  cat load_imm (i0 :: (i1 :: (i2 :: [])))

(** val arm_tmp :
    (register, empty, empty, rflag, condt) arch_toIdent -> Ident.Ident.ident **)

let arm_tmp atoI =
  Var.vname
    (mk_var_i
      (to_var (Coq_lword arm_decl.reg_size) arm_decl.toS_r atoI.toI_r R12)).v_var

(** val arm_tmp2 :
    (register, empty, empty, rflag, condt) arch_toIdent -> Ident.Ident.ident **)

let arm_tmp2 atoI =
  Var.vname
    (mk_var_i
      (to_var (Coq_lword arm_decl.reg_size) arm_decl.toS_r atoI.toI_r LR)).v_var

(** val arm_lmove :
    (register, empty, empty, rflag, condt) arch_toIdent -> var_i -> var_i ->
    (lexpr list * arm_extended_op sopn) * rexpr list **)

let arm_lmove atoI xd xs =
  ((((LLvar xd) :: []), (coq_Oarm atoI (ARM_op (MOV, default_opts)))),
    ((Rexpr (Fvar xs)) :: []))

(** val arm_check_ws : Equality.sort -> bool **)

let arm_check_ws ws =
  eq_op wsize_wsize__canonical__eqtype_Equality ws
    (Obj.magic arm_decl.reg_size)

(** val arm_lstore :
    (register, empty, empty, rflag, condt) arch_toIdent -> var_i -> coq_Z ->
    var_i -> (lexpr list * arm_extended_op sopn) * rexpr list **)

let arm_lstore atoI xd ofs xs =
  let ws = arm_decl.reg_size in
  let mn = STR in
  ((((Store (Aligned, ws,
  (faddv (arch_pd arm_decl) xd (fconst ws ofs)))) :: []),
  (coq_Oarm atoI (ARM_op (mn, default_opts)))), ((Rexpr (Fvar xs)) :: []))

(** val arm_lload :
    (register, empty, empty, rflag, condt) arch_toIdent -> var_i -> var_i ->
    coq_Z -> (lexpr list * arm_extended_op sopn) * rexpr list **)

let arm_lload atoI xd xs ofs =
  let ws = arm_decl.reg_size in
  let mn = LDR in
  ((((LLvar xd) :: []), (coq_Oarm atoI (ARM_op (mn, default_opts)))), ((Load
  (Aligned, ws, (faddv (arch_pd arm_decl) xs (fconst ws ofs)))) :: []))

(** val arm_liparams :
    (register, empty, empty, rflag, condt) arch_toIdent -> (register, empty,
    empty, rflag, condt, arm_op, arm_extra_op) extended_op
    linearization_params **)

let arm_liparams atoI =
  { lip_tmp = (arm_tmp atoI); lip_tmp2 = (arm_tmp2 atoI);
    lip_not_saved_stack = ((arm_tmp atoI) :: []); lip_allocate_stack_frame =
    (arm_allocate_stack_frame atoI); lip_free_stack_frame =
    (arm_free_stack_frame atoI); lip_set_up_sp_register =
    (arm_set_up_sp_register atoI); lip_lmove = (arm_lmove atoI);
    lip_check_ws = (Obj.magic arm_check_ws); lip_lstore = (arm_lstore atoI);
    lip_lload = (arm_lload atoI); lip_lstores =
    (lstores_imm_dfl (arch_pd arm_decl) (asm_opI (arm_extra atoI))
      (arm_tmp2 atoI) (arm_lstore atoI) (ARMFopn.smart_addi atoI)
      is_arith_small); lip_lloads =
    (lloads_imm_dfl (arch_pd arm_decl) (asm_opI (arm_extra atoI))
      (arm_tmp2 atoI) (arm_lload atoI) (ARMFopn.smart_addi atoI)
      is_arith_small) }

(** val arm_fvars_correct :
    (register, empty, empty, rflag, condt) arch_toIdent -> fresh_vars ->
    progT -> (register, empty, empty, rflag, condt, arm_op, arm_extra_op)
    extended_op fun_decl list -> bool **)

let arm_fvars_correct atoI fv pT fds =
  fvars_correct (asm_opI (arm_extra atoI)) pT (all_fresh_vars fv) (fvars fv)
    fds

(** val arm_loparams :
    (register, empty, empty, rflag, condt) arch_toIdent -> ((register, empty,
    empty, rflag, condt, arm_op, arm_extra_op) extended_op, lowering_options)
    lowering_params **)

let arm_loparams atoI =
  { lop_lower_i = (fun _ _ -> Arm_lowering.lower_i atoI); lop_fvars_correct =
    (arm_fvars_correct atoI) }

(** val arm_shparams :
    (register, empty, empty, rflag, condt) arch_toIdent -> (register, empty,
    empty, rflag, condt, arm_op, arm_extra_op) extended_op sh_params **)

let arm_shparams _ _ _ _ =
  None

(** val condt_of_rflag : rflag -> condt **)

let condt_of_rflag = function
| NF -> MI_ct
| ZF -> EQ_ct
| CF -> CS_ct
| VF -> VS_ct

(** val condt_not : condt -> condt **)

let condt_not = function
| EQ_ct -> NE_ct
| NE_ct -> EQ_ct
| CS_ct -> CC_ct
| CC_ct -> CS_ct
| MI_ct -> PL_ct
| PL_ct -> MI_ct
| VS_ct -> VC_ct
| VC_ct -> VS_ct
| HI_ct -> LS_ct
| LS_ct -> HI_ct
| GE_ct -> LT_ct
| LT_ct -> GE_ct
| GT_ct -> LE_ct
| LE_ct -> GT_ct

(** val condt_and : condt -> condt -> condt option **)

let condt_and c0 c1 =
  match c0 with
  | NE_ct ->
    (match c1 with
     | CS_ct -> Some HI_ct
     | GE_ct -> Some GT_ct
     | _ -> None)
  | CS_ct -> (match c1 with
              | NE_ct -> Some HI_ct
              | _ -> None)
  | GE_ct -> (match c1 with
              | NE_ct -> Some GT_ct
              | _ -> None)
  | _ -> None

(** val condt_or : condt -> condt -> condt option **)

let condt_or c0 c1 =
  match c0 with
  | EQ_ct ->
    (match c1 with
     | CC_ct -> Some LS_ct
     | LT_ct -> Some LE_ct
     | _ -> None)
  | CC_ct -> (match c1 with
              | EQ_ct -> Some LS_ct
              | _ -> None)
  | LT_ct -> (match c1 with
              | EQ_ct -> Some LE_ct
              | _ -> None)
  | _ -> None

(** val is_rflags_GE : rflag -> rflag -> bool **)

let is_rflags_GE r0 r1 =
  match r0 with
  | NF -> (match r1 with
           | VF -> true
           | _ -> false)
  | VF -> (match r1 with
           | NF -> true
           | _ -> false)
  | _ -> false

(** val assemble_cond :
    (register, empty, empty, rflag, condt) arch_toIdent -> instr_info ->
    fexpr -> condt cexec **)

let rec assemble_cond atoI ii e = match e with
| Fvar v ->
  (match of_var_e Coq_lbool arm_decl.toS_f atoI.toI_f ii v with
   | Ok x -> Ok (condt_of_rflag x)
   | Error s -> Error s)
| Fapp1 (s, e0) ->
  (match s with
   | Onot ->
     (match assemble_cond atoI ii e0 with
      | Ok x -> Ok (condt_not x)
      | Error s0 -> Error s0)
   | _ -> Error (Asm_gen.E.berror ii e "Can't assemble condition."))
| Fapp2 (s, e0, e1) ->
  (match s with
   | Obeq ->
     (match e0 with
      | Fvar x0 ->
        (match e1 with
         | Fvar x1 ->
           (match of_var_e Coq_lbool arm_decl.toS_f atoI.toI_f ii x0 with
            | Ok x ->
              (match of_var_e Coq_lbool arm_decl.toS_f atoI.toI_f ii x1 with
               | Ok x2 ->
                 if is_rflags_GE x x2
                 then Ok GE_ct
                 else Error (Asm_gen.E.berror ii e "Invalid condition (EQ).")
               | Error s0 -> Error s0)
            | Error s0 -> Error s0)
         | _ -> Error (Asm_gen.E.berror ii e "Can't assemble condition."))
      | _ -> Error (Asm_gen.E.berror ii e "Can't assemble condition."))
   | Oand ->
     (match assemble_cond atoI ii e0 with
      | Ok x ->
        (match assemble_cond atoI ii e1 with
         | Ok x0 ->
           (match condt_and x x0 with
            | Some ct -> Ok ct
            | None -> Error (Asm_gen.E.berror ii e "Invalid condition (AND)"))
         | Error s0 -> Error s0)
      | Error s0 -> Error s0)
   | Oor ->
     (match assemble_cond atoI ii e0 with
      | Ok x ->
        (match assemble_cond atoI ii e1 with
         | Ok x0 ->
           (match condt_or x x0 with
            | Some ct -> Ok ct
            | None -> Error (Asm_gen.E.berror ii e "Invalid condition (OR)"))
         | Error s0 -> Error s0)
      | Error s0 -> Error s0)
   | _ -> Error (Asm_gen.E.berror ii e "Can't assemble condition."))
| _ -> Error (Asm_gen.E.berror ii e "Can't assemble condition.")

(** val is_valid_address :
    (register, empty, empty, rflag, condt) reg_address -> bool **)

let is_valid_address addr =
  if negb
       (eq_op
         (GRing.ComRing.Exports.coq_GRing_ComRing__to__eqtype_Equality
           (word (arch_pd arm_decl))) addr.ad_disp
         (GRing.zero
           (GRing.ComRing.Exports.coq_GRing_ComRing__to__GRing_Nmodule
             (word (arch_pd arm_decl)))))
  then if isSome addr.ad_offset
       then false
       else if negb
                 (eq_op coq_Datatypes_nat__canonical__eqtype_Equality
                   (Obj.magic addr.ad_scale) (Obj.magic O))
            then false
            else true
  else if isSome addr.ad_offset
       then true
       else if negb
                 (eq_op coq_Datatypes_nat__canonical__eqtype_Equality
                   (Obj.magic addr.ad_scale) (Obj.magic O))
            then false
            else true

(** val arm_agparams :
    (register, empty, empty, rflag, condt) arch_toIdent -> (register, empty,
    empty, rflag, condt, arm_op, arm_extra_op) asm_gen_params **)

let arm_agparams atoI =
  { agp_assemble_cond = (assemble_cond atoI); agp_is_valid_address =
    is_valid_address }

(** val arm_szparams :
    (register, empty, empty, rflag, condt) arch_toIdent -> (register, empty,
    empty, rflag, condt, arm_op, arm_extra_op) extended_op
    stack_zeroization_params **)

let arm_szparams =
  stack_zeroization_cmd

(** val arm_is_move_op :
    (register, empty, empty, rflag, condt) arch_toIdent -> (register, empty,
    empty, rflag, condt, arm_op, arm_extra_op) extended_op asm_op_t -> bool **)

let arm_is_move_op _ = function
| BaseOp a ->
  let (o0, a0) = a in
  (match o0 with
   | Some _ -> false
   | None ->
     let ARM_op (o1, opts) = a0 in
     if in_mem (Obj.magic o1)
          (mem (seq_predType arm_mnemonic_eqType)
            (Obj.magic (MOV :: (LDR :: (STR :: (STRH :: (STRB :: [])))))))
     then (&&) (negb opts.set_flags)
            ((&&) (negb opts.is_conditional) (negb (isSome opts.has_shift)))
     else false)
| ExtOp _ -> false

(** val arm_params :
    (register, empty, empty, rflag, condt) arch_toIdent -> (register, empty,
    empty, rflag, condt, arm_op, arm_extra_op, lowering_options)
    architecture_params **)

let arm_params atoI =
  { ap_sap = (arm_saparams atoI); ap_lip = (arm_liparams atoI); ap_plp =
    false; ap_lop = (arm_loparams atoI); ap_shp = (arm_shparams atoI);
    ap_lap = (fun _ p -> Ok p); ap_agp = (arm_agparams atoI); ap_szp =
    (arm_szparams atoI); ap_is_move_op = (arm_is_move_op atoI) }

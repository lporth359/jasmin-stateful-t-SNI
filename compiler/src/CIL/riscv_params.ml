open BinNums
open Datatypes
open Arch_decl
open Arch_extra
open Arch_params
open Arch_utils
open Asm_gen
open Compiler_util
open Eqtype
open Expr
open Fexpr
open Lea
open Linearization
open Memory_model
open Riscv_decl
open Riscv_extra
open Riscv_instr_decl
open Riscv_lower_addressing
open Riscv_lowering
open Riscv_params_common
open Riscv_params_core
open Riscv_stack_zeroization
open Seq
open Slh_lowering
open Sopn
open Ssrbool
open Ssrnat
open Stack_alloc_params
open Stack_zeroization
open Type
open Utils0
open Var0
open Word_ssrZ
open Wsize

(** val riscv_mov_ofs :
    (register, empty, empty, empty, condt) arch_toIdent -> lval -> assgn_tag
    -> mov_kind -> pexpr -> pexpr -> (register, empty, empty, empty, condt,
    riscv_op, riscv_extra_op) extended_op instr_r option **)

let riscv_mov_ofs atoI x tag movk y ofs =
  let mk = fun oa ->
    let (op, args) = oa in
    Some (Copn ((x :: []), tag, (coq_Oriscv atoI op), args))
  in
  (match movk with
   | MK_LEA ->
     mk (LA,
       ((if is_zero (Obj.magic arch_pd riscv_decl) ofs
         then y
         else add (arch_pd riscv_decl) y ofs) :: []))
   | MK_MOV ->
     (match x with
      | Lvar _ ->
        if is_Pload y
        then if is_zero (Obj.magic arch_pd riscv_decl) ofs
             then mk ((LOAD (Signed, U32)), (y :: []))
             else None
        else (match mk_lea (arch_pd riscv_decl)
                      (add (arch_pd riscv_decl) y ofs) with
              | Some lea ->
                (match lea.lea_base with
                 | Some base ->
                   (match lea.lea_offset with
                    | Some off ->
                      if (&&)
                           (eq_op coq_BinNums_Z__canonical__eqtype_Equality
                             (Obj.magic lea.lea_disp) (Obj.magic Z0))
                           (eq_op coq_BinNums_Z__canonical__eqtype_Equality
                             (Obj.magic lea.lea_scale)
                             (Obj.magic (Zpos Coq_xH)))
                      then mk (ADD,
                             ((coq_Plvar base) :: ((coq_Plvar off) :: [])))
                      else None
                    | None ->
                      if eq_op coq_BinNums_Z__canonical__eqtype_Equality
                           (Obj.magic lea.lea_disp) (Obj.magic Z0)
                      then mk (MV, ((coq_Plvar base) :: []))
                      else if is_arith_small lea.lea_disp
                           then mk (ADDI,
                                  ((coq_Plvar base) :: ((cast_const
                                                          (arch_pd riscv_decl)
                                                          lea.lea_disp) :: [])))
                           else Some (Copn ((x :: []), tag, (Oasm (ExtOp
                                  Oriscv_add_large_imm)),
                                  ((coq_Plvar base) :: ((cast_const
                                                          (arch_pd riscv_decl)
                                                          lea.lea_disp) :: [])))))
                 | None -> None)
              | None -> None)
      | Lmem (_, _, _, _) ->
        if is_zero (Obj.magic arch_pd riscv_decl) ofs
        then mk ((STORE U32), (y :: []))
        else None
      | _ -> None))

(** val riscv_immediate :
    (register, empty, empty, empty, condt) arch_toIdent -> var_i -> coq_Z ->
    riscv_extended_op instr_r **)

let riscv_immediate atoI x z =
  Copn (((Lvar x) :: []), AT_none, (coq_Oriscv atoI LI),
    ((cast_const (arch_pd riscv_decl) z) :: []))

(** val riscv_swap :
    (register, empty, empty, empty, condt) arch_toIdent -> assgn_tag -> var_i
    -> var_i -> var_i -> var_i -> (register, empty, empty, empty, condt,
    riscv_op, riscv_extra_op) extended_op instr_r **)

let riscv_swap _ t x y z w =
  Copn (((Lvar x) :: ((Lvar y) :: [])), t, (Oasm (ExtOp (SWAP
    riscv_decl.reg_size))), ((coq_Plvar z) :: ((coq_Plvar w) :: [])))

(** val riscv_saparams :
    (register, empty, empty, empty, condt) arch_toIdent -> (register, empty,
    empty, empty, condt, riscv_op, riscv_extra_op) extended_op
    stack_alloc_params **)

let riscv_saparams atoI =
  { sap_mov_ofs = (riscv_mov_ofs atoI); sap_immediate =
    (riscv_immediate atoI); sap_swap = (riscv_swap atoI) }

(** val riscv_allocate_stack_frame :
    (register, empty, empty, empty, condt) arch_toIdent -> var_i -> var_i
    option -> coq_Z -> ((lexpr list * (register, empty, empty, empty, condt,
    riscv_op, riscv_extra_op) extended_op sopn) * rexpr list) list **)

let riscv_allocate_stack_frame atoI rspi tmp sz =
  match tmp with
  | Some aux -> RISCVFopn.smart_subi_tmp atoI rspi aux sz
  | None -> (RISCVFopn.subi atoI rspi rspi sz) :: []

(** val riscv_free_stack_frame :
    (register, empty, empty, empty, condt) arch_toIdent -> var_i -> var_i
    option -> coq_Z -> ((lexpr list * (register, empty, empty, empty, condt,
    riscv_op, riscv_extra_op) extended_op sopn) * rexpr list) list **)

let riscv_free_stack_frame atoI rspi tmp sz =
  match tmp with
  | Some aux -> RISCVFopn.smart_addi_tmp atoI rspi aux sz
  | None -> (RISCVFopn.addi atoI rspi rspi sz) :: []

(** val riscv_set_up_sp_register :
    (register, empty, empty, empty, condt) arch_toIdent -> var_i -> coq_Z ->
    wsize -> var_i -> var_i -> ((lexpr list * (register, empty, empty, empty,
    condt, riscv_op, riscv_extra_op) extended_op sopn) * rexpr list) list **)

let riscv_set_up_sp_register atoI rspi sf_sz al r _ =
  let i0 = RISCVFopn.mov atoI r rspi in
  let i2 = RISCVFopn.align atoI rspi rspi al in
  i0 :: (rcons
          (if negb
                (eq_op coq_BinNums_Z__canonical__eqtype_Equality
                  (Obj.magic sf_sz) (Obj.magic Z0))
           then RISCVFopn.smart_subi atoI rspi r sf_sz
           else []) i2)

(** val riscv_tmp :
    (register, empty, empty, empty, condt) arch_toIdent -> Ident.Ident.ident **)

let riscv_tmp atoI =
  Var.vname
    (mk_var_i
      (to_var (Coq_lword riscv_decl.reg_size) riscv_decl.toS_r atoI.toI_r X28)).v_var

(** val riscv_tmp2 :
    (register, empty, empty, empty, condt) arch_toIdent -> Ident.Ident.ident **)

let riscv_tmp2 atoI =
  Var.vname
    (mk_var_i
      (to_var (Coq_lword riscv_decl.reg_size) riscv_decl.toS_r atoI.toI_r X29)).v_var

(** val riscv_lmove :
    (register, empty, empty, empty, condt) arch_toIdent -> var_i -> var_i ->
    (lexpr list * riscv_extended_op sopn) * rexpr list **)

let riscv_lmove atoI xd xs =
  ((((LLvar xd) :: []), (coq_Oriscv atoI MV)), ((Rexpr (Fvar xs)) :: []))

(** val riscv_check_ws : Equality.sort -> bool **)

let riscv_check_ws ws =
  eq_op wsize_wsize__canonical__eqtype_Equality ws
    (Obj.magic riscv_decl.reg_size)

(** val riscv_lstore :
    (register, empty, empty, empty, condt) arch_toIdent -> var_i -> coq_Z ->
    var_i -> (lexpr list * riscv_extended_op sopn) * rexpr list **)

let riscv_lstore atoI xd ofs xs =
  let ws = riscv_decl.reg_size in
  ((((Store (Aligned, ws,
  (faddv (arch_pd riscv_decl) xd (fconst ws ofs)))) :: []),
  (coq_Oriscv atoI (STORE ws))), ((Rexpr (Fvar xs)) :: []))

(** val riscv_lload :
    (register, empty, empty, empty, condt) arch_toIdent -> var_i -> var_i ->
    coq_Z -> (lexpr list * riscv_extended_op sopn) * rexpr list **)

let riscv_lload atoI xd xs ofs =
  let ws = riscv_decl.reg_size in
  ((((LLvar xd) :: []), (coq_Oriscv atoI (LOAD (Signed, ws)))), ((Load
  (Aligned, ws, (faddv (arch_pd riscv_decl) xs (fconst ws ofs)))) :: []))

(** val riscv_liparams :
    (register, empty, empty, empty, condt) arch_toIdent -> (register, empty,
    empty, empty, condt, riscv_op, riscv_extra_op) extended_op
    linearization_params **)

let riscv_liparams atoI =
  { lip_tmp = (riscv_tmp atoI); lip_tmp2 = (riscv_tmp2 atoI);
    lip_not_saved_stack = ((riscv_tmp atoI) :: []);
    lip_allocate_stack_frame = (riscv_allocate_stack_frame atoI);
    lip_free_stack_frame = (riscv_free_stack_frame atoI);
    lip_set_up_sp_register = (riscv_set_up_sp_register atoI); lip_lmove =
    (riscv_lmove atoI); lip_check_ws = (Obj.magic riscv_check_ws);
    lip_lstore = (riscv_lstore atoI); lip_lload = (riscv_lload atoI);
    lip_lstores =
    (lstores_imm_dfl (arch_pd riscv_decl) (asm_opI (riscv_extra atoI))
      (riscv_tmp2 atoI) (riscv_lstore atoI) (RISCVFopn.smart_addi atoI)
      is_arith_small); lip_lloads =
    (lloads_imm_dfl (arch_pd riscv_decl) (asm_opI (riscv_extra atoI))
      (riscv_tmp2 atoI) (riscv_lload atoI) (RISCVFopn.smart_addi atoI)
      is_arith_small) }

(** val riscv_loparams :
    (register, empty, empty, empty, condt) arch_toIdent -> ((register, empty,
    empty, empty, condt, riscv_op, riscv_extra_op) extended_op,
    lowering_options) lowering_params **)

let riscv_loparams atoI =
  { lop_lower_i = (fun _ _ _ -> Riscv_lowering.lower_i atoI);
    lop_fvars_correct = (fun _ _ _ -> true) }

(** val riscv_shparams :
    (register, empty, empty, empty, condt) arch_toIdent -> (register, empty,
    empty, empty, condt, riscv_op, riscv_extra_op) extended_op sh_params **)

let riscv_shparams _ _ _ _ =
  None

(** val condt_not : condt -> condt **)

let condt_not c =
  let ck =
    match c.cond_kind with
    | EQ -> NE
    | NE -> EQ
    | LT sg -> GE sg
    | GE sg -> LT sg
  in
  { cond_kind = ck; cond_fst = c.cond_fst; cond_snd = c.cond_snd }

(** val assemble_cond_arg :
    (register, empty, empty, empty, condt) arch_toIdent -> instr_info ->
    fexpr -> register option cexec **)

let assemble_cond_arg atoI ii e = match e with
| Fvar x ->
  (match of_var_e (Coq_lword riscv_decl.reg_size) riscv_decl.toS_r atoI.toI_r
           ii x with
   | Ok x0 -> Ok (Some x0)
   | Error s -> Error s)
| Fapp1 (s, f) ->
  (match s with
   | Oword_of_int w ->
     (match w with
      | U32 ->
        (match f with
         | Fconst z ->
           (match z with
            | Z0 -> Ok None
            | _ -> Error (Asm_gen.E.berror ii e "Can't assemble condition."))
         | _ -> Error (Asm_gen.E.berror ii e "Can't assemble condition."))
      | _ -> Error (Asm_gen.E.berror ii e "Can't assemble condition."))
   | _ -> Error (Asm_gen.E.berror ii e "Can't assemble condition."))
| _ -> Error (Asm_gen.E.berror ii e "Can't assemble condition.")

(** val assemble_cond_app2 : sop2 -> (condition_kind * bool) option **)

let assemble_cond_app2 = function
| Oeq o0 ->
  (match o0 with
   | Op_int -> None
   | Op_w w -> (match w with
                | U32 -> Some (EQ, false)
                | _ -> None))
| Oneq o0 ->
  (match o0 with
   | Op_int -> None
   | Op_w w -> (match w with
                | U32 -> Some (NE, false)
                | _ -> None))
| Olt c ->
  (match c with
   | Cmp_int -> None
   | Cmp_w (sg, w) -> (match w with
                       | U32 -> Some ((LT sg), false)
                       | _ -> None))
| Ole c ->
  (match c with
   | Cmp_int -> None
   | Cmp_w (sg, w) -> (match w with
                       | U32 -> Some ((GE sg), true)
                       | _ -> None))
| Ogt c ->
  (match c with
   | Cmp_int -> None
   | Cmp_w (sg, w) -> (match w with
                       | U32 -> Some ((LT sg), true)
                       | _ -> None))
| Oge c ->
  (match c with
   | Cmp_int -> None
   | Cmp_w (sg, w) -> (match w with
                       | U32 -> Some ((GE sg), false)
                       | _ -> None))
| _ -> None

(** val assemble_cond :
    (register, empty, empty, empty, condt) arch_toIdent -> instr_info ->
    fexpr -> condt cexec **)

let rec assemble_cond atoI ii e = match e with
| Fapp1 (s, e0) ->
  (match s with
   | Onot ->
     (match assemble_cond atoI ii e0 with
      | Ok x -> Ok (condt_not x)
      | Error s0 -> Error s0)
   | _ -> Error (Asm_gen.E.berror ii e "Can't assemble condition."))
| Fapp2 (o, e0, e1) ->
  (match o2r (Asm_gen.E.berror ii e "Could not match condition.")
           (assemble_cond_app2 o) with
   | Ok x ->
     let (o0, swap) = x in
     (match assemble_cond_arg atoI ii e0 with
      | Ok x0 ->
        (match assemble_cond_arg atoI ii e1 with
         | Ok x1 ->
           if swap
           then Ok { cond_kind = o0; cond_fst = x1; cond_snd = x0 }
           else Ok { cond_kind = o0; cond_fst = x0; cond_snd = x1 }
         | Error s -> Error s)
      | Error s -> Error s)
   | Error s -> Error s)
| _ -> Error (Asm_gen.E.berror ii e "Can't assemble condition.")

(** val is_valid_address :
    (register, empty, empty, empty, condt) reg_address -> bool **)

let is_valid_address addr =
  if isSome addr.ad_offset
  then false
  else if negb
            (eq_op coq_Datatypes_nat__canonical__eqtype_Equality
              (Obj.magic addr.ad_scale) (Obj.magic O))
       then false
       else true

(** val riscv_agparams :
    (register, empty, empty, empty, condt) arch_toIdent -> (register, empty,
    empty, empty, condt, riscv_op, riscv_extra_op) asm_gen_params **)

let riscv_agparams atoI =
  { agp_assemble_cond = (assemble_cond atoI); agp_is_valid_address =
    is_valid_address }

(** val riscv_szparams :
    (register, empty, empty, empty, condt) arch_toIdent -> (register, empty,
    empty, empty, condt, riscv_op, riscv_extra_op) extended_op
    stack_zeroization_params **)

let riscv_szparams =
  stack_zeroization_cmd

(** val riscv_laparams :
    (register, empty, empty, empty, condt) arch_toIdent -> (register, empty,
    empty, empty, condt, riscv_op, riscv_extra_op) lower_addressing_params **)

let riscv_laparams atoI =
  Obj.magic lower_addressing_prog atoI (progStack (arch_pd riscv_decl))

(** val riscv_is_move_op :
    (register, empty, empty, empty, condt) arch_toIdent -> (register, empty,
    empty, empty, condt, riscv_op, riscv_extra_op) extended_op asm_op_t ->
    bool **)

let riscv_is_move_op _ = function
| BaseOp a ->
  let (o0, o1) = a in
  (match o0 with
   | Some _ -> false
   | None ->
     (match o1 with
      | MV -> true
      | LOAD (_, w) -> (match w with
                        | U32 -> true
                        | _ -> false)
      | STORE _ -> true
      | _ -> false))
| ExtOp _ -> false

(** val riscv_params :
    (register, empty, empty, empty, condt) arch_toIdent -> (register, empty,
    empty, empty, condt, riscv_op, riscv_extra_op, lowering_options)
    architecture_params **)

let riscv_params atoI =
  { ap_sap = (riscv_saparams atoI); ap_lip = (riscv_liparams atoI); ap_plp =
    true; ap_lop = (riscv_loparams atoI); ap_shp = (riscv_shparams atoI);
    ap_lap = (riscv_laparams atoI); ap_agp = (riscv_agparams atoI); ap_szp =
    (riscv_szparams atoI); ap_is_move_op = (riscv_is_move_op atoI) }

open BinInt
open BinNums
open Datatypes
open Arch_decl
open Arch_extra
open Arch_params
open Asm_gen
open Compiler_util
open Eqtype
open Expr
open Fexpr
open Linearization
open Memory_model
open Seq
open Slh_lowering
open Slh_ops
open Sopn
open Stack_alloc_params
open Stack_zeroization
open Type
open Utils0
open Var0
open Word_ssrZ
open Wsize
open X86_decl
open X86_extra
open X86_instr_decl
open X86_lowering
open X86_stack_zeroization

(** val x86_op_align :
    (register, register_ext, xmm_register, rflag, condt) arch_toIdent ->
    var_i -> wsize -> wsize -> (lexpr list * (register, register_ext,
    xmm_register, rflag, condt, x86_op, x86_extra_op) extended_op
    sopn) * rexpr list **)

let x86_op_align atoI x ws al =
  let f_to_lvar = fun x0 -> LLvar
    (mk_var_i (to_var Coq_lbool x86_decl.toS_f atoI.toI_f x0))
  in
  let eflags = map f_to_lvar (OF :: (CF :: (SF :: (PF :: (ZF :: []))))) in
  let ex = Rexpr (Fvar x) in
  let emask = fconst ws (Z.opp (wsize_size al)) in
  (((cat eflags ((LLvar x) :: [])), (coq_Ox86 atoI (AND ws))), (ex :: ((Rexpr
  emask) :: [])))

(** val lea_ptr :
    (register, register_ext, xmm_register, rflag, condt) arch_toIdent -> lval
    -> pexpr -> assgn_tag -> pexpr -> (register, register_ext, xmm_register,
    rflag, condt, x86_op, x86_extra_op) extended_op instr_r **)

let lea_ptr atoI x y tag ofs =
  Copn ((x :: []), tag, (coq_Ox86 atoI (LEA (arch_pd x86_decl))),
    ((add (arch_pd x86_decl) y ofs) :: []))

(** val x86_mov_ofs :
    (register, register_ext, xmm_register, rflag, condt) arch_toIdent -> lval
    -> assgn_tag -> mov_kind -> pexpr -> pexpr -> (register, register_ext,
    xmm_register, rflag, condt, x86_op, x86_extra_op) extended_op instr_r
    option **)

let x86_mov_ofs atoI x tag movk y ofs =
  let addr =
    match movk with
    | MK_LEA -> lea_ptr atoI x y tag ofs
    | MK_MOV ->
      if is_zero (Obj.magic arch_pd x86_decl) ofs
      then mov_ws atoI (arch_pd x86_decl) x y tag
      else lea_ptr atoI x y tag ofs
  in
  Some addr

(** val x86_immediate :
    (register, register_ext, xmm_register, rflag, condt) arch_toIdent ->
    var_i -> coq_Z -> x86_extended_op instr_r **)

let x86_immediate atoI x z =
  mov_ws atoI (arch_pd x86_decl) (Lvar x) (cast_const (arch_pd x86_decl) z)
    AT_none

(** val x86_swap :
    (register, register_ext, xmm_register, rflag, condt) arch_toIdent ->
    assgn_tag -> var_i -> var_i -> var_i -> var_i -> x86_extended_op instr_r **)

let x86_swap atoI t x y z w =
  Copn (((Lvar x) :: ((Lvar y) :: [])), t,
    (coq_Ox86 atoI (XCHG x86_decl.reg_size)),
    ((coq_Plvar z) :: ((coq_Plvar w) :: [])))

(** val x86_saparams :
    (register, register_ext, xmm_register, rflag, condt) arch_toIdent ->
    (register, register_ext, xmm_register, rflag, condt, x86_op,
    x86_extra_op) extended_op stack_alloc_params **)

let x86_saparams atoI =
  { sap_mov_ofs = (x86_mov_ofs atoI); sap_immediate = (x86_immediate atoI);
    sap_swap = (x86_swap atoI) }

(** val x86_allocate_stack_frame :
    (register, register_ext, xmm_register, rflag, condt) arch_toIdent ->
    var_i -> var_i option -> coq_Z -> ((lexpr list * x86_extended_op
    sopn) * rexpr list) list **)

let x86_allocate_stack_frame atoI rspi _ sz =
  let p = Fapp2 ((Osub (Op_w (arch_pd x86_decl))), (Fvar rspi),
    (fconst (arch_pd x86_decl) sz))
  in
  ((((LLvar rspi) :: []), (coq_Ox86 atoI (LEA (arch_pd x86_decl)))), ((Rexpr
  p) :: [])) :: []

(** val x86_free_stack_frame :
    (register, register_ext, xmm_register, rflag, condt) arch_toIdent ->
    var_i -> var_i option -> coq_Z -> ((lexpr list * x86_extended_op
    sopn) * rexpr list) list **)

let x86_free_stack_frame atoI rspi _ sz =
  let p = Fapp2 ((Oadd (Op_w (arch_pd x86_decl))), (Fvar rspi),
    (fconst (arch_pd x86_decl) sz))
  in
  ((((LLvar rspi) :: []), (coq_Ox86 atoI (LEA (arch_pd x86_decl)))), ((Rexpr
  p) :: [])) :: []

(** val x86_lassign :
    (register, register_ext, xmm_register, rflag, condt) arch_toIdent ->
    lexpr -> wsize -> rexpr -> (lexpr list * x86_extended_op sopn) * rexpr
    list **)

let x86_lassign atoI x ws e =
  let op = if cmp_le wsize_cmp ws U64 then MOV ws else VMOVDQU ws in
  (((x :: []), (coq_Ox86 atoI op)), (e :: []))

(** val x86_set_up_sp_register :
    (register, register_ext, xmm_register, rflag, condt) arch_toIdent ->
    var_i -> coq_Z -> wsize -> var_i -> var_i -> ((lexpr list * (register,
    register_ext, xmm_register, rflag, condt, x86_op, x86_extra_op)
    extended_op sopn) * rexpr list) list **)

let x86_set_up_sp_register atoI rspi sf_sz al r _ =
  let i0 = x86_lassign atoI (LLvar r) (arch_pd x86_decl) (Rexpr (Fvar rspi))
  in
  let i2 = x86_op_align atoI rspi (arch_pd x86_decl) al in
  i0 :: (rcons
          (if negb
                (eq_op coq_BinNums_Z__canonical__eqtype_Equality
                  (Obj.magic sf_sz) (Obj.magic Z0))
           then x86_allocate_stack_frame atoI rspi None sf_sz
           else []) i2)

(** val x86_lmove :
    (register, register_ext, xmm_register, rflag, condt) arch_toIdent ->
    var_i -> var_i -> (lexpr list * x86_extended_op sopn) * rexpr list **)

let x86_lmove atoI xd xs =
  x86_lassign atoI (LLvar xd) (wsize_of_atype (Var.vtype xd.v_var)) (Rexpr
    (Fvar xs))

(** val x86_check_ws : wsize -> bool **)

let x86_check_ws _ =
  true

(** val x86_lstore :
    (register, register_ext, xmm_register, rflag, condt) arch_toIdent ->
    var_i -> coq_Z -> var_i -> (lexpr list * x86_extended_op sopn) * rexpr
    list **)

let x86_lstore atoI xd ofs xs =
  let ws = wsize_of_atype (Var.vtype xs.v_var) in
  x86_lassign atoI (Store (Aligned, ws,
    (faddv (arch_pd x86_decl) xd (fconst (arch_pd x86_decl) ofs)))) ws (Rexpr
    (Fvar xs))

(** val x86_lload :
    (register, register_ext, xmm_register, rflag, condt) arch_toIdent ->
    var_i -> var_i -> coq_Z -> (lexpr list * x86_extended_op sopn) * rexpr
    list **)

let x86_lload atoI xd xs ofs =
  let ws = wsize_of_atype (Var.vtype xd.v_var) in
  x86_lassign atoI (LLvar xd) ws (Load (Aligned, ws,
    (faddv (arch_pd x86_decl) xs (fconst (arch_pd x86_decl) ofs))))

(** val x86_tmp :
    (register, register_ext, xmm_register, rflag, condt) arch_toIdent ->
    Ident.Ident.ident **)

let x86_tmp atoI =
  Var.vname
    (mk_var_i
      (to_var (Coq_lword x86_decl.reg_size) x86_decl.toS_r atoI.toI_r RAX)).v_var

(** val x86_liparams :
    (register, register_ext, xmm_register, rflag, condt) arch_toIdent ->
    (register, register_ext, xmm_register, rflag, condt, x86_op,
    x86_extra_op) extended_op linearization_params **)

let x86_liparams atoI =
  { lip_tmp = (x86_tmp atoI); lip_tmp2 =
    (Var.vname
      (mk_var_i
        (to_var (Coq_lword x86_decl.reg_size) x86_decl.toS_r atoI.toI_r R10)).v_var);
    lip_not_saved_stack = ((x86_tmp atoI) :: []); lip_allocate_stack_frame =
    (x86_allocate_stack_frame atoI); lip_free_stack_frame =
    (x86_free_stack_frame atoI); lip_set_up_sp_register =
    (x86_set_up_sp_register atoI); lip_lmove = (x86_lmove atoI);
    lip_check_ws = x86_check_ws; lip_lstore = (x86_lstore atoI); lip_lload =
    (x86_lload atoI); lip_lstores =
    (lstores_dfl (asm_opI (x86_extra atoI)) (x86_lstore atoI)); lip_lloads =
    (lloads_dfl (asm_opI (x86_extra atoI)) (x86_lload atoI)) }

(** val x86_loparams :
    (register, register_ext, xmm_register, rflag, condt) arch_toIdent ->
    ((register, register_ext, xmm_register, rflag, condt, x86_op,
    x86_extra_op) extended_op, lowering_options) lowering_params **)

let x86_loparams atoI =
  { lop_lower_i = (lower_i atoI); lop_fvars_correct = (fvars_correct atoI) }

(** val lflags : lval list **)

let lflags =
  nseq (S (S (S (S (S O))))) (Lnone (dummy_var_info, Coq_abool))

(** val is_mmx_protect : wsize -> lval list -> bool **)

let is_mmx_protect ws lvs =
  match ws with
  | U64 ->
    (match lvs with
     | [] -> false
     | l :: l0 ->
       (match l with
        | Lvar y -> (match l0 with
                     | [] -> is_regx y.v_var
                     | _ :: _ -> false)
        | _ -> false))
  | _ -> false

(** val x86_sh_lower :
    (register, register_ext, xmm_register, rflag, condt) arch_toIdent -> lval
    list -> slh_op -> pexpr list -> ((lval list * (register, register_ext,
    xmm_register, rflag, condt, x86_op, x86_extra_op) extended_op
    sopn) * pexpr list) option **)

let x86_sh_lower _ lvs slho es =
  let o = fun x -> Oasm (ExtOp x) in
  (match slho with
   | SLHinit -> Some ((lvs, (o Ox86SLHinit)), es)
   | SLHupdate ->
     Some ((((Lnone (dummy_var_info, (Coq_aword
       (arch_msfsz x86_decl)))) :: lvs), (o Ox86SLHupdate)), es)
   | SLHmove -> Some ((lvs, (o Ox86SLHmove)), es)
   | SLHprotect ws ->
     if cmp_le wsize_cmp ws U64
     then if is_mmx_protect ws lvs
          then let rk = Extra in
               let extra = [] in
               Some (((cat extra lvs), (o (Ox86SLHprotect (rk, ws)))), es)
          else let rk = Normal in
               Some (((cat lflags lvs), (o (Ox86SLHprotect (rk, ws)))), es)
     else let rk = Normal in
          let extra = (Lnone (dummy_var_info, (Coq_aword ws))) :: [] in
          Some (((cat extra lvs), (o (Ox86SLHprotect (rk, ws)))), es)
   | _ -> None)

(** val x86_shparams :
    (register, register_ext, xmm_register, rflag, condt) arch_toIdent ->
    (register, register_ext, xmm_register, rflag, condt, x86_op,
    x86_extra_op) extended_op sh_params **)

let x86_shparams =
  x86_sh_lower

(** val not_condt : condt -> condt **)

let not_condt = function
| O_ct -> NO_ct
| NO_ct -> O_ct
| B_ct -> NB_ct
| NB_ct -> B_ct
| E_ct -> NE_ct
| NE_ct -> E_ct
| BE_ct -> NBE_ct
| NBE_ct -> BE_ct
| S_ct -> NS_ct
| NS_ct -> S_ct
| P_ct -> NP_ct
| NP_ct -> P_ct
| L_ct -> NL_ct
| NL_ct -> L_ct
| LE_ct -> NLE_ct
| NLE_ct -> LE_ct

(** val or_condt : instr_info -> fexpr -> condt -> condt -> condt cexec **)

let or_condt ii e c1 c2 =
  match c1 with
  | B_ct ->
    (match c2 with
     | E_ct -> Ok BE_ct
     | _ -> Error (Asm_gen.E.berror ii e "Invalid condition (OR)"))
  | E_ct ->
    (match c2 with
     | B_ct -> Ok BE_ct
     | L_ct -> Ok LE_ct
     | _ -> Error (Asm_gen.E.berror ii e "Invalid condition (OR)"))
  | L_ct ->
    (match c2 with
     | E_ct -> Ok LE_ct
     | _ -> Error (Asm_gen.E.berror ii e "Invalid condition (OR)"))
  | _ -> Error (Asm_gen.E.berror ii e "Invalid condition (OR)")

(** val and_condt :
    instr_info -> fexpr -> condt -> condt -> (pp_error_loc, condt) result **)

let and_condt ii e c1 c2 =
  match c1 with
  | NB_ct ->
    (match c2 with
     | NE_ct -> Ok NBE_ct
     | _ -> Error (Asm_gen.E.berror ii e "Invalid condition (AND)"))
  | NE_ct ->
    (match c2 with
     | NB_ct -> Ok NBE_ct
     | NL_ct -> Ok NLE_ct
     | _ -> Error (Asm_gen.E.berror ii e "Invalid condition (AND)"))
  | NL_ct ->
    (match c2 with
     | NE_ct -> Ok NLE_ct
     | _ -> Error (Asm_gen.E.berror ii e "Invalid condition (AND)"))
  | _ -> Error (Asm_gen.E.berror ii e "Invalid condition (AND)")

(** val of_var_e_bool :
    (register, register_ext, xmm_register, rflag, condt) arch_toIdent ->
    instr_info -> var_i -> rflag cexec **)

let of_var_e_bool atoI ii v =
  match of_var Coq_lbool x86_decl.toS_f atoI.toI_f v.v_var with
  | Some r -> Ok r
  | None -> Error (Asm_gen.E.invalid_flag ii v)

(** val assemble_cond_r :
    (register, register_ext, xmm_register, rflag, condt) arch_toIdent ->
    instr_info -> fexpr -> condt cexec **)

let rec assemble_cond_r atoI ii e = match e with
| Fvar v ->
  (match of_var_e_bool atoI ii v with
   | Ok x ->
     (match x with
      | CF -> Ok B_ct
      | PF -> Ok P_ct
      | ZF -> Ok E_ct
      | SF -> Ok S_ct
      | OF -> Ok O_ct)
   | Error s -> Error s)
| Fapp1 (s, e0) ->
  (match s with
   | Onot ->
     (match assemble_cond_r atoI ii e0 with
      | Ok x -> Ok (not_condt x)
      | Error s0 -> Error s0)
   | _ ->
     Error (Asm_gen.E.berror ii e "don't known how to compile the condition"))
| Fapp2 (s, e1, e2) ->
  (match s with
   | Obeq ->
     (match e1 with
      | Fvar x1 ->
        (match e2 with
         | Fvar x2 ->
           (match of_var_e_bool atoI ii x1 with
            | Ok x ->
              (match of_var_e_bool atoI ii x2 with
               | Ok x0 ->
                 if (||)
                      ((&&)
                        (eq_op x86_decl_rflag__canonical__eqtype_Equality
                          (Obj.magic x) (Obj.magic SF))
                        (eq_op x86_decl_rflag__canonical__eqtype_Equality
                          (Obj.magic x0) (Obj.magic OF)))
                      ((&&)
                        (eq_op x86_decl_rflag__canonical__eqtype_Equality
                          (Obj.magic x) (Obj.magic OF))
                        (eq_op x86_decl_rflag__canonical__eqtype_Equality
                          (Obj.magic x0) (Obj.magic SF)))
                 then Ok NL_ct
                 else Error (Asm_gen.E.berror ii e "Invalid condition (NL)")
               | Error s0 -> Error s0)
            | Error s0 -> Error s0)
         | _ ->
           Error
             (Asm_gen.E.berror ii e
               "don't known how to compile the condition"))
      | _ ->
        Error
          (Asm_gen.E.berror ii e "don't known how to compile the condition"))
   | Oand ->
     (match assemble_cond_r atoI ii e1 with
      | Ok x ->
        (match assemble_cond_r atoI ii e2 with
         | Ok x0 -> and_condt ii e x x0
         | Error s0 -> Error s0)
      | Error s0 -> Error s0)
   | Oor ->
     (match assemble_cond_r atoI ii e1 with
      | Ok x ->
        (match assemble_cond_r atoI ii e2 with
         | Ok x0 -> or_condt ii e x x0
         | Error s0 -> Error s0)
      | Error s0 -> Error s0)
   | _ ->
     Error (Asm_gen.E.berror ii e "don't known how to compile the condition"))
| _ ->
  Error (Asm_gen.E.berror ii e "don't known how to compile the condition")

(** val assemble_cond :
    (register, register_ext, xmm_register, rflag, condt) arch_toIdent ->
    instr_info -> fexpr -> condt cexec **)

let assemble_cond =
  assemble_cond_r

(** val is_valid_address :
    (register, register_ext, xmm_register, rflag, condt) reg_address -> bool **)

let is_valid_address _ =
  true

(** val x86_agparams :
    (register, register_ext, xmm_register, rflag, condt) arch_toIdent ->
    (register, register_ext, xmm_register, rflag, condt, x86_op,
    x86_extra_op) asm_gen_params **)

let x86_agparams atoI =
  { agp_assemble_cond = (assemble_cond atoI); agp_is_valid_address =
    is_valid_address }

(** val x86_szparams :
    (register, register_ext, xmm_register, rflag, condt) arch_toIdent ->
    (register, register_ext, xmm_register, rflag, condt, x86_op,
    x86_extra_op) extended_op stack_zeroization_params **)

let x86_szparams =
  x86_stack_zero_cmd

(** val x86_is_move_op :
    (register, register_ext, xmm_register, rflag, condt) arch_toIdent ->
    (register, register_ext, xmm_register, rflag, condt, x86_op,
    x86_extra_op) extended_op asm_op_t -> bool **)

let x86_is_move_op _ = function
| BaseOp a ->
  let (o0, x) = a in
  (match o0 with
   | Some _ -> false
   | None ->
     (match x with
      | MOV _ -> true
      | VMOVDQA _ -> true
      | VMOVDQU _ -> true
      | _ -> false))
| ExtOp e -> (match e with
              | Ox86SLHmove -> true
              | _ -> false)

(** val x86_params :
    (register, register_ext, xmm_register, rflag, condt) arch_toIdent ->
    (register, register_ext, xmm_register, rflag, condt, x86_op,
    x86_extra_op, lowering_options) architecture_params **)

let x86_params atoI =
  { ap_sap = (x86_saparams atoI); ap_lip = (x86_liparams atoI); ap_plp =
    false; ap_lop = (x86_loparams atoI); ap_shp = (x86_shparams atoI);
    ap_lap = (fun _ p -> Ok p); ap_agp = (x86_agparams atoI); ap_szp =
    (x86_szparams atoI); ap_is_move_op = (x86_is_move_op atoI) }

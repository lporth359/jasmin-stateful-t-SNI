open BinNums
open Datatypes
open Arch_decl
open Arch_extra
open Arch_utils
open Arm_decl
open Arm_extra
open Arm_instr_decl
open Eqtype
open Expr
open Lowering
open Pseudo_operator
open Seq
open Shift_kind
open Sopn
open Ssralg
open Ssrbool
open Type
open Utils0
open Var0
open Word0
open Wsize

(** val fv_NF : fresh_vars -> Ident.Ident.ident **)

let fv_NF fv =
  fv "__n__" Coq_abool

(** val fv_ZF : fresh_vars -> Ident.Ident.ident **)

let fv_ZF fv =
  fv "__z__" Coq_abool

(** val fv_CF : fresh_vars -> Ident.Ident.ident **)

let fv_CF fv =
  fv "__c__" Coq_abool

(** val fv_VF : fresh_vars -> Ident.Ident.ident **)

let fv_VF fv =
  fv "__v__" Coq_abool

(** val all_fresh_vars : fresh_vars -> Ident.Ident.ident list **)

let all_fresh_vars fv =
  (fv_NF fv) :: ((fv_ZF fv) :: ((fv_CF fv) :: ((fv_VF fv) :: [])))

(** val fvNF : fresh_vars -> Var.var **)

let fvNF fv =
  { Var.vtype = Coq_abool; Var.vname = (fv_NF fv) }

(** val fvZF : fresh_vars -> Var.var **)

let fvZF fv =
  { Var.vtype = Coq_abool; Var.vname = (fv_ZF fv) }

(** val fvCF : fresh_vars -> Var.var **)

let fvCF fv =
  { Var.vtype = Coq_abool; Var.vname = (fv_CF fv) }

(** val fvVF : fresh_vars -> Var.var **)

let fvVF fv =
  { Var.vtype = Coq_abool; Var.vname = (fv_VF fv) }

(** val fresh_flags : fresh_vars -> Var.var list **)

let fresh_flags fv =
  (fvNF fv) :: ((fvZF fv) :: ((fvCF fv) :: ((fvVF fv) :: [])))

(** val fvars : fresh_vars -> SvExtra.Sv.t **)

let fvars fv =
  SvExtra.sv_of_list (fun x -> Obj.magic x) (fresh_flags fv)

type low_expr =
  ((register, empty, empty, rflag, condt, arm_op, arm_extra_op) extended_op
  sopn * pexpr list) option

(** val le_skip :
    (register, empty, empty, rflag, condt) arch_toIdent -> low_expr **)

let le_skip _ =
  None

(** val le_issue_sopn :
    (register, empty, empty, rflag, condt) arch_toIdent -> (register, empty,
    empty, rflag, condt, arm_op, arm_extra_op) extended_op sopn -> pexpr list
    -> low_expr **)

let le_issue_sopn _ op es =
  Some (op, es)

(** val le_issue_aop :
    (register, empty, empty, rflag, condt) arch_toIdent -> arm_op -> pexpr
    list -> low_expr **)

let le_issue_aop atoI aop =
  le_issue_sopn atoI (Oasm (BaseOp (None, aop)))

(** val le_issue_opts :
    (register, empty, empty, rflag, condt) arch_toIdent -> arm_mnemonic ->
    arm_options -> pexpr list -> low_expr **)

let le_issue_opts atoI mn opts =
  le_issue_aop atoI (ARM_op (mn, opts))

(** val le_issue :
    (register, empty, empty, rflag, condt) arch_toIdent -> arm_mnemonic ->
    pexpr list -> low_expr **)

let le_issue atoI mn =
  le_issue_opts atoI mn default_opts

(** val no_pre :
    (register, empty, empty, rflag, condt) arch_toIdent -> low_expr ->
    (((register, empty, empty, rflag, condt, arm_op, arm_extra_op)
    extended_op instr_r list * (register, empty, empty, rflag, condt, arm_op,
    arm_extra_op) extended_op sopn) * pexpr list) option **)

let no_pre _ = function
| Some p -> let (aop, es) = p in Some (([], aop), es)
| None -> None

(** val chk_ws_reg : wsize -> unit option **)

let chk_ws_reg ws =
  oassert
    (eq_op wsize_wsize__canonical__eqtype_Equality (Obj.magic ws)
      (Obj.magic arm_decl.reg_size))

(** val flags_of_mn : fresh_vars -> arm_mnemonic -> Var.var list **)

let flags_of_mn fv mn =
  let ids =
    match mn with
    | ADD -> []
    | ADC -> []
    | MUL -> []
    | MLA -> []
    | MLS -> []
    | SDIV -> []
    | SUB -> []
    | SBC -> []
    | RSB -> []
    | UDIV -> []
    | UMULL -> []
    | UMAAL -> []
    | UMLAL -> []
    | SMULL -> []
    | SMLAL -> []
    | SMMUL -> []
    | SMMULR -> []
    | SMUL_hw (_, _) -> []
    | SMLA_hw (_, _) -> []
    | SMULW_hw _ -> []
    | AND -> []
    | BFC -> []
    | BFI -> []
    | BIC -> []
    | EOR -> []
    | MVN -> []
    | ORR -> []
    | ASR -> []
    | LSL -> []
    | LSR -> []
    | ROR -> []
    | REV -> []
    | REV16 -> []
    | REVSH -> []
    | ADR -> []
    | MOV -> []
    | MOVT -> []
    | UBFX -> []
    | UXTB -> []
    | UXTH -> []
    | SBFX -> []
    | CLZ -> []
    | CMP -> fvNF :: (fvZF :: (fvCF :: (fvVF :: [])))
    | TST -> fvNF :: (fvZF :: (fvCF :: []))
    | _ -> []
  in
  map (fun x -> x fv) ids

(** val lflags_of_mn : fresh_vars -> var_info -> arm_mnemonic -> lval list **)

let lflags_of_mn fv vi mn =
  map (fun x -> Lvar { v_var = x; v_info = vi }) (flags_of_mn fv mn)

(** val lower_TST : pexpr -> pexpr -> pexpr list option **)

let lower_TST e0 e1 =
  match e0 with
  | Papp2 (s, e00, e01) ->
    (match s with
     | Oland _ ->
       (match e1 with
        | Papp1 (s0, p) ->
          (match s0 with
           | Oword_of_int _ ->
             (match p with
              | Pconst z ->
                (match z with
                 | Z0 -> Some (e00 :: (e01 :: []))
                 | _ -> None)
              | _ -> None)
           | _ -> None)
        | _ -> None)
     | _ -> None)
  | _ -> None

(** val lower_condition_Papp2 :
    fresh_vars -> var_info -> sop2 -> pexpr -> pexpr ->
    ((arm_mnemonic * pexpr) * pexpr list) option **)

let lower_condition_Papp2 fv vi op e0 e1 =
  match cf_of_condition op with
  | Some p ->
    let (cf, ws) = p in
    (match chk_ws_reg ws with
     | Some _ ->
       let cmp = ((CMP, (pexpr_of_cf cf vi (fresh_flags fv))),
         (e0 :: (e1 :: [])))
       in
       (match op with
        | Oeq o ->
          (match o with
           | Op_int -> None
           | Op_w _ ->
             let zf_var = { v_var = (fvZF fv); v_info = vi } in
             let eZF = Pvar (mk_lvar zf_var) in
             Some
             (match lower_TST e0 e1 with
              | Some es -> ((TST, eZF), es)
              | None -> cmp))
        | Oneq o -> (match o with
                     | Op_int -> None
                     | Op_w _ -> Some cmp)
        | Olt c -> (match c with
                    | Cmp_int -> None
                    | Cmp_w (_, _) -> Some cmp)
        | Ole c -> (match c with
                    | Cmp_int -> None
                    | Cmp_w (_, _) -> Some cmp)
        | Ogt c -> (match c with
                    | Cmp_int -> None
                    | Cmp_w (_, _) -> Some cmp)
        | Oge c -> (match c with
                    | Cmp_int -> None
                    | Cmp_w (_, _) -> Some cmp)
        | _ -> None)
     | None -> None)
  | None -> None

(** val lower_condition_pexpr :
    (register, empty, empty, rflag, condt) arch_toIdent -> fresh_vars ->
    var_info -> pexpr -> (((lval list * (register, empty, empty, rflag,
    condt, arm_op, arm_extra_op) extended_op sopn) * pexpr list) * pexpr)
    option **)

let lower_condition_pexpr atoI fv vi e =
  match is_Papp2 e with
  | Some p ->
    let (p0, e1) = p in
    let (op, e0) = p0 in
    (match lower_condition_Papp2 fv vi op e0 e1 with
     | Some p1 ->
       let (p2, es) = p1 in
       let (mn, e') = p2 in
       Some ((((lflags_of_mn fv vi mn),
       (coq_Oarm atoI (ARM_op (mn, default_opts)))), es), e')
     | None -> None)
  | None -> None

(** val lower_condition :
    (register, empty, empty, rflag, condt) arch_toIdent -> fresh_vars ->
    var_info -> pexpr -> (register, empty, empty, rflag, condt, arm_op,
    arm_extra_op) extended_op instr_r list * pexpr **)

let lower_condition atoI fv vi e =
  match lower_condition_pexpr atoI fv vi e with
  | Some p ->
    let (p0, c) = p in
    let (p1, es) = p0 in
    let (lvs, op) = p1 in (((Copn (lvs, AT_none, op, es)) :: []), c)
  | None -> ([], e)

(** val get_arg_shift :
    wsize -> pexpr list -> ((pexpr * shift_kind) * pexpr) option **)

let get_arg_shift ws = function
| [] -> None
| p :: l ->
  (match p with
   | Papp2 (op, v, n) ->
     (match v with
      | Pvar _ ->
        (match n with
         | Papp1 (s, p0) ->
           (match s with
            | Oword_of_int w ->
              (match w with
               | U8 ->
                 (match p0 with
                  | Pconst z ->
                    (match l with
                     | [] ->
                       (match shift_of_sop2 ws op with
                        | Some sh ->
                          (match oassert (check_shift_amount sh z) with
                           | Some _ -> Some ((v, sh), n)
                           | None -> None)
                        | None -> None)
                     | _ :: _ -> None)
                  | _ -> None)
               | _ -> None)
            | _ -> None)
         | _ -> None)
      | _ -> None)
   | _ -> None)

(** val arg_shift :
    arm_mnemonic -> wsize -> pexpr list -> arm_op * pexpr list **)

let arg_shift mn ws e =
  if in_mem (Obj.magic mn)
       (mem (seq_predType arm_mnemonic_eqType)
         (Obj.magic has_shift_mnemonics))
  then (match get_arg_shift ws e with
        | Some p ->
          let (p0, esham) = p in
          let (ebase, sh) = p0 in
          let osh = Some sh in
          let es = ebase :: (esham :: []) in
          let opts = { set_flags = false; is_conditional = false; has_shift =
            osh }
          in
          ((ARM_op (mn, opts)), es)
        | None ->
          let osh = None in
          let opts = { set_flags = false; is_conditional = false; has_shift =
            osh }
          in
          ((ARM_op (mn, opts)), e))
  else let osh = None in
       let opts = { set_flags = false; is_conditional = false; has_shift =
         osh }
       in
       ((ARM_op (mn, opts)), e)

(** val lower_Pvar :
    (register, empty, empty, rflag, condt) arch_toIdent -> wsize -> gvar ->
    low_expr **)

let lower_Pvar atoI ws v =
  match chk_ws_reg ws with
  | Some _ ->
    let mn = if is_var_in_memory v.gv.v_var then LDR else MOV in
    le_issue atoI mn ((Pvar v) :: [])
  | None -> None

(** val lower_load :
    (register, empty, empty, rflag, condt) arch_toIdent -> wsize -> pexpr ->
    low_expr **)

let lower_load atoI ws e =
  match chk_ws_reg ws with
  | Some _ -> le_issue atoI LDR (e :: [])
  | None -> None

(** val mov_imm_op :
    (register, empty, empty, rflag, condt) arch_toIdent -> pexpr ->
    (register, empty, empty, rflag, condt, arm_op, arm_extra_op) extended_op
    sopn **)

let mov_imm_op atoI e =
  if isSome (is_const e)
  then Oasm (ExtOp (Osmart_li U32))
  else coq_Oarm atoI (ARM_op (MOV, default_opts))

(** val lower_Papp1 :
    (register, empty, empty, rflag, condt) arch_toIdent -> wsize -> sop1 ->
    pexpr -> low_expr **)

let lower_Papp1 atoI ws op e =
  match chk_ws_reg ws with
  | Some _ ->
    (match op with
     | Oword_of_int ws' ->
       (match oassert (cmp_le wsize_cmp U32 ws') with
        | Some _ ->
          let op0 = mov_imm_op atoI e in
          le_issue_sopn atoI op0 ((Papp1 ((Oword_of_int U32), e)) :: [])
        | None -> None)
     | Osignext (w, ws') ->
       (match w with
        | U32 ->
          (match oassert (is_load e) with
           | Some _ ->
             (match sload_mn_of_wsize ws' with
              | Some mn -> le_issue atoI mn (e :: [])
              | None -> None)
           | None -> None)
        | _ -> le_skip atoI)
     | Ozeroext (w, ws') ->
       (match w with
        | U32 ->
          (match oassert (is_load e) with
           | Some _ ->
             (match uload_mn_of_wsize ws' with
              | Some mn -> le_issue atoI mn (e :: [])
              | None -> None)
           | None -> None)
        | _ -> le_skip atoI)
     | Olnot w ->
       (match w with
        | U32 ->
          let (op0, es) = arg_shift MVN U32 (e :: []) in
          le_issue_aop atoI op0 es
        | _ -> le_skip atoI)
     | Oneg o ->
       (match o with
        | Op_int -> le_skip atoI
        | Op_w w ->
          (match w with
           | U32 ->
             le_issue atoI RSB (e :: ((wconst U32 (wrepr U32 Z0)) :: []))
           | _ -> le_skip atoI))
     | _ -> le_skip atoI)
  | None -> None

(** val is_mul : pexpr -> (pexpr * pexpr) option **)

let is_mul = function
| Papp2 (s, x, y) ->
  (match s with
   | Omul o ->
     (match o with
      | Op_int -> None
      | Op_w w -> (match w with
                   | U32 -> Some (x, y)
                   | _ -> None))
   | _ -> None)
| _ -> None

(** val is_rsb : wsize -> pexpr -> pexpr -> bool **)

let is_rsb ws e0 e1 =
  match get_arg_shift ws (e0 :: []) with
  | Some _ ->
    (match get_arg_shift ws (e1 :: []) with
     | Some _ -> false
     | None -> true)
  | None ->
    (match get_arg_shift ws (e1 :: []) with
     | Some _ -> false
     | None -> (match is_wconst ws e0 with
                | Some _ -> true
                | None -> false))

(** val lower_Papp2_op :
    wsize -> sop2 -> pexpr -> pexpr -> ((arm_mnemonic * pexpr) * pexpr list)
    option **)

let lower_Papp2_op ws op e0 e1 =
  match chk_ws_reg ws with
  | Some _ ->
    (match op with
     | Oadd o ->
       (match o with
        | Op_int -> None
        | Op_w _ ->
          (match is_mul e0 with
           | Some p -> let (x, y) = p in Some ((MLA, x), (y :: (e1 :: [])))
           | None ->
             (match is_mul e1 with
              | Some p -> let (x, y) = p in Some ((MLA, x), (y :: (e0 :: [])))
              | None -> Some ((ADD, e0), (e1 :: [])))))
     | Omul o ->
       (match o with
        | Op_int -> None
        | Op_w _ -> Some ((MUL, e0), (e1 :: [])))
     | Osub o ->
       (match o with
        | Op_int -> None
        | Op_w _ ->
          (match is_mul e1 with
           | Some p -> let (x, y) = p in Some ((MLS, x), (y :: (e0 :: [])))
           | None ->
             if is_rsb ws e0 e1
             then Some ((RSB, e1), (e0 :: []))
             else Some ((SUB, e0), (e1 :: []))))
     | Odiv (s, o) ->
       (match s with
        | Signed ->
          (match o with
           | Op_int -> None
           | Op_w w ->
             (match w with
              | U32 -> Some ((SDIV, e0), (e1 :: []))
              | _ -> None))
        | Unsigned ->
          (match o with
           | Op_int -> None
           | Op_w w ->
             (match w with
              | U32 -> Some ((UDIV, e0), (e1 :: []))
              | _ -> None)))
     | Oland _ -> Some ((AND, e0), (e1 :: []))
     | Olor _ -> Some ((ORR, e0), (e1 :: []))
     | Olxor _ -> Some ((EOR, e0), (e1 :: []))
     | Olsr w ->
       (match w with
        | U32 ->
          if is_zero (Obj.magic U8) e1
          then Some ((MOV, e0), [])
          else Some ((LSR, e0), (e1 :: []))
        | _ -> None)
     | Olsl o ->
       (match o with
        | Op_int -> None
        | Op_w w ->
          (match w with
           | U32 -> Some ((LSL, e0), (e1 :: []))
           | _ -> None))
     | Oasr o ->
       (match o with
        | Op_int -> None
        | Op_w w ->
          (match w with
           | U32 ->
             if is_zero (Obj.magic U8) e1
             then Some ((MOV, e0), [])
             else Some ((ASR, e0), (e1 :: []))
           | _ -> None))
     | Oror w ->
       (match w with
        | U32 ->
          if is_zero (Obj.magic U8) e1
          then Some ((MOV, e0), [])
          else Some ((ROR, e0), (e1 :: []))
        | _ -> None)
     | Orol w ->
       (match w with
        | U32 ->
          (match is_wconst U8 e1 with
           | Some c ->
             if eq_op
                  (GRing.ComRing.Exports.coq_GRing_ComRing__to__eqtype_Equality
                    (word U8)) c
                  (GRing.zero
                    (GRing.ComRing.Exports.coq_GRing_ComRing__to__GRing_Nmodule
                      (word U8)))
             then Some ((MOV, e0), [])
             else Some ((ROR, e0),
                    ((wconst U8
                       (GRing.add
                         (GRing.SemiRing.Exports.coq_GRing_SemiRing__to__GRing_Nmodule
                           (GRing.Ring.Exports.coq_GRing_Ring__to__GRing_SemiRing
                             (GRing.ComRing.Exports.coq_GRing_ComRing__to__GRing_Ring
                               (word U8))))
                         (GRing.natmul
                           (GRing.SemiRing.Exports.coq_GRing_SemiRing__to__GRing_Nmodule
                             (GRing.Ring.Exports.coq_GRing_Ring__to__GRing_SemiRing
                               (GRing.ComRing.Exports.coq_GRing_ComRing__to__GRing_Ring
                                 (word U8))))
                           (GRing.one
                             (GRing.Ring.Exports.coq_GRing_Ring__to__GRing_SemiRing
                               (GRing.ComRing.Exports.coq_GRing_ComRing__to__GRing_Ring
                                 (word U8)))) (S (S (S (S (S (S (S (S (S (S
                           (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S
                           (S (S (S (S (S O)))))))))))))))))))))))))))))))))
                         (GRing.opp
                           (GRing.ComRing.Exports.coq_GRing_ComRing__to__GRing_Zmodule
                             (word U8)) c))) :: []))
           | None -> None)
        | _ -> None)
     | _ -> None)
  | None -> None

(** val lower_Papp2 :
    (register, empty, empty, rflag, condt) arch_toIdent -> wsize -> sop2 ->
    pexpr -> pexpr -> low_expr **)

let lower_Papp2 atoI ws op e0 e1 =
  match lower_Papp2_op ws op e0 e1 with
  | Some p ->
    let (p0, e1') = p in
    let (mn, e0') = p0 in
    let (aop, es) = arg_shift mn ws e1' in le_issue_aop atoI aop (e0' :: es)
  | None -> None

(** val lower_pexpr_aux :
    (register, empty, empty, rflag, condt) arch_toIdent -> wsize -> pexpr ->
    low_expr **)

let lower_pexpr_aux atoI ws e = match e with
| Pvar v -> lower_Pvar atoI ws v
| Pget (_, _, _, _, _) -> lower_load atoI ws e
| Pload (_, _, _) -> lower_load atoI ws e
| Papp1 (op, e0) -> lower_Papp1 atoI ws op e0
| Papp2 (op, a, b) -> lower_Papp2 atoI ws op a b
| _ -> le_skip atoI

(** val sopn_set_is_conditional :
    (register, empty, empty, rflag, condt) arch_toIdent -> (register, empty,
    empty, rflag, condt, arm_op, arm_extra_op) extended_op sopn -> (register,
    empty, empty, rflag, condt, arm_op, arm_extra_op) extended_op sopn option **)

let sopn_set_is_conditional atoI = function
| Oasm a ->
  (match a with
   | BaseOp a0 ->
     let (o, a1) = a0 in
     (match o with
      | Some _ -> None
      | None ->
        let ARM_op (mn, opts) = a1 in
        Some (coq_Oarm atoI (ARM_op (mn, (set_is_conditional opts)))))
   | ExtOp e ->
     (match e with
      | Osmart_li ws -> Some (Oasm (ExtOp (Osmart_li_cc ws)))
      | _ -> None))
| _ -> None

(** val lower_pexpr :
    (register, empty, empty, rflag, condt) arch_toIdent -> fresh_vars ->
    var_info -> wsize -> pexpr -> (((register, empty, empty, rflag, condt,
    arm_op, arm_extra_op) extended_op instr_r list * (register, empty, empty,
    rflag, condt, arm_op, arm_extra_op) extended_op sopn) * pexpr list) option **)

let lower_pexpr atoI fv vi ws e = match e with
| Pif (a, c, e0, e1) ->
  (match a with
   | Coq_aword ws' ->
     (match oassert
              (eq_op wsize_wsize__canonical__eqtype_Equality (Obj.magic ws)
                (Obj.magic ws')) with
      | Some _ ->
        (match lower_pexpr_aux atoI ws e0 with
         | Some p ->
           let (op, es) = p in
           (match sopn_set_is_conditional atoI op with
            | Some op0 ->
              let (pre, c') = lower_condition atoI fv vi c in
              Some ((pre, op0), (cat es (c' :: (e1 :: []))))
            | None -> None)
         | None -> None)
      | None -> None)
   | _ -> no_pre atoI (lower_pexpr_aux atoI ws e))
| _ -> no_pre atoI (lower_pexpr_aux atoI ws e)

(** val lower_store : wsize -> pexpr -> (arm_op * pexpr list) option **)

let lower_store ws e =
  match store_mn_of_wsize ws with
  | Some mn ->
    (match e with
     | Pconst _ -> None
     | Pbool _ -> None
     | Parr_init (_, _) -> None
     | Pvar _ ->
       let p = (default_opts, (e :: [])) in
       let (opts, es) = p in Some ((ARM_op (mn, opts)), es)
     | Pif (_, c, e0, e1) ->
       let p = ((set_is_conditional default_opts), (e0 :: (c :: (e1 :: []))))
       in
       let (opts, es) = p in Some ((ARM_op (mn, opts)), es)
     | _ -> None)
  | None -> None

(** val lower_cassgn_word :
    (register, empty, empty, rflag, condt) arch_toIdent -> fresh_vars -> lval
    -> wsize -> pexpr -> ((register, empty, empty, rflag, condt, arm_op,
    arm_extra_op) extended_op instr_r list * ((lval list * (register, empty,
    empty, rflag, condt, arm_op, arm_extra_op) extended_op sopn) * pexpr
    list)) option **)

let lower_cassgn_word atoI fv lv ws e =
  let vi = var_info_of_lval lv in
  (match if is_lval_in_memory lv
         then (match lower_store ws e with
               | Some p ->
                 let (aop, es) = p in no_pre atoI (le_issue_aop atoI aop es)
               | None -> None)
         else lower_pexpr atoI fv vi ws e with
   | Some p ->
     let (p0, es) = p in
     let (pre, op) = p0 in Some (pre, (((lv :: []), op), es))
   | None -> None)

(** val lower_cassgn_bool :
    (register, empty, empty, rflag, condt) arch_toIdent -> fresh_vars -> lval
    -> assgn_tag -> pexpr -> (register, empty, empty, rflag, condt, arm_op,
    arm_extra_op) extended_op instr_r list option **)

let lower_cassgn_bool atoI fv lv tag e =
  let vi = var_info_of_lval lv in
  (match lower_condition_pexpr atoI fv vi e with
   | Some p ->
     let (p0, c) = p in
     let (p1, es) = p0 in
     let (lvs, op) = p1 in
     Some ((Copn (lvs, tag, op, es)) :: ((Cassgn (lv, AT_inline, Coq_abool,
     c)) :: []))
   | None -> None)

(** val lower_add_carry :
    (register, empty, empty, rflag, condt) arch_toIdent -> lval list -> pexpr
    list -> ((lval list * (register, empty, empty, rflag, condt, arm_op,
    arm_extra_op) extended_op sopn) * pexpr list) option **)

let lower_add_carry _ lvs es =
  match lvs with
  | [] -> None
  | cf :: l ->
    (match l with
     | [] -> None
     | r :: l0 ->
       (match l0 with
        | [] ->
          (match es with
           | [] -> None
           | x :: l1 ->
             (match l1 with
              | [] -> None
              | y :: l2 ->
                (match l2 with
                 | [] -> None
                 | b :: l3 ->
                   (match l3 with
                    | [] ->
                      (match b with
                       | Pconst _ -> None
                       | Pbool b0 ->
                         if b0
                         then None
                         else let p = (ADD, (x :: (y :: []))) in
                              let (mn, es') = p in
                              let opts = { set_flags = true; is_conditional =
                                false; has_shift = None }
                              in
                              let lnoneb = Lnone (dummy_var_info, Coq_abool)
                              in
                              let lvs' =
                                lnoneb :: (lnoneb :: (cf :: (lnoneb :: (r :: []))))
                              in
                              Some ((lvs', (Oasm (BaseOp (None, (ARM_op (mn,
                              opts)))))), es')
                       | Pvar _ ->
                         let p = (ADC, es) in
                         let (mn, es') = p in
                         let opts = { set_flags = true; is_conditional =
                           false; has_shift = None }
                         in
                         let lnoneb = Lnone (dummy_var_info, Coq_abool) in
                         let lvs' =
                           lnoneb :: (lnoneb :: (cf :: (lnoneb :: (r :: []))))
                         in
                         Some ((lvs', (Oasm (BaseOp (None, (ARM_op (mn,
                         opts)))))), es')
                       | _ -> None)
                    | _ :: _ -> None))))
        | _ :: _ -> None))

(** val lower_mulu :
    (register, empty, empty, rflag, condt) arch_toIdent -> lval list -> pexpr
    list -> ((lval list * (register, empty, empty, rflag, condt, arm_op,
    arm_extra_op) extended_op sopn) * pexpr list) option **)

let lower_mulu _ lvs es =
  match lvs with
  | [] -> None
  | l :: l0 ->
    (match l with
     | Lvar hi ->
       (match l0 with
        | [] -> None
        | l1 :: l2 ->
          (match l1 with
           | Lvar lo ->
             (match l2 with
              | [] ->
                if negb
                     (eq_op Var.coq_MvMake_var__canonical__eqtype_Equality
                       (Obj.magic hi.v_var) (Obj.magic lo.v_var))
                then Some ((((Lvar lo) :: ((Lvar hi) :: [])), (Oasm (BaseOp
                       (None, (ARM_op (UMULL, default_opts)))))), es)
                else None
              | _ :: _ -> None)
           | _ -> None))
     | _ -> None)

(** val with_shift : arm_options -> shift_kind -> arm_options **)

let with_shift opts sh =
  { set_flags = opts.set_flags; is_conditional = opts.is_conditional;
    has_shift = (Some sh) }

(** val lower_base_op :
    (register, empty, empty, rflag, condt) arch_toIdent -> lval list ->
    arm_op -> pexpr list -> ((lval list * (register, empty, empty, rflag,
    condt, arm_op, arm_extra_op) extended_op sopn) * pexpr list) option **)

let lower_base_op _ lvs aop es =
  let ARM_op (mn, opts) = aop in
  if negb
       (eq_op
         (coq_Datatypes_option__canonical__eqtype_Equality
           shift_kind_shift_kind__canonical__eqtype_Equality)
         (Obj.magic opts.has_shift) (Obj.magic None))
  then (match oassert
                (in_mem (Obj.magic mn)
                  (mem (seq_predType arm_mnemonic_eqType)
                    (Obj.magic has_shift_mnemonics))) with
        | Some _ ->
          Some ((lvs, (Oasm (BaseOp (None, (ARM_op (mn, opts)))))), es)
        | None -> None)
  else if eq_op arm_mnemonic_eqType (Obj.magic MVN) (Obj.magic mn)
       then (match es with
             | [] -> None
             | x :: rest ->
               (match get_arg_shift U32 (x :: []) with
                | Some p ->
                  let (p0, esham) = p in
                  let (ebase, sh) = p0 in
                  Some ((lvs, (Oasm (BaseOp (None, (ARM_op (mn,
                  (with_shift opts sh))))))), (ebase :: (esham :: rest)))
                | None ->
                  Some ((lvs, (Oasm (BaseOp (None, (ARM_op (mn, opts)))))),
                    es)))
       else if in_mem (Obj.magic mn)
                 (mem (seq_predType arm_mnemonic_eqType)
                   (Obj.magic
                     (ADD :: (SUB :: (RSB :: (AND :: (BIC :: (EOR :: (ORR :: (CMP :: (TST :: [])))))))))))
            then (match es with
                  | [] -> None
                  | x :: l ->
                    (match l with
                     | [] -> None
                     | y :: rest ->
                       (match get_arg_shift U32 (y :: []) with
                        | Some p ->
                          let (p0, esham) = p in
                          let (ebase, sh) = p0 in
                          Some ((lvs, (Oasm (BaseOp (None, (ARM_op (mn,
                          (with_shift opts sh))))))),
                          (x :: (ebase :: (esham :: rest))))
                        | None ->
                          Some ((lvs, (Oasm (BaseOp (None, (ARM_op (mn,
                            opts)))))), es))))
            else if in_mem (Obj.magic mn)
                      (mem (seq_predType arm_mnemonic_eqType)
                        (Obj.magic (ADC :: (SBC :: []))))
                 then (match es with
                       | [] -> None
                       | x :: l ->
                         (match l with
                          | [] -> None
                          | y :: l0 ->
                            (match l0 with
                             | [] -> None
                             | z :: rest ->
                               (match get_arg_shift U32 (y :: []) with
                                | Some p ->
                                  let (p0, esham) = p in
                                  let (ebase, sh) = p0 in
                                  Some ((lvs, (Oasm (BaseOp (None, (ARM_op
                                  (mn, (with_shift opts sh))))))),
                                  (x :: (ebase :: (z :: (esham :: rest)))))
                                | None ->
                                  Some ((lvs, (Oasm (BaseOp (None, (ARM_op
                                    (mn, opts)))))), es)))))
                 else None

(** val lower_swap :
    (register, empty, empty, rflag, condt) arch_toIdent -> atype -> lval list
    -> pexpr list -> ((lval list * (register, empty, empty, rflag, condt,
    arm_op, arm_extra_op) extended_op sopn) * pexpr list) option **)

let lower_swap _ ty lvs es =
  match ty with
  | Coq_aarr (_, _) -> Some ((lvs, (Opseudo_op (Oswap ty))), es)
  | Coq_aword sz ->
    if cmp_le wsize_cmp sz U32
    then Some ((lvs, (Oasm (ExtOp (Oarm_swap sz)))), es)
    else None
  | _ -> None

(** val lower_pseudo_operator :
    (register, empty, empty, rflag, condt) arch_toIdent -> lval list ->
    pseudo_operator -> pexpr list -> ((lval list * (register, empty, empty,
    rflag, condt, arm_op, arm_extra_op) extended_op sopn) * pexpr list) option **)

let lower_pseudo_operator atoI lvs op es =
  match op with
  | Omulu w -> (match w with
                | U32 -> lower_mulu atoI lvs es
                | _ -> None)
  | Oaddcarry w ->
    (match w with
     | U32 -> lower_add_carry atoI lvs es
     | _ -> None)
  | Oswap ty -> lower_swap atoI ty lvs es
  | _ -> None

(** val lower_copn :
    (register, empty, empty, rflag, condt) arch_toIdent -> lval list ->
    (register, empty, empty, rflag, condt, arm_op, arm_extra_op) extended_op
    sopn -> pexpr list -> ((lval list * (register, empty, empty, rflag,
    condt, arm_op, arm_extra_op) extended_op sopn) * pexpr list) option **)

let lower_copn atoI lvs op es =
  match op with
  | Opseudo_op pop -> lower_pseudo_operator atoI lvs pop es
  | Oslh _ -> None
  | Oasm a ->
    (match a with
     | BaseOp a0 ->
       let (o, aop) = a0 in
       (match o with
        | Some _ -> None
        | None -> lower_base_op atoI lvs aop es)
     | ExtOp _ -> None)

type lowering_options = unit

(** val lower_i :
    (register, empty, empty, rflag, condt) arch_toIdent -> fresh_vars ->
    (register, empty, empty, rflag, condt, arm_op, arm_extra_op) extended_op
    instr -> (register, empty, empty, rflag, condt, arm_op, arm_extra_op)
    extended_op instr list **)

let rec lower_i atoI fv i = match i with
| MkI (ii, ir) ->
  (match ir with
   | Cassgn (lv, tag, ty, e) ->
     let oirs =
       match ty with
       | Coq_abool -> lower_cassgn_bool atoI fv lv tag e
       | Coq_aword ws ->
         (match lower_cassgn_word atoI fv lv ws e with
          | Some p ->
            let (pre, p0) = p in
            let (p1, es) = p0 in
            let (lvs, op) = p1 in
            Some (cat pre ((Copn (lvs, tag, op, es)) :: []))
          | None -> None)
       | _ -> None
     in
     let irs = match oirs with
               | Some irs -> irs
               | None -> ir :: [] in
     map (fun x -> MkI (ii, x)) irs
   | Copn (lvs, tag, op, es) ->
     let ir' =
       match lower_copn atoI lvs op es with
       | Some p ->
         let (p0, es') = p in
         let (lvs', op') = p0 in Copn (lvs', tag, op', es')
       | None -> ir
     in
     (MkI (ii, ir')) :: []
   | Cif (e, c1, c2) ->
     let (pre, e') = lower_condition atoI fv (var_info_of_ii ii) e in
     let c1' = conc_map (lower_i atoI fv) c1 in
     let c2' = conc_map (lower_i atoI fv) c2 in
     map (fun x -> MkI (ii, x)) (cat pre ((Cif (e', c1', c2')) :: []))
   | Cfor (v, r, c) ->
     let c' = conc_map (lower_i atoI fv) c in
     (MkI (ii, (Cfor (v, r, c')))) :: []
   | Cwhile (a, c0, e, info, c1) ->
     let (pre, e') = lower_condition atoI fv (var_info_of_ii info) e in
     let c0' = conc_map (lower_i atoI fv) c0 in
     let c1' = conc_map (lower_i atoI fv) c1 in
     (MkI (ii, (Cwhile (a, (cat c0' (map (fun x -> MkI (info, x)) pre)), e',
     info, c1')))) :: []
   | _ -> i :: [])

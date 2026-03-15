open BinInt
open BinNums
open Datatypes
open Arch_decl
open Arch_extra
open Arch_utils
open Eqtype
open Expr
open Lowering
open Pseudo_operator
open Riscv_decl
open Riscv_extra
open Riscv_instr_decl
open Riscv_params_core
open Seq
open Sopn
open Ssralg
open Ssrbool
open Type
open Utils0
open Var0
open Word0
open Wsize

(** val chk_ws_reg : wsize -> unit option **)

let chk_ws_reg ws =
  oassert
    (eq_op wsize_wsize__canonical__eqtype_Equality (Obj.magic ws)
      (Obj.magic riscv_decl.reg_size))

(** val check_shift_amount : pexpr -> pexpr option **)

let check_shift_amount e =
  match is_wconst U8 e with
  | Some n ->
    if eq_op
         (GRing.ComRing.Exports.coq_GRing_ComRing__to__eqtype_Equality
           (word U8)) n
         (wand U8 n
           (wrepr U8 (Zpos (Coq_xI (Coq_xI (Coq_xI (Coq_xI Coq_xH)))))))
    then Some e
    else None
  | None ->
    (match e with
     | Papp2 (s, a, b) ->
       (match s with
        | Oland _ ->
          (match is_wconst U8 b with
           | Some n ->
             if eq_op
                  (GRing.ComRing.Exports.coq_GRing_ComRing__to__eqtype_Equality
                    (word U8)) n
                  (wrepr U8 (Zpos (Coq_xI (Coq_xI (Coq_xI (Coq_xI Coq_xH))))))
             then Some a
             else None
           | None -> None)
        | _ -> None)
     | _ -> None)

(** val lower_Papp1 :
    (register, empty, empty, empty, condt) arch_toIdent -> wsize -> sop1 ->
    pexpr -> (riscv_extended_op * pexpr list) option **)

let lower_Papp1 _ ws op e =
  match chk_ws_reg ws with
  | Some _ ->
    (match op with
     | Oword_of_int _ ->
       (match is_const e with
        | Some _ ->
          Some ((BaseOp (None, LI)), ((Papp1 ((Oword_of_int U32), e)) :: []))
        | None -> None)
     | Osignext (w, ws') ->
       (match w with
        | U32 ->
          (match oassert (cmp_le wsize_cmp ws' U32) with
           | Some _ ->
             (match oassert (is_load e) with
              | Some _ ->
                Some ((BaseOp (None, (LOAD (Signed, ws')))), (e :: []))
              | None -> None)
           | None -> None)
        | _ -> None)
     | Ozeroext (w, ws') ->
       (match w with
        | U32 ->
          (match oassert (cmp_le wsize_cmp ws' U16) with
           | Some _ ->
             (match oassert (is_load e) with
              | Some _ ->
                Some ((BaseOp (None, (LOAD (Unsigned, ws')))), (e :: []))
              | None -> None)
           | None -> None)
        | _ -> None)
     | Olnot w ->
       (match w with
        | U32 -> Some ((BaseOp (None, NOT)), (e :: []))
        | _ -> None)
     | Oneg o ->
       (match o with
        | Op_int -> None
        | Op_w w ->
          (match w with
           | U32 -> Some ((BaseOp (None, NEG)), (e :: []))
           | _ -> None))
     | _ -> None)
  | None -> None

(** val decide_op_reg_imm :
    (register, empty, empty, empty, condt) arch_toIdent -> wsize -> pexpr ->
    pexpr -> riscv_extended_op -> riscv_extended_op ->
    (riscv_extended_op * pexpr list) option **)

let decide_op_reg_imm _ ws e0 e1 op_reg_reg op_reg_imm =
  match is_wconst ws e1 with
  | Some word0 ->
    if is_arith_small (wsigned ws word0)
    then Some (op_reg_imm, (e0 :: (e1 :: [])))
    else None
  | None -> Some (op_reg_reg, (e0 :: (e1 :: [])))

(** val insert_minus : pexpr -> pexpr option **)

let insert_minus = function
| Papp1 (s, p) ->
  (match s with
   | Oword_of_int sz ->
     (match p with
      | Pconst n -> Some (Papp1 ((Oword_of_int sz), (Pconst (Z.opp n))))
      | _ -> None)
   | _ -> None)
| _ -> None

(** val decide_op_reg_imm_neg :
    (register, empty, empty, empty, condt) arch_toIdent -> wsize -> pexpr ->
    pexpr -> riscv_extended_op -> riscv_extended_op ->
    (riscv_extended_op * pexpr list) option **)

let decide_op_reg_imm_neg _ ws e0 e1 op_reg_reg op_reg_imm =
  match is_wconst ws e1 with
  | Some word0 ->
    if is_arith_small_neg (wsigned ws word0)
    then (match insert_minus e1 with
          | Some e2 -> Some (op_reg_imm, (e0 :: (e2 :: [])))
          | None -> None)
    else None
  | None -> Some (op_reg_reg, (e0 :: (e1 :: [])))

(** val lower_Papp2 :
    (register, empty, empty, empty, condt) arch_toIdent -> wsize -> sop2 ->
    pexpr -> pexpr -> (riscv_extended_op * pexpr list) option **)

let lower_Papp2 atoI ws op e0 e1 =
  match chk_ws_reg ws with
  | Some _ ->
    (match op with
     | Oadd o ->
       (match o with
        | Op_int -> None
        | Op_w _ ->
          decide_op_reg_imm atoI U32 e0 e1 (BaseOp (None, ADD)) (BaseOp
            (None, ADDI)))
     | Omul o ->
       (match o with
        | Op_int -> None
        | Op_w _ -> Some ((BaseOp (None, MUL)), (e0 :: (e1 :: []))))
     | Osub o ->
       (match o with
        | Op_int -> None
        | Op_w _ ->
          decide_op_reg_imm_neg atoI U32 e0 e1 (BaseOp (None, SUB)) (BaseOp
            (None, ADDI)))
     | Odiv (sg, o) ->
       (match o with
        | Op_int -> None
        | Op_w w ->
          (match w with
           | U32 ->
             let o0 = match sg with
                      | Signed -> DIV
                      | Unsigned -> DIVU in
             Some ((BaseOp (None, o0)), (e0 :: (e1 :: [])))
           | _ -> None))
     | Omod (sg, o) ->
       (match o with
        | Op_int -> None
        | Op_w w ->
          (match w with
           | U32 ->
             let o0 = match sg with
                      | Signed -> REM
                      | Unsigned -> REMU in
             Some ((BaseOp (None, o0)), (e0 :: (e1 :: [])))
           | _ -> None))
     | Oland _ ->
       decide_op_reg_imm atoI U32 e0 e1 (BaseOp (None, AND)) (BaseOp (None,
         ANDI))
     | Olor _ ->
       decide_op_reg_imm atoI U32 e0 e1 (BaseOp (None, OR)) (BaseOp (None,
         ORI))
     | Olxor _ ->
       decide_op_reg_imm atoI U32 e0 e1 (BaseOp (None, XOR)) (BaseOp (None,
         XORI))
     | Olsr w ->
       (match w with
        | U32 ->
          (match check_shift_amount e1 with
           | Some e2 ->
             let op0 = if isSome (is_wconst U8 e2) then SRLI else SRL in
             Some ((BaseOp (None, op0)), (e0 :: (e2 :: [])))
           | None -> None)
        | _ -> None)
     | Olsl o ->
       (match o with
        | Op_int -> None
        | Op_w _ ->
          (match check_shift_amount e1 with
           | Some e2 ->
             let op0 = if isSome (is_wconst U8 e2) then SLLI else SLL in
             Some ((BaseOp (None, op0)), (e0 :: (e2 :: [])))
           | None -> None))
     | Oasr o ->
       (match o with
        | Op_int -> None
        | Op_w w ->
          (match w with
           | U32 ->
             (match check_shift_amount e1 with
              | Some e2 ->
                let op0 = if isSome (is_wconst U8 e2) then SRAI else SRA in
                Some ((BaseOp (None, op0)), (e0 :: (e2 :: [])))
              | None -> None)
           | _ -> None))
     | _ -> None)
  | None -> None

(** val lower_load :
    (register, empty, empty, empty, condt) arch_toIdent -> wsize -> pexpr ->
    (riscv_extended_op * pexpr list) option **)

let lower_load _ ws e =
  match chk_ws_reg ws with
  | Some _ -> Some ((BaseOp (None, (LOAD (Signed, U32)))), (e :: []))
  | None -> None

(** val lower_Pvar :
    (register, empty, empty, empty, condt) arch_toIdent -> wsize -> gvar ->
    (riscv_extended_op * pexpr list) option **)

let lower_Pvar _ ws v =
  if negb
       (eq_op wsize_wsize__canonical__eqtype_Equality (Obj.magic ws)
         (Obj.magic U32))
  then None
  else let op = if is_var_in_memory v.gv.v_var then LOAD (Signed, U32) else MV
       in
       Some ((BaseOp (None, op)), ((Pvar v) :: []))

(** val lower_cassgn :
    (register, empty, empty, empty, condt) arch_toIdent -> lval -> wsize ->
    pexpr -> ((lval list * (register, empty, empty, empty, condt, riscv_op,
    riscv_extra_op) extended_op sopn) * pexpr list) option **)

let lower_cassgn atoI lv ws e =
  if is_lval_in_memory lv
  then if cmp_le wsize_cmp ws U32
       then Some (((lv :: []), (coq_Oriscv atoI (STORE ws))), (e :: []))
       else None
  else (match match e with
              | Pconst _ -> None
              | Pbool _ -> None
              | Parr_init (_, _) -> None
              | Pvar v -> lower_Pvar atoI ws v
              | Pget (_, _, _, _, _) -> lower_load atoI ws e
              | Pload (_, _, _) -> lower_load atoI ws e
              | Papp1 (op, e0) -> lower_Papp1 atoI ws op e0
              | Papp2 (op, a, b) -> lower_Papp2 atoI ws op a b
              | _ -> None with
        | Some p -> let (op, e0) = p in Some (((lv :: []), (Oasm op)), e0)
        | None -> None)

(** val lower_swap :
    (register, empty, empty, empty, condt) arch_toIdent -> atype -> lval list
    -> pexpr list -> ((lval list * (register, empty, empty, empty, condt,
    riscv_op, riscv_extra_op) extended_op sopn) * pexpr list) list option **)

let lower_swap _ ty lvs es =
  match ty with
  | Coq_aarr (_, _) -> Some (((lvs, (Opseudo_op (Oswap ty))), es) :: [])
  | Coq_aword sz ->
    if cmp_le wsize_cmp sz U32
    then Some (((lvs, (Oasm (ExtOp (SWAP sz)))), es) :: [])
    else None
  | _ -> None

(** val lower_mulu :
    (register, empty, empty, empty, condt) arch_toIdent -> lval list -> pexpr
    list -> ((lval list * (register, empty, empty, empty, condt, riscv_op,
    riscv_extra_op) extended_op sopn) * pexpr list) list option **)

let lower_mulu _ lvs es =
  match lvs with
  | [] -> None
  | l :: l0 ->
    (match l with
     | Lvar r1 ->
       (match l0 with
        | [] -> None
        | l1 :: l2 ->
          (match l1 with
           | Lvar r2 ->
             (match l2 with
              | [] ->
                (match es with
                 | [] -> None
                 | p :: l3 ->
                   (match p with
                    | Pvar x ->
                      (match l3 with
                       | [] -> None
                       | p0 :: l4 ->
                         (match p0 with
                          | Pvar y ->
                            (match l4 with
                             | [] ->
                               if (||)
                                    (eq_op
                                      Var.coq_MvMake_var__canonical__eqtype_Equality
                                      (Obj.magic r1.v_var)
                                      (Obj.magic x.gv.v_var))
                                    (eq_op
                                      Var.coq_MvMake_var__canonical__eqtype_Equality
                                      (Obj.magic r1.v_var)
                                      (Obj.magic y.gv.v_var))
                               then None
                               else Some (((((Lvar r1) :: []), (Oasm (BaseOp
                                      (None, MULHU)))), es) :: (((((Lvar
                                      r2) :: []), (Oasm (BaseOp (None,
                                      MUL)))), es) :: []))
                             | _ :: _ -> None)
                          | _ -> None))
                    | _ -> None))
              | _ :: _ -> None)
           | _ -> None))
     | _ -> None)

(** val lower_pseudo_operator :
    (register, empty, empty, empty, condt) arch_toIdent -> lval list ->
    pseudo_operator -> pexpr list -> ((lval list * (register, empty, empty,
    empty, condt, riscv_op, riscv_extra_op) extended_op sopn) * pexpr list)
    list option **)

let lower_pseudo_operator atoI lvs op es =
  match op with
  | Omulu w -> (match w with
                | U32 -> lower_mulu atoI lvs es
                | _ -> None)
  | Oswap ty -> lower_swap atoI ty lvs es
  | _ -> None

(** val lower_copn :
    (register, empty, empty, empty, condt) arch_toIdent -> lval list ->
    (register, empty, empty, empty, condt, riscv_op, riscv_extra_op)
    extended_op sopn -> pexpr list -> ((lval list * (register, empty, empty,
    empty, condt, riscv_op, riscv_extra_op) extended_op sopn) * pexpr list)
    list option **)

let lower_copn atoI lvs op es =
  match op with
  | Opseudo_op pop -> lower_pseudo_operator atoI lvs pop es
  | _ -> None

type lowering_options = unit

(** val lower_i :
    (register, empty, empty, empty, condt) arch_toIdent -> (register, empty,
    empty, empty, condt, riscv_op, riscv_extra_op) extended_op instr ->
    (register, empty, empty, empty, condt, riscv_op, riscv_extra_op)
    extended_op instr list **)

let rec lower_i atoI i = match i with
| MkI (ii, ir) ->
  (match ir with
   | Cassgn (lv, tg, ty, e) ->
     let oirs =
       match ty with
       | Coq_abool -> None
       | Coq_aint -> None
       | Coq_aarr (_, _) -> None
       | Coq_aword ws ->
         (match lower_cassgn atoI lv ws e with
          | Some p ->
            let (p0, es) = p in
            let (lvs, op) = p0 in Some ((Copn (lvs, tg, op, es)) :: [])
          | None -> None)
     in
     let irs = match oirs with
               | Some irs -> irs
               | None -> ir :: [] in
     map (fun x -> MkI (ii, x)) irs
   | Copn (lvs, tag, op, es) ->
     let seq_ir =
       match lower_copn atoI lvs op es with
       | Some l ->
         map (fun pat ->
           let (y, es') = pat in
           let (lvs', op') = y in Copn (lvs', tag, op', es')) l
       | None -> ir :: []
     in
     map (fun x -> MkI (ii, x)) seq_ir
   | Cif (e, c1, c2) ->
     let c1' = conc_map (lower_i atoI) c1 in
     let c2' = conc_map (lower_i atoI) c2 in
     (MkI (ii, (Cif (e, c1', c2')))) :: []
   | Cfor (v, r, c) ->
     let c' = conc_map (lower_i atoI) c in (MkI (ii, (Cfor (v, r, c')))) :: []
   | Cwhile (a, c0, e, info, c1) ->
     let c0' = conc_map (lower_i atoI) c0 in
     let c1' = conc_map (lower_i atoI) c1 in
     (MkI (ii, (Cwhile (a, c0', e, info, c1')))) :: []
   | _ -> i :: [])

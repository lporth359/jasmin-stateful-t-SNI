open BinInt
open BinNums
open Bool
open Datatypes
open Eqtype
open Pseudo_operator
open Sem_type
open Seq
open Slh_ops
open Ssralg
open Ssrfun
open Type
open Utils0
open Values
open Var0
open Warray_
open Word0
open Wsize

type arg_constrained_register =
| ACR_any
| ACR_exact of Var.var
| ACR_subset of Var.var list

type arg_desc =
| ADImplicit of Var.var
| ADExplicit of nat * arg_constrained_register

type arg_position =
| APout of nat
| APin of nat

type instruction_desc = { str : (unit -> string); tin : atype list;
                          i_in : arg_desc list; tout : atype list;
                          i_out : arg_desc list;
                          conflicts : (arg_position * arg_position) list;
                          semi : sem_tuple exec sem_prod; i_valid : bool;
                          i_safe : safe_cond list }

(** val str : instruction_desc -> unit -> string **)

let str i =
  i.str

(** val tin : instruction_desc -> atype list **)

let tin i =
  i.tin

(** val i_in : instruction_desc -> arg_desc list **)

let i_in i =
  i.i_in

(** val tout : instruction_desc -> atype list **)

let tout i =
  i.tout

(** val i_out : instruction_desc -> arg_desc list **)

let i_out i =
  i.i_out

(** val conflicts : instruction_desc -> (arg_position * arg_position) list **)

let conflicts i =
  i.conflicts

(** val semi : instruction_desc -> sem_tuple exec sem_prod **)

let semi i =
  i.semi

(** val i_valid : instruction_desc -> bool **)

let i_valid i =
  i.i_valid

(** val i_safe : instruction_desc -> safe_cond list **)

let i_safe i =
  i.i_safe

type prim_x86_suffix =
| PVp of wsize
| PVs of signedness * wsize
| PVv of velem * wsize
| PVsv of signedness * velem * wsize
| PVx of wsize * wsize
| PVvv of velem * wsize * velem * wsize

type 'asm_op prim_constructor =
| PrimX86 of prim_x86_suffix list * (prim_x86_suffix -> 'asm_op option)
| PrimARM of (bool -> bool -> (string, 'asm_op) result)

type 'asm_op asmOp = { _eqT : 'asm_op eqTypeC;
                       asm_op_instr : ('asm_op -> instruction_desc);
                       prim_string : (string * 'asm_op prim_constructor) list }

(** val _eqT : 'a1 asmOp -> 'a1 eqTypeC **)

let _eqT asmOp0 =
  asmOp0._eqT

(** val asm_op_instr : 'a1 asmOp -> 'a1 -> instruction_desc **)

let asm_op_instr asmOp0 =
  asmOp0.asm_op_instr

(** val prim_string : 'a1 asmOp -> (string * 'a1 prim_constructor) list **)

let prim_string asmOp0 =
  asmOp0.prim_string

type 'asm_op asm_op_t = 'asm_op

type 'asm_op sopn =
| Opseudo_op of pseudo_operator
| Oslh of slh_op
| Oasm of 'asm_op asm_op_t

(** val sopn_beq : 'a1 asmOp -> 'a1 sopn -> 'a1 sopn -> bool **)

let sopn_beq asmop o1 o2 =
  match o1 with
  | Opseudo_op o3 ->
    (match o2 with
     | Opseudo_op o4 ->
       eq_op pseudo_operator_eqType (Obj.magic o3) (Obj.magic o4)
     | _ -> false)
  | Oslh o3 ->
    (match o2 with
     | Oslh o4 ->
       eq_op slh_ops_slh_op__canonical__eqtype_Equality (Obj.magic o3)
         (Obj.magic o4)
     | _ -> false)
  | Oasm o3 ->
    (match o2 with
     | Oasm o4 -> eq_op (ceqT_eqType asmop._eqT) (Obj.magic o3) (Obj.magic o4)
     | _ -> false)

(** val sopn_eq_axiom : 'a1 asmOp -> 'a1 sopn eq_axiom **)

let sopn_eq_axiom asmop __top_assumption_ =
  let _evar_0_ = fun _p_ __top_assumption_0 ->
    let _evar_0_ = fun _p1_ ->
      reflect_inj pseudo_operator_eqType (Obj.magic (fun x -> Opseudo_op x))
        _p_ _p1_ (eqP pseudo_operator_eqType _p_ _p1_)
    in
    let _evar_0_0 = fun _ -> ReflectF in
    let _evar_0_1 = fun _ -> ReflectF in
    (match __top_assumption_0 with
     | Opseudo_op p -> Obj.magic _evar_0_ p
     | Oslh s -> _evar_0_0 s
     | Oasm a -> _evar_0_1 a)
  in
  let _evar_0_0 = fun _s_ __top_assumption_0 ->
    let _evar_0_0 = fun _ -> ReflectF in
    let _evar_0_1 = fun _s1_ ->
      reflect_inj slh_ops_slh_op__canonical__eqtype_Equality
        (Obj.magic (fun x -> Oslh x)) _s_ _s1_
        (eqP slh_ops_slh_op__canonical__eqtype_Equality _s_ _s1_)
    in
    let _evar_0_2 = fun _ -> ReflectF in
    (match __top_assumption_0 with
     | Opseudo_op p -> _evar_0_0 p
     | Oslh s -> Obj.magic _evar_0_1 s
     | Oasm a -> _evar_0_2 a)
  in
  let _evar_0_1 = fun _a_ __top_assumption_0 ->
    let _evar_0_1 = fun _ -> ReflectF in
    let _evar_0_2 = fun _ -> ReflectF in
    let _evar_0_3 = fun _a1_ ->
      reflect_inj (ceqT_eqType asmop._eqT) (fun x -> Oasm x) _a_ _a1_
        (eqP (ceqT_eqType asmop._eqT) _a_ _a1_)
    in
    (match __top_assumption_0 with
     | Opseudo_op p -> _evar_0_1 p
     | Oslh s -> _evar_0_2 s
     | Oasm a -> _evar_0_3 a)
  in
  (match __top_assumption_ with
   | Opseudo_op p -> Obj.magic _evar_0_ p
   | Oslh s -> Obj.magic _evar_0_0 s
   | Oasm a -> Obj.magic _evar_0_1 a)

(** val coq_HB_unnamed_factory_1 :
    'a1 asmOp -> 'a1 sopn Coq_hasDecEq.axioms_ **)

let coq_HB_unnamed_factory_1 asmop =
  { Coq_hasDecEq.eq_op = (sopn_beq asmop); Coq_hasDecEq.eqP =
    (sopn_eq_axiom asmop) }

(** val sopn_sopn__canonical__eqtype_Equality :
    'a1 asmOp -> Equality.coq_type **)

let sopn_sopn__canonical__eqtype_Equality asmop =
  Obj.magic coq_HB_unnamed_factory_1 asmop

(** val sopn_copy : 'a1 asmOp -> wsize -> positive -> 'a1 sopn **)

let sopn_copy _ ws p =
  Opseudo_op (Ocopy (ws, p))

(** val sopn_nop : 'a1 asmOp -> 'a1 sopn **)

let sopn_nop _ =
  Opseudo_op Onop

(** val sopn_mulu : 'a1 asmOp -> wsize -> 'a1 sopn **)

let sopn_mulu _ ws =
  Opseudo_op (Omulu ws)

(** val sopn_addcarry : 'a1 asmOp -> wsize -> 'a1 sopn **)

let sopn_addcarry _ ws =
  Opseudo_op (Oaddcarry ws)

(** val sopn_subcarry : 'a1 asmOp -> wsize -> 'a1 sopn **)

let sopn_subcarry _ ws =
  Opseudo_op (Osubcarry ws)

(** val is_Oslh : 'a1 asmOp -> 'a1 sopn -> slh_op option **)

let is_Oslh _ = function
| Oslh op0 -> Some op0
| _ -> None

(** val is_spill_op :
    'a1 asmOp -> 'a1 sopn -> (spill_op * atype list) option **)

let is_spill_op _ = function
| Opseudo_op p ->
  (match p with
   | Ospill (o0, tys) -> Some (o0, tys)
   | _ -> None)
| _ -> None

(** val coq_Ocopy_instr : wsize -> positive -> instruction_desc **)

let coq_Ocopy_instr ws p =
  { str = (pp_sz "copy" ws); tin = ((Coq_aarr (ws, p)) :: []); i_in =
    ((ADExplicit ((S O), ACR_any)) :: []); tout = ((Coq_aarr (ws, p)) :: []);
    i_out = ((ADExplicit (O, ACR_any)) :: []); conflicts = []; semi =
    (Obj.magic WArray.copy ws p); i_valid = true; i_safe = ((AllInit (ws, p,
    O)) :: []) }

(** val declassify_semi : ctype -> sem_tuple exec sem_prod **)

let declassify_semi _ =
  Obj.magic (fun _ -> Ok ())

(** val coq_Odeclassify_instr : atype -> instruction_desc **)

let coq_Odeclassify_instr ty =
  { str = (pp_s (string_of_pseudo_operator (Odeclassify ty))); tin =
    (ty :: []); i_in = ((ADExplicit (O, ACR_any)) :: []); tout = []; i_out =
    []; conflicts = []; semi = (Obj.magic (fun _ -> Ok ())); i_valid = true;
    i_safe = [] }

(** val coq_Odeclassify_mem_instr :
    coq_PointerData -> positive -> instruction_desc **)

let coq_Odeclassify_mem_instr pd len =
  let ty = Coq_aword pd in
  { str = (pp_s (string_of_pseudo_operator (Odeclassify_mem len))); tin =
  (ty :: []); i_in = ((ADExplicit (O, ACR_any)) :: []); tout = []; i_out =
  []; conflicts = []; semi = (Obj.magic (fun _ -> Ok ())); i_valid = true;
  i_safe = [] }

(** val coq_Onop_instr : instruction_desc **)

let coq_Onop_instr =
  { str = (pp_s "NOP"); tin = []; i_in = []; tout = []; i_out = [];
    conflicts = []; semi = (sem_prod_ok (map eval_atype []) (Obj.magic ()));
    i_valid = true; i_safe = [] }

(** val coq_Omulu_instr : wsize -> instruction_desc **)

let coq_Omulu_instr sz =
  { str = (pp_sz "mulu" sz); tin = ((Coq_aword sz) :: ((Coq_aword
    sz) :: [])); i_in = ((ADExplicit (O, ACR_any)) :: ((ADExplicit ((S O),
    ACR_any)) :: [])); tout = ((Coq_aword sz) :: ((Coq_aword sz) :: []));
    i_out = ((ADExplicit ((S (S O)), ACR_any)) :: ((ADExplicit ((S (S (S
    O))), ACR_any)) :: [])); conflicts = []; semi =
    (sem_prod_ok (map eval_atype ((Coq_aword sz) :: ((Coq_aword sz) :: [])))
      (Obj.magic wumul sz)); i_valid = true; i_safe = [] }

(** val coq_Oaddcarry_instr : wsize -> instruction_desc **)

let coq_Oaddcarry_instr sz =
  { str = (pp_sz "adc" sz); tin = ((Coq_aword sz) :: ((Coq_aword
    sz) :: (Coq_abool :: []))); i_in = ((ADExplicit (O,
    ACR_any)) :: ((ADExplicit ((S O), ACR_any)) :: ((ADExplicit ((S (S O)),
    ACR_any)) :: []))); tout = (Coq_abool :: ((Coq_aword sz) :: [])); i_out =
    ((ADExplicit ((S (S (S O))), ACR_any)) :: ((ADExplicit ((S (S (S (S
    O)))), ACR_any)) :: [])); conflicts = []; semi =
    (sem_prod_ok
      (map eval_atype ((Coq_aword sz) :: ((Coq_aword
        sz) :: (Coq_abool :: []))))
      (Obj.magic (fun x y c ->
        let p = waddcarry sz x y c in ((Some (fst p)), (snd p))))); i_valid =
    true; i_safe = [] }

(** val coq_Osubcarry_instr : wsize -> instruction_desc **)

let coq_Osubcarry_instr sz =
  { str = (pp_sz "sbb" sz); tin = ((Coq_aword sz) :: ((Coq_aword
    sz) :: (Coq_abool :: []))); i_in = ((ADExplicit (O,
    ACR_any)) :: ((ADExplicit ((S O), ACR_any)) :: ((ADExplicit ((S (S O)),
    ACR_any)) :: []))); tout = (Coq_abool :: ((Coq_aword sz) :: [])); i_out =
    ((ADExplicit ((S (S (S O))), ACR_any)) :: ((ADExplicit ((S (S (S (S
    O)))), ACR_any)) :: [])); conflicts = []; semi =
    (sem_prod_ok
      (map eval_atype ((Coq_aword sz) :: ((Coq_aword
        sz) :: (Coq_abool :: []))))
      (Obj.magic (fun x y c ->
        let p = wsubcarry sz x y c in ((Some (fst p)), (snd p))))); i_valid =
    true; i_safe = [] }

(** val spill_semi : ctype list -> sem_tuple sem_prod **)

let rec spill_semi = function
| [] -> Obj.magic ()
| _ :: tys0 -> Obj.magic (fun _ -> spill_semi tys0)

(** val coq_Ospill_instr : spill_op -> atype list -> instruction_desc **)

let coq_Ospill_instr o tys =
  let ctys = map eval_atype tys in
  let semi0 = spill_semi ctys in
  { str = (fun _ -> string_of_pseudo_operator (Ospill (o, tys))); tin = tys;
  i_in = (mapi (fun i _ -> ADExplicit (i, ACR_any)) tys); tout = []; i_out =
  []; conflicts = []; semi = (sem_prod_ok ctys semi0); i_valid = true;
  i_safe = [] }

(** val coq_Oswap_instr : atype -> instruction_desc **)

let coq_Oswap_instr ty =
  let cty = eval_atype ty in
  let ctys = cty :: (cty :: []) in
  let semi0 = swap_semi cty in
  { str = (fun _ -> "swap"); tin = (ty :: (ty :: [])); i_in = ((ADExplicit
  (O, ACR_any)) :: ((ADExplicit ((S O), ACR_any)) :: [])); tout =
  (ty :: (ty :: [])); i_out = ((ADExplicit (O, ACR_any)) :: ((ADExplicit ((S
  O), ACR_any)) :: [])); conflicts = []; semi =
  (sem_prod_ok ctys (Obj.magic semi0)); i_valid = true; i_safe = [] }

(** val pseudo_op_get_instr_desc :
    coq_PointerData -> pseudo_operator -> instruction_desc **)

let pseudo_op_get_instr_desc pd = function
| Ospill (o0, tys) -> coq_Ospill_instr o0 tys
| Ocopy (ws, p) -> coq_Ocopy_instr ws p
| Odeclassify t -> coq_Odeclassify_instr t
| Odeclassify_mem len -> coq_Odeclassify_mem_instr pd len
| Onop -> coq_Onop_instr
| Omulu sz -> coq_Omulu_instr sz
| Oaddcarry sz -> coq_Oaddcarry_instr sz
| Osubcarry sz -> coq_Osubcarry_instr sz
| Oswap ty -> coq_Oswap_instr ty

(** val se_init_sem : coq_MSFsize -> GRing.ComRing.sort **)

let se_init_sem msfsz =
  GRing.zero
    (GRing.ComRing.Exports.coq_GRing_ComRing__to__GRing_Nmodule (word msfsz))

(** val se_update_sem :
    coq_MSFsize -> bool -> GRing.ComRing.sort -> GRing.ComRing.sort **)

let se_update_sem msfsz b msf =
  if b
  then msf
  else GRing.opp
         (GRing.Ring.Exports.join_GRing_Ring_between_GRing_SemiRing_and_GRing_Zmodule
           (GRing.ComRing.Exports.coq_GRing_ComRing__to__GRing_Ring
             (word msfsz)))
         (GRing.one
           (GRing.Ring.Exports.coq_GRing_Ring__to__GRing_SemiRing
             (GRing.ComRing.Exports.coq_GRing_ComRing__to__GRing_Ring
               (word msfsz))))

(** val se_move_sem :
    coq_MSFsize -> GRing.ComRing.sort -> GRing.ComRing.sort **)

let se_move_sem _ w =
  w

(** val se_protect_sem :
    coq_MSFsize -> wsize -> GRing.ComRing.sort -> GRing.ComRing.sort ->
    GRing.ComRing.sort **)

let se_protect_sem _ _ w _ =
  w

(** val se_protect_ptr_sem :
    coq_MSFsize -> positive -> WArray.array -> GRing.ComRing.sort ->
    WArray.array **)

let se_protect_ptr_sem _ _ t _ =
  t

(** val se_protect_ptr_fail_sem :
    coq_MSFsize -> positive -> WArray.array -> GRing.ComRing.sort ->
    WArray.array exec **)

let se_protect_ptr_fail_sem msfsz _ t msf =
  if eq_op
       (GRing.ComRing.Exports.coq_GRing_ComRing__to__eqtype_Equality
         (word msfsz)) msf
       (GRing.zero
         (GRing.ComRing.Exports.coq_GRing_ComRing__to__GRing_Nmodule
           (word msfsz)))
  then Ok t
  else let s = ErrSemUndef in Error s

(** val coq_SLHinit_str : string **)

let coq_SLHinit_str =
  "init_msf"

(** val coq_SLHinit_instr : coq_MSFsize -> instruction_desc **)

let coq_SLHinit_instr msfsz =
  { str = (pp_s coq_SLHinit_str); tin = []; i_in = []; tout = ((Coq_aword
    msfsz) :: []); i_out = ((ADExplicit (O, ACR_any)) :: []); conflicts = [];
    semi = (sem_prod_ok (map eval_atype []) (se_init_sem msfsz)); i_valid =
    true; i_safe = [] }

(** val coq_SLHupdate_str : string **)

let coq_SLHupdate_str =
  "update_msf"

(** val coq_SLHupdate_instr : coq_MSFsize -> instruction_desc **)

let coq_SLHupdate_instr msfsz =
  { str = (pp_s coq_SLHupdate_str); tin = (Coq_abool :: ((Coq_aword
    msfsz) :: [])); i_in = ((ADExplicit (O, ACR_any)) :: ((ADExplicit ((S O),
    ACR_any)) :: [])); tout = ((Coq_aword msfsz) :: []); i_out = ((ADExplicit
    ((S (S O)), ACR_any)) :: []); conflicts = []; semi =
    (sem_prod_ok (map eval_atype (Coq_abool :: ((Coq_aword msfsz) :: [])))
      (Obj.magic se_update_sem msfsz)); i_valid = true; i_safe = [] }

(** val coq_SLHmove_str : string **)

let coq_SLHmove_str =
  "mov_msf"

(** val coq_SLHmove_instr : coq_MSFsize -> instruction_desc **)

let coq_SLHmove_instr msfsz =
  { str = (pp_s coq_SLHmove_str); tin = ((Coq_aword msfsz) :: []); i_in =
    ((ADExplicit (O, ACR_any)) :: []); tout = ((Coq_aword msfsz) :: []);
    i_out = ((ADExplicit ((S O), ACR_any)) :: []); conflicts = []; semi =
    (sem_prod_ok (map eval_atype ((Coq_aword msfsz) :: []))
      (Obj.magic se_move_sem msfsz)); i_valid = true; i_safe = [] }

(** val coq_SLHprotect_str : string **)

let coq_SLHprotect_str =
  "protect"

(** val coq_SLHprotect_instr : coq_MSFsize -> wsize -> instruction_desc **)

let coq_SLHprotect_instr msfsz ws =
  { str = (pp_sz coq_SLHprotect_str ws); tin = ((Coq_aword ws) :: ((Coq_aword
    msfsz) :: [])); i_in = ((ADExplicit (O, ACR_any)) :: ((ADExplicit ((S O),
    ACR_any)) :: [])); tout = ((Coq_aword ws) :: []); i_out = ((ADExplicit
    ((S (S O)), ACR_any)) :: []); conflicts = []; semi =
    (sem_prod_ok
      (map eval_atype ((Coq_aword ws) :: ((Coq_aword msfsz) :: [])))
      (Obj.magic se_protect_sem msfsz ws)); i_valid = true; i_safe = [] }

(** val coq_SLHprotect_ptr_str : string **)

let coq_SLHprotect_ptr_str =
  "protect_ptr"

(** val coq_SLHprotect_ptr_instr :
    coq_MSFsize -> wsize -> positive -> instruction_desc **)

let coq_SLHprotect_ptr_instr msfsz ws p =
  let tin0 = (Coq_aarr (ws, p)) :: ((Coq_aword msfsz) :: []) in
  let ctin = map eval_atype tin0 in
  let semi0 = se_protect_ptr_sem msfsz (Z.to_pos (arr_size ws p)) in
  { str = (pp_s coq_SLHprotect_ptr_str); tin = tin0; i_in = ((ADExplicit (O,
  ACR_any)) :: ((ADExplicit ((S O), ACR_any)) :: [])); tout = ((Coq_aarr (ws,
  p)) :: []); i_out = ((ADExplicit ((S (S O)), ACR_any)) :: []); conflicts =
  []; semi = (sem_prod_ok ctin (Obj.magic semi0)); i_valid = true; i_safe =
  [] }

(** val coq_SLHprotect_ptr_fail_str : string **)

let coq_SLHprotect_ptr_fail_str =
  "protect_ptr_fail"

(** val coq_SLHprotect_ptr_fail_instr :
    coq_MSFsize -> wsize -> positive -> instruction_desc **)

let coq_SLHprotect_ptr_fail_instr msfsz ws p =
  let len = Z.to_pos (arr_size ws p) in
  { str = (pp_s coq_SLHprotect_ptr_fail_str); tin = ((Coq_aarr (ws,
  p)) :: ((Coq_aword msfsz) :: [])); i_in = ((ADExplicit (O,
  ACR_any)) :: ((ADExplicit ((S O), ACR_any)) :: [])); tout = ((Coq_aarr (ws,
  p)) :: []); i_out = ((ADExplicit ((S (S O)), ACR_any)) :: []); conflicts =
  []; semi = (Obj.magic se_protect_ptr_fail_sem msfsz len); i_valid = true;
  i_safe = (ScFalse :: []) }

(** val slh_op_instruction_desc :
    coq_MSFsize -> slh_op -> instruction_desc **)

let slh_op_instruction_desc msfsz = function
| SLHinit -> coq_SLHinit_instr msfsz
| SLHupdate -> coq_SLHupdate_instr msfsz
| SLHmove -> coq_SLHmove_instr msfsz
| SLHprotect ws -> coq_SLHprotect_instr msfsz ws
| SLHprotect_ptr (ws, p) -> coq_SLHprotect_ptr_instr msfsz ws p
| SLHprotect_ptr_fail (ws, p) -> coq_SLHprotect_ptr_fail_instr msfsz ws p

(** val get_instr_desc :
    coq_PointerData -> coq_MSFsize -> 'a1 asmOp -> 'a1 sopn ->
    instruction_desc **)

let get_instr_desc pd msfsz asmop = function
| Opseudo_op o0 -> pseudo_op_get_instr_desc pd o0
| Oslh o0 -> slh_op_instruction_desc msfsz o0
| Oasm o0 -> asmop.asm_op_instr o0

(** val string_of_sopn :
    coq_PointerData -> coq_MSFsize -> 'a1 asmOp -> 'a1 sopn -> string **)

let string_of_sopn pd msfsz asmop o =
  (get_instr_desc pd msfsz asmop o).str ()

(** val sopn_tin :
    coq_PointerData -> coq_MSFsize -> 'a1 asmOp -> 'a1 sopn -> atype list **)

let sopn_tin pd msfsz asmop o =
  (get_instr_desc pd msfsz asmop o).tin

(** val sopn_tout :
    coq_PointerData -> coq_MSFsize -> 'a1 asmOp -> 'a1 sopn -> atype list **)

let sopn_tout pd msfsz asmop o =
  (get_instr_desc pd msfsz asmop o).tout

(** val sopn_sem_ :
    coq_PointerData -> coq_MSFsize -> 'a1 asmOp -> 'a1 sopn -> sem_tuple exec
    sem_prod **)

let sopn_sem_ pd msfsz asmop o =
  (get_instr_desc pd msfsz asmop o).semi

(** val sopn_sem :
    coq_PointerData -> coq_MSFsize -> 'a1 asmOp -> 'a1 sopn -> sem_tuple exec
    sem_prod exec **)

let sopn_sem pd msfsz asmop o =
  if (get_instr_desc pd msfsz asmop o).i_valid
  then Ok (sopn_sem_ pd msfsz asmop o)
  else let s = ErrType in Error s

(** val eqC_sopn : 'a1 asmOp -> 'a1 sopn eqTypeC **)

let eqC_sopn asmop =
  { beq = (sopn_beq asmop); ceqP = (sopn_eq_axiom asmop) }

(** val map_prim_constructor :
    ('a1 -> 'a2) -> 'a1 prim_constructor -> 'a2 prim_constructor **)

let map_prim_constructor f = function
| PrimX86 (a, k) -> PrimX86 (a, (fun x -> Ssrfun.Option.map f (k x)))
| PrimARM mk ->
  PrimARM (fun sf ic ->
    match mk sf ic with
    | Ok x -> Ok (f x)
    | Error s -> Error s)

(** val primM : 'a1 -> 'a1 prim_constructor **)

let primM f =
  PrimX86 ([], (fun _ -> Some f))

(** val primP : coq_PointerData -> (wsize -> 'a1) -> 'a1 prim_constructor **)

let primP pd f =
  PrimX86
    ((map (Obj.magic (fun x -> PVp x))
       ((Obj.magic pd) :: (rem wsize_wsize__canonical__eqtype_Equality
                            (Obj.magic pd) (Obj.magic wsizes)))), (fun s ->
    match s with
    | PVp sz -> Some (f sz)
    | _ -> None))

(** val sopn_prim_string :
    coq_PointerData -> 'a1 asmOp -> (string * 'a1 sopn prim_constructor) list **)

let sopn_prim_string pd asmop =
  cat (("copy",
    (primP pd (fun sz -> Opseudo_op (Ocopy (sz, Coq_xH))))) :: (("swap",
    (primM (Opseudo_op (Oswap Coq_abool)))) :: (("mulu",
    (primP pd (fun sz -> Opseudo_op (Omulu sz)))) :: (("adc",
    (primP pd (fun sz -> Opseudo_op (Oaddcarry sz)))) :: (("sbb",
    (primP pd (fun sz -> Opseudo_op (Osubcarry sz)))) :: (("init_msf",
    (primM (Oslh SLHinit))) :: (("update_msf",
    (primM (Oslh SLHupdate))) :: (("mov_msf",
    (primM (Oslh SLHmove))) :: (("protect",
    (primP pd (fun sz -> Oslh (SLHprotect sz)))) :: (("protect_ptr",
    (primM (Oslh (SLHprotect_ptr (U8, Coq_xH))))) :: []))))))))))
    (map (fun pat ->
      let (s, p) = pat in (s, (map_prim_constructor (fun x -> Oasm x) p)))
      asmop.prim_string)

(** val asmOp_sopn :
    coq_PointerData -> coq_MSFsize -> 'a1 asmOp -> 'a1 sopn asmOp **)

let asmOp_sopn pd msfsz asmop =
  { _eqT = (eqC_sopn asmop); asm_op_instr = (get_instr_desc pd msfsz asmop);
    prim_string = (sopn_prim_string pd asmop) }

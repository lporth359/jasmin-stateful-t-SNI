open BinNums
open Bool
open Datatypes
open Arch_decl
open Arch_extra
open Compiler_util
open EqbOK
open Eqb_core_defs
open Eqtype
open Expr
open Fexpr
open Sem_type
open Seq
open Sopn
open Ssralg
open Type
open Utils0
open Var0
open Word0
open Wsize
open X86
open X86_decl
open X86_instr_decl

type __ = Obj.t

module E =
 struct
  (** val pass_name : string **)

  let pass_name =
    "asmgen"

  (** val error : instr_info -> string -> pp_error_loc **)

  let error ii msg =
    { pel_msg = (PPEstring msg); pel_fn = None; pel_fi = None; pel_ii = (Some
      ii); pel_vi = None; pel_pass = (Some pass_name); pel_internal = true }

  (** val se_update_arguments : instr_info -> pp_error_loc **)

  let se_update_arguments ii =
    pp_internal_error_s_at pass_name ii
      "x86_update_msf arguments are invalid."

  (** val se_protect_arguments : instr_info -> pp_error_loc **)

  let se_protect_arguments ii =
    pp_internal_error_s_at pass_name ii "x86_protect arguments are invalid."

  (** val se_protect_ptr : instr_info -> pp_error_loc **)

  let se_protect_ptr ii =
    pp_internal_error_s_at pass_name ii "Found protect_ptr."
 end

type x86_extra_op =
| Oset0 of wsize
| Oconcat128
| Ox86MOVZX32
| Ox86MULX of wsize
| Ox86MULX_hi of wsize
| Ox86SLHinit
| Ox86SLHupdate
| Ox86SLHmove
| Ox86SLHprotect of reg_kind * wsize

(** val x86_extra_op_rect :
    (wsize -> 'a1) -> 'a1 -> 'a1 -> (wsize -> 'a1) -> (wsize -> 'a1) -> 'a1
    -> 'a1 -> 'a1 -> (reg_kind -> wsize -> 'a1) -> x86_extra_op -> 'a1 **)

let x86_extra_op_rect f f0 f1 f2 f3 f4 f5 f6 f7 = function
| Oset0 w -> f w
| Oconcat128 -> f0
| Ox86MOVZX32 -> f1
| Ox86MULX w -> f2 w
| Ox86MULX_hi w -> f3 w
| Ox86SLHinit -> f4
| Ox86SLHupdate -> f5
| Ox86SLHmove -> f6
| Ox86SLHprotect (r, w) -> f7 r w

(** val x86_extra_op_rec :
    (wsize -> 'a1) -> 'a1 -> 'a1 -> (wsize -> 'a1) -> (wsize -> 'a1) -> 'a1
    -> 'a1 -> 'a1 -> (reg_kind -> wsize -> 'a1) -> x86_extra_op -> 'a1 **)

let x86_extra_op_rec f f0 f1 f2 f3 f4 f5 f6 f7 = function
| Oset0 w -> f w
| Oconcat128 -> f0
| Ox86MOVZX32 -> f1
| Ox86MULX w -> f2 w
| Ox86MULX_hi w -> f3 w
| Ox86SLHinit -> f4
| Ox86SLHupdate -> f5
| Ox86SLHmove -> f6
| Ox86SLHprotect (r, w) -> f7 r w

type is_x86_extra_op =
| Coq_is_Oset0 of wsize * is_wsize
| Coq_is_Oconcat128
| Coq_is_Ox86MOVZX32
| Coq_is_Ox86MULX of wsize * is_wsize
| Coq_is_Ox86MULX_hi of wsize * is_wsize
| Coq_is_Ox86SLHinit
| Coq_is_Ox86SLHupdate
| Coq_is_Ox86SLHmove
| Coq_is_Ox86SLHprotect of reg_kind * is_reg_kind * wsize * is_wsize

(** val is_x86_extra_op_rect :
    (wsize -> is_wsize -> 'a1) -> 'a1 -> 'a1 -> (wsize -> is_wsize -> 'a1) ->
    (wsize -> is_wsize -> 'a1) -> 'a1 -> 'a1 -> 'a1 -> (reg_kind ->
    is_reg_kind -> wsize -> is_wsize -> 'a1) -> x86_extra_op ->
    is_x86_extra_op -> 'a1 **)

let is_x86_extra_op_rect f f0 f1 f2 f3 f4 f5 f6 f7 _ = function
| Coq_is_Oset0 (w, p_) -> f w p_
| Coq_is_Oconcat128 -> f0
| Coq_is_Ox86MOVZX32 -> f1
| Coq_is_Ox86MULX (w, p_) -> f2 w p_
| Coq_is_Ox86MULX_hi (w, p_) -> f3 w p_
| Coq_is_Ox86SLHinit -> f4
| Coq_is_Ox86SLHupdate -> f5
| Coq_is_Ox86SLHmove -> f6
| Coq_is_Ox86SLHprotect (r, p_, w, p_0) -> f7 r p_ w p_0

(** val is_x86_extra_op_rec :
    (wsize -> is_wsize -> 'a1) -> 'a1 -> 'a1 -> (wsize -> is_wsize -> 'a1) ->
    (wsize -> is_wsize -> 'a1) -> 'a1 -> 'a1 -> 'a1 -> (reg_kind ->
    is_reg_kind -> wsize -> is_wsize -> 'a1) -> x86_extra_op ->
    is_x86_extra_op -> 'a1 **)

let is_x86_extra_op_rec f f0 f1 f2 f3 f4 f5 f6 f7 _ = function
| Coq_is_Oset0 (w, p_) -> f w p_
| Coq_is_Oconcat128 -> f0
| Coq_is_Ox86MOVZX32 -> f1
| Coq_is_Ox86MULX (w, p_) -> f2 w p_
| Coq_is_Ox86MULX_hi (w, p_) -> f3 w p_
| Coq_is_Ox86SLHinit -> f4
| Coq_is_Ox86SLHupdate -> f5
| Coq_is_Ox86SLHmove -> f6
| Coq_is_Ox86SLHprotect (r, p_, w, p_0) -> f7 r p_ w p_0

(** val x86_extra_op_tag : x86_extra_op -> positive **)

let x86_extra_op_tag = function
| Oset0 _ -> Coq_xH
| Oconcat128 -> Coq_xO Coq_xH
| Ox86MOVZX32 -> Coq_xI Coq_xH
| Ox86MULX _ -> Coq_xO (Coq_xO Coq_xH)
| Ox86MULX_hi _ -> Coq_xI (Coq_xO Coq_xH)
| Ox86SLHinit -> Coq_xO (Coq_xI Coq_xH)
| Ox86SLHupdate -> Coq_xI (Coq_xI Coq_xH)
| Ox86SLHmove -> Coq_xO (Coq_xO (Coq_xO Coq_xH))
| Ox86SLHprotect (_, _) -> Coq_xI (Coq_xO (Coq_xO Coq_xH))

(** val is_x86_extra_op_inhab : x86_extra_op -> is_x86_extra_op **)

let is_x86_extra_op_inhab = function
| Oset0 h -> Coq_is_Oset0 (h, (is_wsize_inhab h))
| Oconcat128 -> Coq_is_Oconcat128
| Ox86MOVZX32 -> Coq_is_Ox86MOVZX32
| Ox86MULX h -> Coq_is_Ox86MULX (h, (is_wsize_inhab h))
| Ox86MULX_hi h -> Coq_is_Ox86MULX_hi (h, (is_wsize_inhab h))
| Ox86SLHinit -> Coq_is_Ox86SLHinit
| Ox86SLHupdate -> Coq_is_Ox86SLHupdate
| Ox86SLHmove -> Coq_is_Ox86SLHmove
| Ox86SLHprotect (h, h0) ->
  Coq_is_Ox86SLHprotect (h, (is_reg_kind_inhab h), h0, (is_wsize_inhab h0))

(** val is_x86_extra_op_functor :
    x86_extra_op -> is_x86_extra_op -> is_x86_extra_op **)

let rec is_x86_extra_op_functor _ x =
  x

type box_x86_extra_op_Oset0 =
  wsize
  (* singleton inductive, whose constructor was Box_x86_extra_op_Oset0 *)

(** val coq_Box_x86_extra_op_Oset0_0 : box_x86_extra_op_Oset0 -> wsize **)

let coq_Box_x86_extra_op_Oset0_0 record =
  record

type box_x86_extra_op_Oconcat128 =
| Box_x86_extra_op_Oconcat128

type box_x86_extra_op_Ox86SLHprotect = { coq_Box_x86_extra_op_Ox86SLHprotect_0 : 
                                         reg_kind;
                                         coq_Box_x86_extra_op_Ox86SLHprotect_1 : 
                                         wsize }

(** val coq_Box_x86_extra_op_Ox86SLHprotect_0 :
    box_x86_extra_op_Ox86SLHprotect -> reg_kind **)

let coq_Box_x86_extra_op_Ox86SLHprotect_0 record =
  record.coq_Box_x86_extra_op_Ox86SLHprotect_0

(** val coq_Box_x86_extra_op_Ox86SLHprotect_1 :
    box_x86_extra_op_Ox86SLHprotect -> wsize **)

let coq_Box_x86_extra_op_Ox86SLHprotect_1 record =
  record.coq_Box_x86_extra_op_Ox86SLHprotect_1

type x86_extra_op_fields_t = __

(** val x86_extra_op_fields : x86_extra_op -> x86_extra_op_fields_t **)

let x86_extra_op_fields = function
| Oset0 h -> Obj.magic h
| Ox86MULX h -> Obj.magic h
| Ox86MULX_hi h -> Obj.magic h
| Ox86SLHprotect (h, h0) ->
  Obj.magic { coq_Box_x86_extra_op_Ox86SLHprotect_0 = h;
    coq_Box_x86_extra_op_Ox86SLHprotect_1 = h0 }
| _ -> Obj.magic Box_x86_extra_op_Oconcat128

(** val x86_extra_op_construct :
    positive -> x86_extra_op_fields_t -> x86_extra_op option **)

let x86_extra_op_construct p b =
  match p with
  | Coq_xI x ->
    (match x with
     | Coq_xI _ -> Some Ox86SLHupdate
     | Coq_xO x0 ->
       (match x0 with
        | Coq_xI _ -> None
        | Coq_xO _ ->
          let { coq_Box_x86_extra_op_Ox86SLHprotect_0 =
            box_x86_extra_op_Ox86SLHprotect_0;
            coq_Box_x86_extra_op_Ox86SLHprotect_1 =
            box_x86_extra_op_Ox86SLHprotect_1 } = Obj.magic b
          in
          Some (Ox86SLHprotect (box_x86_extra_op_Ox86SLHprotect_0,
          box_x86_extra_op_Ox86SLHprotect_1))
        | Coq_xH -> Some (Ox86MULX_hi (Obj.magic b)))
     | Coq_xH -> Some Ox86MOVZX32)
  | Coq_xO x ->
    (match x with
     | Coq_xI _ -> Some Ox86SLHinit
     | Coq_xO x0 ->
       (match x0 with
        | Coq_xI _ -> None
        | Coq_xO _ -> Some Ox86SLHmove
        | Coq_xH -> Some (Ox86MULX (Obj.magic b)))
     | Coq_xH -> Some Oconcat128)
  | Coq_xH -> Some (Oset0 (Obj.magic b))

(** val x86_extra_op_induction :
    (wsize -> is_wsize -> 'a1) -> 'a1 -> 'a1 -> (wsize -> is_wsize -> 'a1) ->
    (wsize -> is_wsize -> 'a1) -> 'a1 -> 'a1 -> 'a1 -> (reg_kind ->
    is_reg_kind -> wsize -> is_wsize -> 'a1) -> x86_extra_op ->
    is_x86_extra_op -> 'a1 **)

let x86_extra_op_induction his_Oset0 his_Oconcat128 his_Ox86MOVZX32 his_Ox86MULX his_Ox86MULX_hi his_Ox86SLHinit his_Ox86SLHupdate his_Ox86SLHmove his_Ox86SLHprotect _ = function
| Coq_is_Oset0 (x0, p_) -> his_Oset0 x0 p_
| Coq_is_Oconcat128 -> his_Oconcat128
| Coq_is_Ox86MOVZX32 -> his_Ox86MOVZX32
| Coq_is_Ox86MULX (x0, p_) -> his_Ox86MULX x0 p_
| Coq_is_Ox86MULX_hi (x0, p_) -> his_Ox86MULX_hi x0 p_
| Coq_is_Ox86SLHinit -> his_Ox86SLHinit
| Coq_is_Ox86SLHupdate -> his_Ox86SLHupdate
| Coq_is_Ox86SLHmove -> his_Ox86SLHmove
| Coq_is_Ox86SLHprotect (x0, p_, x1, p_0) -> his_Ox86SLHprotect x0 p_ x1 p_0

(** val x86_extra_op_eqb_fields :
    (x86_extra_op -> x86_extra_op -> bool) -> positive ->
    x86_extra_op_fields_t -> x86_extra_op_fields_t -> bool **)

let x86_extra_op_eqb_fields _ x a b =
  match x with
  | Coq_xI x0 ->
    (match x0 with
     | Coq_xO x1 ->
       (match x1 with
        | Coq_xI _ -> true
        | Coq_xO _ ->
          let { coq_Box_x86_extra_op_Ox86SLHprotect_0 =
            box_x86_extra_op_Ox86SLHprotect_0;
            coq_Box_x86_extra_op_Ox86SLHprotect_1 =
            box_x86_extra_op_Ox86SLHprotect_1 } = Obj.magic a
          in
          let { coq_Box_x86_extra_op_Ox86SLHprotect_0 =
            box_x86_extra_op_Ox86SLHprotect_2;
            coq_Box_x86_extra_op_Ox86SLHprotect_1 =
            box_x86_extra_op_Ox86SLHprotect_3 } = Obj.magic b
          in
          (&&)
            (reg_kind_eqb box_x86_extra_op_Ox86SLHprotect_0
              box_x86_extra_op_Ox86SLHprotect_2)
            ((&&)
              (wsize_eqb box_x86_extra_op_Ox86SLHprotect_1
                box_x86_extra_op_Ox86SLHprotect_3) true)
        | Coq_xH -> (&&) (wsize_eqb (Obj.magic a) (Obj.magic b)) true)
     | _ -> true)
  | Coq_xO x0 ->
    (match x0 with
     | Coq_xO x1 ->
       (match x1 with
        | Coq_xH -> (&&) (wsize_eqb (Obj.magic a) (Obj.magic b)) true
        | _ -> true)
     | _ -> true)
  | Coq_xH -> (&&) (wsize_eqb (Obj.magic a) (Obj.magic b)) true

(** val x86_extra_op_eqb : x86_extra_op -> x86_extra_op -> bool **)

let x86_extra_op_eqb x1 x2 =
  match x1 with
  | Oset0 h ->
    eqb_body x86_extra_op_tag x86_extra_op_fields
      (Obj.magic x86_extra_op_eqb_fields (fun _ _ -> true))
      (x86_extra_op_tag (Oset0 h)) h x2
  | Ox86MULX h ->
    eqb_body x86_extra_op_tag x86_extra_op_fields
      (Obj.magic x86_extra_op_eqb_fields (fun _ _ -> true))
      (x86_extra_op_tag (Ox86MULX h)) h x2
  | Ox86MULX_hi h ->
    eqb_body x86_extra_op_tag x86_extra_op_fields
      (Obj.magic x86_extra_op_eqb_fields (fun _ _ -> true))
      (x86_extra_op_tag (Ox86MULX_hi h)) h x2
  | Ox86SLHprotect (h, h0) ->
    eqb_body x86_extra_op_tag x86_extra_op_fields
      (Obj.magic x86_extra_op_eqb_fields (fun _ _ -> true))
      (x86_extra_op_tag (Ox86SLHprotect (h, h0)))
      { coq_Box_x86_extra_op_Ox86SLHprotect_0 = h;
      coq_Box_x86_extra_op_Ox86SLHprotect_1 = h0 } x2
  | x ->
    eqb_body x86_extra_op_tag x86_extra_op_fields
      (Obj.magic x86_extra_op_eqb_fields (fun _ _ -> true))
      (x86_extra_op_tag x) Box_x86_extra_op_Oconcat128 x2

(** val x86_extra_op_eqb_OK : x86_extra_op -> x86_extra_op -> reflect **)

let x86_extra_op_eqb_OK =
  iffP2 x86_extra_op_eqb

(** val x86_extra_op_eqb_OK_sumbool : x86_extra_op -> x86_extra_op -> bool **)

let x86_extra_op_eqb_OK_sumbool =
  reflect_dec x86_extra_op_eqb x86_extra_op_eqb_OK

(** val coq_HB_unnamed_factory_1 : x86_extra_op Coq_hasDecEq.axioms_ **)

let coq_HB_unnamed_factory_1 =
  { Coq_hasDecEq.eq_op = x86_extra_op_eqb; Coq_hasDecEq.eqP =
    x86_extra_op_eqb_OK }

(** val x86_extra_x86_extra_op__canonical__eqtype_Equality :
    Equality.coq_type **)

let x86_extra_x86_extra_op__canonical__eqtype_Equality =
  Obj.magic coq_HB_unnamed_factory_1

(** val coq_Oset0_instr :
    (register, register_ext, xmm_register, rflag, condt) arch_toIdent ->
    wsize -> instruction_desc **)

let coq_Oset0_instr atoI sz =
  if cmp_le wsize_cmp sz U64
  then { str = (pp_sz "set0" sz); tin = []; i_in = []; tout =
         (map atype_of_ltype (b5w_ty sz)); i_out =
         (cat (map (sopn_arg_desc x86_decl atoI) implicit_flags) ((ADExplicit
           (O, ACR_any)) :: [])); conflicts = []; semi =
         (sem_prod_ok (map eval_atype [])
           (let vf = Some false in
            let vt = Some true in
            Obj.magic (vf, (vf, (vf, (vt, (vt,
              (GRing.zero
                (GRing.ComRing.Exports.coq_GRing_ComRing__to__GRing_Nmodule
                  (word sz)))))))))); i_valid = true; i_safe = [] }
  else { str = (pp_sz "set0" sz); tin = []; i_in = []; tout =
         (map atype_of_ltype (w_ty sz)); i_out = ((ADExplicit (O,
         ACR_any)) :: []); conflicts = []; semi =
         (sem_prod_ok (map eval_atype [])
           (GRing.zero
             (GRing.ComRing.Exports.coq_GRing_ComRing__to__GRing_Nmodule
               (word sz)))); i_valid = true; i_safe = [] }

(** val coq_Oconcat128_instr : instruction_desc **)

let coq_Oconcat128_instr =
  { str = (pp_s "concat_2u128"); tin = ((Coq_aword U128) :: ((Coq_aword
    U128) :: [])); i_in = ((ADExplicit ((S O), ACR_any)) :: ((ADExplicit ((S
    (S O)), ACR_any)) :: [])); tout = ((Coq_aword U256) :: []); i_out =
    ((ADExplicit (O, ACR_any)) :: []); conflicts = []; semi =
    (sem_prod_ok
      (map eval_atype ((Coq_aword U128) :: ((Coq_aword U128) :: [])))
      (Obj.magic (fun h l -> make_vec U128 U256 (l :: (h :: [])))));
    i_valid = true; i_safe = [] }

(** val coq_Ox86MOVZX32_instr : instruction_desc **)

let coq_Ox86MOVZX32_instr =
  { str = (pp_s "MOVZX32"); tin = ((Coq_aword U32) :: []); i_in =
    ((ADExplicit ((S O), ACR_any)) :: []); tout = ((Coq_aword U64) :: []);
    i_out = ((ADExplicit (O, ACR_any)) :: []); conflicts = []; semi =
    (sem_prod_ok (map eval_atype ((Coq_aword U32) :: []))
      (Obj.magic (fun x -> zero_extend U64 U32 x))); i_valid = true; i_safe =
    [] }

(** val x86_MULX :
    wsize -> GRing.ComRing.sort -> GRing.ComRing.sort -> sem_tuple **)

let x86_MULX sz v1 v2 =
  Obj.magic wumul sz v1 v2

(** val coq_Ox86MULX_instr :
    (register, register_ext, xmm_register, rflag, condt) arch_toIdent ->
    wsize -> instruction_desc **)

let coq_Ox86MULX_instr atoI sz =
  let name = "MULX" in
  { str = (pp_sz name sz); tin = ((Coq_aword sz) :: ((Coq_aword sz) :: []));
  i_in = ((ADImplicit
  (to_var (Coq_lword x86_decl.reg_size) x86_decl.toS_r atoI.toI_r RDX)) :: ((ADExplicit
  ((S (S O)), ACR_any)) :: [])); tout = ((Coq_aword sz) :: ((Coq_aword
  sz) :: [])); i_out = ((ADExplicit (O, ACR_any)) :: ((ADExplicit ((S O),
  ACR_any)) :: [])); conflicts = []; semi =
  (sem_prod_ok (map eval_atype ((Coq_aword sz) :: ((Coq_aword sz) :: [])))
    (Obj.magic x86_MULX sz)); i_valid = (size_32_64 sz); i_safe = [] }

(** val x86_MULX_hi :
    wsize -> GRing.ComRing.sort -> GRing.ComRing.sort -> sem_tuple **)

let x86_MULX_hi =
  wmulhu

(** val coq_Ox86MULX_hi_instr :
    (register, register_ext, xmm_register, rflag, condt) arch_toIdent ->
    wsize -> instruction_desc **)

let coq_Ox86MULX_hi_instr atoI sz =
  let name = "MULX_hi" in
  { str = (pp_sz name sz); tin = ((Coq_aword sz) :: ((Coq_aword sz) :: []));
  i_in = ((ADImplicit
  (to_var (Coq_lword x86_decl.reg_size) x86_decl.toS_r atoI.toI_r RDX)) :: ((ADExplicit
  ((S O), ACR_any)) :: [])); tout = ((Coq_aword sz) :: []); i_out =
  ((ADExplicit (O, ACR_any)) :: []); conflicts = []; semi =
  (sem_prod_ok (map eval_atype ((Coq_aword sz) :: ((Coq_aword sz) :: [])))
    (Obj.magic x86_MULX_hi sz)); i_valid = (size_32_64 sz); i_safe = [] }

(** val coq_Ox86SLHinit_str : string **)

let coq_Ox86SLHinit_str =
  (^) "Ox86_" coq_SLHinit_str

(** val coq_Ox86SLHinit_instr : instruction_desc **)

let coq_Ox86SLHinit_instr =
  { str = (pp_s coq_Ox86SLHinit_str); tin = []; i_in = []; tout = ((Coq_aword
    (arch_msfsz x86_decl)) :: []); i_out = ((ADExplicit (O, ACR_any)) :: []);
    conflicts = []; semi =
    (sem_prod_ok (map eval_atype []) (se_init_sem (arch_msfsz x86_decl)));
    i_valid = true; i_safe = [] }

(** val x86_se_update_sem :
    bool -> GRing.ComRing.sort -> GRing.ComRing.sort * GRing.ComRing.sort **)

let x86_se_update_sem b w =
  let aux = wrepr (arch_pd x86_decl) (Zneg Coq_xH) in
  let w0 = if negb b then aux else w in (aux, w0)

(** val coq_Ox86SLHupdate_str : string **)

let coq_Ox86SLHupdate_str =
  (^) "Ox86_" coq_SLHupdate_str

(** val coq_Ox86SLHupdate_instr : instruction_desc **)

let coq_Ox86SLHupdate_instr =
  { str = (pp_s coq_Ox86SLHupdate_str); tin = (Coq_abool :: ((Coq_aword
    (arch_msfsz x86_decl)) :: [])); i_in = ((ADExplicit (O,
    ACR_any)) :: ((ADExplicit ((S O), ACR_any)) :: [])); tout = ((Coq_aword
    (arch_msfsz x86_decl)) :: ((Coq_aword (arch_msfsz x86_decl)) :: []));
    i_out = ((ADExplicit ((S (S O)), ACR_any)) :: ((ADExplicit ((S O),
    ACR_any)) :: [])); conflicts = []; semi =
    (sem_prod_ok
      (map eval_atype (Coq_abool :: ((Coq_aword
        (arch_msfsz x86_decl)) :: []))) (Obj.magic x86_se_update_sem));
    i_valid = true; i_safe = [] }

(** val coq_Ox86SLHmove_str : string **)

let coq_Ox86SLHmove_str =
  (^) "Ox86_" coq_SLHmove_str

(** val coq_Ox86SLHmove_instr : instruction_desc **)

let coq_Ox86SLHmove_instr =
  { str = (pp_s coq_Ox86SLHmove_str); tin = ((Coq_aword
    (arch_msfsz x86_decl)) :: []); i_in = ((ADExplicit ((S O),
    ACR_any)) :: []); tout = ((Coq_aword (arch_msfsz x86_decl)) :: []);
    i_out = ((ADExplicit (O, ACR_any)) :: []); conflicts = []; semi =
    (sem_prod_ok (map eval_atype ((Coq_aword (arch_msfsz x86_decl)) :: []))
      (Obj.magic se_move_sem (arch_msfsz x86_decl))); i_valid = true;
    i_safe = [] }

(** val se_protect_small_sem :
    wsize -> GRing.ComRing.sort -> GRing.ComRing.sort -> sem_tuple **)

let se_protect_small_sem =
  x86_OR

(** val se_protect_mmx_sem :
    wsize -> GRing.ComRing.sort -> GRing.ComRing.sort -> GRing.ComRing.sort **)

let se_protect_mmx_sem =
  wor

(** val se_protect_large_sem :
    wsize -> GRing.ComRing.sort -> GRing.ComRing.sort ->
    GRing.ComRing.sort * GRing.ComRing.sort **)

let se_protect_large_sem ws w msf =
  let aux = wpbroadcast (arch_msfsz x86_decl) ws msf in (aux, (wor ws w aux))

(** val coq_Ox86SLHprotect_str : string **)

let coq_Ox86SLHprotect_str =
  (^) "Ox86_" coq_SLHprotect_str

(** val coq_Ox86SLHprotect_instr :
    (register, register_ext, xmm_register, rflag, condt) arch_toIdent ->
    reg_kind -> wsize -> instruction_desc **)

let coq_Ox86SLHprotect_instr atoI rk =
  let out =
    cat (map (sopn_arg_desc x86_decl atoI) implicit_flags) ((ADExplicit (O,
      ACR_any)) :: [])
  in
  (fun ws ->
  match rk with
  | Normal ->
    if cmp_le wsize_cmp ws (arch_pd x86_decl)
    then { str = (pp_sz coq_SLHprotect_str ws); tin = ((Coq_aword
           ws) :: ((Coq_aword ws) :: [])); i_in = ((ADExplicit (O,
           ACR_any)) :: ((ADExplicit ((S O), ACR_any)) :: [])); tout =
           (Coq_abool :: (Coq_abool :: (Coq_abool :: (Coq_abool :: (Coq_abool :: ((Coq_aword
           ws) :: [])))))); i_out = out; conflicts = []; semi =
           (sem_prod_ok
             (map eval_atype ((Coq_aword ws) :: ((Coq_aword ws) :: [])))
             (Obj.magic se_protect_small_sem ws)); i_valid = true; i_safe =
           [] }
    else { str = (pp_sz coq_SLHprotect_str ws); tin = ((Coq_aword
           ws) :: ((Coq_aword (arch_msfsz x86_decl)) :: [])); i_in =
           ((ADExplicit (O, ACR_any)) :: ((ADExplicit ((S O),
           ACR_any)) :: [])); tout = ((Coq_aword ws) :: ((Coq_aword
           ws) :: [])); i_out = ((ADExplicit ((S (S O)),
           ACR_any)) :: ((ADExplicit (O, ACR_any)) :: [])); conflicts = [];
           semi =
           (sem_prod_ok
             (map eval_atype ((Coq_aword ws) :: ((Coq_aword
               (arch_msfsz x86_decl)) :: [])))
             (Obj.magic se_protect_large_sem ws)); i_valid =
           (cmp_lt wsize_cmp (arch_pd x86_decl) ws); i_safe = [] }
  | Extra ->
    { str = (pp_sz coq_SLHprotect_str ws); tin = ((Coq_aword
      ws) :: ((Coq_aword ws) :: [])); i_in = ((ADExplicit (O,
      ACR_any)) :: ((ADExplicit ((S O), ACR_any)) :: [])); tout = ((Coq_aword
      ws) :: []); i_out = ((ADExplicit (O, ACR_any)) :: []); conflicts = [];
      semi =
      (sem_prod_ok
        (map eval_atype ((Coq_aword ws) :: ((Coq_aword ws) :: [])))
        (Obj.magic se_protect_mmx_sem ws)); i_valid =
      (eq_op wsize_wsize__canonical__eqtype_Equality (Obj.magic ws)
        (Obj.magic x86_decl.reg_size)); i_safe = [] })

(** val get_instr_desc :
    (register, register_ext, xmm_register, rflag, condt) arch_toIdent ->
    x86_extra_op -> instruction_desc **)

let get_instr_desc atoI = function
| Oset0 ws -> coq_Oset0_instr atoI ws
| Oconcat128 -> coq_Oconcat128_instr
| Ox86MOVZX32 -> coq_Ox86MOVZX32_instr
| Ox86MULX ws -> coq_Ox86MULX_instr atoI ws
| Ox86MULX_hi ws -> coq_Ox86MULX_hi_instr atoI ws
| Ox86SLHinit -> coq_Ox86SLHinit_instr
| Ox86SLHupdate -> coq_Ox86SLHupdate_instr
| Ox86SLHmove -> coq_Ox86SLHmove_instr
| Ox86SLHprotect (rk, ws) -> coq_Ox86SLHprotect_instr atoI rk ws

(** val prim_string : (string * x86_extra_op prim_constructor) list **)

let prim_string =
  ("set0",
    (primP (arch_pd x86_decl) (fun x -> Oset0 x))) :: (("concat_2u128",
    (primM Oconcat128)) :: (("MULX",
    (prim_32_64 (fun x -> Ox86MULX x))) :: (("MULX_hi",
    (prim_32_64 (fun x -> Ox86MULX_hi x))) :: [])))

(** val re_i : wsize -> coq_Z -> rexpr **)

let re_i ws i =
  Rexpr (fconst ws i)

(** val re8_0 : rexpr **)

let re8_0 =
  re_i U8 Z0

(** val re8_1 : rexpr **)

let re8_1 =
  re_i U8 (Zpos Coq_xH)

(** val assemble_slh_init :
    lexpr list -> (((register, register_ext, xmm_register, rflag, condt,
    x86_op) asm_op_msb_t * lexpr list) * rexpr list) list cexec **)

let assemble_slh_init les =
  Ok ((((None, LFENCE), []), []) :: ((((None, (MOV U64)), les),
    ((re_i U64 Z0) :: [])) :: []))

(** val assemble_slh_update :
    instr_info -> lexpr list -> rexpr list -> (((register, register_ext,
    xmm_register, rflag, condt, x86_op) asm_op_msb_t * lexpr list) * rexpr
    list) list cexec **)

let assemble_slh_update ii les res =
  match les with
  | [] -> Error (E.se_update_arguments ii)
  | l1 :: l2 ->
    (match l1 with
     | Store (_, _, _) -> Error (E.se_update_arguments ii)
     | LLvar aux ->
       (match l2 with
        | [] -> Error (E.se_update_arguments ii)
        | ms0 :: l3 ->
          (match l3 with
           | [] ->
             (match res with
              | [] -> Error (E.se_update_arguments ii)
              | r :: l4 ->
                (match r with
                 | Load (_, _, _) -> Error (E.se_update_arguments ii)
                 | Rexpr b ->
                   (match l4 with
                    | [] -> Error (E.se_update_arguments ii)
                    | msf :: l5 ->
                      (match l5 with
                       | [] ->
                         if (&&)
                              (negb
                                ((||)
                                  (SvExtra.Sv.mem (Obj.magic aux.v_var)
                                    (free_vars b))
                                  (SvExtra.Sv.mem (Obj.magic aux.v_var)
                                    (free_vars_r msf))))
                              (convertible (Var.vtype aux.v_var) (Coq_aword
                                U64))
                         then let res' = (Rexpr (Fapp1 (Onot, b))) :: ((Rexpr
                                (Fvar aux)) :: (msf :: []))
                              in
                              Ok ((((None, (MOV U64)), ((LLvar aux) :: [])),
                              ((re_i U64 (Zneg Coq_xH)) :: [])) :: ((((None,
                              (CMOVcc U64)), (ms0 :: [])), res') :: []))
                         else let s = E.se_update_arguments ii in Error s
                       | _ :: _ -> Error (E.se_update_arguments ii)))))
           | _ :: _ -> Error (E.se_update_arguments ii))))

(** val assemble_slh_protect :
    instr_info -> reg_kind -> wsize -> lexpr list -> rexpr list ->
    (((register, register_ext, xmm_register, rflag, condt, x86_op)
    asm_op_msb_t * lexpr list) * rexpr list) list cexec **)

let assemble_slh_protect ii rk ws les res =
  if cmp_le wsize_cmp ws U64
  then Ok ((((None, (match rk with
                     | Normal -> OR ws
                     | Extra -> POR)), les), res) :: [])
  else (match les with
        | [] -> Error (E.se_protect_arguments ii)
        | l1 :: l2 ->
          (match l1 with
           | Store (_, _, _) -> Error (E.se_protect_arguments ii)
           | LLvar aux ->
             (match l2 with
              | [] -> Error (E.se_protect_arguments ii)
              | y :: l3 ->
                (match l3 with
                 | [] ->
                   (match res with
                    | [] -> Error (E.se_protect_arguments ii)
                    | x :: l4 ->
                      (match l4 with
                       | [] -> Error (E.se_protect_arguments ii)
                       | msf :: l5 ->
                         (match l5 with
                          | [] ->
                            if negb
                                 ((||)
                                   (SvExtra.Sv.mem (Obj.magic aux.v_var)
                                     (free_vars_r x))
                                   (SvExtra.Sv.mem (Obj.magic aux.v_var)
                                     (free_vars_r msf)))
                            then let eaux = Rexpr (Fvar aux) in
                                 let laux = (LLvar aux) :: [] in
                                 Ok
                                 (cat ((((None, (VPINSR VE64)), laux),
                                   (eaux :: (msf :: (re8_0 :: [])))) :: ((((None,
                                   (VPINSR VE64)), laux),
                                   (eaux :: (msf :: (re8_1 :: [])))) :: []))
                                   (cat
                                     (if eq_op
                                           wsize_wsize__canonical__eqtype_Equality
                                           (Obj.magic ws) (Obj.magic U256)
                                      then (((None, VINSERTI128), laux),
                                             (eaux :: (eaux :: (re8_1 :: [])))) :: []
                                      else []) ((((None, (VPOR ws)),
                                     (y :: [])), (x :: (eaux :: []))) :: [])))
                            else let s = E.se_protect_arguments ii in Error s
                          | _ :: _ -> Error (E.se_protect_arguments ii))))
                 | _ :: _ -> Error (E.se_protect_arguments ii)))))

(** val assemble_slh_move :
    lexpr list -> rexpr list -> (((register, register_ext, xmm_register,
    rflag, condt, x86_op) asm_op_msb_t * lexpr list) * rexpr list) list cexec **)

let assemble_slh_move les res =
  let lmmx =
    match les with
    | [] -> false
    | l :: l0 ->
      (match l with
       | Store (_, _, _) -> false
       | LLvar x -> (match l0 with
                     | [] -> is_regx x.v_var
                     | _ :: _ -> false))
  in
  let rmmx =
    match res with
    | [] -> false
    | r :: l ->
      (match r with
       | Load (_, _, _) -> false
       | Rexpr f ->
         (match f with
          | Fvar x -> (match l with
                       | [] -> is_regx x.v_var
                       | _ :: _ -> false)
          | _ -> false))
  in
  let op = fun x -> if (||) lmmx rmmx then MOVX x else MOV x in
  Ok ((((None, (op (arch_pd x86_decl))), les), res) :: [])

(** val assemble_extra :
    instr_info -> x86_extra_op -> lexpr list -> rexpr list -> (((register,
    register_ext, xmm_register, rflag, condt, x86_op) asm_op_msb_t * lexpr
    list) * rexpr list) list cexec **)

let assemble_extra ii o outx inx =
  match o with
  | Oset0 sz ->
    let op =
      if cmp_le wsize_cmp sz U64
      then if eq_op wsize_wsize__canonical__eqtype_Equality (Obj.magic sz)
                (Obj.magic U64)
           then ((Some U64), (XOR U32))
           else (None, (XOR sz))
      else (None, (VPXOR sz))
    in
    (match rev outx with
     | [] ->
       let s = E.error ii "set0 : destination is not a register" in Error s
     | y :: _ ->
       (match y with
        | Store (_, _, _) ->
          let s = E.error ii "set0 : destination is not a register" in Error s
        | LLvar x ->
          let x0 = Rexpr (Fvar x) in
          Ok (((op, outx), (x0 :: (x0 :: []))) :: [])))
  | Oconcat128 ->
    (match inx with
     | [] -> let s = E.error ii "Oconcat: assert false" in Error s
     | h :: l0 ->
       (match l0 with
        | [] -> let s = E.error ii "Oconcat: assert false" in Error s
        | l :: l1 ->
          (match l with
           | Load (_, _, _) ->
             let s = E.error ii "Oconcat: assert false" in Error s
           | Rexpr f ->
             (match f with
              | Fconst _ ->
                let s = E.error ii "Oconcat: assert false" in Error s
              | Fvar _ ->
                (match l1 with
                 | [] ->
                   let x = l :: (h :: (re8_1 :: [])) in
                   Ok ((((None, VINSERTI128), outx), x) :: [])
                 | _ :: _ ->
                   let s = E.error ii "Oconcat: assert false" in Error s)
              | _ -> let s = E.error ii "Oconcat: assert false" in Error s))))
  | Ox86MOVZX32 ->
    (match outx with
     | [] ->
       let s = E.error ii "Ox86MOVZX32: destination is not a register" in
       Error s
     | l :: l0 ->
       (match l with
        | Store (_, _, _) ->
          let s = E.error ii "Ox86MOVZX32: destination is not a register" in
          Error s
        | LLvar _ ->
          (match l0 with
           | [] -> Ok ((((None, (MOV U32)), outx), inx) :: [])
           | _ :: _ ->
             let s = E.error ii "Ox86MOVZX32: destination is not a register"
             in
             Error s)))
  | Ox86MULX sz ->
    (match match outx with
           | [] -> Error (E.error ii "Ox86MULX: assert false")
           | h :: l0 ->
             (match h with
              | Store (_, _, _) -> Error (E.error ii "Ox86MULX: assert false")
              | LLvar hi ->
                (match l0 with
                 | [] -> Error (E.error ii "Ox86MULX: assert false")
                 | l :: l1 ->
                   (match l with
                    | Store (_, _, _) ->
                      Error (E.error ii "Ox86MULX: assert false")
                    | LLvar lo ->
                      (match l1 with
                       | [] ->
                         if negb
                              (eq_op
                                Var.coq_MvMake_var__canonical__eqtype_Equality
                                (Obj.magic lo.v_var) (Obj.magic hi.v_var))
                         then Ok (l :: (h :: []))
                         else let s = E.error ii "Ox86MULX: lo = hi" in
                              Error s
                       | _ :: _ -> Error (E.error ii "Ox86MULX: assert false"))))) with
     | Ok x -> Ok ((((None, (MULX_lo_hi sz)), x), inx) :: [])
     | Error s -> Error s)
  | Ox86MULX_hi sz ->
    (match outx with
     | [] -> let s = E.error ii "Ox86MULX_hi: assert false" in Error s
     | l :: l0 ->
       (match l with
        | Store (_, _, _) ->
          let s = E.error ii "Ox86MULX_hi: assert false" in Error s
        | LLvar hi ->
          (match l0 with
           | [] ->
             let x = (LLvar hi) :: ((LLvar hi) :: []) in
             Ok ((((None, (MULX_lo_hi sz)), x), inx) :: [])
           | _ :: _ ->
             let s = E.error ii "Ox86MULX_hi: assert false" in Error s)))
  | Ox86SLHinit -> assemble_slh_init outx
  | Ox86SLHupdate -> assemble_slh_update ii outx inx
  | Ox86SLHmove -> assemble_slh_move outx inx
  | Ox86SLHprotect (rk, ws) -> assemble_slh_protect ii rk ws outx inx

(** val eqC_x86_extra_op : x86_extra_op eqTypeC **)

let eqC_x86_extra_op =
  { beq = x86_extra_op_eqb; ceqP = x86_extra_op_eqb_OK }

(** val x86_extra_op_decl :
    (register, register_ext, xmm_register, rflag, condt) arch_toIdent ->
    x86_extra_op asmOp **)

let x86_extra_op_decl atoI =
  { _eqT = eqC_x86_extra_op; asm_op_instr = (get_instr_desc atoI);
    Sopn.prim_string = prim_string }

(** val x86_extra :
    (register, register_ext, xmm_register, rflag, condt) arch_toIdent ->
    (register, register_ext, xmm_register, rflag, condt, x86_op,
    x86_extra_op) asm_extra **)

let x86_extra atoI =
  { _asm = x86; _atoI = atoI; _extra = (x86_extra_op_decl atoI); to_asm =
    assemble_extra }

type x86_extended_op =
  (register, register_ext, xmm_register, rflag, condt, x86_op, x86_extra_op)
  extended_op

(** val coq_Ox86 :
    (register, register_ext, xmm_register, rflag, condt) arch_toIdent ->
    x86_op -> x86_extended_op sopn **)

let coq_Ox86 _ o =
  Oasm (BaseOp (None, o))

open BinNums
open Bool
open Datatypes
open Prelude
open Arm_expand_imm
open EqbOK
open Eqb_core_defs
open Eqtype
open Expr
open Flag_combination
open Label
open Memory_model
open Param1
open Param1_trivial
open Sem_type
open Seq
open Shift_kind
open Sopn
open Ssralg
open Ssrbool
open Ssrnat
open Type
open Utils0
open Var0
open Word0
open Word_ssrZ
open Wsize

type __ = Obj.t
let __ = let rec f _ = Obj.repr f in Obj.repr f

type 't coq_ToString = { category : string; _finC : 't finTypeC;
                         to_string : ('t -> string) }

(** val category : ltype -> 'a1 coq_ToString -> string **)

let category _ toString =
  toString.category

(** val _finC : ltype -> 'a1 coq_ToString -> 'a1 finTypeC **)

let _finC _ toString =
  toString._finC

(** val to_string : ltype -> 'a1 coq_ToString -> 'a1 -> string **)

let to_string _ toString =
  toString.to_string

(** val rtype : ltype -> 'a1 coq_ToString -> ltype **)

let rtype t _ =
  t

type caimm_checker_s =
| CAimmC_none
| CAimmC_arm_shift_amout of shift_kind
| CAimmC_arm_wencoding of expected_wencoding
| CAimmC_arm_0_8_16_24
| CAimmC_riscv_12bits_signed
| CAimmC_riscv_5bits_unsigned

(** val caimm_checker_s_rect :
    'a1 -> (shift_kind -> 'a1) -> (expected_wencoding -> 'a1) -> 'a1 -> 'a1
    -> 'a1 -> caimm_checker_s -> 'a1 **)

let caimm_checker_s_rect f f0 f1 f2 f3 f4 = function
| CAimmC_none -> f
| CAimmC_arm_shift_amout s -> f0 s
| CAimmC_arm_wencoding e -> f1 e
| CAimmC_arm_0_8_16_24 -> f2
| CAimmC_riscv_12bits_signed -> f3
| CAimmC_riscv_5bits_unsigned -> f4

(** val caimm_checker_s_rec :
    'a1 -> (shift_kind -> 'a1) -> (expected_wencoding -> 'a1) -> 'a1 -> 'a1
    -> 'a1 -> caimm_checker_s -> 'a1 **)

let caimm_checker_s_rec f f0 f1 f2 f3 f4 = function
| CAimmC_none -> f
| CAimmC_arm_shift_amout s -> f0 s
| CAimmC_arm_wencoding e -> f1 e
| CAimmC_arm_0_8_16_24 -> f2
| CAimmC_riscv_12bits_signed -> f3
| CAimmC_riscv_5bits_unsigned -> f4

type is_caimm_checker_s =
| Coq_is_CAimmC_none
| Coq_is_CAimmC_arm_shift_amout of shift_kind * is_shift_kind
| Coq_is_CAimmC_arm_wencoding of expected_wencoding * is_expected_wencoding
| Coq_is_CAimmC_arm_0_8_16_24
| Coq_is_CAimmC_riscv_12bits_signed
| Coq_is_CAimmC_riscv_5bits_unsigned

(** val is_caimm_checker_s_rect :
    'a1 -> (shift_kind -> is_shift_kind -> 'a1) -> (expected_wencoding ->
    is_expected_wencoding -> 'a1) -> 'a1 -> 'a1 -> 'a1 -> caimm_checker_s ->
    is_caimm_checker_s -> 'a1 **)

let is_caimm_checker_s_rect f f0 f1 f2 f3 f4 _ = function
| Coq_is_CAimmC_none -> f
| Coq_is_CAimmC_arm_shift_amout (s, p_) -> f0 s p_
| Coq_is_CAimmC_arm_wencoding (e, p_) -> f1 e p_
| Coq_is_CAimmC_arm_0_8_16_24 -> f2
| Coq_is_CAimmC_riscv_12bits_signed -> f3
| Coq_is_CAimmC_riscv_5bits_unsigned -> f4

(** val is_caimm_checker_s_rec :
    'a1 -> (shift_kind -> is_shift_kind -> 'a1) -> (expected_wencoding ->
    is_expected_wencoding -> 'a1) -> 'a1 -> 'a1 -> 'a1 -> caimm_checker_s ->
    is_caimm_checker_s -> 'a1 **)

let is_caimm_checker_s_rec f f0 f1 f2 f3 f4 _ = function
| Coq_is_CAimmC_none -> f
| Coq_is_CAimmC_arm_shift_amout (s, p_) -> f0 s p_
| Coq_is_CAimmC_arm_wencoding (e, p_) -> f1 e p_
| Coq_is_CAimmC_arm_0_8_16_24 -> f2
| Coq_is_CAimmC_riscv_12bits_signed -> f3
| Coq_is_CAimmC_riscv_5bits_unsigned -> f4

(** val caimm_checker_s_tag : caimm_checker_s -> positive **)

let caimm_checker_s_tag = function
| CAimmC_none -> Coq_xH
| CAimmC_arm_shift_amout _ -> Coq_xO Coq_xH
| CAimmC_arm_wencoding _ -> Coq_xI Coq_xH
| CAimmC_arm_0_8_16_24 -> Coq_xO (Coq_xO Coq_xH)
| CAimmC_riscv_12bits_signed -> Coq_xI (Coq_xO Coq_xH)
| CAimmC_riscv_5bits_unsigned -> Coq_xO (Coq_xI Coq_xH)

(** val is_caimm_checker_s_inhab : caimm_checker_s -> is_caimm_checker_s **)

let is_caimm_checker_s_inhab = function
| CAimmC_none -> Coq_is_CAimmC_none
| CAimmC_arm_shift_amout h ->
  Coq_is_CAimmC_arm_shift_amout (h, (is_shift_kind_inhab h))
| CAimmC_arm_wencoding h ->
  Coq_is_CAimmC_arm_wencoding (h, (is_expected_wencoding_inhab h))
| CAimmC_arm_0_8_16_24 -> Coq_is_CAimmC_arm_0_8_16_24
| CAimmC_riscv_12bits_signed -> Coq_is_CAimmC_riscv_12bits_signed
| CAimmC_riscv_5bits_unsigned -> Coq_is_CAimmC_riscv_5bits_unsigned

(** val is_caimm_checker_s_functor :
    caimm_checker_s -> is_caimm_checker_s -> is_caimm_checker_s **)

let rec is_caimm_checker_s_functor _ x =
  x

type box_caimm_checker_s_CAimmC_none =
| Box_caimm_checker_s_CAimmC_none

type box_caimm_checker_s_CAimmC_arm_shift_amout =
  shift_kind
  (* singleton inductive, whose constructor was Box_caimm_checker_s_CAimmC_arm_shift_amout *)

(** val coq_Box_caimm_checker_s_CAimmC_arm_shift_amout_0 :
    box_caimm_checker_s_CAimmC_arm_shift_amout -> shift_kind **)

let coq_Box_caimm_checker_s_CAimmC_arm_shift_amout_0 record =
  record

type box_caimm_checker_s_CAimmC_arm_wencoding =
  expected_wencoding
  (* singleton inductive, whose constructor was Box_caimm_checker_s_CAimmC_arm_wencoding *)

(** val coq_Box_caimm_checker_s_CAimmC_arm_wencoding_0 :
    box_caimm_checker_s_CAimmC_arm_wencoding -> expected_wencoding **)

let coq_Box_caimm_checker_s_CAimmC_arm_wencoding_0 record =
  record

type caimm_checker_s_fields_t = __

(** val caimm_checker_s_fields :
    caimm_checker_s -> caimm_checker_s_fields_t **)

let caimm_checker_s_fields = function
| CAimmC_arm_shift_amout h -> Obj.magic h
| CAimmC_arm_wencoding h -> Obj.magic h
| _ -> Obj.magic Box_caimm_checker_s_CAimmC_none

(** val caimm_checker_s_construct :
    positive -> caimm_checker_s_fields_t -> caimm_checker_s option **)

let caimm_checker_s_construct p x =
  match p with
  | Coq_xI x0 ->
    (match x0 with
     | Coq_xI _ -> None
     | Coq_xO _ -> Some CAimmC_riscv_12bits_signed
     | Coq_xH -> Some (CAimmC_arm_wencoding (Obj.magic x)))
  | Coq_xO x0 ->
    (match x0 with
     | Coq_xI _ -> Some CAimmC_riscv_5bits_unsigned
     | Coq_xO _ -> Some CAimmC_arm_0_8_16_24
     | Coq_xH -> Some (CAimmC_arm_shift_amout (Obj.magic x)))
  | Coq_xH -> Some CAimmC_none

(** val caimm_checker_s_induction :
    'a1 -> (shift_kind -> is_shift_kind -> 'a1) -> (expected_wencoding ->
    is_expected_wencoding -> 'a1) -> 'a1 -> 'a1 -> 'a1 -> caimm_checker_s ->
    is_caimm_checker_s -> 'a1 **)

let caimm_checker_s_induction his_CAimmC_none his_CAimmC_arm_shift_amout his_CAimmC_arm_wencoding his_CAimmC_arm_0_8_16_24 his_CAimmC_riscv_12bits_signed his_CAimmC_riscv_5bits_unsigned _ = function
| Coq_is_CAimmC_none -> his_CAimmC_none
| Coq_is_CAimmC_arm_shift_amout (x0, p_) -> his_CAimmC_arm_shift_amout x0 p_
| Coq_is_CAimmC_arm_wencoding (x0, p_) -> his_CAimmC_arm_wencoding x0 p_
| Coq_is_CAimmC_arm_0_8_16_24 -> his_CAimmC_arm_0_8_16_24
| Coq_is_CAimmC_riscv_12bits_signed -> his_CAimmC_riscv_12bits_signed
| Coq_is_CAimmC_riscv_5bits_unsigned -> his_CAimmC_riscv_5bits_unsigned

(** val caimm_checker_s_eqb_fields :
    (caimm_checker_s -> caimm_checker_s -> bool) -> positive ->
    caimm_checker_s_fields_t -> caimm_checker_s_fields_t -> bool **)

let caimm_checker_s_eqb_fields _ x x0 x1 =
  match x with
  | Coq_xI x2 ->
    (match x2 with
     | Coq_xH ->
       (&&) (expected_wencoding_eqb (Obj.magic x0) (Obj.magic x1)) true
     | _ -> true)
  | Coq_xO x2 ->
    (match x2 with
     | Coq_xH -> (&&) (shift_kind_eqb (Obj.magic x0) (Obj.magic x1)) true
     | _ -> true)
  | Coq_xH -> true

(** val caimm_checker_s_eqb : caimm_checker_s -> caimm_checker_s -> bool **)

let caimm_checker_s_eqb x1 x2 =
  match x1 with
  | CAimmC_arm_shift_amout h ->
    eqb_body caimm_checker_s_tag caimm_checker_s_fields
      (Obj.magic caimm_checker_s_eqb_fields (fun _ _ -> true))
      (caimm_checker_s_tag (CAimmC_arm_shift_amout h)) h x2
  | CAimmC_arm_wencoding h ->
    eqb_body caimm_checker_s_tag caimm_checker_s_fields
      (Obj.magic caimm_checker_s_eqb_fields (fun _ _ -> true))
      (caimm_checker_s_tag (CAimmC_arm_wencoding h)) h x2
  | x ->
    eqb_body caimm_checker_s_tag caimm_checker_s_fields
      (Obj.magic caimm_checker_s_eqb_fields (fun _ _ -> true))
      (caimm_checker_s_tag x) Box_caimm_checker_s_CAimmC_none x2

(** val caimm_checker_s_eqb_OK :
    caimm_checker_s -> caimm_checker_s -> reflect **)

let caimm_checker_s_eqb_OK =
  iffP2 caimm_checker_s_eqb

(** val caimm_checker_s_eqb_OK_sumbool :
    caimm_checker_s -> caimm_checker_s -> bool **)

let caimm_checker_s_eqb_OK_sumbool =
  reflect_dec caimm_checker_s_eqb caimm_checker_s_eqb_OK

(** val coq_HB_unnamed_factory_1 : caimm_checker_s Coq_hasDecEq.axioms_ **)

let coq_HB_unnamed_factory_1 =
  { Coq_hasDecEq.eq_op = caimm_checker_s_eqb; Coq_hasDecEq.eqP =
    caimm_checker_s_eqb_OK }

(** val arch_decl_caimm_checker_s__canonical__eqtype_Equality :
    Equality.coq_type **)

let arch_decl_caimm_checker_s__canonical__eqtype_Equality =
  Obj.magic coq_HB_unnamed_factory_1

type ('reg, 'regx, 'xreg, 'rflag, 'cond) arch_decl = { reg_size : wsize;
                                                       xreg_size : wsize;
                                                       cond_eqC : 'cond
                                                                  eqTypeC;
                                                       toS_r : 'reg
                                                               coq_ToString;
                                                       toS_rx : 'regx
                                                                coq_ToString;
                                                       toS_x : 'xreg
                                                               coq_ToString;
                                                       toS_f : 'rflag
                                                               coq_ToString;
                                                       ad_rsp : 'reg;
                                                       ad_fcp : coq_FlagCombinationParams;
                                                       check_CAimm : 
                                                       (caimm_checker_s ->
                                                       wsize ->
                                                       GRing.ComRing.sort ->
                                                       bool) }

(** val reg_size : ('a1, 'a2, 'a3, 'a4, 'a5) arch_decl -> wsize **)

let reg_size arch_decl0 =
  arch_decl0.reg_size

(** val xreg_size : ('a1, 'a2, 'a3, 'a4, 'a5) arch_decl -> wsize **)

let xreg_size arch_decl0 =
  arch_decl0.xreg_size

(** val cond_eqC : ('a1, 'a2, 'a3, 'a4, 'a5) arch_decl -> 'a5 eqTypeC **)

let cond_eqC arch_decl0 =
  arch_decl0.cond_eqC

(** val toS_r : ('a1, 'a2, 'a3, 'a4, 'a5) arch_decl -> 'a1 coq_ToString **)

let toS_r arch_decl0 =
  arch_decl0.toS_r

(** val toS_rx : ('a1, 'a2, 'a3, 'a4, 'a5) arch_decl -> 'a2 coq_ToString **)

let toS_rx arch_decl0 =
  arch_decl0.toS_rx

(** val toS_x : ('a1, 'a2, 'a3, 'a4, 'a5) arch_decl -> 'a3 coq_ToString **)

let toS_x arch_decl0 =
  arch_decl0.toS_x

(** val toS_f : ('a1, 'a2, 'a3, 'a4, 'a5) arch_decl -> 'a4 coq_ToString **)

let toS_f arch_decl0 =
  arch_decl0.toS_f

(** val ad_rsp : ('a1, 'a2, 'a3, 'a4, 'a5) arch_decl -> 'a1 **)

let ad_rsp arch_decl0 =
  arch_decl0.ad_rsp

(** val ad_fcp :
    ('a1, 'a2, 'a3, 'a4, 'a5) arch_decl -> coq_FlagCombinationParams **)

let ad_fcp arch_decl0 =
  arch_decl0.ad_fcp

(** val check_CAimm :
    ('a1, 'a2, 'a3, 'a4, 'a5) arch_decl -> caimm_checker_s -> wsize ->
    GRing.ComRing.sort -> bool **)

let check_CAimm arch_decl0 =
  arch_decl0.check_CAimm

(** val arch_pd : ('a1, 'a2, 'a3, 'a4, 'a5) arch_decl -> coq_PointerData **)

let arch_pd h =
  h.reg_size

(** val arch_msfsz : ('a1, 'a2, 'a3, 'a4, 'a5) arch_decl -> coq_MSFsize **)

let arch_msfsz h =
  h.reg_size

(** val mk_ptr :
    ('a1, 'a2, 'a3, 'a4, 'a5) arch_decl -> Ident.Ident.ident -> Var.var **)

let mk_ptr h name =
  { Var.vtype = (Coq_aword (arch_pd h)); Var.vname = name }

type ('reg, 'regx, 'xreg, 'rflag, 'cond) reg_t = 'reg

type ('reg, 'regx, 'xreg, 'rflag, 'cond) regx_t = 'regx

type ('reg, 'regx, 'xreg, 'rflag, 'cond) xreg_t = 'xreg

type ('reg, 'regx, 'xreg, 'rflag, 'cond) rflag_t = 'rflag

type ('reg, 'regx, 'xreg, 'rflag, 'cond) cond_t = 'cond

(** val lreg : ('a1, 'a2, 'a3, 'a4, 'a5) arch_decl -> ltype **)

let lreg arch =
  Coq_lword arch.reg_size

type ('reg, 'regx, 'xreg, 'rflag, 'cond) wreg = sem_t

(** val lxreg : ('a1, 'a2, 'a3, 'a4, 'a5) arch_decl -> ltype **)

let lxreg arch =
  Coq_lword arch.xreg_size

type ('reg, 'regx, 'xreg, 'rflag, 'cond) wxreg = sem_t

type ('reg, 'regx, 'xreg, 'rflag, 'cond) reg_address = { ad_disp : GRing.ComRing.sort;
                                                         ad_base : ('reg,
                                                                   'regx,
                                                                   'xreg,
                                                                   'rflag,
                                                                   'cond)
                                                                   reg_t
                                                                   option;
                                                         ad_scale : nat;
                                                         ad_offset : 
                                                         ('reg, 'regx, 'xreg,
                                                         'rflag, 'cond) reg_t
                                                         option }

(** val ad_disp :
    ('a1, 'a2, 'a3, 'a4, 'a5) arch_decl -> ('a1, 'a2, 'a3, 'a4, 'a5)
    reg_address -> GRing.ComRing.sort **)

let ad_disp _ r =
  r.ad_disp

(** val ad_base :
    ('a1, 'a2, 'a3, 'a4, 'a5) arch_decl -> ('a1, 'a2, 'a3, 'a4, 'a5)
    reg_address -> ('a1, 'a2, 'a3, 'a4, 'a5) reg_t option **)

let ad_base _ r =
  r.ad_base

(** val ad_scale :
    ('a1, 'a2, 'a3, 'a4, 'a5) arch_decl -> ('a1, 'a2, 'a3, 'a4, 'a5)
    reg_address -> nat **)

let ad_scale _ r =
  r.ad_scale

(** val ad_offset :
    ('a1, 'a2, 'a3, 'a4, 'a5) arch_decl -> ('a1, 'a2, 'a3, 'a4, 'a5)
    reg_address -> ('a1, 'a2, 'a3, 'a4, 'a5) reg_t option **)

let ad_offset _ r =
  r.ad_offset

type ('reg, 'regx, 'xreg, 'rflag, 'cond) address =
| Areg of ('reg, 'regx, 'xreg, 'rflag, 'cond) reg_address
| Arip of GRing.ComRing.sort

(** val oeq_reg :
    ('a1, 'a2, 'a3, 'a4, 'a5) arch_decl -> ('a1, 'a2, 'a3, 'a4, 'a5) reg_t
    option -> ('a1, 'a2, 'a3, 'a4, 'a5) reg_t option -> bool **)

let oeq_reg arch x y =
  eq_op
    (reverse_coercion
      (coq_Datatypes_option__canonical__eqtype_Equality
        (ceqT_eqType arch.toS_r._finC._eqC)) __) (Obj.magic x) (Obj.magic y)

(** val reg_address_beq :
    ('a1, 'a2, 'a3, 'a4, 'a5) arch_decl -> ('a1, 'a2, 'a3, 'a4, 'a5)
    reg_address -> ('a1, 'a2, 'a3, 'a4, 'a5) reg_address -> bool **)

let reg_address_beq arch addr1 addr2 =
  let { ad_disp = d1; ad_base = b1; ad_scale = s1; ad_offset = o1 } = addr1 in
  let { ad_disp = d2; ad_base = b2; ad_scale = s2; ad_offset = o2 } = addr2 in
  (&&)
    (eq_op
      (GRing.ComRing.Exports.coq_GRing_ComRing__to__eqtype_Equality
        (word (arch_pd arch))) d1 d2)
    ((&&) (oeq_reg arch b1 b2)
      ((&&)
        (eq_op coq_Datatypes_nat__canonical__eqtype_Equality (Obj.magic s1)
          (Obj.magic s2)) (oeq_reg arch o1 o2)))

(** val reg_address_eq_axiom :
    ('a1, 'a2, 'a3, 'a4, 'a5) arch_decl -> ('a1, 'a2, 'a3, 'a4, 'a5)
    reg_address eq_axiom **)

let reg_address_eq_axiom arch _top_assumption_ =
  let _evar_0_ = fun d1 b1 s1 o1 __top_assumption_ ->
    let _evar_0_ = fun d2 b2 s2 o2 ->
      iffP
        (reg_address_beq arch { ad_disp = d1; ad_base = b1; ad_scale = s1;
          ad_offset = o1 } { ad_disp = d2; ad_base = b2; ad_scale = s2;
          ad_offset = o2 })
        (if reg_address_beq arch { ad_disp = d1; ad_base = b1; ad_scale = s1;
              ad_offset = o1 } { ad_disp = d2; ad_base = b2; ad_scale = s2;
              ad_offset = o2 }
         then ReflectT
         else ReflectF)
    in
    let { ad_disp = ad_disp0; ad_base = ad_base0; ad_scale = ad_scale0;
      ad_offset = ad_offset0 } = __top_assumption_
    in
    _evar_0_ ad_disp0 ad_base0 ad_scale0 ad_offset0
  in
  let { ad_disp = ad_disp0; ad_base = ad_base0; ad_scale = ad_scale0;
    ad_offset = ad_offset0 } = _top_assumption_
  in
  _evar_0_ ad_disp0 ad_base0 ad_scale0 ad_offset0

(** val coq_HB_unnamed_factory_3 :
    ('a1, 'a2, 'a3, 'a4, 'a5) arch_decl -> ('a1, 'a2, 'a3, 'a4, 'a5)
    reg_address Coq_hasDecEq.axioms_ **)

let coq_HB_unnamed_factory_3 arch =
  { Coq_hasDecEq.eq_op = (reg_address_beq arch); Coq_hasDecEq.eqP =
    (reg_address_eq_axiom arch) }

(** val arch_decl_reg_address__canonical__eqtype_Equality :
    ('a1, 'a2, 'a3, 'a4, 'a5) arch_decl -> Equality.coq_type **)

let arch_decl_reg_address__canonical__eqtype_Equality arch =
  Obj.magic coq_HB_unnamed_factory_3 arch

(** val address_beq :
    ('a1, 'a2, 'a3, 'a4, 'a5) arch_decl -> ('a1, 'a2, 'a3, 'a4, 'a5) address
    -> ('a1, 'a2, 'a3, 'a4, 'a5) address -> bool **)

let address_beq arch addr1 addr2 =
  match addr1 with
  | Areg ra1 ->
    (match addr2 with
     | Areg ra2 ->
       eq_op (arch_decl_reg_address__canonical__eqtype_Equality arch)
         (Obj.magic ra1) (Obj.magic ra2)
     | Arip _ -> false)
  | Arip p1 ->
    (match addr2 with
     | Areg _ -> false
     | Arip p2 ->
       eq_op
         (GRing.ComRing.Exports.coq_GRing_ComRing__to__eqtype_Equality
           (word (arch_pd arch))) p1 p2)

(** val address_eq_axiom :
    ('a1, 'a2, 'a3, 'a4, 'a5) arch_decl -> ('a1, 'a2, 'a3, 'a4, 'a5) address
    eq_axiom **)

let address_eq_axiom arch _top_assumption_ =
  let _evar_0_ = fun _r_ __top_assumption_ ->
    let _evar_0_ = fun _r1_ ->
      reflect_inj (arch_decl_reg_address__canonical__eqtype_Equality arch)
        (Obj.magic (fun x -> Areg x)) _r_ _r1_
        (eqP (arch_decl_reg_address__canonical__eqtype_Equality arch) _r_
          _r1_)
    in
    let _evar_0_0 = fun _ -> ReflectF in
    (match __top_assumption_ with
     | Areg r -> Obj.magic _evar_0_ r
     | Arip s -> _evar_0_0 s)
  in
  let _evar_0_0 = fun _s_ __top_assumption_ ->
    let _evar_0_0 = fun _ -> ReflectF in
    let _evar_0_1 = fun _s1_ ->
      reflect_inj
        (GRing.ComRing.Exports.coq_GRing_ComRing__to__eqtype_Equality
          (word arch.reg_size)) (fun x -> Arip x) _s_ _s1_
        (eqP
          (GRing.ComRing.Exports.coq_GRing_ComRing__to__eqtype_Equality
            (word arch.reg_size)) _s_ _s1_)
    in
    (match __top_assumption_ with
     | Areg r -> _evar_0_0 r
     | Arip s -> _evar_0_1 s)
  in
  (match _top_assumption_ with
   | Areg r -> Obj.magic _evar_0_ r
   | Arip s -> _evar_0_0 s)

(** val coq_HB_unnamed_factory_5 :
    ('a1, 'a2, 'a3, 'a4, 'a5) arch_decl -> ('a1, 'a2, 'a3, 'a4, 'a5) address
    Coq_hasDecEq.axioms_ **)

let coq_HB_unnamed_factory_5 arch =
  { Coq_hasDecEq.eq_op = (address_beq arch); Coq_hasDecEq.eqP =
    (address_eq_axiom arch) }

(** val arch_decl_address__canonical__eqtype_Equality :
    ('a1, 'a2, 'a3, 'a4, 'a5) arch_decl -> Equality.coq_type **)

let arch_decl_address__canonical__eqtype_Equality arch =
  Obj.magic coq_HB_unnamed_factory_5 arch

type ('reg, 'regx, 'xreg, 'rflag, 'cond) asm_arg =
| Condt of ('reg, 'regx, 'xreg, 'rflag, 'cond) cond_t
| Imm of wsize * GRing.ComRing.sort
| Reg of ('reg, 'regx, 'xreg, 'rflag, 'cond) reg_t
| Regx of ('reg, 'regx, 'xreg, 'rflag, 'cond) regx_t
| Addr of ('reg, 'regx, 'xreg, 'rflag, 'cond) address
| XReg of ('reg, 'regx, 'xreg, 'rflag, 'cond) xreg_t

type ('reg, 'regx, 'xreg, 'rflag, 'cond) asm_args =
  ('reg, 'regx, 'xreg, 'rflag, 'cond) asm_arg list

(** val is_Condt :
    ('a1, 'a2, 'a3, 'a4, 'a5) arch_decl -> ('a1, 'a2, 'a3, 'a4, 'a5) asm_arg
    -> ('a1, 'a2, 'a3, 'a4, 'a5) cond_t option **)

let is_Condt _ = function
| Condt c -> Some c
| _ -> None

(** val asm_arg_beq :
    ('a1, 'a2, 'a3, 'a4, 'a5) arch_decl -> ('a1, 'a2, 'a3, 'a4, 'a5) asm_arg
    -> ('a1, 'a2, 'a3, 'a4, 'a5) asm_arg -> bool **)

let asm_arg_beq arch a1 a2 =
  match a1 with
  | Condt t1 ->
    (match a2 with
     | Condt t2 ->
       eq_op (ceqT_eqType arch.cond_eqC) (Obj.magic t1) (Obj.magic t2)
     | _ -> false)
  | Imm (sz1, w1) ->
    (match a2 with
     | Imm (sz2, w2) ->
       (&&)
         (eq_op wsize_wsize__canonical__eqtype_Equality (Obj.magic sz1)
           (Obj.magic sz2))
         (eq_op coq_BinNums_Z__canonical__eqtype_Equality
           (Obj.magic wunsigned sz1 w1) (Obj.magic wunsigned sz2 w2))
     | _ -> false)
  | Reg r1 ->
    (match a2 with
     | Reg r2 ->
       eq_op (ceqT_eqType arch.toS_r._finC._eqC) (Obj.magic r1) (Obj.magic r2)
     | _ -> false)
  | Regx r1 ->
    (match a2 with
     | Regx r2 ->
       eq_op (ceqT_eqType arch.toS_rx._finC._eqC) (Obj.magic r1)
         (Obj.magic r2)
     | _ -> false)
  | Addr a3 ->
    (match a2 with
     | Addr a4 ->
       eq_op (arch_decl_address__canonical__eqtype_Equality arch)
         (Obj.magic a3) (Obj.magic a4)
     | _ -> false)
  | XReg r1 ->
    (match a2 with
     | XReg r2 ->
       eq_op (ceqT_eqType arch.toS_x._finC._eqC) (Obj.magic r1) (Obj.magic r2)
     | _ -> false)

(** val asm_arg_eq_axiom :
    ('a1, 'a2, 'a3, 'a4, 'a5) arch_decl -> ('a1, 'a2, 'a3, 'a4, 'a5) asm_arg
    eq_axiom **)

let asm_arg_eq_axiom arch _top_assumption_ =
  let _evar_0_ = fun t1 __top_assumption_ ->
    let _evar_0_ = fun t2 ->
      reflect_inj (ceqT_eqType arch.cond_eqC) (fun x -> Condt x) t1 t2
        (eqP (ceqT_eqType arch.cond_eqC) t1 t2)
    in
    let _evar_0_0 = fun _ _ -> ReflectF in
    let _evar_0_1 = fun _ -> ReflectF in
    let _evar_0_2 = fun _ -> ReflectF in
    let _evar_0_3 = fun _ -> ReflectF in
    let _evar_0_4 = fun _ -> ReflectF in
    (match __top_assumption_ with
     | Condt c -> _evar_0_ c
     | Imm (ws, s) -> _evar_0_0 ws s
     | Reg r -> _evar_0_1 r
     | Regx r -> _evar_0_2 r
     | Addr a -> _evar_0_3 a
     | XReg x -> _evar_0_4 x)
  in
  let _evar_0_0 = fun sz1 w1 __top_assumption_ ->
    let _evar_0_0 = fun _ -> ReflectF in
    let _evar_0_1 = fun sz2 w2 ->
      iffP
        ((&&)
          (eq_op wsize_wsize__canonical__eqtype_Equality (Obj.magic sz1)
            (Obj.magic sz2))
          (eq_op coq_BinNums_Z__canonical__eqtype_Equality
            (Obj.magic wunsigned sz1 w1) (Obj.magic wunsigned sz2 w2)))
        (if (&&)
              (eq_op wsize_wsize__canonical__eqtype_Equality (Obj.magic sz1)
                (Obj.magic sz2))
              (eq_op coq_BinNums_Z__canonical__eqtype_Equality
                (Obj.magic wunsigned sz1 w1) (Obj.magic wunsigned sz2 w2))
         then ReflectT
         else ReflectF)
    in
    let _evar_0_2 = fun _ -> ReflectF in
    let _evar_0_3 = fun _ -> ReflectF in
    let _evar_0_4 = fun _ -> ReflectF in
    let _evar_0_5 = fun _ -> ReflectF in
    (match __top_assumption_ with
     | Condt c -> _evar_0_0 c
     | Imm (ws, s) -> _evar_0_1 ws s
     | Reg r -> _evar_0_2 r
     | Regx r -> _evar_0_3 r
     | Addr a -> _evar_0_4 a
     | XReg x -> _evar_0_5 x)
  in
  let _evar_0_1 = fun r1 __top_assumption_ ->
    let _evar_0_1 = fun _ -> ReflectF in
    let _evar_0_2 = fun _ _ -> ReflectF in
    let _evar_0_3 = fun r2 ->
      reflect_inj (ceqT_eqType arch.toS_r._finC._eqC) (fun x -> Reg x) r1 r2
        (eqP (ceqT_eqType arch.toS_r._finC._eqC) r1 r2)
    in
    let _evar_0_4 = fun _ -> ReflectF in
    let _evar_0_5 = fun _ -> ReflectF in
    let _evar_0_6 = fun _ -> ReflectF in
    (match __top_assumption_ with
     | Condt c -> _evar_0_1 c
     | Imm (ws, s) -> _evar_0_2 ws s
     | Reg r -> _evar_0_3 r
     | Regx r -> _evar_0_4 r
     | Addr a -> _evar_0_5 a
     | XReg x -> _evar_0_6 x)
  in
  let _evar_0_2 = fun r1 __top_assumption_ ->
    let _evar_0_2 = fun _ -> ReflectF in
    let _evar_0_3 = fun _ _ -> ReflectF in
    let _evar_0_4 = fun _ -> ReflectF in
    let _evar_0_5 = fun r2 ->
      reflect_inj (ceqT_eqType arch.toS_rx._finC._eqC) (fun x -> Regx x) r1
        r2 (eqP (ceqT_eqType arch.toS_rx._finC._eqC) r1 r2)
    in
    let _evar_0_6 = fun _ -> ReflectF in
    let _evar_0_7 = fun _ -> ReflectF in
    (match __top_assumption_ with
     | Condt c -> _evar_0_2 c
     | Imm (ws, s) -> _evar_0_3 ws s
     | Reg r -> _evar_0_4 r
     | Regx r -> _evar_0_5 r
     | Addr a -> _evar_0_6 a
     | XReg x -> _evar_0_7 x)
  in
  let _evar_0_3 = fun a1 __top_assumption_ ->
    let _evar_0_3 = fun _ -> ReflectF in
    let _evar_0_4 = fun _ _ -> ReflectF in
    let _evar_0_5 = fun _ -> ReflectF in
    let _evar_0_6 = fun _ -> ReflectF in
    let _evar_0_7 = fun a2 ->
      reflect_inj (arch_decl_address__canonical__eqtype_Equality arch)
        (Obj.magic (fun x -> Addr x)) a1 a2
        (eqP (arch_decl_address__canonical__eqtype_Equality arch) a1 a2)
    in
    let _evar_0_8 = fun _ -> ReflectF in
    (match __top_assumption_ with
     | Condt c -> _evar_0_3 c
     | Imm (ws, s) -> _evar_0_4 ws s
     | Reg r -> _evar_0_5 r
     | Regx r -> _evar_0_6 r
     | Addr a -> Obj.magic _evar_0_7 a
     | XReg x -> _evar_0_8 x)
  in
  let _evar_0_4 = fun xr1 __top_assumption_ ->
    let _evar_0_4 = fun _ -> ReflectF in
    let _evar_0_5 = fun _ _ -> ReflectF in
    let _evar_0_6 = fun _ -> ReflectF in
    let _evar_0_7 = fun _ -> ReflectF in
    let _evar_0_8 = fun _ -> ReflectF in
    let _evar_0_9 = fun xr2 ->
      reflect_inj (ceqT_eqType arch.toS_x._finC._eqC) (fun x -> XReg x) xr1
        xr2 (eqP (ceqT_eqType arch.toS_x._finC._eqC) xr1 xr2)
    in
    (match __top_assumption_ with
     | Condt c -> _evar_0_4 c
     | Imm (ws, s) -> _evar_0_5 ws s
     | Reg r -> _evar_0_6 r
     | Regx r -> _evar_0_7 r
     | Addr a -> _evar_0_8 a
     | XReg x -> _evar_0_9 x)
  in
  (match _top_assumption_ with
   | Condt c -> Obj.magic _evar_0_ c
   | Imm (ws, s) -> _evar_0_0 ws s
   | Reg r -> Obj.magic _evar_0_1 r
   | Regx r -> Obj.magic _evar_0_2 r
   | Addr a -> Obj.magic _evar_0_3 a
   | XReg x -> Obj.magic _evar_0_4 x)

(** val coq_HB_unnamed_factory_7 :
    ('a1, 'a2, 'a3, 'a4, 'a5) arch_decl -> ('a1, 'a2, 'a3, 'a4, 'a5) asm_arg
    Coq_hasDecEq.axioms_ **)

let coq_HB_unnamed_factory_7 arch =
  { Coq_hasDecEq.eq_op = (asm_arg_beq arch); Coq_hasDecEq.eqP =
    (asm_arg_eq_axiom arch) }

(** val arch_decl_asm_arg__canonical__eqtype_Equality :
    ('a1, 'a2, 'a3, 'a4, 'a5) arch_decl -> Equality.coq_type **)

let arch_decl_asm_arg__canonical__eqtype_Equality arch =
  Obj.magic coq_HB_unnamed_factory_7 arch

type msb_flag =
| MSB_CLEAR
| MSB_MERGE

(** val msb_flag_rect : 'a1 -> 'a1 -> msb_flag -> 'a1 **)

let msb_flag_rect f f0 = function
| MSB_CLEAR -> f
| MSB_MERGE -> f0

(** val msb_flag_rec : 'a1 -> 'a1 -> msb_flag -> 'a1 **)

let msb_flag_rec f f0 = function
| MSB_CLEAR -> f
| MSB_MERGE -> f0

type is_msb_flag =
| Coq_is_MSB_CLEAR
| Coq_is_MSB_MERGE

(** val is_msb_flag_rect : 'a1 -> 'a1 -> msb_flag -> is_msb_flag -> 'a1 **)

let is_msb_flag_rect f f0 _ = function
| Coq_is_MSB_CLEAR -> f
| Coq_is_MSB_MERGE -> f0

(** val is_msb_flag_rec : 'a1 -> 'a1 -> msb_flag -> is_msb_flag -> 'a1 **)

let is_msb_flag_rec f f0 _ = function
| Coq_is_MSB_CLEAR -> f
| Coq_is_MSB_MERGE -> f0

(** val msb_flag_tag : msb_flag -> positive **)

let msb_flag_tag = function
| MSB_CLEAR -> Coq_xH
| MSB_MERGE -> Coq_xO Coq_xH

(** val is_msb_flag_inhab : msb_flag -> is_msb_flag **)

let is_msb_flag_inhab = function
| MSB_CLEAR -> Coq_is_MSB_CLEAR
| MSB_MERGE -> Coq_is_MSB_MERGE

(** val is_msb_flag_functor : msb_flag -> is_msb_flag -> is_msb_flag **)

let rec is_msb_flag_functor _ x =
  x

type box_msb_flag_MSB_CLEAR =
| Box_msb_flag_MSB_CLEAR

type msb_flag_fields_t = __

(** val msb_flag_fields : msb_flag -> msb_flag_fields_t **)

let msb_flag_fields _ =
  Obj.magic Box_msb_flag_MSB_CLEAR

(** val msb_flag_construct :
    positive -> msb_flag_fields_t -> msb_flag option **)

let msb_flag_construct p _ =
  match p with
  | Coq_xI _ -> None
  | Coq_xO _ -> Some MSB_MERGE
  | Coq_xH -> Some MSB_CLEAR

(** val msb_flag_induction : 'a1 -> 'a1 -> msb_flag -> is_msb_flag -> 'a1 **)

let msb_flag_induction his_MSB_CLEAR his_MSB_MERGE _ = function
| Coq_is_MSB_CLEAR -> his_MSB_CLEAR
| Coq_is_MSB_MERGE -> his_MSB_MERGE

(** val msb_flag_eqb_fields :
    (msb_flag -> msb_flag -> bool) -> positive -> msb_flag_fields_t ->
    msb_flag_fields_t -> bool **)

let msb_flag_eqb_fields _ _ _ _ =
  true

(** val msb_flag_eqb : msb_flag -> msb_flag -> bool **)

let msb_flag_eqb x1 x2 =
  eqb_body msb_flag_tag msb_flag_fields
    (Obj.magic msb_flag_eqb_fields (fun _ _ -> true)) (msb_flag_tag x1)
    Box_msb_flag_MSB_CLEAR x2

(** val msb_flag_eqb_OK : msb_flag -> msb_flag -> reflect **)

let msb_flag_eqb_OK =
  iffP2 msb_flag_eqb

(** val msb_flag_eqb_OK_sumbool : msb_flag -> msb_flag -> bool **)

let msb_flag_eqb_OK_sumbool =
  reflect_dec msb_flag_eqb msb_flag_eqb_OK

(** val coq_HB_unnamed_factory_9 : msb_flag Coq_hasDecEq.axioms_ **)

let coq_HB_unnamed_factory_9 =
  { Coq_hasDecEq.eq_op = msb_flag_eqb; Coq_hasDecEq.eqP = msb_flag_eqb_OK }

(** val arch_decl_msb_flag__canonical__eqtype_Equality : Equality.coq_type **)

let arch_decl_msb_flag__canonical__eqtype_Equality =
  Obj.magic coq_HB_unnamed_factory_9

type ('reg, 'regx, 'xreg, 'rflag, 'cond) implicit_arg =
| IArflag of ('reg, 'regx, 'xreg, 'rflag, 'cond) rflag_t
| IAreg of ('reg, 'regx, 'xreg, 'rflag, 'cond) reg_t

(** val implicit_arg_beq :
    ('a1, 'a2, 'a3, 'a4, 'a5) arch_decl -> ('a1, 'a2, 'a3, 'a4, 'a5)
    implicit_arg -> ('a1, 'a2, 'a3, 'a4, 'a5) implicit_arg -> bool **)

let implicit_arg_beq arch i1 i2 =
  match i1 with
  | IArflag f1 ->
    (match i2 with
     | IArflag f2 ->
       eq_op (ceqT_eqType arch.toS_f._finC._eqC) (Obj.magic f1) (Obj.magic f2)
     | IAreg _ -> false)
  | IAreg r1 ->
    (match i2 with
     | IArflag _ -> false
     | IAreg r2 ->
       eq_op (ceqT_eqType arch.toS_r._finC._eqC) (Obj.magic r1) (Obj.magic r2))

(** val implicit_arg_eq_axiom :
    ('a1, 'a2, 'a3, 'a4, 'a5) arch_decl -> ('a1, 'a2, 'a3, 'a4, 'a5)
    implicit_arg eq_axiom **)

let implicit_arg_eq_axiom arch _top_assumption_ =
  let _evar_0_ = fun _r_ __top_assumption_ ->
    let _evar_0_ = fun _r1_ ->
      reflect_inj (ceqT_eqType arch.toS_f._finC._eqC) (fun x -> IArflag x)
        _r_ _r1_ (eqP (ceqT_eqType arch.toS_f._finC._eqC) _r_ _r1_)
    in
    let _evar_0_0 = fun _ -> ReflectF in
    (match __top_assumption_ with
     | IArflag r -> _evar_0_ r
     | IAreg r -> _evar_0_0 r)
  in
  let _evar_0_0 = fun _r_ __top_assumption_ ->
    let _evar_0_0 = fun _ -> ReflectF in
    let _evar_0_1 = fun _r1_ ->
      reflect_inj (ceqT_eqType arch.toS_r._finC._eqC) (fun x -> IAreg x) _r_
        _r1_ (eqP (ceqT_eqType arch.toS_r._finC._eqC) _r_ _r1_)
    in
    (match __top_assumption_ with
     | IArflag r -> _evar_0_0 r
     | IAreg r -> _evar_0_1 r)
  in
  (match _top_assumption_ with
   | IArflag r -> Obj.magic _evar_0_ r
   | IAreg r -> Obj.magic _evar_0_0 r)

(** val coq_HB_unnamed_factory_11 :
    ('a1, 'a2, 'a3, 'a4, 'a5) arch_decl -> ('a1, 'a2, 'a3, 'a4, 'a5)
    implicit_arg Coq_hasDecEq.axioms_ **)

let coq_HB_unnamed_factory_11 arch =
  { Coq_hasDecEq.eq_op = (implicit_arg_beq arch); Coq_hasDecEq.eqP =
    (implicit_arg_eq_axiom arch) }

(** val arch_decl_implicit_arg__canonical__eqtype_Equality :
    ('a1, 'a2, 'a3, 'a4, 'a5) arch_decl -> Equality.coq_type **)

let arch_decl_implicit_arg__canonical__eqtype_Equality arch =
  Obj.magic coq_HB_unnamed_factory_11 arch

type addr_kind =
| AK_compute
| AK_mem of aligned

type ('reg, 'regx, 'xreg, 'rflag, 'cond) arg_constrained_register =
| ACR_any
| ACR_exact of ('reg, 'regx, 'xreg, 'rflag, 'cond) reg_t
| ACR_vector of ('reg, 'regx, 'xreg, 'rflag, 'cond) xreg_t
| ACR_subset of ('reg, 'regx, 'xreg, 'rflag, 'cond) reg_t list

type ('reg, 'regx, 'xreg, 'rflag, 'cond) arg_desc =
| ADImplicit of ('reg, 'regx, 'xreg, 'rflag, 'cond) implicit_arg
| ADExplicit of addr_kind * nat
   * ('reg, 'regx, 'xreg, 'rflag, 'cond) arg_constrained_register

(** val coq_F :
    ('a1, 'a2, 'a3, 'a4, 'a5) arch_decl -> ('a1, 'a2, 'a3, 'a4, 'a5) rflag_t
    -> ('a1, 'a2, 'a3, 'a4, 'a5) arg_desc **)

let coq_F _ f =
  ADImplicit (IArflag f)

(** val coq_R :
    ('a1, 'a2, 'a3, 'a4, 'a5) arch_decl -> ('a1, 'a2, 'a3, 'a4, 'a5) reg_t ->
    ('a1, 'a2, 'a3, 'a4, 'a5) arg_desc **)

let coq_R _ r =
  ADImplicit (IAreg r)

(** val coq_Ea :
    ('a1, 'a2, 'a3, 'a4, 'a5) arch_decl -> nat -> ('a1, 'a2, 'a3, 'a4, 'a5)
    arg_desc **)

let coq_Ea _ n =
  ADExplicit ((AK_mem Aligned), n, ACR_any)

(** val coq_Eu :
    ('a1, 'a2, 'a3, 'a4, 'a5) arch_decl -> nat -> ('a1, 'a2, 'a3, 'a4, 'a5)
    arg_desc **)

let coq_Eu _ n =
  ADExplicit ((AK_mem Unaligned), n, ACR_any)

(** val coq_Ec :
    ('a1, 'a2, 'a3, 'a4, 'a5) arch_decl -> nat -> ('a1, 'a2, 'a3, 'a4, 'a5)
    arg_desc **)

let coq_Ec _ n =
  ADExplicit (AK_compute, n, ACR_any)

(** val coq_Ef :
    ('a1, 'a2, 'a3, 'a4, 'a5) arch_decl -> nat -> ('a1, 'a2, 'a3, 'a4, 'a5)
    reg_t -> ('a1, 'a2, 'a3, 'a4, 'a5) arg_desc **)

let coq_Ef _ n r =
  ADExplicit ((AK_mem Aligned), n, (ACR_exact r))

(** val check_oreg :
    ('a1, 'a2, 'a3, 'a4, 'a5) arch_decl -> ('a1, 'a2, 'a3, 'a4, 'a5)
    arg_constrained_register -> ('a1, 'a2, 'a3, 'a4, 'a5) asm_arg -> bool **)

let check_oreg arch or0 ai =
  match or0 with
  | ACR_any -> true
  | ACR_exact r ->
    (match ai with
     | Imm (_, _) -> true
     | Reg r' ->
       eq_op (ceqT_eqType arch.toS_r._finC._eqC) (Obj.magic r) (Obj.magic r')
     | _ -> false)
  | ACR_vector x ->
    (match ai with
     | XReg r ->
       eq_op (ceqT_eqType arch.toS_x._finC._eqC) (Obj.magic x) (Obj.magic r)
     | _ -> false)
  | ACR_subset s ->
    (match ai with
     | Reg r ->
       negb
         (in_mem (Obj.magic r)
           (mem (seq_predType (ceqT_eqType arch.toS_r._finC._eqC))
             (Obj.magic s)))
     | _ -> false)

type arg_kind =
| CAcond
| CAreg
| CAregx
| CAxmm
| CAmem of bool
| CAimm of caimm_checker_s * wsize

(** val arg_kind_rect :
    'a1 -> 'a1 -> 'a1 -> 'a1 -> (bool -> 'a1) -> (caimm_checker_s -> wsize ->
    'a1) -> arg_kind -> 'a1 **)

let arg_kind_rect f f0 f1 f2 f3 f4 = function
| CAcond -> f
| CAreg -> f0
| CAregx -> f1
| CAxmm -> f2
| CAmem b -> f3 b
| CAimm (c, w) -> f4 c w

(** val arg_kind_rec :
    'a1 -> 'a1 -> 'a1 -> 'a1 -> (bool -> 'a1) -> (caimm_checker_s -> wsize ->
    'a1) -> arg_kind -> 'a1 **)

let arg_kind_rec f f0 f1 f2 f3 f4 = function
| CAcond -> f
| CAreg -> f0
| CAregx -> f1
| CAxmm -> f2
| CAmem b -> f3 b
| CAimm (c, w) -> f4 c w

type is_arg_kind =
| Coq_is_CAcond
| Coq_is_CAreg
| Coq_is_CAregx
| Coq_is_CAxmm
| Coq_is_CAmem of bool * Param1.Coq_exports.is_bool
| Coq_is_CAimm of caimm_checker_s * is_caimm_checker_s * wsize * is_wsize

(** val is_arg_kind_rect :
    'a1 -> 'a1 -> 'a1 -> 'a1 -> (bool -> Param1.Coq_exports.is_bool -> 'a1)
    -> (caimm_checker_s -> is_caimm_checker_s -> wsize -> is_wsize -> 'a1) ->
    arg_kind -> is_arg_kind -> 'a1 **)

let is_arg_kind_rect f f0 f1 f2 f3 f4 _ = function
| Coq_is_CAcond -> f
| Coq_is_CAreg -> f0
| Coq_is_CAregx -> f1
| Coq_is_CAxmm -> f2
| Coq_is_CAmem (b, p_) -> f3 b p_
| Coq_is_CAimm (c, p_, w, p_0) -> f4 c p_ w p_0

(** val is_arg_kind_rec :
    'a1 -> 'a1 -> 'a1 -> 'a1 -> (bool -> Param1.Coq_exports.is_bool -> 'a1)
    -> (caimm_checker_s -> is_caimm_checker_s -> wsize -> is_wsize -> 'a1) ->
    arg_kind -> is_arg_kind -> 'a1 **)

let is_arg_kind_rec f f0 f1 f2 f3 f4 _ = function
| Coq_is_CAcond -> f
| Coq_is_CAreg -> f0
| Coq_is_CAregx -> f1
| Coq_is_CAxmm -> f2
| Coq_is_CAmem (b, p_) -> f3 b p_
| Coq_is_CAimm (c, p_, w, p_0) -> f4 c p_ w p_0

(** val arg_kind_tag : arg_kind -> positive **)

let arg_kind_tag = function
| CAcond -> Coq_xH
| CAreg -> Coq_xO Coq_xH
| CAregx -> Coq_xI Coq_xH
| CAxmm -> Coq_xO (Coq_xO Coq_xH)
| CAmem _ -> Coq_xI (Coq_xO Coq_xH)
| CAimm (_, _) -> Coq_xO (Coq_xI Coq_xH)

(** val is_arg_kind_inhab : arg_kind -> is_arg_kind **)

let is_arg_kind_inhab = function
| CAcond -> Coq_is_CAcond
| CAreg -> Coq_is_CAreg
| CAregx -> Coq_is_CAregx
| CAxmm -> Coq_is_CAxmm
| CAmem h -> Coq_is_CAmem (h, (Coq_exports.is_bool_inhab h))
| CAimm (h, h0) ->
  Coq_is_CAimm (h, (is_caimm_checker_s_inhab h), h0, (is_wsize_inhab h0))

(** val is_arg_kind_functor : arg_kind -> is_arg_kind -> is_arg_kind **)

let rec is_arg_kind_functor _ x =
  x

type box_arg_kind_CAcond =
| Box_arg_kind_CAcond

type box_arg_kind_CAmem =
  bool
  (* singleton inductive, whose constructor was Box_arg_kind_CAmem *)

(** val coq_Box_arg_kind_CAmem_0 : box_arg_kind_CAmem -> bool **)

let coq_Box_arg_kind_CAmem_0 record =
  record

type box_arg_kind_CAimm = { coq_Box_arg_kind_CAimm_0 : caimm_checker_s;
                            coq_Box_arg_kind_CAimm_1 : wsize }

(** val coq_Box_arg_kind_CAimm_0 : box_arg_kind_CAimm -> caimm_checker_s **)

let coq_Box_arg_kind_CAimm_0 record =
  record.coq_Box_arg_kind_CAimm_0

(** val coq_Box_arg_kind_CAimm_1 : box_arg_kind_CAimm -> wsize **)

let coq_Box_arg_kind_CAimm_1 record =
  record.coq_Box_arg_kind_CAimm_1

type arg_kind_fields_t = __

(** val arg_kind_fields : arg_kind -> arg_kind_fields_t **)

let arg_kind_fields = function
| CAmem h -> Obj.magic h
| CAimm (h, h0) ->
  Obj.magic { coq_Box_arg_kind_CAimm_0 = h; coq_Box_arg_kind_CAimm_1 = h0 }
| _ -> Obj.magic Box_arg_kind_CAcond

(** val arg_kind_construct :
    positive -> arg_kind_fields_t -> arg_kind option **)

let arg_kind_construct p x =
  match p with
  | Coq_xI x0 ->
    (match x0 with
     | Coq_xI _ -> None
     | Coq_xO _ -> Some (CAmem (Obj.magic x))
     | Coq_xH -> Some CAregx)
  | Coq_xO x0 ->
    (match x0 with
     | Coq_xI _ ->
       let { coq_Box_arg_kind_CAimm_0 = box_arg_kind_CAimm_0;
         coq_Box_arg_kind_CAimm_1 = box_arg_kind_CAimm_1 } = Obj.magic x
       in
       Some (CAimm (box_arg_kind_CAimm_0, box_arg_kind_CAimm_1))
     | Coq_xO _ -> Some CAxmm
     | Coq_xH -> Some CAreg)
  | Coq_xH -> Some CAcond

(** val arg_kind_induction :
    'a1 -> 'a1 -> 'a1 -> 'a1 -> (bool -> Param1.Coq_exports.is_bool -> 'a1)
    -> (caimm_checker_s -> is_caimm_checker_s -> wsize -> is_wsize -> 'a1) ->
    arg_kind -> is_arg_kind -> 'a1 **)

let arg_kind_induction his_CAcond his_CAreg his_CAregx his_CAxmm his_CAmem his_CAimm _ = function
| Coq_is_CAcond -> his_CAcond
| Coq_is_CAreg -> his_CAreg
| Coq_is_CAregx -> his_CAregx
| Coq_is_CAxmm -> his_CAxmm
| Coq_is_CAmem (x0, p_) -> his_CAmem x0 p_
| Coq_is_CAimm (x0, p_, x1, p_0) -> his_CAimm x0 p_ x1 p_0

(** val arg_kind_eqb_fields :
    (arg_kind -> arg_kind -> bool) -> positive -> arg_kind_fields_t ->
    arg_kind_fields_t -> bool **)

let arg_kind_eqb_fields _ x x0 x1 =
  match x with
  | Coq_xI x2 ->
    (match x2 with
     | Coq_xO _ ->
       (&&) (Std.Prelude.bool_eqb (Obj.magic x0) (Obj.magic x1)) true
     | _ -> true)
  | Coq_xO x2 ->
    (match x2 with
     | Coq_xI _ ->
       let { coq_Box_arg_kind_CAimm_0 = box_arg_kind_CAimm_0;
         coq_Box_arg_kind_CAimm_1 = box_arg_kind_CAimm_1 } = Obj.magic x0
       in
       let { coq_Box_arg_kind_CAimm_0 = box_arg_kind_CAimm_2;
         coq_Box_arg_kind_CAimm_1 = box_arg_kind_CAimm_3 } = Obj.magic x1
       in
       (&&) (caimm_checker_s_eqb box_arg_kind_CAimm_0 box_arg_kind_CAimm_2)
         ((&&) (wsize_eqb box_arg_kind_CAimm_1 box_arg_kind_CAimm_3) true)
     | _ -> true)
  | Coq_xH -> true

(** val arg_kind_eqb : arg_kind -> arg_kind -> bool **)

let arg_kind_eqb x1 x2 =
  match x1 with
  | CAmem h ->
    eqb_body arg_kind_tag arg_kind_fields
      (Obj.magic arg_kind_eqb_fields (fun _ _ -> true))
      (arg_kind_tag (CAmem h)) h x2
  | CAimm (h, h0) ->
    eqb_body arg_kind_tag arg_kind_fields
      (Obj.magic arg_kind_eqb_fields (fun _ _ -> true))
      (arg_kind_tag (CAimm (h, h0))) { coq_Box_arg_kind_CAimm_0 = h;
      coq_Box_arg_kind_CAimm_1 = h0 } x2
  | x ->
    eqb_body arg_kind_tag arg_kind_fields
      (Obj.magic arg_kind_eqb_fields (fun _ _ -> true)) (arg_kind_tag x)
      Box_arg_kind_CAcond x2

(** val arg_kind_eqb_OK : arg_kind -> arg_kind -> reflect **)

let arg_kind_eqb_OK =
  iffP2 arg_kind_eqb

(** val arg_kind_eqb_OK_sumbool : arg_kind -> arg_kind -> bool **)

let arg_kind_eqb_OK_sumbool =
  reflect_dec arg_kind_eqb arg_kind_eqb_OK

(** val coq_HB_unnamed_factory_13 : arg_kind Coq_hasDecEq.axioms_ **)

let coq_HB_unnamed_factory_13 =
  { Coq_hasDecEq.eq_op = arg_kind_eqb; Coq_hasDecEq.eqP = arg_kind_eqb_OK }

(** val arch_decl_arg_kind__canonical__eqtype_Equality : Equality.coq_type **)

let arch_decl_arg_kind__canonical__eqtype_Equality =
  Obj.magic coq_HB_unnamed_factory_13

type arg_kinds = arg_kind list

type args_kinds = arg_kinds list

type i_args_kinds = args_kinds list

(** val check_arg_kind :
    ('a1, 'a2, 'a3, 'a4, 'a5) arch_decl -> ('a1, 'a2, 'a3, 'a4, 'a5) asm_arg
    -> arg_kind -> bool **)

let check_arg_kind arch a cond =
  match a with
  | Condt _ -> (match cond with
                | CAcond -> true
                | _ -> false)
  | Imm (sz, z) ->
    (match cond with
     | CAimm (checker, sz') ->
       (&&)
         (eq_op wsize_wsize__canonical__eqtype_Equality (Obj.magic sz)
           (Obj.magic sz')) (arch.check_CAimm checker sz z)
     | _ -> false)
  | Reg _ -> (match cond with
              | CAreg -> true
              | _ -> false)
  | Regx _ -> (match cond with
               | CAregx -> true
               | _ -> false)
  | Addr _ -> (match cond with
               | CAmem _ -> true
               | _ -> false)
  | XReg _ -> (match cond with
               | CAxmm -> true
               | _ -> false)

(** val check_arg_kinds :
    ('a1, 'a2, 'a3, 'a4, 'a5) arch_decl -> ('a1, 'a2, 'a3, 'a4, 'a5) asm_arg
    -> arg_kinds -> bool **)

let check_arg_kinds arch a cond =
  has (check_arg_kind arch a) cond

(** val check_args_kinds :
    ('a1, 'a2, 'a3, 'a4, 'a5) arch_decl -> ('a1, 'a2, 'a3, 'a4, 'a5) asm_args
    -> args_kinds -> bool **)

let check_args_kinds arch a cond =
  all2 (check_arg_kinds arch) a cond

(** val check_i_args_kinds :
    ('a1, 'a2, 'a3, 'a4, 'a5) arch_decl -> i_args_kinds -> ('a1, 'a2, 'a3,
    'a4, 'a5) asm_args -> bool **)

let check_i_args_kinds arch cond a =
  has (check_args_kinds arch a) cond

(** val check_arg_dest :
    ('a1, 'a2, 'a3, 'a4, 'a5) arch_decl -> ('a1, 'a2, 'a3, 'a4, 'a5) arg_desc
    -> ltype -> bool **)

let check_arg_dest _ ad ty =
  match ad with
  | ADImplicit _ -> true
  | ADExplicit (_, _, _) ->
    negb
      (eq_op type_ltype__canonical__eqtype_Equality (Obj.magic ty)
        (Obj.magic Coq_lbool))

type ('reg, 'regx, 'xreg, 'rflag, 'cond) pp_asm_op_ext =
| PP_error
| PP_name
| PP_iname of wsize
| PP_iname2 of string * wsize * wsize
| PP_viname of velem * bool
| PP_viname2 of velem * velem
| PP_ct of ('reg, 'regx, 'xreg, 'rflag, 'cond) asm_arg

type ('reg, 'regx, 'xreg, 'rflag, 'cond) pp_asm_op = { pp_aop_name : 
                                                       string;
                                                       pp_aop_ext : ('reg,
                                                                    'regx,
                                                                    'xreg,
                                                                    'rflag,
                                                                    'cond)
                                                                    pp_asm_op_ext;
                                                       pp_aop_args : 
                                                       (wsize * ('reg, 'regx,
                                                       'xreg, 'rflag, 'cond)
                                                       asm_arg) list }

(** val pp_aop_name :
    ('a1, 'a2, 'a3, 'a4, 'a5) arch_decl -> ('a1, 'a2, 'a3, 'a4, 'a5)
    pp_asm_op -> string **)

let pp_aop_name _ p =
  p.pp_aop_name

(** val pp_aop_ext :
    ('a1, 'a2, 'a3, 'a4, 'a5) arch_decl -> ('a1, 'a2, 'a3, 'a4, 'a5)
    pp_asm_op -> ('a1, 'a2, 'a3, 'a4, 'a5) pp_asm_op_ext **)

let pp_aop_ext _ p =
  p.pp_aop_ext

(** val pp_aop_args :
    ('a1, 'a2, 'a3, 'a4, 'a5) arch_decl -> ('a1, 'a2, 'a3, 'a4, 'a5)
    pp_asm_op -> (wsize * ('a1, 'a2, 'a3, 'a4, 'a5) asm_arg) list **)

let pp_aop_args _ p =
  p.pp_aop_args

type ('reg, 'regx, 'xreg, 'rflag, 'cond) instr_desc_t = { id_valid : 
                                                          bool;
                                                          id_msb_flag : 
                                                          msb_flag;
                                                          id_tin : ltype list;
                                                          id_in : ('reg,
                                                                  'regx,
                                                                  'xreg,
                                                                  'rflag,
                                                                  'cond)
                                                                  arg_desc
                                                                  list;
                                                          id_tout : ltype list;
                                                          id_out : ('reg,
                                                                   'regx,
                                                                   'xreg,
                                                                   'rflag,
                                                                   'cond)
                                                                   arg_desc
                                                                   list;
                                                          id_semi : sem_tuple
                                                                    exec
                                                                    sem_prod;
                                                          id_args_kinds : 
                                                          i_args_kinds;
                                                          id_nargs : 
                                                          nat;
                                                          id_str_jas : 
                                                          (unit -> string);
                                                          id_safe : safe_cond
                                                                    list;
                                                          id_pp_asm : 
                                                          (('reg, 'regx,
                                                          'xreg, 'rflag,
                                                          'cond) asm_args ->
                                                          ('reg, 'regx,
                                                          'xreg, 'rflag,
                                                          'cond) pp_asm_op) }

(** val id_valid :
    ('a1, 'a2, 'a3, 'a4, 'a5) arch_decl -> ('a1, 'a2, 'a3, 'a4, 'a5)
    instr_desc_t -> bool **)

let id_valid _ i =
  i.id_valid

(** val id_msb_flag :
    ('a1, 'a2, 'a3, 'a4, 'a5) arch_decl -> ('a1, 'a2, 'a3, 'a4, 'a5)
    instr_desc_t -> msb_flag **)

let id_msb_flag _ i =
  i.id_msb_flag

(** val id_tin :
    ('a1, 'a2, 'a3, 'a4, 'a5) arch_decl -> ('a1, 'a2, 'a3, 'a4, 'a5)
    instr_desc_t -> ltype list **)

let id_tin _ i =
  i.id_tin

(** val id_in :
    ('a1, 'a2, 'a3, 'a4, 'a5) arch_decl -> ('a1, 'a2, 'a3, 'a4, 'a5)
    instr_desc_t -> ('a1, 'a2, 'a3, 'a4, 'a5) arg_desc list **)

let id_in _ i =
  i.id_in

(** val id_tout :
    ('a1, 'a2, 'a3, 'a4, 'a5) arch_decl -> ('a1, 'a2, 'a3, 'a4, 'a5)
    instr_desc_t -> ltype list **)

let id_tout _ i =
  i.id_tout

(** val id_out :
    ('a1, 'a2, 'a3, 'a4, 'a5) arch_decl -> ('a1, 'a2, 'a3, 'a4, 'a5)
    instr_desc_t -> ('a1, 'a2, 'a3, 'a4, 'a5) arg_desc list **)

let id_out _ i =
  i.id_out

(** val id_semi :
    ('a1, 'a2, 'a3, 'a4, 'a5) arch_decl -> ('a1, 'a2, 'a3, 'a4, 'a5)
    instr_desc_t -> sem_tuple exec sem_prod **)

let id_semi _ i =
  i.id_semi

(** val id_args_kinds :
    ('a1, 'a2, 'a3, 'a4, 'a5) arch_decl -> ('a1, 'a2, 'a3, 'a4, 'a5)
    instr_desc_t -> i_args_kinds **)

let id_args_kinds _ i =
  i.id_args_kinds

(** val id_nargs :
    ('a1, 'a2, 'a3, 'a4, 'a5) arch_decl -> ('a1, 'a2, 'a3, 'a4, 'a5)
    instr_desc_t -> nat **)

let id_nargs _ i =
  i.id_nargs

(** val id_str_jas :
    ('a1, 'a2, 'a3, 'a4, 'a5) arch_decl -> ('a1, 'a2, 'a3, 'a4, 'a5)
    instr_desc_t -> unit -> string **)

let id_str_jas _ i =
  i.id_str_jas

(** val id_safe :
    ('a1, 'a2, 'a3, 'a4, 'a5) arch_decl -> ('a1, 'a2, 'a3, 'a4, 'a5)
    instr_desc_t -> safe_cond list **)

let id_safe _ i =
  i.id_safe

(** val id_pp_asm :
    ('a1, 'a2, 'a3, 'a4, 'a5) arch_decl -> ('a1, 'a2, 'a3, 'a4, 'a5)
    instr_desc_t -> ('a1, 'a2, 'a3, 'a4, 'a5) asm_args -> ('a1, 'a2, 'a3,
    'a4, 'a5) pp_asm_op **)

let id_pp_asm _ i =
  i.id_pp_asm

type ('reg, 'regx, 'xreg, 'rflag, 'cond, 'asm_op) asm_op_decl = { _eqT : 
                                                                  'asm_op
                                                                  eqTypeC;
                                                                  instr_desc_op : 
                                                                  ('asm_op ->
                                                                  ('reg,
                                                                  'regx,
                                                                  'xreg,
                                                                  'rflag,
                                                                  'cond)
                                                                  instr_desc_t);
                                                                  prim_string : 
                                                                  (string * 'asm_op
                                                                  prim_constructor)
                                                                  list }

(** val _eqT :
    ('a1, 'a2, 'a3, 'a4, 'a5) arch_decl -> ('a1, 'a2, 'a3, 'a4, 'a5, 'a6)
    asm_op_decl -> 'a6 eqTypeC **)

let _eqT _ asm_op_decl0 =
  asm_op_decl0._eqT

(** val instr_desc_op :
    ('a1, 'a2, 'a3, 'a4, 'a5) arch_decl -> ('a1, 'a2, 'a3, 'a4, 'a5, 'a6)
    asm_op_decl -> 'a6 -> ('a1, 'a2, 'a3, 'a4, 'a5) instr_desc_t **)

let instr_desc_op _ asm_op_decl0 =
  asm_op_decl0.instr_desc_op

(** val prim_string :
    ('a1, 'a2, 'a3, 'a4, 'a5) arch_decl -> ('a1, 'a2, 'a3, 'a4, 'a5, 'a6)
    asm_op_decl -> (string * 'a6 prim_constructor) list **)

let prim_string _ asm_op_decl0 =
  asm_op_decl0.prim_string

type ('reg, 'regx, 'xreg, 'rflag, 'cond, 'asm_op) asm_op_t' = 'asm_op

type ('reg, 'regx, 'xreg, 'rflag, 'cond, 'asm_op) asm_op_msb_t =
  wsize option * 'asm_op

(** val extend_size : wsize -> ltype -> ltype **)

let extend_size ws t = match t with
| Coq_lbool -> t
| Coq_lword ws' ->
  if cmp_le wsize_cmp ws' ws then Coq_lword ws else Coq_lword ws'

(** val wextend_size : wsize -> ltype -> sem_ot -> sem_ot **)

let wextend_size ws t x =
  match t with
  | Coq_lbool -> x
  | Coq_lword ws' ->
    if cmp_le wsize_cmp ws' ws then zero_extend ws ws' x else x

(** val extend_tuple : wsize -> ltype list -> sem_tuple -> sem_tuple **)

let rec extend_tuple ws id_tout0 t =
  match id_tout0 with
  | [] -> Obj.magic ()
  | t0 :: ts ->
    let rec_ = extend_tuple ws ts in
    (match ts with
     | [] -> wextend_size ws t0 t
     | _ :: _ ->
       Obj.magic ((wextend_size ws t0 (fst (Obj.magic t))),
         (rec_ (snd (Obj.magic t)))))

(** val apply_lprod : ('a1 -> 'a2) -> __ list -> 'a1 lprod -> 'a2 lprod **)

let rec apply_lprod f ts a =
  match ts with
  | [] -> Obj.magic f a
  | _ :: ts' -> Obj.magic (fun x -> apply_lprod f ts' (Obj.magic a x))

(** val is_not_CAmem : arg_kind -> bool **)

let is_not_CAmem = function
| CAmem _ -> false
| _ -> true

(** val exclude_mem_args_kinds :
    ('a1, 'a2, 'a3, 'a4, 'a5) arch_decl -> ('a1, 'a2, 'a3, 'a4, 'a5) arg_desc
    -> args_kinds -> args_kinds **)

let exclude_mem_args_kinds _ d cond =
  match d with
  | ADImplicit _ -> cond
  | ADExplicit (_, i, _) ->
    mapi (fun k c ->
      if eq_op coq_Datatypes_nat__canonical__eqtype_Equality (Obj.magic k)
           (Obj.magic i)
      then filter is_not_CAmem c
      else c) cond

(** val exclude_mem_i_args_kinds :
    ('a1, 'a2, 'a3, 'a4, 'a5) arch_decl -> ('a1, 'a2, 'a3, 'a4, 'a5) arg_desc
    -> i_args_kinds -> i_args_kinds **)

let exclude_mem_i_args_kinds arch d cond =
  map (exclude_mem_args_kinds arch d) cond

(** val exclude_mem_aux :
    ('a1, 'a2, 'a3, 'a4, 'a5) arch_decl -> i_args_kinds -> ('a1, 'a2, 'a3,
    'a4, 'a5) arg_desc list -> i_args_kinds **)

let exclude_mem_aux arch cond d =
  foldl (fun cond0 d0 -> exclude_mem_i_args_kinds arch d0 cond0) cond d

(** val is_nil : 'a1 list -> bool **)

let is_nil = function
| [] -> true
| _ :: _ -> false

(** val exclude_mem :
    ('a1, 'a2, 'a3, 'a4, 'a5) arch_decl -> i_args_kinds -> ('a1, 'a2, 'a3,
    'a4, 'a5) arg_desc list -> i_args_kinds **)

let exclude_mem arch cond d =
  filter (fun c -> negb (has is_nil c)) (exclude_mem_aux arch cond d)

(** val extend_sem :
    ltype list -> ltype list -> wsize -> sem_tuple exec sem_prod -> sem_tuple
    exec sem_prod **)

let extend_sem tin tout ws semi =
  apply_lprod (Result.map (extend_tuple ws tout))
    (map (Obj.magic __) (map eval_ltype tin)) semi

(** val instr_desc :
    ('a1, 'a2, 'a3, 'a4, 'a5) arch_decl -> ('a1, 'a2, 'a3, 'a4, 'a5, 'a6)
    asm_op_decl -> ('a1, 'a2, 'a3, 'a4, 'a5, 'a6) asm_op_msb_t -> ('a1, 'a2,
    'a3, 'a4, 'a5) instr_desc_t **)

let instr_desc arch asm_op_d = function
| (ws, o0) ->
  let d = asm_op_d.instr_desc_op o0 in
  (match ws with
   | Some ws0 ->
     if eq_op arch_decl_msb_flag__canonical__eqtype_Equality
          (Obj.magic d.id_msb_flag) (Obj.magic MSB_CLEAR)
     then { id_valid = d.id_valid; id_msb_flag = d.id_msb_flag; id_tin =
            d.id_tin; id_in = d.id_in; id_tout =
            (map (extend_size ws0) d.id_tout); id_out = d.id_out; id_semi =
            (extend_sem d.id_tin d.id_tout ws0 d.id_semi); id_args_kinds =
            (exclude_mem arch d.id_args_kinds d.id_out); id_nargs =
            d.id_nargs; id_str_jas = d.id_str_jas; id_safe = d.id_safe;
            id_pp_asm = d.id_pp_asm }
     else d
   | None -> d)

type ('reg, 'regx, 'xreg, 'rflag, 'cond, 'asm_op) asm_i_r =
| ALIGN
| LABEL of label_kind * label
| STORELABEL of ('reg, 'regx, 'xreg, 'rflag, 'cond) reg_t * label
| JMP of remote_label
| JMPI of ('reg, 'regx, 'xreg, 'rflag, 'cond) asm_arg
| Jcc of label * ('reg, 'regx, 'xreg, 'rflag, 'cond) cond_t
| JAL of ('reg, 'regx, 'xreg, 'rflag, 'cond) reg_t * remote_label
| CALL of remote_label
| POPPC
| AsmOp of ('reg, 'regx, 'xreg, 'rflag, 'cond, 'asm_op) asm_op_t'
   * ('reg, 'regx, 'xreg, 'rflag, 'cond) asm_args
| SysCall of (Wsize.wsize * BinNums.positive) Syscall_t.syscall_t

type ('reg, 'regx, 'xreg, 'rflag, 'cond, 'asm_op) asm_i = { asmi_ii : 
                                                            instr_info;
                                                            asmi_i : 
                                                            ('reg, 'regx,
                                                            'xreg, 'rflag,
                                                            'cond, 'asm_op)
                                                            asm_i_r }

(** val asmi_ii :
    ('a1, 'a2, 'a3, 'a4, 'a5) arch_decl -> ('a1, 'a2, 'a3, 'a4, 'a5, 'a6)
    asm_op_decl -> ('a1, 'a2, 'a3, 'a4, 'a5, 'a6) asm_i -> instr_info **)

let asmi_ii _ _ a =
  a.asmi_ii

(** val asmi_i :
    ('a1, 'a2, 'a3, 'a4, 'a5) arch_decl -> ('a1, 'a2, 'a3, 'a4, 'a5, 'a6)
    asm_op_decl -> ('a1, 'a2, 'a3, 'a4, 'a5, 'a6) asm_i -> ('a1, 'a2, 'a3,
    'a4, 'a5, 'a6) asm_i_r **)

let asmi_i _ _ a =
  a.asmi_i

type ('reg, 'regx, 'xreg, 'rflag, 'cond, 'asm_op) asm_code =
  ('reg, 'regx, 'xreg, 'rflag, 'cond, 'asm_op) asm_i list

type ('reg, 'regx, 'xreg, 'rflag, 'cond) asm_typed_reg =
| ARReg of ('reg, 'regx, 'xreg, 'rflag, 'cond) reg_t
| ARegX of ('reg, 'regx, 'xreg, 'rflag, 'cond) regx_t
| AXReg of ('reg, 'regx, 'xreg, 'rflag, 'cond) xreg_t
| ABReg of ('reg, 'regx, 'xreg, 'rflag, 'cond) rflag_t

(** val asm_typed_reg_beq :
    ('a1, 'a2, 'a3, 'a4, 'a5) arch_decl -> ('a1, 'a2, 'a3, 'a4, 'a5)
    asm_typed_reg -> ('a1, 'a2, 'a3, 'a4, 'a5) asm_typed_reg -> bool **)

let asm_typed_reg_beq arch r1 r2 =
  match r1 with
  | ARReg r3 ->
    (match r2 with
     | ARReg r4 ->
       eq_op (ceqT_eqType arch.toS_r._finC._eqC) (Obj.magic r3) (Obj.magic r4)
     | _ -> false)
  | ARegX r3 ->
    (match r2 with
     | ARegX r4 ->
       eq_op (ceqT_eqType arch.toS_rx._finC._eqC) (Obj.magic r3)
         (Obj.magic r4)
     | _ -> false)
  | AXReg r3 ->
    (match r2 with
     | AXReg r4 ->
       eq_op (ceqT_eqType arch.toS_x._finC._eqC) (Obj.magic r3) (Obj.magic r4)
     | _ -> false)
  | ABReg r3 ->
    (match r2 with
     | ABReg r4 ->
       eq_op (ceqT_eqType arch.toS_f._finC._eqC) (Obj.magic r3) (Obj.magic r4)
     | _ -> false)

(** val asm_typed_reg_eq_axiom :
    ('a1, 'a2, 'a3, 'a4, 'a5) arch_decl -> ('a1, 'a2, 'a3, 'a4, 'a5)
    asm_typed_reg eq_axiom **)

let asm_typed_reg_eq_axiom arch _top_assumption_ =
  let _evar_0_ = fun r1 __top_assumption_ ->
    let _evar_0_ = fun r2 ->
      reflect_inj (ceqT_eqType arch.toS_r._finC._eqC) (fun x -> ARReg x) r1
        r2 (eqP (ceqT_eqType arch.toS_r._finC._eqC) r1 r2)
    in
    let _evar_0_0 = fun _ -> ReflectF in
    let _evar_0_1 = fun _ -> ReflectF in
    let _evar_0_2 = fun _ -> ReflectF in
    (match __top_assumption_ with
     | ARReg r -> _evar_0_ r
     | ARegX r -> _evar_0_0 r
     | AXReg x -> _evar_0_1 x
     | ABReg r -> _evar_0_2 r)
  in
  let _evar_0_0 = fun r1 __top_assumption_ ->
    let _evar_0_0 = fun _ -> ReflectF in
    let _evar_0_1 = fun r2 ->
      reflect_inj (ceqT_eqType arch.toS_rx._finC._eqC) (fun x -> ARegX x) r1
        r2 (eqP (ceqT_eqType arch.toS_rx._finC._eqC) r1 r2)
    in
    let _evar_0_2 = fun _ -> ReflectF in
    let _evar_0_3 = fun _ -> ReflectF in
    (match __top_assumption_ with
     | ARReg r -> _evar_0_0 r
     | ARegX r -> _evar_0_1 r
     | AXReg x -> _evar_0_2 x
     | ABReg r -> _evar_0_3 r)
  in
  let _evar_0_1 = fun r1 __top_assumption_ ->
    let _evar_0_1 = fun _ -> ReflectF in
    let _evar_0_2 = fun _ -> ReflectF in
    let _evar_0_3 = fun r2 ->
      reflect_inj (ceqT_eqType arch.toS_x._finC._eqC) (fun x -> AXReg x) r1
        r2 (eqP (ceqT_eqType arch.toS_x._finC._eqC) r1 r2)
    in
    let _evar_0_4 = fun _ -> ReflectF in
    (match __top_assumption_ with
     | ARReg r -> _evar_0_1 r
     | ARegX r -> _evar_0_2 r
     | AXReg x -> _evar_0_3 x
     | ABReg r -> _evar_0_4 r)
  in
  let _evar_0_2 = fun r1 __top_assumption_ ->
    let _evar_0_2 = fun _ -> ReflectF in
    let _evar_0_3 = fun _ -> ReflectF in
    let _evar_0_4 = fun _ -> ReflectF in
    let _evar_0_5 = fun r2 ->
      reflect_inj (ceqT_eqType arch.toS_f._finC._eqC) (fun x -> ABReg x) r1
        r2 (eqP (ceqT_eqType arch.toS_f._finC._eqC) r1 r2)
    in
    (match __top_assumption_ with
     | ARReg r -> _evar_0_2 r
     | ARegX r -> _evar_0_3 r
     | AXReg x -> _evar_0_4 x
     | ABReg r -> _evar_0_5 r)
  in
  (match _top_assumption_ with
   | ARReg r -> Obj.magic _evar_0_ r
   | ARegX r -> Obj.magic _evar_0_0 r
   | AXReg x -> Obj.magic _evar_0_1 x
   | ABReg r -> Obj.magic _evar_0_2 r)

(** val coq_HB_unnamed_factory_15 :
    ('a1, 'a2, 'a3, 'a4, 'a5) arch_decl -> ('a1, 'a2, 'a3, 'a4, 'a5)
    asm_typed_reg Coq_hasDecEq.axioms_ **)

let coq_HB_unnamed_factory_15 arch =
  { Coq_hasDecEq.eq_op = (asm_typed_reg_beq arch); Coq_hasDecEq.eqP =
    (asm_typed_reg_eq_axiom arch) }

(** val arch_decl_asm_typed_reg__canonical__eqtype_Equality :
    ('a1, 'a2, 'a3, 'a4, 'a5) arch_decl -> Equality.coq_type **)

let arch_decl_asm_typed_reg__canonical__eqtype_Equality arch =
  Obj.magic coq_HB_unnamed_factory_15 arch

type ('reg, 'regx, 'xreg, 'rflag, 'cond, 'asm_op) asm_fundef = { asm_fd_align : 
                                                                 wsize;
                                                                 asm_fd_arg : 
                                                                 ('reg,
                                                                 'regx,
                                                                 'xreg,
                                                                 'rflag,
                                                                 'cond)
                                                                 asm_typed_reg
                                                                 list;
                                                                 asm_fd_body : 
                                                                 ('reg,
                                                                 'regx,
                                                                 'xreg,
                                                                 'rflag,
                                                                 'cond,
                                                                 'asm_op)
                                                                 asm_code;
                                                                 asm_fd_res : 
                                                                 ('reg,
                                                                 'regx,
                                                                 'xreg,
                                                                 'rflag,
                                                                 'cond)
                                                                 asm_typed_reg
                                                                 list;
                                                                 asm_fd_export : 
                                                                 bool;
                                                                 asm_fd_total_stack : 
                                                                 coq_Z;
                                                                 asm_fd_align_args : 
                                                                 wsize list }

(** val asm_fd_align :
    ('a1, 'a2, 'a3, 'a4, 'a5) arch_decl -> ('a1, 'a2, 'a3, 'a4, 'a5, 'a6)
    asm_op_decl -> ('a1, 'a2, 'a3, 'a4, 'a5, 'a6) asm_fundef -> wsize **)

let asm_fd_align _ _ a =
  a.asm_fd_align

(** val asm_fd_arg :
    ('a1, 'a2, 'a3, 'a4, 'a5) arch_decl -> ('a1, 'a2, 'a3, 'a4, 'a5, 'a6)
    asm_op_decl -> ('a1, 'a2, 'a3, 'a4, 'a5, 'a6) asm_fundef -> ('a1, 'a2,
    'a3, 'a4, 'a5) asm_typed_reg list **)

let asm_fd_arg _ _ a =
  a.asm_fd_arg

(** val asm_fd_body :
    ('a1, 'a2, 'a3, 'a4, 'a5) arch_decl -> ('a1, 'a2, 'a3, 'a4, 'a5, 'a6)
    asm_op_decl -> ('a1, 'a2, 'a3, 'a4, 'a5, 'a6) asm_fundef -> ('a1, 'a2,
    'a3, 'a4, 'a5, 'a6) asm_code **)

let asm_fd_body _ _ a =
  a.asm_fd_body

(** val asm_fd_res :
    ('a1, 'a2, 'a3, 'a4, 'a5) arch_decl -> ('a1, 'a2, 'a3, 'a4, 'a5, 'a6)
    asm_op_decl -> ('a1, 'a2, 'a3, 'a4, 'a5, 'a6) asm_fundef -> ('a1, 'a2,
    'a3, 'a4, 'a5) asm_typed_reg list **)

let asm_fd_res _ _ a =
  a.asm_fd_res

(** val asm_fd_export :
    ('a1, 'a2, 'a3, 'a4, 'a5) arch_decl -> ('a1, 'a2, 'a3, 'a4, 'a5, 'a6)
    asm_op_decl -> ('a1, 'a2, 'a3, 'a4, 'a5, 'a6) asm_fundef -> bool **)

let asm_fd_export _ _ a =
  a.asm_fd_export

(** val asm_fd_total_stack :
    ('a1, 'a2, 'a3, 'a4, 'a5) arch_decl -> ('a1, 'a2, 'a3, 'a4, 'a5, 'a6)
    asm_op_decl -> ('a1, 'a2, 'a3, 'a4, 'a5, 'a6) asm_fundef -> coq_Z **)

let asm_fd_total_stack _ _ a =
  a.asm_fd_total_stack

(** val asm_fd_align_args :
    ('a1, 'a2, 'a3, 'a4, 'a5) arch_decl -> ('a1, 'a2, 'a3, 'a4, 'a5, 'a6)
    asm_op_decl -> ('a1, 'a2, 'a3, 'a4, 'a5, 'a6) asm_fundef -> wsize list **)

let asm_fd_align_args _ _ a =
  a.asm_fd_align_args

type ('reg, 'regx, 'xreg, 'rflag, 'cond, 'asm_op) asm_prog = { asm_globs : 
                                                               GRing.ComRing.sort
                                                               list;
                                                               asm_glob_names : 
                                                               ((Var.var * wsize) * coq_Z)
                                                               list;
                                                               asm_funcs : 
                                                               (funname * ('reg,
                                                               'regx, 'xreg,
                                                               'rflag, 'cond,
                                                               'asm_op)
                                                               asm_fundef)
                                                               list }

(** val asm_globs :
    ('a1, 'a2, 'a3, 'a4, 'a5) arch_decl -> ('a1, 'a2, 'a3, 'a4, 'a5, 'a6)
    asm_op_decl -> ('a1, 'a2, 'a3, 'a4, 'a5, 'a6) asm_prog ->
    GRing.ComRing.sort list **)

let asm_globs _ _ a =
  a.asm_globs

(** val asm_glob_names :
    ('a1, 'a2, 'a3, 'a4, 'a5) arch_decl -> ('a1, 'a2, 'a3, 'a4, 'a5, 'a6)
    asm_op_decl -> ('a1, 'a2, 'a3, 'a4, 'a5, 'a6) asm_prog ->
    ((Var.var * wsize) * coq_Z) list **)

let asm_glob_names _ _ a =
  a.asm_glob_names

(** val asm_funcs :
    ('a1, 'a2, 'a3, 'a4, 'a5) arch_decl -> ('a1, 'a2, 'a3, 'a4, 'a5, 'a6)
    asm_op_decl -> ('a1, 'a2, 'a3, 'a4, 'a5, 'a6) asm_prog ->
    (funname * ('a1, 'a2, 'a3, 'a4, 'a5, 'a6) asm_fundef) list **)

let asm_funcs _ _ a =
  a.asm_funcs

(** val is_ABReg :
    ('a1, 'a2, 'a3, 'a4, 'a5) arch_decl -> ('a1, 'a2, 'a3, 'a4, 'a5)
    asm_typed_reg -> bool **)

let is_ABReg _ = function
| ABReg _ -> true
| _ -> false

type ('reg, 'regx, 'xreg, 'rflag, 'cond) calling_convention = { callee_saved : 
                                                                ('reg, 'regx,
                                                                'xreg,
                                                                'rflag,
                                                                'cond)
                                                                asm_typed_reg
                                                                list;
                                                                call_reg_args : 
                                                                ('reg, 'regx,
                                                                'xreg,
                                                                'rflag,
                                                                'cond) reg_t
                                                                list;
                                                                call_xreg_args : 
                                                                ('reg, 'regx,
                                                                'xreg,
                                                                'rflag,
                                                                'cond) xreg_t
                                                                list;
                                                                call_reg_ret : 
                                                                ('reg, 'regx,
                                                                'xreg,
                                                                'rflag,
                                                                'cond) reg_t
                                                                list;
                                                                call_xreg_ret : 
                                                                ('reg, 'regx,
                                                                'xreg,
                                                                'rflag,
                                                                'cond) xreg_t
                                                                list }

(** val callee_saved :
    ('a1, 'a2, 'a3, 'a4, 'a5) arch_decl -> ('a1, 'a2, 'a3, 'a4, 'a5)
    calling_convention -> ('a1, 'a2, 'a3, 'a4, 'a5) asm_typed_reg list **)

let callee_saved _ calling_convention0 =
  calling_convention0.callee_saved

(** val call_reg_args :
    ('a1, 'a2, 'a3, 'a4, 'a5) arch_decl -> ('a1, 'a2, 'a3, 'a4, 'a5)
    calling_convention -> ('a1, 'a2, 'a3, 'a4, 'a5) reg_t list **)

let call_reg_args _ calling_convention0 =
  calling_convention0.call_reg_args

(** val call_xreg_args :
    ('a1, 'a2, 'a3, 'a4, 'a5) arch_decl -> ('a1, 'a2, 'a3, 'a4, 'a5)
    calling_convention -> ('a1, 'a2, 'a3, 'a4, 'a5) xreg_t list **)

let call_xreg_args _ calling_convention0 =
  calling_convention0.call_xreg_args

(** val call_reg_ret :
    ('a1, 'a2, 'a3, 'a4, 'a5) arch_decl -> ('a1, 'a2, 'a3, 'a4, 'a5)
    calling_convention -> ('a1, 'a2, 'a3, 'a4, 'a5) reg_t list **)

let call_reg_ret _ calling_convention0 =
  calling_convention0.call_reg_ret

(** val call_xreg_ret :
    ('a1, 'a2, 'a3, 'a4, 'a5) arch_decl -> ('a1, 'a2, 'a3, 'a4, 'a5)
    calling_convention -> ('a1, 'a2, 'a3, 'a4, 'a5) xreg_t list **)

let call_xreg_ret _ calling_convention0 =
  calling_convention0.call_xreg_ret

(** val get_ARReg :
    ('a1, 'a2, 'a3, 'a4, 'a5) arch_decl -> ('a1, 'a2, 'a3, 'a4, 'a5)
    asm_typed_reg -> ('a1, 'a2, 'a3, 'a4, 'a5) reg_t option **)

let get_ARReg _ = function
| ARReg r -> Some r
| _ -> None

(** val get_ARegX :
    ('a1, 'a2, 'a3, 'a4, 'a5) arch_decl -> ('a1, 'a2, 'a3, 'a4, 'a5)
    asm_typed_reg -> ('a1, 'a2, 'a3, 'a4, 'a5) regx_t option **)

let get_ARegX _ = function
| ARegX r -> Some r
| _ -> None

(** val get_AXReg :
    ('a1, 'a2, 'a3, 'a4, 'a5) arch_decl -> ('a1, 'a2, 'a3, 'a4, 'a5)
    asm_typed_reg -> ('a1, 'a2, 'a3, 'a4, 'a5) xreg_t option **)

let get_AXReg _ = function
| AXReg r -> Some r
| _ -> None

(** val check_list :
    ('a1, 'a2, 'a3, 'a4, 'a5) arch_decl -> 'a6 eqTypeC -> (('a1, 'a2, 'a3,
    'a4, 'a5) asm_typed_reg -> 'a6 option) -> ('a1, 'a2, 'a3, 'a4, 'a5)
    asm_typed_reg list -> 'a6 list -> bool **)

let check_list _ eqc get l expected =
  let r = pmap get l in
  eq_op (coq_Datatypes_list__canonical__eqtype_Equality (ceqT_eqType eqc))
    (Obj.magic r) (Obj.magic take (size r) expected)

(** val check_call_conv :
    ('a1, 'a2, 'a3, 'a4, 'a5) arch_decl -> ('a1, 'a2, 'a3, 'a4, 'a5, 'a6)
    asm_op_decl -> ('a1, 'a2, 'a3, 'a4, 'a5) calling_convention -> ('a1, 'a2,
    'a3, 'a4, 'a5, 'a6) asm_fundef -> bool **)

let check_call_conv arch _ call_conv fd =
  if fd.asm_fd_export
  then (&&)
         (check_list arch arch.toS_r._finC._eqC (get_ARReg arch)
           fd.asm_fd_arg call_conv.call_reg_args)
         ((&&)
           (check_list arch arch.toS_x._finC._eqC (get_AXReg arch)
             fd.asm_fd_arg call_conv.call_xreg_args)
           ((&&)
             (check_list arch arch.toS_r._finC._eqC (get_ARReg arch)
               fd.asm_fd_res call_conv.call_reg_ret)
             (check_list arch arch.toS_x._finC._eqC (get_AXReg arch)
               fd.asm_fd_res call_conv.call_xreg_ret)))
  else true

(** val registers :
    ('a1, 'a2, 'a3, 'a4, 'a5) arch_decl -> ('a1, 'a2, 'a3, 'a4, 'a5) reg_t
    list **)

let registers arch =
  arch.toS_r._finC.cenum

(** val registerxs :
    ('a1, 'a2, 'a3, 'a4, 'a5) arch_decl -> ('a1, 'a2, 'a3, 'a4, 'a5) regx_t
    list **)

let registerxs arch =
  arch.toS_rx._finC.cenum

(** val xregisters :
    ('a1, 'a2, 'a3, 'a4, 'a5) arch_decl -> ('a1, 'a2, 'a3, 'a4, 'a5) xreg_t
    list **)

let xregisters arch =
  arch.toS_x._finC.cenum

(** val rflags :
    ('a1, 'a2, 'a3, 'a4, 'a5) arch_decl -> ('a1, 'a2, 'a3, 'a4, 'a5) rflag_t
    list **)

let rflags arch =
  arch.toS_f._finC.cenum

type rflagv =
| Def of bool
| Undef

(** val rflagv_rect : (bool -> 'a1) -> 'a1 -> rflagv -> 'a1 **)

let rflagv_rect f f0 = function
| Def b -> f b
| Undef -> f0

(** val rflagv_rec : (bool -> 'a1) -> 'a1 -> rflagv -> 'a1 **)

let rflagv_rec f f0 = function
| Def b -> f b
| Undef -> f0

type is_rflagv =
| Coq_is_Def of bool * Param1.Coq_exports.is_bool
| Coq_is_Undef

(** val is_rflagv_rect :
    (bool -> Param1.Coq_exports.is_bool -> 'a1) -> 'a1 -> rflagv -> is_rflagv
    -> 'a1 **)

let is_rflagv_rect f f0 _ = function
| Coq_is_Def (b, p_) -> f b p_
| Coq_is_Undef -> f0

(** val is_rflagv_rec :
    (bool -> Param1.Coq_exports.is_bool -> 'a1) -> 'a1 -> rflagv -> is_rflagv
    -> 'a1 **)

let is_rflagv_rec f f0 _ = function
| Coq_is_Def (b, p_) -> f b p_
| Coq_is_Undef -> f0

(** val rflagv_tag : rflagv -> positive **)

let rflagv_tag = function
| Def _ -> Coq_xH
| Undef -> Coq_xO Coq_xH

(** val is_rflagv_inhab : rflagv -> is_rflagv **)

let is_rflagv_inhab = function
| Def h -> Coq_is_Def (h, (Coq_exports.is_bool_inhab h))
| Undef -> Coq_is_Undef

(** val is_rflagv_functor : rflagv -> is_rflagv -> is_rflagv **)

let rec is_rflagv_functor _ x =
  x

type box_rflagv_Def =
  bool
  (* singleton inductive, whose constructor was Box_rflagv_Def *)

(** val coq_Box_rflagv_Def_0 : box_rflagv_Def -> bool **)

let coq_Box_rflagv_Def_0 record =
  record

type box_rflagv_Undef =
| Box_rflagv_Undef

type rflagv_fields_t = __

(** val rflagv_fields : rflagv -> rflagv_fields_t **)

let rflagv_fields = function
| Def h -> Obj.magic h
| Undef -> Obj.magic Box_rflagv_Undef

(** val rflagv_construct : positive -> rflagv_fields_t -> rflagv option **)

let rflagv_construct p x =
  match p with
  | Coq_xI _ -> None
  | Coq_xO _ -> Some Undef
  | Coq_xH -> Some (Def (Obj.magic x))

(** val rflagv_induction :
    (bool -> Param1.Coq_exports.is_bool -> 'a1) -> 'a1 -> rflagv -> is_rflagv
    -> 'a1 **)

let rflagv_induction his_Def his_Undef _ = function
| Coq_is_Def (x0, p_) -> his_Def x0 p_
| Coq_is_Undef -> his_Undef

(** val rflagv_eqb_fields :
    (rflagv -> rflagv -> bool) -> positive -> rflagv_fields_t ->
    rflagv_fields_t -> bool **)

let rflagv_eqb_fields _ x x0 x1 =
  match x with
  | Coq_xH -> (&&) (Std.Prelude.bool_eqb (Obj.magic x0) (Obj.magic x1)) true
  | _ -> true

(** val rflagv_eqb : rflagv -> rflagv -> bool **)

let rflagv_eqb x1 x2 =
  match x1 with
  | Def h ->
    eqb_body rflagv_tag rflagv_fields
      (Obj.magic rflagv_eqb_fields (fun _ _ -> true)) (rflagv_tag (Def h)) h
      x2
  | Undef ->
    eqb_body rflagv_tag rflagv_fields
      (Obj.magic rflagv_eqb_fields (fun _ _ -> true)) (rflagv_tag Undef)
      Box_rflagv_Undef x2

(** val rflagv_eqb_OK : rflagv -> rflagv -> reflect **)

let rflagv_eqb_OK =
  iffP2 rflagv_eqb

(** val rflagv_eqb_OK_sumbool : rflagv -> rflagv -> bool **)

let rflagv_eqb_OK_sumbool =
  reflect_dec rflagv_eqb rflagv_eqb_OK

(** val coq_HB_unnamed_factory_17 : rflagv Coq_hasDecEq.axioms_ **)

let coq_HB_unnamed_factory_17 =
  { Coq_hasDecEq.eq_op = rflagv_eqb; Coq_hasDecEq.eqP = rflagv_eqb_OK }

(** val arch_decl_rflagv__canonical__eqtype_Equality : Equality.coq_type **)

let arch_decl_rflagv__canonical__eqtype_Equality =
  Obj.magic coq_HB_unnamed_factory_17

type ('reg, 'regx, 'xreg, 'rflag, 'cond, 'asm_op) asm = { _arch_decl : 
                                                          ('reg, 'regx,
                                                          'xreg, 'rflag,
                                                          'cond) arch_decl;
                                                          _asm_op_decl : 
                                                          ('reg, 'regx,
                                                          'xreg, 'rflag,
                                                          'cond, 'asm_op)
                                                          asm_op_decl;
                                                          eval_cond : 
                                                          ((('reg, 'regx,
                                                          'xreg, 'rflag,
                                                          'cond) reg_t ->
                                                          GRing.ComRing.sort)
                                                          -> (('reg, 'regx,
                                                          'xreg, 'rflag,
                                                          'cond) rflag_t ->
                                                          bool exec) ->
                                                          ('reg, 'regx,
                                                          'xreg, 'rflag,
                                                          'cond) cond_t ->
                                                          bool exec) }

(** val _arch_decl :
    ('a1, 'a2, 'a3, 'a4, 'a5, 'a6) asm -> ('a1, 'a2, 'a3, 'a4, 'a5) arch_decl **)

let _arch_decl asm0 =
  asm0._arch_decl

(** val _asm_op_decl :
    ('a1, 'a2, 'a3, 'a4, 'a5, 'a6) asm -> ('a1, 'a2, 'a3, 'a4, 'a5, 'a6)
    asm_op_decl **)

let _asm_op_decl asm0 =
  asm0._asm_op_decl

(** val eval_cond :
    ('a1, 'a2, 'a3, 'a4, 'a5, 'a6) asm -> (('a1, 'a2, 'a3, 'a4, 'a5) reg_t ->
    GRing.ComRing.sort) -> (('a1, 'a2, 'a3, 'a4, 'a5) rflag_t -> bool exec)
    -> ('a1, 'a2, 'a3, 'a4, 'a5) cond_t -> bool exec **)

let eval_cond asm0 =
  asm0.eval_cond

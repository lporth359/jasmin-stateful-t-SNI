open BinInt
open BinNums
open Bool
open Arch_decl
open Arch_utils
open Arm_expand_imm
open EqbOK
open Eqb_core_defs
open Eqtype
open Expr
open Fintype
open Flag_combination
open Seq
open Shift_kind
open Ssralg
open Ssrbool
open Type
open Utils0
open Word0
open Word_ssrZ
open Wsize

type __ = Obj.t

(** val arm_reg_size : wsize **)

let arm_reg_size =
  U32

(** val arm_xreg_size : wsize **)

let arm_xreg_size =
  U64

type register =
| R00
| R01
| R02
| R03
| R04
| R05
| R06
| R07
| R08
| R09
| R10
| R11
| R12
| LR
| SP

(** val register_rect :
    'a1 -> 'a1 -> 'a1 -> 'a1 -> 'a1 -> 'a1 -> 'a1 -> 'a1 -> 'a1 -> 'a1 -> 'a1
    -> 'a1 -> 'a1 -> 'a1 -> 'a1 -> register -> 'a1 **)

let register_rect f f0 f1 f2 f3 f4 f5 f6 f7 f8 f9 f10 f11 f12 f13 = function
| R00 -> f
| R01 -> f0
| R02 -> f1
| R03 -> f2
| R04 -> f3
| R05 -> f4
| R06 -> f5
| R07 -> f6
| R08 -> f7
| R09 -> f8
| R10 -> f9
| R11 -> f10
| R12 -> f11
| LR -> f12
| SP -> f13

(** val register_rec :
    'a1 -> 'a1 -> 'a1 -> 'a1 -> 'a1 -> 'a1 -> 'a1 -> 'a1 -> 'a1 -> 'a1 -> 'a1
    -> 'a1 -> 'a1 -> 'a1 -> 'a1 -> register -> 'a1 **)

let register_rec f f0 f1 f2 f3 f4 f5 f6 f7 f8 f9 f10 f11 f12 f13 = function
| R00 -> f
| R01 -> f0
| R02 -> f1
| R03 -> f2
| R04 -> f3
| R05 -> f4
| R06 -> f5
| R07 -> f6
| R08 -> f7
| R09 -> f8
| R10 -> f9
| R11 -> f10
| R12 -> f11
| LR -> f12
| SP -> f13

type is_register =
| Coq_is_R00
| Coq_is_R01
| Coq_is_R02
| Coq_is_R03
| Coq_is_R04
| Coq_is_R05
| Coq_is_R06
| Coq_is_R07
| Coq_is_R08
| Coq_is_R09
| Coq_is_R10
| Coq_is_R11
| Coq_is_R12
| Coq_is_LR
| Coq_is_SP

(** val is_register_rect :
    'a1 -> 'a1 -> 'a1 -> 'a1 -> 'a1 -> 'a1 -> 'a1 -> 'a1 -> 'a1 -> 'a1 -> 'a1
    -> 'a1 -> 'a1 -> 'a1 -> 'a1 -> register -> is_register -> 'a1 **)

let is_register_rect f f0 f1 f2 f3 f4 f5 f6 f7 f8 f9 f10 f11 f12 f13 _ = function
| Coq_is_R00 -> f
| Coq_is_R01 -> f0
| Coq_is_R02 -> f1
| Coq_is_R03 -> f2
| Coq_is_R04 -> f3
| Coq_is_R05 -> f4
| Coq_is_R06 -> f5
| Coq_is_R07 -> f6
| Coq_is_R08 -> f7
| Coq_is_R09 -> f8
| Coq_is_R10 -> f9
| Coq_is_R11 -> f10
| Coq_is_R12 -> f11
| Coq_is_LR -> f12
| Coq_is_SP -> f13

(** val is_register_rec :
    'a1 -> 'a1 -> 'a1 -> 'a1 -> 'a1 -> 'a1 -> 'a1 -> 'a1 -> 'a1 -> 'a1 -> 'a1
    -> 'a1 -> 'a1 -> 'a1 -> 'a1 -> register -> is_register -> 'a1 **)

let is_register_rec f f0 f1 f2 f3 f4 f5 f6 f7 f8 f9 f10 f11 f12 f13 _ = function
| Coq_is_R00 -> f
| Coq_is_R01 -> f0
| Coq_is_R02 -> f1
| Coq_is_R03 -> f2
| Coq_is_R04 -> f3
| Coq_is_R05 -> f4
| Coq_is_R06 -> f5
| Coq_is_R07 -> f6
| Coq_is_R08 -> f7
| Coq_is_R09 -> f8
| Coq_is_R10 -> f9
| Coq_is_R11 -> f10
| Coq_is_R12 -> f11
| Coq_is_LR -> f12
| Coq_is_SP -> f13

(** val register_tag : register -> positive **)

let register_tag = function
| R00 -> Coq_xH
| R01 -> Coq_xO Coq_xH
| R02 -> Coq_xI Coq_xH
| R03 -> Coq_xO (Coq_xO Coq_xH)
| R04 -> Coq_xI (Coq_xO Coq_xH)
| R05 -> Coq_xO (Coq_xI Coq_xH)
| R06 -> Coq_xI (Coq_xI Coq_xH)
| R07 -> Coq_xO (Coq_xO (Coq_xO Coq_xH))
| R08 -> Coq_xI (Coq_xO (Coq_xO Coq_xH))
| R09 -> Coq_xO (Coq_xI (Coq_xO Coq_xH))
| R10 -> Coq_xI (Coq_xI (Coq_xO Coq_xH))
| R11 -> Coq_xO (Coq_xO (Coq_xI Coq_xH))
| R12 -> Coq_xI (Coq_xO (Coq_xI Coq_xH))
| LR -> Coq_xO (Coq_xI (Coq_xI Coq_xH))
| SP -> Coq_xI (Coq_xI (Coq_xI Coq_xH))

(** val is_register_inhab : register -> is_register **)

let is_register_inhab = function
| R00 -> Coq_is_R00
| R01 -> Coq_is_R01
| R02 -> Coq_is_R02
| R03 -> Coq_is_R03
| R04 -> Coq_is_R04
| R05 -> Coq_is_R05
| R06 -> Coq_is_R06
| R07 -> Coq_is_R07
| R08 -> Coq_is_R08
| R09 -> Coq_is_R09
| R10 -> Coq_is_R10
| R11 -> Coq_is_R11
| R12 -> Coq_is_R12
| LR -> Coq_is_LR
| SP -> Coq_is_SP

(** val is_register_functor : register -> is_register -> is_register **)

let rec is_register_functor _ x =
  x

type box_register_R00 =
| Box_register_R00

type register_fields_t = __

(** val register_fields : register -> register_fields_t **)

let register_fields _ =
  Obj.magic Box_register_R00

(** val register_construct :
    positive -> register_fields_t -> register option **)

let register_construct p _ =
  match p with
  | Coq_xI x ->
    (match x with
     | Coq_xI x0 ->
       (match x0 with
        | Coq_xI _ -> Some SP
        | Coq_xO _ -> Some R10
        | Coq_xH -> Some R06)
     | Coq_xO x0 ->
       (match x0 with
        | Coq_xI _ -> Some R12
        | Coq_xO _ -> Some R08
        | Coq_xH -> Some R04)
     | Coq_xH -> Some R02)
  | Coq_xO x ->
    (match x with
     | Coq_xI x0 ->
       (match x0 with
        | Coq_xI _ -> Some LR
        | Coq_xO _ -> Some R09
        | Coq_xH -> Some R05)
     | Coq_xO x0 ->
       (match x0 with
        | Coq_xI _ -> Some R11
        | Coq_xO _ -> Some R07
        | Coq_xH -> Some R03)
     | Coq_xH -> Some R01)
  | Coq_xH -> Some R00

(** val register_induction :
    'a1 -> 'a1 -> 'a1 -> 'a1 -> 'a1 -> 'a1 -> 'a1 -> 'a1 -> 'a1 -> 'a1 -> 'a1
    -> 'a1 -> 'a1 -> 'a1 -> 'a1 -> register -> is_register -> 'a1 **)

let register_induction his_R00 his_R01 his_R02 his_R03 his_R04 his_R05 his_R06 his_R07 his_R08 his_R09 his_R10 his_R11 his_R12 his_LR his_SP _ = function
| Coq_is_R00 -> his_R00
| Coq_is_R01 -> his_R01
| Coq_is_R02 -> his_R02
| Coq_is_R03 -> his_R03
| Coq_is_R04 -> his_R04
| Coq_is_R05 -> his_R05
| Coq_is_R06 -> his_R06
| Coq_is_R07 -> his_R07
| Coq_is_R08 -> his_R08
| Coq_is_R09 -> his_R09
| Coq_is_R10 -> his_R10
| Coq_is_R11 -> his_R11
| Coq_is_R12 -> his_R12
| Coq_is_LR -> his_LR
| Coq_is_SP -> his_SP

(** val register_eqb_fields :
    (register -> register -> bool) -> positive -> register_fields_t ->
    register_fields_t -> bool **)

let register_eqb_fields _ _ _ _ =
  true

(** val register_eqb : register -> register -> bool **)

let register_eqb x1 x2 =
  eqb_body register_tag register_fields
    (Obj.magic register_eqb_fields (fun _ _ -> true)) (register_tag x1)
    Box_register_R00 x2

(** val register_eqb_OK : register -> register -> reflect **)

let register_eqb_OK =
  iffP2 register_eqb

(** val register_eqb_OK_sumbool : register -> register -> bool **)

let register_eqb_OK_sumbool =
  reflect_dec register_eqb register_eqb_OK

(** val eqTC_register : register eqTypeC **)

let eqTC_register =
  { beq = register_eqb; ceqP = register_eqb_OK }

(** val arm_register_eqType : Equality.coq_type **)

let arm_register_eqType =
  ceqT_eqType eqTC_register

(** val registers : register list **)

let registers =
  R00 :: (R01 :: (R02 :: (R03 :: (R04 :: (R05 :: (R06 :: (R07 :: (R08 :: (R09 :: (R10 :: (R11 :: (R12 :: (LR :: (SP :: []))))))))))))))

(** val finTC_register : register finTypeC **)

let finTC_register =
  { _eqC = eqTC_register; cenum = registers }

(** val register_finType : Finite.coq_type **)

let register_finType =
  cfinT_finType finTC_register

(** val register_to_string : register -> string **)

let register_to_string = function
| R00 -> "r0"
| R01 -> "r1"
| R02 -> "r2"
| R03 -> "r3"
| R04 -> "r4"
| R05 -> "r5"
| R06 -> "r6"
| R07 -> "r7"
| R08 -> "r8"
| R09 -> "r9"
| R10 -> "r10"
| R11 -> "r11"
| R12 -> "r12"
| LR -> "lr"
| SP -> "sp"

(** val reg_toS : register coq_ToString **)

let reg_toS =
  { category = "register"; _finC = finTC_register; to_string =
    register_to_string }

type rflag =
| NF
| ZF
| CF
| VF

(** val rflag_rect : 'a1 -> 'a1 -> 'a1 -> 'a1 -> rflag -> 'a1 **)

let rflag_rect f f0 f1 f2 = function
| NF -> f
| ZF -> f0
| CF -> f1
| VF -> f2

(** val rflag_rec : 'a1 -> 'a1 -> 'a1 -> 'a1 -> rflag -> 'a1 **)

let rflag_rec f f0 f1 f2 = function
| NF -> f
| ZF -> f0
| CF -> f1
| VF -> f2

type is_rflag =
| Coq_is_NF
| Coq_is_ZF
| Coq_is_CF
| Coq_is_VF

(** val is_rflag_rect :
    'a1 -> 'a1 -> 'a1 -> 'a1 -> rflag -> is_rflag -> 'a1 **)

let is_rflag_rect f f0 f1 f2 _ = function
| Coq_is_NF -> f
| Coq_is_ZF -> f0
| Coq_is_CF -> f1
| Coq_is_VF -> f2

(** val is_rflag_rec :
    'a1 -> 'a1 -> 'a1 -> 'a1 -> rflag -> is_rflag -> 'a1 **)

let is_rflag_rec f f0 f1 f2 _ = function
| Coq_is_NF -> f
| Coq_is_ZF -> f0
| Coq_is_CF -> f1
| Coq_is_VF -> f2

(** val rflag_tag : rflag -> positive **)

let rflag_tag = function
| NF -> Coq_xH
| ZF -> Coq_xO Coq_xH
| CF -> Coq_xI Coq_xH
| VF -> Coq_xO (Coq_xO Coq_xH)

(** val is_rflag_inhab : rflag -> is_rflag **)

let is_rflag_inhab = function
| NF -> Coq_is_NF
| ZF -> Coq_is_ZF
| CF -> Coq_is_CF
| VF -> Coq_is_VF

(** val is_rflag_functor : rflag -> is_rflag -> is_rflag **)

let rec is_rflag_functor _ x =
  x

type box_rflag_NF =
| Box_rflag_NF

type rflag_fields_t = __

(** val rflag_fields : rflag -> rflag_fields_t **)

let rflag_fields _ =
  Obj.magic Box_rflag_NF

(** val rflag_construct : positive -> rflag_fields_t -> rflag option **)

let rflag_construct p _ =
  match p with
  | Coq_xI _ -> Some CF
  | Coq_xO x ->
    (match x with
     | Coq_xI _ -> None
     | Coq_xO _ -> Some VF
     | Coq_xH -> Some ZF)
  | Coq_xH -> Some NF

(** val rflag_induction :
    'a1 -> 'a1 -> 'a1 -> 'a1 -> rflag -> is_rflag -> 'a1 **)

let rflag_induction his_NF his_ZF his_CF his_VF _ = function
| Coq_is_NF -> his_NF
| Coq_is_ZF -> his_ZF
| Coq_is_CF -> his_CF
| Coq_is_VF -> his_VF

(** val rflag_eqb_fields :
    (rflag -> rflag -> bool) -> positive -> rflag_fields_t -> rflag_fields_t
    -> bool **)

let rflag_eqb_fields _ _ _ _ =
  true

(** val rflag_eqb : rflag -> rflag -> bool **)

let rflag_eqb x1 x2 =
  eqb_body rflag_tag rflag_fields
    (Obj.magic rflag_eqb_fields (fun _ _ -> true)) (rflag_tag x1)
    Box_rflag_NF x2

(** val rflag_eqb_OK : rflag -> rflag -> reflect **)

let rflag_eqb_OK =
  iffP2 rflag_eqb

(** val rflag_eqb_OK_sumbool : rflag -> rflag -> bool **)

let rflag_eqb_OK_sumbool =
  reflect_dec rflag_eqb rflag_eqb_OK

(** val eqTC_rflag : rflag eqTypeC **)

let eqTC_rflag =
  { beq = rflag_eqb; ceqP = rflag_eqb_OK }

(** val rflag_eqType : Equality.coq_type **)

let rflag_eqType =
  ceqT_eqType eqTC_rflag

(** val rflags : rflag list **)

let rflags =
  NF :: (ZF :: (CF :: (VF :: [])))

(** val finTC_rflag : rflag finTypeC **)

let finTC_rflag =
  { _eqC = eqTC_rflag; cenum = rflags }

(** val rflag_finType : Finite.coq_type **)

let rflag_finType =
  cfinT_finType finTC_rflag

(** val flag_to_string : rflag -> string **)

let flag_to_string = function
| NF -> "NF"
| ZF -> "ZF"
| CF -> "CF"
| VF -> "VF"

(** val rflag_toS : rflag coq_ToString **)

let rflag_toS =
  { category = "rflag"; _finC = finTC_rflag; to_string = flag_to_string }

type condt =
| EQ_ct
| NE_ct
| CS_ct
| CC_ct
| MI_ct
| PL_ct
| VS_ct
| VC_ct
| HI_ct
| LS_ct
| GE_ct
| LT_ct
| GT_ct
| LE_ct

(** val condt_rect :
    'a1 -> 'a1 -> 'a1 -> 'a1 -> 'a1 -> 'a1 -> 'a1 -> 'a1 -> 'a1 -> 'a1 -> 'a1
    -> 'a1 -> 'a1 -> 'a1 -> condt -> 'a1 **)

let condt_rect f f0 f1 f2 f3 f4 f5 f6 f7 f8 f9 f10 f11 f12 = function
| EQ_ct -> f
| NE_ct -> f0
| CS_ct -> f1
| CC_ct -> f2
| MI_ct -> f3
| PL_ct -> f4
| VS_ct -> f5
| VC_ct -> f6
| HI_ct -> f7
| LS_ct -> f8
| GE_ct -> f9
| LT_ct -> f10
| GT_ct -> f11
| LE_ct -> f12

(** val condt_rec :
    'a1 -> 'a1 -> 'a1 -> 'a1 -> 'a1 -> 'a1 -> 'a1 -> 'a1 -> 'a1 -> 'a1 -> 'a1
    -> 'a1 -> 'a1 -> 'a1 -> condt -> 'a1 **)

let condt_rec f f0 f1 f2 f3 f4 f5 f6 f7 f8 f9 f10 f11 f12 = function
| EQ_ct -> f
| NE_ct -> f0
| CS_ct -> f1
| CC_ct -> f2
| MI_ct -> f3
| PL_ct -> f4
| VS_ct -> f5
| VC_ct -> f6
| HI_ct -> f7
| LS_ct -> f8
| GE_ct -> f9
| LT_ct -> f10
| GT_ct -> f11
| LE_ct -> f12

type is_condt =
| Coq_is_EQ_ct
| Coq_is_NE_ct
| Coq_is_CS_ct
| Coq_is_CC_ct
| Coq_is_MI_ct
| Coq_is_PL_ct
| Coq_is_VS_ct
| Coq_is_VC_ct
| Coq_is_HI_ct
| Coq_is_LS_ct
| Coq_is_GE_ct
| Coq_is_LT_ct
| Coq_is_GT_ct
| Coq_is_LE_ct

(** val is_condt_rect :
    'a1 -> 'a1 -> 'a1 -> 'a1 -> 'a1 -> 'a1 -> 'a1 -> 'a1 -> 'a1 -> 'a1 -> 'a1
    -> 'a1 -> 'a1 -> 'a1 -> condt -> is_condt -> 'a1 **)

let is_condt_rect f f0 f1 f2 f3 f4 f5 f6 f7 f8 f9 f10 f11 f12 _ = function
| Coq_is_EQ_ct -> f
| Coq_is_NE_ct -> f0
| Coq_is_CS_ct -> f1
| Coq_is_CC_ct -> f2
| Coq_is_MI_ct -> f3
| Coq_is_PL_ct -> f4
| Coq_is_VS_ct -> f5
| Coq_is_VC_ct -> f6
| Coq_is_HI_ct -> f7
| Coq_is_LS_ct -> f8
| Coq_is_GE_ct -> f9
| Coq_is_LT_ct -> f10
| Coq_is_GT_ct -> f11
| Coq_is_LE_ct -> f12

(** val is_condt_rec :
    'a1 -> 'a1 -> 'a1 -> 'a1 -> 'a1 -> 'a1 -> 'a1 -> 'a1 -> 'a1 -> 'a1 -> 'a1
    -> 'a1 -> 'a1 -> 'a1 -> condt -> is_condt -> 'a1 **)

let is_condt_rec f f0 f1 f2 f3 f4 f5 f6 f7 f8 f9 f10 f11 f12 _ = function
| Coq_is_EQ_ct -> f
| Coq_is_NE_ct -> f0
| Coq_is_CS_ct -> f1
| Coq_is_CC_ct -> f2
| Coq_is_MI_ct -> f3
| Coq_is_PL_ct -> f4
| Coq_is_VS_ct -> f5
| Coq_is_VC_ct -> f6
| Coq_is_HI_ct -> f7
| Coq_is_LS_ct -> f8
| Coq_is_GE_ct -> f9
| Coq_is_LT_ct -> f10
| Coq_is_GT_ct -> f11
| Coq_is_LE_ct -> f12

(** val condt_tag : condt -> positive **)

let condt_tag = function
| EQ_ct -> Coq_xH
| NE_ct -> Coq_xO Coq_xH
| CS_ct -> Coq_xI Coq_xH
| CC_ct -> Coq_xO (Coq_xO Coq_xH)
| MI_ct -> Coq_xI (Coq_xO Coq_xH)
| PL_ct -> Coq_xO (Coq_xI Coq_xH)
| VS_ct -> Coq_xI (Coq_xI Coq_xH)
| VC_ct -> Coq_xO (Coq_xO (Coq_xO Coq_xH))
| HI_ct -> Coq_xI (Coq_xO (Coq_xO Coq_xH))
| LS_ct -> Coq_xO (Coq_xI (Coq_xO Coq_xH))
| GE_ct -> Coq_xI (Coq_xI (Coq_xO Coq_xH))
| LT_ct -> Coq_xO (Coq_xO (Coq_xI Coq_xH))
| GT_ct -> Coq_xI (Coq_xO (Coq_xI Coq_xH))
| LE_ct -> Coq_xO (Coq_xI (Coq_xI Coq_xH))

(** val is_condt_inhab : condt -> is_condt **)

let is_condt_inhab = function
| EQ_ct -> Coq_is_EQ_ct
| NE_ct -> Coq_is_NE_ct
| CS_ct -> Coq_is_CS_ct
| CC_ct -> Coq_is_CC_ct
| MI_ct -> Coq_is_MI_ct
| PL_ct -> Coq_is_PL_ct
| VS_ct -> Coq_is_VS_ct
| VC_ct -> Coq_is_VC_ct
| HI_ct -> Coq_is_HI_ct
| LS_ct -> Coq_is_LS_ct
| GE_ct -> Coq_is_GE_ct
| LT_ct -> Coq_is_LT_ct
| GT_ct -> Coq_is_GT_ct
| LE_ct -> Coq_is_LE_ct

(** val is_condt_functor : condt -> is_condt -> is_condt **)

let rec is_condt_functor _ x =
  x

type box_condt_EQ_ct =
| Box_condt_EQ_ct

type condt_fields_t = __

(** val condt_fields : condt -> condt_fields_t **)

let condt_fields _ =
  Obj.magic Box_condt_EQ_ct

(** val condt_construct : positive -> condt_fields_t -> condt option **)

let condt_construct p _ =
  match p with
  | Coq_xI x ->
    (match x with
     | Coq_xI x0 ->
       (match x0 with
        | Coq_xI _ -> None
        | Coq_xO _ -> Some GE_ct
        | Coq_xH -> Some VS_ct)
     | Coq_xO x0 ->
       (match x0 with
        | Coq_xI _ -> Some GT_ct
        | Coq_xO _ -> Some HI_ct
        | Coq_xH -> Some MI_ct)
     | Coq_xH -> Some CS_ct)
  | Coq_xO x ->
    (match x with
     | Coq_xI x0 ->
       (match x0 with
        | Coq_xI _ -> Some LE_ct
        | Coq_xO _ -> Some LS_ct
        | Coq_xH -> Some PL_ct)
     | Coq_xO x0 ->
       (match x0 with
        | Coq_xI _ -> Some LT_ct
        | Coq_xO _ -> Some VC_ct
        | Coq_xH -> Some CC_ct)
     | Coq_xH -> Some NE_ct)
  | Coq_xH -> Some EQ_ct

(** val condt_induction :
    'a1 -> 'a1 -> 'a1 -> 'a1 -> 'a1 -> 'a1 -> 'a1 -> 'a1 -> 'a1 -> 'a1 -> 'a1
    -> 'a1 -> 'a1 -> 'a1 -> condt -> is_condt -> 'a1 **)

let condt_induction his_EQ_ct his_NE_ct his_CS_ct his_CC_ct his_MI_ct his_PL_ct his_VS_ct his_VC_ct his_HI_ct his_LS_ct his_GE_ct his_LT_ct his_GT_ct his_LE_ct _ = function
| Coq_is_EQ_ct -> his_EQ_ct
| Coq_is_NE_ct -> his_NE_ct
| Coq_is_CS_ct -> his_CS_ct
| Coq_is_CC_ct -> his_CC_ct
| Coq_is_MI_ct -> his_MI_ct
| Coq_is_PL_ct -> his_PL_ct
| Coq_is_VS_ct -> his_VS_ct
| Coq_is_VC_ct -> his_VC_ct
| Coq_is_HI_ct -> his_HI_ct
| Coq_is_LS_ct -> his_LS_ct
| Coq_is_GE_ct -> his_GE_ct
| Coq_is_LT_ct -> his_LT_ct
| Coq_is_GT_ct -> his_GT_ct
| Coq_is_LE_ct -> his_LE_ct

(** val condt_eqb_fields :
    (condt -> condt -> bool) -> positive -> condt_fields_t -> condt_fields_t
    -> bool **)

let condt_eqb_fields _ _ _ _ =
  true

(** val condt_eqb : condt -> condt -> bool **)

let condt_eqb x1 x2 =
  eqb_body condt_tag condt_fields
    (Obj.magic condt_eqb_fields (fun _ _ -> true)) (condt_tag x1)
    Box_condt_EQ_ct x2

(** val condt_eqb_OK : condt -> condt -> reflect **)

let condt_eqb_OK =
  iffP2 condt_eqb

(** val condt_eqb_OK_sumbool : condt -> condt -> bool **)

let condt_eqb_OK_sumbool =
  reflect_dec condt_eqb condt_eqb_OK

(** val eqTC_condt : condt eqTypeC **)

let eqTC_condt =
  { beq = condt_eqb; ceqP = condt_eqb_OK }

(** val condt_eqType : Equality.coq_type **)

let condt_eqType =
  ceqT_eqType eqTC_condt

(** val condts : condt list **)

let condts =
  EQ_ct :: (NE_ct :: (CS_ct :: (CC_ct :: (MI_ct :: (PL_ct :: (VS_ct :: (VC_ct :: (HI_ct :: (LS_ct :: (GE_ct :: (LT_ct :: (GT_ct :: (LE_ct :: [])))))))))))))

(** val finTC_condt : condt finTypeC **)

let finTC_condt =
  { _eqC = eqTC_condt; cenum = condts }

(** val condt_finType : Finite.coq_type **)

let condt_finType =
  cfinT_finType finTC_condt

(** val string_of_condt : condt -> string **)

let string_of_condt = function
| EQ_ct -> "eq"
| NE_ct -> "ne"
| CS_ct -> "cs"
| CC_ct -> "cc"
| MI_ct -> "mi"
| PL_ct -> "pl"
| VS_ct -> "vs"
| VC_ct -> "vc"
| HI_ct -> "hi"
| LS_ct -> "ls"
| GE_ct -> "ge"
| LT_ct -> "lt"
| GT_ct -> "gt"
| LE_ct -> "le"

(** val eqTC_shift_kind : shift_kind eqTypeC **)

let eqTC_shift_kind =
  { beq = shift_kind_eqb; ceqP = shift_kind_eqb_OK }

(** val shift_kind_eqType : Equality.coq_type **)

let shift_kind_eqType =
  ceqT_eqType eqTC_shift_kind

(** val shift_kinds : shift_kind list **)

let shift_kinds =
  SLSL :: (SLSR :: (SASR :: (SROR :: [])))

(** val string_of_shift_kind : shift_kind -> string **)

let string_of_shift_kind = function
| SLSL -> "lsl"
| SLSR -> "lsr"
| SASR -> "asr"
| SROR -> "ror"

(** val check_shift_amount : shift_kind -> coq_Z -> bool **)

let check_shift_amount sk z =
  let (lo, hi) = shift_amount_bounds sk in (&&) (Z.leb lo z) (Z.leb z hi)

(** val shift_op :
    shift_kind -> wsize -> GRing.ComRing.sort -> coq_Z -> GRing.ComRing.sort **)

let shift_op = function
| SLSL -> wshl
| SLSR -> wshr
| SASR -> wsar
| SROR -> wror

(** val shift_of_sop2 : wsize -> sop2 -> shift_kind option **)

let shift_of_sop2 ws op =
  match oassert
          (eq_op wsize_wsize__canonical__eqtype_Equality (Obj.magic ws)
            (Obj.magic U32)) with
  | Some _ ->
    (match op with
     | Olsr w -> (match w with
                  | U32 -> Some SLSR
                  | _ -> None)
     | Olsl o ->
       (match o with
        | Op_int -> None
        | Op_w w -> (match w with
                     | U32 -> Some SLSL
                     | _ -> None))
     | Oasr o ->
       (match o with
        | Op_int -> None
        | Op_w w -> (match w with
                     | U32 -> Some SASR
                     | _ -> None))
     | Oror w -> (match w with
                  | U32 -> Some SROR
                  | _ -> None)
     | _ -> None)
  | None -> None

(** val arm_fc_of_cfc : combine_flags_core -> flag_combination **)

let arm_fc_of_cfc cfc =
  let vnf = FCVar0 in
  let vzf = FCVar1 in
  let vcf = FCVar2 in
  let vvf = FCVar3 in
  (match cfc with
   | CFC_B -> FCNot vcf
   | CFC_E -> vzf
   | CFC_L -> FCNot (FCEq (vnf, vvf))
   | CFC_BE -> FCOr ((FCNot vcf), vzf)
   | CFC_LE -> FCOr (vzf, (FCNot (FCEq (vnf, vvf)))))

(** val arm_fcp : coq_FlagCombinationParams **)

let arm_fcp =
  arm_fc_of_cfc

(** val arm_check_CAimm :
    caimm_checker_s -> wsize -> GRing.ComRing.sort -> bool **)

let arm_check_CAimm checker ws w =
  match checker with
  | CAimmC_none -> true
  | CAimmC_arm_shift_amout sk -> check_shift_amount sk (wunsigned ws w)
  | CAimmC_arm_wencoding ew -> check_ei_kind ew ws w
  | CAimmC_arm_0_8_16_24 ->
    let x = wunsigned ws w in
    in_mem (Obj.magic x)
      (mem (seq_predType coq_BinNums_Z__canonical__eqtype_Equality)
        (Obj.magic (Z0 :: ((Zpos (Coq_xO (Coq_xO (Coq_xO Coq_xH)))) :: ((Zpos
          (Coq_xO (Coq_xO (Coq_xO (Coq_xO Coq_xH))))) :: ((Zpos (Coq_xO
          (Coq_xO (Coq_xO (Coq_xI Coq_xH))))) :: []))))))
  | _ -> false

(** val arm_decl : (register, empty, empty, rflag, condt) arch_decl **)

let arm_decl =
  { reg_size = arm_reg_size; xreg_size = arm_xreg_size; cond_eqC =
    eqTC_condt; toS_r = reg_toS; toS_rx = (empty_toS (Coq_lword U32));
    toS_x = (empty_toS (Coq_lword U64)); toS_f = rflag_toS; ad_rsp = SP;
    ad_fcp = arm_fcp; check_CAimm = arm_check_CAimm }

(** val arm_linux_call_conv :
    (register, empty, empty, rflag, condt) calling_convention **)

let arm_linux_call_conv =
  { callee_saved =
    (map (fun x -> ARReg x)
      (R04 :: (R05 :: (R06 :: (R07 :: (R08 :: (R09 :: (R10 :: (R11 :: (SP :: []))))))))));
    call_reg_args = (R00 :: (R01 :: (R02 :: (R03 :: [])))); call_xreg_args =
    []; call_reg_ret = (R00 :: (R01 :: [])); call_xreg_ret = [] }

(** val is_expandable : coq_Z -> bool **)

let is_expandable n =
  match ei_kind n with
  | EI_none -> false
  | EI_shift -> false
  | _ -> true

(** val is_expandable_or_shift : coq_Z -> bool **)

let is_expandable_or_shift n =
  match ei_kind n with
  | EI_none -> false
  | _ -> true

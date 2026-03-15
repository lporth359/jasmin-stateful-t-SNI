open BinInt
open BinNums
open Bool
open Arch_decl
open Arch_utils
open EqbOK
open Eqb_core_defs
open Eqtype
open Fintype
open Flag_combination
open Seq
open Ssralg
open Std
open Type
open Utils0
open Word0
open Wsize

type __ = Obj.t

(** val riscv_reg_size : wsize **)

let riscv_reg_size =
  U32

(** val riscv_xreg_size : wsize **)

let riscv_xreg_size =
  U64

type register =
| RA
| SP
| X5
| X6
| X7
| X8
| X9
| X10
| X11
| X12
| X13
| X14
| X15
| X16
| X17
| X18
| X19
| X20
| X21
| X22
| X23
| X24
| X25
| X26
| X27
| X28
| X29
| X30
| X31

(** val register_rect :
    'a1 -> 'a1 -> 'a1 -> 'a1 -> 'a1 -> 'a1 -> 'a1 -> 'a1 -> 'a1 -> 'a1 -> 'a1
    -> 'a1 -> 'a1 -> 'a1 -> 'a1 -> 'a1 -> 'a1 -> 'a1 -> 'a1 -> 'a1 -> 'a1 ->
    'a1 -> 'a1 -> 'a1 -> 'a1 -> 'a1 -> 'a1 -> 'a1 -> 'a1 -> register -> 'a1 **)

let register_rect f f0 f1 f2 f3 f4 f5 f6 f7 f8 f9 f10 f11 f12 f13 f14 f15 f16 f17 f18 f19 f20 f21 f22 f23 f24 f25 f26 f27 = function
| RA -> f
| SP -> f0
| X5 -> f1
| X6 -> f2
| X7 -> f3
| X8 -> f4
| X9 -> f5
| X10 -> f6
| X11 -> f7
| X12 -> f8
| X13 -> f9
| X14 -> f10
| X15 -> f11
| X16 -> f12
| X17 -> f13
| X18 -> f14
| X19 -> f15
| X20 -> f16
| X21 -> f17
| X22 -> f18
| X23 -> f19
| X24 -> f20
| X25 -> f21
| X26 -> f22
| X27 -> f23
| X28 -> f24
| X29 -> f25
| X30 -> f26
| X31 -> f27

(** val register_rec :
    'a1 -> 'a1 -> 'a1 -> 'a1 -> 'a1 -> 'a1 -> 'a1 -> 'a1 -> 'a1 -> 'a1 -> 'a1
    -> 'a1 -> 'a1 -> 'a1 -> 'a1 -> 'a1 -> 'a1 -> 'a1 -> 'a1 -> 'a1 -> 'a1 ->
    'a1 -> 'a1 -> 'a1 -> 'a1 -> 'a1 -> 'a1 -> 'a1 -> 'a1 -> register -> 'a1 **)

let register_rec f f0 f1 f2 f3 f4 f5 f6 f7 f8 f9 f10 f11 f12 f13 f14 f15 f16 f17 f18 f19 f20 f21 f22 f23 f24 f25 f26 f27 = function
| RA -> f
| SP -> f0
| X5 -> f1
| X6 -> f2
| X7 -> f3
| X8 -> f4
| X9 -> f5
| X10 -> f6
| X11 -> f7
| X12 -> f8
| X13 -> f9
| X14 -> f10
| X15 -> f11
| X16 -> f12
| X17 -> f13
| X18 -> f14
| X19 -> f15
| X20 -> f16
| X21 -> f17
| X22 -> f18
| X23 -> f19
| X24 -> f20
| X25 -> f21
| X26 -> f22
| X27 -> f23
| X28 -> f24
| X29 -> f25
| X30 -> f26
| X31 -> f27

type is_register =
| Coq_is_RA
| Coq_is_SP
| Coq_is_X5
| Coq_is_X6
| Coq_is_X7
| Coq_is_X8
| Coq_is_X9
| Coq_is_X10
| Coq_is_X11
| Coq_is_X12
| Coq_is_X13
| Coq_is_X14
| Coq_is_X15
| Coq_is_X16
| Coq_is_X17
| Coq_is_X18
| Coq_is_X19
| Coq_is_X20
| Coq_is_X21
| Coq_is_X22
| Coq_is_X23
| Coq_is_X24
| Coq_is_X25
| Coq_is_X26
| Coq_is_X27
| Coq_is_X28
| Coq_is_X29
| Coq_is_X30
| Coq_is_X31

(** val is_register_rect :
    'a1 -> 'a1 -> 'a1 -> 'a1 -> 'a1 -> 'a1 -> 'a1 -> 'a1 -> 'a1 -> 'a1 -> 'a1
    -> 'a1 -> 'a1 -> 'a1 -> 'a1 -> 'a1 -> 'a1 -> 'a1 -> 'a1 -> 'a1 -> 'a1 ->
    'a1 -> 'a1 -> 'a1 -> 'a1 -> 'a1 -> 'a1 -> 'a1 -> 'a1 -> register ->
    is_register -> 'a1 **)

let is_register_rect f f0 f1 f2 f3 f4 f5 f6 f7 f8 f9 f10 f11 f12 f13 f14 f15 f16 f17 f18 f19 f20 f21 f22 f23 f24 f25 f26 f27 _ = function
| Coq_is_RA -> f
| Coq_is_SP -> f0
| Coq_is_X5 -> f1
| Coq_is_X6 -> f2
| Coq_is_X7 -> f3
| Coq_is_X8 -> f4
| Coq_is_X9 -> f5
| Coq_is_X10 -> f6
| Coq_is_X11 -> f7
| Coq_is_X12 -> f8
| Coq_is_X13 -> f9
| Coq_is_X14 -> f10
| Coq_is_X15 -> f11
| Coq_is_X16 -> f12
| Coq_is_X17 -> f13
| Coq_is_X18 -> f14
| Coq_is_X19 -> f15
| Coq_is_X20 -> f16
| Coq_is_X21 -> f17
| Coq_is_X22 -> f18
| Coq_is_X23 -> f19
| Coq_is_X24 -> f20
| Coq_is_X25 -> f21
| Coq_is_X26 -> f22
| Coq_is_X27 -> f23
| Coq_is_X28 -> f24
| Coq_is_X29 -> f25
| Coq_is_X30 -> f26
| Coq_is_X31 -> f27

(** val is_register_rec :
    'a1 -> 'a1 -> 'a1 -> 'a1 -> 'a1 -> 'a1 -> 'a1 -> 'a1 -> 'a1 -> 'a1 -> 'a1
    -> 'a1 -> 'a1 -> 'a1 -> 'a1 -> 'a1 -> 'a1 -> 'a1 -> 'a1 -> 'a1 -> 'a1 ->
    'a1 -> 'a1 -> 'a1 -> 'a1 -> 'a1 -> 'a1 -> 'a1 -> 'a1 -> register ->
    is_register -> 'a1 **)

let is_register_rec f f0 f1 f2 f3 f4 f5 f6 f7 f8 f9 f10 f11 f12 f13 f14 f15 f16 f17 f18 f19 f20 f21 f22 f23 f24 f25 f26 f27 _ = function
| Coq_is_RA -> f
| Coq_is_SP -> f0
| Coq_is_X5 -> f1
| Coq_is_X6 -> f2
| Coq_is_X7 -> f3
| Coq_is_X8 -> f4
| Coq_is_X9 -> f5
| Coq_is_X10 -> f6
| Coq_is_X11 -> f7
| Coq_is_X12 -> f8
| Coq_is_X13 -> f9
| Coq_is_X14 -> f10
| Coq_is_X15 -> f11
| Coq_is_X16 -> f12
| Coq_is_X17 -> f13
| Coq_is_X18 -> f14
| Coq_is_X19 -> f15
| Coq_is_X20 -> f16
| Coq_is_X21 -> f17
| Coq_is_X22 -> f18
| Coq_is_X23 -> f19
| Coq_is_X24 -> f20
| Coq_is_X25 -> f21
| Coq_is_X26 -> f22
| Coq_is_X27 -> f23
| Coq_is_X28 -> f24
| Coq_is_X29 -> f25
| Coq_is_X30 -> f26
| Coq_is_X31 -> f27

(** val register_tag : register -> positive **)

let register_tag = function
| RA -> Coq_xH
| SP -> Coq_xO Coq_xH
| X5 -> Coq_xI Coq_xH
| X6 -> Coq_xO (Coq_xO Coq_xH)
| X7 -> Coq_xI (Coq_xO Coq_xH)
| X8 -> Coq_xO (Coq_xI Coq_xH)
| X9 -> Coq_xI (Coq_xI Coq_xH)
| X10 -> Coq_xO (Coq_xO (Coq_xO Coq_xH))
| X11 -> Coq_xI (Coq_xO (Coq_xO Coq_xH))
| X12 -> Coq_xO (Coq_xI (Coq_xO Coq_xH))
| X13 -> Coq_xI (Coq_xI (Coq_xO Coq_xH))
| X14 -> Coq_xO (Coq_xO (Coq_xI Coq_xH))
| X15 -> Coq_xI (Coq_xO (Coq_xI Coq_xH))
| X16 -> Coq_xO (Coq_xI (Coq_xI Coq_xH))
| X17 -> Coq_xI (Coq_xI (Coq_xI Coq_xH))
| X18 -> Coq_xO (Coq_xO (Coq_xO (Coq_xO Coq_xH)))
| X19 -> Coq_xI (Coq_xO (Coq_xO (Coq_xO Coq_xH)))
| X20 -> Coq_xO (Coq_xI (Coq_xO (Coq_xO Coq_xH)))
| X21 -> Coq_xI (Coq_xI (Coq_xO (Coq_xO Coq_xH)))
| X22 -> Coq_xO (Coq_xO (Coq_xI (Coq_xO Coq_xH)))
| X23 -> Coq_xI (Coq_xO (Coq_xI (Coq_xO Coq_xH)))
| X24 -> Coq_xO (Coq_xI (Coq_xI (Coq_xO Coq_xH)))
| X25 -> Coq_xI (Coq_xI (Coq_xI (Coq_xO Coq_xH)))
| X26 -> Coq_xO (Coq_xO (Coq_xO (Coq_xI Coq_xH)))
| X27 -> Coq_xI (Coq_xO (Coq_xO (Coq_xI Coq_xH)))
| X28 -> Coq_xO (Coq_xI (Coq_xO (Coq_xI Coq_xH)))
| X29 -> Coq_xI (Coq_xI (Coq_xO (Coq_xI Coq_xH)))
| X30 -> Coq_xO (Coq_xO (Coq_xI (Coq_xI Coq_xH)))
| X31 -> Coq_xI (Coq_xO (Coq_xI (Coq_xI Coq_xH)))

(** val is_register_inhab : register -> is_register **)

let is_register_inhab = function
| RA -> Coq_is_RA
| SP -> Coq_is_SP
| X5 -> Coq_is_X5
| X6 -> Coq_is_X6
| X7 -> Coq_is_X7
| X8 -> Coq_is_X8
| X9 -> Coq_is_X9
| X10 -> Coq_is_X10
| X11 -> Coq_is_X11
| X12 -> Coq_is_X12
| X13 -> Coq_is_X13
| X14 -> Coq_is_X14
| X15 -> Coq_is_X15
| X16 -> Coq_is_X16
| X17 -> Coq_is_X17
| X18 -> Coq_is_X18
| X19 -> Coq_is_X19
| X20 -> Coq_is_X20
| X21 -> Coq_is_X21
| X22 -> Coq_is_X22
| X23 -> Coq_is_X23
| X24 -> Coq_is_X24
| X25 -> Coq_is_X25
| X26 -> Coq_is_X26
| X27 -> Coq_is_X27
| X28 -> Coq_is_X28
| X29 -> Coq_is_X29
| X30 -> Coq_is_X30
| X31 -> Coq_is_X31

(** val is_register_functor : register -> is_register -> is_register **)

let rec is_register_functor _ x =
  x

type box_register_RA =
| Box_register_RA

type register_fields_t = __

(** val register_fields : register -> register_fields_t **)

let register_fields _ =
  Obj.magic Box_register_RA

(** val register_construct :
    positive -> register_fields_t -> register option **)

let register_construct p _ =
  match p with
  | Coq_xI x ->
    (match x with
     | Coq_xI x0 ->
       (match x0 with
        | Coq_xI x1 ->
          (match x1 with
           | Coq_xI _ -> None
           | Coq_xO _ -> Some X25
           | Coq_xH -> Some X17)
        | Coq_xO x1 ->
          (match x1 with
           | Coq_xI _ -> Some X29
           | Coq_xO _ -> Some X21
           | Coq_xH -> Some X13)
        | Coq_xH -> Some X9)
     | Coq_xO x0 ->
       (match x0 with
        | Coq_xI x1 ->
          (match x1 with
           | Coq_xI _ -> Some X31
           | Coq_xO _ -> Some X23
           | Coq_xH -> Some X15)
        | Coq_xO x1 ->
          (match x1 with
           | Coq_xI _ -> Some X27
           | Coq_xO _ -> Some X19
           | Coq_xH -> Some X11)
        | Coq_xH -> Some X7)
     | Coq_xH -> Some X5)
  | Coq_xO x ->
    (match x with
     | Coq_xI x0 ->
       (match x0 with
        | Coq_xI x1 ->
          (match x1 with
           | Coq_xI _ -> None
           | Coq_xO _ -> Some X24
           | Coq_xH -> Some X16)
        | Coq_xO x1 ->
          (match x1 with
           | Coq_xI _ -> Some X28
           | Coq_xO _ -> Some X20
           | Coq_xH -> Some X12)
        | Coq_xH -> Some X8)
     | Coq_xO x0 ->
       (match x0 with
        | Coq_xI x1 ->
          (match x1 with
           | Coq_xI _ -> Some X30
           | Coq_xO _ -> Some X22
           | Coq_xH -> Some X14)
        | Coq_xO x1 ->
          (match x1 with
           | Coq_xI _ -> Some X26
           | Coq_xO _ -> Some X18
           | Coq_xH -> Some X10)
        | Coq_xH -> Some X6)
     | Coq_xH -> Some SP)
  | Coq_xH -> Some RA

(** val register_induction :
    'a1 -> 'a1 -> 'a1 -> 'a1 -> 'a1 -> 'a1 -> 'a1 -> 'a1 -> 'a1 -> 'a1 -> 'a1
    -> 'a1 -> 'a1 -> 'a1 -> 'a1 -> 'a1 -> 'a1 -> 'a1 -> 'a1 -> 'a1 -> 'a1 ->
    'a1 -> 'a1 -> 'a1 -> 'a1 -> 'a1 -> 'a1 -> 'a1 -> 'a1 -> register ->
    is_register -> 'a1 **)

let register_induction his_RA his_SP his_X5 his_X6 his_X7 his_X8 his_X9 his_X10 his_X11 his_X12 his_X13 his_X14 his_X15 his_X16 his_X17 his_X18 his_X19 his_X20 his_X21 his_X22 his_X23 his_X24 his_X25 his_X26 his_X27 his_X28 his_X29 his_X30 his_X31 _ = function
| Coq_is_RA -> his_RA
| Coq_is_SP -> his_SP
| Coq_is_X5 -> his_X5
| Coq_is_X6 -> his_X6
| Coq_is_X7 -> his_X7
| Coq_is_X8 -> his_X8
| Coq_is_X9 -> his_X9
| Coq_is_X10 -> his_X10
| Coq_is_X11 -> his_X11
| Coq_is_X12 -> his_X12
| Coq_is_X13 -> his_X13
| Coq_is_X14 -> his_X14
| Coq_is_X15 -> his_X15
| Coq_is_X16 -> his_X16
| Coq_is_X17 -> his_X17
| Coq_is_X18 -> his_X18
| Coq_is_X19 -> his_X19
| Coq_is_X20 -> his_X20
| Coq_is_X21 -> his_X21
| Coq_is_X22 -> his_X22
| Coq_is_X23 -> his_X23
| Coq_is_X24 -> his_X24
| Coq_is_X25 -> his_X25
| Coq_is_X26 -> his_X26
| Coq_is_X27 -> his_X27
| Coq_is_X28 -> his_X28
| Coq_is_X29 -> his_X29
| Coq_is_X30 -> his_X30
| Coq_is_X31 -> his_X31

(** val register_eqb_fields :
    (register -> register -> bool) -> positive -> register_fields_t ->
    register_fields_t -> bool **)

let register_eqb_fields _ _ _ _ =
  true

(** val register_eqb : register -> register -> bool **)

let register_eqb x1 x2 =
  eqb_body register_tag register_fields
    (Obj.magic register_eqb_fields (fun _ _ -> true)) (register_tag x1)
    Box_register_RA x2

(** val register_eqb_OK : register -> register -> reflect **)

let register_eqb_OK =
  iffP2 register_eqb

(** val register_eqb_OK_sumbool : register -> register -> bool **)

let register_eqb_OK_sumbool =
  reflect_dec register_eqb register_eqb_OK

(** val eqTC_register : register eqTypeC **)

let eqTC_register =
  { beq = register_eqb; ceqP = register_eqb_OK }

(** val riscv_register_eqType : Equality.coq_type **)

let riscv_register_eqType =
  ceqT_eqType eqTC_register

(** val registers : register list **)

let registers =
  RA :: (SP :: (X5 :: (X6 :: (X7 :: (X8 :: (X9 :: (X10 :: (X11 :: (X12 :: (X13 :: (X14 :: (X15 :: (X16 :: (X17 :: (X18 :: (X19 :: (X20 :: (X21 :: (X22 :: (X23 :: (X24 :: (X25 :: (X26 :: (X27 :: (X28 :: (X29 :: (X30 :: (X31 :: []))))))))))))))))))))))))))))

(** val finTC_register : register finTypeC **)

let finTC_register =
  { _eqC = eqTC_register; cenum = registers }

(** val register_finType : Finite.coq_type **)

let register_finType =
  cfinT_finType finTC_register

(** val register_to_string : register -> string **)

let register_to_string = function
| RA -> "ra"
| SP -> "sp"
| X5 -> "x5"
| X6 -> "x6"
| X7 -> "x7"
| X8 -> "x8"
| X9 -> "x9"
| X10 -> "x10"
| X11 -> "x11"
| X12 -> "x12"
| X13 -> "x13"
| X14 -> "x14"
| X15 -> "x15"
| X16 -> "x16"
| X17 -> "x17"
| X18 -> "x18"
| X19 -> "x19"
| X20 -> "x20"
| X21 -> "x21"
| X22 -> "x22"
| X23 -> "x23"
| X24 -> "x24"
| X25 -> "x25"
| X26 -> "x26"
| X27 -> "x27"
| X28 -> "x28"
| X29 -> "x29"
| X30 -> "x30"
| X31 -> "x31"

(** val reg_toS : register coq_ToString **)

let reg_toS =
  { category = "register"; _finC = finTC_register; to_string =
    register_to_string }

type condition_kind =
| EQ
| NE
| LT of signedness
| GE of signedness

(** val condition_kind_rect :
    'a1 -> 'a1 -> (signedness -> 'a1) -> (signedness -> 'a1) ->
    condition_kind -> 'a1 **)

let condition_kind_rect f f0 f1 f2 = function
| EQ -> f
| NE -> f0
| LT s -> f1 s
| GE s -> f2 s

(** val condition_kind_rec :
    'a1 -> 'a1 -> (signedness -> 'a1) -> (signedness -> 'a1) ->
    condition_kind -> 'a1 **)

let condition_kind_rec f f0 f1 f2 = function
| EQ -> f
| NE -> f0
| LT s -> f1 s
| GE s -> f2 s

type is_condition_kind =
| Coq_is_EQ
| Coq_is_NE
| Coq_is_LT of signedness * is_signedness
| Coq_is_GE of signedness * is_signedness

(** val is_condition_kind_rect :
    'a1 -> 'a1 -> (signedness -> is_signedness -> 'a1) -> (signedness ->
    is_signedness -> 'a1) -> condition_kind -> is_condition_kind -> 'a1 **)

let is_condition_kind_rect f f0 f1 f2 _ = function
| Coq_is_EQ -> f
| Coq_is_NE -> f0
| Coq_is_LT (s, p_) -> f1 s p_
| Coq_is_GE (s, p_) -> f2 s p_

(** val is_condition_kind_rec :
    'a1 -> 'a1 -> (signedness -> is_signedness -> 'a1) -> (signedness ->
    is_signedness -> 'a1) -> condition_kind -> is_condition_kind -> 'a1 **)

let is_condition_kind_rec f f0 f1 f2 _ = function
| Coq_is_EQ -> f
| Coq_is_NE -> f0
| Coq_is_LT (s, p_) -> f1 s p_
| Coq_is_GE (s, p_) -> f2 s p_

(** val condition_kind_tag : condition_kind -> positive **)

let condition_kind_tag = function
| EQ -> Coq_xH
| NE -> Coq_xO Coq_xH
| LT _ -> Coq_xI Coq_xH
| GE _ -> Coq_xO (Coq_xO Coq_xH)

(** val is_condition_kind_inhab : condition_kind -> is_condition_kind **)

let is_condition_kind_inhab = function
| EQ -> Coq_is_EQ
| NE -> Coq_is_NE
| LT h -> Coq_is_LT (h, (is_signedness_inhab h))
| GE h -> Coq_is_GE (h, (is_signedness_inhab h))

(** val is_condition_kind_functor :
    condition_kind -> is_condition_kind -> is_condition_kind **)

let rec is_condition_kind_functor _ x =
  x

type box_condition_kind_EQ =
| Box_condition_kind_EQ

type box_condition_kind_LT =
  signedness
  (* singleton inductive, whose constructor was Box_condition_kind_LT *)

(** val coq_Box_condition_kind_LT_0 : box_condition_kind_LT -> signedness **)

let coq_Box_condition_kind_LT_0 record =
  record

type condition_kind_fields_t = __

(** val condition_kind_fields : condition_kind -> condition_kind_fields_t **)

let condition_kind_fields = function
| LT h -> Obj.magic h
| GE h -> Obj.magic h
| _ -> Obj.magic Box_condition_kind_EQ

(** val condition_kind_construct :
    positive -> condition_kind_fields_t -> condition_kind option **)

let condition_kind_construct p b =
  match p with
  | Coq_xI _ -> Some (LT (Obj.magic b))
  | Coq_xO x ->
    (match x with
     | Coq_xI _ -> None
     | Coq_xO _ -> Some (GE (Obj.magic b))
     | Coq_xH -> Some NE)
  | Coq_xH -> Some EQ

(** val condition_kind_induction :
    'a1 -> 'a1 -> (signedness -> is_signedness -> 'a1) -> (signedness ->
    is_signedness -> 'a1) -> condition_kind -> is_condition_kind -> 'a1 **)

let condition_kind_induction his_EQ his_NE his_LT his_GE _ = function
| Coq_is_EQ -> his_EQ
| Coq_is_NE -> his_NE
| Coq_is_LT (x0, p_) -> his_LT x0 p_
| Coq_is_GE (x0, p_) -> his_GE x0 p_

(** val condition_kind_eqb_fields :
    (condition_kind -> condition_kind -> bool) -> positive ->
    condition_kind_fields_t -> condition_kind_fields_t -> bool **)

let condition_kind_eqb_fields _ x a b =
  match x with
  | Coq_xI _ -> (&&) (signedness_eqb (Obj.magic a) (Obj.magic b)) true
  | Coq_xO x0 ->
    (match x0 with
     | Coq_xO _ -> (&&) (signedness_eqb (Obj.magic a) (Obj.magic b)) true
     | _ -> true)
  | Coq_xH -> true

(** val condition_kind_eqb : condition_kind -> condition_kind -> bool **)

let condition_kind_eqb x1 x2 =
  match x1 with
  | LT h ->
    eqb_body condition_kind_tag condition_kind_fields
      (Obj.magic condition_kind_eqb_fields (fun _ _ -> true))
      (condition_kind_tag (LT h)) h x2
  | GE h ->
    eqb_body condition_kind_tag condition_kind_fields
      (Obj.magic condition_kind_eqb_fields (fun _ _ -> true))
      (condition_kind_tag (GE h)) h x2
  | x ->
    eqb_body condition_kind_tag condition_kind_fields
      (Obj.magic condition_kind_eqb_fields (fun _ _ -> true))
      (condition_kind_tag x) Box_condition_kind_EQ x2

(** val condition_kind_eqb_OK :
    condition_kind -> condition_kind -> reflect **)

let condition_kind_eqb_OK =
  iffP2 condition_kind_eqb

(** val condition_kind_eqb_OK_sumbool :
    condition_kind -> condition_kind -> bool **)

let condition_kind_eqb_OK_sumbool =
  reflect_dec condition_kind_eqb condition_kind_eqb_OK

type condt = { cond_kind : condition_kind; cond_fst : register option;
               cond_snd : register option }

(** val cond_kind : condt -> condition_kind **)

let cond_kind record =
  record.cond_kind

(** val cond_fst : condt -> register option **)

let cond_fst record =
  record.cond_fst

(** val cond_snd : condt -> register option **)

let cond_snd record =
  record.cond_snd

type is_condt =
| Coq_is_Build_condt of condition_kind * is_condition_kind * register option
   * (register, is_register) Prelude.is_option * register option
   * (register, is_register) Prelude.is_option

(** val is_condt_rect :
    (condition_kind -> is_condition_kind -> register option -> (register,
    is_register) Prelude.is_option -> register option -> (register,
    is_register) Prelude.is_option -> 'a1) -> condt -> is_condt -> 'a1 **)

let is_condt_rect f _ = function
| Coq_is_Build_condt (cond_kind0, pcond_kind, cond_fst0, pcond_fst,
                      cond_snd0, pcond_snd) ->
  f cond_kind0 pcond_kind cond_fst0 pcond_fst cond_snd0 pcond_snd

(** val is_condt_rec :
    (condition_kind -> is_condition_kind -> register option -> (register,
    is_register) Prelude.is_option -> register option -> (register,
    is_register) Prelude.is_option -> 'a1) -> condt -> is_condt -> 'a1 **)

let is_condt_rec f _ = function
| Coq_is_Build_condt (cond_kind0, pcond_kind, cond_fst0, pcond_fst,
                      cond_snd0, pcond_snd) ->
  f cond_kind0 pcond_kind cond_fst0 pcond_fst cond_snd0 pcond_snd

(** val condt_tag : condt -> positive **)

let condt_tag _ =
  Coq_xH

(** val is_condt_inhab : condt -> is_condt **)

let is_condt_inhab x =
  let { cond_kind = cond_kind0; cond_fst = cond_fst0; cond_snd =
    cond_snd0 } = x
  in
  Coq_is_Build_condt (cond_kind0, (is_condition_kind_inhab cond_kind0),
  cond_fst0, (Prelude.is_option_inhab is_register_inhab cond_fst0),
  cond_snd0, (Prelude.is_option_inhab is_register_inhab cond_snd0))

(** val is_condt_functor : condt -> is_condt -> is_condt **)

let rec is_condt_functor _ x =
  x

type box_condt_Build_condt = { coq_Box_condt_Build_condt_0 : condition_kind;
                               coq_Box_condt_Build_condt_1 : register option;
                               coq_Box_condt_Build_condt_2 : register option }

(** val coq_Box_condt_Build_condt_0 :
    box_condt_Build_condt -> condition_kind **)

let coq_Box_condt_Build_condt_0 record =
  record.coq_Box_condt_Build_condt_0

(** val coq_Box_condt_Build_condt_1 :
    box_condt_Build_condt -> register option **)

let coq_Box_condt_Build_condt_1 record =
  record.coq_Box_condt_Build_condt_1

(** val coq_Box_condt_Build_condt_2 :
    box_condt_Build_condt -> register option **)

let coq_Box_condt_Build_condt_2 record =
  record.coq_Box_condt_Build_condt_2

type condt_fields_t = box_condt_Build_condt

(** val condt_fields : condt -> condt_fields_t **)

let condt_fields i =
  let { cond_kind = cond_kind0; cond_fst = cond_fst0; cond_snd =
    cond_snd0 } = i
  in
  { coq_Box_condt_Build_condt_0 = cond_kind0; coq_Box_condt_Build_condt_1 =
  cond_fst0; coq_Box_condt_Build_condt_2 = cond_snd0 }

(** val condt_construct :
    positive -> box_condt_Build_condt -> condt option **)

let condt_construct _ b =
  let { coq_Box_condt_Build_condt_0 = box_condt_Build_condt_0;
    coq_Box_condt_Build_condt_1 = box_condt_Build_condt_1;
    coq_Box_condt_Build_condt_2 = box_condt_Build_condt_2 } = b
  in
  Some { cond_kind = box_condt_Build_condt_0; cond_fst =
  box_condt_Build_condt_1; cond_snd = box_condt_Build_condt_2 }

(** val condt_induction :
    (condition_kind -> is_condition_kind -> register option -> (register,
    is_register) Prelude.is_option -> register option -> (register,
    is_register) Prelude.is_option -> 'a1) -> condt -> is_condt -> 'a1 **)

let condt_induction his_Build_condt _ = function
| Coq_is_Build_condt (cond_kind0, pcond_kind, cond_fst0, pcond_fst,
                      cond_snd0, pcond_snd) ->
  his_Build_condt cond_kind0 pcond_kind cond_fst0 pcond_fst cond_snd0
    pcond_snd

(** val condt_eqb_fields :
    (condt -> condt -> bool) -> positive -> box_condt_Build_condt ->
    box_condt_Build_condt -> bool **)

let condt_eqb_fields _ _ a b =
  let { coq_Box_condt_Build_condt_0 = box_condt_Build_condt_0;
    coq_Box_condt_Build_condt_1 = box_condt_Build_condt_1;
    coq_Box_condt_Build_condt_2 = box_condt_Build_condt_2 } = a
  in
  let { coq_Box_condt_Build_condt_0 = box_condt_Build_condt_3;
    coq_Box_condt_Build_condt_1 = box_condt_Build_condt_4;
    coq_Box_condt_Build_condt_2 = box_condt_Build_condt_5 } = b
  in
  (&&) (condition_kind_eqb box_condt_Build_condt_0 box_condt_Build_condt_3)
    ((&&)
      (Prelude.option_eqb register_eqb box_condt_Build_condt_1
        box_condt_Build_condt_4)
      ((&&)
        (Prelude.option_eqb register_eqb box_condt_Build_condt_2
          box_condt_Build_condt_5) true))

(** val condt_eqb : condt -> condt -> bool **)

let condt_eqb x1 x2 =
  let { cond_kind = cond_kind0; cond_fst = cond_fst0; cond_snd =
    cond_snd0 } = x1
  in
  eqb_body condt_tag condt_fields (condt_eqb_fields (fun _ _ -> true))
    (condt_tag { cond_kind = cond_kind0; cond_fst = cond_fst0; cond_snd =
      cond_snd0 }) { coq_Box_condt_Build_condt_0 = cond_kind0;
    coq_Box_condt_Build_condt_1 = cond_fst0; coq_Box_condt_Build_condt_2 =
    cond_snd0 } x2

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

(** val riscv_fc_of_cfc : combine_flags_core -> flag_combination **)

let riscv_fc_of_cfc _ =
  FCVar0

(** val riscv_fcp : coq_FlagCombinationParams **)

let riscv_fcp =
  riscv_fc_of_cfc

(** val riscv_check_CAimm :
    caimm_checker_s -> wsize -> GRing.ComRing.sort -> bool **)

let riscv_check_CAimm checker ws w =
  match checker with
  | CAimmC_none -> true
  | CAimmC_riscv_12bits_signed ->
    let i = wsigned ws w in
    (&&)
      (Z.leb (Zneg (Coq_xO (Coq_xO (Coq_xO (Coq_xO (Coq_xO (Coq_xO (Coq_xO
        (Coq_xO (Coq_xO (Coq_xO (Coq_xO Coq_xH)))))))))))) i)
      (Z.leb i (Zpos (Coq_xI (Coq_xI (Coq_xI (Coq_xI (Coq_xI (Coq_xI (Coq_xI
        (Coq_xI (Coq_xI (Coq_xI Coq_xH))))))))))))
  | CAimmC_riscv_5bits_unsigned ->
    let i = wunsigned ws w in
    Z.leb i (Zpos (Coq_xI (Coq_xI (Coq_xI (Coq_xI Coq_xH)))))
  | _ -> false

(** val riscv_decl : (register, empty, empty, empty, condt) arch_decl **)

let riscv_decl =
  { reg_size = riscv_reg_size; xreg_size = riscv_xreg_size; cond_eqC =
    eqTC_condt; toS_r = reg_toS; toS_rx = (empty_toS (Coq_lword U32));
    toS_x = (empty_toS (Coq_lword U64)); toS_f = (empty_toS Coq_lbool);
    ad_rsp = SP; ad_fcp = riscv_fcp; check_CAimm = riscv_check_CAimm }

(** val riscv_linux_call_conv :
    (register, empty, empty, empty, condt) calling_convention **)

let riscv_linux_call_conv =
  { callee_saved =
    (map (fun x -> ARReg x)
      (SP :: (X8 :: (X9 :: (X18 :: (X19 :: (X20 :: (X21 :: (X22 :: (X23 :: (X24 :: (X25 :: (X26 :: (X27 :: []))))))))))))));
    call_reg_args =
    (X10 :: (X11 :: (X12 :: (X13 :: (X14 :: (X15 :: (X16 :: (X17 :: []))))))));
    call_xreg_args = []; call_reg_ret = (X10 :: (X11 :: [])); call_xreg_ret =
    [] }

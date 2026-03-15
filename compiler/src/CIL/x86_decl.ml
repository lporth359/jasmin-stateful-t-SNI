open BinNums
open Bool
open Arch_decl
open EqbOK
open Eqb_core_defs
open Eqtype
open Fintype
open Flag_combination
open Seq
open Ssralg
open Utils0
open Wsize

type __ = Obj.t

(** val x86_reg_size : wsize **)

let x86_reg_size =
  U64

(** val x86_xreg_size : wsize **)

let x86_xreg_size =
  U256

type register =
| RAX
| RCX
| RDX
| RBX
| RSP
| RBP
| RSI
| RDI
| R8
| R9
| R10
| R11
| R12
| R13
| R14
| R15

(** val register_rect :
    'a1 -> 'a1 -> 'a1 -> 'a1 -> 'a1 -> 'a1 -> 'a1 -> 'a1 -> 'a1 -> 'a1 -> 'a1
    -> 'a1 -> 'a1 -> 'a1 -> 'a1 -> 'a1 -> register -> 'a1 **)

let register_rect f f0 f1 f2 f3 f4 f5 f6 f7 f8 f9 f10 f11 f12 f13 f14 = function
| RAX -> f
| RCX -> f0
| RDX -> f1
| RBX -> f2
| RSP -> f3
| RBP -> f4
| RSI -> f5
| RDI -> f6
| R8 -> f7
| R9 -> f8
| R10 -> f9
| R11 -> f10
| R12 -> f11
| R13 -> f12
| R14 -> f13
| R15 -> f14

(** val register_rec :
    'a1 -> 'a1 -> 'a1 -> 'a1 -> 'a1 -> 'a1 -> 'a1 -> 'a1 -> 'a1 -> 'a1 -> 'a1
    -> 'a1 -> 'a1 -> 'a1 -> 'a1 -> 'a1 -> register -> 'a1 **)

let register_rec f f0 f1 f2 f3 f4 f5 f6 f7 f8 f9 f10 f11 f12 f13 f14 = function
| RAX -> f
| RCX -> f0
| RDX -> f1
| RBX -> f2
| RSP -> f3
| RBP -> f4
| RSI -> f5
| RDI -> f6
| R8 -> f7
| R9 -> f8
| R10 -> f9
| R11 -> f10
| R12 -> f11
| R13 -> f12
| R14 -> f13
| R15 -> f14

type is_register =
| Coq_is_RAX
| Coq_is_RCX
| Coq_is_RDX
| Coq_is_RBX
| Coq_is_RSP
| Coq_is_RBP
| Coq_is_RSI
| Coq_is_RDI
| Coq_is_R8
| Coq_is_R9
| Coq_is_R10
| Coq_is_R11
| Coq_is_R12
| Coq_is_R13
| Coq_is_R14
| Coq_is_R15

(** val is_register_rect :
    'a1 -> 'a1 -> 'a1 -> 'a1 -> 'a1 -> 'a1 -> 'a1 -> 'a1 -> 'a1 -> 'a1 -> 'a1
    -> 'a1 -> 'a1 -> 'a1 -> 'a1 -> 'a1 -> register -> is_register -> 'a1 **)

let is_register_rect f f0 f1 f2 f3 f4 f5 f6 f7 f8 f9 f10 f11 f12 f13 f14 _ = function
| Coq_is_RAX -> f
| Coq_is_RCX -> f0
| Coq_is_RDX -> f1
| Coq_is_RBX -> f2
| Coq_is_RSP -> f3
| Coq_is_RBP -> f4
| Coq_is_RSI -> f5
| Coq_is_RDI -> f6
| Coq_is_R8 -> f7
| Coq_is_R9 -> f8
| Coq_is_R10 -> f9
| Coq_is_R11 -> f10
| Coq_is_R12 -> f11
| Coq_is_R13 -> f12
| Coq_is_R14 -> f13
| Coq_is_R15 -> f14

(** val is_register_rec :
    'a1 -> 'a1 -> 'a1 -> 'a1 -> 'a1 -> 'a1 -> 'a1 -> 'a1 -> 'a1 -> 'a1 -> 'a1
    -> 'a1 -> 'a1 -> 'a1 -> 'a1 -> 'a1 -> register -> is_register -> 'a1 **)

let is_register_rec f f0 f1 f2 f3 f4 f5 f6 f7 f8 f9 f10 f11 f12 f13 f14 _ = function
| Coq_is_RAX -> f
| Coq_is_RCX -> f0
| Coq_is_RDX -> f1
| Coq_is_RBX -> f2
| Coq_is_RSP -> f3
| Coq_is_RBP -> f4
| Coq_is_RSI -> f5
| Coq_is_RDI -> f6
| Coq_is_R8 -> f7
| Coq_is_R9 -> f8
| Coq_is_R10 -> f9
| Coq_is_R11 -> f10
| Coq_is_R12 -> f11
| Coq_is_R13 -> f12
| Coq_is_R14 -> f13
| Coq_is_R15 -> f14

(** val register_tag : register -> positive **)

let register_tag = function
| RAX -> Coq_xH
| RCX -> Coq_xO Coq_xH
| RDX -> Coq_xI Coq_xH
| RBX -> Coq_xO (Coq_xO Coq_xH)
| RSP -> Coq_xI (Coq_xO Coq_xH)
| RBP -> Coq_xO (Coq_xI Coq_xH)
| RSI -> Coq_xI (Coq_xI Coq_xH)
| RDI -> Coq_xO (Coq_xO (Coq_xO Coq_xH))
| R8 -> Coq_xI (Coq_xO (Coq_xO Coq_xH))
| R9 -> Coq_xO (Coq_xI (Coq_xO Coq_xH))
| R10 -> Coq_xI (Coq_xI (Coq_xO Coq_xH))
| R11 -> Coq_xO (Coq_xO (Coq_xI Coq_xH))
| R12 -> Coq_xI (Coq_xO (Coq_xI Coq_xH))
| R13 -> Coq_xO (Coq_xI (Coq_xI Coq_xH))
| R14 -> Coq_xI (Coq_xI (Coq_xI Coq_xH))
| R15 -> Coq_xO (Coq_xO (Coq_xO (Coq_xO Coq_xH)))

(** val is_register_inhab : register -> is_register **)

let is_register_inhab = function
| RAX -> Coq_is_RAX
| RCX -> Coq_is_RCX
| RDX -> Coq_is_RDX
| RBX -> Coq_is_RBX
| RSP -> Coq_is_RSP
| RBP -> Coq_is_RBP
| RSI -> Coq_is_RSI
| RDI -> Coq_is_RDI
| R8 -> Coq_is_R8
| R9 -> Coq_is_R9
| R10 -> Coq_is_R10
| R11 -> Coq_is_R11
| R12 -> Coq_is_R12
| R13 -> Coq_is_R13
| R14 -> Coq_is_R14
| R15 -> Coq_is_R15

(** val is_register_functor : register -> is_register -> is_register **)

let rec is_register_functor _ x =
  x

type box_register_RAX =
| Box_register_RAX

type register_fields_t = __

(** val register_fields : register -> register_fields_t **)

let register_fields _ =
  Obj.magic Box_register_RAX

(** val register_construct :
    positive -> register_fields_t -> register option **)

let register_construct p _ =
  match p with
  | Coq_xI x ->
    (match x with
     | Coq_xI x0 ->
       (match x0 with
        | Coq_xI _ -> Some R14
        | Coq_xO _ -> Some R10
        | Coq_xH -> Some RSI)
     | Coq_xO x0 ->
       (match x0 with
        | Coq_xI _ -> Some R12
        | Coq_xO _ -> Some R8
        | Coq_xH -> Some RSP)
     | Coq_xH -> Some RDX)
  | Coq_xO x ->
    (match x with
     | Coq_xI x0 ->
       (match x0 with
        | Coq_xI _ -> Some R13
        | Coq_xO _ -> Some R9
        | Coq_xH -> Some RBP)
     | Coq_xO x0 ->
       (match x0 with
        | Coq_xI _ -> Some R11
        | Coq_xO x1 ->
          (match x1 with
           | Coq_xI _ -> None
           | Coq_xO _ -> Some R15
           | Coq_xH -> Some RDI)
        | Coq_xH -> Some RBX)
     | Coq_xH -> Some RCX)
  | Coq_xH -> Some RAX

(** val register_induction :
    'a1 -> 'a1 -> 'a1 -> 'a1 -> 'a1 -> 'a1 -> 'a1 -> 'a1 -> 'a1 -> 'a1 -> 'a1
    -> 'a1 -> 'a1 -> 'a1 -> 'a1 -> 'a1 -> register -> is_register -> 'a1 **)

let register_induction his_RAX his_RCX his_RDX his_RBX his_RSP his_RBP his_RSI his_RDI his_R8 his_R9 his_R10 his_R11 his_R12 his_R13 his_R14 his_R15 _ = function
| Coq_is_RAX -> his_RAX
| Coq_is_RCX -> his_RCX
| Coq_is_RDX -> his_RDX
| Coq_is_RBX -> his_RBX
| Coq_is_RSP -> his_RSP
| Coq_is_RBP -> his_RBP
| Coq_is_RSI -> his_RSI
| Coq_is_RDI -> his_RDI
| Coq_is_R8 -> his_R8
| Coq_is_R9 -> his_R9
| Coq_is_R10 -> his_R10
| Coq_is_R11 -> his_R11
| Coq_is_R12 -> his_R12
| Coq_is_R13 -> his_R13
| Coq_is_R14 -> his_R14
| Coq_is_R15 -> his_R15

(** val register_eqb_fields :
    (register -> register -> bool) -> positive -> register_fields_t ->
    register_fields_t -> bool **)

let register_eqb_fields _ _ _ _ =
  true

(** val register_eqb : register -> register -> bool **)

let register_eqb x1 x2 =
  eqb_body register_tag register_fields
    (Obj.magic register_eqb_fields (fun _ _ -> true)) (register_tag x1)
    Box_register_RAX x2

(** val register_eqb_OK : register -> register -> reflect **)

let register_eqb_OK =
  iffP2 register_eqb

(** val register_eqb_OK_sumbool : register -> register -> bool **)

let register_eqb_OK_sumbool =
  reflect_dec register_eqb register_eqb_OK

type register_ext =
| MM0
| MM1
| MM2
| MM3
| MM4
| MM5
| MM6
| MM7

(** val register_ext_rect :
    'a1 -> 'a1 -> 'a1 -> 'a1 -> 'a1 -> 'a1 -> 'a1 -> 'a1 -> register_ext ->
    'a1 **)

let register_ext_rect f f0 f1 f2 f3 f4 f5 f6 = function
| MM0 -> f
| MM1 -> f0
| MM2 -> f1
| MM3 -> f2
| MM4 -> f3
| MM5 -> f4
| MM6 -> f5
| MM7 -> f6

(** val register_ext_rec :
    'a1 -> 'a1 -> 'a1 -> 'a1 -> 'a1 -> 'a1 -> 'a1 -> 'a1 -> register_ext ->
    'a1 **)

let register_ext_rec f f0 f1 f2 f3 f4 f5 f6 = function
| MM0 -> f
| MM1 -> f0
| MM2 -> f1
| MM3 -> f2
| MM4 -> f3
| MM5 -> f4
| MM6 -> f5
| MM7 -> f6

type is_register_ext =
| Coq_is_MM0
| Coq_is_MM1
| Coq_is_MM2
| Coq_is_MM3
| Coq_is_MM4
| Coq_is_MM5
| Coq_is_MM6
| Coq_is_MM7

(** val is_register_ext_rect :
    'a1 -> 'a1 -> 'a1 -> 'a1 -> 'a1 -> 'a1 -> 'a1 -> 'a1 -> register_ext ->
    is_register_ext -> 'a1 **)

let is_register_ext_rect f f0 f1 f2 f3 f4 f5 f6 _ = function
| Coq_is_MM0 -> f
| Coq_is_MM1 -> f0
| Coq_is_MM2 -> f1
| Coq_is_MM3 -> f2
| Coq_is_MM4 -> f3
| Coq_is_MM5 -> f4
| Coq_is_MM6 -> f5
| Coq_is_MM7 -> f6

(** val is_register_ext_rec :
    'a1 -> 'a1 -> 'a1 -> 'a1 -> 'a1 -> 'a1 -> 'a1 -> 'a1 -> register_ext ->
    is_register_ext -> 'a1 **)

let is_register_ext_rec f f0 f1 f2 f3 f4 f5 f6 _ = function
| Coq_is_MM0 -> f
| Coq_is_MM1 -> f0
| Coq_is_MM2 -> f1
| Coq_is_MM3 -> f2
| Coq_is_MM4 -> f3
| Coq_is_MM5 -> f4
| Coq_is_MM6 -> f5
| Coq_is_MM7 -> f6

(** val register_ext_tag : register_ext -> positive **)

let register_ext_tag = function
| MM0 -> Coq_xH
| MM1 -> Coq_xO Coq_xH
| MM2 -> Coq_xI Coq_xH
| MM3 -> Coq_xO (Coq_xO Coq_xH)
| MM4 -> Coq_xI (Coq_xO Coq_xH)
| MM5 -> Coq_xO (Coq_xI Coq_xH)
| MM6 -> Coq_xI (Coq_xI Coq_xH)
| MM7 -> Coq_xO (Coq_xO (Coq_xO Coq_xH))

(** val is_register_ext_inhab : register_ext -> is_register_ext **)

let is_register_ext_inhab = function
| MM0 -> Coq_is_MM0
| MM1 -> Coq_is_MM1
| MM2 -> Coq_is_MM2
| MM3 -> Coq_is_MM3
| MM4 -> Coq_is_MM4
| MM5 -> Coq_is_MM5
| MM6 -> Coq_is_MM6
| MM7 -> Coq_is_MM7

(** val is_register_ext_functor :
    register_ext -> is_register_ext -> is_register_ext **)

let rec is_register_ext_functor _ x =
  x

type box_register_ext_MM0 =
| Box_register_ext_MM0

type register_ext_fields_t = __

(** val register_ext_fields : register_ext -> register_ext_fields_t **)

let register_ext_fields _ =
  Obj.magic Box_register_ext_MM0

(** val register_ext_construct :
    positive -> register_ext_fields_t -> register_ext option **)

let register_ext_construct p _ =
  match p with
  | Coq_xI x ->
    (match x with
     | Coq_xI _ -> Some MM6
     | Coq_xO _ -> Some MM4
     | Coq_xH -> Some MM2)
  | Coq_xO x ->
    (match x with
     | Coq_xI _ -> Some MM5
     | Coq_xO x0 ->
       (match x0 with
        | Coq_xI _ -> None
        | Coq_xO _ -> Some MM7
        | Coq_xH -> Some MM3)
     | Coq_xH -> Some MM1)
  | Coq_xH -> Some MM0

(** val register_ext_induction :
    'a1 -> 'a1 -> 'a1 -> 'a1 -> 'a1 -> 'a1 -> 'a1 -> 'a1 -> register_ext ->
    is_register_ext -> 'a1 **)

let register_ext_induction his_MM0 his_MM1 his_MM2 his_MM3 his_MM4 his_MM5 his_MM6 his_MM7 _ = function
| Coq_is_MM0 -> his_MM0
| Coq_is_MM1 -> his_MM1
| Coq_is_MM2 -> his_MM2
| Coq_is_MM3 -> his_MM3
| Coq_is_MM4 -> his_MM4
| Coq_is_MM5 -> his_MM5
| Coq_is_MM6 -> his_MM6
| Coq_is_MM7 -> his_MM7

(** val register_ext_eqb_fields :
    (register_ext -> register_ext -> bool) -> positive ->
    register_ext_fields_t -> register_ext_fields_t -> bool **)

let register_ext_eqb_fields _ _ _ _ =
  true

(** val register_ext_eqb : register_ext -> register_ext -> bool **)

let register_ext_eqb x1 x2 =
  eqb_body register_ext_tag register_ext_fields
    (Obj.magic register_ext_eqb_fields (fun _ _ -> true))
    (register_ext_tag x1) Box_register_ext_MM0 x2

(** val register_ext_eqb_OK : register_ext -> register_ext -> reflect **)

let register_ext_eqb_OK =
  iffP2 register_ext_eqb

(** val register_ext_eqb_OK_sumbool : register_ext -> register_ext -> bool **)

let register_ext_eqb_OK_sumbool =
  reflect_dec register_ext_eqb register_ext_eqb_OK

type xmm_register =
| XMM0
| XMM1
| XMM2
| XMM3
| XMM4
| XMM5
| XMM6
| XMM7
| XMM8
| XMM9
| XMM10
| XMM11
| XMM12
| XMM13
| XMM14
| XMM15

(** val xmm_register_rect :
    'a1 -> 'a1 -> 'a1 -> 'a1 -> 'a1 -> 'a1 -> 'a1 -> 'a1 -> 'a1 -> 'a1 -> 'a1
    -> 'a1 -> 'a1 -> 'a1 -> 'a1 -> 'a1 -> xmm_register -> 'a1 **)

let xmm_register_rect f f0 f1 f2 f3 f4 f5 f6 f7 f8 f9 f10 f11 f12 f13 f14 = function
| XMM0 -> f
| XMM1 -> f0
| XMM2 -> f1
| XMM3 -> f2
| XMM4 -> f3
| XMM5 -> f4
| XMM6 -> f5
| XMM7 -> f6
| XMM8 -> f7
| XMM9 -> f8
| XMM10 -> f9
| XMM11 -> f10
| XMM12 -> f11
| XMM13 -> f12
| XMM14 -> f13
| XMM15 -> f14

(** val xmm_register_rec :
    'a1 -> 'a1 -> 'a1 -> 'a1 -> 'a1 -> 'a1 -> 'a1 -> 'a1 -> 'a1 -> 'a1 -> 'a1
    -> 'a1 -> 'a1 -> 'a1 -> 'a1 -> 'a1 -> xmm_register -> 'a1 **)

let xmm_register_rec f f0 f1 f2 f3 f4 f5 f6 f7 f8 f9 f10 f11 f12 f13 f14 = function
| XMM0 -> f
| XMM1 -> f0
| XMM2 -> f1
| XMM3 -> f2
| XMM4 -> f3
| XMM5 -> f4
| XMM6 -> f5
| XMM7 -> f6
| XMM8 -> f7
| XMM9 -> f8
| XMM10 -> f9
| XMM11 -> f10
| XMM12 -> f11
| XMM13 -> f12
| XMM14 -> f13
| XMM15 -> f14

type is_xmm_register =
| Coq_is_XMM0
| Coq_is_XMM1
| Coq_is_XMM2
| Coq_is_XMM3
| Coq_is_XMM4
| Coq_is_XMM5
| Coq_is_XMM6
| Coq_is_XMM7
| Coq_is_XMM8
| Coq_is_XMM9
| Coq_is_XMM10
| Coq_is_XMM11
| Coq_is_XMM12
| Coq_is_XMM13
| Coq_is_XMM14
| Coq_is_XMM15

(** val is_xmm_register_rect :
    'a1 -> 'a1 -> 'a1 -> 'a1 -> 'a1 -> 'a1 -> 'a1 -> 'a1 -> 'a1 -> 'a1 -> 'a1
    -> 'a1 -> 'a1 -> 'a1 -> 'a1 -> 'a1 -> xmm_register -> is_xmm_register ->
    'a1 **)

let is_xmm_register_rect f f0 f1 f2 f3 f4 f5 f6 f7 f8 f9 f10 f11 f12 f13 f14 _ = function
| Coq_is_XMM0 -> f
| Coq_is_XMM1 -> f0
| Coq_is_XMM2 -> f1
| Coq_is_XMM3 -> f2
| Coq_is_XMM4 -> f3
| Coq_is_XMM5 -> f4
| Coq_is_XMM6 -> f5
| Coq_is_XMM7 -> f6
| Coq_is_XMM8 -> f7
| Coq_is_XMM9 -> f8
| Coq_is_XMM10 -> f9
| Coq_is_XMM11 -> f10
| Coq_is_XMM12 -> f11
| Coq_is_XMM13 -> f12
| Coq_is_XMM14 -> f13
| Coq_is_XMM15 -> f14

(** val is_xmm_register_rec :
    'a1 -> 'a1 -> 'a1 -> 'a1 -> 'a1 -> 'a1 -> 'a1 -> 'a1 -> 'a1 -> 'a1 -> 'a1
    -> 'a1 -> 'a1 -> 'a1 -> 'a1 -> 'a1 -> xmm_register -> is_xmm_register ->
    'a1 **)

let is_xmm_register_rec f f0 f1 f2 f3 f4 f5 f6 f7 f8 f9 f10 f11 f12 f13 f14 _ = function
| Coq_is_XMM0 -> f
| Coq_is_XMM1 -> f0
| Coq_is_XMM2 -> f1
| Coq_is_XMM3 -> f2
| Coq_is_XMM4 -> f3
| Coq_is_XMM5 -> f4
| Coq_is_XMM6 -> f5
| Coq_is_XMM7 -> f6
| Coq_is_XMM8 -> f7
| Coq_is_XMM9 -> f8
| Coq_is_XMM10 -> f9
| Coq_is_XMM11 -> f10
| Coq_is_XMM12 -> f11
| Coq_is_XMM13 -> f12
| Coq_is_XMM14 -> f13
| Coq_is_XMM15 -> f14

(** val xmm_register_tag : xmm_register -> positive **)

let xmm_register_tag = function
| XMM0 -> Coq_xH
| XMM1 -> Coq_xO Coq_xH
| XMM2 -> Coq_xI Coq_xH
| XMM3 -> Coq_xO (Coq_xO Coq_xH)
| XMM4 -> Coq_xI (Coq_xO Coq_xH)
| XMM5 -> Coq_xO (Coq_xI Coq_xH)
| XMM6 -> Coq_xI (Coq_xI Coq_xH)
| XMM7 -> Coq_xO (Coq_xO (Coq_xO Coq_xH))
| XMM8 -> Coq_xI (Coq_xO (Coq_xO Coq_xH))
| XMM9 -> Coq_xO (Coq_xI (Coq_xO Coq_xH))
| XMM10 -> Coq_xI (Coq_xI (Coq_xO Coq_xH))
| XMM11 -> Coq_xO (Coq_xO (Coq_xI Coq_xH))
| XMM12 -> Coq_xI (Coq_xO (Coq_xI Coq_xH))
| XMM13 -> Coq_xO (Coq_xI (Coq_xI Coq_xH))
| XMM14 -> Coq_xI (Coq_xI (Coq_xI Coq_xH))
| XMM15 -> Coq_xO (Coq_xO (Coq_xO (Coq_xO Coq_xH)))

(** val is_xmm_register_inhab : xmm_register -> is_xmm_register **)

let is_xmm_register_inhab = function
| XMM0 -> Coq_is_XMM0
| XMM1 -> Coq_is_XMM1
| XMM2 -> Coq_is_XMM2
| XMM3 -> Coq_is_XMM3
| XMM4 -> Coq_is_XMM4
| XMM5 -> Coq_is_XMM5
| XMM6 -> Coq_is_XMM6
| XMM7 -> Coq_is_XMM7
| XMM8 -> Coq_is_XMM8
| XMM9 -> Coq_is_XMM9
| XMM10 -> Coq_is_XMM10
| XMM11 -> Coq_is_XMM11
| XMM12 -> Coq_is_XMM12
| XMM13 -> Coq_is_XMM13
| XMM14 -> Coq_is_XMM14
| XMM15 -> Coq_is_XMM15

(** val is_xmm_register_functor :
    xmm_register -> is_xmm_register -> is_xmm_register **)

let rec is_xmm_register_functor _ x =
  x

type box_xmm_register_XMM0 =
| Box_xmm_register_XMM0

type xmm_register_fields_t = __

(** val xmm_register_fields : xmm_register -> xmm_register_fields_t **)

let xmm_register_fields _ =
  Obj.magic Box_xmm_register_XMM0

(** val xmm_register_construct :
    positive -> xmm_register_fields_t -> xmm_register option **)

let xmm_register_construct p _ =
  match p with
  | Coq_xI x ->
    (match x with
     | Coq_xI x0 ->
       (match x0 with
        | Coq_xI _ -> Some XMM14
        | Coq_xO _ -> Some XMM10
        | Coq_xH -> Some XMM6)
     | Coq_xO x0 ->
       (match x0 with
        | Coq_xI _ -> Some XMM12
        | Coq_xO _ -> Some XMM8
        | Coq_xH -> Some XMM4)
     | Coq_xH -> Some XMM2)
  | Coq_xO x ->
    (match x with
     | Coq_xI x0 ->
       (match x0 with
        | Coq_xI _ -> Some XMM13
        | Coq_xO _ -> Some XMM9
        | Coq_xH -> Some XMM5)
     | Coq_xO x0 ->
       (match x0 with
        | Coq_xI _ -> Some XMM11
        | Coq_xO x1 ->
          (match x1 with
           | Coq_xI _ -> None
           | Coq_xO _ -> Some XMM15
           | Coq_xH -> Some XMM7)
        | Coq_xH -> Some XMM3)
     | Coq_xH -> Some XMM1)
  | Coq_xH -> Some XMM0

(** val xmm_register_induction :
    'a1 -> 'a1 -> 'a1 -> 'a1 -> 'a1 -> 'a1 -> 'a1 -> 'a1 -> 'a1 -> 'a1 -> 'a1
    -> 'a1 -> 'a1 -> 'a1 -> 'a1 -> 'a1 -> xmm_register -> is_xmm_register ->
    'a1 **)

let xmm_register_induction his_XMM0 his_XMM1 his_XMM2 his_XMM3 his_XMM4 his_XMM5 his_XMM6 his_XMM7 his_XMM8 his_XMM9 his_XMM10 his_XMM11 his_XMM12 his_XMM13 his_XMM14 his_XMM15 _ = function
| Coq_is_XMM0 -> his_XMM0
| Coq_is_XMM1 -> his_XMM1
| Coq_is_XMM2 -> his_XMM2
| Coq_is_XMM3 -> his_XMM3
| Coq_is_XMM4 -> his_XMM4
| Coq_is_XMM5 -> his_XMM5
| Coq_is_XMM6 -> his_XMM6
| Coq_is_XMM7 -> his_XMM7
| Coq_is_XMM8 -> his_XMM8
| Coq_is_XMM9 -> his_XMM9
| Coq_is_XMM10 -> his_XMM10
| Coq_is_XMM11 -> his_XMM11
| Coq_is_XMM12 -> his_XMM12
| Coq_is_XMM13 -> his_XMM13
| Coq_is_XMM14 -> his_XMM14
| Coq_is_XMM15 -> his_XMM15

(** val xmm_register_eqb_fields :
    (xmm_register -> xmm_register -> bool) -> positive ->
    xmm_register_fields_t -> xmm_register_fields_t -> bool **)

let xmm_register_eqb_fields _ _ _ _ =
  true

(** val xmm_register_eqb : xmm_register -> xmm_register -> bool **)

let xmm_register_eqb x1 x2 =
  eqb_body xmm_register_tag xmm_register_fields
    (Obj.magic xmm_register_eqb_fields (fun _ _ -> true))
    (xmm_register_tag x1) Box_xmm_register_XMM0 x2

(** val xmm_register_eqb_OK : xmm_register -> xmm_register -> reflect **)

let xmm_register_eqb_OK =
  iffP2 xmm_register_eqb

(** val xmm_register_eqb_OK_sumbool : xmm_register -> xmm_register -> bool **)

let xmm_register_eqb_OK_sumbool =
  reflect_dec xmm_register_eqb xmm_register_eqb_OK

type rflag =
| CF
| PF
| ZF
| SF
| OF

(** val rflag_rect : 'a1 -> 'a1 -> 'a1 -> 'a1 -> 'a1 -> rflag -> 'a1 **)

let rflag_rect f f0 f1 f2 f3 = function
| CF -> f
| PF -> f0
| ZF -> f1
| SF -> f2
| OF -> f3

(** val rflag_rec : 'a1 -> 'a1 -> 'a1 -> 'a1 -> 'a1 -> rflag -> 'a1 **)

let rflag_rec f f0 f1 f2 f3 = function
| CF -> f
| PF -> f0
| ZF -> f1
| SF -> f2
| OF -> f3

type is_rflag =
| Coq_is_CF
| Coq_is_PF
| Coq_is_ZF
| Coq_is_SF
| Coq_is_OF

(** val is_rflag_rect :
    'a1 -> 'a1 -> 'a1 -> 'a1 -> 'a1 -> rflag -> is_rflag -> 'a1 **)

let is_rflag_rect f f0 f1 f2 f3 _ = function
| Coq_is_CF -> f
| Coq_is_PF -> f0
| Coq_is_ZF -> f1
| Coq_is_SF -> f2
| Coq_is_OF -> f3

(** val is_rflag_rec :
    'a1 -> 'a1 -> 'a1 -> 'a1 -> 'a1 -> rflag -> is_rflag -> 'a1 **)

let is_rflag_rec f f0 f1 f2 f3 _ = function
| Coq_is_CF -> f
| Coq_is_PF -> f0
| Coq_is_ZF -> f1
| Coq_is_SF -> f2
| Coq_is_OF -> f3

(** val rflag_tag : rflag -> positive **)

let rflag_tag = function
| CF -> Coq_xH
| PF -> Coq_xO Coq_xH
| ZF -> Coq_xI Coq_xH
| SF -> Coq_xO (Coq_xO Coq_xH)
| OF -> Coq_xI (Coq_xO Coq_xH)

(** val is_rflag_inhab : rflag -> is_rflag **)

let is_rflag_inhab = function
| CF -> Coq_is_CF
| PF -> Coq_is_PF
| ZF -> Coq_is_ZF
| SF -> Coq_is_SF
| OF -> Coq_is_OF

(** val is_rflag_functor : rflag -> is_rflag -> is_rflag **)

let rec is_rflag_functor _ x =
  x

type box_rflag_CF =
| Box_rflag_CF

type rflag_fields_t = __

(** val rflag_fields : rflag -> rflag_fields_t **)

let rflag_fields _ =
  Obj.magic Box_rflag_CF

(** val rflag_construct : positive -> rflag_fields_t -> rflag option **)

let rflag_construct p _ =
  match p with
  | Coq_xI x ->
    (match x with
     | Coq_xI _ -> None
     | Coq_xO _ -> Some OF
     | Coq_xH -> Some ZF)
  | Coq_xO x ->
    (match x with
     | Coq_xI _ -> None
     | Coq_xO _ -> Some SF
     | Coq_xH -> Some PF)
  | Coq_xH -> Some CF

(** val rflag_induction :
    'a1 -> 'a1 -> 'a1 -> 'a1 -> 'a1 -> rflag -> is_rflag -> 'a1 **)

let rflag_induction his_CF his_PF his_ZF his_SF his_OF _ = function
| Coq_is_CF -> his_CF
| Coq_is_PF -> his_PF
| Coq_is_ZF -> his_ZF
| Coq_is_SF -> his_SF
| Coq_is_OF -> his_OF

(** val rflag_eqb_fields :
    (rflag -> rflag -> bool) -> positive -> rflag_fields_t -> rflag_fields_t
    -> bool **)

let rflag_eqb_fields _ _ _ _ =
  true

(** val rflag_eqb : rflag -> rflag -> bool **)

let rflag_eqb x1 x2 =
  eqb_body rflag_tag rflag_fields
    (Obj.magic rflag_eqb_fields (fun _ _ -> true)) (rflag_tag x1)
    Box_rflag_CF x2

(** val rflag_eqb_OK : rflag -> rflag -> reflect **)

let rflag_eqb_OK =
  iffP2 rflag_eqb

(** val rflag_eqb_OK_sumbool : rflag -> rflag -> bool **)

let rflag_eqb_OK_sumbool =
  reflect_dec rflag_eqb rflag_eqb_OK

type condt =
| O_ct
| NO_ct
| B_ct
| NB_ct
| E_ct
| NE_ct
| BE_ct
| NBE_ct
| S_ct
| NS_ct
| P_ct
| NP_ct
| L_ct
| NL_ct
| LE_ct
| NLE_ct

(** val condt_rect :
    'a1 -> 'a1 -> 'a1 -> 'a1 -> 'a1 -> 'a1 -> 'a1 -> 'a1 -> 'a1 -> 'a1 -> 'a1
    -> 'a1 -> 'a1 -> 'a1 -> 'a1 -> 'a1 -> condt -> 'a1 **)

let condt_rect f f0 f1 f2 f3 f4 f5 f6 f7 f8 f9 f10 f11 f12 f13 f14 = function
| O_ct -> f
| NO_ct -> f0
| B_ct -> f1
| NB_ct -> f2
| E_ct -> f3
| NE_ct -> f4
| BE_ct -> f5
| NBE_ct -> f6
| S_ct -> f7
| NS_ct -> f8
| P_ct -> f9
| NP_ct -> f10
| L_ct -> f11
| NL_ct -> f12
| LE_ct -> f13
| NLE_ct -> f14

(** val condt_rec :
    'a1 -> 'a1 -> 'a1 -> 'a1 -> 'a1 -> 'a1 -> 'a1 -> 'a1 -> 'a1 -> 'a1 -> 'a1
    -> 'a1 -> 'a1 -> 'a1 -> 'a1 -> 'a1 -> condt -> 'a1 **)

let condt_rec f f0 f1 f2 f3 f4 f5 f6 f7 f8 f9 f10 f11 f12 f13 f14 = function
| O_ct -> f
| NO_ct -> f0
| B_ct -> f1
| NB_ct -> f2
| E_ct -> f3
| NE_ct -> f4
| BE_ct -> f5
| NBE_ct -> f6
| S_ct -> f7
| NS_ct -> f8
| P_ct -> f9
| NP_ct -> f10
| L_ct -> f11
| NL_ct -> f12
| LE_ct -> f13
| NLE_ct -> f14

type is_condt =
| Coq_is_O_ct
| Coq_is_NO_ct
| Coq_is_B_ct
| Coq_is_NB_ct
| Coq_is_E_ct
| Coq_is_NE_ct
| Coq_is_BE_ct
| Coq_is_NBE_ct
| Coq_is_S_ct
| Coq_is_NS_ct
| Coq_is_P_ct
| Coq_is_NP_ct
| Coq_is_L_ct
| Coq_is_NL_ct
| Coq_is_LE_ct
| Coq_is_NLE_ct

(** val is_condt_rect :
    'a1 -> 'a1 -> 'a1 -> 'a1 -> 'a1 -> 'a1 -> 'a1 -> 'a1 -> 'a1 -> 'a1 -> 'a1
    -> 'a1 -> 'a1 -> 'a1 -> 'a1 -> 'a1 -> condt -> is_condt -> 'a1 **)

let is_condt_rect f f0 f1 f2 f3 f4 f5 f6 f7 f8 f9 f10 f11 f12 f13 f14 _ = function
| Coq_is_O_ct -> f
| Coq_is_NO_ct -> f0
| Coq_is_B_ct -> f1
| Coq_is_NB_ct -> f2
| Coq_is_E_ct -> f3
| Coq_is_NE_ct -> f4
| Coq_is_BE_ct -> f5
| Coq_is_NBE_ct -> f6
| Coq_is_S_ct -> f7
| Coq_is_NS_ct -> f8
| Coq_is_P_ct -> f9
| Coq_is_NP_ct -> f10
| Coq_is_L_ct -> f11
| Coq_is_NL_ct -> f12
| Coq_is_LE_ct -> f13
| Coq_is_NLE_ct -> f14

(** val is_condt_rec :
    'a1 -> 'a1 -> 'a1 -> 'a1 -> 'a1 -> 'a1 -> 'a1 -> 'a1 -> 'a1 -> 'a1 -> 'a1
    -> 'a1 -> 'a1 -> 'a1 -> 'a1 -> 'a1 -> condt -> is_condt -> 'a1 **)

let is_condt_rec f f0 f1 f2 f3 f4 f5 f6 f7 f8 f9 f10 f11 f12 f13 f14 _ = function
| Coq_is_O_ct -> f
| Coq_is_NO_ct -> f0
| Coq_is_B_ct -> f1
| Coq_is_NB_ct -> f2
| Coq_is_E_ct -> f3
| Coq_is_NE_ct -> f4
| Coq_is_BE_ct -> f5
| Coq_is_NBE_ct -> f6
| Coq_is_S_ct -> f7
| Coq_is_NS_ct -> f8
| Coq_is_P_ct -> f9
| Coq_is_NP_ct -> f10
| Coq_is_L_ct -> f11
| Coq_is_NL_ct -> f12
| Coq_is_LE_ct -> f13
| Coq_is_NLE_ct -> f14

(** val condt_tag : condt -> positive **)

let condt_tag = function
| O_ct -> Coq_xH
| NO_ct -> Coq_xO Coq_xH
| B_ct -> Coq_xI Coq_xH
| NB_ct -> Coq_xO (Coq_xO Coq_xH)
| E_ct -> Coq_xI (Coq_xO Coq_xH)
| NE_ct -> Coq_xO (Coq_xI Coq_xH)
| BE_ct -> Coq_xI (Coq_xI Coq_xH)
| NBE_ct -> Coq_xO (Coq_xO (Coq_xO Coq_xH))
| S_ct -> Coq_xI (Coq_xO (Coq_xO Coq_xH))
| NS_ct -> Coq_xO (Coq_xI (Coq_xO Coq_xH))
| P_ct -> Coq_xI (Coq_xI (Coq_xO Coq_xH))
| NP_ct -> Coq_xO (Coq_xO (Coq_xI Coq_xH))
| L_ct -> Coq_xI (Coq_xO (Coq_xI Coq_xH))
| NL_ct -> Coq_xO (Coq_xI (Coq_xI Coq_xH))
| LE_ct -> Coq_xI (Coq_xI (Coq_xI Coq_xH))
| NLE_ct -> Coq_xO (Coq_xO (Coq_xO (Coq_xO Coq_xH)))

(** val is_condt_inhab : condt -> is_condt **)

let is_condt_inhab = function
| O_ct -> Coq_is_O_ct
| NO_ct -> Coq_is_NO_ct
| B_ct -> Coq_is_B_ct
| NB_ct -> Coq_is_NB_ct
| E_ct -> Coq_is_E_ct
| NE_ct -> Coq_is_NE_ct
| BE_ct -> Coq_is_BE_ct
| NBE_ct -> Coq_is_NBE_ct
| S_ct -> Coq_is_S_ct
| NS_ct -> Coq_is_NS_ct
| P_ct -> Coq_is_P_ct
| NP_ct -> Coq_is_NP_ct
| L_ct -> Coq_is_L_ct
| NL_ct -> Coq_is_NL_ct
| LE_ct -> Coq_is_LE_ct
| NLE_ct -> Coq_is_NLE_ct

(** val is_condt_functor : condt -> is_condt -> is_condt **)

let rec is_condt_functor _ x =
  x

type box_condt_O_ct =
| Box_condt_O_ct

type condt_fields_t = __

(** val condt_fields : condt -> condt_fields_t **)

let condt_fields _ =
  Obj.magic Box_condt_O_ct

(** val condt_construct : positive -> condt_fields_t -> condt option **)

let condt_construct p _ =
  match p with
  | Coq_xI x ->
    (match x with
     | Coq_xI x0 ->
       (match x0 with
        | Coq_xI _ -> Some LE_ct
        | Coq_xO _ -> Some P_ct
        | Coq_xH -> Some BE_ct)
     | Coq_xO x0 ->
       (match x0 with
        | Coq_xI _ -> Some L_ct
        | Coq_xO _ -> Some S_ct
        | Coq_xH -> Some E_ct)
     | Coq_xH -> Some B_ct)
  | Coq_xO x ->
    (match x with
     | Coq_xI x0 ->
       (match x0 with
        | Coq_xI _ -> Some NL_ct
        | Coq_xO _ -> Some NS_ct
        | Coq_xH -> Some NE_ct)
     | Coq_xO x0 ->
       (match x0 with
        | Coq_xI _ -> Some NP_ct
        | Coq_xO x1 ->
          (match x1 with
           | Coq_xI _ -> None
           | Coq_xO _ -> Some NLE_ct
           | Coq_xH -> Some NBE_ct)
        | Coq_xH -> Some NB_ct)
     | Coq_xH -> Some NO_ct)
  | Coq_xH -> Some O_ct

(** val condt_induction :
    'a1 -> 'a1 -> 'a1 -> 'a1 -> 'a1 -> 'a1 -> 'a1 -> 'a1 -> 'a1 -> 'a1 -> 'a1
    -> 'a1 -> 'a1 -> 'a1 -> 'a1 -> 'a1 -> condt -> is_condt -> 'a1 **)

let condt_induction his_O_ct his_NO_ct his_B_ct his_NB_ct his_E_ct his_NE_ct his_BE_ct his_NBE_ct his_S_ct his_NS_ct his_P_ct his_NP_ct his_L_ct his_NL_ct his_LE_ct his_NLE_ct _ = function
| Coq_is_O_ct -> his_O_ct
| Coq_is_NO_ct -> his_NO_ct
| Coq_is_B_ct -> his_B_ct
| Coq_is_NB_ct -> his_NB_ct
| Coq_is_E_ct -> his_E_ct
| Coq_is_NE_ct -> his_NE_ct
| Coq_is_BE_ct -> his_BE_ct
| Coq_is_NBE_ct -> his_NBE_ct
| Coq_is_S_ct -> his_S_ct
| Coq_is_NS_ct -> his_NS_ct
| Coq_is_P_ct -> his_P_ct
| Coq_is_NP_ct -> his_NP_ct
| Coq_is_L_ct -> his_L_ct
| Coq_is_NL_ct -> his_NL_ct
| Coq_is_LE_ct -> his_LE_ct
| Coq_is_NLE_ct -> his_NLE_ct

(** val condt_eqb_fields :
    (condt -> condt -> bool) -> positive -> condt_fields_t -> condt_fields_t
    -> bool **)

let condt_eqb_fields _ _ _ _ =
  true

(** val condt_eqb : condt -> condt -> bool **)

let condt_eqb x1 x2 =
  eqb_body condt_tag condt_fields
    (Obj.magic condt_eqb_fields (fun _ _ -> true)) (condt_tag x1)
    Box_condt_O_ct x2

(** val condt_eqb_OK : condt -> condt -> reflect **)

let condt_eqb_OK =
  iffP2 condt_eqb

(** val condt_eqb_OK_sumbool : condt -> condt -> bool **)

let condt_eqb_OK_sumbool =
  reflect_dec condt_eqb condt_eqb_OK

(** val coq_HB_unnamed_factory_1 : register Coq_hasDecEq.axioms_ **)

let coq_HB_unnamed_factory_1 =
  { Coq_hasDecEq.eq_op = register_eqb; Coq_hasDecEq.eqP = register_eqb_OK }

(** val x86_decl_register__canonical__eqtype_Equality : Equality.coq_type **)

let x86_decl_register__canonical__eqtype_Equality =
  Obj.magic coq_HB_unnamed_factory_1

(** val coq_HB_unnamed_factory_3 : register_ext Coq_hasDecEq.axioms_ **)

let coq_HB_unnamed_factory_3 =
  { Coq_hasDecEq.eq_op = register_ext_eqb; Coq_hasDecEq.eqP =
    register_ext_eqb_OK }

(** val x86_decl_register_ext__canonical__eqtype_Equality :
    Equality.coq_type **)

let x86_decl_register_ext__canonical__eqtype_Equality =
  Obj.magic coq_HB_unnamed_factory_3

(** val coq_HB_unnamed_factory_5 : xmm_register Coq_hasDecEq.axioms_ **)

let coq_HB_unnamed_factory_5 =
  { Coq_hasDecEq.eq_op = xmm_register_eqb; Coq_hasDecEq.eqP =
    xmm_register_eqb_OK }

(** val x86_decl_xmm_register__canonical__eqtype_Equality :
    Equality.coq_type **)

let x86_decl_xmm_register__canonical__eqtype_Equality =
  Obj.magic coq_HB_unnamed_factory_5

(** val coq_HB_unnamed_factory_7 : rflag Coq_hasDecEq.axioms_ **)

let coq_HB_unnamed_factory_7 =
  { Coq_hasDecEq.eq_op = rflag_eqb; Coq_hasDecEq.eqP = rflag_eqb_OK }

(** val x86_decl_rflag__canonical__eqtype_Equality : Equality.coq_type **)

let x86_decl_rflag__canonical__eqtype_Equality =
  Obj.magic coq_HB_unnamed_factory_7

(** val coq_HB_unnamed_factory_9 : condt Coq_hasDecEq.axioms_ **)

let coq_HB_unnamed_factory_9 =
  { Coq_hasDecEq.eq_op = condt_eqb; Coq_hasDecEq.eqP = condt_eqb_OK }

(** val x86_decl_condt__canonical__eqtype_Equality : Equality.coq_type **)

let x86_decl_condt__canonical__eqtype_Equality =
  Obj.magic coq_HB_unnamed_factory_9

(** val registers : register list **)

let registers =
  RAX :: (RCX :: (RDX :: (RBX :: (RSP :: (RBP :: (RSI :: (RDI :: (R8 :: (R9 :: (R10 :: (R11 :: (R12 :: (R13 :: (R14 :: (R15 :: [])))))))))))))))

(** val coq_HB_unnamed_factory_11 : register Choice.Countable.axioms_ **)

let coq_HB_unnamed_factory_11 =
  Obj.magic Choice.eqtype_pcan_type__canonical__choice_Countable
    Choice.coq_Datatypes_nat__canonical__choice_Countable
    (FinIsCount.pickle x86_decl_register__canonical__eqtype_Equality
      (Obj.magic registers))
    (FinIsCount.unpickle x86_decl_register__canonical__eqtype_Equality
      (Obj.magic registers))

(** val choice_Countable__to__choice_hasChoice :
    register Choice.Coq_hasChoice.axioms_ **)

let choice_Countable__to__choice_hasChoice =
  coq_HB_unnamed_factory_11.Choice.Countable.choice_hasChoice_mixin

(** val choice_Countable__to__choice_Choice_isCountable :
    register Choice.Choice_isCountable.axioms_ **)

let choice_Countable__to__choice_Choice_isCountable =
  coq_HB_unnamed_factory_11.Choice.Countable.choice_Choice_isCountable_mixin

(** val coq_HB_unnamed_mixin_15 : register Choice.Coq_hasChoice.axioms_ **)

let coq_HB_unnamed_mixin_15 =
  coq_HB_unnamed_factory_11.Choice.Countable.choice_hasChoice_mixin

(** val x86_decl_register__canonical__choice_Choice :
    Choice.Choice.coq_type **)

let x86_decl_register__canonical__choice_Choice =
  { Choice.Choice.choice_hasChoice_mixin =
    (Obj.magic coq_HB_unnamed_mixin_15);
    Choice.Choice.eqtype_hasDecEq_mixin =
    (Obj.magic coq_HB_unnamed_factory_1) }

(** val coq_HB_unnamed_mixin_16 :
    register Choice.Choice_isCountable.axioms_ **)

let coq_HB_unnamed_mixin_16 =
  coq_HB_unnamed_factory_11.Choice.Countable.choice_Choice_isCountable_mixin

(** val x86_decl_register__canonical__choice_Countable :
    Choice.Countable.coq_type **)

let x86_decl_register__canonical__choice_Countable =
  { Choice.Countable.choice_hasChoice_mixin =
    (Obj.magic coq_HB_unnamed_mixin_15);
    Choice.Countable.eqtype_hasDecEq_mixin =
    (Obj.magic coq_HB_unnamed_factory_1);
    Choice.Countable.choice_Choice_isCountable_mixin =
    (Obj.magic coq_HB_unnamed_mixin_16) }

(** val coq_HB_unnamed_factory_17 : register Coq_isFinite.axioms_ **)

let coq_HB_unnamed_factory_17 =
  registers

(** val x86_decl_register__canonical__fintype_Finite : Finite.coq_type **)

let x86_decl_register__canonical__fintype_Finite =
  { Finite.choice_hasChoice_mixin = (Obj.magic coq_HB_unnamed_mixin_15);
    Finite.choice_Choice_isCountable_mixin =
    (Obj.magic coq_HB_unnamed_mixin_16); Finite.eqtype_hasDecEq_mixin =
    (Obj.magic coq_HB_unnamed_factory_1); Finite.fintype_isFinite_mixin =
    (Obj.magic coq_HB_unnamed_factory_17) }

(** val regxs : register_ext list **)

let regxs =
  MM0 :: (MM1 :: (MM2 :: (MM3 :: (MM4 :: (MM5 :: (MM6 :: (MM7 :: [])))))))

(** val coq_HB_unnamed_factory_19 : register_ext Choice.Countable.axioms_ **)

let coq_HB_unnamed_factory_19 =
  Obj.magic Choice.eqtype_pcan_type__canonical__choice_Countable
    Choice.coq_Datatypes_nat__canonical__choice_Countable
    (FinIsCount.pickle x86_decl_register_ext__canonical__eqtype_Equality
      (Obj.magic regxs))
    (FinIsCount.unpickle x86_decl_register_ext__canonical__eqtype_Equality
      (Obj.magic regxs))

(** val choice_Countable__to__choice_hasChoice__21 :
    register_ext Choice.Coq_hasChoice.axioms_ **)

let choice_Countable__to__choice_hasChoice__21 =
  coq_HB_unnamed_factory_19.Choice.Countable.choice_hasChoice_mixin

(** val choice_Countable__to__choice_Choice_isCountable__24 :
    register_ext Choice.Choice_isCountable.axioms_ **)

let choice_Countable__to__choice_Choice_isCountable__24 =
  coq_HB_unnamed_factory_19.Choice.Countable.choice_Choice_isCountable_mixin

(** val coq_HB_unnamed_mixin_25 :
    register_ext Choice.Coq_hasChoice.axioms_ **)

let coq_HB_unnamed_mixin_25 =
  coq_HB_unnamed_factory_19.Choice.Countable.choice_hasChoice_mixin

(** val x86_decl_register_ext__canonical__choice_Choice :
    Choice.Choice.coq_type **)

let x86_decl_register_ext__canonical__choice_Choice =
  { Choice.Choice.choice_hasChoice_mixin =
    (Obj.magic coq_HB_unnamed_mixin_25);
    Choice.Choice.eqtype_hasDecEq_mixin =
    (Obj.magic coq_HB_unnamed_factory_3) }

(** val coq_HB_unnamed_mixin_26 :
    register_ext Choice.Choice_isCountable.axioms_ **)

let coq_HB_unnamed_mixin_26 =
  coq_HB_unnamed_factory_19.Choice.Countable.choice_Choice_isCountable_mixin

(** val x86_decl_register_ext__canonical__choice_Countable :
    Choice.Countable.coq_type **)

let x86_decl_register_ext__canonical__choice_Countable =
  { Choice.Countable.choice_hasChoice_mixin =
    (Obj.magic coq_HB_unnamed_mixin_25);
    Choice.Countable.eqtype_hasDecEq_mixin =
    (Obj.magic coq_HB_unnamed_factory_3);
    Choice.Countable.choice_Choice_isCountable_mixin =
    (Obj.magic coq_HB_unnamed_mixin_26) }

(** val coq_HB_unnamed_factory_27 : register_ext Coq_isFinite.axioms_ **)

let coq_HB_unnamed_factory_27 =
  regxs

(** val x86_decl_register_ext__canonical__fintype_Finite : Finite.coq_type **)

let x86_decl_register_ext__canonical__fintype_Finite =
  { Finite.choice_hasChoice_mixin = (Obj.magic coq_HB_unnamed_mixin_25);
    Finite.choice_Choice_isCountable_mixin =
    (Obj.magic coq_HB_unnamed_mixin_26); Finite.eqtype_hasDecEq_mixin =
    (Obj.magic coq_HB_unnamed_factory_3); Finite.fintype_isFinite_mixin =
    (Obj.magic coq_HB_unnamed_factory_27) }

(** val xmm_registers : xmm_register list **)

let xmm_registers =
  XMM0 :: (XMM1 :: (XMM2 :: (XMM3 :: (XMM4 :: (XMM5 :: (XMM6 :: (XMM7 :: (XMM8 :: (XMM9 :: (XMM10 :: (XMM11 :: (XMM12 :: (XMM13 :: (XMM14 :: (XMM15 :: [])))))))))))))))

(** val coq_HB_unnamed_factory_29 : xmm_register Choice.Countable.axioms_ **)

let coq_HB_unnamed_factory_29 =
  Obj.magic Choice.eqtype_pcan_type__canonical__choice_Countable
    Choice.coq_Datatypes_nat__canonical__choice_Countable
    (FinIsCount.pickle x86_decl_xmm_register__canonical__eqtype_Equality
      (Obj.magic xmm_registers))
    (FinIsCount.unpickle x86_decl_xmm_register__canonical__eqtype_Equality
      (Obj.magic xmm_registers))

(** val choice_Countable__to__choice_hasChoice__31 :
    xmm_register Choice.Coq_hasChoice.axioms_ **)

let choice_Countable__to__choice_hasChoice__31 =
  coq_HB_unnamed_factory_29.Choice.Countable.choice_hasChoice_mixin

(** val choice_Countable__to__choice_Choice_isCountable__34 :
    xmm_register Choice.Choice_isCountable.axioms_ **)

let choice_Countable__to__choice_Choice_isCountable__34 =
  coq_HB_unnamed_factory_29.Choice.Countable.choice_Choice_isCountable_mixin

(** val coq_HB_unnamed_mixin_35 :
    xmm_register Choice.Coq_hasChoice.axioms_ **)

let coq_HB_unnamed_mixin_35 =
  coq_HB_unnamed_factory_29.Choice.Countable.choice_hasChoice_mixin

(** val x86_decl_xmm_register__canonical__choice_Choice :
    Choice.Choice.coq_type **)

let x86_decl_xmm_register__canonical__choice_Choice =
  { Choice.Choice.choice_hasChoice_mixin =
    (Obj.magic coq_HB_unnamed_mixin_35);
    Choice.Choice.eqtype_hasDecEq_mixin =
    (Obj.magic coq_HB_unnamed_factory_5) }

(** val coq_HB_unnamed_mixin_36 :
    xmm_register Choice.Choice_isCountable.axioms_ **)

let coq_HB_unnamed_mixin_36 =
  coq_HB_unnamed_factory_29.Choice.Countable.choice_Choice_isCountable_mixin

(** val x86_decl_xmm_register__canonical__choice_Countable :
    Choice.Countable.coq_type **)

let x86_decl_xmm_register__canonical__choice_Countable =
  { Choice.Countable.choice_hasChoice_mixin =
    (Obj.magic coq_HB_unnamed_mixin_35);
    Choice.Countable.eqtype_hasDecEq_mixin =
    (Obj.magic coq_HB_unnamed_factory_5);
    Choice.Countable.choice_Choice_isCountable_mixin =
    (Obj.magic coq_HB_unnamed_mixin_36) }

(** val coq_HB_unnamed_factory_37 : xmm_register Coq_isFinite.axioms_ **)

let coq_HB_unnamed_factory_37 =
  xmm_registers

(** val x86_decl_xmm_register__canonical__fintype_Finite : Finite.coq_type **)

let x86_decl_xmm_register__canonical__fintype_Finite =
  { Finite.choice_hasChoice_mixin = (Obj.magic coq_HB_unnamed_mixin_35);
    Finite.choice_Choice_isCountable_mixin =
    (Obj.magic coq_HB_unnamed_mixin_36); Finite.eqtype_hasDecEq_mixin =
    (Obj.magic coq_HB_unnamed_factory_5); Finite.fintype_isFinite_mixin =
    (Obj.magic coq_HB_unnamed_factory_37) }

(** val rflags : rflag list **)

let rflags =
  CF :: (PF :: (ZF :: (SF :: (OF :: []))))

(** val coq_HB_unnamed_factory_39 : rflag Choice.Countable.axioms_ **)

let coq_HB_unnamed_factory_39 =
  Obj.magic Choice.eqtype_pcan_type__canonical__choice_Countable
    Choice.coq_Datatypes_nat__canonical__choice_Countable
    (FinIsCount.pickle x86_decl_rflag__canonical__eqtype_Equality
      (Obj.magic rflags))
    (FinIsCount.unpickle x86_decl_rflag__canonical__eqtype_Equality
      (Obj.magic rflags))

(** val choice_Countable__to__choice_hasChoice__41 :
    rflag Choice.Coq_hasChoice.axioms_ **)

let choice_Countable__to__choice_hasChoice__41 =
  coq_HB_unnamed_factory_39.Choice.Countable.choice_hasChoice_mixin

(** val choice_Countable__to__choice_Choice_isCountable__44 :
    rflag Choice.Choice_isCountable.axioms_ **)

let choice_Countable__to__choice_Choice_isCountable__44 =
  coq_HB_unnamed_factory_39.Choice.Countable.choice_Choice_isCountable_mixin

(** val coq_HB_unnamed_mixin_45 : rflag Choice.Coq_hasChoice.axioms_ **)

let coq_HB_unnamed_mixin_45 =
  coq_HB_unnamed_factory_39.Choice.Countable.choice_hasChoice_mixin

(** val x86_decl_rflag__canonical__choice_Choice : Choice.Choice.coq_type **)

let x86_decl_rflag__canonical__choice_Choice =
  { Choice.Choice.choice_hasChoice_mixin =
    (Obj.magic coq_HB_unnamed_mixin_45);
    Choice.Choice.eqtype_hasDecEq_mixin =
    (Obj.magic coq_HB_unnamed_factory_7) }

(** val coq_HB_unnamed_mixin_46 : rflag Choice.Choice_isCountable.axioms_ **)

let coq_HB_unnamed_mixin_46 =
  coq_HB_unnamed_factory_39.Choice.Countable.choice_Choice_isCountable_mixin

(** val x86_decl_rflag__canonical__choice_Countable :
    Choice.Countable.coq_type **)

let x86_decl_rflag__canonical__choice_Countable =
  { Choice.Countable.choice_hasChoice_mixin =
    (Obj.magic coq_HB_unnamed_mixin_45);
    Choice.Countable.eqtype_hasDecEq_mixin =
    (Obj.magic coq_HB_unnamed_factory_7);
    Choice.Countable.choice_Choice_isCountable_mixin =
    (Obj.magic coq_HB_unnamed_mixin_46) }

(** val coq_HB_unnamed_factory_47 : rflag Coq_isFinite.axioms_ **)

let coq_HB_unnamed_factory_47 =
  rflags

(** val x86_decl_rflag__canonical__fintype_Finite : Finite.coq_type **)

let x86_decl_rflag__canonical__fintype_Finite =
  { Finite.choice_hasChoice_mixin = (Obj.magic coq_HB_unnamed_mixin_45);
    Finite.choice_Choice_isCountable_mixin =
    (Obj.magic coq_HB_unnamed_mixin_46); Finite.eqtype_hasDecEq_mixin =
    (Obj.magic coq_HB_unnamed_factory_7); Finite.fintype_isFinite_mixin =
    (Obj.magic coq_HB_unnamed_factory_47) }

(** val eqTC_register : register eqTypeC **)

let eqTC_register =
  { beq = register_eqb; ceqP = register_eqb_OK }

(** val finC_register : register finTypeC **)

let finC_register =
  { _eqC = eqTC_register; cenum = registers }

(** val register_to_string : register -> string **)

let register_to_string = function
| RAX -> "RAX"
| RCX -> "RCX"
| RDX -> "RDX"
| RBX -> "RBX"
| RSP -> "RSP"
| RBP -> "RBP"
| RSI -> "RSI"
| RDI -> "RDI"
| R8 -> "R8"
| R9 -> "R9"
| R10 -> "R10"
| R11 -> "R11"
| R12 -> "R12"
| R13 -> "R13"
| R14 -> "R14"
| R15 -> "R15"

(** val x86_reg_toS : register coq_ToString **)

let x86_reg_toS =
  { category = "register"; _finC = finC_register; to_string =
    register_to_string }

(** val eqTC_regx : register_ext eqTypeC **)

let eqTC_regx =
  { beq = register_ext_eqb; ceqP = register_ext_eqb_OK }

(** val finC_regx : register_ext finTypeC **)

let finC_regx =
  { _eqC = eqTC_regx; cenum = regxs }

(** val regx_to_string : register_ext -> string **)

let regx_to_string = function
| MM0 -> "MM0"
| MM1 -> "MM1"
| MM2 -> "MM2"
| MM3 -> "MM3"
| MM4 -> "MM4"
| MM5 -> "MM5"
| MM6 -> "MM6"
| MM7 -> "MM7"

(** val x86_regx_toS : register_ext coq_ToString **)

let x86_regx_toS =
  { category = "register"; _finC = finC_regx; to_string = regx_to_string }

(** val eqTC_xmm_register : xmm_register eqTypeC **)

let eqTC_xmm_register =
  { beq = xmm_register_eqb; ceqP = xmm_register_eqb_OK }

(** val finC_xmm_register : xmm_register finTypeC **)

let finC_xmm_register =
  { _eqC = eqTC_xmm_register; cenum = xmm_registers }

(** val xreg_to_string : xmm_register -> string **)

let xreg_to_string = function
| XMM0 -> "XMM0"
| XMM1 -> "XMM1"
| XMM2 -> "XMM2"
| XMM3 -> "XMM3"
| XMM4 -> "XMM4"
| XMM5 -> "XMM5"
| XMM6 -> "XMM6"
| XMM7 -> "XMM7"
| XMM8 -> "XMM8"
| XMM9 -> "XMM9"
| XMM10 -> "XMM10"
| XMM11 -> "XMM11"
| XMM12 -> "XMM12"
| XMM13 -> "XMM13"
| XMM14 -> "XMM14"
| XMM15 -> "XMM15"

(** val x86_xreg_toS : xmm_register coq_ToString **)

let x86_xreg_toS =
  { category = "ymm_register"; _finC = finC_xmm_register; to_string =
    xreg_to_string }

(** val eqTC_rflag : rflag eqTypeC **)

let eqTC_rflag =
  { beq = rflag_eqb; ceqP = rflag_eqb_OK }

(** val finC_rflag : rflag finTypeC **)

let finC_rflag =
  { _eqC = eqTC_rflag; cenum = rflags }

(** val rflag_to_string : rflag -> string **)

let rflag_to_string = function
| CF -> "CF"
| PF -> "PF"
| ZF -> "ZF"
| SF -> "SF"
| OF -> "OF"

(** val x86_rflag_toS : rflag coq_ToString **)

let x86_rflag_toS =
  { category = "rflag"; _finC = finC_rflag; to_string = rflag_to_string }

(** val eqC_condt : condt eqTypeC **)

let eqC_condt =
  { beq = condt_eqb; ceqP = condt_eqb_OK }

(** val x86_fc_of_cfc : combine_flags_core -> flag_combination **)

let x86_fc_of_cfc cfc =
  let vof = FCVar0 in
  let vcf = FCVar1 in
  let vsf = FCVar2 in
  let vzf = FCVar3 in
  (match cfc with
   | CFC_B -> vcf
   | CFC_E -> vzf
   | CFC_L -> FCNot (FCEq (vof, vsf))
   | CFC_BE -> FCOr (vcf, vzf)
   | CFC_LE -> FCOr ((FCNot (FCEq (vof, vsf))), vzf))

(** val x86_fcp : coq_FlagCombinationParams **)

let x86_fcp =
  x86_fc_of_cfc

(** val x86_check_CAimm :
    caimm_checker_s -> wsize -> GRing.ComRing.sort -> bool **)

let x86_check_CAimm checker _ _ =
  match checker with
  | CAimmC_none -> true
  | _ -> false

(** val x86_decl :
    (register, register_ext, xmm_register, rflag, condt) arch_decl **)

let x86_decl =
  { reg_size = x86_reg_size; xreg_size = x86_xreg_size; cond_eqC = eqC_condt;
    toS_r = x86_reg_toS; toS_rx = x86_regx_toS; toS_x = x86_xreg_toS; toS_f =
    x86_rflag_toS; ad_rsp = RSP; ad_fcp = x86_fcp; check_CAimm =
    x86_check_CAimm }

(** val x86_linux_call_conv :
    (register, register_ext, xmm_register, rflag, condt) calling_convention **)

let x86_linux_call_conv =
  { callee_saved =
    (map (fun x -> ARReg x)
      (RBX :: (RBP :: (RSP :: (R12 :: (R13 :: (R14 :: (R15 :: []))))))));
    call_reg_args = (RDI :: (RSI :: (RDX :: (RCX :: (R8 :: (R9 :: []))))));
    call_xreg_args =
    (XMM0 :: (XMM1 :: (XMM2 :: (XMM3 :: (XMM4 :: (XMM5 :: (XMM6 :: (XMM7 :: []))))))));
    call_reg_ret = (RAX :: (RDX :: [])); call_xreg_ret =
    (XMM0 :: (XMM1 :: [])) }

(** val x86_windows_call_conv :
    (register, register_ext, xmm_register, rflag, condt) calling_convention **)

let x86_windows_call_conv =
  { callee_saved =
    (cat
      (map (fun x -> ARReg x)
        (RBX :: (RBP :: (RDI :: (RSI :: (RSP :: (R12 :: (R13 :: (R14 :: (R15 :: []))))))))))
      (map (fun x -> AXReg x)
        (XMM6 :: (XMM7 :: (XMM8 :: (XMM9 :: (XMM10 :: (XMM11 :: (XMM12 :: (XMM13 :: (XMM14 :: (XMM15 :: []))))))))))));
    call_reg_args = (RCX :: (RDX :: (R8 :: (R9 :: [])))); call_xreg_args =
    (XMM0 :: (XMM1 :: (XMM2 :: (XMM3 :: [])))); call_reg_ret = (RAX :: []);
    call_xreg_ret = (XMM0 :: []) }

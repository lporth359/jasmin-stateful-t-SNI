open BinNums
open Bool
open Datatypes
open Arch_decl
open Arch_utils
open EqbOK
open Eqb_core_defs
open Eqtype
open Riscv_decl
open Sem_type
open Seq
open Sopn
open Ssralg
open Type
open Utils0
open Word0
open Wsize

type __ = Obj.t

module E =
 struct
  (** val no_semantics : error **)

  let no_semantics =
    ErrType
 end

(** val pp_name :
    string -> (register, empty, empty, empty, condt) asm_arg list ->
    (register, empty, empty, empty, condt) pp_asm_op **)

let pp_name name args =
  { pp_aop_name = name; pp_aop_ext = PP_name; pp_aop_args =
    (map (fun a -> (riscv_decl.reg_size, a)) args) }

(** val coq_RTypeInstruction :
    wsize -> sem_tuple sem_prod -> string -> string -> (register, empty,
    empty, empty, condt) instr_desc_t **)

let coq_RTypeInstruction ws semi jazz_name asm_name =
  let tin = (lreg riscv_decl) :: ((Coq_lword ws) :: []) in
  { id_valid = true; id_msb_flag = MSB_MERGE; id_tin = tin; id_in =
  ((coq_Ea riscv_decl (S O)) :: ((coq_Ea riscv_decl (S (S O))) :: []));
  id_tout = ((lreg riscv_decl) :: []); id_out =
  ((coq_Ea riscv_decl O) :: []); id_semi =
  (sem_prod_ok (map eval_ltype tin) semi); id_args_kinds = ak_reg_reg_reg;
  id_nargs = (S (S (S O))); id_str_jas = (pp_s jazz_name); id_safe = [];
  id_pp_asm = (pp_name asm_name) }

(** val coq_ITypeInstruction :
    caimm_checker_s -> wsize -> sem_tuple sem_prod -> string -> string ->
    (register, empty, empty, empty, condt) instr_desc_t **)

let coq_ITypeInstruction chk_imm ws semi jazz_name asm_name =
  let tin = (lreg riscv_decl) :: ((Coq_lword ws) :: []) in
  { id_valid = true; id_msb_flag = MSB_MERGE; id_tin = tin; id_in =
  ((coq_Ea riscv_decl (S O)) :: ((coq_Ea riscv_decl (S (S O))) :: []));
  id_tout = ((lreg riscv_decl) :: []); id_out =
  ((coq_Ea riscv_decl O) :: []); id_semi =
  (sem_prod_ok (map eval_ltype tin) semi); id_args_kinds =
  (((CAreg :: []) :: ((CAreg :: []) :: (((CAimm (chk_imm,
  riscv_decl.reg_size)) :: []) :: []))) :: []); id_nargs = (S (S (S O)));
  id_str_jas = (pp_s jazz_name); id_safe = []; id_pp_asm =
  (pp_name asm_name) }

(** val coq_ITypeInstruction_12s :
    wsize -> sem_tuple sem_prod -> string -> string -> (register, empty,
    empty, empty, condt) instr_desc_t **)

let coq_ITypeInstruction_12s =
  coq_ITypeInstruction CAimmC_riscv_12bits_signed

(** val coq_ITypeInstruction_5u :
    wsize -> sem_tuple sem_prod -> string -> string -> (register, empty,
    empty, empty, condt) instr_desc_t **)

let coq_ITypeInstruction_5u =
  coq_ITypeInstruction CAimmC_riscv_5bits_unsigned

type riscv_op =
| ADD
| ADDI
| SUB
| SLT
| SLTI
| SLTU
| SLTIU
| AND
| ANDI
| OR
| ORI
| XOR
| XORI
| SLL
| SLLI
| SRL
| SRLI
| SRA
| SRAI
| MV
| LA
| LI
| NOT
| NEG
| LOAD of signedness * wsize
| STORE of wsize
| MUL
| MULH
| MULHU
| MULHSU
| DIV
| DIVU
| REM
| REMU

(** val riscv_op_rect :
    'a1 -> 'a1 -> 'a1 -> 'a1 -> 'a1 -> 'a1 -> 'a1 -> 'a1 -> 'a1 -> 'a1 -> 'a1
    -> 'a1 -> 'a1 -> 'a1 -> 'a1 -> 'a1 -> 'a1 -> 'a1 -> 'a1 -> 'a1 -> 'a1 ->
    'a1 -> 'a1 -> 'a1 -> (signedness -> wsize -> 'a1) -> (wsize -> 'a1) ->
    'a1 -> 'a1 -> 'a1 -> 'a1 -> 'a1 -> 'a1 -> 'a1 -> 'a1 -> riscv_op -> 'a1 **)

let riscv_op_rect f f0 f1 f2 f3 f4 f5 f6 f7 f8 f9 f10 f11 f12 f13 f14 f15 f16 f17 f18 f19 f20 f21 f22 f23 f24 f25 f26 f27 f28 f29 f30 f31 f32 = function
| ADD -> f
| ADDI -> f0
| SUB -> f1
| SLT -> f2
| SLTI -> f3
| SLTU -> f4
| SLTIU -> f5
| AND -> f6
| ANDI -> f7
| OR -> f8
| ORI -> f9
| XOR -> f10
| XORI -> f11
| SLL -> f12
| SLLI -> f13
| SRL -> f14
| SRLI -> f15
| SRA -> f16
| SRAI -> f17
| MV -> f18
| LA -> f19
| LI -> f20
| NOT -> f21
| NEG -> f22
| LOAD (s, w) -> f23 s w
| STORE w -> f24 w
| MUL -> f25
| MULH -> f26
| MULHU -> f27
| MULHSU -> f28
| DIV -> f29
| DIVU -> f30
| REM -> f31
| REMU -> f32

(** val riscv_op_rec :
    'a1 -> 'a1 -> 'a1 -> 'a1 -> 'a1 -> 'a1 -> 'a1 -> 'a1 -> 'a1 -> 'a1 -> 'a1
    -> 'a1 -> 'a1 -> 'a1 -> 'a1 -> 'a1 -> 'a1 -> 'a1 -> 'a1 -> 'a1 -> 'a1 ->
    'a1 -> 'a1 -> 'a1 -> (signedness -> wsize -> 'a1) -> (wsize -> 'a1) ->
    'a1 -> 'a1 -> 'a1 -> 'a1 -> 'a1 -> 'a1 -> 'a1 -> 'a1 -> riscv_op -> 'a1 **)

let riscv_op_rec f f0 f1 f2 f3 f4 f5 f6 f7 f8 f9 f10 f11 f12 f13 f14 f15 f16 f17 f18 f19 f20 f21 f22 f23 f24 f25 f26 f27 f28 f29 f30 f31 f32 = function
| ADD -> f
| ADDI -> f0
| SUB -> f1
| SLT -> f2
| SLTI -> f3
| SLTU -> f4
| SLTIU -> f5
| AND -> f6
| ANDI -> f7
| OR -> f8
| ORI -> f9
| XOR -> f10
| XORI -> f11
| SLL -> f12
| SLLI -> f13
| SRL -> f14
| SRLI -> f15
| SRA -> f16
| SRAI -> f17
| MV -> f18
| LA -> f19
| LI -> f20
| NOT -> f21
| NEG -> f22
| LOAD (s, w) -> f23 s w
| STORE w -> f24 w
| MUL -> f25
| MULH -> f26
| MULHU -> f27
| MULHSU -> f28
| DIV -> f29
| DIVU -> f30
| REM -> f31
| REMU -> f32

type is_riscv_op =
| Coq_is_ADD
| Coq_is_ADDI
| Coq_is_SUB
| Coq_is_SLT
| Coq_is_SLTI
| Coq_is_SLTU
| Coq_is_SLTIU
| Coq_is_AND
| Coq_is_ANDI
| Coq_is_OR
| Coq_is_ORI
| Coq_is_XOR
| Coq_is_XORI
| Coq_is_SLL
| Coq_is_SLLI
| Coq_is_SRL
| Coq_is_SRLI
| Coq_is_SRA
| Coq_is_SRAI
| Coq_is_MV
| Coq_is_LA
| Coq_is_LI
| Coq_is_NOT
| Coq_is_NEG
| Coq_is_LOAD of signedness * is_signedness * wsize * is_wsize
| Coq_is_STORE of wsize * is_wsize
| Coq_is_MUL
| Coq_is_MULH
| Coq_is_MULHU
| Coq_is_MULHSU
| Coq_is_DIV
| Coq_is_DIVU
| Coq_is_REM
| Coq_is_REMU

(** val is_riscv_op_rect :
    'a1 -> 'a1 -> 'a1 -> 'a1 -> 'a1 -> 'a1 -> 'a1 -> 'a1 -> 'a1 -> 'a1 -> 'a1
    -> 'a1 -> 'a1 -> 'a1 -> 'a1 -> 'a1 -> 'a1 -> 'a1 -> 'a1 -> 'a1 -> 'a1 ->
    'a1 -> 'a1 -> 'a1 -> (signedness -> is_signedness -> wsize -> is_wsize ->
    'a1) -> (wsize -> is_wsize -> 'a1) -> 'a1 -> 'a1 -> 'a1 -> 'a1 -> 'a1 ->
    'a1 -> 'a1 -> 'a1 -> riscv_op -> is_riscv_op -> 'a1 **)

let is_riscv_op_rect f f0 f1 f2 f3 f4 f5 f6 f7 f8 f9 f10 f11 f12 f13 f14 f15 f16 f17 f18 f19 f20 f21 f22 f23 f24 f25 f26 f27 f28 f29 f30 f31 f32 _ = function
| Coq_is_ADD -> f
| Coq_is_ADDI -> f0
| Coq_is_SUB -> f1
| Coq_is_SLT -> f2
| Coq_is_SLTI -> f3
| Coq_is_SLTU -> f4
| Coq_is_SLTIU -> f5
| Coq_is_AND -> f6
| Coq_is_ANDI -> f7
| Coq_is_OR -> f8
| Coq_is_ORI -> f9
| Coq_is_XOR -> f10
| Coq_is_XORI -> f11
| Coq_is_SLL -> f12
| Coq_is_SLLI -> f13
| Coq_is_SRL -> f14
| Coq_is_SRLI -> f15
| Coq_is_SRA -> f16
| Coq_is_SRAI -> f17
| Coq_is_MV -> f18
| Coq_is_LA -> f19
| Coq_is_LI -> f20
| Coq_is_NOT -> f21
| Coq_is_NEG -> f22
| Coq_is_LOAD (s, p_, w, p_0) -> f23 s p_ w p_0
| Coq_is_STORE (w, p_) -> f24 w p_
| Coq_is_MUL -> f25
| Coq_is_MULH -> f26
| Coq_is_MULHU -> f27
| Coq_is_MULHSU -> f28
| Coq_is_DIV -> f29
| Coq_is_DIVU -> f30
| Coq_is_REM -> f31
| Coq_is_REMU -> f32

(** val is_riscv_op_rec :
    'a1 -> 'a1 -> 'a1 -> 'a1 -> 'a1 -> 'a1 -> 'a1 -> 'a1 -> 'a1 -> 'a1 -> 'a1
    -> 'a1 -> 'a1 -> 'a1 -> 'a1 -> 'a1 -> 'a1 -> 'a1 -> 'a1 -> 'a1 -> 'a1 ->
    'a1 -> 'a1 -> 'a1 -> (signedness -> is_signedness -> wsize -> is_wsize ->
    'a1) -> (wsize -> is_wsize -> 'a1) -> 'a1 -> 'a1 -> 'a1 -> 'a1 -> 'a1 ->
    'a1 -> 'a1 -> 'a1 -> riscv_op -> is_riscv_op -> 'a1 **)

let is_riscv_op_rec f f0 f1 f2 f3 f4 f5 f6 f7 f8 f9 f10 f11 f12 f13 f14 f15 f16 f17 f18 f19 f20 f21 f22 f23 f24 f25 f26 f27 f28 f29 f30 f31 f32 _ = function
| Coq_is_ADD -> f
| Coq_is_ADDI -> f0
| Coq_is_SUB -> f1
| Coq_is_SLT -> f2
| Coq_is_SLTI -> f3
| Coq_is_SLTU -> f4
| Coq_is_SLTIU -> f5
| Coq_is_AND -> f6
| Coq_is_ANDI -> f7
| Coq_is_OR -> f8
| Coq_is_ORI -> f9
| Coq_is_XOR -> f10
| Coq_is_XORI -> f11
| Coq_is_SLL -> f12
| Coq_is_SLLI -> f13
| Coq_is_SRL -> f14
| Coq_is_SRLI -> f15
| Coq_is_SRA -> f16
| Coq_is_SRAI -> f17
| Coq_is_MV -> f18
| Coq_is_LA -> f19
| Coq_is_LI -> f20
| Coq_is_NOT -> f21
| Coq_is_NEG -> f22
| Coq_is_LOAD (s, p_, w, p_0) -> f23 s p_ w p_0
| Coq_is_STORE (w, p_) -> f24 w p_
| Coq_is_MUL -> f25
| Coq_is_MULH -> f26
| Coq_is_MULHU -> f27
| Coq_is_MULHSU -> f28
| Coq_is_DIV -> f29
| Coq_is_DIVU -> f30
| Coq_is_REM -> f31
| Coq_is_REMU -> f32

(** val riscv_op_tag : riscv_op -> positive **)

let riscv_op_tag = function
| ADD -> Coq_xH
| ADDI -> Coq_xO Coq_xH
| SUB -> Coq_xI Coq_xH
| SLT -> Coq_xO (Coq_xO Coq_xH)
| SLTI -> Coq_xI (Coq_xO Coq_xH)
| SLTU -> Coq_xO (Coq_xI Coq_xH)
| SLTIU -> Coq_xI (Coq_xI Coq_xH)
| AND -> Coq_xO (Coq_xO (Coq_xO Coq_xH))
| ANDI -> Coq_xI (Coq_xO (Coq_xO Coq_xH))
| OR -> Coq_xO (Coq_xI (Coq_xO Coq_xH))
| ORI -> Coq_xI (Coq_xI (Coq_xO Coq_xH))
| XOR -> Coq_xO (Coq_xO (Coq_xI Coq_xH))
| XORI -> Coq_xI (Coq_xO (Coq_xI Coq_xH))
| SLL -> Coq_xO (Coq_xI (Coq_xI Coq_xH))
| SLLI -> Coq_xI (Coq_xI (Coq_xI Coq_xH))
| SRL -> Coq_xO (Coq_xO (Coq_xO (Coq_xO Coq_xH)))
| SRLI -> Coq_xI (Coq_xO (Coq_xO (Coq_xO Coq_xH)))
| SRA -> Coq_xO (Coq_xI (Coq_xO (Coq_xO Coq_xH)))
| SRAI -> Coq_xI (Coq_xI (Coq_xO (Coq_xO Coq_xH)))
| MV -> Coq_xO (Coq_xO (Coq_xI (Coq_xO Coq_xH)))
| LA -> Coq_xI (Coq_xO (Coq_xI (Coq_xO Coq_xH)))
| LI -> Coq_xO (Coq_xI (Coq_xI (Coq_xO Coq_xH)))
| NOT -> Coq_xI (Coq_xI (Coq_xI (Coq_xO Coq_xH)))
| NEG -> Coq_xO (Coq_xO (Coq_xO (Coq_xI Coq_xH)))
| LOAD (_, _) -> Coq_xI (Coq_xO (Coq_xO (Coq_xI Coq_xH)))
| STORE _ -> Coq_xO (Coq_xI (Coq_xO (Coq_xI Coq_xH)))
| MUL -> Coq_xI (Coq_xI (Coq_xO (Coq_xI Coq_xH)))
| MULH -> Coq_xO (Coq_xO (Coq_xI (Coq_xI Coq_xH)))
| MULHU -> Coq_xI (Coq_xO (Coq_xI (Coq_xI Coq_xH)))
| MULHSU -> Coq_xO (Coq_xI (Coq_xI (Coq_xI Coq_xH)))
| DIV -> Coq_xI (Coq_xI (Coq_xI (Coq_xI Coq_xH)))
| DIVU -> Coq_xO (Coq_xO (Coq_xO (Coq_xO (Coq_xO Coq_xH))))
| REM -> Coq_xI (Coq_xO (Coq_xO (Coq_xO (Coq_xO Coq_xH))))
| REMU -> Coq_xO (Coq_xI (Coq_xO (Coq_xO (Coq_xO Coq_xH))))

(** val is_riscv_op_inhab : riscv_op -> is_riscv_op **)

let is_riscv_op_inhab = function
| ADD -> Coq_is_ADD
| ADDI -> Coq_is_ADDI
| SUB -> Coq_is_SUB
| SLT -> Coq_is_SLT
| SLTI -> Coq_is_SLTI
| SLTU -> Coq_is_SLTU
| SLTIU -> Coq_is_SLTIU
| AND -> Coq_is_AND
| ANDI -> Coq_is_ANDI
| OR -> Coq_is_OR
| ORI -> Coq_is_ORI
| XOR -> Coq_is_XOR
| XORI -> Coq_is_XORI
| SLL -> Coq_is_SLL
| SLLI -> Coq_is_SLLI
| SRL -> Coq_is_SRL
| SRLI -> Coq_is_SRLI
| SRA -> Coq_is_SRA
| SRAI -> Coq_is_SRAI
| MV -> Coq_is_MV
| LA -> Coq_is_LA
| LI -> Coq_is_LI
| NOT -> Coq_is_NOT
| NEG -> Coq_is_NEG
| LOAD (h, h0) ->
  Coq_is_LOAD (h, (is_signedness_inhab h), h0, (is_wsize_inhab h0))
| STORE h -> Coq_is_STORE (h, (is_wsize_inhab h))
| MUL -> Coq_is_MUL
| MULH -> Coq_is_MULH
| MULHU -> Coq_is_MULHU
| MULHSU -> Coq_is_MULHSU
| DIV -> Coq_is_DIV
| DIVU -> Coq_is_DIVU
| REM -> Coq_is_REM
| REMU -> Coq_is_REMU

(** val is_riscv_op_functor : riscv_op -> is_riscv_op -> is_riscv_op **)

let rec is_riscv_op_functor _ x =
  x

type box_riscv_op_ADD =
| Box_riscv_op_ADD

type box_riscv_op_LOAD = { coq_Box_riscv_op_LOAD_0 : signedness;
                           coq_Box_riscv_op_LOAD_1 : wsize }

(** val coq_Box_riscv_op_LOAD_0 : box_riscv_op_LOAD -> signedness **)

let coq_Box_riscv_op_LOAD_0 record =
  record.coq_Box_riscv_op_LOAD_0

(** val coq_Box_riscv_op_LOAD_1 : box_riscv_op_LOAD -> wsize **)

let coq_Box_riscv_op_LOAD_1 record =
  record.coq_Box_riscv_op_LOAD_1

type box_riscv_op_STORE =
  wsize
  (* singleton inductive, whose constructor was Box_riscv_op_STORE *)

(** val coq_Box_riscv_op_STORE_0 : box_riscv_op_STORE -> wsize **)

let coq_Box_riscv_op_STORE_0 record =
  record

type riscv_op_fields_t = __

(** val riscv_op_fields : riscv_op -> riscv_op_fields_t **)

let riscv_op_fields = function
| LOAD (h, h0) ->
  Obj.magic { coq_Box_riscv_op_LOAD_0 = h; coq_Box_riscv_op_LOAD_1 = h0 }
| STORE h -> Obj.magic h
| _ -> Obj.magic Box_riscv_op_ADD

(** val riscv_op_construct :
    positive -> riscv_op_fields_t -> riscv_op option **)

let riscv_op_construct p b =
  match p with
  | Coq_xI x ->
    (match x with
     | Coq_xI x0 ->
       (match x0 with
        | Coq_xI x1 ->
          (match x1 with
           | Coq_xI _ -> Some DIV
           | Coq_xO _ -> Some NOT
           | Coq_xH -> Some SLLI)
        | Coq_xO x1 ->
          (match x1 with
           | Coq_xI _ -> Some MUL
           | Coq_xO _ -> Some SRAI
           | Coq_xH -> Some ORI)
        | Coq_xH -> Some SLTIU)
     | Coq_xO x0 ->
       (match x0 with
        | Coq_xI x1 ->
          (match x1 with
           | Coq_xI _ -> Some MULHU
           | Coq_xO _ -> Some LA
           | Coq_xH -> Some XORI)
        | Coq_xO x1 ->
          (match x1 with
           | Coq_xI _ ->
             let { coq_Box_riscv_op_LOAD_0 = box_riscv_op_LOAD_0;
               coq_Box_riscv_op_LOAD_1 = box_riscv_op_LOAD_1 } = Obj.magic b
             in
             Some (LOAD (box_riscv_op_LOAD_0, box_riscv_op_LOAD_1))
           | Coq_xO x2 ->
             (match x2 with
              | Coq_xI _ -> None
              | Coq_xO _ -> Some REM
              | Coq_xH -> Some SRLI)
           | Coq_xH -> Some ANDI)
        | Coq_xH -> Some SLTI)
     | Coq_xH -> Some SUB)
  | Coq_xO x ->
    (match x with
     | Coq_xI x0 ->
       (match x0 with
        | Coq_xI x1 ->
          (match x1 with
           | Coq_xI _ -> Some MULHSU
           | Coq_xO _ -> Some LI
           | Coq_xH -> Some SLL)
        | Coq_xO x1 ->
          (match x1 with
           | Coq_xI _ -> Some (STORE (Obj.magic b))
           | Coq_xO x2 ->
             (match x2 with
              | Coq_xI _ -> None
              | Coq_xO _ -> Some REMU
              | Coq_xH -> Some SRA)
           | Coq_xH -> Some OR)
        | Coq_xH -> Some SLTU)
     | Coq_xO x0 ->
       (match x0 with
        | Coq_xI x1 ->
          (match x1 with
           | Coq_xI _ -> Some MULH
           | Coq_xO _ -> Some MV
           | Coq_xH -> Some XOR)
        | Coq_xO x1 ->
          (match x1 with
           | Coq_xI _ -> Some NEG
           | Coq_xO x2 ->
             (match x2 with
              | Coq_xI _ -> None
              | Coq_xO _ -> Some DIVU
              | Coq_xH -> Some SRL)
           | Coq_xH -> Some AND)
        | Coq_xH -> Some SLT)
     | Coq_xH -> Some ADDI)
  | Coq_xH -> Some ADD

(** val riscv_op_induction :
    'a1 -> 'a1 -> 'a1 -> 'a1 -> 'a1 -> 'a1 -> 'a1 -> 'a1 -> 'a1 -> 'a1 -> 'a1
    -> 'a1 -> 'a1 -> 'a1 -> 'a1 -> 'a1 -> 'a1 -> 'a1 -> 'a1 -> 'a1 -> 'a1 ->
    'a1 -> 'a1 -> 'a1 -> (signedness -> is_signedness -> wsize -> is_wsize ->
    'a1) -> (wsize -> is_wsize -> 'a1) -> 'a1 -> 'a1 -> 'a1 -> 'a1 -> 'a1 ->
    'a1 -> 'a1 -> 'a1 -> riscv_op -> is_riscv_op -> 'a1 **)

let riscv_op_induction his_ADD his_ADDI his_SUB his_SLT his_SLTI his_SLTU his_SLTIU his_AND his_ANDI his_OR his_ORI his_XOR his_XORI his_SLL his_SLLI his_SRL his_SRLI his_SRA his_SRAI his_MV his_LA his_LI his_NOT his_NEG his_LOAD his_STORE his_MUL his_MULH his_MULHU his_MULHSU his_DIV his_DIVU his_REM his_REMU _ = function
| Coq_is_ADD -> his_ADD
| Coq_is_ADDI -> his_ADDI
| Coq_is_SUB -> his_SUB
| Coq_is_SLT -> his_SLT
| Coq_is_SLTI -> his_SLTI
| Coq_is_SLTU -> his_SLTU
| Coq_is_SLTIU -> his_SLTIU
| Coq_is_AND -> his_AND
| Coq_is_ANDI -> his_ANDI
| Coq_is_OR -> his_OR
| Coq_is_ORI -> his_ORI
| Coq_is_XOR -> his_XOR
| Coq_is_XORI -> his_XORI
| Coq_is_SLL -> his_SLL
| Coq_is_SLLI -> his_SLLI
| Coq_is_SRL -> his_SRL
| Coq_is_SRLI -> his_SRLI
| Coq_is_SRA -> his_SRA
| Coq_is_SRAI -> his_SRAI
| Coq_is_MV -> his_MV
| Coq_is_LA -> his_LA
| Coq_is_LI -> his_LI
| Coq_is_NOT -> his_NOT
| Coq_is_NEG -> his_NEG
| Coq_is_LOAD (x0, p_, x1, p_0) -> his_LOAD x0 p_ x1 p_0
| Coq_is_STORE (x0, p_) -> his_STORE x0 p_
| Coq_is_MUL -> his_MUL
| Coq_is_MULH -> his_MULH
| Coq_is_MULHU -> his_MULHU
| Coq_is_MULHSU -> his_MULHSU
| Coq_is_DIV -> his_DIV
| Coq_is_DIVU -> his_DIVU
| Coq_is_REM -> his_REM
| Coq_is_REMU -> his_REMU

(** val riscv_op_eqb_fields :
    (riscv_op -> riscv_op -> bool) -> positive -> riscv_op_fields_t ->
    riscv_op_fields_t -> bool **)

let riscv_op_eqb_fields _ x a b =
  match x with
  | Coq_xI x0 ->
    (match x0 with
     | Coq_xO x1 ->
       (match x1 with
        | Coq_xO x2 ->
          (match x2 with
           | Coq_xI _ ->
             let { coq_Box_riscv_op_LOAD_0 = box_riscv_op_LOAD_0;
               coq_Box_riscv_op_LOAD_1 = box_riscv_op_LOAD_1 } = Obj.magic a
             in
             let { coq_Box_riscv_op_LOAD_0 = box_riscv_op_LOAD_2;
               coq_Box_riscv_op_LOAD_1 = box_riscv_op_LOAD_3 } = Obj.magic b
             in
             (&&) (signedness_eqb box_riscv_op_LOAD_0 box_riscv_op_LOAD_2)
               ((&&) (wsize_eqb box_riscv_op_LOAD_1 box_riscv_op_LOAD_3) true)
           | _ -> true)
        | _ -> true)
     | _ -> true)
  | Coq_xO x0 ->
    (match x0 with
     | Coq_xI x1 ->
       (match x1 with
        | Coq_xO x2 ->
          (match x2 with
           | Coq_xI _ -> (&&) (wsize_eqb (Obj.magic a) (Obj.magic b)) true
           | _ -> true)
        | _ -> true)
     | _ -> true)
  | Coq_xH -> true

(** val riscv_op_eqb : riscv_op -> riscv_op -> bool **)

let riscv_op_eqb x1 x2 =
  match x1 with
  | LOAD (h, h0) ->
    eqb_body riscv_op_tag riscv_op_fields
      (Obj.magic riscv_op_eqb_fields (fun _ _ -> true))
      (riscv_op_tag (LOAD (h, h0))) { coq_Box_riscv_op_LOAD_0 = h;
      coq_Box_riscv_op_LOAD_1 = h0 } x2
  | STORE h ->
    eqb_body riscv_op_tag riscv_op_fields
      (Obj.magic riscv_op_eqb_fields (fun _ _ -> true))
      (riscv_op_tag (STORE h)) h x2
  | x ->
    eqb_body riscv_op_tag riscv_op_fields
      (Obj.magic riscv_op_eqb_fields (fun _ _ -> true)) (riscv_op_tag x)
      Box_riscv_op_ADD x2

(** val riscv_op_eqb_OK : riscv_op -> riscv_op -> reflect **)

let riscv_op_eqb_OK =
  iffP2 riscv_op_eqb

(** val riscv_op_eqb_OK_sumbool : riscv_op -> riscv_op -> bool **)

let riscv_op_eqb_OK_sumbool =
  reflect_dec riscv_op_eqb riscv_op_eqb_OK

(** val eqTC_riscv_op : riscv_op eqTypeC **)

let eqTC_riscv_op =
  { beq = riscv_op_eqb; ceqP = riscv_op_eqb_OK }

(** val riscv_op_eqType : Equality.coq_type **)

let riscv_op_eqType =
  ceqT_eqType eqTC_riscv_op

(** val riscv_add_semi : sem_tuple -> sem_tuple -> sem_tuple **)

let riscv_add_semi wn wm =
  GRing.add
    (GRing.ComRing.Exports.coq_GRing_ComRing__to__GRing_Nmodule
      (word riscv_decl.reg_size)) wn wm

(** val riscv_ADD_instr :
    (register, empty, empty, empty, condt) instr_desc_t **)

let riscv_ADD_instr =
  coq_RTypeInstruction riscv_decl.reg_size (Obj.magic riscv_add_semi) "ADD"
    "add"

(** val prim_ADD : string * riscv_op prim_constructor **)

let prim_ADD =
  ("ADD", (primM ADD))

(** val riscv_ADDI_instr :
    (register, empty, empty, empty, condt) instr_desc_t **)

let riscv_ADDI_instr =
  coq_ITypeInstruction_12s riscv_decl.reg_size (Obj.magic riscv_add_semi)
    "ADDI" "addi"

(** val prim_ADDI : string * riscv_op prim_constructor **)

let prim_ADDI =
  ("ADDI", (primM ADDI))

(** val riscv_sub_semi : sem_tuple -> sem_tuple -> sem_tuple **)

let riscv_sub_semi wn wm =
  GRing.add
    (GRing.ComRing.Exports.coq_GRing_ComRing__to__GRing_Nmodule
      (word riscv_decl.reg_size)) wn
    (GRing.opp
      (GRing.ComRing.Exports.coq_GRing_ComRing__to__GRing_Zmodule
        (word riscv_decl.reg_size)) wm)

(** val riscv_SUB_instr :
    (register, empty, empty, empty, condt) instr_desc_t **)

let riscv_SUB_instr =
  coq_RTypeInstruction riscv_decl.reg_size (Obj.magic riscv_sub_semi) "SUB"
    "sub"

(** val prim_SUB : string * riscv_op prim_constructor **)

let prim_SUB =
  ("SUB", (primM SUB))

(** val riscv_slt_semi : sem_tuple -> sem_tuple -> sem_tuple **)

let riscv_slt_semi wn wm =
  if wlt riscv_decl.reg_size Signed wn wm
  then GRing.one
         (GRing.ComRing.Exports.coq_GRing_ComRing__to__GRing_SemiRing
           (word riscv_decl.reg_size))
  else GRing.zero
         (GRing.SemiRing.Exports.coq_GRing_SemiRing__to__GRing_Nmodule
           (GRing.ComRing.Exports.coq_GRing_ComRing__to__GRing_SemiRing
             (word riscv_decl.reg_size)))

(** val riscv_SLT_instr :
    (register, empty, empty, empty, condt) instr_desc_t **)

let riscv_SLT_instr =
  coq_RTypeInstruction riscv_decl.reg_size (Obj.magic riscv_slt_semi) "SLT"
    "slt"

(** val prim_SLT : string * riscv_op prim_constructor **)

let prim_SLT =
  ("SLT", (primM SLT))

(** val riscv_SLTI_instr :
    (register, empty, empty, empty, condt) instr_desc_t **)

let riscv_SLTI_instr =
  coq_ITypeInstruction_12s riscv_decl.reg_size (Obj.magic riscv_slt_semi)
    "SLTI" "slti"

(** val prim_SLTI : string * riscv_op prim_constructor **)

let prim_SLTI =
  ("SLTI", (primM SLTI))

(** val riscv_sltu_semi : sem_tuple -> sem_tuple -> sem_tuple **)

let riscv_sltu_semi wn wm =
  if wlt riscv_decl.reg_size Unsigned wn wm
  then GRing.one
         (GRing.ComRing.Exports.coq_GRing_ComRing__to__GRing_SemiRing
           (word riscv_decl.reg_size))
  else GRing.zero
         (GRing.SemiRing.Exports.coq_GRing_SemiRing__to__GRing_Nmodule
           (GRing.ComRing.Exports.coq_GRing_ComRing__to__GRing_SemiRing
             (word riscv_decl.reg_size)))

(** val riscv_SLTU_instr :
    (register, empty, empty, empty, condt) instr_desc_t **)

let riscv_SLTU_instr =
  coq_RTypeInstruction riscv_decl.reg_size (Obj.magic riscv_sltu_semi) "SLTU"
    "sltu"

(** val prim_SLTU : string * riscv_op prim_constructor **)

let prim_SLTU =
  ("SLTU", (primM SLTU))

(** val riscv_SLTIU_instr :
    (register, empty, empty, empty, condt) instr_desc_t **)

let riscv_SLTIU_instr =
  coq_ITypeInstruction_12s riscv_decl.reg_size (Obj.magic riscv_sltu_semi)
    "SLTIU" "sltiu"

(** val prim_SLTIU : string * riscv_op prim_constructor **)

let prim_SLTIU =
  ("SLTIU", (primM SLTIU))

(** val riscv_and_semi : sem_tuple -> sem_tuple -> sem_tuple **)

let riscv_and_semi wn wm =
  wand riscv_decl.reg_size wn wm

(** val riscv_AND_instr :
    (register, empty, empty, empty, condt) instr_desc_t **)

let riscv_AND_instr =
  coq_RTypeInstruction riscv_decl.reg_size (Obj.magic riscv_and_semi) "AND"
    "and"

(** val prim_AND : string * riscv_op prim_constructor **)

let prim_AND =
  ("AND", (primM AND))

(** val riscv_ANDI_instr :
    (register, empty, empty, empty, condt) instr_desc_t **)

let riscv_ANDI_instr =
  coq_ITypeInstruction_12s riscv_decl.reg_size (Obj.magic riscv_and_semi)
    "ANDI" "andi"

(** val prim_ANDI : string * riscv_op prim_constructor **)

let prim_ANDI =
  ("ANDI", (primM ANDI))

(** val riscv_or_semi : sem_tuple -> sem_tuple -> sem_tuple **)

let riscv_or_semi wn wm =
  wor riscv_decl.reg_size wn wm

(** val riscv_OR_instr :
    (register, empty, empty, empty, condt) instr_desc_t **)

let riscv_OR_instr =
  coq_RTypeInstruction riscv_decl.reg_size (Obj.magic riscv_or_semi) "OR" "or"

(** val prim_OR : string * riscv_op prim_constructor **)

let prim_OR =
  ("OR", (primM OR))

(** val riscv_ORI_instr :
    (register, empty, empty, empty, condt) instr_desc_t **)

let riscv_ORI_instr =
  coq_ITypeInstruction_12s riscv_decl.reg_size (Obj.magic riscv_or_semi)
    "ORI" "ori"

(** val prim_ORI : string * riscv_op prim_constructor **)

let prim_ORI =
  ("ORI", (primM ORI))

(** val riscv_xor_semi : sem_tuple -> sem_tuple -> sem_tuple **)

let riscv_xor_semi wn wm =
  wxor riscv_decl.reg_size wn wm

(** val riscv_XOR_instr :
    (register, empty, empty, empty, condt) instr_desc_t **)

let riscv_XOR_instr =
  coq_RTypeInstruction riscv_decl.reg_size (Obj.magic riscv_xor_semi) "XOR"
    "xor"

(** val prim_XOR : string * riscv_op prim_constructor **)

let prim_XOR =
  ("XOR", (primM XOR))

(** val riscv_XORI_instr :
    (register, empty, empty, empty, condt) instr_desc_t **)

let riscv_XORI_instr =
  coq_ITypeInstruction_12s riscv_decl.reg_size (Obj.magic riscv_xor_semi)
    "XORI" "xori"

(** val prim_XORI : string * riscv_op prim_constructor **)

let prim_XORI =
  ("XORI", (primM XORI))

(** val riscv_sll_semi : sem_tuple -> GRing.ComRing.sort -> sem_tuple **)

let riscv_sll_semi wn wm =
  wshl riscv_decl.reg_size wn
    (wunsigned U8
      (wand U8 wm
        (wrepr U8 (Zpos (Coq_xI (Coq_xI (Coq_xI (Coq_xI Coq_xH))))))))

(** val riscv_SLL_instr :
    (register, empty, empty, empty, condt) instr_desc_t **)

let riscv_SLL_instr =
  coq_RTypeInstruction U8 (Obj.magic riscv_sll_semi) "SLL" "sll"

(** val prim_SLL : string * riscv_op prim_constructor **)

let prim_SLL =
  ("SLL", (primM SLL))

(** val riscv_SLLI_instr :
    (register, empty, empty, empty, condt) instr_desc_t **)

let riscv_SLLI_instr =
  coq_ITypeInstruction_5u U8 (Obj.magic riscv_sll_semi) "SLLI" "slli"

(** val prim_SLLI : string * riscv_op prim_constructor **)

let prim_SLLI =
  ("SLLI", (primM SLLI))

(** val riscv_srl_semi : sem_tuple -> GRing.ComRing.sort -> sem_tuple **)

let riscv_srl_semi wn wm =
  wshr riscv_decl.reg_size wn
    (wunsigned U8
      (wand U8 wm
        (wrepr U8 (Zpos (Coq_xI (Coq_xI (Coq_xI (Coq_xI Coq_xH))))))))

(** val riscv_SRL_instr :
    (register, empty, empty, empty, condt) instr_desc_t **)

let riscv_SRL_instr =
  coq_RTypeInstruction U8 (Obj.magic riscv_srl_semi) "SRL" "srl"

(** val prim_SRL : string * riscv_op prim_constructor **)

let prim_SRL =
  ("SRL", (primM SRL))

(** val riscv_SRLI_instr :
    (register, empty, empty, empty, condt) instr_desc_t **)

let riscv_SRLI_instr =
  coq_ITypeInstruction_5u U8 (Obj.magic riscv_srl_semi) "SRLI" "srli"

(** val prim_SRLI : string * riscv_op prim_constructor **)

let prim_SRLI =
  ("SRLI", (primM SRLI))

(** val riscv_sra_semi : sem_tuple -> GRing.ComRing.sort -> sem_tuple **)

let riscv_sra_semi wn wm =
  wsar riscv_decl.reg_size wn
    (wunsigned U8
      (wand U8 wm
        (wrepr U8 (Zpos (Coq_xI (Coq_xI (Coq_xI (Coq_xI Coq_xH))))))))

(** val riscv_SRA_instr :
    (register, empty, empty, empty, condt) instr_desc_t **)

let riscv_SRA_instr =
  coq_RTypeInstruction U8 (Obj.magic riscv_sra_semi) "SRA" "sra"

(** val prim_SRA : string * riscv_op prim_constructor **)

let prim_SRA =
  ("SRA", (primM SRA))

(** val riscv_SRAI_instr :
    (register, empty, empty, empty, condt) instr_desc_t **)

let riscv_SRAI_instr =
  coq_ITypeInstruction_5u U8 (Obj.magic riscv_sra_semi) "SRAI" "srai"

(** val prim_SRAI : string * riscv_op prim_constructor **)

let prim_SRAI =
  ("SRAI", (primM SRAI))

(** val riscv_MV_semi : sem_tuple -> sem_tuple **)

let riscv_MV_semi wn =
  wn

(** val riscv_MV_instr :
    (register, empty, empty, empty, condt) instr_desc_t **)

let riscv_MV_instr =
  let tin = (lreg riscv_decl) :: [] in
  { id_valid = true; id_msb_flag = MSB_MERGE; id_tin = tin; id_in =
  ((coq_Ea riscv_decl (S O)) :: []); id_tout = ((lreg riscv_decl) :: []);
  id_out = ((coq_Ea riscv_decl O) :: []); id_semi =
  (sem_prod_ok (map eval_ltype tin) (Obj.magic riscv_MV_semi));
  id_args_kinds = ak_reg_reg; id_nargs = (S (S O)); id_str_jas = (pp_s "MV");
  id_safe = []; id_pp_asm = (pp_name "mv") }

(** val prim_MV : string * riscv_op prim_constructor **)

let prim_MV =
  ("MV", (primM MV))

(** val riscv_LA_semi : sem_tuple -> sem_tuple **)

let riscv_LA_semi wn =
  wn

(** val riscv_LA_instr :
    (register, empty, empty, empty, condt) instr_desc_t **)

let riscv_LA_instr =
  let tin = (lreg riscv_decl) :: [] in
  { id_valid = true; id_msb_flag = MSB_MERGE; id_tin =
  ((lreg riscv_decl) :: []); id_in = ((coq_Ec riscv_decl (S O)) :: []);
  id_tout = ((lreg riscv_decl) :: []); id_out =
  ((coq_Ea riscv_decl O) :: []); id_semi =
  (sem_prod_ok (map eval_ltype tin) (Obj.magic riscv_LA_semi));
  id_args_kinds = ak_reg_addr; id_nargs = (S (S O)); id_str_jas =
  (pp_s "LA"); id_safe = []; id_pp_asm = (pp_name "la") }

(** val prim_LA : string * riscv_op prim_constructor **)

let prim_LA =
  ("LA", (primM LA))

(** val riscv_LI_semi : sem_tuple -> sem_tuple **)

let riscv_LI_semi wn =
  wn

(** val riscv_LI_instr :
    (register, empty, empty, empty, condt) instr_desc_t **)

let riscv_LI_instr =
  let tin = (lreg riscv_decl) :: [] in
  { id_valid = true; id_msb_flag = MSB_MERGE; id_tin = tin; id_in =
  ((coq_Ea riscv_decl (S O)) :: []); id_tout = ((lreg riscv_decl) :: []);
  id_out = ((coq_Ea riscv_decl O) :: []); id_semi =
  (sem_prod_ok (map eval_ltype tin) (Obj.magic riscv_LI_semi));
  id_args_kinds = (ak_reg_imm riscv_decl); id_nargs = (S (S O)); id_str_jas =
  (pp_s "LI"); id_safe = []; id_pp_asm = (pp_name "li") }

(** val prim_LI : string * riscv_op prim_constructor **)

let prim_LI =
  ("LI", (primM LI))

(** val riscv_NOT_semi : sem_tuple -> sem_tuple **)

let riscv_NOT_semi wn =
  wnot riscv_decl.reg_size wn

(** val riscv_NOT_instr :
    (register, empty, empty, empty, condt) instr_desc_t **)

let riscv_NOT_instr =
  let tin = (lreg riscv_decl) :: [] in
  { id_valid = true; id_msb_flag = MSB_MERGE; id_tin = tin; id_in =
  ((coq_Ea riscv_decl (S O)) :: []); id_tout = ((lreg riscv_decl) :: []);
  id_out = ((coq_Ea riscv_decl O) :: []); id_semi =
  (sem_prod_ok (map eval_ltype tin) (Obj.magic riscv_NOT_semi));
  id_args_kinds = ak_reg_reg; id_nargs = (S (S O)); id_str_jas =
  (pp_s "NOT"); id_safe = []; id_pp_asm = (pp_name "not") }

(** val prim_NOT : string * riscv_op prim_constructor **)

let prim_NOT =
  ("NOT", (primM NOT))

(** val riscv_NEG_semi : sem_tuple -> sem_tuple **)

let riscv_NEG_semi wn =
  GRing.opp
    (GRing.ComRing.Exports.coq_GRing_ComRing__to__GRing_Zmodule
      (word riscv_decl.reg_size)) wn

(** val riscv_NEG_instr :
    (register, empty, empty, empty, condt) instr_desc_t **)

let riscv_NEG_instr =
  let tin = (lreg riscv_decl) :: [] in
  { id_valid = true; id_msb_flag = MSB_MERGE; id_tin = tin; id_in =
  ((coq_Ea riscv_decl (S O)) :: []); id_tout = ((lreg riscv_decl) :: []);
  id_out = ((coq_Ea riscv_decl O) :: []); id_semi =
  (sem_prod_ok (map eval_ltype tin) (Obj.magic riscv_NEG_semi));
  id_args_kinds = ak_reg_reg; id_nargs = (S (S O)); id_str_jas =
  (pp_s "NEG"); id_safe = []; id_pp_asm = (pp_name "neg") }

(** val prim_NEG : string * riscv_op prim_constructor **)

let prim_NEG =
  ("NEG", (primM NOT))

(** val string_of_sign : signedness -> string **)

let string_of_sign = function
| Signed -> ""
| Unsigned -> "u"

(** val string_of_size : wsize -> string **)

let string_of_size = function
| U8 -> "b"
| U16 -> "h"
| U32 -> "w"
| _ -> ""

(** val pp_sign_sz : string -> signedness -> wsize -> unit -> string **)

let pp_sign_sz s sign sz _ =
  (^) s
    ((^) "_"
      ((^) (match sign with
            | Signed -> "s"
            | Unsigned -> "u") (string_of_wsize sz)))

(** val riscv_extend_semi :
    signedness -> wsize -> wsize -> GRing.ComRing.sort -> GRing.ComRing.sort **)

let riscv_extend_semi s ws' ws w =
  match s with
  | Signed -> sign_extend ws' ws w
  | Unsigned -> zero_extend ws' ws w

(** val riscv_LOAD_instr :
    signedness -> wsize -> (register, empty, empty, empty, condt) instr_desc_t **)

let riscv_LOAD_instr s ws =
  let tin = (Coq_lword ws) :: [] in
  let semi = riscv_extend_semi s riscv_decl.reg_size ws in
  { id_valid =
  (match s with
   | Signed -> cmp_le wsize_cmp ws U32
   | Unsigned -> cmp_le wsize_cmp ws U16); id_msb_flag = MSB_MERGE; id_tin =
  tin; id_in = ((coq_Eu riscv_decl (S O)) :: []); id_tout =
  ((lreg riscv_decl) :: []); id_out = ((coq_Ea riscv_decl O) :: []);
  id_semi = (sem_prod_ok (map eval_ltype tin) (Obj.magic semi));
  id_args_kinds = ak_reg_addr; id_nargs = (S (S O)); id_str_jas =
  (pp_sign_sz "LOAD" s ws); id_safe = []; id_pp_asm =
  (pp_name ((^) "l" ((^) (string_of_size ws) (string_of_sign s)))) }

(** val primS :
    (signedness -> wsize -> riscv_op) -> riscv_op prim_constructor **)

let primS f =
  PrimX86
    ((cat (map (fun ws -> PVs (Signed, ws)) (U8 :: (U16 :: (U32 :: []))))
       (map (fun ws -> PVs (Unsigned, ws)) (U8 :: (U16 :: [])))), (fun s ->
    match s with
    | PVs (sg, ws) -> Some (f sg ws)
    | _ -> None))

(** val prim_LOAD : string * riscv_op prim_constructor **)

let prim_LOAD =
  ("LOAD", (primS (fun x x0 -> LOAD (x, x0))))

(** val riscv_STORE_instr :
    wsize -> (register, empty, empty, empty, condt) instr_desc_t **)

let riscv_STORE_instr ws =
  let tin = (Coq_lword ws) :: [] in
  let semi = riscv_extend_semi Unsigned ws ws in
  { id_valid = (cmp_le wsize_cmp ws U32); id_msb_flag = MSB_MERGE; id_tin =
  ((Coq_lword ws) :: []); id_in = ((coq_Ea riscv_decl O) :: []); id_tout =
  ((Coq_lword ws) :: []); id_out = ((coq_Eu riscv_decl (S O)) :: []);
  id_semi = (sem_prod_ok (map eval_ltype tin) (Obj.magic semi));
  id_args_kinds = ak_reg_addr; id_nargs = (S (S O)); id_str_jas =
  (pp_sz "STORE" ws); id_safe = []; id_pp_asm =
  (pp_name ((^) "s" (string_of_size ws))) }

(** val prim_STORE : string * riscv_op prim_constructor **)

let prim_STORE =
  ("STORE", (primP (arch_pd riscv_decl) (fun x -> STORE x)))

(** val riscv_mul_semi : sem_tuple -> sem_tuple -> sem_tuple **)

let riscv_mul_semi wn wm =
  GRing.mul
    (GRing.ComRing.Exports.coq_GRing_ComRing__to__GRing_SemiRing
      (word riscv_decl.reg_size)) wn wm

(** val riscv_MUL_instr :
    (register, empty, empty, empty, condt) instr_desc_t **)

let riscv_MUL_instr =
  coq_RTypeInstruction riscv_decl.reg_size (Obj.magic riscv_mul_semi) "MUL"
    "mul"

(** val prim_MUL : string * riscv_op prim_constructor **)

let prim_MUL =
  ("MUL", (primM MUL))

(** val riscv_mulh_semi : sem_tuple -> sem_tuple -> sem_tuple **)

let riscv_mulh_semi wn wm =
  wmulhs riscv_decl.reg_size wn wm

(** val riscv_MULH_instr :
    (register, empty, empty, empty, condt) instr_desc_t **)

let riscv_MULH_instr =
  coq_RTypeInstruction riscv_decl.reg_size (Obj.magic riscv_mulh_semi) "MULH"
    "mulh"

(** val prim_MULH : string * riscv_op prim_constructor **)

let prim_MULH =
  ("MULH", (primM MULH))

(** val riscv_mulhu_semi : sem_tuple -> sem_tuple -> sem_tuple **)

let riscv_mulhu_semi wn wm =
  wmulhu riscv_decl.reg_size wn wm

(** val riscv_MULHU_instr :
    (register, empty, empty, empty, condt) instr_desc_t **)

let riscv_MULHU_instr =
  coq_RTypeInstruction riscv_decl.reg_size (Obj.magic riscv_mulhu_semi)
    "MULHU" "mulhu"

(** val prim_MULHU : string * riscv_op prim_constructor **)

let prim_MULHU =
  ("MULHU", (primM MULHU))

(** val riscv_mulhsu_semi : sem_tuple -> sem_tuple -> sem_tuple **)

let riscv_mulhsu_semi wn wm =
  wmulhsu riscv_decl.reg_size wn wm

(** val riscv_MULHSU_instr :
    (register, empty, empty, empty, condt) instr_desc_t **)

let riscv_MULHSU_instr =
  coq_RTypeInstruction riscv_decl.reg_size (Obj.magic riscv_mulhsu_semi)
    "MULHSU" "mulhsu"

(** val prim_MULHSU : string * riscv_op prim_constructor **)

let prim_MULHSU =
  ("MULHSU", (primM MULHSU))

(** val riscv_div_semi : sem_tuple -> sem_tuple -> sem_tuple **)

let riscv_div_semi wn wm =
  if eq_op
       (GRing.ComRing.Exports.coq_GRing_ComRing__to__eqtype_Equality
         (word riscv_decl.reg_size)) wm
       (GRing.zero
         (GRing.ComRing.Exports.coq_GRing_ComRing__to__GRing_Nmodule
           (word riscv_decl.reg_size)))
  then GRing.opp
         (GRing.Ring.Exports.join_GRing_Ring_between_GRing_SemiRing_and_GRing_Zmodule
           (GRing.ComRing.Exports.coq_GRing_ComRing__to__GRing_Ring
             (word riscv_decl.reg_size)))
         (GRing.one
           (GRing.Ring.Exports.coq_GRing_Ring__to__GRing_SemiRing
             (GRing.ComRing.Exports.coq_GRing_ComRing__to__GRing_Ring
               (word riscv_decl.reg_size))))
  else wdivi riscv_decl.reg_size wn wm

(** val riscv_DIV_instr :
    (register, empty, empty, empty, condt) instr_desc_t **)

let riscv_DIV_instr =
  coq_RTypeInstruction riscv_decl.reg_size (Obj.magic riscv_div_semi) "DIV"
    "div"

(** val prim_DIV : string * riscv_op prim_constructor **)

let prim_DIV =
  ("DIV", (primM DIV))

(** val riscv_divu_semi : sem_tuple -> sem_tuple -> sem_tuple **)

let riscv_divu_semi wn wm =
  if eq_op
       (GRing.ComRing.Exports.coq_GRing_ComRing__to__eqtype_Equality
         (word riscv_decl.reg_size)) wm
       (GRing.zero
         (GRing.ComRing.Exports.coq_GRing_ComRing__to__GRing_Nmodule
           (word riscv_decl.reg_size)))
  then GRing.opp
         (GRing.Ring.Exports.join_GRing_Ring_between_GRing_SemiRing_and_GRing_Zmodule
           (GRing.ComRing.Exports.coq_GRing_ComRing__to__GRing_Ring
             (word riscv_decl.reg_size)))
         (GRing.one
           (GRing.Ring.Exports.coq_GRing_Ring__to__GRing_SemiRing
             (GRing.ComRing.Exports.coq_GRing_ComRing__to__GRing_Ring
               (word riscv_decl.reg_size))))
  else wdiv riscv_decl.reg_size wn wm

(** val riscv_DIVU_instr :
    (register, empty, empty, empty, condt) instr_desc_t **)

let riscv_DIVU_instr =
  coq_RTypeInstruction riscv_decl.reg_size (Obj.magic riscv_divu_semi) "DIVU"
    "divu"

(** val prim_DIVU : string * riscv_op prim_constructor **)

let prim_DIVU =
  ("DIVU", (primM DIVU))

(** val riscv_rem_semi : sem_tuple -> sem_tuple -> sem_tuple **)

let riscv_rem_semi wn wm =
  wmodi riscv_decl.reg_size wn wm

(** val riscv_REM_instr :
    (register, empty, empty, empty, condt) instr_desc_t **)

let riscv_REM_instr =
  coq_RTypeInstruction riscv_decl.reg_size (Obj.magic riscv_rem_semi) "REM"
    "rem"

(** val prim_REM : string * riscv_op prim_constructor **)

let prim_REM =
  ("REM", (primM REM))

(** val riscv_remu_semi : sem_tuple -> sem_tuple -> sem_tuple **)

let riscv_remu_semi wn wm =
  wmod riscv_decl.reg_size wn wm

(** val riscv_REMU_instr :
    (register, empty, empty, empty, condt) instr_desc_t **)

let riscv_REMU_instr =
  coq_RTypeInstruction riscv_decl.reg_size (Obj.magic riscv_remu_semi) "REMU"
    "remu"

(** val prim_REMU : string * riscv_op prim_constructor **)

let prim_REMU =
  ("REMU", (primM REMU))

(** val riscv_instr_desc :
    riscv_op -> (register, empty, empty, empty, condt) instr_desc_t **)

let riscv_instr_desc = function
| ADD -> riscv_ADD_instr
| ADDI -> riscv_ADDI_instr
| SUB -> riscv_SUB_instr
| SLT -> riscv_SLT_instr
| SLTI -> riscv_SLTI_instr
| SLTU -> riscv_SLTU_instr
| SLTIU -> riscv_SLTIU_instr
| AND -> riscv_AND_instr
| ANDI -> riscv_ANDI_instr
| OR -> riscv_OR_instr
| ORI -> riscv_ORI_instr
| XOR -> riscv_XOR_instr
| XORI -> riscv_XORI_instr
| SLL -> riscv_SLL_instr
| SLLI -> riscv_SLLI_instr
| SRL -> riscv_SRL_instr
| SRLI -> riscv_SRLI_instr
| SRA -> riscv_SRA_instr
| SRAI -> riscv_SRAI_instr
| MV -> riscv_MV_instr
| LA -> riscv_LA_instr
| LI -> riscv_LI_instr
| NOT -> riscv_NOT_instr
| NEG -> riscv_NEG_instr
| LOAD (s, ws) -> riscv_LOAD_instr s ws
| STORE ws -> riscv_STORE_instr ws
| MUL -> riscv_MUL_instr
| MULH -> riscv_MULH_instr
| MULHU -> riscv_MULHU_instr
| MULHSU -> riscv_MULHSU_instr
| DIV -> riscv_DIV_instr
| DIVU -> riscv_DIVU_instr
| REM -> riscv_REM_instr
| REMU -> riscv_REMU_instr

(** val riscv_prim_string : (string * riscv_op prim_constructor) list **)

let riscv_prim_string =
  prim_ADD :: (prim_ADDI :: (prim_SUB :: (prim_SLT :: (prim_SLTI :: (prim_SLTU :: (prim_SLTIU :: (prim_AND :: (prim_ANDI :: (prim_OR :: (prim_ORI :: (prim_XOR :: (prim_XORI :: (prim_SLL :: (prim_SLLI :: (prim_SRL :: (prim_SRLI :: (prim_SRA :: (prim_SRAI :: (prim_MV :: (prim_LA :: (prim_LI :: (prim_NOT :: (prim_NEG :: (prim_LOAD :: (prim_STORE :: (prim_MUL :: (prim_MULH :: (prim_MULHU :: (prim_MULHSU :: (prim_DIV :: (prim_DIVU :: (prim_REM :: (prim_REMU :: [])))))))))))))))))))))))))))))))))

(** val riscv_op_decl :
    (register, empty, empty, empty, condt, riscv_op) asm_op_decl **)

let riscv_op_decl =
  { Arch_decl._eqT = eqTC_riscv_op; instr_desc_op = riscv_instr_desc;
    Arch_decl.prim_string = riscv_prim_string }

type riscv_prog = (register, empty, empty, empty, condt, riscv_op) asm_prog

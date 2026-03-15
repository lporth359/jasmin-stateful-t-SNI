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

module E :
 sig
  val no_semantics : error
 end

val pp_name :
  string -> (register, empty, empty, empty, condt) asm_arg list -> (register,
  empty, empty, empty, condt) pp_asm_op

val coq_RTypeInstruction :
  wsize -> sem_tuple sem_prod -> string -> string -> (register, empty, empty,
  empty, condt) instr_desc_t

val coq_ITypeInstruction :
  caimm_checker_s -> wsize -> sem_tuple sem_prod -> string -> string ->
  (register, empty, empty, empty, condt) instr_desc_t

val coq_ITypeInstruction_12s :
  wsize -> sem_tuple sem_prod -> string -> string -> (register, empty, empty,
  empty, condt) instr_desc_t

val coq_ITypeInstruction_5u :
  wsize -> sem_tuple sem_prod -> string -> string -> (register, empty, empty,
  empty, condt) instr_desc_t

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

val riscv_op_rect :
  'a1 -> 'a1 -> 'a1 -> 'a1 -> 'a1 -> 'a1 -> 'a1 -> 'a1 -> 'a1 -> 'a1 -> 'a1
  -> 'a1 -> 'a1 -> 'a1 -> 'a1 -> 'a1 -> 'a1 -> 'a1 -> 'a1 -> 'a1 -> 'a1 ->
  'a1 -> 'a1 -> 'a1 -> (signedness -> wsize -> 'a1) -> (wsize -> 'a1) -> 'a1
  -> 'a1 -> 'a1 -> 'a1 -> 'a1 -> 'a1 -> 'a1 -> 'a1 -> riscv_op -> 'a1

val riscv_op_rec :
  'a1 -> 'a1 -> 'a1 -> 'a1 -> 'a1 -> 'a1 -> 'a1 -> 'a1 -> 'a1 -> 'a1 -> 'a1
  -> 'a1 -> 'a1 -> 'a1 -> 'a1 -> 'a1 -> 'a1 -> 'a1 -> 'a1 -> 'a1 -> 'a1 ->
  'a1 -> 'a1 -> 'a1 -> (signedness -> wsize -> 'a1) -> (wsize -> 'a1) -> 'a1
  -> 'a1 -> 'a1 -> 'a1 -> 'a1 -> 'a1 -> 'a1 -> 'a1 -> riscv_op -> 'a1

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

val is_riscv_op_rect :
  'a1 -> 'a1 -> 'a1 -> 'a1 -> 'a1 -> 'a1 -> 'a1 -> 'a1 -> 'a1 -> 'a1 -> 'a1
  -> 'a1 -> 'a1 -> 'a1 -> 'a1 -> 'a1 -> 'a1 -> 'a1 -> 'a1 -> 'a1 -> 'a1 ->
  'a1 -> 'a1 -> 'a1 -> (signedness -> is_signedness -> wsize -> is_wsize ->
  'a1) -> (wsize -> is_wsize -> 'a1) -> 'a1 -> 'a1 -> 'a1 -> 'a1 -> 'a1 ->
  'a1 -> 'a1 -> 'a1 -> riscv_op -> is_riscv_op -> 'a1

val is_riscv_op_rec :
  'a1 -> 'a1 -> 'a1 -> 'a1 -> 'a1 -> 'a1 -> 'a1 -> 'a1 -> 'a1 -> 'a1 -> 'a1
  -> 'a1 -> 'a1 -> 'a1 -> 'a1 -> 'a1 -> 'a1 -> 'a1 -> 'a1 -> 'a1 -> 'a1 ->
  'a1 -> 'a1 -> 'a1 -> (signedness -> is_signedness -> wsize -> is_wsize ->
  'a1) -> (wsize -> is_wsize -> 'a1) -> 'a1 -> 'a1 -> 'a1 -> 'a1 -> 'a1 ->
  'a1 -> 'a1 -> 'a1 -> riscv_op -> is_riscv_op -> 'a1

val riscv_op_tag : riscv_op -> positive

val is_riscv_op_inhab : riscv_op -> is_riscv_op

val is_riscv_op_functor : riscv_op -> is_riscv_op -> is_riscv_op

type box_riscv_op_ADD =
| Box_riscv_op_ADD

type box_riscv_op_LOAD = { coq_Box_riscv_op_LOAD_0 : signedness;
                           coq_Box_riscv_op_LOAD_1 : wsize }

val coq_Box_riscv_op_LOAD_0 : box_riscv_op_LOAD -> signedness

val coq_Box_riscv_op_LOAD_1 : box_riscv_op_LOAD -> wsize

type box_riscv_op_STORE =
  wsize
  (* singleton inductive, whose constructor was Box_riscv_op_STORE *)

val coq_Box_riscv_op_STORE_0 : box_riscv_op_STORE -> wsize

type riscv_op_fields_t = __

val riscv_op_fields : riscv_op -> riscv_op_fields_t

val riscv_op_construct : positive -> riscv_op_fields_t -> riscv_op option

val riscv_op_induction :
  'a1 -> 'a1 -> 'a1 -> 'a1 -> 'a1 -> 'a1 -> 'a1 -> 'a1 -> 'a1 -> 'a1 -> 'a1
  -> 'a1 -> 'a1 -> 'a1 -> 'a1 -> 'a1 -> 'a1 -> 'a1 -> 'a1 -> 'a1 -> 'a1 ->
  'a1 -> 'a1 -> 'a1 -> (signedness -> is_signedness -> wsize -> is_wsize ->
  'a1) -> (wsize -> is_wsize -> 'a1) -> 'a1 -> 'a1 -> 'a1 -> 'a1 -> 'a1 ->
  'a1 -> 'a1 -> 'a1 -> riscv_op -> is_riscv_op -> 'a1

val riscv_op_eqb_fields :
  (riscv_op -> riscv_op -> bool) -> positive -> riscv_op_fields_t ->
  riscv_op_fields_t -> bool

val riscv_op_eqb : riscv_op -> riscv_op -> bool

val riscv_op_eqb_OK : riscv_op -> riscv_op -> reflect

val riscv_op_eqb_OK_sumbool : riscv_op -> riscv_op -> bool

val eqTC_riscv_op : riscv_op eqTypeC

val riscv_op_eqType : Equality.coq_type

val riscv_add_semi : sem_tuple -> sem_tuple -> sem_tuple

val riscv_ADD_instr : (register, empty, empty, empty, condt) instr_desc_t

val prim_ADD : string * riscv_op prim_constructor

val riscv_ADDI_instr : (register, empty, empty, empty, condt) instr_desc_t

val prim_ADDI : string * riscv_op prim_constructor

val riscv_sub_semi : sem_tuple -> sem_tuple -> sem_tuple

val riscv_SUB_instr : (register, empty, empty, empty, condt) instr_desc_t

val prim_SUB : string * riscv_op prim_constructor

val riscv_slt_semi : sem_tuple -> sem_tuple -> sem_tuple

val riscv_SLT_instr : (register, empty, empty, empty, condt) instr_desc_t

val prim_SLT : string * riscv_op prim_constructor

val riscv_SLTI_instr : (register, empty, empty, empty, condt) instr_desc_t

val prim_SLTI : string * riscv_op prim_constructor

val riscv_sltu_semi : sem_tuple -> sem_tuple -> sem_tuple

val riscv_SLTU_instr : (register, empty, empty, empty, condt) instr_desc_t

val prim_SLTU : string * riscv_op prim_constructor

val riscv_SLTIU_instr : (register, empty, empty, empty, condt) instr_desc_t

val prim_SLTIU : string * riscv_op prim_constructor

val riscv_and_semi : sem_tuple -> sem_tuple -> sem_tuple

val riscv_AND_instr : (register, empty, empty, empty, condt) instr_desc_t

val prim_AND : string * riscv_op prim_constructor

val riscv_ANDI_instr : (register, empty, empty, empty, condt) instr_desc_t

val prim_ANDI : string * riscv_op prim_constructor

val riscv_or_semi : sem_tuple -> sem_tuple -> sem_tuple

val riscv_OR_instr : (register, empty, empty, empty, condt) instr_desc_t

val prim_OR : string * riscv_op prim_constructor

val riscv_ORI_instr : (register, empty, empty, empty, condt) instr_desc_t

val prim_ORI : string * riscv_op prim_constructor

val riscv_xor_semi : sem_tuple -> sem_tuple -> sem_tuple

val riscv_XOR_instr : (register, empty, empty, empty, condt) instr_desc_t

val prim_XOR : string * riscv_op prim_constructor

val riscv_XORI_instr : (register, empty, empty, empty, condt) instr_desc_t

val prim_XORI : string * riscv_op prim_constructor

val riscv_sll_semi : sem_tuple -> GRing.ComRing.sort -> sem_tuple

val riscv_SLL_instr : (register, empty, empty, empty, condt) instr_desc_t

val prim_SLL : string * riscv_op prim_constructor

val riscv_SLLI_instr : (register, empty, empty, empty, condt) instr_desc_t

val prim_SLLI : string * riscv_op prim_constructor

val riscv_srl_semi : sem_tuple -> GRing.ComRing.sort -> sem_tuple

val riscv_SRL_instr : (register, empty, empty, empty, condt) instr_desc_t

val prim_SRL : string * riscv_op prim_constructor

val riscv_SRLI_instr : (register, empty, empty, empty, condt) instr_desc_t

val prim_SRLI : string * riscv_op prim_constructor

val riscv_sra_semi : sem_tuple -> GRing.ComRing.sort -> sem_tuple

val riscv_SRA_instr : (register, empty, empty, empty, condt) instr_desc_t

val prim_SRA : string * riscv_op prim_constructor

val riscv_SRAI_instr : (register, empty, empty, empty, condt) instr_desc_t

val prim_SRAI : string * riscv_op prim_constructor

val riscv_MV_semi : sem_tuple -> sem_tuple

val riscv_MV_instr : (register, empty, empty, empty, condt) instr_desc_t

val prim_MV : string * riscv_op prim_constructor

val riscv_LA_semi : sem_tuple -> sem_tuple

val riscv_LA_instr : (register, empty, empty, empty, condt) instr_desc_t

val prim_LA : string * riscv_op prim_constructor

val riscv_LI_semi : sem_tuple -> sem_tuple

val riscv_LI_instr : (register, empty, empty, empty, condt) instr_desc_t

val prim_LI : string * riscv_op prim_constructor

val riscv_NOT_semi : sem_tuple -> sem_tuple

val riscv_NOT_instr : (register, empty, empty, empty, condt) instr_desc_t

val prim_NOT : string * riscv_op prim_constructor

val riscv_NEG_semi : sem_tuple -> sem_tuple

val riscv_NEG_instr : (register, empty, empty, empty, condt) instr_desc_t

val prim_NEG : string * riscv_op prim_constructor

val string_of_sign : signedness -> string

val string_of_size : wsize -> string

val pp_sign_sz : string -> signedness -> wsize -> unit -> string

val riscv_extend_semi :
  signedness -> wsize -> wsize -> GRing.ComRing.sort -> GRing.ComRing.sort

val riscv_LOAD_instr :
  signedness -> wsize -> (register, empty, empty, empty, condt) instr_desc_t

val primS : (signedness -> wsize -> riscv_op) -> riscv_op prim_constructor

val prim_LOAD : string * riscv_op prim_constructor

val riscv_STORE_instr :
  wsize -> (register, empty, empty, empty, condt) instr_desc_t

val prim_STORE : string * riscv_op prim_constructor

val riscv_mul_semi : sem_tuple -> sem_tuple -> sem_tuple

val riscv_MUL_instr : (register, empty, empty, empty, condt) instr_desc_t

val prim_MUL : string * riscv_op prim_constructor

val riscv_mulh_semi : sem_tuple -> sem_tuple -> sem_tuple

val riscv_MULH_instr : (register, empty, empty, empty, condt) instr_desc_t

val prim_MULH : string * riscv_op prim_constructor

val riscv_mulhu_semi : sem_tuple -> sem_tuple -> sem_tuple

val riscv_MULHU_instr : (register, empty, empty, empty, condt) instr_desc_t

val prim_MULHU : string * riscv_op prim_constructor

val riscv_mulhsu_semi : sem_tuple -> sem_tuple -> sem_tuple

val riscv_MULHSU_instr : (register, empty, empty, empty, condt) instr_desc_t

val prim_MULHSU : string * riscv_op prim_constructor

val riscv_div_semi : sem_tuple -> sem_tuple -> sem_tuple

val riscv_DIV_instr : (register, empty, empty, empty, condt) instr_desc_t

val prim_DIV : string * riscv_op prim_constructor

val riscv_divu_semi : sem_tuple -> sem_tuple -> sem_tuple

val riscv_DIVU_instr : (register, empty, empty, empty, condt) instr_desc_t

val prim_DIVU : string * riscv_op prim_constructor

val riscv_rem_semi : sem_tuple -> sem_tuple -> sem_tuple

val riscv_REM_instr : (register, empty, empty, empty, condt) instr_desc_t

val prim_REM : string * riscv_op prim_constructor

val riscv_remu_semi : sem_tuple -> sem_tuple -> sem_tuple

val riscv_REMU_instr : (register, empty, empty, empty, condt) instr_desc_t

val prim_REMU : string * riscv_op prim_constructor

val riscv_instr_desc :
  riscv_op -> (register, empty, empty, empty, condt) instr_desc_t

val riscv_prim_string : (string * riscv_op prim_constructor) list

val riscv_op_decl :
  (register, empty, empty, empty, condt, riscv_op) asm_op_decl

type riscv_prog = (register, empty, empty, empty, condt, riscv_op) asm_prog

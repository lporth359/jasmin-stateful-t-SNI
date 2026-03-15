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

val str : instruction_desc -> unit -> string

val tin : instruction_desc -> atype list

val i_in : instruction_desc -> arg_desc list

val tout : instruction_desc -> atype list

val i_out : instruction_desc -> arg_desc list

val conflicts : instruction_desc -> (arg_position * arg_position) list

val semi : instruction_desc -> sem_tuple exec sem_prod

val i_valid : instruction_desc -> bool

val i_safe : instruction_desc -> safe_cond list

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

val _eqT : 'a1 asmOp -> 'a1 eqTypeC

val asm_op_instr : 'a1 asmOp -> 'a1 -> instruction_desc

val prim_string : 'a1 asmOp -> (string * 'a1 prim_constructor) list

type 'asm_op asm_op_t = 'asm_op

type 'asm_op sopn =
| Opseudo_op of pseudo_operator
| Oslh of slh_op
| Oasm of 'asm_op asm_op_t

val sopn_beq : 'a1 asmOp -> 'a1 sopn -> 'a1 sopn -> bool

val sopn_eq_axiom : 'a1 asmOp -> 'a1 sopn eq_axiom

val coq_HB_unnamed_factory_1 : 'a1 asmOp -> 'a1 sopn Coq_hasDecEq.axioms_

val sopn_sopn__canonical__eqtype_Equality : 'a1 asmOp -> Equality.coq_type

val sopn_copy : 'a1 asmOp -> wsize -> positive -> 'a1 sopn

val sopn_nop : 'a1 asmOp -> 'a1 sopn

val sopn_mulu : 'a1 asmOp -> wsize -> 'a1 sopn

val sopn_addcarry : 'a1 asmOp -> wsize -> 'a1 sopn

val sopn_subcarry : 'a1 asmOp -> wsize -> 'a1 sopn

val is_Oslh : 'a1 asmOp -> 'a1 sopn -> slh_op option

val is_spill_op : 'a1 asmOp -> 'a1 sopn -> (spill_op * atype list) option

val coq_Ocopy_instr : wsize -> positive -> instruction_desc

val declassify_semi : ctype -> sem_tuple exec sem_prod

val coq_Odeclassify_instr : atype -> instruction_desc

val coq_Odeclassify_mem_instr :
  coq_PointerData -> positive -> instruction_desc

val coq_Onop_instr : instruction_desc

val coq_Omulu_instr : wsize -> instruction_desc

val coq_Oaddcarry_instr : wsize -> instruction_desc

val coq_Osubcarry_instr : wsize -> instruction_desc

val spill_semi : ctype list -> sem_tuple sem_prod

val coq_Ospill_instr : spill_op -> atype list -> instruction_desc

val coq_Oswap_instr : atype -> instruction_desc

val pseudo_op_get_instr_desc :
  coq_PointerData -> pseudo_operator -> instruction_desc

val se_init_sem : coq_MSFsize -> GRing.ComRing.sort

val se_update_sem :
  coq_MSFsize -> bool -> GRing.ComRing.sort -> GRing.ComRing.sort

val se_move_sem : coq_MSFsize -> GRing.ComRing.sort -> GRing.ComRing.sort

val se_protect_sem :
  coq_MSFsize -> wsize -> GRing.ComRing.sort -> GRing.ComRing.sort ->
  GRing.ComRing.sort

val se_protect_ptr_sem :
  coq_MSFsize -> positive -> WArray.array -> GRing.ComRing.sort ->
  WArray.array

val se_protect_ptr_fail_sem :
  coq_MSFsize -> positive -> WArray.array -> GRing.ComRing.sort ->
  WArray.array exec

val coq_SLHinit_str : string

val coq_SLHinit_instr : coq_MSFsize -> instruction_desc

val coq_SLHupdate_str : string

val coq_SLHupdate_instr : coq_MSFsize -> instruction_desc

val coq_SLHmove_str : string

val coq_SLHmove_instr : coq_MSFsize -> instruction_desc

val coq_SLHprotect_str : string

val coq_SLHprotect_instr : coq_MSFsize -> wsize -> instruction_desc

val coq_SLHprotect_ptr_str : string

val coq_SLHprotect_ptr_instr :
  coq_MSFsize -> wsize -> positive -> instruction_desc

val coq_SLHprotect_ptr_fail_str : string

val coq_SLHprotect_ptr_fail_instr :
  coq_MSFsize -> wsize -> positive -> instruction_desc

val slh_op_instruction_desc : coq_MSFsize -> slh_op -> instruction_desc

val get_instr_desc :
  coq_PointerData -> coq_MSFsize -> 'a1 asmOp -> 'a1 sopn -> instruction_desc

val string_of_sopn :
  coq_PointerData -> coq_MSFsize -> 'a1 asmOp -> 'a1 sopn -> string

val sopn_tin :
  coq_PointerData -> coq_MSFsize -> 'a1 asmOp -> 'a1 sopn -> atype list

val sopn_tout :
  coq_PointerData -> coq_MSFsize -> 'a1 asmOp -> 'a1 sopn -> atype list

val sopn_sem_ :
  coq_PointerData -> coq_MSFsize -> 'a1 asmOp -> 'a1 sopn -> sem_tuple exec
  sem_prod

val sopn_sem :
  coq_PointerData -> coq_MSFsize -> 'a1 asmOp -> 'a1 sopn -> sem_tuple exec
  sem_prod exec

val eqC_sopn : 'a1 asmOp -> 'a1 sopn eqTypeC

val map_prim_constructor :
  ('a1 -> 'a2) -> 'a1 prim_constructor -> 'a2 prim_constructor

val primM : 'a1 -> 'a1 prim_constructor

val primP : coq_PointerData -> (wsize -> 'a1) -> 'a1 prim_constructor

val sopn_prim_string :
  coq_PointerData -> 'a1 asmOp -> (string * 'a1 sopn prim_constructor) list

val asmOp_sopn : coq_PointerData -> coq_MSFsize -> 'a1 asmOp -> 'a1 sopn asmOp

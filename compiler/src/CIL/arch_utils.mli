open Datatypes
open Arch_decl
open Sem_type
open Seq
open Ssrnat
open Type
open Utils0
open Wsize

type empty = |

val of_empty : empty -> 'a1

val eqTC_empty : empty eqTypeC

val finTC_empty : empty finTypeC

val empty_toS : ltype -> empty coq_ToString

val ak_reg_reg : i_args_kinds

val coq_CAimm_sz : wsize -> arg_kind

val ak_reg_imm : ('a1, 'a2, 'a3, 'a4, 'a5) arch_decl -> i_args_kinds

val ak_reg_addr : i_args_kinds

val ak_reg_imm8_imm8 : i_args_kinds

val ak_reg_reg_reg : i_args_kinds

val ak_reg_reg_imm8_imm8 : i_args_kinds

val ak_reg_reg_reg_reg : i_args_kinds

val behead_tuple :
  ltype list -> ltype list -> sem_tuple exec sem_prod -> sem_tuple exec
  sem_prod

val semi_drop2 :
  ltype list -> ltype list -> sem_tuple exec sem_prod -> sem_tuple exec
  sem_prod

val semi_drop3 :
  ltype list -> ltype list -> sem_tuple exec sem_prod -> sem_tuple exec
  sem_prod

val semi_drop4 :
  ltype list -> ltype list -> sem_tuple exec sem_prod -> sem_tuple exec
  sem_prod

val idt_drop2 :
  ('a1, 'a2, 'a3, 'a4, 'a5) arch_decl -> ('a1, 'a2, 'a3, 'a4, 'a5)
  instr_desc_t -> ('a1, 'a2, 'a3, 'a4, 'a5) instr_desc_t

val idt_drop3 :
  ('a1, 'a2, 'a3, 'a4, 'a5) arch_decl -> ('a1, 'a2, 'a3, 'a4, 'a5)
  instr_desc_t -> ('a1, 'a2, 'a3, 'a4, 'a5) instr_desc_t

val idt_drop4 :
  ('a1, 'a2, 'a3, 'a4, 'a5) arch_decl -> ('a1, 'a2, 'a3, 'a4, 'a5)
  instr_desc_t -> ('a1, 'a2, 'a3, 'a4, 'a5) instr_desc_t

val rtuple_drop5th :
  ctype -> ctype -> ctype -> ctype -> ctype -> sem_tuple ->
  sem_ot * (sem_ot * (sem_ot * sem_ot))

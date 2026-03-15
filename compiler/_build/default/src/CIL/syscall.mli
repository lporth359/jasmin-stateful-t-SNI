open BinNums
open Bool
open Eqb_core_defs
open Eqtype
open Ssralg
open Type
open Utils0
open Wsize

val syscall_t_tag :
  (Wsize.wsize * BinNums.positive) Syscall_t.syscall_t -> positive

type box_syscall_t_RandomBytes = { coq_Box_syscall_t_RandomBytes_0 : 
                                   wsize;
                                   coq_Box_syscall_t_RandomBytes_1 : 
                                   positive }

type syscall_t_fields_t = box_syscall_t_RandomBytes

val syscall_t_fields :
  (Wsize.wsize * BinNums.positive) Syscall_t.syscall_t -> syscall_t_fields_t

val syscall_t_eqb_fields :
  ((Wsize.wsize * BinNums.positive) Syscall_t.syscall_t ->
  (Wsize.wsize * BinNums.positive) Syscall_t.syscall_t -> bool) -> positive
  -> box_syscall_t_RandomBytes -> box_syscall_t_RandomBytes -> bool

val syscall_t_eqb :
  (Wsize.wsize * BinNums.positive) Syscall_t.syscall_t ->
  (Wsize.wsize * BinNums.positive) Syscall_t.syscall_t -> bool

val syscall_t_eqb_OK :
  (Wsize.wsize * BinNums.positive) Syscall_t.syscall_t ->
  (Wsize.wsize * BinNums.positive) Syscall_t.syscall_t -> reflect

val coq_HB_unnamed_factory_1 :
  (Wsize.wsize * BinNums.positive) Syscall_t.syscall_t Coq_hasDecEq.axioms_

val syscall_syscall_t__canonical__eqtype_Equality : Equality.coq_type

type syscall_sig_t = { scs_tin : atype list; scs_tout : atype list }

val syscall_sig_u :
  (Wsize.wsize * BinNums.positive) Syscall_t.syscall_t -> syscall_sig_t

val syscall_sig_s :
  coq_PointerData -> (Wsize.wsize * BinNums.positive) Syscall_t.syscall_t ->
  syscall_sig_t

type 'syscall_state syscall_sem =
  'syscall_state -> coq_Z -> 'syscall_state * GRing.ComRing.sort list
  (* singleton inductive, whose constructor was Build_syscall_sem *)

type 'syscall_state syscall_state_t = 'syscall_state

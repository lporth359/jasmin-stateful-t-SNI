open BinNums
open Bool
open Eqb_core_defs
open Eqtype
open Ssralg
open Type
open Utils0
open Wsize

(** val syscall_t_tag :
    (Wsize.wsize * BinNums.positive) Syscall_t.syscall_t -> positive **)

let syscall_t_tag _ =
  Coq_xH

type box_syscall_t_RandomBytes = { coq_Box_syscall_t_RandomBytes_0 : 
                                   wsize;
                                   coq_Box_syscall_t_RandomBytes_1 : 
                                   positive }

type syscall_t_fields_t = box_syscall_t_RandomBytes

(** val syscall_t_fields :
    (Wsize.wsize * BinNums.positive) Syscall_t.syscall_t -> syscall_t_fields_t **)

let syscall_t_fields = function
| Syscall_t.RandomBytes (h, h0) ->
  { coq_Box_syscall_t_RandomBytes_0 = h; coq_Box_syscall_t_RandomBytes_1 =
    h0 }

(** val syscall_t_eqb_fields :
    ((Wsize.wsize * BinNums.positive) Syscall_t.syscall_t ->
    (Wsize.wsize * BinNums.positive) Syscall_t.syscall_t -> bool) -> positive
    -> box_syscall_t_RandomBytes -> box_syscall_t_RandomBytes -> bool **)

let syscall_t_eqb_fields _ _ a b =
  let { coq_Box_syscall_t_RandomBytes_0 = box_syscall_t_RandomBytes_0;
    coq_Box_syscall_t_RandomBytes_1 = box_syscall_t_RandomBytes_1 } = a
  in
  let { coq_Box_syscall_t_RandomBytes_0 = box_syscall_t_RandomBytes_2;
    coq_Box_syscall_t_RandomBytes_1 = box_syscall_t_RandomBytes_3 } = b
  in
  (&&) (wsize_eqb box_syscall_t_RandomBytes_0 box_syscall_t_RandomBytes_2)
    ((&&)
      (positive_eqb box_syscall_t_RandomBytes_1 box_syscall_t_RandomBytes_3)
      true)

(** val syscall_t_eqb :
    (Wsize.wsize * BinNums.positive) Syscall_t.syscall_t ->
    (Wsize.wsize * BinNums.positive) Syscall_t.syscall_t -> bool **)

let syscall_t_eqb x1 x2 =
  let Syscall_t.RandomBytes (h, h0) = x1 in
  eqb_body syscall_t_tag syscall_t_fields
    (syscall_t_eqb_fields (fun _ _ -> true))
    (syscall_t_tag (Syscall_t.RandomBytes (h, h0)))
    { coq_Box_syscall_t_RandomBytes_0 = h; coq_Box_syscall_t_RandomBytes_1 =
    h0 } x2

(** val syscall_t_eqb_OK :
    (Wsize.wsize * BinNums.positive) Syscall_t.syscall_t ->
    (Wsize.wsize * BinNums.positive) Syscall_t.syscall_t -> reflect **)

let syscall_t_eqb_OK =
  iffP2 syscall_t_eqb

(** val coq_HB_unnamed_factory_1 :
    (Wsize.wsize * BinNums.positive) Syscall_t.syscall_t Coq_hasDecEq.axioms_ **)

let coq_HB_unnamed_factory_1 =
  { Coq_hasDecEq.eq_op = syscall_t_eqb; Coq_hasDecEq.eqP = syscall_t_eqb_OK }

(** val syscall_syscall_t__canonical__eqtype_Equality : Equality.coq_type **)

let syscall_syscall_t__canonical__eqtype_Equality =
  Obj.magic coq_HB_unnamed_factory_1

type syscall_sig_t = { scs_tin : atype list; scs_tout : atype list }

(** val syscall_sig_u :
    (Wsize.wsize * BinNums.positive) Syscall_t.syscall_t -> syscall_sig_t **)

let syscall_sig_u = function
| Syscall_t.RandomBytes (ws, len) ->
  { scs_tin = ((Coq_aarr (ws, len)) :: []); scs_tout = ((Coq_aarr (ws,
    len)) :: []) }

(** val syscall_sig_s :
    coq_PointerData -> (Wsize.wsize * BinNums.positive) Syscall_t.syscall_t
    -> syscall_sig_t **)

let syscall_sig_s pd _ =
  { scs_tin = ((Coq_aword pd) :: ((Coq_aword pd) :: [])); scs_tout =
    ((Coq_aword pd) :: []) }

type 'syscall_state syscall_sem =
  'syscall_state -> coq_Z -> 'syscall_state * GRing.ComRing.sort list
  (* singleton inductive, whose constructor was Build_syscall_sem *)

type 'syscall_state syscall_state_t = 'syscall_state

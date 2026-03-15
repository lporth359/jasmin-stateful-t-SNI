open BinInt
open BinNums
open Bool
open Eqb_core_defs
open Eqtype
open Seq
open Ssralg
open Utils0
open Word0
open Word_ssrZ
open Wsize

type __ = Obj.t

module LE =
 struct
  (** val encode : wsize -> GRing.ComRing.sort -> GRing.ComRing.sort list **)

  let encode sz w =
    Obj.magic split_vec sz (nat_of_wsize U8) w

  (** val decode : wsize -> GRing.ComRing.sort list -> GRing.ComRing.sort **)

  let decode sz n =
    make_vec U8 sz n

  (** val wread8 :
      wsize -> GRing.ComRing.sort -> coq_Z -> GRing.Nmodule.sort **)

  let wread8 ws v k =
    nth
      (GRing.zero
        (GRing.ComRing.Exports.coq_GRing_ComRing__to__GRing_Nmodule (word U8)))
      (encode ws v) (Z.to_nat k)
 end

type pointer_op = { add : (Equality.sort -> coq_Z -> Equality.sort);
                    sub : (Equality.sort -> Equality.sort -> coq_Z);
                    p_to_z : (Equality.sort -> coq_Z) }

(** val is_align :
    Equality.coq_type -> pointer_op -> Equality.sort -> wsize -> bool **)

let is_align _ pointer p sz =
  eq_op coq_BinNums_Z__canonical__eqtype_Equality
    (Obj.magic Z.modulo (pointer.p_to_z p) (wsize_size sz)) (Obj.magic Z0)

type 'core_mem coreMem = { get : ('core_mem -> Equality.sort ->
                                 GRing.ComRing.sort exec);
                           set : ('core_mem -> Equality.sort ->
                                 GRing.ComRing.sort -> 'core_mem exec);
                           valid8 : ('core_mem -> Equality.sort -> bool);
                           valid8P : ('core_mem -> Equality.sort ->
                                     GRing.ComRing.sort -> reflect) }

type aligned =
| Unaligned
| Aligned

(** val aligned_tag : aligned -> positive **)

let aligned_tag = function
| Unaligned -> Coq_xH
| Aligned -> Coq_xO Coq_xH

type box_aligned_Unaligned =
| Box_aligned_Unaligned

type aligned_fields_t = __

(** val aligned_fields : aligned -> aligned_fields_t **)

let aligned_fields _ =
  Obj.magic Box_aligned_Unaligned

(** val aligned_eqb_fields :
    (aligned -> aligned -> bool) -> positive -> aligned_fields_t ->
    aligned_fields_t -> bool **)

let aligned_eqb_fields _ _ _ _ =
  true

(** val aligned_eqb : aligned -> aligned -> bool **)

let aligned_eqb x1 x2 =
  eqb_body aligned_tag aligned_fields
    (Obj.magic aligned_eqb_fields (fun _ _ -> true)) (aligned_tag x1)
    Box_aligned_Unaligned x2

(** val aligned_eqb_OK : aligned -> aligned -> reflect **)

let aligned_eqb_OK =
  iffP2 aligned_eqb

(** val coq_HB_unnamed_factory_1 : aligned Coq_hasDecEq.axioms_ **)

let coq_HB_unnamed_factory_1 =
  { Coq_hasDecEq.eq_op = aligned_eqb; Coq_hasDecEq.eqP = aligned_eqb_OK }

(** val memory_model_aligned__canonical__eqtype_Equality :
    Equality.coq_type **)

let memory_model_aligned__canonical__eqtype_Equality =
  Obj.magic coq_HB_unnamed_factory_1

(** val aligned_le : aligned -> aligned -> bool **)

let aligned_le x y =
  (||)
    (eq_op memory_model_aligned__canonical__eqtype_Equality (Obj.magic x)
      (Obj.magic Unaligned))
    (eq_op memory_model_aligned__canonical__eqtype_Equality (Obj.magic y)
      (Obj.magic Aligned))

module CoreMem =
 struct
  (** val is_aligned_if :
      Equality.coq_type -> pointer_op -> aligned -> Equality.sort -> wsize ->
      bool **)

  let is_aligned_if pointer pointer0 al ptr sz =
    match al with
    | Unaligned -> true
    | Aligned -> is_align pointer pointer0 ptr sz

  (** val read :
      Equality.coq_type -> pointer_op -> 'a1 coreMem -> 'a1 -> aligned ->
      Equality.sort -> wsize -> GRing.ComRing.sort exec **)

  let read pointer pointer0 cM m al ptr sz =
    if is_aligned_if pointer pointer0 al ptr sz
    then (match mapM (fun k -> cM.get m (pointer0.add ptr k))
                  (ziota Z0 (wsize_size sz)) with
          | Ok x -> Ok (LE.decode sz x)
          | Error s -> Error s)
    else let s = ErrAddrInvalid in Error s

  (** val write :
      Equality.coq_type -> pointer_op -> 'a1 coreMem -> 'a1 -> aligned ->
      Equality.sort -> wsize -> GRing.ComRing.sort -> 'a1 exec **)

  let write pointer pointer0 cM m al ptr sz w =
    if is_aligned_if pointer pointer0 al ptr sz
    then foldM (fun k m0 ->
           cM.set m0 (pointer0.add ptr k) (LE.wread8 sz w k)) m
           (ziota Z0 (wsize_size sz))
    else let s = ErrAddrInvalid in Error s
 end

(** val coq_PointerW : coq_PointerData -> pointer_op **)

let coq_PointerW pd =
  { add = (fun p k ->
    GRing.add
      (GRing.ComRing.Exports.coq_GRing_ComRing__to__GRing_Nmodule (word pd))
      p (wrepr pd k)); sub = (fun p1 p2 ->
    wunsigned pd
      (GRing.add
        (GRing.ComRing.Exports.coq_GRing_ComRing__to__GRing_Nmodule (word pd))
        p1
        (GRing.opp
          (GRing.ComRing.Exports.coq_GRing_ComRing__to__GRing_Zmodule
            (word pd)) p2))); p_to_z = (fun p -> wunsigned pd p) }

(** val round_ws : wsize -> coq_Z -> coq_Z **)

let round_ws ws sz =
  let d = wsize_size ws in
  let (q, r) = Z.div_eucl sz d in
  if eq_op coq_BinNums_Z__canonical__eqtype_Equality (Obj.magic r)
       (Obj.magic Z0)
  then sz
  else Z.mul (Z.add q (Zpos Coq_xH)) d

type 'mem memory = { stack_root : ('mem -> GRing.ComRing.sort);
                     stack_limit : ('mem -> GRing.ComRing.sort);
                     frames : ('mem -> GRing.ComRing.sort list);
                     alloc_stack : ('mem -> wsize -> coq_Z -> coq_Z -> coq_Z
                                   -> 'mem exec);
                     free_stack : ('mem -> 'mem);
                     init : ((GRing.ComRing.sort * coq_Z) list ->
                            GRing.ComRing.sort -> 'mem exec) }

module type MemoryT =
 sig
  type mem

  val coq_CM : coq_PointerData -> mem coreMem

  val coq_M : coq_PointerData -> mem memory
 end

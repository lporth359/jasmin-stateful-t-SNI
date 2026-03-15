open Datatypes
open Prelude
open Eqtype
open Seq
open Ssrbool
open Ssrnat

type __ = Obj.t
let __ = let rec f _ = Obj.repr f in Obj.repr f

module Coq_isFinite =
 struct
  type 't axioms_ =
    't list
    (* singleton inductive, whose constructor was Axioms_ *)

  (** val enum_subdef :
      'a1 Coq_hasDecEq.axioms_ -> 'a1 axioms_ -> 'a1 list **)

  let enum_subdef _ record =
    record
 end

module Finite =
 struct
  type 't axioms_ = { choice_hasChoice_mixin : 't Choice.Coq_hasChoice.axioms_;
                      choice_Choice_isCountable_mixin : 't
                                                        Choice.Choice_isCountable.axioms_;
                      eqtype_hasDecEq_mixin : 't Coq_hasDecEq.axioms_;
                      fintype_isFinite_mixin : 't Coq_isFinite.axioms_ }

  type coq_type =
    __ axioms_
    (* singleton inductive, whose constructor was Pack *)

  type sort = __

  (** val coq_class : coq_type -> sort axioms_ **)

  let coq_class record =
    record

  module Exports =
   struct
    (** val fintype_Finite_class__to__eqtype_Equality_class :
        'a1 axioms_ -> 'a1 Equality.axioms_ **)

    let fintype_Finite_class__to__eqtype_Equality_class c =
      c.eqtype_hasDecEq_mixin

    (** val fintype_Finite__to__eqtype_Equality :
        coq_type -> Equality.coq_type **)

    let fintype_Finite__to__eqtype_Equality =
      fintype_Finite_class__to__eqtype_Equality_class
   end
 end

module FiniteNES =
 struct
  module Finite =
   struct
    module type Coq_enum_Locked =
     sig
      val body : Finite.coq_type -> Finite.sort list
     end

    module Coq_enum =
     struct
      (** val body : Finite.coq_type -> Finite.sort list **)

      let body t =
        t.Finite.fintype_isFinite_mixin

      (** val unlock : __ **)

      let unlock =
        __
     end
   end
 end

(** val enum_mem :
    Finite.coq_type -> Finite.sort mem_pred -> Finite.sort list **)

let enum_mem t mA =
  filter (PredOfSimpl.coerce (simpl_of_mem mA))
    (FiniteNES.Finite.Coq_enum.body t)

type ordinal = nat
  (* singleton inductive, whose constructor was Ordinal *)

(** val nat_of_ord : nat -> ordinal -> nat **)

let nat_of_ord _ i =
  i

(** val coq_HB_unnamed_factory_57 :
    nat -> (nat, ordinal) Coq_isSub.axioms_ **)

let coq_HB_unnamed_factory_57 n =
  { Coq_isSub.val_subdef = (nat_of_ord n); Coq_isSub.coq_Sub = (fun x _ ->
    x); Coq_isSub.coq_Sub_rect = (fun _ k_S u -> k_S u __) }

(** val fintype_ordinal__canonical__eqtype_SubType :
    nat -> nat SubType.coq_type **)

let fintype_ordinal__canonical__eqtype_SubType n =
  Obj.magic coq_HB_unnamed_factory_57 n

(** val coq_HB_unnamed_factory_59 :
    nat -> ordinal Choice.Countable.axioms_ **)

let coq_HB_unnamed_factory_59 n =
  Obj.magic Choice.eqtype_sub_type__canonical__choice_Countable
    Choice.coq_Datatypes_nat__canonical__choice_Countable (fun x ->
    leq (S (Obj.magic x)) n)
    (reverse_coercion
      (Obj.magic fintype_ordinal__canonical__eqtype_SubType n) __)

(** val coq_HB_unnamed_mixin_63 : nat -> ordinal Coq_hasDecEq.axioms_ **)

let coq_HB_unnamed_mixin_63 n =
  (coq_HB_unnamed_factory_59 n).Choice.Countable.eqtype_hasDecEq_mixin

(** val coq_HB_unnamed_mixin_64 :
    nat -> ordinal Choice.Coq_hasChoice.axioms_ **)

let coq_HB_unnamed_mixin_64 n =
  (coq_HB_unnamed_factory_59 n).Choice.Countable.choice_hasChoice_mixin

(** val coq_HB_unnamed_mixin_65 :
    nat -> ordinal Choice.Choice_isCountable.axioms_ **)

let coq_HB_unnamed_mixin_65 n =
  (coq_HB_unnamed_factory_59 n).Choice.Countable.choice_Choice_isCountable_mixin

(** val ord_enum : nat -> ordinal list **)

let ord_enum n =
  pmap
    (Obj.magic insub (fun x -> leq (S x) n)
      (fintype_ordinal__canonical__eqtype_SubType n)) (iota O n)

(** val coq_HB_unnamed_factory_66 : nat -> ordinal Coq_isFinite.axioms_ **)

let coq_HB_unnamed_factory_66 =
  ord_enum

(** val fintype_ordinal__canonical__fintype_Finite :
    nat -> Finite.coq_type **)

let fintype_ordinal__canonical__fintype_Finite n =
  { Finite.choice_hasChoice_mixin = (Obj.magic coq_HB_unnamed_mixin_64 n);
    Finite.choice_Choice_isCountable_mixin =
    (Obj.magic coq_HB_unnamed_mixin_65 n); Finite.eqtype_hasDecEq_mixin =
    (Obj.magic coq_HB_unnamed_mixin_63 n); Finite.fintype_isFinite_mixin =
    (Obj.magic coq_HB_unnamed_factory_66 n) }

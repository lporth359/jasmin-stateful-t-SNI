open Datatypes
open Prelude
open Eqtype
open Seq
open Ssrbool
open Ssrnat

type __ = Obj.t

module Coq_isFinite :
 sig
  type 't axioms_ =
    't list
    (* singleton inductive, whose constructor was Axioms_ *)

  val enum_subdef : 'a1 Coq_hasDecEq.axioms_ -> 'a1 axioms_ -> 'a1 list
 end

module Finite :
 sig
  type 't axioms_ = { choice_hasChoice_mixin : 't Choice.Coq_hasChoice.axioms_;
                      choice_Choice_isCountable_mixin : 't
                                                        Choice.Choice_isCountable.axioms_;
                      eqtype_hasDecEq_mixin : 't Coq_hasDecEq.axioms_;
                      fintype_isFinite_mixin : 't Coq_isFinite.axioms_ }

  type coq_type =
    __ axioms_
    (* singleton inductive, whose constructor was Pack *)

  type sort = __

  val coq_class : coq_type -> sort axioms_

  module Exports :
   sig
    val fintype_Finite_class__to__eqtype_Equality_class :
      'a1 axioms_ -> 'a1 Equality.axioms_

    val fintype_Finite__to__eqtype_Equality : coq_type -> Equality.coq_type
   end
 end

module FiniteNES :
 sig
  module Finite :
   sig
    module type Coq_enum_Locked =
     sig
      val body : Finite.coq_type -> Finite.sort list
     end

    module Coq_enum :
     Coq_enum_Locked
   end
 end

val enum_mem : Finite.coq_type -> Finite.sort mem_pred -> Finite.sort list

type ordinal = nat
  (* singleton inductive, whose constructor was Ordinal *)

val nat_of_ord : nat -> ordinal -> nat

val coq_HB_unnamed_factory_57 : nat -> (nat, ordinal) Coq_isSub.axioms_

val fintype_ordinal__canonical__eqtype_SubType : nat -> nat SubType.coq_type

val coq_HB_unnamed_factory_59 : nat -> ordinal Choice.Countable.axioms_

val coq_HB_unnamed_mixin_63 : nat -> ordinal Coq_hasDecEq.axioms_

val coq_HB_unnamed_mixin_64 : nat -> ordinal Choice.Coq_hasChoice.axioms_

val coq_HB_unnamed_mixin_65 : nat -> ordinal Choice.Choice_isCountable.axioms_

val ord_enum : nat -> ordinal list

val coq_HB_unnamed_factory_66 : nat -> ordinal Coq_isFinite.axioms_

val fintype_ordinal__canonical__fintype_Finite : nat -> Finite.coq_type

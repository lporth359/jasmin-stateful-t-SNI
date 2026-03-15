open Bool
open Datatypes
open Specif
open Ssrbool
open Ssrfun

type __ = Obj.t

type 't eq_axiom = 't -> 't -> reflect

module Coq_hasDecEq :
 sig
  type 't axioms_ = { eq_op : 't rel; eqP : 't eq_axiom }

  val eq_op : 'a1 axioms_ -> 'a1 rel

  val eqP : 'a1 axioms_ -> 'a1 eq_axiom
 end

module Equality :
 sig
  type 't axioms_ =
    't Coq_hasDecEq.axioms_
    (* singleton inductive, whose constructor was Class *)

  val eqtype_hasDecEq_mixin : 'a1 axioms_ -> 'a1 Coq_hasDecEq.axioms_

  type coq_type =
    __ axioms_
    (* singleton inductive, whose constructor was Pack *)

  type sort = __

  val coq_class : coq_type -> sort axioms_
 end

val eq_op : Equality.coq_type -> Equality.sort rel

val eqP : Equality.coq_type -> Equality.sort eq_axiom

val unit_eqP : unit eq_axiom

val coq_HB_unnamed_factory_1 : unit Coq_hasDecEq.axioms_

val coq_Datatypes_unit__canonical__eqtype_Equality : Equality.coq_type

val eqb : bool -> bool -> bool

val eqbP : bool eq_axiom

val coq_HB_unnamed_factory_3 : bool Coq_hasDecEq.axioms_

val coq_Datatypes_bool__canonical__eqtype_Equality : Equality.coq_type

val pred1 : Equality.coq_type -> Equality.sort -> Equality.sort simpl_pred

module Coq_isSub :
 sig
  type ('t, 'sub_sort) axioms_ = { val_subdef : ('sub_sort -> 't);
                                   coq_Sub : ('t -> __ -> 'sub_sort);
                                   coq_Sub_rect : (__ -> ('t -> __ -> __) ->
                                                  'sub_sort -> __) }

  val val_subdef : 'a1 pred -> ('a1, 'a2) axioms_ -> 'a2 -> 'a1

  val coq_Sub : 'a1 pred -> ('a1, 'a2) axioms_ -> 'a1 -> 'a2
 end

module SubType :
 sig
  type ('t, 's) axioms_ =
    ('t, 's) Coq_isSub.axioms_
    (* singleton inductive, whose constructor was Class *)

  val eqtype_isSub_mixin :
    'a1 pred -> ('a1, 'a2) axioms_ -> ('a1, 'a2) Coq_isSub.axioms_

  type 't coq_type =
    ('t, __) axioms_
    (* singleton inductive, whose constructor was Pack *)

  type 't sort = __

  val coq_class : 'a1 pred -> 'a1 coq_type -> ('a1, 'a1 sort) axioms_

  val phant_on_ : 'a1 pred -> 'a1 coq_type -> ('a1, 'a1 sort) axioms_
 end

val coq_Sub : 'a1 pred -> 'a1 SubType.coq_type -> 'a1 -> 'a1 SubType.sort

val insub : 'a1 pred -> 'a1 SubType.coq_type -> 'a1 -> 'a1 SubType.sort option

type ('t, 'x) inj_type = 't

type ('t, 'x) pcan_type = 't

type ('t, 'x) can_type = 't

val inj_eqAxiom : Equality.coq_type -> ('a1 -> Equality.sort) -> 'a1 eq_axiom

val coq_HB_unnamed_factory_9 :
  Equality.coq_type -> ('a1 -> Equality.sort) -> ('a1, Equality.sort)
  inj_type Coq_hasDecEq.axioms_

val eqtype_inj_type__canonical__eqtype_Equality :
  Equality.coq_type -> ('a1 -> Equality.sort) -> Equality.coq_type

val coq_HB_unnamed_factory_12 :
  Equality.coq_type -> ('a1 -> Equality.sort) -> (Equality.sort -> 'a1
  option) -> ('a1, Equality.sort) pcan_type Equality.axioms_

val coq_HB_unnamed_mixin_14 :
  Equality.coq_type -> ('a1 -> Equality.sort) -> (Equality.sort -> 'a1
  option) -> ('a1, Equality.sort) pcan_type Coq_hasDecEq.axioms_

val coq_HB_unnamed_factory_16 :
  Equality.coq_type -> ('a1 -> Equality.sort) -> (Equality.sort -> 'a1) ->
  ('a1, Equality.sort) can_type Equality.axioms_

val coq_HB_unnamed_mixin_19 :
  Equality.coq_type -> ('a1 -> Equality.sort) -> (Equality.sort -> 'a1) ->
  ('a1, Equality.sort) can_type Coq_hasDecEq.axioms_

val pair_eq :
  Equality.coq_type -> Equality.coq_type -> (Equality.sort * Equality.sort)
  rel

val pair_eqP :
  Equality.coq_type -> Equality.coq_type -> (Equality.sort * Equality.sort)
  eq_axiom

val coq_HB_unnamed_factory_38 :
  Equality.coq_type -> Equality.coq_type -> (Equality.sort * Equality.sort)
  Coq_hasDecEq.axioms_

val coq_Datatypes_prod__canonical__eqtype_Equality :
  Equality.coq_type -> Equality.coq_type -> Equality.coq_type

val opt_eq :
  Equality.coq_type -> Equality.sort option -> Equality.sort option -> bool

val opt_eqP : Equality.coq_type -> Equality.sort option eq_axiom

val coq_HB_unnamed_factory_40 :
  Equality.coq_type -> Equality.sort option Coq_hasDecEq.axioms_

val coq_Datatypes_option__canonical__eqtype_Equality :
  Equality.coq_type -> Equality.coq_type

val tagged_as :
  Equality.coq_type -> (Equality.sort, 'a1) sigT -> (Equality.sort, 'a1) sigT
  -> 'a1

val tag_eq :
  Equality.coq_type -> (Equality.sort -> Equality.coq_type) ->
  (Equality.sort, Equality.sort) sigT -> (Equality.sort, Equality.sort) sigT
  -> bool

val tag_eqP :
  Equality.coq_type -> (Equality.sort -> Equality.coq_type) ->
  (Equality.sort, Equality.sort) sigT eq_axiom

val coq_HB_unnamed_factory_42 :
  Equality.coq_type -> (Equality.sort -> Equality.coq_type) ->
  (Equality.sort, Equality.sort) sigT Coq_hasDecEq.axioms_

val sum_eq :
  Equality.coq_type -> Equality.coq_type -> (Equality.sort, Equality.sort)
  sum -> (Equality.sort, Equality.sort) sum -> bool

val sum_eqP :
  Equality.coq_type -> Equality.coq_type -> (Equality.sort, Equality.sort)
  sum eq_axiom

val coq_HB_unnamed_factory_44 :
  Equality.coq_type -> Equality.coq_type -> (Equality.sort, Equality.sort)
  sum Coq_hasDecEq.axioms_

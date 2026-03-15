open Datatypes
open Nat0
open Specif
open Eqtype
open Seq
open Ssrbool
open Ssrfun
open Ssrnat

type __ = Obj.t

module CodeSeq :
 sig
  val code : nat list -> nat

  val decode_rec : nat -> nat -> nat -> nat list

  val decode : nat -> nat list
 end

val seq_of_opt : 'a1 option -> 'a1 list

val tag_of_pair : ('a1 * 'a2) -> ('a1, 'a2) sigT

val pair_of_tag : ('a1, 'a2) sigT -> 'a1 * 'a2

val opair_of_sum : ('a1, 'a2) sum -> 'a1 option * 'a2 option

val sum_of_opair : ('a1 option * 'a2 option) -> ('a1, 'a2) sum option

module Coq_hasChoice :
 sig
  type 't axioms_ =
    't pred -> nat -> 't option
    (* singleton inductive, whose constructor was Axioms_ *)

  val find_subdef : 'a1 axioms_ -> 'a1 pred -> nat -> 'a1 option

  type 't phant_axioms = 't axioms_
 end

module Choice :
 sig
  type 't axioms_ = { choice_hasChoice_mixin : 't Coq_hasChoice.axioms_;
                      eqtype_hasDecEq_mixin : 't Coq_hasDecEq.axioms_ }

  val choice_hasChoice_mixin : 'a1 axioms_ -> 'a1 Coq_hasChoice.axioms_

  type coq_type =
    __ axioms_
    (* singleton inductive, whose constructor was Pack *)

  type sort = __

  val coq_class : coq_type -> sort axioms_

  module Exports :
   sig
    val choice_Choice_class__to__eqtype_Equality_class :
      'a1 axioms_ -> 'a1 Equality.axioms_

    val choice_Choice__to__eqtype_Equality : coq_type -> Equality.coq_type
   end
 end

val find_subdef :
  Choice.coq_type -> Choice.sort pred -> nat -> Choice.sort option

val coq_PCanHasChoice :
  Choice.coq_type -> ('a1 -> Choice.sort) -> (Choice.sort -> 'a1 option) ->
  'a1 Coq_hasChoice.phant_axioms

val coq_HB_unnamed_factory_6 :
  Choice.coq_type -> ('a1 -> Choice.sort) -> (Choice.sort -> 'a1 option) ->
  ('a1, Choice.sort) pcan_type Coq_hasChoice.phant_axioms

val eqtype_pcan_type__canonical__choice_Choice :
  Choice.coq_type -> ('a1 -> Choice.sort) -> (Choice.sort -> 'a1 option) ->
  Choice.coq_type

val coq_HB_unnamed_factory_9 :
  Choice.coq_type -> ('a1 -> Choice.sort) -> (Choice.sort -> 'a1) -> ('a1,
  Choice.sort) can_type Coq_hasChoice.phant_axioms

val eqtype_can_type__canonical__choice_Choice :
  Choice.coq_type -> ('a1 -> Choice.sort) -> (Choice.sort -> 'a1) ->
  Choice.coq_type

val seq_hasChoice :
  Choice.coq_type -> Choice.sort list Coq_hasChoice.phant_axioms

val coq_HB_unnamed_factory_16 :
  Choice.coq_type -> Choice.sort list Coq_hasChoice.phant_axioms

val coq_Datatypes_list__canonical__choice_Choice :
  Choice.coq_type -> Choice.coq_type

val tagged_hasChoice :
  Choice.coq_type -> (Choice.sort -> Choice.coq_type) -> (Choice.sort,
  Choice.sort) sigT Coq_hasChoice.phant_axioms

val coq_HB_unnamed_factory_18 :
  Choice.coq_type -> (Choice.sort -> Choice.coq_type) -> (Choice.sort,
  Choice.sort) sigT Coq_hasChoice.phant_axioms

val coq_Specif_sigT__canonical__choice_Choice :
  Choice.coq_type -> (Choice.sort -> Choice.coq_type) -> Choice.coq_type

val nat_hasChoice : nat Coq_hasChoice.phant_axioms

val coq_HB_unnamed_factory_20 : nat Coq_hasChoice.phant_axioms

val coq_Datatypes_nat__canonical__choice_Choice : Choice.coq_type

val coq_HB_unnamed_factory_43 :
  Choice.coq_type -> Choice.sort option Choice.axioms_

val coq_HB_unnamed_mixin_48 :
  Choice.coq_type -> Choice.sort option Coq_hasChoice.axioms_

val coq_Datatypes_option__canonical__choice_Choice :
  Choice.coq_type -> Choice.coq_type

val coq_HB_unnamed_factory_50 :
  Choice.coq_type -> Choice.coq_type -> (Choice.sort * Choice.sort)
  Choice.axioms_

val coq_HB_unnamed_mixin_55 :
  Choice.coq_type -> Choice.coq_type -> (Choice.sort * Choice.sort)
  Coq_hasChoice.axioms_

val coq_Datatypes_prod__canonical__choice_Choice :
  Choice.coq_type -> Choice.coq_type -> Choice.coq_type

val coq_HB_unnamed_factory_57 :
  Choice.coq_type -> Choice.coq_type -> (Choice.sort, Choice.sort) sum
  Choice.axioms_

val coq_HB_unnamed_mixin_62 :
  Choice.coq_type -> Choice.coq_type -> (Choice.sort, Choice.sort) sum
  Coq_hasChoice.axioms_

module Choice_isCountable :
 sig
  type 't axioms_ = { pickle : ('t -> nat); unpickle : (nat -> 't option) }

  val pickle : 'a1 axioms_ -> 'a1 -> nat

  val unpickle : 'a1 axioms_ -> nat -> 'a1 option

  val phant_Build : ('a1 -> nat) -> (nat -> 'a1 option) -> 'a1 axioms_
 end

module Countable :
 sig
  type 't axioms_ = { choice_hasChoice_mixin : 't Coq_hasChoice.axioms_;
                      eqtype_hasDecEq_mixin : 't Coq_hasDecEq.axioms_;
                      choice_Choice_isCountable_mixin : 't
                                                        Choice_isCountable.axioms_ }

  val choice_hasChoice_mixin : 'a1 axioms_ -> 'a1 Coq_hasChoice.axioms_

  val eqtype_hasDecEq_mixin : 'a1 axioms_ -> 'a1 Coq_hasDecEq.axioms_

  val choice_Choice_isCountable_mixin :
    'a1 axioms_ -> 'a1 Choice_isCountable.axioms_

  type coq_type =
    __ axioms_
    (* singleton inductive, whose constructor was Pack *)

  type sort = __

  val coq_class : coq_type -> sort axioms_

  module Exports :
   sig
    val choice_Countable_class__to__eqtype_Equality_class :
      'a1 axioms_ -> 'a1 Equality.axioms_

    val choice_Countable__to__eqtype_Equality : coq_type -> Equality.coq_type

    val choice_Countable_class__to__choice_Choice_class :
      'a1 axioms_ -> 'a1 Choice.axioms_

    val choice_Countable__to__choice_Choice : coq_type -> Choice.coq_type
   end
 end

val pickle : Countable.coq_type -> Countable.sort -> nat

val unpickle : Countable.coq_type -> nat -> Countable.sort option

module Coq_isCountable :
 sig
  type 't axioms_ = { pickle : ('t -> nat); unpickle : (nat -> 't option) }

  val pickle : 'a1 axioms_ -> 'a1 -> nat

  val unpickle : 'a1 axioms_ -> nat -> 'a1 option

  val phant_Build : ('a1 -> nat) -> (nat -> 'a1 option) -> 'a1 axioms_

  type 't phant_axioms = 't axioms_
 end

module Builders_77 :
 sig
  val coq_HB_unnamed_factory_81 :
    'a1 Coq_isCountable.phant_axioms -> 'a1 Coq_hasChoice.phant_axioms

  val coq_HB_unnamed_factory_83 :
    'a1 Coq_isCountable.phant_axioms -> 'a1 Choice_isCountable.axioms_
 end

val coq_PCanIsCountable :
  Countable.coq_type -> ('a1 -> Countable.sort) -> (Countable.sort -> 'a1
  option) -> 'a1 Coq_isCountable.axioms_

val coq_CanIsCountable :
  Countable.coq_type -> ('a1 -> Countable.sort) -> (Countable.sort -> 'a1) ->
  'a1 Coq_isCountable.axioms_

val coq_HB_unnamed_factory_87 :
  Countable.coq_type -> ('a1 -> Countable.sort) -> (Countable.sort -> 'a1
  option) -> ('a1, Countable.sort) pcan_type Coq_isCountable.phant_axioms

val coq_HB_unnamed_mixin_91 :
  Countable.coq_type -> ('a1 -> Countable.sort) -> (Countable.sort -> 'a1
  option) -> ('a1, Countable.sort) pcan_type Choice_isCountable.axioms_

val eqtype_pcan_type__canonical__choice_Countable :
  Countable.coq_type -> ('a1 -> Countable.sort) -> (Countable.sort -> 'a1
  option) -> Countable.coq_type

val coq_HB_unnamed_factory_93 :
  Countable.coq_type -> ('a1 -> Countable.sort) -> (Countable.sort -> 'a1) ->
  ('a1, Countable.sort) can_type Coq_isCountable.phant_axioms

val coq_HB_unnamed_mixin_100 :
  Countable.coq_type -> ('a1 -> Countable.sort) -> (Countable.sort -> 'a1) ->
  ('a1, Countable.sort) can_type Choice_isCountable.axioms_

val eqtype_can_type__canonical__choice_Countable :
  Countable.coq_type -> ('a1 -> Countable.sort) -> (Countable.sort -> 'a1) ->
  Countable.coq_type

val coq_HB_unnamed_mixin_106 :
  Countable.coq_type -> Countable.sort pred -> Countable.sort
  SubType.coq_type -> Choice.sort SubType.sort Coq_hasChoice.phant_axioms

val coq_HB_unnamed_mixin_107 :
  Countable.coq_type -> Countable.sort pred -> Countable.sort
  SubType.coq_type -> (Equality.sort SubType.sort, Equality.sort) inj_type
  Coq_hasDecEq.axioms_

val coq_HB_unnamed_mixin_109 :
  Countable.coq_type -> Countable.sort pred -> Countable.sort
  SubType.coq_type -> (Countable.sort SubType.sort, Countable.sort) pcan_type
  Choice_isCountable.axioms_

val eqtype_sub_type__canonical__choice_Countable :
  Countable.coq_type -> Countable.sort pred -> Countable.sort
  SubType.coq_type -> Countable.coq_type

val pickle_seq : Countable.coq_type -> Countable.sort list -> nat

val unpickle_seq : Countable.coq_type -> nat -> Countable.sort list option

val coq_HB_unnamed_factory_110 :
  Countable.coq_type -> Countable.sort list Coq_isCountable.axioms_

val coq_HB_unnamed_mixin_117 :
  Countable.coq_type -> Countable.sort list Choice_isCountable.axioms_

val coq_Datatypes_list__canonical__choice_Countable :
  Countable.coq_type -> Countable.coq_type

val pickle_tagged :
  Countable.coq_type -> (Countable.sort -> Countable.coq_type) ->
  (Countable.sort, Countable.sort) sigT -> nat

val unpickle_tagged :
  Countable.coq_type -> (Countable.sort -> Countable.coq_type) -> nat ->
  (Countable.sort, Countable.sort) sigT option

val coq_HB_unnamed_factory_118 :
  Countable.coq_type -> (Countable.sort -> Countable.coq_type) ->
  (Countable.sort, Countable.sort) sigT Choice_isCountable.axioms_

val coq_Specif_sigT__canonical__choice_Countable :
  Countable.coq_type -> (Countable.sort -> Countable.coq_type) ->
  Countable.coq_type

val coq_HB_unnamed_factory_120 : nat Choice_isCountable.axioms_

val coq_Datatypes_nat__canonical__choice_Countable : Countable.coq_type

val coq_HB_unnamed_factory_147 :
  Countable.coq_type -> Countable.sort option Countable.axioms_

val coq_HB_unnamed_mixin_154 :
  Countable.coq_type -> Countable.sort option Choice_isCountable.axioms_

val coq_Datatypes_option__canonical__choice_Countable :
  Countable.coq_type -> Countable.coq_type

val coq_HB_unnamed_factory_165 :
  Countable.coq_type -> Countable.coq_type ->
  (Countable.sort * Countable.sort) Countable.axioms_

val coq_HB_unnamed_mixin_172 :
  Countable.coq_type -> Countable.coq_type ->
  (Countable.sort * Countable.sort) Choice_isCountable.axioms_

val coq_Datatypes_prod__canonical__choice_Countable :
  Countable.coq_type -> Countable.coq_type -> Countable.coq_type

val coq_HB_unnamed_factory_174 :
  Countable.coq_type -> Countable.coq_type -> (Countable.sort,
  Countable.sort) sum Countable.axioms_

val coq_HB_unnamed_mixin_181 :
  Countable.coq_type -> Countable.coq_type -> (Countable.sort,
  Countable.sort) sum Choice_isCountable.axioms_

val coq_Datatypes_sum__canonical__choice_Countable :
  Countable.coq_type -> Countable.coq_type -> Countable.coq_type

open Datatypes
open Eqtype
open Ssralg
open Ssrnat

type int =
| Posz of nat
| Negz of nat

(** val natsum_of_int : int -> (nat, nat) sum **)

let natsum_of_int = function
| Posz p -> Coq_inl p
| Negz n -> Coq_inr n

(** val int_of_natsum : (nat, nat) sum -> int **)

let int_of_natsum = function
| Coq_inl p -> Posz p
| Coq_inr n -> Negz n

(** val coq_HB_unnamed_factory_1 : int Choice.Countable.axioms_ **)

let coq_HB_unnamed_factory_1 =
  Obj.magic Choice.eqtype_can_type__canonical__choice_Countable
    (Choice.coq_Datatypes_sum__canonical__choice_Countable
      Choice.coq_Datatypes_nat__canonical__choice_Countable
      Choice.coq_Datatypes_nat__canonical__choice_Countable) natsum_of_int
    int_of_natsum

(** val coq_HB_unnamed_mixin_5 : int Coq_hasDecEq.axioms_ **)

let coq_HB_unnamed_mixin_5 =
  coq_HB_unnamed_factory_1.Choice.Countable.eqtype_hasDecEq_mixin

(** val ssrint_int__canonical__eqtype_Equality : Equality.coq_type **)

let ssrint_int__canonical__eqtype_Equality =
  Obj.magic coq_HB_unnamed_mixin_5

(** val coq_HB_unnamed_mixin_6 : int Choice.Coq_hasChoice.axioms_ **)

let coq_HB_unnamed_mixin_6 =
  coq_HB_unnamed_factory_1.Choice.Countable.choice_hasChoice_mixin

(** val ssrint_int__canonical__choice_Choice : Choice.Choice.coq_type **)

let ssrint_int__canonical__choice_Choice =
  { Choice.Choice.choice_hasChoice_mixin =
    (Obj.magic coq_HB_unnamed_mixin_6); Choice.Choice.eqtype_hasDecEq_mixin =
    (Obj.magic coq_HB_unnamed_mixin_5) }

(** val coq_HB_unnamed_mixin_7 : int Choice.Choice_isCountable.axioms_ **)

let coq_HB_unnamed_mixin_7 =
  coq_HB_unnamed_factory_1.Choice.Countable.choice_Choice_isCountable_mixin

(** val ssrint_int__canonical__choice_Countable :
    Choice.Countable.coq_type **)

let ssrint_int__canonical__choice_Countable =
  { Choice.Countable.choice_hasChoice_mixin =
    (Obj.magic coq_HB_unnamed_mixin_6);
    Choice.Countable.eqtype_hasDecEq_mixin =
    (Obj.magic coq_HB_unnamed_mixin_5);
    Choice.Countable.choice_Choice_isCountable_mixin =
    (Obj.magic coq_HB_unnamed_mixin_7) }

module Coq_intZmod =
 struct
  (** val addz : int -> int -> int **)

  let addz m n =
    match m with
    | Posz m' ->
      (match n with
       | Posz n' -> Posz (addn m' n')
       | Negz n' ->
         if leq (S n') m' then Posz (subn m' (S n')) else Negz (subn n' m'))
    | Negz n' ->
      (match n with
       | Posz m' ->
         if leq (S n') m' then Posz (subn m' (S n')) else Negz (subn n' m')
       | Negz n'0 -> Negz (S (addn n' n'0)))

  (** val oppz : int -> int **)

  let oppz = function
  | Posz n -> (match n with
               | O -> Posz O
               | S n' -> Negz n')
  | Negz n -> Posz (S n)

  (** val coq_Mixin : int GRing.Coq_isZmodule.axioms_ **)

  let coq_Mixin =
    GRing.Coq_isZmodule.phant_Build coq_HB_unnamed_mixin_6
      coq_HB_unnamed_mixin_5 ssrint_int__canonical__choice_Choice
      { Choice.Choice.choice_hasChoice_mixin = coq_HB_unnamed_mixin_6;
      Choice.Choice.eqtype_hasDecEq_mixin = coq_HB_unnamed_mixin_5 }
      coq_HB_unnamed_mixin_6 coq_HB_unnamed_mixin_5
      ssrint_int__canonical__eqtype_Equality coq_HB_unnamed_mixin_5
      coq_HB_unnamed_mixin_5 (Posz O) oppz addz
 end

(** val coq_HB_unnamed_factory_8 : int GRing.Coq_isZmodule.axioms_ **)

let coq_HB_unnamed_factory_8 =
  Coq_intZmod.coq_Mixin

(** val coq_HB_unnamed_mixin_11 : int GRing.Coq_isNmodule.axioms_ **)

let coq_HB_unnamed_mixin_11 =
  GRing.Builders_8.coq_HB_unnamed_factory_10 coq_HB_unnamed_mixin_6
    coq_HB_unnamed_mixin_5 coq_HB_unnamed_factory_8

(** val coq_HB_unnamed_mixin_12 : int GRing.Nmodule_isZmodule.axioms_ **)

let coq_HB_unnamed_mixin_12 =
  GRing.Builders_8.coq_HB_unnamed_factory_12 coq_HB_unnamed_mixin_6
    coq_HB_unnamed_mixin_5 coq_HB_unnamed_factory_8

(** val ssrint_int__canonical__GRing_Zmodule : GRing.Zmodule.coq_type **)

let ssrint_int__canonical__GRing_Zmodule =
  { GRing.Zmodule.coq_GRing_isNmodule_mixin =
    (Obj.magic coq_HB_unnamed_mixin_11);
    GRing.Zmodule.choice_hasChoice_mixin =
    (Obj.magic coq_HB_unnamed_mixin_6); GRing.Zmodule.eqtype_hasDecEq_mixin =
    (Obj.magic coq_HB_unnamed_mixin_5);
    GRing.Zmodule.coq_GRing_Nmodule_isZmodule_mixin =
    (Obj.magic coq_HB_unnamed_mixin_12) }

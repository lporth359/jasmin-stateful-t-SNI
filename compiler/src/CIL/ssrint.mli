open Datatypes
open Eqtype
open Ssralg
open Ssrnat

type int =
| Posz of nat
| Negz of nat

val natsum_of_int : int -> (nat, nat) sum

val int_of_natsum : (nat, nat) sum -> int

val coq_HB_unnamed_factory_1 : int Choice.Countable.axioms_

val coq_HB_unnamed_mixin_5 : int Coq_hasDecEq.axioms_

val ssrint_int__canonical__eqtype_Equality : Equality.coq_type

val coq_HB_unnamed_mixin_6 : int Choice.Coq_hasChoice.axioms_

val ssrint_int__canonical__choice_Choice : Choice.Choice.coq_type

val coq_HB_unnamed_mixin_7 : int Choice.Choice_isCountable.axioms_

val ssrint_int__canonical__choice_Countable : Choice.Countable.coq_type

module Coq_intZmod :
 sig
  val addz : int -> int -> int

  val oppz : int -> int

  val coq_Mixin : int GRing.Coq_isZmodule.axioms_
 end

val coq_HB_unnamed_factory_8 : int GRing.Coq_isZmodule.axioms_

val coq_HB_unnamed_mixin_11 : int GRing.Coq_isNmodule.axioms_

val coq_HB_unnamed_mixin_12 : int GRing.Nmodule_isZmodule.axioms_

val ssrint_int__canonical__GRing_Zmodule : GRing.Zmodule.coq_type

open BinInt
open BinNums
open BinPos
open Bool
open Datatypes
open Eqtype
open Ssralg
open Ssrbool
open Ssrint
open Ssrnum

val int_to_Z : int -> coq_Z

val coq_Z_to_int : coq_Z -> int

val coq_ZeqbP : coq_Z -> coq_Z -> reflect

val coq_HB_unnamed_factory_1 : coq_Z Coq_hasDecEq.axioms_

val coq_BinNums_Z__canonical__eqtype_Equality : Equality.coq_type

val coq_HB_unnamed_factory_3 : coq_Z Choice.Countable.axioms_

val coq_HB_unnamed_mixin_7 : coq_Z Choice.Coq_hasChoice.axioms_

val coq_HB_unnamed_mixin_8 : coq_Z Choice.Choice_isCountable.axioms_

val coq_BinNums_Z__canonical__choice_Countable : Choice.Countable.coq_type

val coq_HB_unnamed_factory_9 : coq_Z GRing.Coq_isZmodule.axioms_

val coq_HB_unnamed_mixin_12 : coq_Z GRing.Coq_isNmodule.axioms_

val coq_BinNums_Z__canonical__GRing_Nmodule : GRing.Nmodule.coq_type

val coq_HB_unnamed_mixin_13 : coq_Z GRing.Nmodule_isZmodule.axioms_

val coq_BinNums_Z__canonical__GRing_Zmodule : GRing.Zmodule.coq_type

val coq_HB_unnamed_factory_14 : coq_Z GRing.Zmodule_isComRing.axioms_

val coq_HB_unnamed_mixin_17 : coq_Z GRing.Nmodule_isSemiRing.axioms_

val coq_BinNums_Z__canonical__GRing_SemiRing : GRing.SemiRing.coq_type

val coq_HB_unnamed_mixin_18 : coq_Z GRing.SemiRing_hasCommutativeMul.axioms_

module ZUnitRing :
 sig
  val unitZ : coq_Z qualifier

  val invZ : coq_Z -> coq_Z
 end

val coq_HB_unnamed_factory_19 : coq_Z GRing.ComRing_hasMulInverse.axioms_

val coq_HB_unnamed_mixin_21 : coq_Z GRing.Ring_hasMulInverse.axioms_

val coq_HB_unnamed_factory_22 : coq_Z GRing.ComUnitRing_isIntegral.axioms_

val coq_BinNums_Z__canonical__GRing_IntegralDomain :
  GRing.IntegralDomain.coq_type

module ZNumDomain :
 sig
  val coq_Z_realLeMixin : coq_Z Num.IntegralDomain_isLeReal.axioms_
 end

val coq_HB_unnamed_factory_24 : coq_Z Num.IntegralDomain_isLeReal.axioms_

val coq_HB_unnamed_mixin_31 : coq_Z Order.Order.Coq_isPOrder.axioms_

val coq_BinNums_Z__canonical__Order_POrder : Order.Order.POrder.coq_type

val coq_BinNums_Z__canonical__Num_POrderedZmodule :
  Num.POrderedZmodule.coq_type

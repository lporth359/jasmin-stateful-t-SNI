open Eqtype
open Ssralg
open Ssrbool

type __ = Obj.t

val ring_display : unit

module Num :
 sig
  module POrderedZmodule :
   sig
    type 'r axioms_ = { coq_GRing_isNmodule_mixin : 'r
                                                    GRing.Coq_isNmodule.axioms_;
                        choice_hasChoice_mixin : 'r
                                                 Choice.Coq_hasChoice.axioms_;
                        eqtype_hasDecEq_mixin : 'r Coq_hasDecEq.axioms_;
                        coq_Order_isPOrder_mixin : 'r
                                                   Order.Order.Coq_isPOrder.axioms_;
                        coq_GRing_Nmodule_isZmodule_mixin : 'r
                                                            GRing.Nmodule_isZmodule.axioms_ }

    type coq_type =
      __ axioms_
      (* singleton inductive, whose constructor was Pack *)

    type sort = __

    val coq_class : coq_type -> sort axioms_

    module Exports :
     sig
      val coq_Num_POrderedZmodule_class__to__Order_POrder_class :
        'a1 axioms_ -> 'a1 Order.Order.POrder.axioms_

      val coq_Num_POrderedZmodule__to__Order_POrder :
        coq_type -> Order.Order.POrder.coq_type

      val coq_Num_POrderedZmodule_class__to__GRing_Nmodule_class :
        'a1 axioms_ -> 'a1 GRing.Nmodule.axioms_

      val coq_Num_POrderedZmodule__to__GRing_Nmodule :
        coq_type -> GRing.Nmodule.coq_type

      val join_Num_POrderedZmodule_between_GRing_Nmodule_and_Order_POrder :
        coq_type -> Order.Order.POrder.coq_type
     end
   end

  module IntegralDomain_isNumRing :
   sig
    type 'r axioms_ = { coq_Rle : 'r rel; coq_Rlt : 'r rel; norm : ('r -> 'r) }

    val coq_Rle :
      'a1 GRing.Coq_isNmodule.axioms_ -> 'a1 Choice.Coq_hasChoice.axioms_ ->
      'a1 Coq_hasDecEq.axioms_ -> 'a1 GRing.Nmodule_isSemiRing.axioms_ -> 'a1
      GRing.SemiRing_hasCommutativeMul.axioms_ -> 'a1
      GRing.Nmodule_isZmodule.axioms_ -> 'a1 GRing.Ring_hasMulInverse.axioms_
      -> 'a1 GRing.ComUnitRing_isIntegral.axioms_ -> 'a1 axioms_ -> 'a1 rel

    val coq_Rlt :
      'a1 GRing.Coq_isNmodule.axioms_ -> 'a1 Choice.Coq_hasChoice.axioms_ ->
      'a1 Coq_hasDecEq.axioms_ -> 'a1 GRing.Nmodule_isSemiRing.axioms_ -> 'a1
      GRing.SemiRing_hasCommutativeMul.axioms_ -> 'a1
      GRing.Nmodule_isZmodule.axioms_ -> 'a1 GRing.Ring_hasMulInverse.axioms_
      -> 'a1 GRing.ComUnitRing_isIntegral.axioms_ -> 'a1 axioms_ -> 'a1 rel

    val phant_Build :
      'a1 GRing.Coq_isNmodule.axioms_ -> 'a1 Choice.Coq_hasChoice.axioms_ ->
      'a1 Coq_hasDecEq.axioms_ -> 'a1 GRing.Nmodule_isSemiRing.axioms_ -> 'a1
      GRing.SemiRing_hasCommutativeMul.axioms_ -> 'a1
      GRing.Nmodule_isZmodule.axioms_ -> 'a1 GRing.Ring_hasMulInverse.axioms_
      -> 'a1 GRing.ComUnitRing_isIntegral.axioms_ ->
      GRing.IntegralDomain.coq_type -> 'a2 GRing.IntegralDomain.axioms_ ->
      'a2 GRing.Coq_isNmodule.axioms_ -> 'a2 Choice.Coq_hasChoice.axioms_ ->
      'a2 Coq_hasDecEq.axioms_ -> 'a2 GRing.Nmodule_isSemiRing.axioms_ -> 'a2
      GRing.SemiRing_hasCommutativeMul.axioms_ -> 'a2
      GRing.Nmodule_isZmodule.axioms_ -> 'a2 GRing.Ring_hasMulInverse.axioms_
      -> 'a2 GRing.ComUnitRing_isIntegral.axioms_ -> 'a1 rel -> 'a1 rel ->
      ('a1 -> 'a1) -> 'a1 axioms_

    type ('r, 'rlocal) phant_axioms = 'r axioms_
   end

  module Builders_57 :
   sig
    val coq_Builders_57_R__canonical__eqtype_Equality :
      'a1 Coq_hasDecEq.axioms_ -> Equality.coq_type

    val coq_HB_unnamed_factory_59 :
      'a1 GRing.Coq_isNmodule.axioms_ -> 'a1 Choice.Coq_hasChoice.axioms_ ->
      'a1 Coq_hasDecEq.axioms_ -> 'a1 GRing.Nmodule_isSemiRing.axioms_ -> 'a1
      GRing.SemiRing_hasCommutativeMul.axioms_ -> 'a1
      GRing.Nmodule_isZmodule.axioms_ -> 'a1 GRing.Ring_hasMulInverse.axioms_
      -> 'a1 GRing.ComUnitRing_isIntegral.axioms_ -> ('a1, 'a1)
      IntegralDomain_isNumRing.phant_axioms -> 'a1
      Order.Order.LtLe_isPOrder.axioms_

    val coq_Num_IntegralDomain_isNumRing__to__Order_isPOrder :
      'a1 GRing.Coq_isNmodule.axioms_ -> 'a1 Choice.Coq_hasChoice.axioms_ ->
      'a1 Coq_hasDecEq.axioms_ -> 'a1 GRing.Nmodule_isSemiRing.axioms_ -> 'a1
      GRing.SemiRing_hasCommutativeMul.axioms_ -> 'a1
      GRing.Nmodule_isZmodule.axioms_ -> 'a1 GRing.Ring_hasMulInverse.axioms_
      -> 'a1 GRing.ComUnitRing_isIntegral.axioms_ -> ('a1, 'a1)
      IntegralDomain_isNumRing.phant_axioms -> 'a1
      Order.Order.Coq_isPOrder.axioms_
   end

  module IntegralDomain_isLeReal :
   sig
    type 'r axioms_ = { coq_Rle : 'r rel; coq_Rlt : 'r rel; norm : ('r -> 'r) }

    val coq_Rle :
      'a1 GRing.Coq_isNmodule.axioms_ -> 'a1 Choice.Coq_hasChoice.axioms_ ->
      'a1 Coq_hasDecEq.axioms_ -> 'a1 GRing.Nmodule_isSemiRing.axioms_ -> 'a1
      GRing.SemiRing_hasCommutativeMul.axioms_ -> 'a1
      GRing.Nmodule_isZmodule.axioms_ -> 'a1 GRing.Ring_hasMulInverse.axioms_
      -> 'a1 GRing.ComUnitRing_isIntegral.axioms_ -> 'a1 axioms_ -> 'a1 rel

    val coq_Rlt :
      'a1 GRing.Coq_isNmodule.axioms_ -> 'a1 Choice.Coq_hasChoice.axioms_ ->
      'a1 Coq_hasDecEq.axioms_ -> 'a1 GRing.Nmodule_isSemiRing.axioms_ -> 'a1
      GRing.SemiRing_hasCommutativeMul.axioms_ -> 'a1
      GRing.Nmodule_isZmodule.axioms_ -> 'a1 GRing.Ring_hasMulInverse.axioms_
      -> 'a1 GRing.ComUnitRing_isIntegral.axioms_ -> 'a1 axioms_ -> 'a1 rel

    val norm :
      'a1 GRing.Coq_isNmodule.axioms_ -> 'a1 Choice.Coq_hasChoice.axioms_ ->
      'a1 Coq_hasDecEq.axioms_ -> 'a1 GRing.Nmodule_isSemiRing.axioms_ -> 'a1
      GRing.SemiRing_hasCommutativeMul.axioms_ -> 'a1
      GRing.Nmodule_isZmodule.axioms_ -> 'a1 GRing.Ring_hasMulInverse.axioms_
      -> 'a1 GRing.ComUnitRing_isIntegral.axioms_ -> 'a1 axioms_ -> 'a1 -> 'a1

    val phant_Build :
      'a1 GRing.Coq_isNmodule.axioms_ -> 'a1 Choice.Coq_hasChoice.axioms_ ->
      'a1 Coq_hasDecEq.axioms_ -> 'a1 GRing.Nmodule_isSemiRing.axioms_ -> 'a1
      GRing.SemiRing_hasCommutativeMul.axioms_ -> 'a1
      GRing.Nmodule_isZmodule.axioms_ -> 'a1 GRing.Ring_hasMulInverse.axioms_
      -> 'a1 GRing.ComUnitRing_isIntegral.axioms_ ->
      GRing.IntegralDomain.coq_type -> 'a2 GRing.IntegralDomain.axioms_ ->
      'a2 GRing.Coq_isNmodule.axioms_ -> 'a2 Choice.Coq_hasChoice.axioms_ ->
      'a2 Coq_hasDecEq.axioms_ -> 'a2 GRing.Nmodule_isSemiRing.axioms_ -> 'a2
      GRing.SemiRing_hasCommutativeMul.axioms_ -> 'a2
      GRing.Nmodule_isZmodule.axioms_ -> 'a2 GRing.Ring_hasMulInverse.axioms_
      -> 'a2 GRing.ComUnitRing_isIntegral.axioms_ -> 'a1 rel -> 'a1 rel ->
      ('a1 -> 'a1) -> 'a1 axioms_

    type ('r, 'rlocal) phant_axioms = 'r axioms_
   end

  module Builders_73 :
   sig
    val coq_Builders_73_R__canonical__GRing_IntegralDomain :
      'a1 GRing.Coq_isNmodule.axioms_ -> 'a1 Choice.Coq_hasChoice.axioms_ ->
      'a1 Coq_hasDecEq.axioms_ -> 'a1 GRing.Nmodule_isSemiRing.axioms_ -> 'a1
      GRing.SemiRing_hasCommutativeMul.axioms_ -> 'a1
      GRing.Nmodule_isZmodule.axioms_ -> 'a1 GRing.Ring_hasMulInverse.axioms_
      -> 'a1 GRing.ComUnitRing_isIntegral.axioms_ ->
      GRing.IntegralDomain.coq_type

    val coq_HB_unnamed_factory_75 :
      'a1 GRing.Coq_isNmodule.axioms_ -> 'a1 Choice.Coq_hasChoice.axioms_ ->
      'a1 Coq_hasDecEq.axioms_ -> 'a1 GRing.Nmodule_isSemiRing.axioms_ -> 'a1
      GRing.SemiRing_hasCommutativeMul.axioms_ -> 'a1
      GRing.Nmodule_isZmodule.axioms_ -> 'a1 GRing.Ring_hasMulInverse.axioms_
      -> 'a1 GRing.ComUnitRing_isIntegral.axioms_ -> ('a1, 'a1)
      IntegralDomain_isLeReal.phant_axioms -> 'a1
      IntegralDomain_isNumRing.axioms_

    val coq_Num_IntegralDomain_isLeReal__to__Order_isPOrder :
      'a1 GRing.Coq_isNmodule.axioms_ -> 'a1 Choice.Coq_hasChoice.axioms_ ->
      'a1 Coq_hasDecEq.axioms_ -> 'a1 GRing.Nmodule_isSemiRing.axioms_ -> 'a1
      GRing.SemiRing_hasCommutativeMul.axioms_ -> 'a1
      GRing.Nmodule_isZmodule.axioms_ -> 'a1 GRing.Ring_hasMulInverse.axioms_
      -> 'a1 GRing.ComUnitRing_isIntegral.axioms_ -> ('a1, 'a1)
      IntegralDomain_isLeReal.phant_axioms -> 'a1
      Order.Order.Coq_isPOrder.axioms_
   end
 end

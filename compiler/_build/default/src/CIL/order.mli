open Eqtype
open Ssrbool

type __ = Obj.t

module Order :
 sig
  module Coq_isPOrder :
   sig
    type 't axioms_ = { le : 't rel; lt : 't rel }

    val le : unit -> 'a1 Coq_hasDecEq.axioms_ -> 'a1 axioms_ -> 'a1 rel

    val lt : unit -> 'a1 Coq_hasDecEq.axioms_ -> 'a1 axioms_ -> 'a1 rel

    val phant_Build :
      unit -> 'a1 Coq_hasDecEq.axioms_ -> Equality.coq_type -> 'a2
      Equality.axioms_ -> 'a2 Coq_hasDecEq.axioms_ -> 'a1 rel -> 'a1 rel ->
      'a1 axioms_
   end

  module POrder :
   sig
    type 't axioms_ = { choice_hasChoice_mixin : 't
                                                 Choice.Coq_hasChoice.axioms_;
                        eqtype_hasDecEq_mixin : 't Coq_hasDecEq.axioms_;
                        coq_Order_isPOrder_mixin : 't Coq_isPOrder.axioms_ }

    val eqtype_hasDecEq_mixin :
      unit -> 'a1 axioms_ -> 'a1 Coq_hasDecEq.axioms_

    val coq_Order_isPOrder_mixin :
      unit -> 'a1 axioms_ -> 'a1 Coq_isPOrder.axioms_

    type coq_type =
      __ axioms_
      (* singleton inductive, whose constructor was Pack *)

    type sort = __

    val coq_class : unit -> coq_type -> sort axioms_
   end

  val le : unit -> POrder.coq_type -> POrder.sort rel

  val lt : unit -> POrder.coq_type -> POrder.sort rel

  module LtLe_isPOrder :
   sig
    type 't axioms_ = { le : 't rel; lt : 't rel }

    val le : unit -> 'a1 Coq_hasDecEq.axioms_ -> 'a1 axioms_ -> 'a1 rel

    val lt : unit -> 'a1 Coq_hasDecEq.axioms_ -> 'a1 axioms_ -> 'a1 rel

    val phant_Build :
      unit -> 'a1 Coq_hasDecEq.axioms_ -> Equality.coq_type -> 'a2
      Equality.axioms_ -> 'a2 Coq_hasDecEq.axioms_ -> 'a1 rel -> 'a1 rel ->
      'a1 axioms_

    type ('t, 'tlocal) phant_axioms = 't axioms_
   end

  module Builders_6 :
   sig
    val coq_Builders_6_T__canonical__eqtype_Equality :
      'a1 Coq_hasDecEq.axioms_ -> Equality.coq_type

    val coq_HB_unnamed_factory_8 :
      unit -> 'a1 Coq_hasDecEq.axioms_ -> ('a1, 'a1)
      LtLe_isPOrder.phant_axioms -> 'a1 Coq_isPOrder.axioms_
   end
 end

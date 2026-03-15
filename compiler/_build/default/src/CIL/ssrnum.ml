open Eqtype
open Ssralg
open Ssrbool

type __ = Obj.t

(** val ring_display : unit **)

let ring_display =
  ()

module Num =
 struct
  module POrderedZmodule =
   struct
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

    (** val coq_class : coq_type -> sort axioms_ **)

    let coq_class record =
      record

    module Exports =
     struct
      (** val coq_Num_POrderedZmodule_class__to__Order_POrder_class :
          'a1 axioms_ -> 'a1 Order.Order.POrder.axioms_ **)

      let coq_Num_POrderedZmodule_class__to__Order_POrder_class c =
        { Order.Order.POrder.choice_hasChoice_mixin =
          c.choice_hasChoice_mixin;
          Order.Order.POrder.eqtype_hasDecEq_mixin = c.eqtype_hasDecEq_mixin;
          Order.Order.POrder.coq_Order_isPOrder_mixin =
          c.coq_Order_isPOrder_mixin }

      (** val coq_Num_POrderedZmodule__to__Order_POrder :
          coq_type -> Order.Order.POrder.coq_type **)

      let coq_Num_POrderedZmodule__to__Order_POrder =
        coq_Num_POrderedZmodule_class__to__Order_POrder_class

      (** val coq_Num_POrderedZmodule_class__to__GRing_Nmodule_class :
          'a1 axioms_ -> 'a1 GRing.Nmodule.axioms_ **)

      let coq_Num_POrderedZmodule_class__to__GRing_Nmodule_class c =
        { GRing.Nmodule.coq_GRing_isNmodule_mixin =
          c.coq_GRing_isNmodule_mixin; GRing.Nmodule.choice_hasChoice_mixin =
          c.choice_hasChoice_mixin; GRing.Nmodule.eqtype_hasDecEq_mixin =
          c.eqtype_hasDecEq_mixin }

      (** val coq_Num_POrderedZmodule__to__GRing_Nmodule :
          coq_type -> GRing.Nmodule.coq_type **)

      let coq_Num_POrderedZmodule__to__GRing_Nmodule =
        coq_Num_POrderedZmodule_class__to__GRing_Nmodule_class

      (** val join_Num_POrderedZmodule_between_GRing_Nmodule_and_Order_POrder :
          coq_type -> Order.Order.POrder.coq_type **)

      let join_Num_POrderedZmodule_between_GRing_Nmodule_and_Order_POrder =
        coq_Num_POrderedZmodule__to__Order_POrder
     end
   end

  module IntegralDomain_isNumRing =
   struct
    type 'r axioms_ = { coq_Rle : 'r rel; coq_Rlt : 'r rel; norm : ('r -> 'r) }

    (** val coq_Rle :
        'a1 GRing.Coq_isNmodule.axioms_ -> 'a1 Choice.Coq_hasChoice.axioms_
        -> 'a1 Coq_hasDecEq.axioms_ -> 'a1 GRing.Nmodule_isSemiRing.axioms_
        -> 'a1 GRing.SemiRing_hasCommutativeMul.axioms_ -> 'a1
        GRing.Nmodule_isZmodule.axioms_ -> 'a1
        GRing.Ring_hasMulInverse.axioms_ -> 'a1
        GRing.ComUnitRing_isIntegral.axioms_ -> 'a1 axioms_ -> 'a1 rel **)

    let coq_Rle _ _ _ _ _ _ _ _ record =
      record.coq_Rle

    (** val coq_Rlt :
        'a1 GRing.Coq_isNmodule.axioms_ -> 'a1 Choice.Coq_hasChoice.axioms_
        -> 'a1 Coq_hasDecEq.axioms_ -> 'a1 GRing.Nmodule_isSemiRing.axioms_
        -> 'a1 GRing.SemiRing_hasCommutativeMul.axioms_ -> 'a1
        GRing.Nmodule_isZmodule.axioms_ -> 'a1
        GRing.Ring_hasMulInverse.axioms_ -> 'a1
        GRing.ComUnitRing_isIntegral.axioms_ -> 'a1 axioms_ -> 'a1 rel **)

    let coq_Rlt _ _ _ _ _ _ _ _ record =
      record.coq_Rlt

    (** val phant_Build :
        'a1 GRing.Coq_isNmodule.axioms_ -> 'a1 Choice.Coq_hasChoice.axioms_
        -> 'a1 Coq_hasDecEq.axioms_ -> 'a1 GRing.Nmodule_isSemiRing.axioms_
        -> 'a1 GRing.SemiRing_hasCommutativeMul.axioms_ -> 'a1
        GRing.Nmodule_isZmodule.axioms_ -> 'a1
        GRing.Ring_hasMulInverse.axioms_ -> 'a1
        GRing.ComUnitRing_isIntegral.axioms_ -> GRing.IntegralDomain.coq_type
        -> 'a2 GRing.IntegralDomain.axioms_ -> 'a2
        GRing.Coq_isNmodule.axioms_ -> 'a2 Choice.Coq_hasChoice.axioms_ ->
        'a2 Coq_hasDecEq.axioms_ -> 'a2 GRing.Nmodule_isSemiRing.axioms_ ->
        'a2 GRing.SemiRing_hasCommutativeMul.axioms_ -> 'a2
        GRing.Nmodule_isZmodule.axioms_ -> 'a2
        GRing.Ring_hasMulInverse.axioms_ -> 'a2
        GRing.ComUnitRing_isIntegral.axioms_ -> 'a1 rel -> 'a1 rel -> ('a1 ->
        'a1) -> 'a1 axioms_ **)

    let phant_Build _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ rle rlt norm0 =
      { coq_Rle = rle; coq_Rlt = rlt; norm = norm0 }

    type ('r, 'rlocal) phant_axioms = 'r axioms_
   end

  module Builders_57 =
   struct
    (** val coq_Builders_57_R__canonical__eqtype_Equality :
        'a1 Coq_hasDecEq.axioms_ -> Equality.coq_type **)

    let coq_Builders_57_R__canonical__eqtype_Equality local_mixin_eqtype_hasDecEq =
      Obj.magic local_mixin_eqtype_hasDecEq

    (** val coq_HB_unnamed_factory_59 :
        'a1 GRing.Coq_isNmodule.axioms_ -> 'a1 Choice.Coq_hasChoice.axioms_
        -> 'a1 Coq_hasDecEq.axioms_ -> 'a1 GRing.Nmodule_isSemiRing.axioms_
        -> 'a1 GRing.SemiRing_hasCommutativeMul.axioms_ -> 'a1
        GRing.Nmodule_isZmodule.axioms_ -> 'a1
        GRing.Ring_hasMulInverse.axioms_ -> 'a1
        GRing.ComUnitRing_isIntegral.axioms_ -> ('a1, 'a1)
        IntegralDomain_isNumRing.phant_axioms -> 'a1
        Order.Order.LtLe_isPOrder.axioms_ **)

    let coq_HB_unnamed_factory_59 _ _ local_mixin_eqtype_hasDecEq _ _ _ _ _ fresh_name_58 =
      Order.Order.LtLe_isPOrder.phant_Build ring_display
        local_mixin_eqtype_hasDecEq
        (coq_Builders_57_R__canonical__eqtype_Equality
          local_mixin_eqtype_hasDecEq) local_mixin_eqtype_hasDecEq
        local_mixin_eqtype_hasDecEq
        fresh_name_58.IntegralDomain_isNumRing.coq_Rle
        fresh_name_58.IntegralDomain_isNumRing.coq_Rlt

    (** val coq_Num_IntegralDomain_isNumRing__to__Order_isPOrder :
        'a1 GRing.Coq_isNmodule.axioms_ -> 'a1 Choice.Coq_hasChoice.axioms_
        -> 'a1 Coq_hasDecEq.axioms_ -> 'a1 GRing.Nmodule_isSemiRing.axioms_
        -> 'a1 GRing.SemiRing_hasCommutativeMul.axioms_ -> 'a1
        GRing.Nmodule_isZmodule.axioms_ -> 'a1
        GRing.Ring_hasMulInverse.axioms_ -> 'a1
        GRing.ComUnitRing_isIntegral.axioms_ -> ('a1, 'a1)
        IntegralDomain_isNumRing.phant_axioms -> 'a1
        Order.Order.Coq_isPOrder.axioms_ **)

    let coq_Num_IntegralDomain_isNumRing__to__Order_isPOrder local_mixin_GRing_isNmodule local_mixin_choice_hasChoice local_mixin_eqtype_hasDecEq local_mixin_GRing_Nmodule_isSemiRing local_mixin_GRing_SemiRing_hasCommutativeMul local_mixin_GRing_Nmodule_isZmodule local_mixin_GRing_Ring_hasMulInverse local_mixin_GRing_ComUnitRing_isIntegral fresh_name_58 =
      Order.Order.Builders_6.coq_HB_unnamed_factory_8 ring_display
        local_mixin_eqtype_hasDecEq
        (coq_HB_unnamed_factory_59 local_mixin_GRing_isNmodule
          local_mixin_choice_hasChoice local_mixin_eqtype_hasDecEq
          local_mixin_GRing_Nmodule_isSemiRing
          local_mixin_GRing_SemiRing_hasCommutativeMul
          local_mixin_GRing_Nmodule_isZmodule
          local_mixin_GRing_Ring_hasMulInverse
          local_mixin_GRing_ComUnitRing_isIntegral fresh_name_58)
   end

  module IntegralDomain_isLeReal =
   struct
    type 'r axioms_ = { coq_Rle : 'r rel; coq_Rlt : 'r rel; norm : ('r -> 'r) }

    (** val coq_Rle :
        'a1 GRing.Coq_isNmodule.axioms_ -> 'a1 Choice.Coq_hasChoice.axioms_
        -> 'a1 Coq_hasDecEq.axioms_ -> 'a1 GRing.Nmodule_isSemiRing.axioms_
        -> 'a1 GRing.SemiRing_hasCommutativeMul.axioms_ -> 'a1
        GRing.Nmodule_isZmodule.axioms_ -> 'a1
        GRing.Ring_hasMulInverse.axioms_ -> 'a1
        GRing.ComUnitRing_isIntegral.axioms_ -> 'a1 axioms_ -> 'a1 rel **)

    let coq_Rle _ _ _ _ _ _ _ _ record =
      record.coq_Rle

    (** val coq_Rlt :
        'a1 GRing.Coq_isNmodule.axioms_ -> 'a1 Choice.Coq_hasChoice.axioms_
        -> 'a1 Coq_hasDecEq.axioms_ -> 'a1 GRing.Nmodule_isSemiRing.axioms_
        -> 'a1 GRing.SemiRing_hasCommutativeMul.axioms_ -> 'a1
        GRing.Nmodule_isZmodule.axioms_ -> 'a1
        GRing.Ring_hasMulInverse.axioms_ -> 'a1
        GRing.ComUnitRing_isIntegral.axioms_ -> 'a1 axioms_ -> 'a1 rel **)

    let coq_Rlt _ _ _ _ _ _ _ _ record =
      record.coq_Rlt

    (** val norm :
        'a1 GRing.Coq_isNmodule.axioms_ -> 'a1 Choice.Coq_hasChoice.axioms_
        -> 'a1 Coq_hasDecEq.axioms_ -> 'a1 GRing.Nmodule_isSemiRing.axioms_
        -> 'a1 GRing.SemiRing_hasCommutativeMul.axioms_ -> 'a1
        GRing.Nmodule_isZmodule.axioms_ -> 'a1
        GRing.Ring_hasMulInverse.axioms_ -> 'a1
        GRing.ComUnitRing_isIntegral.axioms_ -> 'a1 axioms_ -> 'a1 -> 'a1 **)

    let norm _ _ _ _ _ _ _ _ record =
      record.norm

    (** val phant_Build :
        'a1 GRing.Coq_isNmodule.axioms_ -> 'a1 Choice.Coq_hasChoice.axioms_
        -> 'a1 Coq_hasDecEq.axioms_ -> 'a1 GRing.Nmodule_isSemiRing.axioms_
        -> 'a1 GRing.SemiRing_hasCommutativeMul.axioms_ -> 'a1
        GRing.Nmodule_isZmodule.axioms_ -> 'a1
        GRing.Ring_hasMulInverse.axioms_ -> 'a1
        GRing.ComUnitRing_isIntegral.axioms_ -> GRing.IntegralDomain.coq_type
        -> 'a2 GRing.IntegralDomain.axioms_ -> 'a2
        GRing.Coq_isNmodule.axioms_ -> 'a2 Choice.Coq_hasChoice.axioms_ ->
        'a2 Coq_hasDecEq.axioms_ -> 'a2 GRing.Nmodule_isSemiRing.axioms_ ->
        'a2 GRing.SemiRing_hasCommutativeMul.axioms_ -> 'a2
        GRing.Nmodule_isZmodule.axioms_ -> 'a2
        GRing.Ring_hasMulInverse.axioms_ -> 'a2
        GRing.ComUnitRing_isIntegral.axioms_ -> 'a1 rel -> 'a1 rel -> ('a1 ->
        'a1) -> 'a1 axioms_ **)

    let phant_Build _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ rle rlt norm0 =
      { coq_Rle = rle; coq_Rlt = rlt; norm = norm0 }

    type ('r, 'rlocal) phant_axioms = 'r axioms_
   end

  module Builders_73 =
   struct
    (** val coq_Builders_73_R__canonical__GRing_IntegralDomain :
        'a1 GRing.Coq_isNmodule.axioms_ -> 'a1 Choice.Coq_hasChoice.axioms_
        -> 'a1 Coq_hasDecEq.axioms_ -> 'a1 GRing.Nmodule_isSemiRing.axioms_
        -> 'a1 GRing.SemiRing_hasCommutativeMul.axioms_ -> 'a1
        GRing.Nmodule_isZmodule.axioms_ -> 'a1
        GRing.Ring_hasMulInverse.axioms_ -> 'a1
        GRing.ComUnitRing_isIntegral.axioms_ -> GRing.IntegralDomain.coq_type **)

    let coq_Builders_73_R__canonical__GRing_IntegralDomain local_mixin_GRing_isNmodule local_mixin_choice_hasChoice local_mixin_eqtype_hasDecEq local_mixin_GRing_Nmodule_isSemiRing local_mixin_GRing_SemiRing_hasCommutativeMul local_mixin_GRing_Nmodule_isZmodule local_mixin_GRing_Ring_hasMulInverse local_mixin_GRing_ComUnitRing_isIntegral =
      { GRing.IntegralDomain.coq_GRing_isNmodule_mixin =
        (Obj.magic local_mixin_GRing_isNmodule);
        GRing.IntegralDomain.choice_hasChoice_mixin =
        (Obj.magic local_mixin_choice_hasChoice);
        GRing.IntegralDomain.eqtype_hasDecEq_mixin =
        (Obj.magic local_mixin_eqtype_hasDecEq);
        GRing.IntegralDomain.coq_GRing_Nmodule_isSemiRing_mixin =
        (Obj.magic local_mixin_GRing_Nmodule_isSemiRing);
        GRing.IntegralDomain.coq_GRing_SemiRing_hasCommutativeMul_mixin =
        (Obj.magic local_mixin_GRing_SemiRing_hasCommutativeMul);
        GRing.IntegralDomain.coq_GRing_Nmodule_isZmodule_mixin =
        (Obj.magic local_mixin_GRing_Nmodule_isZmodule);
        GRing.IntegralDomain.coq_GRing_Ring_hasMulInverse_mixin =
        (Obj.magic local_mixin_GRing_Ring_hasMulInverse);
        GRing.IntegralDomain.coq_GRing_ComUnitRing_isIntegral_mixin =
        (Obj.magic local_mixin_GRing_ComUnitRing_isIntegral) }

    (** val coq_HB_unnamed_factory_75 :
        'a1 GRing.Coq_isNmodule.axioms_ -> 'a1 Choice.Coq_hasChoice.axioms_
        -> 'a1 Coq_hasDecEq.axioms_ -> 'a1 GRing.Nmodule_isSemiRing.axioms_
        -> 'a1 GRing.SemiRing_hasCommutativeMul.axioms_ -> 'a1
        GRing.Nmodule_isZmodule.axioms_ -> 'a1
        GRing.Ring_hasMulInverse.axioms_ -> 'a1
        GRing.ComUnitRing_isIntegral.axioms_ -> ('a1, 'a1)
        IntegralDomain_isLeReal.phant_axioms -> 'a1
        IntegralDomain_isNumRing.axioms_ **)

    let coq_HB_unnamed_factory_75 local_mixin_GRing_isNmodule local_mixin_choice_hasChoice local_mixin_eqtype_hasDecEq local_mixin_GRing_Nmodule_isSemiRing local_mixin_GRing_SemiRing_hasCommutativeMul local_mixin_GRing_Nmodule_isZmodule local_mixin_GRing_Ring_hasMulInverse local_mixin_GRing_ComUnitRing_isIntegral fresh_name_74 =
      IntegralDomain_isNumRing.phant_Build local_mixin_GRing_isNmodule
        local_mixin_choice_hasChoice local_mixin_eqtype_hasDecEq
        local_mixin_GRing_Nmodule_isSemiRing
        local_mixin_GRing_SemiRing_hasCommutativeMul
        local_mixin_GRing_Nmodule_isZmodule
        local_mixin_GRing_Ring_hasMulInverse
        local_mixin_GRing_ComUnitRing_isIntegral
        (coq_Builders_73_R__canonical__GRing_IntegralDomain
          local_mixin_GRing_isNmodule local_mixin_choice_hasChoice
          local_mixin_eqtype_hasDecEq local_mixin_GRing_Nmodule_isSemiRing
          local_mixin_GRing_SemiRing_hasCommutativeMul
          local_mixin_GRing_Nmodule_isZmodule
          local_mixin_GRing_Ring_hasMulInverse
          local_mixin_GRing_ComUnitRing_isIntegral)
        { GRing.IntegralDomain.coq_GRing_isNmodule_mixin =
        local_mixin_GRing_isNmodule;
        GRing.IntegralDomain.choice_hasChoice_mixin =
        local_mixin_choice_hasChoice;
        GRing.IntegralDomain.eqtype_hasDecEq_mixin =
        local_mixin_eqtype_hasDecEq;
        GRing.IntegralDomain.coq_GRing_Nmodule_isSemiRing_mixin =
        local_mixin_GRing_Nmodule_isSemiRing;
        GRing.IntegralDomain.coq_GRing_SemiRing_hasCommutativeMul_mixin =
        local_mixin_GRing_SemiRing_hasCommutativeMul;
        GRing.IntegralDomain.coq_GRing_Nmodule_isZmodule_mixin =
        local_mixin_GRing_Nmodule_isZmodule;
        GRing.IntegralDomain.coq_GRing_Ring_hasMulInverse_mixin =
        local_mixin_GRing_Ring_hasMulInverse;
        GRing.IntegralDomain.coq_GRing_ComUnitRing_isIntegral_mixin =
        local_mixin_GRing_ComUnitRing_isIntegral }
        local_mixin_GRing_isNmodule local_mixin_choice_hasChoice
        local_mixin_eqtype_hasDecEq local_mixin_GRing_Nmodule_isSemiRing
        local_mixin_GRing_SemiRing_hasCommutativeMul
        local_mixin_GRing_Nmodule_isZmodule
        local_mixin_GRing_Ring_hasMulInverse
        local_mixin_GRing_ComUnitRing_isIntegral
        fresh_name_74.IntegralDomain_isLeReal.coq_Rle
        fresh_name_74.IntegralDomain_isLeReal.coq_Rlt
        fresh_name_74.IntegralDomain_isLeReal.norm

    (** val coq_Num_IntegralDomain_isLeReal__to__Order_isPOrder :
        'a1 GRing.Coq_isNmodule.axioms_ -> 'a1 Choice.Coq_hasChoice.axioms_
        -> 'a1 Coq_hasDecEq.axioms_ -> 'a1 GRing.Nmodule_isSemiRing.axioms_
        -> 'a1 GRing.SemiRing_hasCommutativeMul.axioms_ -> 'a1
        GRing.Nmodule_isZmodule.axioms_ -> 'a1
        GRing.Ring_hasMulInverse.axioms_ -> 'a1
        GRing.ComUnitRing_isIntegral.axioms_ -> ('a1, 'a1)
        IntegralDomain_isLeReal.phant_axioms -> 'a1
        Order.Order.Coq_isPOrder.axioms_ **)

    let coq_Num_IntegralDomain_isLeReal__to__Order_isPOrder local_mixin_GRing_isNmodule local_mixin_choice_hasChoice local_mixin_eqtype_hasDecEq local_mixin_GRing_Nmodule_isSemiRing local_mixin_GRing_SemiRing_hasCommutativeMul local_mixin_GRing_Nmodule_isZmodule local_mixin_GRing_Ring_hasMulInverse local_mixin_GRing_ComUnitRing_isIntegral fresh_name_74 =
      Builders_57.coq_Num_IntegralDomain_isNumRing__to__Order_isPOrder
        local_mixin_GRing_isNmodule local_mixin_choice_hasChoice
        local_mixin_eqtype_hasDecEq local_mixin_GRing_Nmodule_isSemiRing
        local_mixin_GRing_SemiRing_hasCommutativeMul
        local_mixin_GRing_Nmodule_isZmodule
        local_mixin_GRing_Ring_hasMulInverse
        local_mixin_GRing_ComUnitRing_isIntegral
        (coq_HB_unnamed_factory_75 local_mixin_GRing_isNmodule
          local_mixin_choice_hasChoice local_mixin_eqtype_hasDecEq
          local_mixin_GRing_Nmodule_isSemiRing
          local_mixin_GRing_SemiRing_hasCommutativeMul
          local_mixin_GRing_Nmodule_isZmodule
          local_mixin_GRing_Ring_hasMulInverse
          local_mixin_GRing_ComUnitRing_isIntegral fresh_name_74)
   end
 end

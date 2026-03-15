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

(** val int_to_Z : int -> coq_Z **)

let int_to_Z = function
| Posz n -> Z.of_nat n
| Negz n -> Z.opp (Z.of_nat (S n))

(** val coq_Z_to_int : coq_Z -> int **)

let coq_Z_to_int = function
| Z0 -> Posz O
| Zpos p -> Posz (Pos.to_nat p)
| Zneg p ->
  Obj.magic GRing.opp ssrint_int__canonical__GRing_Zmodule (Posz
    (Pos.to_nat p))

(** val coq_ZeqbP : coq_Z -> coq_Z -> reflect **)

let coq_ZeqbP x y =
  iffP (Z.eqb x y) (if Z.eqb x y then ReflectT else ReflectF)

(** val coq_HB_unnamed_factory_1 : coq_Z Coq_hasDecEq.axioms_ **)

let coq_HB_unnamed_factory_1 =
  { Coq_hasDecEq.eq_op = Z.eqb; Coq_hasDecEq.eqP = coq_ZeqbP }

(** val coq_BinNums_Z__canonical__eqtype_Equality : Equality.coq_type **)

let coq_BinNums_Z__canonical__eqtype_Equality =
  Obj.magic coq_HB_unnamed_factory_1

(** val coq_HB_unnamed_factory_3 : coq_Z Choice.Countable.axioms_ **)

let coq_HB_unnamed_factory_3 =
  Obj.magic Choice.eqtype_can_type__canonical__choice_Countable
    ssrint_int__canonical__choice_Countable coq_Z_to_int int_to_Z

(** val coq_HB_unnamed_mixin_7 : coq_Z Choice.Coq_hasChoice.axioms_ **)

let coq_HB_unnamed_mixin_7 =
  coq_HB_unnamed_factory_3.Choice.Countable.choice_hasChoice_mixin

(** val coq_HB_unnamed_mixin_8 : coq_Z Choice.Choice_isCountable.axioms_ **)

let coq_HB_unnamed_mixin_8 =
  coq_HB_unnamed_factory_3.Choice.Countable.choice_Choice_isCountable_mixin

(** val coq_BinNums_Z__canonical__choice_Countable :
    Choice.Countable.coq_type **)

let coq_BinNums_Z__canonical__choice_Countable =
  { Choice.Countable.choice_hasChoice_mixin =
    (Obj.magic coq_HB_unnamed_mixin_7);
    Choice.Countable.eqtype_hasDecEq_mixin =
    (Obj.magic coq_HB_unnamed_factory_1);
    Choice.Countable.choice_Choice_isCountable_mixin =
    (Obj.magic coq_HB_unnamed_mixin_8) }

(** val coq_HB_unnamed_factory_9 : coq_Z GRing.Coq_isZmodule.axioms_ **)

let coq_HB_unnamed_factory_9 =
  { GRing.Coq_isZmodule.zero = Z0; GRing.Coq_isZmodule.opp = Z.opp;
    GRing.Coq_isZmodule.add = Z.add }

(** val coq_HB_unnamed_mixin_12 : coq_Z GRing.Coq_isNmodule.axioms_ **)

let coq_HB_unnamed_mixin_12 =
  GRing.Builders_8.coq_HB_unnamed_factory_10 coq_HB_unnamed_mixin_7
    coq_HB_unnamed_factory_1 coq_HB_unnamed_factory_9

(** val coq_BinNums_Z__canonical__GRing_Nmodule : GRing.Nmodule.coq_type **)

let coq_BinNums_Z__canonical__GRing_Nmodule =
  { GRing.Nmodule.coq_GRing_isNmodule_mixin =
    (Obj.magic coq_HB_unnamed_mixin_12);
    GRing.Nmodule.choice_hasChoice_mixin =
    (Obj.magic coq_HB_unnamed_mixin_7); GRing.Nmodule.eqtype_hasDecEq_mixin =
    (Obj.magic coq_HB_unnamed_factory_1) }

(** val coq_HB_unnamed_mixin_13 : coq_Z GRing.Nmodule_isZmodule.axioms_ **)

let coq_HB_unnamed_mixin_13 =
  GRing.Builders_8.coq_HB_unnamed_factory_12 coq_HB_unnamed_mixin_7
    coq_HB_unnamed_factory_1 coq_HB_unnamed_factory_9

(** val coq_BinNums_Z__canonical__GRing_Zmodule : GRing.Zmodule.coq_type **)

let coq_BinNums_Z__canonical__GRing_Zmodule =
  { GRing.Zmodule.coq_GRing_isNmodule_mixin =
    (Obj.magic coq_HB_unnamed_mixin_12);
    GRing.Zmodule.choice_hasChoice_mixin =
    (Obj.magic coq_HB_unnamed_mixin_7); GRing.Zmodule.eqtype_hasDecEq_mixin =
    (Obj.magic coq_HB_unnamed_factory_1);
    GRing.Zmodule.coq_GRing_Nmodule_isZmodule_mixin =
    (Obj.magic coq_HB_unnamed_mixin_13) }

(** val coq_HB_unnamed_factory_14 : coq_Z GRing.Zmodule_isComRing.axioms_ **)

let coq_HB_unnamed_factory_14 =
  { GRing.Zmodule_isComRing.one = (Zpos Coq_xH);
    GRing.Zmodule_isComRing.mul = Z.mul }

(** val coq_HB_unnamed_mixin_17 : coq_Z GRing.Nmodule_isSemiRing.axioms_ **)

let coq_HB_unnamed_mixin_17 =
  GRing.Builders_211.coq_GRing_Zmodule_isComRing__to__GRing_Nmodule_isSemiRing
    coq_HB_unnamed_mixin_12 coq_HB_unnamed_mixin_7 coq_HB_unnamed_factory_1
    coq_HB_unnamed_mixin_13 coq_HB_unnamed_factory_14

(** val coq_BinNums_Z__canonical__GRing_SemiRing : GRing.SemiRing.coq_type **)

let coq_BinNums_Z__canonical__GRing_SemiRing =
  { GRing.SemiRing.coq_GRing_isNmodule_mixin =
    (Obj.magic coq_HB_unnamed_mixin_12);
    GRing.SemiRing.choice_hasChoice_mixin =
    (Obj.magic coq_HB_unnamed_mixin_7);
    GRing.SemiRing.eqtype_hasDecEq_mixin =
    (Obj.magic coq_HB_unnamed_factory_1);
    GRing.SemiRing.coq_GRing_Nmodule_isSemiRing_mixin =
    (Obj.magic coq_HB_unnamed_mixin_17) }

(** val coq_HB_unnamed_mixin_18 :
    coq_Z GRing.SemiRing_hasCommutativeMul.axioms_ **)

let coq_HB_unnamed_mixin_18 =
  GRing.Builders_211.coq_GRing_Zmodule_isComRing__to__GRing_SemiRing_hasCommutativeMul
    coq_HB_unnamed_mixin_12 coq_HB_unnamed_mixin_7 coq_HB_unnamed_factory_1
    coq_HB_unnamed_mixin_13 coq_HB_unnamed_factory_14

module ZUnitRing =
 struct
  (** val unitZ : coq_Z qualifier **)

  let unitZ =
    Obj.magic (fun n ->
      (||)
        (eq_op coq_BinNums_Z__canonical__eqtype_Equality n
          (Obj.magic (Zpos Coq_xH)))
        (eq_op coq_BinNums_Z__canonical__eqtype_Equality n
          (Obj.magic (Zneg Coq_xH))))

  (** val invZ : coq_Z -> coq_Z **)

  let invZ n =
    n
 end

(** val coq_HB_unnamed_factory_19 :
    coq_Z GRing.ComRing_hasMulInverse.axioms_ **)

let coq_HB_unnamed_factory_19 =
  { GRing.ComRing_hasMulInverse.coq_unit =
    (has_quality (S O) ZUnitRing.unitZ); GRing.ComRing_hasMulInverse.inv =
    ZUnitRing.invZ }

(** val coq_HB_unnamed_mixin_21 : coq_Z GRing.Ring_hasMulInverse.axioms_ **)

let coq_HB_unnamed_mixin_21 =
  GRing.Builders_256.coq_HB_unnamed_factory_258 coq_HB_unnamed_mixin_12
    coq_HB_unnamed_mixin_7 coq_HB_unnamed_factory_1 coq_HB_unnamed_mixin_13
    coq_HB_unnamed_mixin_17 coq_HB_unnamed_mixin_18 coq_HB_unnamed_factory_19

(** val coq_HB_unnamed_factory_22 :
    coq_Z GRing.ComUnitRing_isIntegral.axioms_ **)

let coq_HB_unnamed_factory_22 =
  GRing.ComUnitRing_isIntegral.Axioms_

(** val coq_BinNums_Z__canonical__GRing_IntegralDomain :
    GRing.IntegralDomain.coq_type **)

let coq_BinNums_Z__canonical__GRing_IntegralDomain =
  { GRing.IntegralDomain.coq_GRing_isNmodule_mixin =
    (Obj.magic coq_HB_unnamed_mixin_12);
    GRing.IntegralDomain.choice_hasChoice_mixin =
    (Obj.magic coq_HB_unnamed_mixin_7);
    GRing.IntegralDomain.eqtype_hasDecEq_mixin =
    (Obj.magic coq_HB_unnamed_factory_1);
    GRing.IntegralDomain.coq_GRing_Nmodule_isSemiRing_mixin =
    (Obj.magic coq_HB_unnamed_mixin_17);
    GRing.IntegralDomain.coq_GRing_SemiRing_hasCommutativeMul_mixin =
    (Obj.magic coq_HB_unnamed_mixin_18);
    GRing.IntegralDomain.coq_GRing_Nmodule_isZmodule_mixin =
    (Obj.magic coq_HB_unnamed_mixin_13);
    GRing.IntegralDomain.coq_GRing_Ring_hasMulInverse_mixin =
    (Obj.magic coq_HB_unnamed_mixin_21);
    GRing.IntegralDomain.coq_GRing_ComUnitRing_isIntegral_mixin =
    (Obj.magic coq_HB_unnamed_factory_22) }

module ZNumDomain =
 struct
  (** val coq_Z_realLeMixin : coq_Z Num.IntegralDomain_isLeReal.axioms_ **)

  let coq_Z_realLeMixin =
    Num.IntegralDomain_isLeReal.phant_Build coq_HB_unnamed_mixin_12
      coq_HB_unnamed_mixin_7 coq_HB_unnamed_factory_1 coq_HB_unnamed_mixin_17
      coq_HB_unnamed_mixin_18 coq_HB_unnamed_mixin_13 coq_HB_unnamed_mixin_21
      coq_HB_unnamed_factory_22
      coq_BinNums_Z__canonical__GRing_IntegralDomain
      { GRing.IntegralDomain.coq_GRing_isNmodule_mixin =
      coq_HB_unnamed_mixin_12; GRing.IntegralDomain.choice_hasChoice_mixin =
      coq_HB_unnamed_mixin_7; GRing.IntegralDomain.eqtype_hasDecEq_mixin =
      coq_HB_unnamed_factory_1;
      GRing.IntegralDomain.coq_GRing_Nmodule_isSemiRing_mixin =
      coq_HB_unnamed_mixin_17;
      GRing.IntegralDomain.coq_GRing_SemiRing_hasCommutativeMul_mixin =
      coq_HB_unnamed_mixin_18;
      GRing.IntegralDomain.coq_GRing_Nmodule_isZmodule_mixin =
      coq_HB_unnamed_mixin_13;
      GRing.IntegralDomain.coq_GRing_Ring_hasMulInverse_mixin =
      coq_HB_unnamed_mixin_21;
      GRing.IntegralDomain.coq_GRing_ComUnitRing_isIntegral_mixin =
      coq_HB_unnamed_factory_22 } coq_HB_unnamed_mixin_12
      coq_HB_unnamed_mixin_7 coq_HB_unnamed_factory_1 coq_HB_unnamed_mixin_17
      coq_HB_unnamed_mixin_18 coq_HB_unnamed_mixin_13 coq_HB_unnamed_mixin_21
      coq_HB_unnamed_factory_22 Z.leb Z.ltb Z.abs
 end

(** val coq_HB_unnamed_factory_24 :
    coq_Z Num.IntegralDomain_isLeReal.axioms_ **)

let coq_HB_unnamed_factory_24 =
  ZNumDomain.coq_Z_realLeMixin

(** val coq_HB_unnamed_mixin_31 : coq_Z Order.Order.Coq_isPOrder.axioms_ **)

let coq_HB_unnamed_mixin_31 =
  Num.Builders_73.coq_Num_IntegralDomain_isLeReal__to__Order_isPOrder
    coq_HB_unnamed_mixin_12 coq_HB_unnamed_mixin_7 coq_HB_unnamed_factory_1
    coq_HB_unnamed_mixin_17 coq_HB_unnamed_mixin_18 coq_HB_unnamed_mixin_13
    coq_HB_unnamed_mixin_21 coq_HB_unnamed_factory_22
    coq_HB_unnamed_factory_24

(** val coq_BinNums_Z__canonical__Order_POrder :
    Order.Order.POrder.coq_type **)

let coq_BinNums_Z__canonical__Order_POrder =
  { Order.Order.POrder.choice_hasChoice_mixin =
    (Obj.magic coq_HB_unnamed_mixin_7);
    Order.Order.POrder.eqtype_hasDecEq_mixin =
    (Obj.magic coq_HB_unnamed_factory_1);
    Order.Order.POrder.coq_Order_isPOrder_mixin =
    (Obj.magic coq_HB_unnamed_mixin_31) }

(** val coq_BinNums_Z__canonical__Num_POrderedZmodule :
    Num.POrderedZmodule.coq_type **)

let coq_BinNums_Z__canonical__Num_POrderedZmodule =
  { Num.POrderedZmodule.coq_GRing_isNmodule_mixin =
    (Obj.magic coq_HB_unnamed_mixin_12);
    Num.POrderedZmodule.choice_hasChoice_mixin =
    (Obj.magic coq_HB_unnamed_mixin_7);
    Num.POrderedZmodule.eqtype_hasDecEq_mixin =
    (Obj.magic coq_HB_unnamed_factory_1);
    Num.POrderedZmodule.coq_Order_isPOrder_mixin =
    (Obj.magic coq_HB_unnamed_mixin_31);
    Num.POrderedZmodule.coq_GRing_Nmodule_isZmodule_mixin =
    (Obj.magic coq_HB_unnamed_mixin_13) }

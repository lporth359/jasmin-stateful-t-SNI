open Datatypes
open Eqtype
open Ssrbool
open Ssrnat

type __ = Obj.t

module GRing :
 sig
  module Coq_isNmodule :
   sig
    type 'v axioms_ = { zero : 'v; add : ('v -> 'v -> 'v) }

    val zero : 'a1 axioms_ -> 'a1

    val add : 'a1 axioms_ -> 'a1 -> 'a1 -> 'a1

    val phant_Build : 'a1 -> ('a1 -> 'a1 -> 'a1) -> 'a1 axioms_
   end

  module Nmodule :
   sig
    type 'v axioms_ = { coq_GRing_isNmodule_mixin : 'v Coq_isNmodule.axioms_;
                        choice_hasChoice_mixin : 'v
                                                 Choice.Coq_hasChoice.axioms_;
                        eqtype_hasDecEq_mixin : 'v Coq_hasDecEq.axioms_ }

    val coq_GRing_isNmodule_mixin : 'a1 axioms_ -> 'a1 Coq_isNmodule.axioms_

    type coq_type =
      __ axioms_
      (* singleton inductive, whose constructor was Pack *)

    type sort = __

    val coq_class : coq_type -> sort axioms_
   end

  val zero : Nmodule.coq_type -> Nmodule.sort

  val add : Nmodule.coq_type -> Nmodule.sort -> Nmodule.sort -> Nmodule.sort

  val natmul : Nmodule.coq_type -> Nmodule.sort -> nat -> Nmodule.sort

  module Nmodule_isZmodule :
   sig
    type 'v axioms_ =
      'v -> 'v
      (* singleton inductive, whose constructor was Axioms_ *)

    val opp :
      'a1 Coq_isNmodule.axioms_ -> 'a1 Choice.Coq_hasChoice.axioms_ -> 'a1
      Coq_hasDecEq.axioms_ -> 'a1 axioms_ -> 'a1 -> 'a1

    val phant_Build :
      'a1 Coq_isNmodule.axioms_ -> 'a1 Choice.Coq_hasChoice.axioms_ -> 'a1
      Coq_hasDecEq.axioms_ -> Nmodule.coq_type -> 'a2 Nmodule.axioms_ -> 'a2
      Coq_isNmodule.axioms_ -> 'a2 Choice.Coq_hasChoice.axioms_ -> 'a2
      Coq_hasDecEq.axioms_ -> Choice.Choice.coq_type -> 'a3
      Choice.Choice.axioms_ -> 'a3 Choice.Coq_hasChoice.axioms_ -> 'a3
      Coq_hasDecEq.axioms_ -> Equality.coq_type -> 'a4 Equality.axioms_ ->
      'a4 Coq_hasDecEq.axioms_ -> ('a1 -> 'a1) -> 'a1 axioms_
   end

  module Zmodule :
   sig
    type 'v axioms_ = { coq_GRing_isNmodule_mixin : 'v Coq_isNmodule.axioms_;
                        choice_hasChoice_mixin : 'v
                                                 Choice.Coq_hasChoice.axioms_;
                        eqtype_hasDecEq_mixin : 'v Coq_hasDecEq.axioms_;
                        coq_GRing_Nmodule_isZmodule_mixin : 'v
                                                            Nmodule_isZmodule.axioms_ }

    val coq_GRing_isNmodule_mixin : 'a1 axioms_ -> 'a1 Coq_isNmodule.axioms_

    val choice_hasChoice_mixin :
      'a1 axioms_ -> 'a1 Choice.Coq_hasChoice.axioms_

    val eqtype_hasDecEq_mixin : 'a1 axioms_ -> 'a1 Coq_hasDecEq.axioms_

    val coq_GRing_Nmodule_isZmodule_mixin :
      'a1 axioms_ -> 'a1 Nmodule_isZmodule.axioms_

    type coq_type =
      __ axioms_
      (* singleton inductive, whose constructor was Pack *)

    type sort = __

    val coq_class : coq_type -> sort axioms_

    module Exports :
     sig
      val coq_GRing_Zmodule_class__to__GRing_Nmodule_class :
        'a1 axioms_ -> 'a1 Nmodule.axioms_

      val coq_GRing_Zmodule__to__GRing_Nmodule : coq_type -> Nmodule.coq_type
     end
   end

  val opp : Zmodule.coq_type -> Zmodule.sort -> Zmodule.sort

  module Coq_isZmodule :
   sig
    type 'v axioms_ = { zero : 'v; opp : ('v -> 'v); add : ('v -> 'v -> 'v) }

    val zero :
      'a1 Choice.Coq_hasChoice.axioms_ -> 'a1 Coq_hasDecEq.axioms_ -> 'a1
      axioms_ -> 'a1

    val opp :
      'a1 Choice.Coq_hasChoice.axioms_ -> 'a1 Coq_hasDecEq.axioms_ -> 'a1
      axioms_ -> 'a1 -> 'a1

    val add :
      'a1 Choice.Coq_hasChoice.axioms_ -> 'a1 Coq_hasDecEq.axioms_ -> 'a1
      axioms_ -> 'a1 -> 'a1 -> 'a1

    val phant_Build :
      'a1 Choice.Coq_hasChoice.axioms_ -> 'a1 Coq_hasDecEq.axioms_ ->
      Choice.Choice.coq_type -> 'a2 Choice.Choice.axioms_ -> 'a2
      Choice.Coq_hasChoice.axioms_ -> 'a2 Coq_hasDecEq.axioms_ ->
      Equality.coq_type -> 'a3 Equality.axioms_ -> 'a3 Coq_hasDecEq.axioms_
      -> 'a1 -> ('a1 -> 'a1) -> ('a1 -> 'a1 -> 'a1) -> 'a1 axioms_

    type ('v, 'tlocal, 'tlocal0) phant_axioms = 'v axioms_
   end

  module Builders_8 :
   sig
    val coq_Builders_8_V__canonical__eqtype_Equality :
      'a1 Coq_hasDecEq.axioms_ -> Equality.coq_type

    val coq_Builders_8_V__canonical__choice_Choice :
      'a1 Choice.Coq_hasChoice.axioms_ -> 'a1 Coq_hasDecEq.axioms_ ->
      Choice.Choice.coq_type

    val coq_HB_unnamed_factory_10 :
      'a1 Choice.Coq_hasChoice.axioms_ -> 'a1 Coq_hasDecEq.axioms_ -> ('a1,
      'a1, 'a1) Coq_isZmodule.phant_axioms -> 'a1 Coq_isNmodule.axioms_

    val coq_Builders_8_V__canonical__GRing_Nmodule :
      'a1 Choice.Coq_hasChoice.axioms_ -> 'a1 Coq_hasDecEq.axioms_ -> ('a1,
      'a1, 'a1) Coq_isZmodule.phant_axioms -> Nmodule.coq_type

    val coq_HB_unnamed_factory_12 :
      'a1 Choice.Coq_hasChoice.axioms_ -> 'a1 Coq_hasDecEq.axioms_ -> ('a1,
      'a1, 'a1) Coq_isZmodule.phant_axioms -> 'a1 Nmodule_isZmodule.axioms_
   end

  module Nmodule_isSemiRing :
   sig
    type 'r axioms_ = { one : 'r; mul : ('r -> 'r -> 'r) }

    val one :
      'a1 Coq_isNmodule.axioms_ -> 'a1 Choice.Coq_hasChoice.axioms_ -> 'a1
      Coq_hasDecEq.axioms_ -> 'a1 axioms_ -> 'a1

    val mul :
      'a1 Coq_isNmodule.axioms_ -> 'a1 Choice.Coq_hasChoice.axioms_ -> 'a1
      Coq_hasDecEq.axioms_ -> 'a1 axioms_ -> 'a1 -> 'a1 -> 'a1

    val phant_Build :
      'a1 Coq_isNmodule.axioms_ -> 'a1 Choice.Coq_hasChoice.axioms_ -> 'a1
      Coq_hasDecEq.axioms_ -> Nmodule.coq_type -> 'a2 Nmodule.axioms_ -> 'a2
      Coq_isNmodule.axioms_ -> 'a2 Choice.Coq_hasChoice.axioms_ -> 'a2
      Coq_hasDecEq.axioms_ -> Choice.Choice.coq_type -> 'a3
      Choice.Choice.axioms_ -> 'a3 Choice.Coq_hasChoice.axioms_ -> 'a3
      Coq_hasDecEq.axioms_ -> Equality.coq_type -> 'a4 Equality.axioms_ ->
      'a4 Coq_hasDecEq.axioms_ -> 'a1 -> ('a1 -> 'a1 -> 'a1) -> 'a1 axioms_
   end

  module SemiRing :
   sig
    type 'r axioms_ = { coq_GRing_isNmodule_mixin : 'r Coq_isNmodule.axioms_;
                        choice_hasChoice_mixin : 'r
                                                 Choice.Coq_hasChoice.axioms_;
                        eqtype_hasDecEq_mixin : 'r Coq_hasDecEq.axioms_;
                        coq_GRing_Nmodule_isSemiRing_mixin : 'r
                                                             Nmodule_isSemiRing.axioms_ }

    val coq_GRing_isNmodule_mixin : 'a1 axioms_ -> 'a1 Coq_isNmodule.axioms_

    val choice_hasChoice_mixin :
      'a1 axioms_ -> 'a1 Choice.Coq_hasChoice.axioms_

    val eqtype_hasDecEq_mixin : 'a1 axioms_ -> 'a1 Coq_hasDecEq.axioms_

    val coq_GRing_Nmodule_isSemiRing_mixin :
      'a1 axioms_ -> 'a1 Nmodule_isSemiRing.axioms_

    type coq_type =
      __ axioms_
      (* singleton inductive, whose constructor was Pack *)

    type sort = __

    val coq_class : coq_type -> sort axioms_

    module Exports :
     sig
      val coq_GRing_SemiRing_class__to__eqtype_Equality_class :
        'a1 axioms_ -> 'a1 Equality.axioms_

      val coq_GRing_SemiRing__to__eqtype_Equality :
        coq_type -> Equality.coq_type

      val coq_GRing_SemiRing_class__to__GRing_Nmodule_class :
        'a1 axioms_ -> 'a1 Nmodule.axioms_

      val coq_GRing_SemiRing__to__GRing_Nmodule : coq_type -> Nmodule.coq_type
     end
   end

  val one : SemiRing.coq_type -> SemiRing.sort

  val mul :
    SemiRing.coq_type -> SemiRing.sort -> SemiRing.sort -> SemiRing.sort

  val exp : SemiRing.coq_type -> SemiRing.sort -> nat -> SemiRing.sort

  module Ring :
   sig
    type 'r axioms_ = { coq_GRing_isNmodule_mixin : 'r Coq_isNmodule.axioms_;
                        choice_hasChoice_mixin : 'r
                                                 Choice.Coq_hasChoice.axioms_;
                        eqtype_hasDecEq_mixin : 'r Coq_hasDecEq.axioms_;
                        coq_GRing_Nmodule_isSemiRing_mixin : 'r
                                                             Nmodule_isSemiRing.axioms_;
                        coq_GRing_Nmodule_isZmodule_mixin : 'r
                                                            Nmodule_isZmodule.axioms_ }

    type coq_type =
      __ axioms_
      (* singleton inductive, whose constructor was Pack *)

    type sort = __

    val coq_class : coq_type -> sort axioms_

    module Exports :
     sig
      val coq_GRing_Ring_class__to__eqtype_Equality_class :
        'a1 axioms_ -> 'a1 Equality.axioms_

      val coq_GRing_Ring__to__eqtype_Equality : coq_type -> Equality.coq_type

      val coq_GRing_Ring_class__to__GRing_Zmodule_class :
        'a1 axioms_ -> 'a1 Zmodule.axioms_

      val coq_GRing_Ring__to__GRing_Zmodule : coq_type -> Zmodule.coq_type

      val coq_GRing_Ring_class__to__GRing_SemiRing_class :
        'a1 axioms_ -> 'a1 SemiRing.axioms_

      val coq_GRing_Ring__to__GRing_SemiRing : coq_type -> SemiRing.coq_type

      val join_GRing_Ring_between_GRing_SemiRing_and_GRing_Zmodule :
        coq_type -> Zmodule.coq_type
     end
   end

  module Zmodule_isRing :
   sig
    type 'r axioms_ = { one : 'r; mul : ('r -> 'r -> 'r) }

    val one :
      'a1 Coq_isNmodule.axioms_ -> 'a1 Choice.Coq_hasChoice.axioms_ -> 'a1
      Coq_hasDecEq.axioms_ -> 'a1 Nmodule_isZmodule.axioms_ -> 'a1 axioms_ ->
      'a1

    val mul :
      'a1 Coq_isNmodule.axioms_ -> 'a1 Choice.Coq_hasChoice.axioms_ -> 'a1
      Coq_hasDecEq.axioms_ -> 'a1 Nmodule_isZmodule.axioms_ -> 'a1 axioms_ ->
      'a1 -> 'a1 -> 'a1

    val phant_Build :
      'a1 Coq_isNmodule.axioms_ -> 'a1 Choice.Coq_hasChoice.axioms_ -> 'a1
      Coq_hasDecEq.axioms_ -> 'a1 Nmodule_isZmodule.axioms_ ->
      Zmodule.coq_type -> 'a2 Zmodule.axioms_ -> 'a2 Coq_isNmodule.axioms_ ->
      'a2 Choice.Coq_hasChoice.axioms_ -> 'a2 Coq_hasDecEq.axioms_ -> 'a2
      Nmodule_isZmodule.axioms_ -> 'a1 -> ('a1 -> 'a1 -> 'a1) -> 'a1 axioms_

    type ('r, 'vlocal) phant_axioms = 'r axioms_
   end

  module Builders_31 :
   sig
    val coq_Builders_31_R__canonical__eqtype_Equality :
      'a1 Coq_hasDecEq.axioms_ -> Equality.coq_type

    val coq_Builders_31_R__canonical__choice_Choice :
      'a1 Choice.Coq_hasChoice.axioms_ -> 'a1 Coq_hasDecEq.axioms_ ->
      Choice.Choice.coq_type

    val coq_Builders_31_R__canonical__GRing_Nmodule :
      'a1 Coq_isNmodule.axioms_ -> 'a1 Choice.Coq_hasChoice.axioms_ -> 'a1
      Coq_hasDecEq.axioms_ -> Nmodule.coq_type

    val coq_HB_unnamed_factory_33 :
      'a1 Coq_isNmodule.axioms_ -> 'a1 Choice.Coq_hasChoice.axioms_ -> 'a1
      Coq_hasDecEq.axioms_ -> 'a1 Nmodule_isZmodule.axioms_ -> ('a1, 'a1)
      Zmodule_isRing.phant_axioms -> 'a1 Nmodule_isSemiRing.axioms_
   end

  module SemiRing_hasCommutativeMul :
   sig
    type 'r axioms_ =
    | Axioms_

    val phant_Build :
      'a1 Coq_isNmodule.axioms_ -> 'a1 Choice.Coq_hasChoice.axioms_ -> 'a1
      Coq_hasDecEq.axioms_ -> 'a1 Nmodule_isSemiRing.axioms_ ->
      SemiRing.coq_type -> 'a2 SemiRing.axioms_ -> 'a2 Coq_isNmodule.axioms_
      -> 'a2 Choice.Coq_hasChoice.axioms_ -> 'a2 Coq_hasDecEq.axioms_ -> 'a2
      Nmodule_isSemiRing.axioms_ -> 'a1 axioms_
   end

  module ComRing :
   sig
    type 'r axioms_ = { coq_GRing_isNmodule_mixin : 'r Coq_isNmodule.axioms_;
                        choice_hasChoice_mixin : 'r
                                                 Choice.Coq_hasChoice.axioms_;
                        eqtype_hasDecEq_mixin : 'r Coq_hasDecEq.axioms_;
                        coq_GRing_Nmodule_isZmodule_mixin : 'r
                                                            Nmodule_isZmodule.axioms_;
                        coq_GRing_Nmodule_isSemiRing_mixin : 'r
                                                             Nmodule_isSemiRing.axioms_;
                        coq_GRing_SemiRing_hasCommutativeMul_mixin : 
                        'r SemiRing_hasCommutativeMul.axioms_ }

    type coq_type =
      __ axioms_
      (* singleton inductive, whose constructor was Pack *)

    type sort = __

    val coq_class : coq_type -> sort axioms_

    module Exports :
     sig
      val coq_GRing_ComRing_class__to__eqtype_Equality_class :
        'a1 axioms_ -> 'a1 Equality.axioms_

      val coq_GRing_ComRing__to__eqtype_Equality :
        coq_type -> Equality.coq_type

      val coq_GRing_ComRing_class__to__GRing_Nmodule_class :
        'a1 axioms_ -> 'a1 Nmodule.axioms_

      val coq_GRing_ComRing__to__GRing_Nmodule : coq_type -> Nmodule.coq_type

      val coq_GRing_ComRing_class__to__GRing_Zmodule_class :
        'a1 axioms_ -> 'a1 Zmodule.axioms_

      val coq_GRing_ComRing__to__GRing_Zmodule : coq_type -> Zmodule.coq_type

      val coq_GRing_ComRing_class__to__GRing_SemiRing_class :
        'a1 axioms_ -> 'a1 SemiRing.axioms_

      val coq_GRing_ComRing__to__GRing_SemiRing :
        coq_type -> SemiRing.coq_type

      val coq_GRing_ComRing_class__to__GRing_Ring_class :
        'a1 axioms_ -> 'a1 Ring.axioms_

      val coq_GRing_ComRing__to__GRing_Ring : coq_type -> Ring.coq_type
     end
   end

  module Ring_hasCommutativeMul :
   sig
    type 'r axioms_ =
    | Axioms_

    val phant_Build :
      'a1 Coq_isNmodule.axioms_ -> 'a1 Choice.Coq_hasChoice.axioms_ -> 'a1
      Coq_hasDecEq.axioms_ -> 'a1 Nmodule_isSemiRing.axioms_ -> 'a1
      Nmodule_isZmodule.axioms_ -> SemiRing.coq_type -> 'a2 SemiRing.axioms_
      -> 'a2 Coq_isNmodule.axioms_ -> 'a2 Choice.Coq_hasChoice.axioms_ -> 'a2
      Coq_hasDecEq.axioms_ -> 'a2 Nmodule_isSemiRing.axioms_ ->
      Zmodule.coq_type -> 'a3 Zmodule.axioms_ -> 'a3 Coq_isNmodule.axioms_ ->
      'a3 Choice.Coq_hasChoice.axioms_ -> 'a3 Coq_hasDecEq.axioms_ -> 'a3
      Nmodule_isZmodule.axioms_ -> 'a1 axioms_

    type ('r, 'rlocal, 'vlocal) phant_axioms = 'r axioms_
   end

  module Builders_206 :
   sig
    val coq_Builders_206_R__canonical__GRing_SemiRing :
      'a1 Coq_isNmodule.axioms_ -> 'a1 Choice.Coq_hasChoice.axioms_ -> 'a1
      Coq_hasDecEq.axioms_ -> 'a1 Nmodule_isSemiRing.axioms_ ->
      SemiRing.coq_type

    val coq_HB_unnamed_factory_208 :
      'a1 Coq_isNmodule.axioms_ -> 'a1 Choice.Coq_hasChoice.axioms_ -> 'a1
      Coq_hasDecEq.axioms_ -> 'a1 Nmodule_isSemiRing.axioms_ -> 'a1
      Nmodule_isZmodule.axioms_ -> ('a1, 'a1, 'a1)
      Ring_hasCommutativeMul.phant_axioms -> 'a1
      SemiRing_hasCommutativeMul.axioms_
   end

  module Zmodule_isComRing :
   sig
    type 'r axioms_ = { one : 'r; mul : ('r -> 'r -> 'r) }

    val one :
      'a1 Coq_isNmodule.axioms_ -> 'a1 Choice.Coq_hasChoice.axioms_ -> 'a1
      Coq_hasDecEq.axioms_ -> 'a1 Nmodule_isZmodule.axioms_ -> 'a1 axioms_ ->
      'a1

    val mul :
      'a1 Coq_isNmodule.axioms_ -> 'a1 Choice.Coq_hasChoice.axioms_ -> 'a1
      Coq_hasDecEq.axioms_ -> 'a1 Nmodule_isZmodule.axioms_ -> 'a1 axioms_ ->
      'a1 -> 'a1 -> 'a1

    type ('r, 'vlocal) phant_axioms = 'r axioms_
   end

  module Builders_211 :
   sig
    val coq_Builders_211_R__canonical__GRing_Zmodule :
      'a1 Coq_isNmodule.axioms_ -> 'a1 Choice.Coq_hasChoice.axioms_ -> 'a1
      Coq_hasDecEq.axioms_ -> 'a1 Nmodule_isZmodule.axioms_ ->
      Zmodule.coq_type

    val coq_HB_unnamed_factory_213 :
      'a1 Coq_isNmodule.axioms_ -> 'a1 Choice.Coq_hasChoice.axioms_ -> 'a1
      Coq_hasDecEq.axioms_ -> 'a1 Nmodule_isZmodule.axioms_ -> ('a1, 'a1)
      Zmodule_isComRing.phant_axioms -> 'a1 Zmodule_isRing.axioms_

    val coq_GRing_Zmodule_isComRing__to__GRing_Nmodule_isSemiRing :
      'a1 Coq_isNmodule.axioms_ -> 'a1 Choice.Coq_hasChoice.axioms_ -> 'a1
      Coq_hasDecEq.axioms_ -> 'a1 Nmodule_isZmodule.axioms_ -> ('a1, 'a1)
      Zmodule_isComRing.phant_axioms -> 'a1 Nmodule_isSemiRing.axioms_

    val coq_Builders_211_R__canonical__GRing_SemiRing :
      'a1 Coq_isNmodule.axioms_ -> 'a1 Choice.Coq_hasChoice.axioms_ -> 'a1
      Coq_hasDecEq.axioms_ -> 'a1 Nmodule_isZmodule.axioms_ -> ('a1, 'a1)
      Zmodule_isComRing.phant_axioms -> SemiRing.coq_type

    val coq_HB_unnamed_factory_215 :
      'a1 Coq_isNmodule.axioms_ -> 'a1 Choice.Coq_hasChoice.axioms_ -> 'a1
      Coq_hasDecEq.axioms_ -> 'a1 Nmodule_isZmodule.axioms_ -> ('a1, 'a1)
      Zmodule_isComRing.phant_axioms -> 'a1 Ring_hasCommutativeMul.axioms_

    val coq_GRing_Zmodule_isComRing__to__GRing_SemiRing_hasCommutativeMul :
      'a1 Coq_isNmodule.axioms_ -> 'a1 Choice.Coq_hasChoice.axioms_ -> 'a1
      Coq_hasDecEq.axioms_ -> 'a1 Nmodule_isZmodule.axioms_ -> ('a1, 'a1)
      Zmodule_isComRing.phant_axioms -> 'a1 SemiRing_hasCommutativeMul.axioms_
   end

  module Ring_hasMulInverse :
   sig
    type 'r axioms_ = { unit_subdef : 'r pred; inv : ('r -> 'r) }

    val phant_Build :
      'a1 Coq_isNmodule.axioms_ -> 'a1 Choice.Coq_hasChoice.axioms_ -> 'a1
      Coq_hasDecEq.axioms_ -> 'a1 Nmodule_isSemiRing.axioms_ -> 'a1
      Nmodule_isZmodule.axioms_ -> SemiRing.coq_type -> 'a2 SemiRing.axioms_
      -> 'a2 Coq_isNmodule.axioms_ -> 'a2 Choice.Coq_hasChoice.axioms_ -> 'a2
      Coq_hasDecEq.axioms_ -> 'a2 Nmodule_isSemiRing.axioms_ ->
      Zmodule.coq_type -> 'a3 Zmodule.axioms_ -> 'a3 Coq_isNmodule.axioms_ ->
      'a3 Choice.Coq_hasChoice.axioms_ -> 'a3 Coq_hasDecEq.axioms_ -> 'a3
      Nmodule_isZmodule.axioms_ -> 'a1 pred -> ('a1 -> 'a1) -> 'a1 axioms_
   end

  module ComRing_hasMulInverse :
   sig
    type 'r axioms_ = { coq_unit : 'r pred_sort; inv : ('r -> 'r) }

    val coq_unit :
      'a1 Coq_isNmodule.axioms_ -> 'a1 Choice.Coq_hasChoice.axioms_ -> 'a1
      Coq_hasDecEq.axioms_ -> 'a1 Nmodule_isZmodule.axioms_ -> 'a1
      Nmodule_isSemiRing.axioms_ -> 'a1 SemiRing_hasCommutativeMul.axioms_ ->
      'a1 axioms_ -> 'a1 pred_sort

    val inv :
      'a1 Coq_isNmodule.axioms_ -> 'a1 Choice.Coq_hasChoice.axioms_ -> 'a1
      Coq_hasDecEq.axioms_ -> 'a1 Nmodule_isZmodule.axioms_ -> 'a1
      Nmodule_isSemiRing.axioms_ -> 'a1 SemiRing_hasCommutativeMul.axioms_ ->
      'a1 axioms_ -> 'a1 -> 'a1

    type ('r, 'vlocal, 'rlocal) phant_axioms = 'r axioms_
   end

  module Builders_256 :
   sig
    val coq_Builders_256_R__canonical__GRing_Zmodule :
      'a1 Coq_isNmodule.axioms_ -> 'a1 Choice.Coq_hasChoice.axioms_ -> 'a1
      Coq_hasDecEq.axioms_ -> 'a1 Nmodule_isZmodule.axioms_ ->
      Zmodule.coq_type

    val coq_Builders_256_R__canonical__GRing_SemiRing :
      'a1 Coq_isNmodule.axioms_ -> 'a1 Choice.Coq_hasChoice.axioms_ -> 'a1
      Coq_hasDecEq.axioms_ -> 'a1 Nmodule_isSemiRing.axioms_ ->
      SemiRing.coq_type

    val coq_HB_unnamed_factory_258 :
      'a1 Coq_isNmodule.axioms_ -> 'a1 Choice.Coq_hasChoice.axioms_ -> 'a1
      Coq_hasDecEq.axioms_ -> 'a1 Nmodule_isZmodule.axioms_ -> 'a1
      Nmodule_isSemiRing.axioms_ -> 'a1 SemiRing_hasCommutativeMul.axioms_ ->
      ('a1, 'a1, 'a1) ComRing_hasMulInverse.phant_axioms -> 'a1
      Ring_hasMulInverse.axioms_
   end

  module ComUnitRing_isIntegral :
   sig
    type 'r axioms_ =
    | Axioms_
   end

  module IntegralDomain :
   sig
    type 'r axioms_ = { coq_GRing_isNmodule_mixin : 'r Coq_isNmodule.axioms_;
                        choice_hasChoice_mixin : 'r
                                                 Choice.Coq_hasChoice.axioms_;
                        eqtype_hasDecEq_mixin : 'r Coq_hasDecEq.axioms_;
                        coq_GRing_Nmodule_isSemiRing_mixin : 'r
                                                             Nmodule_isSemiRing.axioms_;
                        coq_GRing_SemiRing_hasCommutativeMul_mixin : 
                        'r SemiRing_hasCommutativeMul.axioms_;
                        coq_GRing_Nmodule_isZmodule_mixin : 'r
                                                            Nmodule_isZmodule.axioms_;
                        coq_GRing_Ring_hasMulInverse_mixin : 'r
                                                             Ring_hasMulInverse.axioms_;
                        coq_GRing_ComUnitRing_isIntegral_mixin : 'r
                                                                 ComUnitRing_isIntegral.axioms_ }

    type coq_type =
      __ axioms_
      (* singleton inductive, whose constructor was Pack *)
   end
 end

open Datatypes
open Eqtype
open Ssrbool
open Ssrnat

type __ = Obj.t

module GRing =
 struct
  module Coq_isNmodule =
   struct
    type 'v axioms_ = { zero : 'v; add : ('v -> 'v -> 'v) }

    (** val zero : 'a1 axioms_ -> 'a1 **)

    let zero record =
      record.zero

    (** val add : 'a1 axioms_ -> 'a1 -> 'a1 -> 'a1 **)

    let add record =
      record.add

    (** val phant_Build : 'a1 -> ('a1 -> 'a1 -> 'a1) -> 'a1 axioms_ **)

    let phant_Build zero0 add0 =
      { zero = zero0; add = add0 }
   end

  module Nmodule =
   struct
    type 'v axioms_ = { coq_GRing_isNmodule_mixin : 'v Coq_isNmodule.axioms_;
                        choice_hasChoice_mixin : 'v
                                                 Choice.Coq_hasChoice.axioms_;
                        eqtype_hasDecEq_mixin : 'v Coq_hasDecEq.axioms_ }

    (** val coq_GRing_isNmodule_mixin :
        'a1 axioms_ -> 'a1 Coq_isNmodule.axioms_ **)

    let coq_GRing_isNmodule_mixin record =
      record.coq_GRing_isNmodule_mixin

    type coq_type =
      __ axioms_
      (* singleton inductive, whose constructor was Pack *)

    type sort = __

    (** val coq_class : coq_type -> sort axioms_ **)

    let coq_class record =
      record
   end

  (** val zero : Nmodule.coq_type -> Nmodule.sort **)

  let zero s =
    s.Nmodule.coq_GRing_isNmodule_mixin.Coq_isNmodule.zero

  (** val add :
      Nmodule.coq_type -> Nmodule.sort -> Nmodule.sort -> Nmodule.sort **)

  let add s x x0 =
    s.Nmodule.coq_GRing_isNmodule_mixin.Coq_isNmodule.add x x0

  (** val natmul : Nmodule.coq_type -> Nmodule.sort -> nat -> Nmodule.sort **)

  let natmul v x n =
    iterop n (add v) x (zero v)

  module Nmodule_isZmodule =
   struct
    type 'v axioms_ =
      'v -> 'v
      (* singleton inductive, whose constructor was Axioms_ *)

    (** val opp :
        'a1 Coq_isNmodule.axioms_ -> 'a1 Choice.Coq_hasChoice.axioms_ -> 'a1
        Coq_hasDecEq.axioms_ -> 'a1 axioms_ -> 'a1 -> 'a1 **)

    let opp _ _ _ record =
      record

    (** val phant_Build :
        'a1 Coq_isNmodule.axioms_ -> 'a1 Choice.Coq_hasChoice.axioms_ -> 'a1
        Coq_hasDecEq.axioms_ -> Nmodule.coq_type -> 'a2 Nmodule.axioms_ ->
        'a2 Coq_isNmodule.axioms_ -> 'a2 Choice.Coq_hasChoice.axioms_ -> 'a2
        Coq_hasDecEq.axioms_ -> Choice.Choice.coq_type -> 'a3
        Choice.Choice.axioms_ -> 'a3 Choice.Coq_hasChoice.axioms_ -> 'a3
        Coq_hasDecEq.axioms_ -> Equality.coq_type -> 'a4 Equality.axioms_ ->
        'a4 Coq_hasDecEq.axioms_ -> ('a1 -> 'a1) -> 'a1 axioms_ **)

    let phant_Build _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ opp0 =
      opp0
   end

  module Zmodule =
   struct
    type 'v axioms_ = { coq_GRing_isNmodule_mixin : 'v Coq_isNmodule.axioms_;
                        choice_hasChoice_mixin : 'v
                                                 Choice.Coq_hasChoice.axioms_;
                        eqtype_hasDecEq_mixin : 'v Coq_hasDecEq.axioms_;
                        coq_GRing_Nmodule_isZmodule_mixin : 'v
                                                            Nmodule_isZmodule.axioms_ }

    (** val coq_GRing_isNmodule_mixin :
        'a1 axioms_ -> 'a1 Coq_isNmodule.axioms_ **)

    let coq_GRing_isNmodule_mixin record =
      record.coq_GRing_isNmodule_mixin

    (** val choice_hasChoice_mixin :
        'a1 axioms_ -> 'a1 Choice.Coq_hasChoice.axioms_ **)

    let choice_hasChoice_mixin record =
      record.choice_hasChoice_mixin

    (** val eqtype_hasDecEq_mixin :
        'a1 axioms_ -> 'a1 Coq_hasDecEq.axioms_ **)

    let eqtype_hasDecEq_mixin record =
      record.eqtype_hasDecEq_mixin

    (** val coq_GRing_Nmodule_isZmodule_mixin :
        'a1 axioms_ -> 'a1 Nmodule_isZmodule.axioms_ **)

    let coq_GRing_Nmodule_isZmodule_mixin record =
      record.coq_GRing_Nmodule_isZmodule_mixin

    type coq_type =
      __ axioms_
      (* singleton inductive, whose constructor was Pack *)

    type sort = __

    (** val coq_class : coq_type -> sort axioms_ **)

    let coq_class record =
      record

    module Exports =
     struct
      (** val coq_GRing_Zmodule_class__to__GRing_Nmodule_class :
          'a1 axioms_ -> 'a1 Nmodule.axioms_ **)

      let coq_GRing_Zmodule_class__to__GRing_Nmodule_class c =
        { Nmodule.coq_GRing_isNmodule_mixin = c.coq_GRing_isNmodule_mixin;
          Nmodule.choice_hasChoice_mixin = c.choice_hasChoice_mixin;
          Nmodule.eqtype_hasDecEq_mixin = c.eqtype_hasDecEq_mixin }

      (** val coq_GRing_Zmodule__to__GRing_Nmodule :
          coq_type -> Nmodule.coq_type **)

      let coq_GRing_Zmodule__to__GRing_Nmodule =
        coq_GRing_Zmodule_class__to__GRing_Nmodule_class
     end
   end

  (** val opp : Zmodule.coq_type -> Zmodule.sort -> Zmodule.sort **)

  let opp s x =
    s.Zmodule.coq_GRing_Nmodule_isZmodule_mixin x

  module Coq_isZmodule =
   struct
    type 'v axioms_ = { zero : 'v; opp : ('v -> 'v); add : ('v -> 'v -> 'v) }

    (** val zero :
        'a1 Choice.Coq_hasChoice.axioms_ -> 'a1 Coq_hasDecEq.axioms_ -> 'a1
        axioms_ -> 'a1 **)

    let zero _ _ record =
      record.zero

    (** val opp :
        'a1 Choice.Coq_hasChoice.axioms_ -> 'a1 Coq_hasDecEq.axioms_ -> 'a1
        axioms_ -> 'a1 -> 'a1 **)

    let opp _ _ record =
      record.opp

    (** val add :
        'a1 Choice.Coq_hasChoice.axioms_ -> 'a1 Coq_hasDecEq.axioms_ -> 'a1
        axioms_ -> 'a1 -> 'a1 -> 'a1 **)

    let add _ _ record =
      record.add

    (** val phant_Build :
        'a1 Choice.Coq_hasChoice.axioms_ -> 'a1 Coq_hasDecEq.axioms_ ->
        Choice.Choice.coq_type -> 'a2 Choice.Choice.axioms_ -> 'a2
        Choice.Coq_hasChoice.axioms_ -> 'a2 Coq_hasDecEq.axioms_ ->
        Equality.coq_type -> 'a3 Equality.axioms_ -> 'a3 Coq_hasDecEq.axioms_
        -> 'a1 -> ('a1 -> 'a1) -> ('a1 -> 'a1 -> 'a1) -> 'a1 axioms_ **)

    let phant_Build _ _ _ _ _ _ _ _ _ zero0 opp0 add0 =
      { zero = zero0; opp = opp0; add = add0 }

    type ('v, 'tlocal, 'tlocal0) phant_axioms = 'v axioms_
   end

  module Builders_8 =
   struct
    (** val coq_Builders_8_V__canonical__eqtype_Equality :
        'a1 Coq_hasDecEq.axioms_ -> Equality.coq_type **)

    let coq_Builders_8_V__canonical__eqtype_Equality local_mixin_eqtype_hasDecEq =
      Obj.magic local_mixin_eqtype_hasDecEq

    (** val coq_Builders_8_V__canonical__choice_Choice :
        'a1 Choice.Coq_hasChoice.axioms_ -> 'a1 Coq_hasDecEq.axioms_ ->
        Choice.Choice.coq_type **)

    let coq_Builders_8_V__canonical__choice_Choice local_mixin_choice_hasChoice local_mixin_eqtype_hasDecEq =
      { Choice.Choice.choice_hasChoice_mixin =
        (Obj.magic local_mixin_choice_hasChoice);
        Choice.Choice.eqtype_hasDecEq_mixin =
        (Obj.magic local_mixin_eqtype_hasDecEq) }

    (** val coq_HB_unnamed_factory_10 :
        'a1 Choice.Coq_hasChoice.axioms_ -> 'a1 Coq_hasDecEq.axioms_ -> ('a1,
        'a1, 'a1) Coq_isZmodule.phant_axioms -> 'a1 Coq_isNmodule.axioms_ **)

    let coq_HB_unnamed_factory_10 _ _ fresh_name_9 =
      Coq_isNmodule.phant_Build fresh_name_9.Coq_isZmodule.zero
        fresh_name_9.Coq_isZmodule.add

    (** val coq_Builders_8_V__canonical__GRing_Nmodule :
        'a1 Choice.Coq_hasChoice.axioms_ -> 'a1 Coq_hasDecEq.axioms_ -> ('a1,
        'a1, 'a1) Coq_isZmodule.phant_axioms -> Nmodule.coq_type **)

    let coq_Builders_8_V__canonical__GRing_Nmodule local_mixin_choice_hasChoice local_mixin_eqtype_hasDecEq fresh_name_9 =
      { Nmodule.coq_GRing_isNmodule_mixin =
        (coq_HB_unnamed_factory_10 (Obj.magic local_mixin_choice_hasChoice)
          (Obj.magic local_mixin_eqtype_hasDecEq) (Obj.magic fresh_name_9));
        Nmodule.choice_hasChoice_mixin =
        (Obj.magic local_mixin_choice_hasChoice);
        Nmodule.eqtype_hasDecEq_mixin =
        (Obj.magic local_mixin_eqtype_hasDecEq) }

    (** val coq_HB_unnamed_factory_12 :
        'a1 Choice.Coq_hasChoice.axioms_ -> 'a1 Coq_hasDecEq.axioms_ -> ('a1,
        'a1, 'a1) Coq_isZmodule.phant_axioms -> 'a1 Nmodule_isZmodule.axioms_ **)

    let coq_HB_unnamed_factory_12 local_mixin_choice_hasChoice local_mixin_eqtype_hasDecEq fresh_name_9 =
      Nmodule_isZmodule.phant_Build
        (coq_HB_unnamed_factory_10 local_mixin_choice_hasChoice
          local_mixin_eqtype_hasDecEq fresh_name_9)
        local_mixin_choice_hasChoice local_mixin_eqtype_hasDecEq
        (coq_Builders_8_V__canonical__GRing_Nmodule
          local_mixin_choice_hasChoice local_mixin_eqtype_hasDecEq
          fresh_name_9) { Nmodule.coq_GRing_isNmodule_mixin =
        (coq_HB_unnamed_factory_10 local_mixin_choice_hasChoice
          local_mixin_eqtype_hasDecEq fresh_name_9);
        Nmodule.choice_hasChoice_mixin = local_mixin_choice_hasChoice;
        Nmodule.eqtype_hasDecEq_mixin = local_mixin_eqtype_hasDecEq }
        (coq_HB_unnamed_factory_10 local_mixin_choice_hasChoice
          local_mixin_eqtype_hasDecEq fresh_name_9)
        local_mixin_choice_hasChoice local_mixin_eqtype_hasDecEq
        (coq_Builders_8_V__canonical__choice_Choice
          local_mixin_choice_hasChoice local_mixin_eqtype_hasDecEq)
        { Choice.Choice.choice_hasChoice_mixin =
        local_mixin_choice_hasChoice; Choice.Choice.eqtype_hasDecEq_mixin =
        local_mixin_eqtype_hasDecEq } local_mixin_choice_hasChoice
        local_mixin_eqtype_hasDecEq
        (coq_Builders_8_V__canonical__eqtype_Equality
          local_mixin_eqtype_hasDecEq) local_mixin_eqtype_hasDecEq
        local_mixin_eqtype_hasDecEq fresh_name_9.Coq_isZmodule.opp
   end

  module Nmodule_isSemiRing =
   struct
    type 'r axioms_ = { one : 'r; mul : ('r -> 'r -> 'r) }

    (** val one :
        'a1 Coq_isNmodule.axioms_ -> 'a1 Choice.Coq_hasChoice.axioms_ -> 'a1
        Coq_hasDecEq.axioms_ -> 'a1 axioms_ -> 'a1 **)

    let one _ _ _ record =
      record.one

    (** val mul :
        'a1 Coq_isNmodule.axioms_ -> 'a1 Choice.Coq_hasChoice.axioms_ -> 'a1
        Coq_hasDecEq.axioms_ -> 'a1 axioms_ -> 'a1 -> 'a1 -> 'a1 **)

    let mul _ _ _ record =
      record.mul

    (** val phant_Build :
        'a1 Coq_isNmodule.axioms_ -> 'a1 Choice.Coq_hasChoice.axioms_ -> 'a1
        Coq_hasDecEq.axioms_ -> Nmodule.coq_type -> 'a2 Nmodule.axioms_ ->
        'a2 Coq_isNmodule.axioms_ -> 'a2 Choice.Coq_hasChoice.axioms_ -> 'a2
        Coq_hasDecEq.axioms_ -> Choice.Choice.coq_type -> 'a3
        Choice.Choice.axioms_ -> 'a3 Choice.Coq_hasChoice.axioms_ -> 'a3
        Coq_hasDecEq.axioms_ -> Equality.coq_type -> 'a4 Equality.axioms_ ->
        'a4 Coq_hasDecEq.axioms_ -> 'a1 -> ('a1 -> 'a1 -> 'a1) -> 'a1 axioms_ **)

    let phant_Build _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ one0 mul0 =
      { one = one0; mul = mul0 }
   end

  module SemiRing =
   struct
    type 'r axioms_ = { coq_GRing_isNmodule_mixin : 'r Coq_isNmodule.axioms_;
                        choice_hasChoice_mixin : 'r
                                                 Choice.Coq_hasChoice.axioms_;
                        eqtype_hasDecEq_mixin : 'r Coq_hasDecEq.axioms_;
                        coq_GRing_Nmodule_isSemiRing_mixin : 'r
                                                             Nmodule_isSemiRing.axioms_ }

    (** val coq_GRing_isNmodule_mixin :
        'a1 axioms_ -> 'a1 Coq_isNmodule.axioms_ **)

    let coq_GRing_isNmodule_mixin record =
      record.coq_GRing_isNmodule_mixin

    (** val choice_hasChoice_mixin :
        'a1 axioms_ -> 'a1 Choice.Coq_hasChoice.axioms_ **)

    let choice_hasChoice_mixin record =
      record.choice_hasChoice_mixin

    (** val eqtype_hasDecEq_mixin :
        'a1 axioms_ -> 'a1 Coq_hasDecEq.axioms_ **)

    let eqtype_hasDecEq_mixin record =
      record.eqtype_hasDecEq_mixin

    (** val coq_GRing_Nmodule_isSemiRing_mixin :
        'a1 axioms_ -> 'a1 Nmodule_isSemiRing.axioms_ **)

    let coq_GRing_Nmodule_isSemiRing_mixin record =
      record.coq_GRing_Nmodule_isSemiRing_mixin

    type coq_type =
      __ axioms_
      (* singleton inductive, whose constructor was Pack *)

    type sort = __

    (** val coq_class : coq_type -> sort axioms_ **)

    let coq_class record =
      record

    module Exports =
     struct
      (** val coq_GRing_SemiRing_class__to__eqtype_Equality_class :
          'a1 axioms_ -> 'a1 Equality.axioms_ **)

      let coq_GRing_SemiRing_class__to__eqtype_Equality_class c =
        c.eqtype_hasDecEq_mixin

      (** val coq_GRing_SemiRing__to__eqtype_Equality :
          coq_type -> Equality.coq_type **)

      let coq_GRing_SemiRing__to__eqtype_Equality =
        coq_GRing_SemiRing_class__to__eqtype_Equality_class

      (** val coq_GRing_SemiRing_class__to__GRing_Nmodule_class :
          'a1 axioms_ -> 'a1 Nmodule.axioms_ **)

      let coq_GRing_SemiRing_class__to__GRing_Nmodule_class c =
        { Nmodule.coq_GRing_isNmodule_mixin = c.coq_GRing_isNmodule_mixin;
          Nmodule.choice_hasChoice_mixin = c.choice_hasChoice_mixin;
          Nmodule.eqtype_hasDecEq_mixin = c.eqtype_hasDecEq_mixin }

      (** val coq_GRing_SemiRing__to__GRing_Nmodule :
          coq_type -> Nmodule.coq_type **)

      let coq_GRing_SemiRing__to__GRing_Nmodule =
        coq_GRing_SemiRing_class__to__GRing_Nmodule_class
     end
   end

  (** val one : SemiRing.coq_type -> SemiRing.sort **)

  let one s =
    s.SemiRing.coq_GRing_Nmodule_isSemiRing_mixin.Nmodule_isSemiRing.one

  (** val mul :
      SemiRing.coq_type -> SemiRing.sort -> SemiRing.sort -> SemiRing.sort **)

  let mul s x x0 =
    s.SemiRing.coq_GRing_Nmodule_isSemiRing_mixin.Nmodule_isSemiRing.mul x x0

  (** val exp : SemiRing.coq_type -> SemiRing.sort -> nat -> SemiRing.sort **)

  let exp r x n =
    iterop n (mul r) x (one r)

  module Ring =
   struct
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

    (** val coq_class : coq_type -> sort axioms_ **)

    let coq_class record =
      record

    module Exports =
     struct
      (** val coq_GRing_Ring_class__to__eqtype_Equality_class :
          'a1 axioms_ -> 'a1 Equality.axioms_ **)

      let coq_GRing_Ring_class__to__eqtype_Equality_class c =
        c.eqtype_hasDecEq_mixin

      (** val coq_GRing_Ring__to__eqtype_Equality :
          coq_type -> Equality.coq_type **)

      let coq_GRing_Ring__to__eqtype_Equality =
        coq_GRing_Ring_class__to__eqtype_Equality_class

      (** val coq_GRing_Ring_class__to__GRing_Zmodule_class :
          'a1 axioms_ -> 'a1 Zmodule.axioms_ **)

      let coq_GRing_Ring_class__to__GRing_Zmodule_class c =
        { Zmodule.coq_GRing_isNmodule_mixin = c.coq_GRing_isNmodule_mixin;
          Zmodule.choice_hasChoice_mixin = c.choice_hasChoice_mixin;
          Zmodule.eqtype_hasDecEq_mixin = c.eqtype_hasDecEq_mixin;
          Zmodule.coq_GRing_Nmodule_isZmodule_mixin =
          c.coq_GRing_Nmodule_isZmodule_mixin }

      (** val coq_GRing_Ring__to__GRing_Zmodule :
          coq_type -> Zmodule.coq_type **)

      let coq_GRing_Ring__to__GRing_Zmodule =
        coq_GRing_Ring_class__to__GRing_Zmodule_class

      (** val coq_GRing_Ring_class__to__GRing_SemiRing_class :
          'a1 axioms_ -> 'a1 SemiRing.axioms_ **)

      let coq_GRing_Ring_class__to__GRing_SemiRing_class c =
        { SemiRing.coq_GRing_isNmodule_mixin = c.coq_GRing_isNmodule_mixin;
          SemiRing.choice_hasChoice_mixin = c.choice_hasChoice_mixin;
          SemiRing.eqtype_hasDecEq_mixin = c.eqtype_hasDecEq_mixin;
          SemiRing.coq_GRing_Nmodule_isSemiRing_mixin =
          c.coq_GRing_Nmodule_isSemiRing_mixin }

      (** val coq_GRing_Ring__to__GRing_SemiRing :
          coq_type -> SemiRing.coq_type **)

      let coq_GRing_Ring__to__GRing_SemiRing =
        coq_GRing_Ring_class__to__GRing_SemiRing_class

      (** val join_GRing_Ring_between_GRing_SemiRing_and_GRing_Zmodule :
          coq_type -> Zmodule.coq_type **)

      let join_GRing_Ring_between_GRing_SemiRing_and_GRing_Zmodule =
        coq_GRing_Ring__to__GRing_Zmodule
     end
   end

  module Zmodule_isRing =
   struct
    type 'r axioms_ = { one : 'r; mul : ('r -> 'r -> 'r) }

    (** val one :
        'a1 Coq_isNmodule.axioms_ -> 'a1 Choice.Coq_hasChoice.axioms_ -> 'a1
        Coq_hasDecEq.axioms_ -> 'a1 Nmodule_isZmodule.axioms_ -> 'a1 axioms_
        -> 'a1 **)

    let one _ _ _ _ record =
      record.one

    (** val mul :
        'a1 Coq_isNmodule.axioms_ -> 'a1 Choice.Coq_hasChoice.axioms_ -> 'a1
        Coq_hasDecEq.axioms_ -> 'a1 Nmodule_isZmodule.axioms_ -> 'a1 axioms_
        -> 'a1 -> 'a1 -> 'a1 **)

    let mul _ _ _ _ record =
      record.mul

    (** val phant_Build :
        'a1 Coq_isNmodule.axioms_ -> 'a1 Choice.Coq_hasChoice.axioms_ -> 'a1
        Coq_hasDecEq.axioms_ -> 'a1 Nmodule_isZmodule.axioms_ ->
        Zmodule.coq_type -> 'a2 Zmodule.axioms_ -> 'a2 Coq_isNmodule.axioms_
        -> 'a2 Choice.Coq_hasChoice.axioms_ -> 'a2 Coq_hasDecEq.axioms_ ->
        'a2 Nmodule_isZmodule.axioms_ -> 'a1 -> ('a1 -> 'a1 -> 'a1) -> 'a1
        axioms_ **)

    let phant_Build _ _ _ _ _ _ _ _ _ _ one0 mul0 =
      { one = one0; mul = mul0 }

    type ('r, 'vlocal) phant_axioms = 'r axioms_
   end

  module Builders_31 =
   struct
    (** val coq_Builders_31_R__canonical__eqtype_Equality :
        'a1 Coq_hasDecEq.axioms_ -> Equality.coq_type **)

    let coq_Builders_31_R__canonical__eqtype_Equality local_mixin_eqtype_hasDecEq =
      Obj.magic local_mixin_eqtype_hasDecEq

    (** val coq_Builders_31_R__canonical__choice_Choice :
        'a1 Choice.Coq_hasChoice.axioms_ -> 'a1 Coq_hasDecEq.axioms_ ->
        Choice.Choice.coq_type **)

    let coq_Builders_31_R__canonical__choice_Choice local_mixin_choice_hasChoice local_mixin_eqtype_hasDecEq =
      { Choice.Choice.choice_hasChoice_mixin =
        (Obj.magic local_mixin_choice_hasChoice);
        Choice.Choice.eqtype_hasDecEq_mixin =
        (Obj.magic local_mixin_eqtype_hasDecEq) }

    (** val coq_Builders_31_R__canonical__GRing_Nmodule :
        'a1 Coq_isNmodule.axioms_ -> 'a1 Choice.Coq_hasChoice.axioms_ -> 'a1
        Coq_hasDecEq.axioms_ -> Nmodule.coq_type **)

    let coq_Builders_31_R__canonical__GRing_Nmodule local_mixin_GRing_isNmodule local_mixin_choice_hasChoice local_mixin_eqtype_hasDecEq =
      { Nmodule.coq_GRing_isNmodule_mixin =
        (Obj.magic local_mixin_GRing_isNmodule);
        Nmodule.choice_hasChoice_mixin =
        (Obj.magic local_mixin_choice_hasChoice);
        Nmodule.eqtype_hasDecEq_mixin =
        (Obj.magic local_mixin_eqtype_hasDecEq) }

    (** val coq_HB_unnamed_factory_33 :
        'a1 Coq_isNmodule.axioms_ -> 'a1 Choice.Coq_hasChoice.axioms_ -> 'a1
        Coq_hasDecEq.axioms_ -> 'a1 Nmodule_isZmodule.axioms_ -> ('a1, 'a1)
        Zmodule_isRing.phant_axioms -> 'a1 Nmodule_isSemiRing.axioms_ **)

    let coq_HB_unnamed_factory_33 local_mixin_GRing_isNmodule local_mixin_choice_hasChoice local_mixin_eqtype_hasDecEq _ fresh_name_32 =
      Nmodule_isSemiRing.phant_Build local_mixin_GRing_isNmodule
        local_mixin_choice_hasChoice local_mixin_eqtype_hasDecEq
        (coq_Builders_31_R__canonical__GRing_Nmodule
          local_mixin_GRing_isNmodule local_mixin_choice_hasChoice
          local_mixin_eqtype_hasDecEq) { Nmodule.coq_GRing_isNmodule_mixin =
        local_mixin_GRing_isNmodule; Nmodule.choice_hasChoice_mixin =
        local_mixin_choice_hasChoice; Nmodule.eqtype_hasDecEq_mixin =
        local_mixin_eqtype_hasDecEq } local_mixin_GRing_isNmodule
        local_mixin_choice_hasChoice local_mixin_eqtype_hasDecEq
        (coq_Builders_31_R__canonical__choice_Choice
          local_mixin_choice_hasChoice local_mixin_eqtype_hasDecEq)
        { Choice.Choice.choice_hasChoice_mixin =
        local_mixin_choice_hasChoice; Choice.Choice.eqtype_hasDecEq_mixin =
        local_mixin_eqtype_hasDecEq } local_mixin_choice_hasChoice
        local_mixin_eqtype_hasDecEq
        (coq_Builders_31_R__canonical__eqtype_Equality
          local_mixin_eqtype_hasDecEq) local_mixin_eqtype_hasDecEq
        local_mixin_eqtype_hasDecEq fresh_name_32.Zmodule_isRing.one
        fresh_name_32.Zmodule_isRing.mul
   end

  module SemiRing_hasCommutativeMul =
   struct
    type 'r axioms_ =
    | Axioms_

    (** val phant_Build :
        'a1 Coq_isNmodule.axioms_ -> 'a1 Choice.Coq_hasChoice.axioms_ -> 'a1
        Coq_hasDecEq.axioms_ -> 'a1 Nmodule_isSemiRing.axioms_ ->
        SemiRing.coq_type -> 'a2 SemiRing.axioms_ -> 'a2
        Coq_isNmodule.axioms_ -> 'a2 Choice.Coq_hasChoice.axioms_ -> 'a2
        Coq_hasDecEq.axioms_ -> 'a2 Nmodule_isSemiRing.axioms_ -> 'a1 axioms_ **)

    let phant_Build _ _ _ _ _ _ _ _ _ _ =
      Axioms_
   end

  module ComRing =
   struct
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

    (** val coq_class : coq_type -> sort axioms_ **)

    let coq_class record =
      record

    module Exports =
     struct
      (** val coq_GRing_ComRing_class__to__eqtype_Equality_class :
          'a1 axioms_ -> 'a1 Equality.axioms_ **)

      let coq_GRing_ComRing_class__to__eqtype_Equality_class c =
        c.eqtype_hasDecEq_mixin

      (** val coq_GRing_ComRing__to__eqtype_Equality :
          coq_type -> Equality.coq_type **)

      let coq_GRing_ComRing__to__eqtype_Equality =
        coq_GRing_ComRing_class__to__eqtype_Equality_class

      (** val coq_GRing_ComRing_class__to__GRing_Nmodule_class :
          'a1 axioms_ -> 'a1 Nmodule.axioms_ **)

      let coq_GRing_ComRing_class__to__GRing_Nmodule_class c =
        { Nmodule.coq_GRing_isNmodule_mixin = c.coq_GRing_isNmodule_mixin;
          Nmodule.choice_hasChoice_mixin = c.choice_hasChoice_mixin;
          Nmodule.eqtype_hasDecEq_mixin = c.eqtype_hasDecEq_mixin }

      (** val coq_GRing_ComRing__to__GRing_Nmodule :
          coq_type -> Nmodule.coq_type **)

      let coq_GRing_ComRing__to__GRing_Nmodule =
        coq_GRing_ComRing_class__to__GRing_Nmodule_class

      (** val coq_GRing_ComRing_class__to__GRing_Zmodule_class :
          'a1 axioms_ -> 'a1 Zmodule.axioms_ **)

      let coq_GRing_ComRing_class__to__GRing_Zmodule_class c =
        { Zmodule.coq_GRing_isNmodule_mixin = c.coq_GRing_isNmodule_mixin;
          Zmodule.choice_hasChoice_mixin = c.choice_hasChoice_mixin;
          Zmodule.eqtype_hasDecEq_mixin = c.eqtype_hasDecEq_mixin;
          Zmodule.coq_GRing_Nmodule_isZmodule_mixin =
          c.coq_GRing_Nmodule_isZmodule_mixin }

      (** val coq_GRing_ComRing__to__GRing_Zmodule :
          coq_type -> Zmodule.coq_type **)

      let coq_GRing_ComRing__to__GRing_Zmodule =
        coq_GRing_ComRing_class__to__GRing_Zmodule_class

      (** val coq_GRing_ComRing_class__to__GRing_SemiRing_class :
          'a1 axioms_ -> 'a1 SemiRing.axioms_ **)

      let coq_GRing_ComRing_class__to__GRing_SemiRing_class c =
        { SemiRing.coq_GRing_isNmodule_mixin = c.coq_GRing_isNmodule_mixin;
          SemiRing.choice_hasChoice_mixin = c.choice_hasChoice_mixin;
          SemiRing.eqtype_hasDecEq_mixin = c.eqtype_hasDecEq_mixin;
          SemiRing.coq_GRing_Nmodule_isSemiRing_mixin =
          c.coq_GRing_Nmodule_isSemiRing_mixin }

      (** val coq_GRing_ComRing__to__GRing_SemiRing :
          coq_type -> SemiRing.coq_type **)

      let coq_GRing_ComRing__to__GRing_SemiRing =
        coq_GRing_ComRing_class__to__GRing_SemiRing_class

      (** val coq_GRing_ComRing_class__to__GRing_Ring_class :
          'a1 axioms_ -> 'a1 Ring.axioms_ **)

      let coq_GRing_ComRing_class__to__GRing_Ring_class c =
        { Ring.coq_GRing_isNmodule_mixin = c.coq_GRing_isNmodule_mixin;
          Ring.choice_hasChoice_mixin = c.choice_hasChoice_mixin;
          Ring.eqtype_hasDecEq_mixin = c.eqtype_hasDecEq_mixin;
          Ring.coq_GRing_Nmodule_isSemiRing_mixin =
          c.coq_GRing_Nmodule_isSemiRing_mixin;
          Ring.coq_GRing_Nmodule_isZmodule_mixin =
          c.coq_GRing_Nmodule_isZmodule_mixin }

      (** val coq_GRing_ComRing__to__GRing_Ring :
          coq_type -> Ring.coq_type **)

      let coq_GRing_ComRing__to__GRing_Ring =
        coq_GRing_ComRing_class__to__GRing_Ring_class
     end
   end

  module Ring_hasCommutativeMul =
   struct
    type 'r axioms_ =
    | Axioms_

    (** val phant_Build :
        'a1 Coq_isNmodule.axioms_ -> 'a1 Choice.Coq_hasChoice.axioms_ -> 'a1
        Coq_hasDecEq.axioms_ -> 'a1 Nmodule_isSemiRing.axioms_ -> 'a1
        Nmodule_isZmodule.axioms_ -> SemiRing.coq_type -> 'a2
        SemiRing.axioms_ -> 'a2 Coq_isNmodule.axioms_ -> 'a2
        Choice.Coq_hasChoice.axioms_ -> 'a2 Coq_hasDecEq.axioms_ -> 'a2
        Nmodule_isSemiRing.axioms_ -> Zmodule.coq_type -> 'a3 Zmodule.axioms_
        -> 'a3 Coq_isNmodule.axioms_ -> 'a3 Choice.Coq_hasChoice.axioms_ ->
        'a3 Coq_hasDecEq.axioms_ -> 'a3 Nmodule_isZmodule.axioms_ -> 'a1
        axioms_ **)

    let phant_Build _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ =
      Axioms_

    type ('r, 'rlocal, 'vlocal) phant_axioms = 'r axioms_
   end

  module Builders_206 =
   struct
    (** val coq_Builders_206_R__canonical__GRing_SemiRing :
        'a1 Coq_isNmodule.axioms_ -> 'a1 Choice.Coq_hasChoice.axioms_ -> 'a1
        Coq_hasDecEq.axioms_ -> 'a1 Nmodule_isSemiRing.axioms_ ->
        SemiRing.coq_type **)

    let coq_Builders_206_R__canonical__GRing_SemiRing local_mixin_GRing_isNmodule local_mixin_choice_hasChoice local_mixin_eqtype_hasDecEq local_mixin_GRing_Nmodule_isSemiRing =
      { SemiRing.coq_GRing_isNmodule_mixin =
        (Obj.magic local_mixin_GRing_isNmodule);
        SemiRing.choice_hasChoice_mixin =
        (Obj.magic local_mixin_choice_hasChoice);
        SemiRing.eqtype_hasDecEq_mixin =
        (Obj.magic local_mixin_eqtype_hasDecEq);
        SemiRing.coq_GRing_Nmodule_isSemiRing_mixin =
        (Obj.magic local_mixin_GRing_Nmodule_isSemiRing) }

    (** val coq_HB_unnamed_factory_208 :
        'a1 Coq_isNmodule.axioms_ -> 'a1 Choice.Coq_hasChoice.axioms_ -> 'a1
        Coq_hasDecEq.axioms_ -> 'a1 Nmodule_isSemiRing.axioms_ -> 'a1
        Nmodule_isZmodule.axioms_ -> ('a1, 'a1, 'a1)
        Ring_hasCommutativeMul.phant_axioms -> 'a1
        SemiRing_hasCommutativeMul.axioms_ **)

    let coq_HB_unnamed_factory_208 local_mixin_GRing_isNmodule local_mixin_choice_hasChoice local_mixin_eqtype_hasDecEq local_mixin_GRing_Nmodule_isSemiRing _ _ =
      SemiRing_hasCommutativeMul.phant_Build local_mixin_GRing_isNmodule
        local_mixin_choice_hasChoice local_mixin_eqtype_hasDecEq
        local_mixin_GRing_Nmodule_isSemiRing
        (coq_Builders_206_R__canonical__GRing_SemiRing
          local_mixin_GRing_isNmodule local_mixin_choice_hasChoice
          local_mixin_eqtype_hasDecEq local_mixin_GRing_Nmodule_isSemiRing)
        { SemiRing.coq_GRing_isNmodule_mixin = local_mixin_GRing_isNmodule;
        SemiRing.choice_hasChoice_mixin = local_mixin_choice_hasChoice;
        SemiRing.eqtype_hasDecEq_mixin = local_mixin_eqtype_hasDecEq;
        SemiRing.coq_GRing_Nmodule_isSemiRing_mixin =
        local_mixin_GRing_Nmodule_isSemiRing } local_mixin_GRing_isNmodule
        local_mixin_choice_hasChoice local_mixin_eqtype_hasDecEq
        local_mixin_GRing_Nmodule_isSemiRing
   end

  module Zmodule_isComRing =
   struct
    type 'r axioms_ = { one : 'r; mul : ('r -> 'r -> 'r) }

    (** val one :
        'a1 Coq_isNmodule.axioms_ -> 'a1 Choice.Coq_hasChoice.axioms_ -> 'a1
        Coq_hasDecEq.axioms_ -> 'a1 Nmodule_isZmodule.axioms_ -> 'a1 axioms_
        -> 'a1 **)

    let one _ _ _ _ record =
      record.one

    (** val mul :
        'a1 Coq_isNmodule.axioms_ -> 'a1 Choice.Coq_hasChoice.axioms_ -> 'a1
        Coq_hasDecEq.axioms_ -> 'a1 Nmodule_isZmodule.axioms_ -> 'a1 axioms_
        -> 'a1 -> 'a1 -> 'a1 **)

    let mul _ _ _ _ record =
      record.mul

    type ('r, 'vlocal) phant_axioms = 'r axioms_
   end

  module Builders_211 =
   struct
    (** val coq_Builders_211_R__canonical__GRing_Zmodule :
        'a1 Coq_isNmodule.axioms_ -> 'a1 Choice.Coq_hasChoice.axioms_ -> 'a1
        Coq_hasDecEq.axioms_ -> 'a1 Nmodule_isZmodule.axioms_ ->
        Zmodule.coq_type **)

    let coq_Builders_211_R__canonical__GRing_Zmodule local_mixin_GRing_isNmodule local_mixin_choice_hasChoice local_mixin_eqtype_hasDecEq local_mixin_GRing_Nmodule_isZmodule =
      { Zmodule.coq_GRing_isNmodule_mixin =
        (Obj.magic local_mixin_GRing_isNmodule);
        Zmodule.choice_hasChoice_mixin =
        (Obj.magic local_mixin_choice_hasChoice);
        Zmodule.eqtype_hasDecEq_mixin =
        (Obj.magic local_mixin_eqtype_hasDecEq);
        Zmodule.coq_GRing_Nmodule_isZmodule_mixin =
        (Obj.magic local_mixin_GRing_Nmodule_isZmodule) }

    (** val coq_HB_unnamed_factory_213 :
        'a1 Coq_isNmodule.axioms_ -> 'a1 Choice.Coq_hasChoice.axioms_ -> 'a1
        Coq_hasDecEq.axioms_ -> 'a1 Nmodule_isZmodule.axioms_ -> ('a1, 'a1)
        Zmodule_isComRing.phant_axioms -> 'a1 Zmodule_isRing.axioms_ **)

    let coq_HB_unnamed_factory_213 local_mixin_GRing_isNmodule local_mixin_choice_hasChoice local_mixin_eqtype_hasDecEq local_mixin_GRing_Nmodule_isZmodule fresh_name_212 =
      Zmodule_isRing.phant_Build local_mixin_GRing_isNmodule
        local_mixin_choice_hasChoice local_mixin_eqtype_hasDecEq
        local_mixin_GRing_Nmodule_isZmodule
        (coq_Builders_211_R__canonical__GRing_Zmodule
          local_mixin_GRing_isNmodule local_mixin_choice_hasChoice
          local_mixin_eqtype_hasDecEq local_mixin_GRing_Nmodule_isZmodule)
        { Zmodule.coq_GRing_isNmodule_mixin = local_mixin_GRing_isNmodule;
        Zmodule.choice_hasChoice_mixin = local_mixin_choice_hasChoice;
        Zmodule.eqtype_hasDecEq_mixin = local_mixin_eqtype_hasDecEq;
        Zmodule.coq_GRing_Nmodule_isZmodule_mixin =
        local_mixin_GRing_Nmodule_isZmodule } local_mixin_GRing_isNmodule
        local_mixin_choice_hasChoice local_mixin_eqtype_hasDecEq
        local_mixin_GRing_Nmodule_isZmodule
        fresh_name_212.Zmodule_isComRing.one
        fresh_name_212.Zmodule_isComRing.mul

    (** val coq_GRing_Zmodule_isComRing__to__GRing_Nmodule_isSemiRing :
        'a1 Coq_isNmodule.axioms_ -> 'a1 Choice.Coq_hasChoice.axioms_ -> 'a1
        Coq_hasDecEq.axioms_ -> 'a1 Nmodule_isZmodule.axioms_ -> ('a1, 'a1)
        Zmodule_isComRing.phant_axioms -> 'a1 Nmodule_isSemiRing.axioms_ **)

    let coq_GRing_Zmodule_isComRing__to__GRing_Nmodule_isSemiRing local_mixin_GRing_isNmodule local_mixin_choice_hasChoice local_mixin_eqtype_hasDecEq local_mixin_GRing_Nmodule_isZmodule fresh_name_212 =
      Builders_31.coq_HB_unnamed_factory_33 local_mixin_GRing_isNmodule
        local_mixin_choice_hasChoice local_mixin_eqtype_hasDecEq
        local_mixin_GRing_Nmodule_isZmodule
        (coq_HB_unnamed_factory_213 local_mixin_GRing_isNmodule
          local_mixin_choice_hasChoice local_mixin_eqtype_hasDecEq
          local_mixin_GRing_Nmodule_isZmodule fresh_name_212)

    (** val coq_Builders_211_R__canonical__GRing_SemiRing :
        'a1 Coq_isNmodule.axioms_ -> 'a1 Choice.Coq_hasChoice.axioms_ -> 'a1
        Coq_hasDecEq.axioms_ -> 'a1 Nmodule_isZmodule.axioms_ -> ('a1, 'a1)
        Zmodule_isComRing.phant_axioms -> SemiRing.coq_type **)

    let coq_Builders_211_R__canonical__GRing_SemiRing local_mixin_GRing_isNmodule local_mixin_choice_hasChoice local_mixin_eqtype_hasDecEq local_mixin_GRing_Nmodule_isZmodule fresh_name_212 =
      { SemiRing.coq_GRing_isNmodule_mixin =
        (Obj.magic local_mixin_GRing_isNmodule);
        SemiRing.choice_hasChoice_mixin =
        (Obj.magic local_mixin_choice_hasChoice);
        SemiRing.eqtype_hasDecEq_mixin =
        (Obj.magic local_mixin_eqtype_hasDecEq);
        SemiRing.coq_GRing_Nmodule_isSemiRing_mixin =
        (coq_GRing_Zmodule_isComRing__to__GRing_Nmodule_isSemiRing
          (Obj.magic local_mixin_GRing_isNmodule)
          (Obj.magic local_mixin_choice_hasChoice)
          (Obj.magic local_mixin_eqtype_hasDecEq)
          (Obj.magic local_mixin_GRing_Nmodule_isZmodule)
          (Obj.magic fresh_name_212)) }

    (** val coq_HB_unnamed_factory_215 :
        'a1 Coq_isNmodule.axioms_ -> 'a1 Choice.Coq_hasChoice.axioms_ -> 'a1
        Coq_hasDecEq.axioms_ -> 'a1 Nmodule_isZmodule.axioms_ -> ('a1, 'a1)
        Zmodule_isComRing.phant_axioms -> 'a1 Ring_hasCommutativeMul.axioms_ **)

    let coq_HB_unnamed_factory_215 local_mixin_GRing_isNmodule local_mixin_choice_hasChoice local_mixin_eqtype_hasDecEq local_mixin_GRing_Nmodule_isZmodule fresh_name_212 =
      Ring_hasCommutativeMul.phant_Build local_mixin_GRing_isNmodule
        local_mixin_choice_hasChoice local_mixin_eqtype_hasDecEq
        (coq_GRing_Zmodule_isComRing__to__GRing_Nmodule_isSemiRing
          local_mixin_GRing_isNmodule local_mixin_choice_hasChoice
          local_mixin_eqtype_hasDecEq local_mixin_GRing_Nmodule_isZmodule
          fresh_name_212) local_mixin_GRing_Nmodule_isZmodule
        (coq_Builders_211_R__canonical__GRing_SemiRing
          local_mixin_GRing_isNmodule local_mixin_choice_hasChoice
          local_mixin_eqtype_hasDecEq local_mixin_GRing_Nmodule_isZmodule
          fresh_name_212) { SemiRing.coq_GRing_isNmodule_mixin =
        local_mixin_GRing_isNmodule; SemiRing.choice_hasChoice_mixin =
        local_mixin_choice_hasChoice; SemiRing.eqtype_hasDecEq_mixin =
        local_mixin_eqtype_hasDecEq;
        SemiRing.coq_GRing_Nmodule_isSemiRing_mixin =
        (coq_GRing_Zmodule_isComRing__to__GRing_Nmodule_isSemiRing
          local_mixin_GRing_isNmodule local_mixin_choice_hasChoice
          local_mixin_eqtype_hasDecEq local_mixin_GRing_Nmodule_isZmodule
          fresh_name_212) } local_mixin_GRing_isNmodule
        local_mixin_choice_hasChoice local_mixin_eqtype_hasDecEq
        (coq_GRing_Zmodule_isComRing__to__GRing_Nmodule_isSemiRing
          local_mixin_GRing_isNmodule local_mixin_choice_hasChoice
          local_mixin_eqtype_hasDecEq local_mixin_GRing_Nmodule_isZmodule
          fresh_name_212)
        (coq_Builders_211_R__canonical__GRing_Zmodule
          local_mixin_GRing_isNmodule local_mixin_choice_hasChoice
          local_mixin_eqtype_hasDecEq local_mixin_GRing_Nmodule_isZmodule)
        { Zmodule.coq_GRing_isNmodule_mixin = local_mixin_GRing_isNmodule;
        Zmodule.choice_hasChoice_mixin = local_mixin_choice_hasChoice;
        Zmodule.eqtype_hasDecEq_mixin = local_mixin_eqtype_hasDecEq;
        Zmodule.coq_GRing_Nmodule_isZmodule_mixin =
        local_mixin_GRing_Nmodule_isZmodule } local_mixin_GRing_isNmodule
        local_mixin_choice_hasChoice local_mixin_eqtype_hasDecEq
        local_mixin_GRing_Nmodule_isZmodule

    (** val coq_GRing_Zmodule_isComRing__to__GRing_SemiRing_hasCommutativeMul :
        'a1 Coq_isNmodule.axioms_ -> 'a1 Choice.Coq_hasChoice.axioms_ -> 'a1
        Coq_hasDecEq.axioms_ -> 'a1 Nmodule_isZmodule.axioms_ -> ('a1, 'a1)
        Zmodule_isComRing.phant_axioms -> 'a1
        SemiRing_hasCommutativeMul.axioms_ **)

    let coq_GRing_Zmodule_isComRing__to__GRing_SemiRing_hasCommutativeMul local_mixin_GRing_isNmodule local_mixin_choice_hasChoice local_mixin_eqtype_hasDecEq local_mixin_GRing_Nmodule_isZmodule fresh_name_212 =
      Builders_206.coq_HB_unnamed_factory_208 local_mixin_GRing_isNmodule
        local_mixin_choice_hasChoice local_mixin_eqtype_hasDecEq
        (coq_GRing_Zmodule_isComRing__to__GRing_Nmodule_isSemiRing
          local_mixin_GRing_isNmodule local_mixin_choice_hasChoice
          local_mixin_eqtype_hasDecEq local_mixin_GRing_Nmodule_isZmodule
          fresh_name_212) local_mixin_GRing_Nmodule_isZmodule
        (coq_HB_unnamed_factory_215 local_mixin_GRing_isNmodule
          local_mixin_choice_hasChoice local_mixin_eqtype_hasDecEq
          local_mixin_GRing_Nmodule_isZmodule fresh_name_212)
   end

  module Ring_hasMulInverse =
   struct
    type 'r axioms_ = { unit_subdef : 'r pred; inv : ('r -> 'r) }

    (** val phant_Build :
        'a1 Coq_isNmodule.axioms_ -> 'a1 Choice.Coq_hasChoice.axioms_ -> 'a1
        Coq_hasDecEq.axioms_ -> 'a1 Nmodule_isSemiRing.axioms_ -> 'a1
        Nmodule_isZmodule.axioms_ -> SemiRing.coq_type -> 'a2
        SemiRing.axioms_ -> 'a2 Coq_isNmodule.axioms_ -> 'a2
        Choice.Coq_hasChoice.axioms_ -> 'a2 Coq_hasDecEq.axioms_ -> 'a2
        Nmodule_isSemiRing.axioms_ -> Zmodule.coq_type -> 'a3 Zmodule.axioms_
        -> 'a3 Coq_isNmodule.axioms_ -> 'a3 Choice.Coq_hasChoice.axioms_ ->
        'a3 Coq_hasDecEq.axioms_ -> 'a3 Nmodule_isZmodule.axioms_ -> 'a1 pred
        -> ('a1 -> 'a1) -> 'a1 axioms_ **)

    let phant_Build _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ unit_subdef0 inv0 =
      { unit_subdef = unit_subdef0; inv = inv0 }
   end

  module ComRing_hasMulInverse =
   struct
    type 'r axioms_ = { coq_unit : 'r pred_sort; inv : ('r -> 'r) }

    (** val coq_unit :
        'a1 Coq_isNmodule.axioms_ -> 'a1 Choice.Coq_hasChoice.axioms_ -> 'a1
        Coq_hasDecEq.axioms_ -> 'a1 Nmodule_isZmodule.axioms_ -> 'a1
        Nmodule_isSemiRing.axioms_ -> 'a1 SemiRing_hasCommutativeMul.axioms_
        -> 'a1 axioms_ -> 'a1 pred_sort **)

    let coq_unit _ _ _ _ _ _ record =
      record.coq_unit

    (** val inv :
        'a1 Coq_isNmodule.axioms_ -> 'a1 Choice.Coq_hasChoice.axioms_ -> 'a1
        Coq_hasDecEq.axioms_ -> 'a1 Nmodule_isZmodule.axioms_ -> 'a1
        Nmodule_isSemiRing.axioms_ -> 'a1 SemiRing_hasCommutativeMul.axioms_
        -> 'a1 axioms_ -> 'a1 -> 'a1 **)

    let inv _ _ _ _ _ _ record =
      record.inv

    type ('r, 'vlocal, 'rlocal) phant_axioms = 'r axioms_
   end

  module Builders_256 =
   struct
    (** val coq_Builders_256_R__canonical__GRing_Zmodule :
        'a1 Coq_isNmodule.axioms_ -> 'a1 Choice.Coq_hasChoice.axioms_ -> 'a1
        Coq_hasDecEq.axioms_ -> 'a1 Nmodule_isZmodule.axioms_ ->
        Zmodule.coq_type **)

    let coq_Builders_256_R__canonical__GRing_Zmodule local_mixin_GRing_isNmodule local_mixin_choice_hasChoice local_mixin_eqtype_hasDecEq local_mixin_GRing_Nmodule_isZmodule =
      { Zmodule.coq_GRing_isNmodule_mixin =
        (Obj.magic local_mixin_GRing_isNmodule);
        Zmodule.choice_hasChoice_mixin =
        (Obj.magic local_mixin_choice_hasChoice);
        Zmodule.eqtype_hasDecEq_mixin =
        (Obj.magic local_mixin_eqtype_hasDecEq);
        Zmodule.coq_GRing_Nmodule_isZmodule_mixin =
        (Obj.magic local_mixin_GRing_Nmodule_isZmodule) }

    (** val coq_Builders_256_R__canonical__GRing_SemiRing :
        'a1 Coq_isNmodule.axioms_ -> 'a1 Choice.Coq_hasChoice.axioms_ -> 'a1
        Coq_hasDecEq.axioms_ -> 'a1 Nmodule_isSemiRing.axioms_ ->
        SemiRing.coq_type **)

    let coq_Builders_256_R__canonical__GRing_SemiRing local_mixin_GRing_isNmodule local_mixin_choice_hasChoice local_mixin_eqtype_hasDecEq local_mixin_GRing_Nmodule_isSemiRing =
      { SemiRing.coq_GRing_isNmodule_mixin =
        (Obj.magic local_mixin_GRing_isNmodule);
        SemiRing.choice_hasChoice_mixin =
        (Obj.magic local_mixin_choice_hasChoice);
        SemiRing.eqtype_hasDecEq_mixin =
        (Obj.magic local_mixin_eqtype_hasDecEq);
        SemiRing.coq_GRing_Nmodule_isSemiRing_mixin =
        (Obj.magic local_mixin_GRing_Nmodule_isSemiRing) }

    (** val coq_HB_unnamed_factory_258 :
        'a1 Coq_isNmodule.axioms_ -> 'a1 Choice.Coq_hasChoice.axioms_ -> 'a1
        Coq_hasDecEq.axioms_ -> 'a1 Nmodule_isZmodule.axioms_ -> 'a1
        Nmodule_isSemiRing.axioms_ -> 'a1 SemiRing_hasCommutativeMul.axioms_
        -> ('a1, 'a1, 'a1) ComRing_hasMulInverse.phant_axioms -> 'a1
        Ring_hasMulInverse.axioms_ **)

    let coq_HB_unnamed_factory_258 local_mixin_GRing_isNmodule local_mixin_choice_hasChoice local_mixin_eqtype_hasDecEq local_mixin_GRing_Nmodule_isZmodule local_mixin_GRing_Nmodule_isSemiRing _ fresh_name_257 =
      Ring_hasMulInverse.phant_Build local_mixin_GRing_isNmodule
        local_mixin_choice_hasChoice local_mixin_eqtype_hasDecEq
        local_mixin_GRing_Nmodule_isSemiRing
        local_mixin_GRing_Nmodule_isZmodule
        (coq_Builders_256_R__canonical__GRing_SemiRing
          local_mixin_GRing_isNmodule local_mixin_choice_hasChoice
          local_mixin_eqtype_hasDecEq local_mixin_GRing_Nmodule_isSemiRing)
        { SemiRing.coq_GRing_isNmodule_mixin = local_mixin_GRing_isNmodule;
        SemiRing.choice_hasChoice_mixin = local_mixin_choice_hasChoice;
        SemiRing.eqtype_hasDecEq_mixin = local_mixin_eqtype_hasDecEq;
        SemiRing.coq_GRing_Nmodule_isSemiRing_mixin =
        local_mixin_GRing_Nmodule_isSemiRing } local_mixin_GRing_isNmodule
        local_mixin_choice_hasChoice local_mixin_eqtype_hasDecEq
        local_mixin_GRing_Nmodule_isSemiRing
        (coq_Builders_256_R__canonical__GRing_Zmodule
          local_mixin_GRing_isNmodule local_mixin_choice_hasChoice
          local_mixin_eqtype_hasDecEq local_mixin_GRing_Nmodule_isZmodule)
        { Zmodule.coq_GRing_isNmodule_mixin = local_mixin_GRing_isNmodule;
        Zmodule.choice_hasChoice_mixin = local_mixin_choice_hasChoice;
        Zmodule.eqtype_hasDecEq_mixin = local_mixin_eqtype_hasDecEq;
        Zmodule.coq_GRing_Nmodule_isZmodule_mixin =
        local_mixin_GRing_Nmodule_isZmodule } local_mixin_GRing_isNmodule
        local_mixin_choice_hasChoice local_mixin_eqtype_hasDecEq
        local_mixin_GRing_Nmodule_isZmodule
        (Obj.magic fresh_name_257.ComRing_hasMulInverse.coq_unit)
        fresh_name_257.ComRing_hasMulInverse.inv
   end

  module ComUnitRing_isIntegral =
   struct
    type 'r axioms_ =
    | Axioms_
   end

  module IntegralDomain =
   struct
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

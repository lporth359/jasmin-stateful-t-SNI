open Eqtype
open Ssrbool

type __ = Obj.t

module Order =
 struct
  module Coq_isPOrder =
   struct
    type 't axioms_ = { le : 't rel; lt : 't rel }

    (** val le :
        unit -> 'a1 Coq_hasDecEq.axioms_ -> 'a1 axioms_ -> 'a1 rel **)

    let le _ _ record =
      record.le

    (** val lt :
        unit -> 'a1 Coq_hasDecEq.axioms_ -> 'a1 axioms_ -> 'a1 rel **)

    let lt _ _ record =
      record.lt

    (** val phant_Build :
        unit -> 'a1 Coq_hasDecEq.axioms_ -> Equality.coq_type -> 'a2
        Equality.axioms_ -> 'a2 Coq_hasDecEq.axioms_ -> 'a1 rel -> 'a1 rel ->
        'a1 axioms_ **)

    let phant_Build _ _ _ _ _ le0 lt0 =
      { le = le0; lt = lt0 }
   end

  module POrder =
   struct
    type 't axioms_ = { choice_hasChoice_mixin : 't
                                                 Choice.Coq_hasChoice.axioms_;
                        eqtype_hasDecEq_mixin : 't Coq_hasDecEq.axioms_;
                        coq_Order_isPOrder_mixin : 't Coq_isPOrder.axioms_ }

    (** val eqtype_hasDecEq_mixin :
        unit -> 'a1 axioms_ -> 'a1 Coq_hasDecEq.axioms_ **)

    let eqtype_hasDecEq_mixin _ record =
      record.eqtype_hasDecEq_mixin

    (** val coq_Order_isPOrder_mixin :
        unit -> 'a1 axioms_ -> 'a1 Coq_isPOrder.axioms_ **)

    let coq_Order_isPOrder_mixin _ record =
      record.coq_Order_isPOrder_mixin

    type coq_type =
      __ axioms_
      (* singleton inductive, whose constructor was Pack *)

    type sort = __

    (** val coq_class : unit -> coq_type -> sort axioms_ **)

    let coq_class _ record =
      record
   end

  (** val le : unit -> POrder.coq_type -> POrder.sort rel **)

  let le _ s =
    s.POrder.coq_Order_isPOrder_mixin.Coq_isPOrder.le

  (** val lt : unit -> POrder.coq_type -> POrder.sort rel **)

  let lt _ s =
    s.POrder.coq_Order_isPOrder_mixin.Coq_isPOrder.lt

  module LtLe_isPOrder =
   struct
    type 't axioms_ = { le : 't rel; lt : 't rel }

    (** val le :
        unit -> 'a1 Coq_hasDecEq.axioms_ -> 'a1 axioms_ -> 'a1 rel **)

    let le _ _ record =
      record.le

    (** val lt :
        unit -> 'a1 Coq_hasDecEq.axioms_ -> 'a1 axioms_ -> 'a1 rel **)

    let lt _ _ record =
      record.lt

    (** val phant_Build :
        unit -> 'a1 Coq_hasDecEq.axioms_ -> Equality.coq_type -> 'a2
        Equality.axioms_ -> 'a2 Coq_hasDecEq.axioms_ -> 'a1 rel -> 'a1 rel ->
        'a1 axioms_ **)

    let phant_Build _ _ _ _ _ le0 lt0 =
      { le = le0; lt = lt0 }

    type ('t, 'tlocal) phant_axioms = 't axioms_
   end

  module Builders_6 =
   struct
    (** val coq_Builders_6_T__canonical__eqtype_Equality :
        'a1 Coq_hasDecEq.axioms_ -> Equality.coq_type **)

    let coq_Builders_6_T__canonical__eqtype_Equality local_mixin_eqtype_hasDecEq =
      Obj.magic local_mixin_eqtype_hasDecEq

    (** val coq_HB_unnamed_factory_8 :
        unit -> 'a1 Coq_hasDecEq.axioms_ -> ('a1, 'a1)
        LtLe_isPOrder.phant_axioms -> 'a1 Coq_isPOrder.axioms_ **)

    let coq_HB_unnamed_factory_8 d local_mixin_eqtype_hasDecEq fresh_name_7 =
      Coq_isPOrder.phant_Build d local_mixin_eqtype_hasDecEq
        (coq_Builders_6_T__canonical__eqtype_Equality
          local_mixin_eqtype_hasDecEq) local_mixin_eqtype_hasDecEq
        local_mixin_eqtype_hasDecEq fresh_name_7.LtLe_isPOrder.le
        fresh_name_7.LtLe_isPOrder.lt
   end
 end

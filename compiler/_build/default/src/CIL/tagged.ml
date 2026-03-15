open Bool
open Datatypes
open MSetDecide
open MSetEqProperties
open Prelude
open PrimInt63
open Eqtype
open Gen_map
open Ssrbool

type __ = Obj.t
let __ = let rec f _ = Obj.repr f in Obj.repr f

module type TaggedCore =
 sig
  type t

  val tag : t -> Uint63.t
 end

module Tagged =
 functor (C:TaggedCore) ->
 struct
  type t = C.t

  (** val tag : t -> Uint63.t **)

  let tag =
    C.tag

  (** val t_eqb : t -> t -> bool **)

  let t_eqb x y =
    PrimInt63.eqb (tag x) (tag y)

  (** val t_eq_axiom : t eq_axiom **)

  let t_eq_axiom x y =
    equivP (t_eqb x y) (iff_reflect (t_eqb x y))

  (** val coq_HB_unnamed_factory_1 : t Coq_hasDecEq.axioms_ **)

  let coq_HB_unnamed_factory_1 =
    { Coq_hasDecEq.eq_op = t_eqb; Coq_hasDecEq.eqP = t_eq_axiom }

  (** val coq_Tagged_t__canonical__eqtype_Equality : Equality.coq_type **)

  let coq_Tagged_t__canonical__eqtype_Equality =
    Obj.magic coq_HB_unnamed_factory_1

  (** val t_eqType : Equality.coq_type **)

  let t_eqType =
    reverse_coercion coq_Tagged_t__canonical__eqtype_Equality __

  (** val cmp : t -> t -> comparison **)

  let cmp x y =
    compares (tag x) (tag y)

  module CmpT =
   struct
    (** val t : Equality.coq_type **)

    let t =
      reverse_coercion coq_Tagged_t__canonical__eqtype_Equality __

    (** val cmp : Equality.sort -> Equality.sort -> comparison **)

    let cmp =
      Obj.magic cmp
   end

  module Mt = Mmake(CmpT)

  module St = Smake(CmpT)

  module StP = EqProperties(St)

  module StD = WDecide(St)
 end

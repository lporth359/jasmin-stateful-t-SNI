open Bool
open Datatypes
open PrimInt63
open Eqtype
open Gen_map
open Wsize

type __ = Obj.t
let __ = let rec f _ = Obj.repr f in Obj.repr f

module type CORE_IDENT =
 sig
  type t

  val tag : t -> Uint63.t

  val id_name : t -> string

  val id_kind : t -> v_kind

  val spill_to_mmx : t -> bool
 end

module Cident =
 struct
  type t = CoreIdent.Cident.t

  (** val tag : t -> Uint63.t **)

  let tag = CoreIdent.Cident.tag

  (** val tagI : __ **)

  let tagI =
    __

  (** val id_name : t -> string **)

  let id_name = CoreIdent.Cident.id_name

  (** val id_kind : t -> v_kind **)

  let id_kind = CoreIdent.Cident.id_kind

  (** val spill_to_mmx : t -> bool **)

  let spill_to_mmx = CoreIdent.Cident.spill_to_mmx
 end

module Tident = Tagged.Tagged(Cident)

(** val ident_eqType : Equality.coq_type **)

let ident_eqType =
  { Coq_hasDecEq.eq_op = (fun x y ->
    PrimInt63.eqb (Cident.tag (Obj.magic x)) (Cident.tag (Obj.magic y)));
    Coq_hasDecEq.eqP = (Obj.magic Tident.t_eq_axiom) }

module WrapIdent =
 struct
  type t = CoreIdent.Cident.t
 end

module type IDENT =
 sig
  type ident = WrapIdent.t

  module Mid :
   MAP
 end

module Ident =
 struct
  type ident = WrapIdent.t

  (** val id_name : ident -> string **)

  let id_name =
    Cident.id_name

  (** val id_kind : ident -> v_kind **)

  let id_kind =
    Cident.id_kind

  module Mid = Tident.Mt

  (** val spill_to_mmx : ident -> bool **)

  let spill_to_mmx =
    Cident.spill_to_mmx
 end

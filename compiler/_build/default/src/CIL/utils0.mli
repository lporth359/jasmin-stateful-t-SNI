open BinInt
open BinNums
open BinPos
open Bool
open CMorphisms
open CRelationClasses
open Datatypes
open Prelude
open EqbOK
open Eqb_core_defs
open Eqtype
open Finfun
open Fintype
open Seq
open Ssrbool
open Ssreflect
open Ssrfun
open Ssrnat

type __ = Obj.t

module FinIsCount :
 sig
  val pickle :
    Equality.coq_type -> Equality.sort list -> Equality.sort -> nat

  val unpickle :
    Equality.coq_type -> Equality.sort list -> nat -> Equality.sort option
 end

type 't eqTypeC = { beq : ('t -> 't -> bool); ceqP : 't eq_axiom }

val beq : 'a1 eqTypeC -> 'a1 -> 'a1 -> bool

val ceqP : 'a1 eqTypeC -> 'a1 eq_axiom

module EqType :
 sig
  val coq_HB_unnamed_factory_1 : 'a1 eqTypeC -> 'a1 Coq_hasDecEq.axioms_

  val coq_EqType_T__canonical__eqtype_Equality :
    'a1 eqTypeC -> Equality.coq_type

  val ceqT_eqType : 'a1 eqTypeC -> Equality.coq_type
 end

val ceqT_eqType : 'a1 eqTypeC -> Equality.coq_type

type 't finTypeC = { _eqC : 't eqTypeC; cenum : 't list }

val _eqC : 'a1 finTypeC -> 'a1 eqTypeC

val cenum : 'a1 finTypeC -> 'a1 list

module FinType :
 sig
  val coq_HB_unnamed_factory_3 : 'a1 finTypeC -> 'a1 Equality.axioms_

  val eqtype_Equality__to__eqtype_hasDecEq :
    'a1 finTypeC -> 'a1 Coq_hasDecEq.axioms_

  val coq_HB_unnamed_mixin_5 : 'a1 finTypeC -> 'a1 Coq_hasDecEq.axioms_

  val coq_FinType_T__canonical__eqtype_Equality :
    'a1 finTypeC -> Equality.coq_type

  val coq_HB_unnamed_factory_6 :
    'a1 finTypeC -> 'a1 Choice.Coq_isCountable.phant_axioms

  val choice_isCountable__to__choice_Choice_isCountable :
    'a1 finTypeC -> 'a1 Choice.Choice_isCountable.axioms_

  val choice_isCountable__to__choice_hasChoice :
    'a1 finTypeC -> 'a1 Choice.Coq_hasChoice.phant_axioms

  val coq_HB_unnamed_mixin_10 :
    'a1 finTypeC -> 'a1 Choice.Coq_hasChoice.phant_axioms

  val coq_FinType_T__canonical__choice_Choice :
    'a1 finTypeC -> Choice.Choice.coq_type

  val coq_HB_unnamed_mixin_11 :
    'a1 finTypeC -> 'a1 Choice.Choice_isCountable.axioms_

  val coq_FinType_T__canonical__choice_Countable :
    'a1 finTypeC -> Choice.Countable.coq_type

  val coq_HB_unnamed_factory_12 : 'a1 finTypeC -> 'a1 Coq_isFinite.axioms_

  val coq_FinType_T__canonical__fintype_Finite :
    'a1 finTypeC -> Finite.coq_type

  val cfinT_finType : 'a1 finTypeC -> Finite.coq_type
 end

val cfinT_finType : 'a1 finTypeC -> Finite.coq_type

module FinMap :
 sig
  type ('t, 'u) map = 'u finfun_of

  val of_fun : 'a1 finTypeC -> (Finite.sort -> 'a2) -> 'a2 finfun_of

  val set : 'a1 finTypeC -> ('a1, 'a2) map -> 'a1 -> 'a2 -> ('a1, 'a2) map
 end

val reflect_inj :
  Equality.coq_type -> (Equality.sort -> 'a1) -> Equality.sort ->
  Equality.sort -> reflect -> reflect

type ('e, 'a) result =
| Ok of 'a
| Error of 'e

val is_ok : ('a1, 'a2) result -> bool

val is_okP : ('a1, 'a2) result -> reflect

module Result :
 sig
  val apply : ('a2 -> 'a3) -> 'a3 -> ('a1, 'a2) result -> 'a3

  val bind :
    ('a2 -> ('a1, 'a3) result) -> ('a1, 'a2) result -> ('a1, 'a3) result

  val map : ('a2 -> 'a3) -> ('a1, 'a2) result -> ('a1, 'a3) result

  val default : 'a2 -> ('a1, 'a2) result -> 'a2

  val map_err : ('a1 -> 'a2) -> ('a1, 'a3) result -> ('a2, 'a3) result
 end

val o2r : 'a1 -> 'a2 option -> ('a1, 'a2) result

val coq_assert : bool -> 'a1 -> ('a1, unit) result

type error =
| ErrOob
| ErrAddrUndef
| ErrAddrInvalid
| ErrStack
| ErrType
| ErrArith
| ErrSemUndef

type 't exec = (error, 't) result

val type_error : (error, 'a1) result

val undef_error : (error, 'a1) result

val rbindP :
  ('a1, 'a2) result -> ('a2 -> ('a1, 'a3) result) -> 'a3 -> ('a2 -> __ -> __
  -> 'a4) -> 'a4

val mapM : ('a2 -> ('a1, 'a3) result) -> 'a2 list -> ('a1, 'a3 list) result

val mapMP :
  Equality.coq_type -> Equality.coq_type -> (Equality.sort -> ('a1,
  Equality.sort) result) -> Equality.sort list -> Equality.sort list ->
  Equality.sort -> reflect

val foldM :
  ('a2 -> 'a3 -> ('a1, 'a3) result) -> 'a3 -> 'a2 list -> ('a1, 'a3) result

val foldrM :
  ('a2 -> 'a3 -> ('a1, 'a3) result) -> 'a3 -> 'a2 list -> ('a1, 'a3) result

val fold2 :
  'a3 -> ('a1 -> 'a2 -> 'a4 -> ('a3, 'a4) result) -> 'a1 list -> 'a2 list ->
  'a4 -> ('a3, 'a4) result

val allM : ('a1 -> ('a2, unit) result) -> 'a1 list -> ('a2, unit) result

val mapM2 :
  'a3 -> ('a1 -> 'a2 -> ('a3, 'a4) result) -> 'a1 list -> 'a2 list -> ('a3,
  'a4 list) result

val fmap : ('a1 -> 'a2 -> 'a1 * 'a3) -> 'a1 -> 'a2 list -> 'a1 * 'a3 list

val fmapM :
  ('a2 -> 'a3 -> ('a1, 'a2 * 'a4) result) -> 'a2 -> 'a3 list -> ('a1,
  'a2 * 'a4 list) result

val fmapM2 :
  'a1 -> ('a2 -> 'a3 -> 'a4 -> ('a1, 'a2 * 'a5) result) -> 'a2 -> 'a3 list ->
  'a4 list -> ('a1, 'a2 * 'a5 list) result

val all2P : ('a1 -> 'a2 -> bool) -> 'a1 list -> 'a2 list -> reflect

val reflect_all2_eqb :
  ('a1 -> 'a1 -> bool) -> ('a1 -> 'a1 -> reflect) -> 'a1 list -> 'a1 list ->
  reflect

val map2 : ('a1 -> 'a2 -> 'a3) -> 'a1 list -> 'a2 list -> 'a3 list

val map3 :
  ('a1 -> 'a2 -> 'a3 -> 'a4) -> 'a1 list -> 'a2 list -> 'a3 list -> 'a4 list

val mapi_aux : (nat -> 'a1 -> 'a2) -> nat -> 'a1 list -> 'a2 list

val mapi : (nat -> 'a1 -> 'a2) -> 'a1 list -> 'a2 list

val find_map : ('a1 -> 'a2 option) -> 'a1 list -> 'a2 option

val isSome_obind : ('a1 -> 'a2 option) -> 'a1 option -> reflect

val list_to_rev : nat -> nat list

val list_to : nat -> nat list

val conc_map : ('a1 -> 'a2 list) -> 'a1 list -> 'a2 list

val ctrans : comparison -> comparison -> comparison option

val coq_HB_unnamed_factory_14 : comparison Coq_hasDecEq.axioms_

val coq_Datatypes_comparison__canonical__eqtype_Equality : Equality.coq_type

val gcmp : ('a1 -> 'a1 -> comparison) -> 'a1 -> 'a1 -> comparison

val cmp_lt : ('a1 -> 'a1 -> comparison) -> 'a1 -> 'a1 -> bool

val cmp_le : ('a1 -> 'a1 -> comparison) -> 'a1 -> 'a1 -> bool

val lex :
  ('a1 -> 'a1 -> comparison) -> ('a2 -> 'a2 -> comparison) -> ('a1 * 'a2) ->
  ('a1 * 'a2) -> comparison

val cmp_min : ('a1 -> 'a1 -> comparison) -> 'a1 -> 'a1 -> 'a1

val cmp_max : ('a1 -> 'a1 -> comparison) -> 'a1 -> 'a1 -> 'a1

val bool_cmp : bool -> bool -> comparison

val subrelation_iff_flip_arrow : (__, __) iffT -> (__, __) arrow

val reflect_m : bool -> bool -> (__, __) iffT

val coq_P_leP : positive -> positive -> reflect

val coq_P_ltP : positive -> positive -> reflect

type is_positive =
| Coq_is_xI of positive * is_positive
| Coq_is_xO of positive * is_positive
| Coq_is_xH

val is_positive_rect :
  (positive -> is_positive -> 'a1 -> 'a1) -> (positive -> is_positive -> 'a1
  -> 'a1) -> 'a1 -> positive -> is_positive -> 'a1

val is_positive_rec :
  (positive -> is_positive -> 'a1 -> 'a1) -> (positive -> is_positive -> 'a1
  -> 'a1) -> 'a1 -> positive -> is_positive -> 'a1

val positive_tag : positive -> positive

val is_positive_inhab : positive -> is_positive

val is_positive_functor : positive -> is_positive -> is_positive

type box_positive_xI =
  positive
  (* singleton inductive, whose constructor was Box_positive_xI *)

val coq_Box_positive_xI_0 : box_positive_xI -> positive

type box_positive_xH =
| Box_positive_xH

type positive_fields_t = __

val positive_fields : positive -> positive_fields_t

val positive_construct : positive -> positive_fields_t -> positive option

val positive_induction :
  (positive -> 'a1 -> 'a1) -> (positive -> 'a1 -> 'a1) -> 'a1 -> positive ->
  is_positive -> 'a1

val positive_eqb_fields :
  (positive -> positive -> bool) -> positive -> positive_fields_t ->
  positive_fields_t -> bool

val positive_eqb : positive -> positive -> bool

val positive_eqb_OK : positive -> positive -> reflect

val positive_eqb_OK_sumbool : positive -> positive -> bool

val coq_HB_unnamed_factory_16 : positive Coq_hasDecEq.axioms_

val coq_BinNums_positive__canonical__eqtype_Equality : Equality.coq_type

val coq_ZleP : coq_Z -> coq_Z -> reflect

val coq_ZltP : coq_Z -> coq_Z -> reflect

val coq_ZNleP : nat -> nat -> reflect

val coq_ZNltP : nat -> nat -> reflect

val ziota_rec : coq_Z -> coq_Z -> coq_Z list

val ziota : coq_Z -> coq_Z -> coq_Z list

val pnth : 'a1 -> 'a1 list -> positive -> 'a1

val znth : 'a1 -> 'a1 list -> coq_Z -> 'a1

val zindex : Equality.coq_type -> Equality.sort -> Equality.sort list -> coq_Z

type 'tr lprod = __

type ltuple = __

val merge_tuple : __ list -> __ list -> ltuple -> ltuple -> ltuple

module Option :
 sig
 end

val obindP :
  'a1 option -> ('a1 -> 'a2 option) -> 'a2 -> ('a1 -> __ -> __ -> 'a3) -> 'a3

val oassert : bool -> unit option

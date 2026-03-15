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
let __ = let rec f _ = Obj.repr f in Obj.repr f

module FinIsCount =
 struct
  (** val pickle :
      Equality.coq_type -> Equality.sort list -> Equality.sort -> nat **)

  let pickle t enum x =
    index t x enum

  (** val unpickle :
      Equality.coq_type -> Equality.sort list -> nat -> Equality.sort option **)

  let unpickle _ enum n =
    nth None (map (fun x -> Some x) enum) n
 end

type 't eqTypeC = { beq : ('t -> 't -> bool); ceqP : 't eq_axiom }

(** val beq : 'a1 eqTypeC -> 'a1 -> 'a1 -> bool **)

let beq eqTypeC0 =
  eqTypeC0.beq

(** val ceqP : 'a1 eqTypeC -> 'a1 eq_axiom **)

let ceqP eqTypeC0 =
  eqTypeC0.ceqP

module EqType =
 struct
  (** val coq_HB_unnamed_factory_1 :
      'a1 eqTypeC -> 'a1 Coq_hasDecEq.axioms_ **)

  let coq_HB_unnamed_factory_1 ceqT =
    { Coq_hasDecEq.eq_op = ceqT.beq; Coq_hasDecEq.eqP = ceqT.ceqP }

  (** val coq_EqType_T__canonical__eqtype_Equality :
      'a1 eqTypeC -> Equality.coq_type **)

  let coq_EqType_T__canonical__eqtype_Equality ceqT =
    coq_HB_unnamed_factory_1 (Obj.magic ceqT)

  (** val ceqT_eqType : 'a1 eqTypeC -> Equality.coq_type **)

  let ceqT_eqType ceqT =
    reverse_coercion (coq_EqType_T__canonical__eqtype_Equality ceqT) __
 end

(** val ceqT_eqType : 'a1 eqTypeC -> Equality.coq_type **)

let ceqT_eqType =
  EqType.ceqT_eqType

type 't finTypeC = { _eqC : 't eqTypeC; cenum : 't list }

(** val _eqC : 'a1 finTypeC -> 'a1 eqTypeC **)

let _eqC finTypeC0 =
  finTypeC0._eqC

(** val cenum : 'a1 finTypeC -> 'a1 list **)

let cenum finTypeC0 =
  finTypeC0.cenum

module FinType =
 struct
  (** val coq_HB_unnamed_factory_3 : 'a1 finTypeC -> 'a1 Equality.axioms_ **)

  let coq_HB_unnamed_factory_3 cfinT =
    Obj.magic ceqT_eqType cfinT._eqC

  (** val eqtype_Equality__to__eqtype_hasDecEq :
      'a1 finTypeC -> 'a1 Coq_hasDecEq.axioms_ **)

  let eqtype_Equality__to__eqtype_hasDecEq =
    coq_HB_unnamed_factory_3

  (** val coq_HB_unnamed_mixin_5 :
      'a1 finTypeC -> 'a1 Coq_hasDecEq.axioms_ **)

  let coq_HB_unnamed_mixin_5 =
    coq_HB_unnamed_factory_3

  (** val coq_FinType_T__canonical__eqtype_Equality :
      'a1 finTypeC -> Equality.coq_type **)

  let coq_FinType_T__canonical__eqtype_Equality cfinT =
    coq_HB_unnamed_mixin_5 (Obj.magic cfinT)

  (** val coq_HB_unnamed_factory_6 :
      'a1 finTypeC -> 'a1 Choice.Coq_isCountable.phant_axioms **)

  let coq_HB_unnamed_factory_6 cfinT =
    Choice.coq_PCanIsCountable
      Choice.coq_Datatypes_nat__canonical__choice_Countable
      (Obj.magic FinIsCount.pickle (ceqT_eqType cfinT._eqC)
        (Obj.magic cfinT).cenum)
      (Obj.magic FinIsCount.unpickle (ceqT_eqType cfinT._eqC)
        (Obj.magic cfinT).cenum)

  (** val choice_isCountable__to__choice_Choice_isCountable :
      'a1 finTypeC -> 'a1 Choice.Choice_isCountable.axioms_ **)

  let choice_isCountable__to__choice_Choice_isCountable cfinT =
    Choice.Builders_77.coq_HB_unnamed_factory_83
      (coq_HB_unnamed_factory_6 cfinT)

  (** val choice_isCountable__to__choice_hasChoice :
      'a1 finTypeC -> 'a1 Choice.Coq_hasChoice.phant_axioms **)

  let choice_isCountable__to__choice_hasChoice cfinT =
    Choice.Builders_77.coq_HB_unnamed_factory_81
      (coq_HB_unnamed_factory_6 cfinT)

  (** val coq_HB_unnamed_mixin_10 :
      'a1 finTypeC -> 'a1 Choice.Coq_hasChoice.phant_axioms **)

  let coq_HB_unnamed_mixin_10 cfinT =
    Choice.Builders_77.coq_HB_unnamed_factory_81
      (coq_HB_unnamed_factory_6 cfinT)

  (** val coq_FinType_T__canonical__choice_Choice :
      'a1 finTypeC -> Choice.Choice.coq_type **)

  let coq_FinType_T__canonical__choice_Choice cfinT =
    { Choice.Choice.choice_hasChoice_mixin =
      (coq_HB_unnamed_mixin_10 (Obj.magic cfinT));
      Choice.Choice.eqtype_hasDecEq_mixin =
      (coq_HB_unnamed_mixin_5 (Obj.magic cfinT)) }

  (** val coq_HB_unnamed_mixin_11 :
      'a1 finTypeC -> 'a1 Choice.Choice_isCountable.axioms_ **)

  let coq_HB_unnamed_mixin_11 cfinT =
    Choice.Builders_77.coq_HB_unnamed_factory_83
      (coq_HB_unnamed_factory_6 cfinT)

  (** val coq_FinType_T__canonical__choice_Countable :
      'a1 finTypeC -> Choice.Countable.coq_type **)

  let coq_FinType_T__canonical__choice_Countable cfinT =
    { Choice.Countable.choice_hasChoice_mixin =
      (coq_HB_unnamed_mixin_10 (Obj.magic cfinT));
      Choice.Countable.eqtype_hasDecEq_mixin =
      (coq_HB_unnamed_mixin_5 (Obj.magic cfinT));
      Choice.Countable.choice_Choice_isCountable_mixin =
      (coq_HB_unnamed_mixin_11 (Obj.magic cfinT)) }

  (** val coq_HB_unnamed_factory_12 :
      'a1 finTypeC -> 'a1 Coq_isFinite.axioms_ **)

  let coq_HB_unnamed_factory_12 cfinT =
    cfinT.cenum

  (** val coq_FinType_T__canonical__fintype_Finite :
      'a1 finTypeC -> Finite.coq_type **)

  let coq_FinType_T__canonical__fintype_Finite cfinT =
    { Finite.choice_hasChoice_mixin =
      (coq_HB_unnamed_mixin_10 (Obj.magic cfinT));
      Finite.choice_Choice_isCountable_mixin =
      (coq_HB_unnamed_mixin_11 (Obj.magic cfinT));
      Finite.eqtype_hasDecEq_mixin =
      (coq_HB_unnamed_mixin_5 (Obj.magic cfinT));
      Finite.fintype_isFinite_mixin =
      (coq_HB_unnamed_factory_12 (Obj.magic cfinT)) }

  (** val cfinT_finType : 'a1 finTypeC -> Finite.coq_type **)

  let cfinT_finType cfinT =
    reverse_coercion (coq_FinType_T__canonical__fintype_Finite cfinT) __
 end

(** val cfinT_finType : 'a1 finTypeC -> Finite.coq_type **)

let cfinT_finType =
  FinType.cfinT_finType

module FinMap =
 struct
  type ('t, 'u) map = 'u finfun_of

  (** val of_fun : 'a1 finTypeC -> (Finite.sort -> 'a2) -> 'a2 finfun_of **)

  let of_fun cfinT =
    Coq_finfun.body (cfinT_finType cfinT)

  (** val set :
      'a1 finTypeC -> ('a1, 'a2) map -> 'a1 -> 'a2 -> ('a1, 'a2) map **)

  let set cfinT m x y =
    of_fun cfinT (fun z ->
      if eq_op (ceqT_eqType cfinT._eqC) z (Obj.magic x)
      then y
      else fun_of_fin (cfinT_finType cfinT) m z)
 end

(** val reflect_inj :
    Equality.coq_type -> (Equality.sort -> 'a1) -> Equality.sort ->
    Equality.sort -> reflect -> reflect **)

let reflect_inj t _ a b heq =
  iffP (eq_op t a b) heq

type ('e, 'a) result =
| Ok of 'a
| Error of 'e

(** val is_ok : ('a1, 'a2) result -> bool **)

let is_ok = function
| Ok _ -> true
| Error _ -> false

(** val is_okP : ('a1, 'a2) result -> reflect **)

let is_okP r =
  let _evar_0_ = fun _ -> ReflectT in
  let _evar_0_0 = fun _ -> ReflectF in
  (match r with
   | Ok a -> _evar_0_ a
   | Error e -> _evar_0_0 e)

module Result =
 struct
  (** val apply : ('a2 -> 'a3) -> 'a3 -> ('a1, 'a2) result -> 'a3 **)

  let apply f x = function
  | Ok y -> f y
  | Error _ -> x

  (** val bind :
      ('a2 -> ('a1, 'a3) result) -> ('a1, 'a2) result -> ('a1, 'a3) result **)

  let bind f = function
  | Ok x -> f x
  | Error s -> Error s

  (** val map : ('a2 -> 'a3) -> ('a1, 'a2) result -> ('a1, 'a3) result **)

  let map f = function
  | Ok x -> Ok (f x)
  | Error s -> Error s

  (** val default : 'a2 -> ('a1, 'a2) result -> 'a2 **)

  let default x =
    apply (fun x0 -> x0) x

  (** val map_err : ('a1 -> 'a2) -> ('a1, 'a3) result -> ('a2, 'a3) result **)

  let map_err f = function
  | Ok x -> Ok x
  | Error e -> Error (f e)
 end

(** val o2r : 'a1 -> 'a2 option -> ('a1, 'a2) result **)

let o2r e = function
| Some x -> Ok x
| None -> Error e

(** val coq_assert : bool -> 'a1 -> ('a1, unit) result **)

let coq_assert b e =
  if b then Ok () else Error e

type error =
| ErrOob
| ErrAddrUndef
| ErrAddrInvalid
| ErrStack
| ErrType
| ErrArith
| ErrSemUndef

type 't exec = (error, 't) result

(** val type_error : (error, 'a1) result **)

let type_error =
  Error ErrType

(** val undef_error : (error, 'a1) result **)

let undef_error =
  Error ErrAddrUndef

(** val rbindP :
    ('a1, 'a2) result -> ('a2 -> ('a1, 'a3) result) -> 'a3 -> ('a2 -> __ ->
    __ -> 'a4) -> 'a4 **)

let rbindP e _ _ x =
  let _evar_0_ = fun a h -> h a __ __ in
  let _evar_0_0 = fun _ _ -> assert false (* absurd case *) in
  (match e with
   | Ok a -> _evar_0_ a x
   | Error e0 -> _evar_0_0 e0 x)

(** val mapM :
    ('a2 -> ('a1, 'a3) result) -> 'a2 list -> ('a1, 'a3 list) result **)

let rec mapM f = function
| [] -> Ok []
| x :: xs0 ->
  (match f x with
   | Ok x0 ->
     (match mapM f xs0 with
      | Ok x1 -> Ok (x0 :: x1)
      | Error s -> Error s)
   | Error s -> Error s)

(** val mapMP :
    Equality.coq_type -> Equality.coq_type -> (Equality.sort -> ('a1,
    Equality.sort) result) -> Equality.sort list -> Equality.sort list ->
    Equality.sort -> reflect **)

let mapMP _ bT f s s' y =
  let _evar_0_ = fun _ -> ReflectF in
  let _evar_0_0 = fun x s0 iHs s'0 ->
    rbindP (f x) (fun y0 ->
      match mapM f s0 with
      | Ok x0 -> Ok (y0 :: x0)
      | Error s1 -> Error s1) s'0 (fun y0 _ _ ->
      rbindP (Obj.magic mapM f s0) (fun ys -> Ok (y0 :: (Obj.magic ys))) s'0
        (fun ys _ _ ->
        ssr_have_upoly (iHs ys __) (fun iHs' ->
          let _evar_0_0 = fun _ -> ReflectT in
          let _evar_0_1 = fun _ ->
            iffP (in_mem y (mem (seq_predType bT) ys)) iHs'
          in
          if eq_op bT y0 y then _evar_0_0 __ else _evar_0_1 __)))
  in
  let rec f0 l s'0 =
    match l with
    | [] -> _evar_0_ s'0
    | y0 :: l0 -> Obj.magic _evar_0_0 y0 l0 (fun s'1 _ -> f0 l0 s'1) s'0
  in f0 s s'

(** val foldM :
    ('a2 -> 'a3 -> ('a1, 'a3) result) -> 'a3 -> 'a2 list -> ('a1, 'a3) result **)

let rec foldM f acc = function
| [] -> Ok acc
| a :: la -> (match f a acc with
              | Ok x -> foldM f x la
              | Error s -> Error s)

(** val foldrM :
    ('a2 -> 'a3 -> ('a1, 'a3) result) -> 'a3 -> 'a2 list -> ('a1, 'a3) result **)

let rec foldrM f acc = function
| [] -> Ok acc
| a :: la -> (match foldrM f acc la with
              | Ok x -> f a x
              | Error s -> Error s)

(** val fold2 :
    'a3 -> ('a1 -> 'a2 -> 'a4 -> ('a3, 'a4) result) -> 'a1 list -> 'a2 list
    -> 'a4 -> ('a3, 'a4) result **)

let rec fold2 e f la lb r =
  match la with
  | [] -> (match lb with
           | [] -> Ok r
           | _ :: _ -> Error e)
  | a :: la0 ->
    (match lb with
     | [] -> Error e
     | b :: lb0 ->
       (match f a b r with
        | Ok x -> fold2 e f la0 lb0 x
        | Error s -> Error s))

(** val allM :
    ('a1 -> ('a2, unit) result) -> 'a1 list -> ('a2, unit) result **)

let allM check m =
  foldM (fun a _ -> check a) () m

(** val mapM2 :
    'a3 -> ('a1 -> 'a2 -> ('a3, 'a4) result) -> 'a1 list -> 'a2 list -> ('a3,
    'a4 list) result **)

let rec mapM2 e f la lb =
  match la with
  | [] -> (match lb with
           | [] -> Ok []
           | _ :: _ -> Error e)
  | a :: la0 ->
    (match lb with
     | [] -> Error e
     | b :: lb0 ->
       (match f a b with
        | Ok x ->
          (match mapM2 e f la0 lb0 with
           | Ok x0 -> Ok (x :: x0)
           | Error s -> Error s)
        | Error s -> Error s))

(** val fmap :
    ('a1 -> 'a2 -> 'a1 * 'a3) -> 'a1 -> 'a2 list -> 'a1 * 'a3 list **)

let rec fmap f a = function
| [] -> (a, [])
| b :: bs0 ->
  let (a0, c) = f a b in let (a1, cs) = fmap f a0 bs0 in (a1, (c :: cs))

(** val fmapM :
    ('a2 -> 'a3 -> ('a1, 'a2 * 'a4) result) -> 'a2 -> 'a3 list -> ('a1,
    'a2 * 'a4 list) result **)

let rec fmapM f a = function
| [] -> Ok (a, [])
| x :: xs0 ->
  (match f a x with
   | Ok x0 ->
     (match fmapM f (fst x0) xs0 with
      | Ok x1 -> Ok ((fst x1), ((snd x0) :: (snd x1)))
      | Error s -> Error s)
   | Error s -> Error s)

(** val fmapM2 :
    'a1 -> ('a2 -> 'a3 -> 'a4 -> ('a1, 'a2 * 'a5) result) -> 'a2 -> 'a3 list
    -> 'a4 list -> ('a1, 'a2 * 'a5 list) result **)

let rec fmapM2 e f a lb lc =
  match lb with
  | [] -> (match lc with
           | [] -> Ok (a, [])
           | _ :: _ -> Error e)
  | b :: bs ->
    (match lc with
     | [] -> Error e
     | c :: cs ->
       (match f a b c with
        | Ok x ->
          (match fmapM2 e f (fst x) bs cs with
           | Ok x0 -> Ok ((fst x0), ((snd x) :: (snd x0)))
           | Error s -> Error s)
        | Error s -> Error s))

(** val all2P : ('a1 -> 'a2 -> bool) -> 'a1 list -> 'a2 list -> reflect **)

let all2P p l1 l2 =
  let _evar_0_ = fun __top_assumption_ ->
    let _evar_0_ = ReflectT in
    let _evar_0_0 = fun _ _ -> ReflectF in
    (match __top_assumption_ with
     | [] -> _evar_0_
     | a :: l -> _evar_0_0 a l)
  in
  let _evar_0_0 = fun a l3 _ __top_assumption_ ->
    let _evar_0_0 = ReflectF in
    let _evar_0_1 = fun b l4 ->
      equivP ((&&) (p a b) (all2 p l3 l4)) (andP (p a b) (all2 p l3 l4))
    in
    (match __top_assumption_ with
     | [] -> _evar_0_0
     | a0 :: l -> _evar_0_1 a0 l)
  in
  let rec f = function
  | [] -> _evar_0_
  | y :: l0 -> _evar_0_0 y l0 (f l0)
  in f l1 l2

(** val reflect_all2_eqb :
    ('a1 -> 'a1 -> bool) -> ('a1 -> 'a1 -> reflect) -> 'a1 list -> 'a1 list
    -> reflect **)

let reflect_all2_eqb eqb _ l1 l2 =
  let _evar_0_ = fun __top_assumption_ ->
    let _evar_0_ = ReflectT in
    let _evar_0_0 = fun _ _ -> ReflectF in
    (match __top_assumption_ with
     | [] -> _evar_0_
     | a :: l -> _evar_0_0 a l)
  in
  let _evar_0_0 = fun e1 l3 _ __top_assumption_ ->
    let _evar_0_0 = ReflectF in
    let _evar_0_1 = fun e2 l4 ->
      iffP ((&&) (eqb e1 e2) (all2 eqb l3 l4))
        (andP (eqb e1 e2) (all2 eqb l3 l4))
    in
    (match __top_assumption_ with
     | [] -> _evar_0_0
     | a :: l -> _evar_0_1 a l)
  in
  let rec f = function
  | [] -> _evar_0_
  | y :: l0 -> _evar_0_0 y l0 (f l0)
  in f l1 l2

(** val map2 : ('a1 -> 'a2 -> 'a3) -> 'a1 list -> 'a2 list -> 'a3 list **)

let rec map2 f la lb =
  match la with
  | [] -> []
  | a :: la0 ->
    (match lb with
     | [] -> []
     | b :: lb0 -> (f a b) :: (map2 f la0 lb0))

(** val map3 :
    ('a1 -> 'a2 -> 'a3 -> 'a4) -> 'a1 list -> 'a2 list -> 'a3 list -> 'a4 list **)

let rec map3 f ma mb mc =
  match ma with
  | [] -> []
  | a :: ma' ->
    (match mb with
     | [] -> []
     | b :: mb' ->
       (match mc with
        | [] -> []
        | c :: mc' -> (f a b c) :: (map3 f ma' mb' mc')))

(** val mapi_aux : (nat -> 'a1 -> 'a2) -> nat -> 'a1 list -> 'a2 list **)

let rec mapi_aux f k = function
| [] -> []
| a :: l0 -> (f k a) :: (mapi_aux f (S k) l0)

(** val mapi : (nat -> 'a1 -> 'a2) -> 'a1 list -> 'a2 list **)

let mapi f =
  mapi_aux f O

(** val find_map : ('a1 -> 'a2 option) -> 'a1 list -> 'a2 option **)

let rec find_map f = function
| [] -> None
| a :: l0 ->
  let fa = f a in (match fa with
                   | Some _ -> fa
                   | None -> find_map f l0)

(** val isSome_obind : ('a1 -> 'a2 option) -> 'a1 option -> reflect **)

let isSome_obind f o =
  iff_reflect (isSome (Option.bind f o))

(** val list_to_rev : nat -> nat list **)

let rec list_to_rev = function
| O -> []
| S x -> x :: (list_to_rev x)

(** val list_to : nat -> nat list **)

let list_to ub =
  rev (list_to_rev ub)

(** val conc_map : ('a1 -> 'a2 list) -> 'a1 list -> 'a2 list **)

let conc_map f l =
  flatten (map f l)

(** val ctrans : comparison -> comparison -> comparison option **)

let ctrans c1 c2 =
  match c1 with
  | Eq -> Some c2
  | Lt -> (match c2 with
           | Eq -> Some c1
           | Lt -> Some Lt
           | Gt -> None)
  | Gt -> (match c2 with
           | Eq -> Some c1
           | Lt -> None
           | Gt -> Some Gt)

(** val coq_HB_unnamed_factory_14 : comparison Coq_hasDecEq.axioms_ **)

let coq_HB_unnamed_factory_14 =
  { Coq_hasDecEq.eq_op = Std.Prelude.comparison_eqb; Coq_hasDecEq.eqP =
    Std.Prelude.comparison_eqb_OK }

(** val coq_Datatypes_comparison__canonical__eqtype_Equality :
    Equality.coq_type **)

let coq_Datatypes_comparison__canonical__eqtype_Equality =
  Obj.magic coq_HB_unnamed_factory_14

(** val gcmp : ('a1 -> 'a1 -> comparison) -> 'a1 -> 'a1 -> comparison **)

let gcmp cmp =
  cmp

(** val cmp_lt : ('a1 -> 'a1 -> comparison) -> 'a1 -> 'a1 -> bool **)

let cmp_lt cmp x1 x2 =
  eq_op coq_Datatypes_comparison__canonical__eqtype_Equality
    (Obj.magic gcmp cmp x1 x2) (Obj.magic Lt)

(** val cmp_le : ('a1 -> 'a1 -> comparison) -> 'a1 -> 'a1 -> bool **)

let cmp_le cmp x1 x2 =
  negb
    (eq_op coq_Datatypes_comparison__canonical__eqtype_Equality
      (Obj.magic gcmp cmp x2 x1) (Obj.magic Lt))

(** val lex :
    ('a1 -> 'a1 -> comparison) -> ('a2 -> 'a2 -> comparison) -> ('a1 * 'a2)
    -> ('a1 * 'a2) -> comparison **)

let lex cmp1 cmp2 x y =
  match cmp1 (fst x) (fst y) with
  | Eq -> cmp2 (snd x) (snd y)
  | x0 -> x0

(** val cmp_min : ('a1 -> 'a1 -> comparison) -> 'a1 -> 'a1 -> 'a1 **)

let cmp_min cmp x y =
  if cmp_le cmp x y then x else y

(** val cmp_max : ('a1 -> 'a1 -> comparison) -> 'a1 -> 'a1 -> 'a1 **)

let cmp_max cmp x y =
  if cmp_le cmp x y then y else x

(** val bool_cmp : bool -> bool -> comparison **)

let bool_cmp b1 b2 =
  if b1 then if b2 then Eq else Gt else if b2 then Lt else Eq

(** val subrelation_iff_flip_arrow : (__, __) iffT -> (__, __) arrow **)

let subrelation_iff_flip_arrow __top_assumption_ =
  let _evar_0_ = fun _ b -> b in
  let (a, b) = __top_assumption_ in _evar_0_ a b

(** val reflect_m : bool -> bool -> (__, __) iffT **)

let reflect_m _ b2 =
  let _evar_0_ = ((fun h -> equivP b2 h), (fun h -> equivP b2 h)) in
  Obj.magic _evar_0_

(** val coq_P_leP : positive -> positive -> reflect **)

let coq_P_leP x y =
  equivP (Pos.leb x y)
    (subrelation_proper (Obj.magic __) (fun _ _ _ x0 x1 _ -> reflect_m x0 x1)
      ()
      (subrelation_respectful (Obj.magic __)
        (subrelation_respectful (Obj.magic __)
          (Obj.magic (fun _ _ -> subrelation_iff_flip_arrow)))) __ __ __
      (Pos.leb x y) (Pos.leb x y) __
      (if Pos.leb x y then ReflectT else ReflectF))

(** val coq_P_ltP : positive -> positive -> reflect **)

let coq_P_ltP x y =
  equivP (Pos.ltb x y)
    (subrelation_proper (Obj.magic __) (fun _ _ _ x0 x1 _ -> reflect_m x0 x1)
      ()
      (subrelation_respectful (Obj.magic __)
        (subrelation_respectful (Obj.magic __)
          (Obj.magic (fun _ _ -> subrelation_iff_flip_arrow)))) __ __ __
      (Pos.ltb x y) (Pos.ltb x y) __
      (if Pos.ltb x y then ReflectT else ReflectF))

type is_positive =
| Coq_is_xI of positive * is_positive
| Coq_is_xO of positive * is_positive
| Coq_is_xH

(** val is_positive_rect :
    (positive -> is_positive -> 'a1 -> 'a1) -> (positive -> is_positive ->
    'a1 -> 'a1) -> 'a1 -> positive -> is_positive -> 'a1 **)

let rec is_positive_rect f f0 f1 _ = function
| Coq_is_xI (p, p_) -> f p p_ (is_positive_rect f f0 f1 p p_)
| Coq_is_xO (p, p_) -> f0 p p_ (is_positive_rect f f0 f1 p p_)
| Coq_is_xH -> f1

(** val is_positive_rec :
    (positive -> is_positive -> 'a1 -> 'a1) -> (positive -> is_positive ->
    'a1 -> 'a1) -> 'a1 -> positive -> is_positive -> 'a1 **)

let rec is_positive_rec f f0 f1 _ = function
| Coq_is_xI (p, p_) -> f p p_ (is_positive_rec f f0 f1 p p_)
| Coq_is_xO (p, p_) -> f0 p p_ (is_positive_rec f f0 f1 p p_)
| Coq_is_xH -> f1

(** val positive_tag : positive -> positive **)

let positive_tag = function
| Coq_xI _ -> Coq_xH
| Coq_xO _ -> Coq_xO Coq_xH
| Coq_xH -> Coq_xI Coq_xH

(** val is_positive_inhab : positive -> is_positive **)

let rec is_positive_inhab = function
| Coq_xI h -> Coq_is_xI (h, (is_positive_inhab h))
| Coq_xO h -> Coq_is_xO (h, (is_positive_inhab h))
| Coq_xH -> Coq_is_xH

(** val is_positive_functor : positive -> is_positive -> is_positive **)

let rec is_positive_functor _ x =
  x

type box_positive_xI =
  positive
  (* singleton inductive, whose constructor was Box_positive_xI *)

(** val coq_Box_positive_xI_0 : box_positive_xI -> positive **)

let coq_Box_positive_xI_0 record =
  record

type box_positive_xH =
| Box_positive_xH

type positive_fields_t = __

(** val positive_fields : positive -> positive_fields_t **)

let positive_fields = function
| Coq_xI h -> Obj.magic h
| Coq_xO h -> Obj.magic h
| Coq_xH -> Obj.magic Box_positive_xH

(** val positive_construct :
    positive -> positive_fields_t -> positive option **)

let positive_construct p b =
  match p with
  | Coq_xI _ -> Some Coq_xH
  | Coq_xO _ -> Some (Coq_xO (Obj.magic b))
  | Coq_xH -> Some (Coq_xI (Obj.magic b))

(** val positive_induction :
    (positive -> 'a1 -> 'a1) -> (positive -> 'a1 -> 'a1) -> 'a1 -> positive
    -> is_positive -> 'a1 **)

let rec positive_induction his_xI his_xO his_xH _ = function
| Coq_is_xI (x0, p_) ->
  his_xI x0 (positive_induction his_xI his_xO his_xH x0 p_)
| Coq_is_xO (x0, p_) ->
  his_xO x0 (positive_induction his_xI his_xO his_xH x0 p_)
| Coq_is_xH -> his_xH

(** val positive_eqb_fields :
    (positive -> positive -> bool) -> positive -> positive_fields_t ->
    positive_fields_t -> bool **)

let positive_eqb_fields rec0 x a b =
  match x with
  | Coq_xI _ -> true
  | _ -> (&&) (rec0 (Obj.magic a) (Obj.magic b)) true

(** val positive_eqb : positive -> positive -> bool **)

let rec positive_eqb x1 x2 =
  match x1 with
  | Coq_xI h ->
    eqb_body positive_tag positive_fields
      (Obj.magic positive_eqb_fields positive_eqb) (positive_tag (Coq_xI h))
      h x2
  | Coq_xO h ->
    eqb_body positive_tag positive_fields
      (Obj.magic positive_eqb_fields positive_eqb) (positive_tag (Coq_xO h))
      h x2
  | Coq_xH ->
    eqb_body positive_tag positive_fields
      (Obj.magic positive_eqb_fields positive_eqb) (positive_tag Coq_xH)
      Box_positive_xH x2

(** val positive_eqb_OK : positive -> positive -> reflect **)

let positive_eqb_OK =
  iffP2 positive_eqb

(** val positive_eqb_OK_sumbool : positive -> positive -> bool **)

let positive_eqb_OK_sumbool =
  reflect_dec positive_eqb positive_eqb_OK

(** val coq_HB_unnamed_factory_16 : positive Coq_hasDecEq.axioms_ **)

let coq_HB_unnamed_factory_16 =
  { Coq_hasDecEq.eq_op = positive_eqb; Coq_hasDecEq.eqP = positive_eqb_OK }

(** val coq_BinNums_positive__canonical__eqtype_Equality :
    Equality.coq_type **)

let coq_BinNums_positive__canonical__eqtype_Equality =
  Obj.magic coq_HB_unnamed_factory_16

(** val coq_ZleP : coq_Z -> coq_Z -> reflect **)

let coq_ZleP =
  Z.leb_spec0

(** val coq_ZltP : coq_Z -> coq_Z -> reflect **)

let coq_ZltP =
  Z.ltb_spec0

(** val coq_ZNleP : nat -> nat -> reflect **)

let coq_ZNleP x y =
  let _evar_0_ = fun _ -> ReflectT in
  let _evar_0_0 = fun _ -> ReflectF in
  if leq x y then _evar_0_ __ else _evar_0_0 __

(** val coq_ZNltP : nat -> nat -> reflect **)

let coq_ZNltP x y =
  let _evar_0_ = fun _ -> ReflectT in
  let _evar_0_0 = fun _ -> ReflectF in
  if leq (S x) y then _evar_0_ __ else _evar_0_0 __

(** val ziota_rec : coq_Z -> coq_Z -> coq_Z list **)

let rec ziota_rec first = function
| Zpos p -> first :: (ziota_rec (Z.succ first) (Z.pred (Zpos p)))
| _ -> []

(** val ziota : coq_Z -> coq_Z -> coq_Z list **)

let ziota =
  ziota_rec

(** val pnth : 'a1 -> 'a1 list -> positive -> 'a1 **)

let rec pnth dfl m p =
  match m with
  | [] -> dfl
  | a :: m0 ->
    (match p with
     | Coq_xI q -> pnth dfl m0 (Coq_xO q)
     | Coq_xO q -> pnth dfl m0 (Pos.pred_double q)
     | Coq_xH -> a)

(** val znth : 'a1 -> 'a1 list -> coq_Z -> 'a1 **)

let znth dfl m z =
  match m with
  | [] -> dfl
  | a :: m0 -> (match z with
                | Z0 -> a
                | Zpos p -> pnth dfl m0 p
                | Zneg _ -> dfl)

(** val zindex :
    Equality.coq_type -> Equality.sort -> Equality.sort list -> coq_Z **)

let zindex t t0 l =
  Z.of_nat (index t t0 l)

type 'tr lprod = __

type ltuple = __

(** val merge_tuple : __ list -> __ list -> ltuple -> ltuple -> ltuple **)

let rec merge_tuple l1 l2 =
  match l1 with
  | [] -> (fun _ p -> p)
  | _ :: l3 ->
    let rec0 = merge_tuple l3 l2 in
    (fun x x0 ->
    match l3 with
    | [] -> (match l2 with
             | [] -> x
             | _ :: _ -> Obj.magic (x, x0))
    | _ :: _ -> Obj.magic ((fst (Obj.magic x)), (rec0 (snd (Obj.magic x)) x0)))

module Option =
 struct
 end

(** val obindP :
    'a1 option -> ('a1 -> 'a2 option) -> 'a2 -> ('a1 -> __ -> __ -> 'a3) ->
    'a3 **)

let obindP oa _ _ x =
  let _evar_0_ = fun a' h -> h a' __ __ in
  let _evar_0_0 = fun _ -> assert false (* absurd case *) in
  (match oa with
   | Some a -> _evar_0_ a x
   | None -> _evar_0_0 x)

(** val oassert : bool -> unit option **)

let oassert = function
| true -> Some ()
| false -> None

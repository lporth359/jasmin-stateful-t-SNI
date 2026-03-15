open Datatypes
open Nat0
open Specif
open Eqtype
open Seq
open Ssrbool
open Ssrfun
open Ssrnat

type __ = Obj.t

module CodeSeq =
 struct
  (** val code : nat list -> nat **)

  let code =
    foldr (fun n m -> muln (expn (S (S O)) n) (S (double m))) O

  (** val decode_rec : nat -> nat -> nat -> nat list **)

  let rec decode_rec v q r =
    match q with
    | O -> v :: []
    | S q' ->
      (match r with
       | O -> v :: (decode_rec O q' q')
       | S n ->
         (match n with
          | O -> decode_rec (S v) q' q'
          | S r' -> decode_rec v q' r'))

  (** val decode : nat -> nat list **)

  let decode n = match n with
  | O -> []
  | S _ -> decode_rec O (pred n) (pred n)
 end

(** val seq_of_opt : 'a1 option -> 'a1 list **)

let seq_of_opt u =
  Option.apply (nseq (S O)) [] u

(** val tag_of_pair : ('a1 * 'a2) -> ('a1, 'a2) sigT **)

let tag_of_pair p =
  coq_Tagged (fst p) (snd p)

(** val pair_of_tag : ('a1, 'a2) sigT -> 'a1 * 'a2 **)

let pair_of_tag u =
  ((tag u), (tagged u))

(** val opair_of_sum : ('a1, 'a2) sum -> 'a1 option * 'a2 option **)

let opair_of_sum = function
| Coq_inl x -> ((Some x), None)
| Coq_inr y -> (None, (Some y))

(** val sum_of_opair : ('a1 option * 'a2 option) -> ('a1, 'a2) sum option **)

let sum_of_opair p =
  Option.apply (comp (fun x -> Some x) (fun x -> Coq_inr x))
    (Option.map (fun x -> Coq_inl x) (fst p)) (snd p)

module Coq_hasChoice =
 struct
  type 't axioms_ =
    't pred -> nat -> 't option
    (* singleton inductive, whose constructor was Axioms_ *)

  (** val find_subdef : 'a1 axioms_ -> 'a1 pred -> nat -> 'a1 option **)

  let find_subdef record =
    record

  type 't phant_axioms = 't axioms_
 end

module Choice =
 struct
  type 't axioms_ = { choice_hasChoice_mixin : 't Coq_hasChoice.axioms_;
                      eqtype_hasDecEq_mixin : 't Coq_hasDecEq.axioms_ }

  (** val choice_hasChoice_mixin :
      'a1 axioms_ -> 'a1 Coq_hasChoice.axioms_ **)

  let choice_hasChoice_mixin record =
    record.choice_hasChoice_mixin

  type coq_type =
    __ axioms_
    (* singleton inductive, whose constructor was Pack *)

  type sort = __

  (** val coq_class : coq_type -> sort axioms_ **)

  let coq_class record =
    record

  module Exports =
   struct
    (** val choice_Choice_class__to__eqtype_Equality_class :
        'a1 axioms_ -> 'a1 Equality.axioms_ **)

    let choice_Choice_class__to__eqtype_Equality_class c =
      c.eqtype_hasDecEq_mixin

    (** val choice_Choice__to__eqtype_Equality :
        coq_type -> Equality.coq_type **)

    let choice_Choice__to__eqtype_Equality =
      choice_Choice_class__to__eqtype_Equality_class
   end
 end

(** val find_subdef :
    Choice.coq_type -> Choice.sort pred -> nat -> Choice.sort option **)

let find_subdef s x x0 =
  s.Choice.choice_hasChoice_mixin x x0

(** val coq_PCanHasChoice :
    Choice.coq_type -> ('a1 -> Choice.sort) -> (Choice.sort -> 'a1 option) ->
    'a1 Coq_hasChoice.phant_axioms **)

let coq_PCanHasChoice t _ f' =
  let liftP = fun sP -> coq_SimplPred (fun x -> Option.apply sP false (f' x))
  in
  let sf = fun sP n ->
    Option.bind f' (find_subdef t (PredOfSimpl.coerce (liftP sP)) n)
  in
  (fun sP -> fun_of_simpl (sf sP))

(** val coq_HB_unnamed_factory_6 :
    Choice.coq_type -> ('a1 -> Choice.sort) -> (Choice.sort -> 'a1 option) ->
    ('a1, Choice.sort) pcan_type Coq_hasChoice.phant_axioms **)

let coq_HB_unnamed_factory_6 =
  coq_PCanHasChoice

(** val eqtype_pcan_type__canonical__choice_Choice :
    Choice.coq_type -> ('a1 -> Choice.sort) -> (Choice.sort -> 'a1 option) ->
    Choice.coq_type **)

let eqtype_pcan_type__canonical__choice_Choice t f f' =
  { Choice.choice_hasChoice_mixin =
    (coq_HB_unnamed_factory_6 t (Obj.magic f) (Obj.magic f'));
    Choice.eqtype_hasDecEq_mixin =
    (coq_HB_unnamed_mixin_14
      (Choice.Exports.choice_Choice__to__eqtype_Equality t) (Obj.magic f)
      (Obj.magic f')) }

(** val coq_HB_unnamed_factory_9 :
    Choice.coq_type -> ('a1 -> Choice.sort) -> (Choice.sort -> 'a1) -> ('a1,
    Choice.sort) can_type Coq_hasChoice.phant_axioms **)

let coq_HB_unnamed_factory_9 t f f' =
  coq_PCanHasChoice t f (fun y -> Some (f' y))

(** val eqtype_can_type__canonical__choice_Choice :
    Choice.coq_type -> ('a1 -> Choice.sort) -> (Choice.sort -> 'a1) ->
    Choice.coq_type **)

let eqtype_can_type__canonical__choice_Choice t f f' =
  { Choice.choice_hasChoice_mixin =
    (coq_HB_unnamed_factory_9 t (Obj.magic f) (Obj.magic f'));
    Choice.eqtype_hasDecEq_mixin =
    (coq_HB_unnamed_mixin_19
      (Choice.Exports.choice_Choice__to__eqtype_Equality t) (Obj.magic f)
      (Obj.magic f')) }

(** val seq_hasChoice :
    Choice.coq_type -> Choice.sort list Coq_hasChoice.phant_axioms **)

let seq_hasChoice t =
  let r = fun f xs x -> f (x :: xs) in
  let f =
    let rec f sP ns xs =
      match ns with
      | [] -> if sP xs then Some xs else None
      | n :: ns1 ->
        let fr = fun_of_simpl (r (f sP ns1)) xs in
        Option.bind fr (find_subdef t (fun x -> isSome (fr x)) n)
    in f
  in
  (fun sP nn -> f sP (CodeSeq.decode nn) [])

(** val coq_HB_unnamed_factory_16 :
    Choice.coq_type -> Choice.sort list Coq_hasChoice.phant_axioms **)

let coq_HB_unnamed_factory_16 =
  seq_hasChoice

(** val coq_Datatypes_list__canonical__choice_Choice :
    Choice.coq_type -> Choice.coq_type **)

let coq_Datatypes_list__canonical__choice_Choice t =
  { Choice.choice_hasChoice_mixin = (Obj.magic coq_HB_unnamed_factory_16 t);
    Choice.eqtype_hasDecEq_mixin =
    (Obj.magic Seq.coq_HB_unnamed_factory_1
      (Choice.Exports.choice_Choice__to__eqtype_Equality t)) }

(** val tagged_hasChoice :
    Choice.coq_type -> (Choice.sort -> Choice.coq_type) -> (Choice.sort,
    Choice.sort) sigT Coq_hasChoice.phant_axioms **)

let tagged_hasChoice i t_ =
  let ft = fun tP n i0 ->
    Option.map (coq_Tagged i0)
      (find_subdef (t_ i0) (comp tP (coq_Tagged i0)) n)
  in
  let fi = fun tP ni nt ->
    Option.bind (ft tP nt) (find_subdef i (fun i0 -> isSome (ft tP nt i0)) ni)
  in
  (fun tP n ->
  match CodeSeq.decode n with
  | [] -> None
  | ni :: l ->
    (match l with
     | [] -> None
     | nt :: l0 -> (match l0 with
                    | [] -> fi tP ni nt
                    | _ :: _ -> None)))

(** val coq_HB_unnamed_factory_18 :
    Choice.coq_type -> (Choice.sort -> Choice.coq_type) -> (Choice.sort,
    Choice.sort) sigT Coq_hasChoice.phant_axioms **)

let coq_HB_unnamed_factory_18 =
  tagged_hasChoice

(** val coq_Specif_sigT__canonical__choice_Choice :
    Choice.coq_type -> (Choice.sort -> Choice.coq_type) -> Choice.coq_type **)

let coq_Specif_sigT__canonical__choice_Choice i t_ =
  { Choice.choice_hasChoice_mixin =
    (Obj.magic coq_HB_unnamed_factory_18 i t_);
    Choice.eqtype_hasDecEq_mixin =
    (Obj.magic coq_HB_unnamed_factory_42
      (Choice.Exports.choice_Choice__to__eqtype_Equality i) (fun i0 ->
      Choice.Exports.choice_Choice__to__eqtype_Equality (t_ i0))) }

(** val nat_hasChoice : nat Coq_hasChoice.phant_axioms **)

let nat_hasChoice =
  let f = fun p n -> if p n then Some n else None in
  (fun p -> fun_of_simpl (f p))

(** val coq_HB_unnamed_factory_20 : nat Coq_hasChoice.phant_axioms **)

let coq_HB_unnamed_factory_20 =
  nat_hasChoice

(** val coq_Datatypes_nat__canonical__choice_Choice : Choice.coq_type **)

let coq_Datatypes_nat__canonical__choice_Choice =
  { Choice.choice_hasChoice_mixin = (Obj.magic coq_HB_unnamed_factory_20);
    Choice.eqtype_hasDecEq_mixin = (Obj.magic coq_HB_unnamed_factory_1) }

(** val coq_HB_unnamed_factory_43 :
    Choice.coq_type -> Choice.sort option Choice.axioms_ **)

let coq_HB_unnamed_factory_43 t =
  Obj.magic eqtype_can_type__canonical__choice_Choice
    (coq_Datatypes_list__canonical__choice_Choice t) seq_of_opt ohead

(** val coq_HB_unnamed_mixin_48 :
    Choice.coq_type -> Choice.sort option Coq_hasChoice.axioms_ **)

let coq_HB_unnamed_mixin_48 t =
  (coq_HB_unnamed_factory_43 t).Choice.choice_hasChoice_mixin

(** val coq_Datatypes_option__canonical__choice_Choice :
    Choice.coq_type -> Choice.coq_type **)

let coq_Datatypes_option__canonical__choice_Choice t =
  { Choice.choice_hasChoice_mixin = (Obj.magic coq_HB_unnamed_mixin_48 t);
    Choice.eqtype_hasDecEq_mixin =
    (Obj.magic coq_HB_unnamed_factory_40
      (Choice.Exports.choice_Choice__to__eqtype_Equality t)) }

(** val coq_HB_unnamed_factory_50 :
    Choice.coq_type -> Choice.coq_type -> (Choice.sort * Choice.sort)
    Choice.axioms_ **)

let coq_HB_unnamed_factory_50 t1 t2 =
  Obj.magic eqtype_can_type__canonical__choice_Choice
    (coq_Specif_sigT__canonical__choice_Choice t1 (fun _ -> t2)) tag_of_pair
    pair_of_tag

(** val coq_HB_unnamed_mixin_55 :
    Choice.coq_type -> Choice.coq_type -> (Choice.sort * Choice.sort)
    Coq_hasChoice.axioms_ **)

let coq_HB_unnamed_mixin_55 t1 t2 =
  (coq_HB_unnamed_factory_50 t1 t2).Choice.choice_hasChoice_mixin

(** val coq_Datatypes_prod__canonical__choice_Choice :
    Choice.coq_type -> Choice.coq_type -> Choice.coq_type **)

let coq_Datatypes_prod__canonical__choice_Choice t1 t2 =
  { Choice.choice_hasChoice_mixin =
    (Obj.magic coq_HB_unnamed_mixin_55 t1 t2); Choice.eqtype_hasDecEq_mixin =
    (Obj.magic coq_HB_unnamed_factory_38
      (Choice.Exports.choice_Choice__to__eqtype_Equality t1)
      (Choice.Exports.choice_Choice__to__eqtype_Equality t2)) }

(** val coq_HB_unnamed_factory_57 :
    Choice.coq_type -> Choice.coq_type -> (Choice.sort, Choice.sort) sum
    Choice.axioms_ **)

let coq_HB_unnamed_factory_57 t1 t2 =
  Obj.magic eqtype_pcan_type__canonical__choice_Choice
    (coq_Datatypes_prod__canonical__choice_Choice
      (coq_Datatypes_option__canonical__choice_Choice t1)
      (coq_Datatypes_option__canonical__choice_Choice t2)) opair_of_sum
    sum_of_opair

(** val coq_HB_unnamed_mixin_62 :
    Choice.coq_type -> Choice.coq_type -> (Choice.sort, Choice.sort) sum
    Coq_hasChoice.axioms_ **)

let coq_HB_unnamed_mixin_62 t1 t2 =
  (coq_HB_unnamed_factory_57 t1 t2).Choice.choice_hasChoice_mixin

module Choice_isCountable =
 struct
  type 't axioms_ = { pickle : ('t -> nat); unpickle : (nat -> 't option) }

  (** val pickle : 'a1 axioms_ -> 'a1 -> nat **)

  let pickle record =
    record.pickle

  (** val unpickle : 'a1 axioms_ -> nat -> 'a1 option **)

  let unpickle record =
    record.unpickle

  (** val phant_Build : ('a1 -> nat) -> (nat -> 'a1 option) -> 'a1 axioms_ **)

  let phant_Build pickle0 unpickle0 =
    { pickle = pickle0; unpickle = unpickle0 }
 end

module Countable =
 struct
  type 't axioms_ = { choice_hasChoice_mixin : 't Coq_hasChoice.axioms_;
                      eqtype_hasDecEq_mixin : 't Coq_hasDecEq.axioms_;
                      choice_Choice_isCountable_mixin : 't
                                                        Choice_isCountable.axioms_ }

  (** val choice_hasChoice_mixin :
      'a1 axioms_ -> 'a1 Coq_hasChoice.axioms_ **)

  let choice_hasChoice_mixin record =
    record.choice_hasChoice_mixin

  (** val eqtype_hasDecEq_mixin : 'a1 axioms_ -> 'a1 Coq_hasDecEq.axioms_ **)

  let eqtype_hasDecEq_mixin record =
    record.eqtype_hasDecEq_mixin

  (** val choice_Choice_isCountable_mixin :
      'a1 axioms_ -> 'a1 Choice_isCountable.axioms_ **)

  let choice_Choice_isCountable_mixin record =
    record.choice_Choice_isCountable_mixin

  type coq_type =
    __ axioms_
    (* singleton inductive, whose constructor was Pack *)

  type sort = __

  (** val coq_class : coq_type -> sort axioms_ **)

  let coq_class record =
    record

  module Exports =
   struct
    (** val choice_Countable_class__to__eqtype_Equality_class :
        'a1 axioms_ -> 'a1 Equality.axioms_ **)

    let choice_Countable_class__to__eqtype_Equality_class c =
      c.eqtype_hasDecEq_mixin

    (** val choice_Countable__to__eqtype_Equality :
        coq_type -> Equality.coq_type **)

    let choice_Countable__to__eqtype_Equality =
      choice_Countable_class__to__eqtype_Equality_class

    (** val choice_Countable_class__to__choice_Choice_class :
        'a1 axioms_ -> 'a1 Choice.axioms_ **)

    let choice_Countable_class__to__choice_Choice_class c =
      { Choice.choice_hasChoice_mixin = c.choice_hasChoice_mixin;
        Choice.eqtype_hasDecEq_mixin = c.eqtype_hasDecEq_mixin }

    (** val choice_Countable__to__choice_Choice :
        coq_type -> Choice.coq_type **)

    let choice_Countable__to__choice_Choice =
      choice_Countable_class__to__choice_Choice_class
   end
 end

(** val pickle : Countable.coq_type -> Countable.sort -> nat **)

let pickle s x =
  s.Countable.choice_Choice_isCountable_mixin.Choice_isCountable.pickle x

(** val unpickle : Countable.coq_type -> nat -> Countable.sort option **)

let unpickle s x =
  s.Countable.choice_Choice_isCountable_mixin.Choice_isCountable.unpickle x

module Coq_isCountable =
 struct
  type 't axioms_ = { pickle : ('t -> nat); unpickle : (nat -> 't option) }

  (** val pickle : 'a1 axioms_ -> 'a1 -> nat **)

  let pickle record =
    record.pickle

  (** val unpickle : 'a1 axioms_ -> nat -> 'a1 option **)

  let unpickle record =
    record.unpickle

  (** val phant_Build : ('a1 -> nat) -> (nat -> 'a1 option) -> 'a1 axioms_ **)

  let phant_Build pickle0 unpickle0 =
    { pickle = pickle0; unpickle = unpickle0 }

  type 't phant_axioms = 't axioms_
 end

module Builders_77 =
 struct
  (** val coq_HB_unnamed_factory_81 :
      'a1 Coq_isCountable.phant_axioms -> 'a1 Coq_hasChoice.phant_axioms **)

  let coq_HB_unnamed_factory_81 fresh_name_78 =
    coq_PCanHasChoice coq_Datatypes_nat__canonical__choice_Choice
      (Obj.magic fresh_name_78.Coq_isCountable.pickle)
      (Obj.magic fresh_name_78.Coq_isCountable.unpickle)

  (** val coq_HB_unnamed_factory_83 :
      'a1 Coq_isCountable.phant_axioms -> 'a1 Choice_isCountable.axioms_ **)

  let coq_HB_unnamed_factory_83 fresh_name_78 =
    Choice_isCountable.phant_Build fresh_name_78.Coq_isCountable.pickle
      fresh_name_78.Coq_isCountable.unpickle
 end

(** val coq_PCanIsCountable :
    Countable.coq_type -> ('a1 -> Countable.sort) -> (Countable.sort -> 'a1
    option) -> 'a1 Coq_isCountable.axioms_ **)

let coq_PCanIsCountable t f f' =
  Coq_isCountable.phant_Build (comp (pickle t) f) (pcomp f' (unpickle t))

(** val coq_CanIsCountable :
    Countable.coq_type -> ('a1 -> Countable.sort) -> (Countable.sort -> 'a1)
    -> 'a1 Coq_isCountable.axioms_ **)

let coq_CanIsCountable t f f' =
  coq_PCanIsCountable t f (fun y -> Some (f' y))

(** val coq_HB_unnamed_factory_87 :
    Countable.coq_type -> ('a1 -> Countable.sort) -> (Countable.sort -> 'a1
    option) -> ('a1, Countable.sort) pcan_type Coq_isCountable.phant_axioms **)

let coq_HB_unnamed_factory_87 =
  coq_PCanIsCountable

(** val coq_HB_unnamed_mixin_91 :
    Countable.coq_type -> ('a1 -> Countable.sort) -> (Countable.sort -> 'a1
    option) -> ('a1, Countable.sort) pcan_type Choice_isCountable.axioms_ **)

let coq_HB_unnamed_mixin_91 t f f' =
  Builders_77.coq_HB_unnamed_factory_83 (coq_HB_unnamed_factory_87 t f f')

(** val eqtype_pcan_type__canonical__choice_Countable :
    Countable.coq_type -> ('a1 -> Countable.sort) -> (Countable.sort -> 'a1
    option) -> Countable.coq_type **)

let eqtype_pcan_type__canonical__choice_Countable t f f' =
  { Countable.choice_hasChoice_mixin =
    (coq_HB_unnamed_factory_6
      (Countable.Exports.choice_Countable__to__choice_Choice t) (Obj.magic f)
      (Obj.magic f')); Countable.eqtype_hasDecEq_mixin =
    (coq_HB_unnamed_mixin_14
      (Countable.Exports.choice_Countable__to__eqtype_Equality t)
      (Obj.magic f) (Obj.magic f'));
    Countable.choice_Choice_isCountable_mixin =
    (coq_HB_unnamed_mixin_91 t (Obj.magic f) (Obj.magic f')) }

(** val coq_HB_unnamed_factory_93 :
    Countable.coq_type -> ('a1 -> Countable.sort) -> (Countable.sort -> 'a1)
    -> ('a1, Countable.sort) can_type Coq_isCountable.phant_axioms **)

let coq_HB_unnamed_factory_93 =
  coq_CanIsCountable

(** val coq_HB_unnamed_mixin_100 :
    Countable.coq_type -> ('a1 -> Countable.sort) -> (Countable.sort -> 'a1)
    -> ('a1, Countable.sort) can_type Choice_isCountable.axioms_ **)

let coq_HB_unnamed_mixin_100 t f f' =
  Builders_77.coq_HB_unnamed_factory_83 (coq_HB_unnamed_factory_93 t f f')

(** val eqtype_can_type__canonical__choice_Countable :
    Countable.coq_type -> ('a1 -> Countable.sort) -> (Countable.sort -> 'a1)
    -> Countable.coq_type **)

let eqtype_can_type__canonical__choice_Countable t f f' =
  { Countable.choice_hasChoice_mixin =
    (coq_HB_unnamed_factory_9
      (Countable.Exports.choice_Countable__to__choice_Choice t) (Obj.magic f)
      (Obj.magic f')); Countable.eqtype_hasDecEq_mixin =
    (coq_HB_unnamed_mixin_19
      (Countable.Exports.choice_Countable__to__eqtype_Equality t)
      (Obj.magic f) (Obj.magic f'));
    Countable.choice_Choice_isCountable_mixin =
    (coq_HB_unnamed_mixin_100 t (Obj.magic f) (Obj.magic f')) }

(** val coq_HB_unnamed_mixin_106 :
    Countable.coq_type -> Countable.sort pred -> Countable.sort
    SubType.coq_type -> Choice.sort SubType.sort Coq_hasChoice.phant_axioms **)

let coq_HB_unnamed_mixin_106 t p sT =
  coq_PCanHasChoice (Countable.Exports.choice_Countable__to__choice_Choice t)
    (SubType.phant_on_ p sT).Coq_isSub.val_subdef (insub p sT)

(** val coq_HB_unnamed_mixin_107 :
    Countable.coq_type -> Countable.sort pred -> Countable.sort
    SubType.coq_type -> (Equality.sort SubType.sort, Equality.sort) inj_type
    Coq_hasDecEq.axioms_ **)

let coq_HB_unnamed_mixin_107 t p sT =
  { Coq_hasDecEq.eq_op = (fun x y ->
    eq_op (Countable.Exports.choice_Countable__to__eqtype_Equality t)
      ((SubType.phant_on_ p sT).Coq_isSub.val_subdef x)
      ((SubType.phant_on_ p sT).Coq_isSub.val_subdef y)); Coq_hasDecEq.eqP =
    (inj_eqAxiom (Countable.Exports.choice_Countable__to__eqtype_Equality t)
      (SubType.phant_on_ p sT).Coq_isSub.val_subdef) }

(** val coq_HB_unnamed_mixin_109 :
    Countable.coq_type -> Countable.sort pred -> Countable.sort
    SubType.coq_type -> (Countable.sort SubType.sort, Countable.sort)
    pcan_type Choice_isCountable.axioms_ **)

let coq_HB_unnamed_mixin_109 t p sT =
  { Choice_isCountable.pickle =
    (coq_HB_unnamed_factory_87 t
      (SubType.phant_on_ p sT).Coq_isSub.val_subdef (insub p sT)).Coq_isCountable.pickle;
    Choice_isCountable.unpickle =
    (coq_HB_unnamed_factory_87 t
      (SubType.phant_on_ p sT).Coq_isSub.val_subdef (insub p sT)).Coq_isCountable.unpickle }

(** val eqtype_sub_type__canonical__choice_Countable :
    Countable.coq_type -> Countable.sort pred -> Countable.sort
    SubType.coq_type -> Countable.coq_type **)

let eqtype_sub_type__canonical__choice_Countable t p sT =
  { Countable.choice_hasChoice_mixin = (coq_HB_unnamed_mixin_106 t p sT);
    Countable.eqtype_hasDecEq_mixin = (coq_HB_unnamed_mixin_107 t p sT);
    Countable.choice_Choice_isCountable_mixin =
    (coq_HB_unnamed_mixin_109 t p sT) }

(** val pickle_seq : Countable.coq_type -> Countable.sort list -> nat **)

let pickle_seq t s =
  CodeSeq.code (map (pickle t) s)

(** val unpickle_seq :
    Countable.coq_type -> nat -> Countable.sort list option **)

let unpickle_seq t n =
  Some (pmap (unpickle t) (CodeSeq.decode n))

(** val coq_HB_unnamed_factory_110 :
    Countable.coq_type -> Countable.sort list Coq_isCountable.axioms_ **)

let coq_HB_unnamed_factory_110 t =
  { Coq_isCountable.pickle = (pickle_seq t); Coq_isCountable.unpickle =
    (unpickle_seq t) }

(** val coq_HB_unnamed_mixin_117 :
    Countable.coq_type -> Countable.sort list Choice_isCountable.axioms_ **)

let coq_HB_unnamed_mixin_117 t =
  Builders_77.coq_HB_unnamed_factory_83 (coq_HB_unnamed_factory_110 t)

(** val coq_Datatypes_list__canonical__choice_Countable :
    Countable.coq_type -> Countable.coq_type **)

let coq_Datatypes_list__canonical__choice_Countable t =
  { Countable.choice_hasChoice_mixin =
    (Obj.magic coq_HB_unnamed_factory_16
      (Countable.Exports.choice_Countable__to__choice_Choice t));
    Countable.eqtype_hasDecEq_mixin =
    (Obj.magic Seq.coq_HB_unnamed_factory_1
      (Countable.Exports.choice_Countable__to__eqtype_Equality t));
    Countable.choice_Choice_isCountable_mixin =
    (Obj.magic coq_HB_unnamed_mixin_117 t) }

(** val pickle_tagged :
    Countable.coq_type -> (Countable.sort -> Countable.coq_type) ->
    (Countable.sort, Countable.sort) sigT -> nat **)

let pickle_tagged i t_ u =
  CodeSeq.code
    ((pickle i (tag u)) :: ((pickle (t_ (tag u)) (tagged u)) :: []))

(** val unpickle_tagged :
    Countable.coq_type -> (Countable.sort -> Countable.coq_type) -> nat ->
    (Countable.sort, Countable.sort) sigT option **)

let unpickle_tagged i t_ s =
  match CodeSeq.decode s with
  | [] -> None
  | ni :: l ->
    (match l with
     | [] -> None
     | nx :: l0 ->
       (match l0 with
        | [] ->
          Option.bind (fun i0 ->
            Option.map (coq_Tagged i0) (unpickle (t_ i0) nx)) (unpickle i ni)
        | _ :: _ -> None))

(** val coq_HB_unnamed_factory_118 :
    Countable.coq_type -> (Countable.sort -> Countable.coq_type) ->
    (Countable.sort, Countable.sort) sigT Choice_isCountable.axioms_ **)

let coq_HB_unnamed_factory_118 i t_ =
  { Choice_isCountable.pickle = (pickle_tagged i t_);
    Choice_isCountable.unpickle = (unpickle_tagged i t_) }

(** val coq_Specif_sigT__canonical__choice_Countable :
    Countable.coq_type -> (Countable.sort -> Countable.coq_type) ->
    Countable.coq_type **)

let coq_Specif_sigT__canonical__choice_Countable i t_ =
  { Countable.choice_hasChoice_mixin =
    (Obj.magic coq_HB_unnamed_factory_18
      (Countable.Exports.choice_Countable__to__choice_Choice i) (fun i0 ->
      Countable.Exports.choice_Countable__to__choice_Choice (t_ i0)));
    Countable.eqtype_hasDecEq_mixin =
    (Obj.magic coq_HB_unnamed_factory_42
      (Countable.Exports.choice_Countable__to__eqtype_Equality i) (fun i0 ->
      Countable.Exports.choice_Countable__to__eqtype_Equality (t_ i0)));
    Countable.choice_Choice_isCountable_mixin =
    (Obj.magic coq_HB_unnamed_factory_118 i t_) }

(** val coq_HB_unnamed_factory_120 : nat Choice_isCountable.axioms_ **)

let coq_HB_unnamed_factory_120 =
  { Choice_isCountable.pickle = (fun x -> x); Choice_isCountable.unpickle =
    (fun x -> Some x) }

(** val coq_Datatypes_nat__canonical__choice_Countable :
    Countable.coq_type **)

let coq_Datatypes_nat__canonical__choice_Countable =
  { Countable.choice_hasChoice_mixin = (Obj.magic coq_HB_unnamed_factory_20);
    Countable.eqtype_hasDecEq_mixin = (Obj.magic coq_HB_unnamed_factory_1);
    Countable.choice_Choice_isCountable_mixin =
    (Obj.magic coq_HB_unnamed_factory_120) }

(** val coq_HB_unnamed_factory_147 :
    Countable.coq_type -> Countable.sort option Countable.axioms_ **)

let coq_HB_unnamed_factory_147 t =
  Obj.magic eqtype_can_type__canonical__choice_Countable
    (coq_Datatypes_list__canonical__choice_Countable t) seq_of_opt ohead

(** val coq_HB_unnamed_mixin_154 :
    Countable.coq_type -> Countable.sort option Choice_isCountable.axioms_ **)

let coq_HB_unnamed_mixin_154 t =
  (coq_HB_unnamed_factory_147 t).Countable.choice_Choice_isCountable_mixin

(** val coq_Datatypes_option__canonical__choice_Countable :
    Countable.coq_type -> Countable.coq_type **)

let coq_Datatypes_option__canonical__choice_Countable t =
  { Countable.choice_hasChoice_mixin =
    (Obj.magic coq_HB_unnamed_mixin_48
      (Countable.Exports.choice_Countable__to__choice_Choice t));
    Countable.eqtype_hasDecEq_mixin =
    (Obj.magic coq_HB_unnamed_factory_40
      (Countable.Exports.choice_Countable__to__eqtype_Equality t));
    Countable.choice_Choice_isCountable_mixin =
    (Obj.magic coq_HB_unnamed_mixin_154 t) }

(** val coq_HB_unnamed_factory_165 :
    Countable.coq_type -> Countable.coq_type ->
    (Countable.sort * Countable.sort) Countable.axioms_ **)

let coq_HB_unnamed_factory_165 t1 t2 =
  Obj.magic eqtype_can_type__canonical__choice_Countable
    (coq_Specif_sigT__canonical__choice_Countable t1 (fun _ -> t2))
    tag_of_pair pair_of_tag

(** val coq_HB_unnamed_mixin_172 :
    Countable.coq_type -> Countable.coq_type ->
    (Countable.sort * Countable.sort) Choice_isCountable.axioms_ **)

let coq_HB_unnamed_mixin_172 t1 t2 =
  (coq_HB_unnamed_factory_165 t1 t2).Countable.choice_Choice_isCountable_mixin

(** val coq_Datatypes_prod__canonical__choice_Countable :
    Countable.coq_type -> Countable.coq_type -> Countable.coq_type **)

let coq_Datatypes_prod__canonical__choice_Countable t1 t2 =
  { Countable.choice_hasChoice_mixin =
    (Obj.magic coq_HB_unnamed_mixin_55
      (Countable.Exports.choice_Countable__to__choice_Choice t1)
      (Countable.Exports.choice_Countable__to__choice_Choice t2));
    Countable.eqtype_hasDecEq_mixin =
    (Obj.magic coq_HB_unnamed_factory_38
      (Countable.Exports.choice_Countable__to__eqtype_Equality t1)
      (Countable.Exports.choice_Countable__to__eqtype_Equality t2));
    Countable.choice_Choice_isCountable_mixin =
    (Obj.magic coq_HB_unnamed_mixin_172 t1 t2) }

(** val coq_HB_unnamed_factory_174 :
    Countable.coq_type -> Countable.coq_type -> (Countable.sort,
    Countable.sort) sum Countable.axioms_ **)

let coq_HB_unnamed_factory_174 t1 t2 =
  Obj.magic eqtype_pcan_type__canonical__choice_Countable
    (coq_Datatypes_prod__canonical__choice_Countable
      (coq_Datatypes_option__canonical__choice_Countable t1)
      (coq_Datatypes_option__canonical__choice_Countable t2)) opair_of_sum
    sum_of_opair

(** val coq_HB_unnamed_mixin_181 :
    Countable.coq_type -> Countable.coq_type -> (Countable.sort,
    Countable.sort) sum Choice_isCountable.axioms_ **)

let coq_HB_unnamed_mixin_181 t1 t2 =
  (coq_HB_unnamed_factory_174 t1 t2).Countable.choice_Choice_isCountable_mixin

(** val coq_Datatypes_sum__canonical__choice_Countable :
    Countable.coq_type -> Countable.coq_type -> Countable.coq_type **)

let coq_Datatypes_sum__canonical__choice_Countable t1 t2 =
  { Countable.choice_hasChoice_mixin =
    (Obj.magic coq_HB_unnamed_mixin_62
      (Countable.Exports.choice_Countable__to__choice_Choice t1)
      (Countable.Exports.choice_Countable__to__choice_Choice t2));
    Countable.eqtype_hasDecEq_mixin =
    (Obj.magic coq_HB_unnamed_factory_44
      (Countable.Exports.choice_Countable__to__eqtype_Equality t1)
      (Countable.Exports.choice_Countable__to__eqtype_Equality t2));
    Countable.choice_Choice_isCountable_mixin =
    (Obj.magic coq_HB_unnamed_mixin_181 t1 t2) }

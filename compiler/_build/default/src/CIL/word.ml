open BinInt
open BinNums
open Datatypes
open Nat0
open PeanoNat
open Prelude
open Zpower
open Bigop
open Div
open Eqtype
open Fintype
open Ssralg
open Ssrint
open Ssrnat
open Ssrnum
open Tuple
open Word_ssrZ

let __ = let rec f _ = Obj.repr f in Obj.repr f

(** val modulus : nat -> coq_Z **)

let modulus =
  two_power_nat

type word = coq_Z
  (* singleton inductive, whose constructor was mkWord *)

(** val coq_HB_unnamed_factory_1 : nat -> (coq_Z, word) Coq_isSub.axioms_ **)

let coq_HB_unnamed_factory_1 _ =
  { Coq_isSub.val_subdef = (fun w -> w); Coq_isSub.coq_Sub = (fun x _ -> x);
    Coq_isSub.coq_Sub_rect = (fun _ k_S u -> k_S u __) }

(** val word_word__canonical__eqtype_SubType :
    nat -> coq_Z SubType.coq_type **)

let word_word__canonical__eqtype_SubType nbits =
  Obj.magic coq_HB_unnamed_factory_1 nbits

(** val coq_HB_unnamed_factory_3 : nat -> word Choice.Countable.axioms_ **)

let coq_HB_unnamed_factory_3 nbits =
  Obj.magic Choice.eqtype_sub_type__canonical__choice_Countable
    coq_BinNums_Z__canonical__choice_Countable (fun x ->
    (&&)
      (Order.Order.le ring_display
        (Num.POrderedZmodule.Exports.join_Num_POrderedZmodule_between_GRing_Nmodule_and_Order_POrder
          coq_BinNums_Z__canonical__Num_POrderedZmodule)
        (GRing.zero
          (Num.POrderedZmodule.Exports.coq_Num_POrderedZmodule__to__GRing_Nmodule
            coq_BinNums_Z__canonical__Num_POrderedZmodule)) x)
      (Order.Order.lt ring_display coq_BinNums_Z__canonical__Order_POrder x
        (Obj.magic modulus nbits)))
    (reverse_coercion (Obj.magic word_word__canonical__eqtype_SubType nbits)
      __)

(** val coq_HB_unnamed_mixin_7 : nat -> word Coq_hasDecEq.axioms_ **)

let coq_HB_unnamed_mixin_7 nbits =
  (coq_HB_unnamed_factory_3 nbits).Choice.Countable.eqtype_hasDecEq_mixin

(** val coq_HB_unnamed_mixin_8 : nat -> word Choice.Coq_hasChoice.axioms_ **)

let coq_HB_unnamed_mixin_8 nbits =
  (coq_HB_unnamed_factory_3 nbits).Choice.Countable.choice_hasChoice_mixin

(** val mkword : nat -> coq_Z -> word **)

let mkword n z =
  Z.modulo z (modulus n)

(** val urepr : nat -> word -> coq_Z **)

let urepr n w =
  (Obj.magic SubType.phant_on_ (fun x ->
    (&&)
      (Order.Order.le ring_display
        (Num.POrderedZmodule.Exports.join_Num_POrderedZmodule_between_GRing_Nmodule_and_Order_POrder
          coq_BinNums_Z__canonical__Num_POrderedZmodule)
        (GRing.zero
          (Num.POrderedZmodule.Exports.coq_Num_POrderedZmodule__to__GRing_Nmodule
            coq_BinNums_Z__canonical__Num_POrderedZmodule)) (Obj.magic x))
      (Order.Order.lt ring_display coq_BinNums_Z__canonical__Order_POrder
        (Obj.magic x) (Obj.magic modulus n)))
    (word_word__canonical__eqtype_SubType n)).Coq_isSub.val_subdef w

(** val word0 : nat -> word **)

let word0 _ =
  Z0

(** val wsize : nat -> word -> nat **)

let wsize n _ =
  n

(** val add_word : nat -> word -> word -> word **)

let add_word n w1 w2 =
  mkword n (Z.add (urepr n w1) (urepr n w2))

(** val opp_word : nat -> word -> word **)

let opp_word n w =
  mkword n (Z.opp (urepr n w))

(** val mul_word : nat -> word -> word -> word **)

let mul_word n w1 w2 =
  mkword n (Z.mul (urepr n w1) (urepr n w2))

(** val coq_HB_unnamed_factory_10 :
    nat -> word GRing.Coq_isZmodule.axioms_ **)

let coq_HB_unnamed_factory_10 n =
  { GRing.Coq_isZmodule.zero = (word0 n); GRing.Coq_isZmodule.opp =
    (opp_word n); GRing.Coq_isZmodule.add = (add_word n) }

(** val coq_HB_unnamed_mixin_13 : nat -> word GRing.Coq_isNmodule.axioms_ **)

let coq_HB_unnamed_mixin_13 n =
  GRing.Builders_8.coq_HB_unnamed_factory_10 (coq_HB_unnamed_mixin_8 n)
    (coq_HB_unnamed_mixin_7 n) (coq_HB_unnamed_factory_10 n)

(** val word_word__canonical__GRing_Nmodule :
    nat -> GRing.Nmodule.coq_type **)

let word_word__canonical__GRing_Nmodule n =
  { GRing.Nmodule.coq_GRing_isNmodule_mixin =
    (Obj.magic coq_HB_unnamed_mixin_13 n);
    GRing.Nmodule.choice_hasChoice_mixin =
    (Obj.magic coq_HB_unnamed_mixin_8 n);
    GRing.Nmodule.eqtype_hasDecEq_mixin =
    (Obj.magic coq_HB_unnamed_mixin_7 n) }

(** val coq_HB_unnamed_mixin_14 :
    nat -> word GRing.Nmodule_isZmodule.axioms_ **)

let coq_HB_unnamed_mixin_14 n =
  GRing.Builders_8.coq_HB_unnamed_factory_12 (coq_HB_unnamed_mixin_8 n)
    (coq_HB_unnamed_mixin_7 n) (coq_HB_unnamed_factory_10 n)

(** val word1 : nat -> word **)

let word1 _ =
  Zpos Coq_xH

(** val coq_HB_unnamed_factory_30 :
    nat -> word GRing.Zmodule_isComRing.axioms_ **)

let coq_HB_unnamed_factory_30 n =
  { GRing.Zmodule_isComRing.one = (word1 n); GRing.Zmodule_isComRing.mul =
    (mul_word (S n)) }

(** val coq_HB_unnamed_mixin_33 :
    nat -> word GRing.Nmodule_isSemiRing.axioms_ **)

let coq_HB_unnamed_mixin_33 n =
  GRing.Builders_211.coq_GRing_Zmodule_isComRing__to__GRing_Nmodule_isSemiRing
    (coq_HB_unnamed_mixin_13 (S n)) (coq_HB_unnamed_mixin_8 (S n))
    (coq_HB_unnamed_mixin_7 (S n)) (coq_HB_unnamed_mixin_14 (S n))
    (coq_HB_unnamed_factory_30 n)

(** val word_word__canonical__GRing_SemiRing :
    nat -> GRing.SemiRing.coq_type **)

let word_word__canonical__GRing_SemiRing n =
  { GRing.SemiRing.coq_GRing_isNmodule_mixin =
    (Obj.magic coq_HB_unnamed_mixin_13 (S n));
    GRing.SemiRing.choice_hasChoice_mixin =
    (Obj.magic coq_HB_unnamed_mixin_8 (S n));
    GRing.SemiRing.eqtype_hasDecEq_mixin =
    (Obj.magic coq_HB_unnamed_mixin_7 (S n));
    GRing.SemiRing.coq_GRing_Nmodule_isSemiRing_mixin =
    (Obj.magic coq_HB_unnamed_mixin_33 n) }

(** val word_word__canonical__GRing_Ring : nat -> GRing.Ring.coq_type **)

let word_word__canonical__GRing_Ring n =
  { GRing.Ring.coq_GRing_isNmodule_mixin =
    (Obj.magic coq_HB_unnamed_mixin_13 (S n));
    GRing.Ring.choice_hasChoice_mixin =
    (Obj.magic coq_HB_unnamed_mixin_8 (S n));
    GRing.Ring.eqtype_hasDecEq_mixin =
    (Obj.magic coq_HB_unnamed_mixin_7 (S n));
    GRing.Ring.coq_GRing_Nmodule_isSemiRing_mixin =
    (Obj.magic coq_HB_unnamed_mixin_33 n);
    GRing.Ring.coq_GRing_Nmodule_isZmodule_mixin =
    (Obj.magic coq_HB_unnamed_mixin_14 (S n)) }

(** val coq_HB_unnamed_mixin_34 :
    nat -> word GRing.SemiRing_hasCommutativeMul.axioms_ **)

let coq_HB_unnamed_mixin_34 n =
  GRing.Builders_211.coq_GRing_Zmodule_isComRing__to__GRing_SemiRing_hasCommutativeMul
    (coq_HB_unnamed_mixin_13 (S n)) (coq_HB_unnamed_mixin_8 (S n))
    (coq_HB_unnamed_mixin_7 (S n)) (coq_HB_unnamed_mixin_14 (S n))
    (coq_HB_unnamed_factory_30 n)

(** val word_word__canonical__GRing_ComRing :
    nat -> GRing.ComRing.coq_type **)

let word_word__canonical__GRing_ComRing n =
  { GRing.ComRing.coq_GRing_isNmodule_mixin =
    (Obj.magic coq_HB_unnamed_mixin_13 (S n));
    GRing.ComRing.choice_hasChoice_mixin =
    (Obj.magic coq_HB_unnamed_mixin_8 (S n));
    GRing.ComRing.eqtype_hasDecEq_mixin =
    (Obj.magic coq_HB_unnamed_mixin_7 (S n));
    GRing.ComRing.coq_GRing_Nmodule_isZmodule_mixin =
    (Obj.magic coq_HB_unnamed_mixin_14 (S n));
    GRing.ComRing.coq_GRing_Nmodule_isSemiRing_mixin =
    (Obj.magic coq_HB_unnamed_mixin_33 n);
    GRing.ComRing.coq_GRing_SemiRing_hasCommutativeMul_mixin =
    (Obj.magic coq_HB_unnamed_mixin_34 n) }

(** val wbit : coq_Z -> nat -> bool **)

let wbit z n =
  Z.testbit z (Z.of_nat n)

(** val w2t : nat -> word -> bool tuple_of **)

let w2t n w =
  mktuple n (fun k -> wbit w (nat_of_ord n k))

(** val t2w_def : nat -> bool tuple_of -> coq_Z **)

let t2w_def n t =
  Coq_bigop.body
    (Obj.magic GRing.zero
      (GRing.SemiRing.Exports.coq_GRing_SemiRing__to__GRing_Nmodule
        coq_BinNums_Z__canonical__GRing_SemiRing))
    (Obj.magic index_enum (fintype_ordinal__canonical__fintype_Finite n))
    (fun i -> BigBody (i,
    (Obj.magic GRing.add
      (GRing.SemiRing.Exports.coq_GRing_SemiRing__to__GRing_Nmodule
        coq_BinNums_Z__canonical__GRing_SemiRing)), true,
    (Obj.magic GRing.mul coq_BinNums_Z__canonical__GRing_SemiRing
      (GRing.exp coq_BinNums_Z__canonical__GRing_SemiRing
        (GRing.natmul
          (GRing.SemiRing.Exports.coq_GRing_SemiRing__to__GRing_Nmodule
            coq_BinNums_Z__canonical__GRing_SemiRing)
          (GRing.one coq_BinNums_Z__canonical__GRing_SemiRing) (S (S O)))
        (nat_of_ord n i))
      (GRing.natmul
        (GRing.SemiRing.Exports.coq_GRing_SemiRing__to__GRing_Nmodule
          coq_BinNums_Z__canonical__GRing_SemiRing)
        (GRing.one coq_BinNums_Z__canonical__GRing_SemiRing)
        (nat_of_bool (tnth n t i))))))

(** val t2w : nat -> bool tuple_of -> word **)

let t2w =
  t2w_def

(** val srepr : nat -> word -> GRing.Nmodule.sort **)

let srepr n w =
  if wbit w (pred (wsize n w))
  then GRing.add coq_BinNums_Z__canonical__GRing_Nmodule
         ((Obj.magic SubType.phant_on_ (fun x ->
            (&&)
              (Order.Order.le ring_display
                (Num.POrderedZmodule.Exports.join_Num_POrderedZmodule_between_GRing_Nmodule_and_Order_POrder
                  coq_BinNums_Z__canonical__Num_POrderedZmodule)
                (GRing.zero
                  (Num.POrderedZmodule.Exports.coq_Num_POrderedZmodule__to__GRing_Nmodule
                    coq_BinNums_Z__canonical__Num_POrderedZmodule))
                (Obj.magic x))
              (Order.Order.lt ring_display
                coq_BinNums_Z__canonical__Order_POrder (Obj.magic x)
                (Obj.magic modulus n)))
            (word_word__canonical__eqtype_SubType n)).Coq_isSub.val_subdef w)
         (GRing.opp coq_BinNums_Z__canonical__GRing_Zmodule
           (Obj.magic modulus n))
  else (Obj.magic SubType.phant_on_ (fun x ->
         (&&)
           (Order.Order.le ring_display
             (Num.POrderedZmodule.Exports.join_Num_POrderedZmodule_between_GRing_Nmodule_and_Order_POrder
               coq_BinNums_Z__canonical__Num_POrderedZmodule)
             (GRing.zero
               (Num.POrderedZmodule.Exports.coq_Num_POrderedZmodule__to__GRing_Nmodule
                 coq_BinNums_Z__canonical__Num_POrderedZmodule))
             (Obj.magic x))
           (Order.Order.lt ring_display
             coq_BinNums_Z__canonical__Order_POrder (Obj.magic x)
             (Obj.magic modulus n))) (word_word__canonical__eqtype_SubType n)).Coq_isSub.val_subdef
         w

(** val wand : nat -> word -> word -> word **)

let wand _ =
  Z.coq_land

(** val wor : nat -> word -> word -> word **)

let wor _ =
  Z.coq_lor

(** val wxor : nat -> word -> word -> word **)

let wxor _ =
  Z.coq_lxor

(** val shiftr_nat : coq_Z -> nat -> coq_Z **)

let shiftr_nat a n =
  Nat.iter n Z.div2 a

(** val coq_lsr : nat -> word -> nat -> word **)

let coq_lsr n w k =
  shiftr_nat (urepr n w) k

(** val rotl : nat -> word -> nat -> word **)

let rotl n w k =
  t2w n
    (mktuple n (fun i ->
      wbit w (modn (addn (nat_of_ord n i) (subn n (modn k n))) n)))

(** val rotr : nat -> word -> nat -> word **)

let rotr n w k =
  t2w n (mktuple n (fun i -> wbit w (modn (addn (nat_of_ord n i) k) n)))

(** val subword : nat -> nat -> nat -> word -> word **)

let subword n i l w =
  mkword l (coq_lsr n w i)

(** val wcat_r : nat -> word list -> coq_Z **)

let rec wcat_r n = function
| [] -> Z0
| w :: s0 ->
  Z.coq_lor (urepr n w) (Z.shiftl (wcat_r n s0) (int_to_Z (Posz n)))

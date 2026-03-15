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

val modulus : nat -> coq_Z

type word = coq_Z
  (* singleton inductive, whose constructor was mkWord *)

val coq_HB_unnamed_factory_1 : nat -> (coq_Z, word) Coq_isSub.axioms_

val word_word__canonical__eqtype_SubType : nat -> coq_Z SubType.coq_type

val coq_HB_unnamed_factory_3 : nat -> word Choice.Countable.axioms_

val coq_HB_unnamed_mixin_7 : nat -> word Coq_hasDecEq.axioms_

val coq_HB_unnamed_mixin_8 : nat -> word Choice.Coq_hasChoice.axioms_

val mkword : nat -> coq_Z -> word

val urepr : nat -> word -> coq_Z

val word0 : nat -> word

val wsize : nat -> word -> nat

val add_word : nat -> word -> word -> word

val opp_word : nat -> word -> word

val mul_word : nat -> word -> word -> word

val coq_HB_unnamed_factory_10 : nat -> word GRing.Coq_isZmodule.axioms_

val coq_HB_unnamed_mixin_13 : nat -> word GRing.Coq_isNmodule.axioms_

val word_word__canonical__GRing_Nmodule : nat -> GRing.Nmodule.coq_type

val coq_HB_unnamed_mixin_14 : nat -> word GRing.Nmodule_isZmodule.axioms_

val word1 : nat -> word

val coq_HB_unnamed_factory_30 : nat -> word GRing.Zmodule_isComRing.axioms_

val coq_HB_unnamed_mixin_33 : nat -> word GRing.Nmodule_isSemiRing.axioms_

val word_word__canonical__GRing_SemiRing : nat -> GRing.SemiRing.coq_type

val word_word__canonical__GRing_Ring : nat -> GRing.Ring.coq_type

val coq_HB_unnamed_mixin_34 :
  nat -> word GRing.SemiRing_hasCommutativeMul.axioms_

val word_word__canonical__GRing_ComRing : nat -> GRing.ComRing.coq_type

val wbit : coq_Z -> nat -> bool

val w2t : nat -> word -> bool tuple_of

val t2w_def : nat -> bool tuple_of -> coq_Z

val t2w : nat -> bool tuple_of -> word

val srepr : nat -> word -> GRing.Nmodule.sort

val wand : nat -> word -> word -> word

val wor : nat -> word -> word -> word

val wxor : nat -> word -> word -> word

val shiftr_nat : coq_Z -> nat -> coq_Z

val coq_lsr : nat -> word -> nat -> word

val rotl : nat -> word -> nat -> word

val rotr : nat -> word -> nat -> word

val subword : nat -> nat -> nat -> word -> word

val wcat_r : nat -> word list -> coq_Z

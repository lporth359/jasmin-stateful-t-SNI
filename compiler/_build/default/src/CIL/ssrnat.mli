open Bool
open Datatypes
open Nat0
open Eqtype
open Ssrbool

val eqn : nat -> nat -> bool

val eqnP : nat eq_axiom

val coq_HB_unnamed_factory_1 : nat Coq_hasDecEq.axioms_

val coq_Datatypes_nat__canonical__eqtype_Equality : Equality.coq_type

val addn_rec : nat -> nat -> nat

val addn : nat -> nat -> nat

val subn_rec : nat -> nat -> nat

val subn : nat -> nat -> nat

val leq : nat -> nat -> bool

val iter : nat -> ('a1 -> 'a1) -> 'a1 -> 'a1

val iteri : nat -> (nat -> 'a1 -> 'a1) -> 'a1 -> 'a1

val iterop : nat -> ('a1 -> 'a1 -> 'a1) -> 'a1 -> 'a1 -> 'a1

val muln_rec : nat -> nat -> nat

val muln : nat -> nat -> nat

val expn_rec : nat -> nat -> nat

val expn : nat -> nat -> nat

val nat_of_bool : bool -> nat

val double_rec : nat -> nat

val double : nat -> nat

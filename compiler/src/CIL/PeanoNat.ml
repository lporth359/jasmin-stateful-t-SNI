open Datatypes

module Nat =
 struct
  (** val leb : nat -> nat -> bool **)

  let rec leb n m =
    match n with
    | O -> true
    | S n' -> (match m with
               | O -> false
               | S m' -> leb n' m')

  (** val iter : nat -> ('a1 -> 'a1) -> 'a1 -> 'a1 **)

  let rec iter n f x =
    match n with
    | O -> x
    | S n0 -> f (iter n0 f x)
 end

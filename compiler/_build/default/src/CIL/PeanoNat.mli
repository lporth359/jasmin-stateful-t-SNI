open Datatypes

module Nat :
 sig
  val leb : nat -> nat -> bool

  val iter : nat -> ('a1 -> 'a1) -> 'a1 -> 'a1
 end

open BinNums
open Bool
open Ssrbool

val iffP2 : ('a1 -> 'a1 -> bool) -> 'a1 -> 'a1 -> reflect

val pos_eq_dec : positive -> positive -> bool

val eqb_body :
  ('a1 -> positive) -> ('a1 -> 'a3) -> (positive -> 'a2 -> 'a3 -> bool) ->
  positive -> 'a2 -> 'a1 -> bool

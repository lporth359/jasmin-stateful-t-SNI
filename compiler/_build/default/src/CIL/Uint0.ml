open PrimInt63

(** val succ : Uint63.t -> Uint63.t **)

let succ i =
  add i (Uint63.of_int (1))

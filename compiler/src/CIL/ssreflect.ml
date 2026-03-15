
(** val locked_with : unit -> 'a1 -> 'a1 **)

let locked_with _ x =
  x

(** val ssr_have_upoly : 'a1 -> ('a1 -> 'a2) -> 'a2 **)

let ssr_have_upoly step rest =
  rest step

open Bool

(** val reflect_dec :
    ('a1 -> 'a1 -> bool) -> ('a1 -> 'a1 -> reflect) -> 'a1 -> 'a1 -> bool **)

let reflect_dec _ h x y =
  let r = h x y in (match r with
                    | ReflectT -> true
                    | ReflectF -> false)

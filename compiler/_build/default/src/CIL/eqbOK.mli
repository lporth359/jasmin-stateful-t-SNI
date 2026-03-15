open Bool

val reflect_dec :
  ('a1 -> 'a1 -> bool) -> ('a1 -> 'a1 -> reflect) -> 'a1 -> 'a1 -> bool

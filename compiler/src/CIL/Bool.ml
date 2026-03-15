
type reflect =
| ReflectT
| ReflectF

(** val iff_reflect : bool -> reflect **)

let iff_reflect = function
| true -> ReflectT
| false -> ReflectF

(** val reflect_dec : bool -> reflect -> bool **)

let reflect_dec _ = function
| ReflectT -> true
| ReflectF -> false

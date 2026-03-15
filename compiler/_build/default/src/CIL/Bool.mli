
type reflect =
| ReflectT
| ReflectF

val iff_reflect : bool -> reflect

val reflect_dec : bool -> reflect -> bool

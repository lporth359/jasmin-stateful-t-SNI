open BinNums
open Bool
open Ssrbool

(** val iffP2 : ('a1 -> 'a1 -> bool) -> 'a1 -> 'a1 -> reflect **)

let iffP2 f x1 x2 =
  iffP (f x1 x2) (if f x1 x2 then ReflectT else ReflectF)

(** val pos_eq_dec : positive -> positive -> bool **)

let rec pos_eq_dec p x0 =
  match p with
  | Coq_xI p0 -> (match x0 with
                  | Coq_xI p1 -> pos_eq_dec p0 p1
                  | _ -> false)
  | Coq_xO p0 -> (match x0 with
                  | Coq_xO p1 -> pos_eq_dec p0 p1
                  | _ -> false)
  | Coq_xH -> (match x0 with
               | Coq_xH -> true
               | _ -> false)

(** val eqb_body :
    ('a1 -> positive) -> ('a1 -> 'a3) -> (positive -> 'a2 -> 'a3 -> bool) ->
    positive -> 'a2 -> 'a1 -> bool **)

let eqb_body tagB fieldsB eqb_fields t1 f1 x2 =
  let t2 = tagB x2 in
  if pos_eq_dec t2 t1
  then let f2 = fieldsB x2 in eqb_fields t1 f1 f2
  else false

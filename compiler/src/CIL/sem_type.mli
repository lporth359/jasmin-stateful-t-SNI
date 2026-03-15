open Datatypes
open Eqtype
open Seq
open Type
open Utils0

type __ = Obj.t

type coq_WithSubWord =
  bool
  (* singleton inductive, whose constructor was Build_WithSubWord *)

val sw_allowed : coq_WithSubWord -> bool

val nosubword : coq_WithSubWord

val withsubword : coq_WithSubWord

val compat_atype : bool -> atype -> atype -> bool

val compat_ctype : bool -> ctype -> ctype -> bool

type sem_t = __

type 'tr sem_prod = 'tr lprod

type sem_ot = __

type sem_tuple = ltuple

val sem_prod_ok : ctype list -> 'a1 sem_prod -> 'a1 exec sem_prod

val curry : ctype -> nat -> (sem_t list -> 'a1) -> 'a1 sem_prod

val sem_prod_const : ctype list -> 'a1 -> 'a1 sem_prod

val sem_prod_id : ctype -> sem_t -> sem_ot

val sem_prod_app : ctype list -> 'a1 sem_prod -> ('a1 -> 'a2) -> 'a2 sem_prod

val add_tuple : __ list -> 'a1 -> ltuple -> ltuple

val sem_prod_tuple : ctype list -> sem_tuple sem_prod

val app_sopn :
  (ctype -> 'a1 -> sem_t exec) -> ctype list -> 'a2 exec sem_prod -> 'a1 list
  -> 'a2 exec

open BinNums
open Datatypes
open Eqtype
open Sem_type
open Ssralg
open Type
open Utils0
open Warray_
open Word0
open Wsize

val undef_t : ctype -> ctype

type value =
| Vbool of bool
| Vint of coq_Z
| Varr of positive * WArray.array
| Vword of wsize * GRing.ComRing.sort
| Vundef of ctype

val undef_v : ctype -> value

val undef_addr : ctype -> value

type values = value list

val is_defined : value -> bool

val type_of_val : value -> ctype

val coq_DB : bool -> value -> bool

val to_bool : value -> (error, bool) result

val to_int : value -> (error, coq_Z) result

val to_arr : positive -> value -> sem_t exec

val to_word : wsize -> value -> GRing.ComRing.sort exec

val of_val : ctype -> value -> sem_t exec

val to_val : ctype -> sem_t -> value

val oto_val : ctype -> sem_ot -> value

val truncate_val : ctype -> value -> value exec

val list_ltuple : ctype list -> sem_tuple -> values

val app_sopn : ctype list -> 'a1 exec sem_prod -> value list -> 'a1 exec

val swap_semi : ctype -> sem_t -> sem_t -> sem_tuple

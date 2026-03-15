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

(** val undef_t : ctype -> ctype **)

let undef_t t =
  if OtherDefs.is_cword t then Coq_cword U8 else t

type value =
| Vbool of bool
| Vint of coq_Z
| Varr of positive * WArray.array
| Vword of wsize * GRing.ComRing.sort
| Vundef of ctype

(** val undef_v : ctype -> value **)

let undef_v t =
  Vundef (undef_t t)

(** val undef_addr : ctype -> value **)

let undef_addr = function
| Coq_carr n -> Varr (n, (WArray.empty n))
| x -> undef_v x

type values = value list

(** val is_defined : value -> bool **)

let is_defined = function
| Vundef _ -> false
| _ -> true

(** val type_of_val : value -> ctype **)

let type_of_val = function
| Vbool _ -> Coq_cbool
| Vint _ -> Coq_cint
| Varr (n, _) -> Coq_carr n
| Vword (s, _) -> Coq_cword s
| Vundef t -> t

(** val coq_DB : bool -> value -> bool **)

let coq_DB wdb v =
  (||) (negb wdb)
    ((||) (is_defined v)
      (eq_op type_ctype__canonical__eqtype_Equality (Obj.magic type_of_val v)
        (Obj.magic Coq_cbool)))

(** val to_bool : value -> (error, bool) result **)

let to_bool = function
| Vbool b -> Ok b
| Vundef t -> (match t with
               | Coq_cbool -> undef_error
               | _ -> type_error)
| _ -> type_error

(** val to_int : value -> (error, coq_Z) result **)

let to_int = function
| Vint i -> Ok i
| Vundef t -> (match t with
               | Coq_cint -> undef_error
               | _ -> type_error)
| _ -> type_error

(** val to_arr : positive -> value -> sem_t exec **)

let to_arr len = function
| Varr (len', t) -> Obj.magic WArray.cast len' len t
| _ -> type_error

(** val to_word : wsize -> value -> GRing.ComRing.sort exec **)

let to_word s = function
| Vword (s', w) -> truncate_word s s' w
| Vundef t -> (match t with
               | Coq_cword _ -> undef_error
               | _ -> type_error)
| _ -> type_error

(** val of_val : ctype -> value -> sem_t exec **)

let of_val = function
| Coq_cbool -> Obj.magic to_bool
| Coq_cint -> Obj.magic to_int
| Coq_carr n -> to_arr n
| Coq_cword s -> to_word s

(** val to_val : ctype -> sem_t -> value **)

let to_val = function
| Coq_cbool -> Obj.magic (fun x -> Vbool x)
| Coq_cint -> Obj.magic (fun x -> Vint x)
| Coq_carr n -> Obj.magic (fun x -> Varr (n, x))
| Coq_cword s -> (fun x -> Vword (s, x))

(** val oto_val : ctype -> sem_ot -> value **)

let oto_val = function
| Coq_cbool ->
  (fun ob ->
    match Obj.magic ob with
    | Some b -> Vbool b
    | None -> Vundef Coq_cbool)
| x -> to_val x

(** val truncate_val : ctype -> value -> value exec **)

let truncate_val ty v =
  match of_val ty v with
  | Ok x -> Ok (to_val ty x)
  | Error s -> Error s

(** val list_ltuple : ctype list -> sem_tuple -> values **)

let rec list_ltuple = function
| [] -> (fun _ -> [])
| t :: ts0 ->
  let rec0 = list_ltuple ts0 in
  (fun x ->
  match ts0 with
  | [] -> (oto_val t x) :: []
  | _ :: _ -> (oto_val t (fst (Obj.magic x))) :: (rec0 (snd (Obj.magic x))))

(** val app_sopn :
    ctype list -> 'a1 exec sem_prod -> value list -> 'a1 exec **)

let app_sopn ts x x0 =
  app_sopn of_val ts x x0

(** val swap_semi : ctype -> sem_t -> sem_t -> sem_tuple **)

let swap_semi ty x y =
  Obj.magic ((sem_prod_id ty y), (sem_prod_id ty x))

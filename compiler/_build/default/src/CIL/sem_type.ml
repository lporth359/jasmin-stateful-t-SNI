open Datatypes
open Eqtype
open Seq
open Type
open Utils0

type __ = Obj.t
let __ = let rec f _ = Obj.repr f in Obj.repr f

type coq_WithSubWord =
  bool
  (* singleton inductive, whose constructor was Build_WithSubWord *)

(** val sw_allowed : coq_WithSubWord -> bool **)

let sw_allowed withSubWord =
  withSubWord

(** val nosubword : coq_WithSubWord **)

let nosubword =
  false

(** val withsubword : coq_WithSubWord **)

let withsubword =
  true

(** val compat_atype : bool -> atype -> atype -> bool **)

let compat_atype = function
| true -> subatype
| false -> convertible

(** val compat_ctype : bool -> ctype -> ctype -> bool **)

let compat_ctype = function
| true -> subctype
| false -> Obj.magic eq_op type_ctype__canonical__eqtype_Equality

type sem_t = __

type 'tr sem_prod = 'tr lprod

type sem_ot = __

type sem_tuple = ltuple

(** val sem_prod_ok : ctype list -> 'a1 sem_prod -> 'a1 exec sem_prod **)

let rec sem_prod_ok tin o =
  match tin with
  | [] -> Obj.magic (Ok o)
  | _ :: ts -> Obj.magic (fun v -> sem_prod_ok ts (Obj.magic o v))

(** val curry : ctype -> nat -> (sem_t list -> 'a1) -> 'a1 sem_prod **)

let curry _ n f =
  let rec loop = function
  | O -> Obj.magic f
  | S n' -> (fun acc -> Obj.magic (fun a -> loop n' (a :: acc)))
  in loop n []

(** val sem_prod_const : ctype list -> 'a1 -> 'a1 sem_prod **)

let rec sem_prod_const lt a =
  match lt with
  | [] -> Obj.magic a
  | _ :: lt' -> Obj.magic (fun _ -> sem_prod_const lt' a)

(** val sem_prod_id : ctype -> sem_t -> sem_ot **)

let sem_prod_id = function
| Coq_cbool -> Obj.magic (fun x -> Some x)
| _ -> (fun x -> x)

(** val sem_prod_app :
    ctype list -> 'a1 sem_prod -> ('a1 -> 'a2) -> 'a2 sem_prod **)

let rec sem_prod_app lt a g =
  match lt with
  | [] -> Obj.magic g a
  | _ :: lt' -> Obj.magic (fun x -> sem_prod_app lt' (Obj.magic a x) g)

(** val add_tuple : __ list -> 'a1 -> ltuple -> ltuple **)

let add_tuple ts x xs =
  match ts with
  | [] -> Obj.magic x
  | _ :: _ -> Obj.magic (x, xs)

(** val sem_prod_tuple : ctype list -> sem_tuple sem_prod **)

let rec sem_prod_tuple = function
| [] -> Obj.magic ()
| t :: lt' ->
  Obj.magic (fun v ->
    sem_prod_app lt' (sem_prod_tuple lt') (fun xs ->
      add_tuple (map (Obj.magic __) lt') (sem_prod_id t v) xs))

(** val app_sopn :
    (ctype -> 'a1 -> sem_t exec) -> ctype list -> 'a2 exec sem_prod -> 'a1
    list -> 'a2 exec **)

let rec app_sopn of_T ts x x0 =
  match ts with
  | [] -> (match x0 with
           | [] -> Obj.magic x
           | _ :: _ -> type_error)
  | t :: ts0 ->
    (match x0 with
     | [] -> type_error
     | v :: vs ->
       (match of_T t v with
        | Ok x1 -> app_sopn of_T ts0 (Obj.magic x x1) vs
        | Error s -> Error s))

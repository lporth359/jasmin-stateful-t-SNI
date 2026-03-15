open Datatypes
open Fintype
open Seq
open Ssrbool

type __ = Obj.t
let __ = let rec f _ = Obj.repr f in Obj.repr f

type 't tuple_of =
  't list
  (* singleton inductive, whose constructor was Tuple *)

(** val tnth_default : nat -> 'a1 tuple_of -> ordinal -> 'a1 **)

let tnth_default _ t =
  let _evar_0_ = fun _ -> assert false (* absurd case *) in
  let _evar_0_0 = fun a _ _ -> a in
  (match t with
   | [] -> _evar_0_
   | a :: l -> _evar_0_0 a l)

(** val tnth : nat -> 'a1 tuple_of -> ordinal -> 'a1 **)

let tnth n t i =
  nth (tnth_default n t i) t (nat_of_ord n i)

(** val tuple :
    nat -> 'a1 tuple_of -> (__ -> 'a1 tuple_of) -> 'a1 tuple_of **)

let tuple _ _ mkT =
  mkT __

(** val cons_tuple : nat -> 'a1 -> 'a1 tuple_of -> 'a1 tuple_of **)

let cons_tuple _ x t =
  x :: t

(** val in_tuple : 'a1 list -> 'a1 tuple_of **)

let in_tuple s =
  s

(** val iota_tuple : nat -> nat -> nat tuple_of **)

let iota_tuple n m =
  iota m n

(** val behead_tuple : nat -> 'a1 tuple_of -> 'a1 tuple_of **)

let behead_tuple _ =
  behead

(** val map_tuple : nat -> ('a1 -> 'a2) -> 'a1 tuple_of -> 'a2 tuple_of **)

let map_tuple _ =
  map

(** val ord_tuple : nat -> ordinal tuple_of **)

let ord_tuple n =
  Obj.magic enum_mem (fintype_ordinal__canonical__fintype_Finite n)
    (mem predPredType
      (Obj.magic PredOfSimpl.coerce (coq_SimplPred (fun _ -> true))))

(** val mktuple : nat -> (ordinal -> 'a1) -> 'a1 tuple_of **)

let mktuple n f =
  map_tuple n f (ord_tuple n)

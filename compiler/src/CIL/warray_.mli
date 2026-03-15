open BinInt
open BinNums
open BinPos
open Bool
open Datatypes
open EqbOK
open Eqb_core_defs
open Eqtype
open Gen_map
open Memory_model
open Seq
open Ssralg
open Ssrbool
open Ssrfun
open Ssrnat
open Type
open Utils0
open Word0
open Word_ssrZ
open Wsize

type __ = Obj.t

type arr_access =
| AAdirect
| AAscale

val arr_access_rect : 'a1 -> 'a1 -> arr_access -> 'a1

val arr_access_rec : 'a1 -> 'a1 -> arr_access -> 'a1

type is_arr_access =
| Coq_is_AAdirect
| Coq_is_AAscale

val is_arr_access_rect : 'a1 -> 'a1 -> arr_access -> is_arr_access -> 'a1

val is_arr_access_rec : 'a1 -> 'a1 -> arr_access -> is_arr_access -> 'a1

val arr_access_tag : arr_access -> positive

val is_arr_access_inhab : arr_access -> is_arr_access

val is_arr_access_functor : arr_access -> is_arr_access -> is_arr_access

type box_arr_access_AAdirect =
| Box_arr_access_AAdirect

type arr_access_fields_t = __

val arr_access_fields : arr_access -> arr_access_fields_t

val arr_access_construct :
  positive -> arr_access_fields_t -> arr_access option

val arr_access_induction : 'a1 -> 'a1 -> arr_access -> is_arr_access -> 'a1

val arr_access_eqb_fields :
  (arr_access -> arr_access -> bool) -> positive -> arr_access_fields_t ->
  arr_access_fields_t -> bool

val arr_access_eqb : arr_access -> arr_access -> bool

val arr_access_eqb_OK : arr_access -> arr_access -> reflect

val arr_access_eqb_OK_sumbool : arr_access -> arr_access -> bool

val coq_HB_unnamed_factory_1 : arr_access Coq_hasDecEq.axioms_

val warray__arr_access__canonical__eqtype_Equality : Equality.coq_type

val mk_scale : arr_access -> wsize -> coq_Z

module WArray :
 sig
  type array =
    GRing.ComRing.sort Mz.t
    (* singleton inductive, whose constructor was Build_array *)

  val arr_data : positive -> array -> GRing.ComRing.sort Mz.t

  val empty : positive -> array

  val coq_PointerZ : pointer_op

  val in_bound : positive -> array -> coq_Z -> bool

  val in_boundP : positive -> array -> coq_Z -> reflect

  val is_init : positive -> array -> coq_Z -> bool

  val get8 : positive -> array -> coq_Z -> (error, GRing.Nmodule.sort) result

  val set8 :
    positive -> array -> coq_Z -> GRing.ComRing.sort -> (error, array) result

  val valid8P : positive -> array -> coq_Z -> GRing.ComRing.sort -> reflect

  val array_CM : positive -> array coreMem

  val in_range : positive -> coq_Z -> wsize -> bool

  val in_rangeP : positive -> coq_Z -> wsize -> reflect

  val get :
    positive -> aligned -> arr_access -> wsize -> array -> coq_Z ->
    GRing.ComRing.sort exec

  val set :
    positive -> wsize -> array -> aligned -> arr_access -> coq_Z ->
    GRing.ComRing.sort -> array exec

  val fcopy :
    wsize -> positive -> array -> array -> coq_Z -> coq_Z -> (error, array)
    result

  val copy : wsize -> positive -> array -> (error, array) result

  val fill : positive -> GRing.ComRing.sort list -> array exec

  val get_sub_data :
    arr_access -> wsize -> positive -> GRing.ComRing.sort Mz.t -> coq_Z ->
    GRing.ComRing.sort Mz.t

  val get_sub :
    positive -> arr_access -> wsize -> positive -> array -> coq_Z -> array
    exec

  val set_sub_data :
    arr_access -> wsize -> positive -> GRing.ComRing.sort Mz.t -> coq_Z ->
    GRing.ComRing.sort Mz.t -> GRing.ComRing.sort Mz.t

  val set_sub :
    positive -> arr_access -> wsize -> positive -> array -> coq_Z -> array ->
    array exec

  val cast : positive -> positive -> array -> (error, array) result

  val of_list : wsize -> GRing.ComRing.sort list -> array
 end

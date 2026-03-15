open BinNums
open Bool
open Datatypes
open Eqb_core_defs
open Param1

type __ = Obj.t

module Prelude :
 sig
  val bool_tag : bool -> positive

  type box_bool_true =
  | Box_bool_true

  type bool_fields_t = __

  val bool_fields : bool -> bool_fields_t

  val bool_eqb_fields :
    (bool -> bool -> bool) -> positive -> bool_fields_t -> bool_fields_t ->
    bool

  val bool_eqb : bool -> bool -> bool

  type ('a, 'pA) is_option =
  | Coq_is_Some of 'a * 'pA
  | Coq_is_None

  val option_tag : 'a1 option -> positive

  val is_option_inhab : ('a1, 'a2) full -> 'a1 option -> ('a1, 'a2) is_option

  type 'a box_option_None =
  | Box_option_None

  type 'p option_fields_t = __

  val option_fields : 'a1 option -> 'a1 option_fields_t

  val option_eqb_fields :
    ('a1 -> 'a1 -> bool) -> ('a1 option -> 'a1 option -> bool) -> positive ->
    'a1 option_fields_t -> 'a1 option_fields_t -> bool

  val option_eqb : ('a1 -> 'a1 -> bool) -> 'a1 option -> 'a1 option -> bool

  val list_tag : 'a1 list -> positive

  type 'a box_list_nil =
  | Box_list_nil

  type 'a box_list_cons = { coq_Box_list_cons_0 : 'a;
                            coq_Box_list_cons_1 : 'a list }

  type 'p list_fields_t = __

  val list_fields : 'a1 list -> 'a1 list_fields_t

  val list_eqb_fields :
    ('a1 -> 'a1 -> bool) -> ('a1 list -> 'a1 list -> bool) -> positive -> 'a1
    list_fields_t -> 'a1 list_fields_t -> bool

  val list_eqb : ('a1 -> 'a1 -> bool) -> 'a1 list -> 'a1 list -> bool

  val comparison_tag : comparison -> positive

  type box_comparison_Eq =
  | Box_comparison_Eq

  type comparison_fields_t = __

  val comparison_fields : comparison -> comparison_fields_t

  val comparison_eqb_fields :
    (comparison -> comparison -> bool) -> positive -> comparison_fields_t ->
    comparison_fields_t -> bool

  val comparison_eqb : comparison -> comparison -> bool

  val comparison_eqb_OK : comparison -> comparison -> reflect
 end

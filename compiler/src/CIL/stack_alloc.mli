open BinInt
open BinNums
open Bool
open Datatypes
open Prelude
open PrimInt63
open Uint0
open Compiler_util
open Eqtype
open Expr
open Gen_map
open Global
open Memory_model
open Pseudo_operator
open Seq
open Slh_lowering
open Slh_ops
open Sopn
open Ssralg
open Ssrbool
open Ssrfun
open Ssrnat
open Stack_alloc_params
open Type
open Utils0
open Var0
open Warray_
open Word0
open Word_ssrZ
open Wsize
open Xseq

type __ = Obj.t

module E :
 sig
  val pass : string

  val stk_error_gen : bool -> var_info -> pp_error -> pp_error_loc

  val stk_error : var_i -> pp_error -> pp_error_loc

  val stk_ierror : var_i -> pp_error -> pp_error_loc

  val stk_ierror_basic : var_i -> string -> pp_error_loc

  val stk_ierror_basic_lv : lval -> string -> pp_error_loc

  val stk_error_no_var_gen : bool -> pp_error -> pp_error_loc

  val stk_error_no_var_box : pp_error -> pp_error_loc

  val stk_error_no_var : string -> pp_error_loc

  val stk_ierror_no_var : string -> pp_error_loc
 end

val size_of : atype -> coq_Z

type slot = Var.var

type region = { r_slot : slot; r_align : wsize; r_writable : bool }

val region_beq : region -> region -> bool

val region_same : region -> region -> bool

val region_axiom : region eq_axiom

val coq_HB_unnamed_factory_1 : region Coq_hasDecEq.axioms_

val stack_alloc_region__canonical__eqtype_Equality : Equality.coq_type

module CmpR :
 sig
  val t : Equality.coq_type

  val cmp : Equality.sort -> Equality.sort -> comparison
 end

module Mr :
 sig
  module K :
   sig
    val t : Equality.coq_type

    val cmp : Equality.sort -> Equality.sort -> comparison
   end

  module Ordered :
   sig
    type t = Equality.sort

    val compare :
      Equality.sort -> Equality.sort -> Equality.sort OrderedType.coq_Compare

    val eq_dec : Equality.sort -> Equality.sort -> bool
   end

  module Map :
   sig
    module E :
     sig
      type t = Equality.sort

      val compare :
        Equality.sort -> Equality.sort -> Equality.sort
        OrderedType.coq_Compare

      val eq_dec : Equality.sort -> Equality.sort -> bool
     end

    module Raw :
     sig
      type key = Equality.sort

      type 'elt tree =
      | Leaf
      | Node of 'elt tree * key * 'elt * 'elt tree * Int.Z_as_Int.t

      val tree_rect :
        'a2 -> ('a1 tree -> 'a2 -> key -> 'a1 -> 'a1 tree -> 'a2 ->
        Int.Z_as_Int.t -> 'a2) -> 'a1 tree -> 'a2

      val tree_rec :
        'a2 -> ('a1 tree -> 'a2 -> key -> 'a1 -> 'a1 tree -> 'a2 ->
        Int.Z_as_Int.t -> 'a2) -> 'a1 tree -> 'a2

      val height : 'a1 tree -> Int.Z_as_Int.t

      val cardinal : 'a1 tree -> nat

      val empty : 'a1 tree

      val is_empty : 'a1 tree -> bool

      val mem : Equality.sort -> 'a1 tree -> bool

      val find : Equality.sort -> 'a1 tree -> 'a1 option

      val create : 'a1 tree -> key -> 'a1 -> 'a1 tree -> 'a1 tree

      val assert_false : 'a1 tree -> key -> 'a1 -> 'a1 tree -> 'a1 tree

      val bal : 'a1 tree -> key -> 'a1 -> 'a1 tree -> 'a1 tree

      val add : key -> 'a1 -> 'a1 tree -> 'a1 tree

      val remove_min :
        'a1 tree -> key -> 'a1 -> 'a1 tree -> 'a1 tree * (key * 'a1)

      val merge : 'a1 tree -> 'a1 tree -> 'a1 tree

      val remove : Equality.sort -> 'a1 tree -> 'a1 tree

      val join : 'a1 tree -> key -> 'a1 -> 'a1 tree -> 'a1 tree

      type 'elt triple = { t_left : 'elt tree; t_opt : 'elt option;
                           t_right : 'elt tree }

      val t_left : 'a1 triple -> 'a1 tree

      val t_opt : 'a1 triple -> 'a1 option

      val t_right : 'a1 triple -> 'a1 tree

      val split : Equality.sort -> 'a1 tree -> 'a1 triple

      val concat : 'a1 tree -> 'a1 tree -> 'a1 tree

      val elements_aux : (key * 'a1) list -> 'a1 tree -> (key * 'a1) list

      val elements : 'a1 tree -> (key * 'a1) list

      val fold : (key -> 'a1 -> 'a2 -> 'a2) -> 'a1 tree -> 'a2 -> 'a2

      type 'elt enumeration =
      | End
      | More of key * 'elt * 'elt tree * 'elt enumeration

      val enumeration_rect :
        'a2 -> (key -> 'a1 -> 'a1 tree -> 'a1 enumeration -> 'a2 -> 'a2) ->
        'a1 enumeration -> 'a2

      val enumeration_rec :
        'a2 -> (key -> 'a1 -> 'a1 tree -> 'a1 enumeration -> 'a2 -> 'a2) ->
        'a1 enumeration -> 'a2

      val cons : 'a1 tree -> 'a1 enumeration -> 'a1 enumeration

      val equal_more :
        ('a1 -> 'a1 -> bool) -> Equality.sort -> 'a1 -> ('a1 enumeration ->
        bool) -> 'a1 enumeration -> bool

      val equal_cont :
        ('a1 -> 'a1 -> bool) -> 'a1 tree -> ('a1 enumeration -> bool) -> 'a1
        enumeration -> bool

      val equal_end : 'a1 enumeration -> bool

      val equal : ('a1 -> 'a1 -> bool) -> 'a1 tree -> 'a1 tree -> bool

      val map : ('a1 -> 'a2) -> 'a1 tree -> 'a2 tree

      val mapi : (key -> 'a1 -> 'a2) -> 'a1 tree -> 'a2 tree

      val map_option : (key -> 'a1 -> 'a2 option) -> 'a1 tree -> 'a2 tree

      val map2_opt :
        (key -> 'a1 -> 'a2 option -> 'a3 option) -> ('a1 tree -> 'a3 tree) ->
        ('a2 tree -> 'a3 tree) -> 'a1 tree -> 'a2 tree -> 'a3 tree

      val map2 :
        ('a1 option -> 'a2 option -> 'a3 option) -> 'a1 tree -> 'a2 tree ->
        'a3 tree

      module Proofs :
       sig
        module MX :
         sig
          module TO :
           sig
            type t = Equality.sort
           end

          module IsTO :
           sig
           end

          module OrderTac :
           sig
           end

          val eq_dec : Equality.sort -> Equality.sort -> bool

          val lt_dec : Equality.sort -> Equality.sort -> bool

          val eqb : Equality.sort -> Equality.sort -> bool
         end

        module PX :
         sig
          module MO :
           sig
            module TO :
             sig
              type t = Equality.sort
             end

            module IsTO :
             sig
             end

            module OrderTac :
             sig
             end

            val eq_dec : Equality.sort -> Equality.sort -> bool

            val lt_dec : Equality.sort -> Equality.sort -> bool

            val eqb : Equality.sort -> Equality.sort -> bool
           end
         end

        module L :
         sig
          module MX :
           sig
            module TO :
             sig
              type t = Equality.sort
             end

            module IsTO :
             sig
             end

            module OrderTac :
             sig
             end

            val eq_dec : Equality.sort -> Equality.sort -> bool

            val lt_dec : Equality.sort -> Equality.sort -> bool

            val eqb : Equality.sort -> Equality.sort -> bool
           end

          module PX :
           sig
            module MO :
             sig
              module TO :
               sig
                type t = Equality.sort
               end

              module IsTO :
               sig
               end

              module OrderTac :
               sig
               end

              val eq_dec : Equality.sort -> Equality.sort -> bool

              val lt_dec : Equality.sort -> Equality.sort -> bool

              val eqb : Equality.sort -> Equality.sort -> bool
             end
           end

          type key = Equality.sort

          type 'elt t = (Equality.sort * 'elt) list

          val empty : 'a1 t

          val is_empty : 'a1 t -> bool

          val mem : key -> 'a1 t -> bool

          val find : key -> 'a1 t -> 'a1 option

          val add : key -> 'a1 -> 'a1 t -> 'a1 t

          val remove : key -> 'a1 t -> 'a1 t

          val elements : 'a1 t -> 'a1 t

          val fold : (key -> 'a1 -> 'a2 -> 'a2) -> 'a1 t -> 'a2 -> 'a2

          val equal : ('a1 -> 'a1 -> bool) -> 'a1 t -> 'a1 t -> bool

          val map : ('a1 -> 'a2) -> 'a1 t -> 'a2 t

          val mapi : (key -> 'a1 -> 'a2) -> 'a1 t -> 'a2 t

          val option_cons :
            key -> 'a1 option -> (key * 'a1) list -> (key * 'a1) list

          val map2_l :
            ('a1 option -> 'a2 option -> 'a3 option) -> 'a1 t -> 'a3 t

          val map2_r :
            ('a1 option -> 'a2 option -> 'a3 option) -> 'a2 t -> 'a3 t

          val map2 :
            ('a1 option -> 'a2 option -> 'a3 option) -> 'a1 t -> 'a2 t -> 'a3
            t

          val combine : 'a1 t -> 'a2 t -> ('a1 option * 'a2 option) t

          val fold_right_pair :
            ('a1 -> 'a2 -> 'a3 -> 'a3) -> ('a1 * 'a2) list -> 'a3 -> 'a3

          val map2_alt :
            ('a1 option -> 'a2 option -> 'a3 option) -> 'a1 t -> 'a2 t ->
            (key * 'a3) list

          val at_least_one :
            'a1 option -> 'a2 option -> ('a1 option * 'a2 option) option

          val at_least_one_then_f :
            ('a1 option -> 'a2 option -> 'a3 option) -> 'a1 option -> 'a2
            option -> 'a3 option
         end

        type 'elt coq_R_mem =
        | R_mem_0 of 'elt tree
        | R_mem_1 of 'elt tree * 'elt tree * key * 'elt * 'elt tree
           * Int.Z_as_Int.t * bool * 'elt coq_R_mem
        | R_mem_2 of 'elt tree * 'elt tree * key * 'elt * 'elt tree
           * Int.Z_as_Int.t
        | R_mem_3 of 'elt tree * 'elt tree * key * 'elt * 'elt tree
           * Int.Z_as_Int.t * bool * 'elt coq_R_mem

        val coq_R_mem_rect :
          Equality.sort -> ('a1 tree -> __ -> 'a2) -> ('a1 tree -> 'a1 tree
          -> key -> 'a1 -> 'a1 tree -> Int.Z_as_Int.t -> __ -> __ -> __ ->
          bool -> 'a1 coq_R_mem -> 'a2 -> 'a2) -> ('a1 tree -> 'a1 tree ->
          key -> 'a1 -> 'a1 tree -> Int.Z_as_Int.t -> __ -> __ -> __ -> 'a2)
          -> ('a1 tree -> 'a1 tree -> key -> 'a1 -> 'a1 tree ->
          Int.Z_as_Int.t -> __ -> __ -> __ -> bool -> 'a1 coq_R_mem -> 'a2 ->
          'a2) -> 'a1 tree -> bool -> 'a1 coq_R_mem -> 'a2

        val coq_R_mem_rec :
          Equality.sort -> ('a1 tree -> __ -> 'a2) -> ('a1 tree -> 'a1 tree
          -> key -> 'a1 -> 'a1 tree -> Int.Z_as_Int.t -> __ -> __ -> __ ->
          bool -> 'a1 coq_R_mem -> 'a2 -> 'a2) -> ('a1 tree -> 'a1 tree ->
          key -> 'a1 -> 'a1 tree -> Int.Z_as_Int.t -> __ -> __ -> __ -> 'a2)
          -> ('a1 tree -> 'a1 tree -> key -> 'a1 -> 'a1 tree ->
          Int.Z_as_Int.t -> __ -> __ -> __ -> bool -> 'a1 coq_R_mem -> 'a2 ->
          'a2) -> 'a1 tree -> bool -> 'a1 coq_R_mem -> 'a2

        type 'elt coq_R_find =
        | R_find_0 of 'elt tree
        | R_find_1 of 'elt tree * 'elt tree * key * 'elt * 'elt tree
           * Int.Z_as_Int.t * 'elt option * 'elt coq_R_find
        | R_find_2 of 'elt tree * 'elt tree * key * 'elt * 'elt tree
           * Int.Z_as_Int.t
        | R_find_3 of 'elt tree * 'elt tree * key * 'elt * 'elt tree
           * Int.Z_as_Int.t * 'elt option * 'elt coq_R_find

        val coq_R_find_rect :
          Equality.sort -> ('a1 tree -> __ -> 'a2) -> ('a1 tree -> 'a1 tree
          -> key -> 'a1 -> 'a1 tree -> Int.Z_as_Int.t -> __ -> __ -> __ ->
          'a1 option -> 'a1 coq_R_find -> 'a2 -> 'a2) -> ('a1 tree -> 'a1
          tree -> key -> 'a1 -> 'a1 tree -> Int.Z_as_Int.t -> __ -> __ -> __
          -> 'a2) -> ('a1 tree -> 'a1 tree -> key -> 'a1 -> 'a1 tree ->
          Int.Z_as_Int.t -> __ -> __ -> __ -> 'a1 option -> 'a1 coq_R_find ->
          'a2 -> 'a2) -> 'a1 tree -> 'a1 option -> 'a1 coq_R_find -> 'a2

        val coq_R_find_rec :
          Equality.sort -> ('a1 tree -> __ -> 'a2) -> ('a1 tree -> 'a1 tree
          -> key -> 'a1 -> 'a1 tree -> Int.Z_as_Int.t -> __ -> __ -> __ ->
          'a1 option -> 'a1 coq_R_find -> 'a2 -> 'a2) -> ('a1 tree -> 'a1
          tree -> key -> 'a1 -> 'a1 tree -> Int.Z_as_Int.t -> __ -> __ -> __
          -> 'a2) -> ('a1 tree -> 'a1 tree -> key -> 'a1 -> 'a1 tree ->
          Int.Z_as_Int.t -> __ -> __ -> __ -> 'a1 option -> 'a1 coq_R_find ->
          'a2 -> 'a2) -> 'a1 tree -> 'a1 option -> 'a1 coq_R_find -> 'a2

        type 'elt coq_R_bal =
        | R_bal_0 of 'elt tree * key * 'elt * 'elt tree
        | R_bal_1 of 'elt tree * key * 'elt * 'elt tree * 'elt tree * 
           key * 'elt * 'elt tree * Int.Z_as_Int.t
        | R_bal_2 of 'elt tree * key * 'elt * 'elt tree * 'elt tree * 
           key * 'elt * 'elt tree * Int.Z_as_Int.t
        | R_bal_3 of 'elt tree * key * 'elt * 'elt tree * 'elt tree * 
           key * 'elt * 'elt tree * Int.Z_as_Int.t * 'elt tree * key * 
           'elt * 'elt tree * Int.Z_as_Int.t
        | R_bal_4 of 'elt tree * key * 'elt * 'elt tree
        | R_bal_5 of 'elt tree * key * 'elt * 'elt tree * 'elt tree * 
           key * 'elt * 'elt tree * Int.Z_as_Int.t
        | R_bal_6 of 'elt tree * key * 'elt * 'elt tree * 'elt tree * 
           key * 'elt * 'elt tree * Int.Z_as_Int.t
        | R_bal_7 of 'elt tree * key * 'elt * 'elt tree * 'elt tree * 
           key * 'elt * 'elt tree * Int.Z_as_Int.t * 'elt tree * key * 
           'elt * 'elt tree * Int.Z_as_Int.t
        | R_bal_8 of 'elt tree * key * 'elt * 'elt tree

        val coq_R_bal_rect :
          ('a1 tree -> key -> 'a1 -> 'a1 tree -> __ -> __ -> __ -> 'a2) ->
          ('a1 tree -> key -> 'a1 -> 'a1 tree -> __ -> __ -> 'a1 tree -> key
          -> 'a1 -> 'a1 tree -> Int.Z_as_Int.t -> __ -> __ -> __ -> 'a2) ->
          ('a1 tree -> key -> 'a1 -> 'a1 tree -> __ -> __ -> 'a1 tree -> key
          -> 'a1 -> 'a1 tree -> Int.Z_as_Int.t -> __ -> __ -> __ -> __ ->
          'a2) -> ('a1 tree -> key -> 'a1 -> 'a1 tree -> __ -> __ -> 'a1 tree
          -> key -> 'a1 -> 'a1 tree -> Int.Z_as_Int.t -> __ -> __ -> __ ->
          'a1 tree -> key -> 'a1 -> 'a1 tree -> Int.Z_as_Int.t -> __ -> 'a2)
          -> ('a1 tree -> key -> 'a1 -> 'a1 tree -> __ -> __ -> __ -> __ ->
          __ -> 'a2) -> ('a1 tree -> key -> 'a1 -> 'a1 tree -> __ -> __ -> __
          -> __ -> 'a1 tree -> key -> 'a1 -> 'a1 tree -> Int.Z_as_Int.t -> __
          -> __ -> __ -> 'a2) -> ('a1 tree -> key -> 'a1 -> 'a1 tree -> __ ->
          __ -> __ -> __ -> 'a1 tree -> key -> 'a1 -> 'a1 tree ->
          Int.Z_as_Int.t -> __ -> __ -> __ -> __ -> 'a2) -> ('a1 tree -> key
          -> 'a1 -> 'a1 tree -> __ -> __ -> __ -> __ -> 'a1 tree -> key ->
          'a1 -> 'a1 tree -> Int.Z_as_Int.t -> __ -> __ -> __ -> 'a1 tree ->
          key -> 'a1 -> 'a1 tree -> Int.Z_as_Int.t -> __ -> 'a2) -> ('a1 tree
          -> key -> 'a1 -> 'a1 tree -> __ -> __ -> __ -> __ -> 'a2) -> 'a1
          tree -> key -> 'a1 -> 'a1 tree -> 'a1 tree -> 'a1 coq_R_bal -> 'a2

        val coq_R_bal_rec :
          ('a1 tree -> key -> 'a1 -> 'a1 tree -> __ -> __ -> __ -> 'a2) ->
          ('a1 tree -> key -> 'a1 -> 'a1 tree -> __ -> __ -> 'a1 tree -> key
          -> 'a1 -> 'a1 tree -> Int.Z_as_Int.t -> __ -> __ -> __ -> 'a2) ->
          ('a1 tree -> key -> 'a1 -> 'a1 tree -> __ -> __ -> 'a1 tree -> key
          -> 'a1 -> 'a1 tree -> Int.Z_as_Int.t -> __ -> __ -> __ -> __ ->
          'a2) -> ('a1 tree -> key -> 'a1 -> 'a1 tree -> __ -> __ -> 'a1 tree
          -> key -> 'a1 -> 'a1 tree -> Int.Z_as_Int.t -> __ -> __ -> __ ->
          'a1 tree -> key -> 'a1 -> 'a1 tree -> Int.Z_as_Int.t -> __ -> 'a2)
          -> ('a1 tree -> key -> 'a1 -> 'a1 tree -> __ -> __ -> __ -> __ ->
          __ -> 'a2) -> ('a1 tree -> key -> 'a1 -> 'a1 tree -> __ -> __ -> __
          -> __ -> 'a1 tree -> key -> 'a1 -> 'a1 tree -> Int.Z_as_Int.t -> __
          -> __ -> __ -> 'a2) -> ('a1 tree -> key -> 'a1 -> 'a1 tree -> __ ->
          __ -> __ -> __ -> 'a1 tree -> key -> 'a1 -> 'a1 tree ->
          Int.Z_as_Int.t -> __ -> __ -> __ -> __ -> 'a2) -> ('a1 tree -> key
          -> 'a1 -> 'a1 tree -> __ -> __ -> __ -> __ -> 'a1 tree -> key ->
          'a1 -> 'a1 tree -> Int.Z_as_Int.t -> __ -> __ -> __ -> 'a1 tree ->
          key -> 'a1 -> 'a1 tree -> Int.Z_as_Int.t -> __ -> 'a2) -> ('a1 tree
          -> key -> 'a1 -> 'a1 tree -> __ -> __ -> __ -> __ -> 'a2) -> 'a1
          tree -> key -> 'a1 -> 'a1 tree -> 'a1 tree -> 'a1 coq_R_bal -> 'a2

        type 'elt coq_R_add =
        | R_add_0 of 'elt tree
        | R_add_1 of 'elt tree * 'elt tree * key * 'elt * 'elt tree
           * Int.Z_as_Int.t * 'elt tree * 'elt coq_R_add
        | R_add_2 of 'elt tree * 'elt tree * key * 'elt * 'elt tree
           * Int.Z_as_Int.t
        | R_add_3 of 'elt tree * 'elt tree * key * 'elt * 'elt tree
           * Int.Z_as_Int.t * 'elt tree * 'elt coq_R_add

        val coq_R_add_rect :
          key -> 'a1 -> ('a1 tree -> __ -> 'a2) -> ('a1 tree -> 'a1 tree ->
          key -> 'a1 -> 'a1 tree -> Int.Z_as_Int.t -> __ -> __ -> __ -> 'a1
          tree -> 'a1 coq_R_add -> 'a2 -> 'a2) -> ('a1 tree -> 'a1 tree ->
          key -> 'a1 -> 'a1 tree -> Int.Z_as_Int.t -> __ -> __ -> __ -> 'a2)
          -> ('a1 tree -> 'a1 tree -> key -> 'a1 -> 'a1 tree ->
          Int.Z_as_Int.t -> __ -> __ -> __ -> 'a1 tree -> 'a1 coq_R_add ->
          'a2 -> 'a2) -> 'a1 tree -> 'a1 tree -> 'a1 coq_R_add -> 'a2

        val coq_R_add_rec :
          key -> 'a1 -> ('a1 tree -> __ -> 'a2) -> ('a1 tree -> 'a1 tree ->
          key -> 'a1 -> 'a1 tree -> Int.Z_as_Int.t -> __ -> __ -> __ -> 'a1
          tree -> 'a1 coq_R_add -> 'a2 -> 'a2) -> ('a1 tree -> 'a1 tree ->
          key -> 'a1 -> 'a1 tree -> Int.Z_as_Int.t -> __ -> __ -> __ -> 'a2)
          -> ('a1 tree -> 'a1 tree -> key -> 'a1 -> 'a1 tree ->
          Int.Z_as_Int.t -> __ -> __ -> __ -> 'a1 tree -> 'a1 coq_R_add ->
          'a2 -> 'a2) -> 'a1 tree -> 'a1 tree -> 'a1 coq_R_add -> 'a2

        type 'elt coq_R_remove_min =
        | R_remove_min_0 of 'elt tree * key * 'elt * 'elt tree
        | R_remove_min_1 of 'elt tree * key * 'elt * 'elt tree * 'elt tree
           * key * 'elt * 'elt tree * Int.Z_as_Int.t
           * ('elt tree * (key * 'elt)) * 'elt coq_R_remove_min * 'elt tree
           * (key * 'elt)

        val coq_R_remove_min_rect :
          ('a1 tree -> key -> 'a1 -> 'a1 tree -> __ -> 'a2) -> ('a1 tree ->
          key -> 'a1 -> 'a1 tree -> 'a1 tree -> key -> 'a1 -> 'a1 tree ->
          Int.Z_as_Int.t -> __ -> ('a1 tree * (key * 'a1)) -> 'a1
          coq_R_remove_min -> 'a2 -> 'a1 tree -> (key * 'a1) -> __ -> 'a2) ->
          'a1 tree -> key -> 'a1 -> 'a1 tree -> ('a1 tree * (key * 'a1)) ->
          'a1 coq_R_remove_min -> 'a2

        val coq_R_remove_min_rec :
          ('a1 tree -> key -> 'a1 -> 'a1 tree -> __ -> 'a2) -> ('a1 tree ->
          key -> 'a1 -> 'a1 tree -> 'a1 tree -> key -> 'a1 -> 'a1 tree ->
          Int.Z_as_Int.t -> __ -> ('a1 tree * (key * 'a1)) -> 'a1
          coq_R_remove_min -> 'a2 -> 'a1 tree -> (key * 'a1) -> __ -> 'a2) ->
          'a1 tree -> key -> 'a1 -> 'a1 tree -> ('a1 tree * (key * 'a1)) ->
          'a1 coq_R_remove_min -> 'a2

        type 'elt coq_R_merge =
        | R_merge_0 of 'elt tree * 'elt tree
        | R_merge_1 of 'elt tree * 'elt tree * 'elt tree * key * 'elt
           * 'elt tree * Int.Z_as_Int.t
        | R_merge_2 of 'elt tree * 'elt tree * 'elt tree * key * 'elt
           * 'elt tree * Int.Z_as_Int.t * 'elt tree * key * 'elt * 'elt tree
           * Int.Z_as_Int.t * 'elt tree * (key * 'elt) * key * 'elt

        val coq_R_merge_rect :
          ('a1 tree -> 'a1 tree -> __ -> 'a2) -> ('a1 tree -> 'a1 tree -> 'a1
          tree -> key -> 'a1 -> 'a1 tree -> Int.Z_as_Int.t -> __ -> __ ->
          'a2) -> ('a1 tree -> 'a1 tree -> 'a1 tree -> key -> 'a1 -> 'a1 tree
          -> Int.Z_as_Int.t -> __ -> 'a1 tree -> key -> 'a1 -> 'a1 tree ->
          Int.Z_as_Int.t -> __ -> 'a1 tree -> (key * 'a1) -> __ -> key -> 'a1
          -> __ -> 'a2) -> 'a1 tree -> 'a1 tree -> 'a1 tree -> 'a1
          coq_R_merge -> 'a2

        val coq_R_merge_rec :
          ('a1 tree -> 'a1 tree -> __ -> 'a2) -> ('a1 tree -> 'a1 tree -> 'a1
          tree -> key -> 'a1 -> 'a1 tree -> Int.Z_as_Int.t -> __ -> __ ->
          'a2) -> ('a1 tree -> 'a1 tree -> 'a1 tree -> key -> 'a1 -> 'a1 tree
          -> Int.Z_as_Int.t -> __ -> 'a1 tree -> key -> 'a1 -> 'a1 tree ->
          Int.Z_as_Int.t -> __ -> 'a1 tree -> (key * 'a1) -> __ -> key -> 'a1
          -> __ -> 'a2) -> 'a1 tree -> 'a1 tree -> 'a1 tree -> 'a1
          coq_R_merge -> 'a2

        type 'elt coq_R_remove =
        | R_remove_0 of 'elt tree
        | R_remove_1 of 'elt tree * 'elt tree * key * 'elt * 'elt tree
           * Int.Z_as_Int.t * 'elt tree * 'elt coq_R_remove
        | R_remove_2 of 'elt tree * 'elt tree * key * 'elt * 'elt tree
           * Int.Z_as_Int.t
        | R_remove_3 of 'elt tree * 'elt tree * key * 'elt * 'elt tree
           * Int.Z_as_Int.t * 'elt tree * 'elt coq_R_remove

        val coq_R_remove_rect :
          Equality.sort -> ('a1 tree -> __ -> 'a2) -> ('a1 tree -> 'a1 tree
          -> key -> 'a1 -> 'a1 tree -> Int.Z_as_Int.t -> __ -> __ -> __ ->
          'a1 tree -> 'a1 coq_R_remove -> 'a2 -> 'a2) -> ('a1 tree -> 'a1
          tree -> key -> 'a1 -> 'a1 tree -> Int.Z_as_Int.t -> __ -> __ -> __
          -> 'a2) -> ('a1 tree -> 'a1 tree -> key -> 'a1 -> 'a1 tree ->
          Int.Z_as_Int.t -> __ -> __ -> __ -> 'a1 tree -> 'a1 coq_R_remove ->
          'a2 -> 'a2) -> 'a1 tree -> 'a1 tree -> 'a1 coq_R_remove -> 'a2

        val coq_R_remove_rec :
          Equality.sort -> ('a1 tree -> __ -> 'a2) -> ('a1 tree -> 'a1 tree
          -> key -> 'a1 -> 'a1 tree -> Int.Z_as_Int.t -> __ -> __ -> __ ->
          'a1 tree -> 'a1 coq_R_remove -> 'a2 -> 'a2) -> ('a1 tree -> 'a1
          tree -> key -> 'a1 -> 'a1 tree -> Int.Z_as_Int.t -> __ -> __ -> __
          -> 'a2) -> ('a1 tree -> 'a1 tree -> key -> 'a1 -> 'a1 tree ->
          Int.Z_as_Int.t -> __ -> __ -> __ -> 'a1 tree -> 'a1 coq_R_remove ->
          'a2 -> 'a2) -> 'a1 tree -> 'a1 tree -> 'a1 coq_R_remove -> 'a2

        type 'elt coq_R_concat =
        | R_concat_0 of 'elt tree * 'elt tree
        | R_concat_1 of 'elt tree * 'elt tree * 'elt tree * key * 'elt
           * 'elt tree * Int.Z_as_Int.t
        | R_concat_2 of 'elt tree * 'elt tree * 'elt tree * key * 'elt
           * 'elt tree * Int.Z_as_Int.t * 'elt tree * key * 'elt * 'elt tree
           * Int.Z_as_Int.t * 'elt tree * (key * 'elt)

        val coq_R_concat_rect :
          ('a1 tree -> 'a1 tree -> __ -> 'a2) -> ('a1 tree -> 'a1 tree -> 'a1
          tree -> key -> 'a1 -> 'a1 tree -> Int.Z_as_Int.t -> __ -> __ ->
          'a2) -> ('a1 tree -> 'a1 tree -> 'a1 tree -> key -> 'a1 -> 'a1 tree
          -> Int.Z_as_Int.t -> __ -> 'a1 tree -> key -> 'a1 -> 'a1 tree ->
          Int.Z_as_Int.t -> __ -> 'a1 tree -> (key * 'a1) -> __ -> 'a2) ->
          'a1 tree -> 'a1 tree -> 'a1 tree -> 'a1 coq_R_concat -> 'a2

        val coq_R_concat_rec :
          ('a1 tree -> 'a1 tree -> __ -> 'a2) -> ('a1 tree -> 'a1 tree -> 'a1
          tree -> key -> 'a1 -> 'a1 tree -> Int.Z_as_Int.t -> __ -> __ ->
          'a2) -> ('a1 tree -> 'a1 tree -> 'a1 tree -> key -> 'a1 -> 'a1 tree
          -> Int.Z_as_Int.t -> __ -> 'a1 tree -> key -> 'a1 -> 'a1 tree ->
          Int.Z_as_Int.t -> __ -> 'a1 tree -> (key * 'a1) -> __ -> 'a2) ->
          'a1 tree -> 'a1 tree -> 'a1 tree -> 'a1 coq_R_concat -> 'a2

        type 'elt coq_R_split =
        | R_split_0 of 'elt tree
        | R_split_1 of 'elt tree * 'elt tree * key * 'elt * 'elt tree
           * Int.Z_as_Int.t * 'elt triple * 'elt coq_R_split * 'elt tree
           * 'elt option * 'elt tree
        | R_split_2 of 'elt tree * 'elt tree * key * 'elt * 'elt tree
           * Int.Z_as_Int.t
        | R_split_3 of 'elt tree * 'elt tree * key * 'elt * 'elt tree
           * Int.Z_as_Int.t * 'elt triple * 'elt coq_R_split * 'elt tree
           * 'elt option * 'elt tree

        val coq_R_split_rect :
          Equality.sort -> ('a1 tree -> __ -> 'a2) -> ('a1 tree -> 'a1 tree
          -> key -> 'a1 -> 'a1 tree -> Int.Z_as_Int.t -> __ -> __ -> __ ->
          'a1 triple -> 'a1 coq_R_split -> 'a2 -> 'a1 tree -> 'a1 option ->
          'a1 tree -> __ -> 'a2) -> ('a1 tree -> 'a1 tree -> key -> 'a1 ->
          'a1 tree -> Int.Z_as_Int.t -> __ -> __ -> __ -> 'a2) -> ('a1 tree
          -> 'a1 tree -> key -> 'a1 -> 'a1 tree -> Int.Z_as_Int.t -> __ -> __
          -> __ -> 'a1 triple -> 'a1 coq_R_split -> 'a2 -> 'a1 tree -> 'a1
          option -> 'a1 tree -> __ -> 'a2) -> 'a1 tree -> 'a1 triple -> 'a1
          coq_R_split -> 'a2

        val coq_R_split_rec :
          Equality.sort -> ('a1 tree -> __ -> 'a2) -> ('a1 tree -> 'a1 tree
          -> key -> 'a1 -> 'a1 tree -> Int.Z_as_Int.t -> __ -> __ -> __ ->
          'a1 triple -> 'a1 coq_R_split -> 'a2 -> 'a1 tree -> 'a1 option ->
          'a1 tree -> __ -> 'a2) -> ('a1 tree -> 'a1 tree -> key -> 'a1 ->
          'a1 tree -> Int.Z_as_Int.t -> __ -> __ -> __ -> 'a2) -> ('a1 tree
          -> 'a1 tree -> key -> 'a1 -> 'a1 tree -> Int.Z_as_Int.t -> __ -> __
          -> __ -> 'a1 triple -> 'a1 coq_R_split -> 'a2 -> 'a1 tree -> 'a1
          option -> 'a1 tree -> __ -> 'a2) -> 'a1 tree -> 'a1 triple -> 'a1
          coq_R_split -> 'a2

        type ('elt, 'x) coq_R_map_option =
        | R_map_option_0 of 'elt tree
        | R_map_option_1 of 'elt tree * 'elt tree * key * 'elt * 'elt tree
           * Int.Z_as_Int.t * 'x * 'x tree * ('elt, 'x) coq_R_map_option
           * 'x tree * ('elt, 'x) coq_R_map_option
        | R_map_option_2 of 'elt tree * 'elt tree * key * 'elt * 'elt tree
           * Int.Z_as_Int.t * 'x tree * ('elt, 'x) coq_R_map_option * 
           'x tree * ('elt, 'x) coq_R_map_option

        val coq_R_map_option_rect :
          (key -> 'a1 -> 'a2 option) -> ('a1 tree -> __ -> 'a3) -> ('a1 tree
          -> 'a1 tree -> key -> 'a1 -> 'a1 tree -> Int.Z_as_Int.t -> __ ->
          'a2 -> __ -> 'a2 tree -> ('a1, 'a2) coq_R_map_option -> 'a3 -> 'a2
          tree -> ('a1, 'a2) coq_R_map_option -> 'a3 -> 'a3) -> ('a1 tree ->
          'a1 tree -> key -> 'a1 -> 'a1 tree -> Int.Z_as_Int.t -> __ -> __ ->
          'a2 tree -> ('a1, 'a2) coq_R_map_option -> 'a3 -> 'a2 tree -> ('a1,
          'a2) coq_R_map_option -> 'a3 -> 'a3) -> 'a1 tree -> 'a2 tree ->
          ('a1, 'a2) coq_R_map_option -> 'a3

        val coq_R_map_option_rec :
          (key -> 'a1 -> 'a2 option) -> ('a1 tree -> __ -> 'a3) -> ('a1 tree
          -> 'a1 tree -> key -> 'a1 -> 'a1 tree -> Int.Z_as_Int.t -> __ ->
          'a2 -> __ -> 'a2 tree -> ('a1, 'a2) coq_R_map_option -> 'a3 -> 'a2
          tree -> ('a1, 'a2) coq_R_map_option -> 'a3 -> 'a3) -> ('a1 tree ->
          'a1 tree -> key -> 'a1 -> 'a1 tree -> Int.Z_as_Int.t -> __ -> __ ->
          'a2 tree -> ('a1, 'a2) coq_R_map_option -> 'a3 -> 'a2 tree -> ('a1,
          'a2) coq_R_map_option -> 'a3 -> 'a3) -> 'a1 tree -> 'a2 tree ->
          ('a1, 'a2) coq_R_map_option -> 'a3

        type ('elt, 'x0, 'x) coq_R_map2_opt =
        | R_map2_opt_0 of 'elt tree * 'x0 tree
        | R_map2_opt_1 of 'elt tree * 'x0 tree * 'elt tree * key * 'elt
           * 'elt tree * Int.Z_as_Int.t
        | R_map2_opt_2 of 'elt tree * 'x0 tree * 'elt tree * key * 'elt
           * 'elt tree * Int.Z_as_Int.t * 'x0 tree * key * 'x0 * 'x0 tree
           * Int.Z_as_Int.t * 'x0 tree * 'x0 option * 'x0 tree * 'x * 
           'x tree * ('elt, 'x0, 'x) coq_R_map2_opt * 'x tree
           * ('elt, 'x0, 'x) coq_R_map2_opt
        | R_map2_opt_3 of 'elt tree * 'x0 tree * 'elt tree * key * 'elt
           * 'elt tree * Int.Z_as_Int.t * 'x0 tree * key * 'x0 * 'x0 tree
           * Int.Z_as_Int.t * 'x0 tree * 'x0 option * 'x0 tree * 'x tree
           * ('elt, 'x0, 'x) coq_R_map2_opt * 'x tree
           * ('elt, 'x0, 'x) coq_R_map2_opt

        val coq_R_map2_opt_rect :
          (key -> 'a1 -> 'a2 option -> 'a3 option) -> ('a1 tree -> 'a3 tree)
          -> ('a2 tree -> 'a3 tree) -> ('a1 tree -> 'a2 tree -> __ -> 'a4) ->
          ('a1 tree -> 'a2 tree -> 'a1 tree -> key -> 'a1 -> 'a1 tree ->
          Int.Z_as_Int.t -> __ -> __ -> 'a4) -> ('a1 tree -> 'a2 tree -> 'a1
          tree -> key -> 'a1 -> 'a1 tree -> Int.Z_as_Int.t -> __ -> 'a2 tree
          -> key -> 'a2 -> 'a2 tree -> Int.Z_as_Int.t -> __ -> 'a2 tree ->
          'a2 option -> 'a2 tree -> __ -> 'a3 -> __ -> 'a3 tree -> ('a1, 'a2,
          'a3) coq_R_map2_opt -> 'a4 -> 'a3 tree -> ('a1, 'a2, 'a3)
          coq_R_map2_opt -> 'a4 -> 'a4) -> ('a1 tree -> 'a2 tree -> 'a1 tree
          -> key -> 'a1 -> 'a1 tree -> Int.Z_as_Int.t -> __ -> 'a2 tree ->
          key -> 'a2 -> 'a2 tree -> Int.Z_as_Int.t -> __ -> 'a2 tree -> 'a2
          option -> 'a2 tree -> __ -> __ -> 'a3 tree -> ('a1, 'a2, 'a3)
          coq_R_map2_opt -> 'a4 -> 'a3 tree -> ('a1, 'a2, 'a3) coq_R_map2_opt
          -> 'a4 -> 'a4) -> 'a1 tree -> 'a2 tree -> 'a3 tree -> ('a1, 'a2,
          'a3) coq_R_map2_opt -> 'a4

        val coq_R_map2_opt_rec :
          (key -> 'a1 -> 'a2 option -> 'a3 option) -> ('a1 tree -> 'a3 tree)
          -> ('a2 tree -> 'a3 tree) -> ('a1 tree -> 'a2 tree -> __ -> 'a4) ->
          ('a1 tree -> 'a2 tree -> 'a1 tree -> key -> 'a1 -> 'a1 tree ->
          Int.Z_as_Int.t -> __ -> __ -> 'a4) -> ('a1 tree -> 'a2 tree -> 'a1
          tree -> key -> 'a1 -> 'a1 tree -> Int.Z_as_Int.t -> __ -> 'a2 tree
          -> key -> 'a2 -> 'a2 tree -> Int.Z_as_Int.t -> __ -> 'a2 tree ->
          'a2 option -> 'a2 tree -> __ -> 'a3 -> __ -> 'a3 tree -> ('a1, 'a2,
          'a3) coq_R_map2_opt -> 'a4 -> 'a3 tree -> ('a1, 'a2, 'a3)
          coq_R_map2_opt -> 'a4 -> 'a4) -> ('a1 tree -> 'a2 tree -> 'a1 tree
          -> key -> 'a1 -> 'a1 tree -> Int.Z_as_Int.t -> __ -> 'a2 tree ->
          key -> 'a2 -> 'a2 tree -> Int.Z_as_Int.t -> __ -> 'a2 tree -> 'a2
          option -> 'a2 tree -> __ -> __ -> 'a3 tree -> ('a1, 'a2, 'a3)
          coq_R_map2_opt -> 'a4 -> 'a3 tree -> ('a1, 'a2, 'a3) coq_R_map2_opt
          -> 'a4 -> 'a4) -> 'a1 tree -> 'a2 tree -> 'a3 tree -> ('a1, 'a2,
          'a3) coq_R_map2_opt -> 'a4

        val fold' : (key -> 'a1 -> 'a2 -> 'a2) -> 'a1 tree -> 'a2 -> 'a2

        val flatten_e : 'a1 enumeration -> (key * 'a1) list
       end
     end

    type 'elt bst =
      'elt Raw.tree
      (* singleton inductive, whose constructor was Bst *)

    val this : 'a1 bst -> 'a1 Raw.tree

    type 'elt t = 'elt bst

    type key = Equality.sort

    val empty : 'a1 t

    val is_empty : 'a1 t -> bool

    val add : key -> 'a1 -> 'a1 t -> 'a1 t

    val remove : key -> 'a1 t -> 'a1 t

    val mem : key -> 'a1 t -> bool

    val find : key -> 'a1 t -> 'a1 option

    val map : ('a1 -> 'a2) -> 'a1 t -> 'a2 t

    val mapi : (key -> 'a1 -> 'a2) -> 'a1 t -> 'a2 t

    val map2 :
      ('a1 option -> 'a2 option -> 'a3 option) -> 'a1 t -> 'a2 t -> 'a3 t

    val elements : 'a1 t -> (key * 'a1) list

    val cardinal : 'a1 t -> nat

    val fold : (key -> 'a1 -> 'a2 -> 'a2) -> 'a1 t -> 'a2 -> 'a2

    val equal : ('a1 -> 'a1 -> bool) -> 'a1 t -> 'a1 t -> bool
   end

  module Facts :
   sig
    val eqb : Equality.sort -> Equality.sort -> bool

    val coq_In_dec : 'a1 Map.t -> Map.key -> bool
   end

  type 't t = 't Map.t

  val empty : 'a1 t

  val is_empty : 'a1 t -> bool

  val get : 'a1 t -> Equality.sort -> 'a1 option

  val set : 'a1 t -> Equality.sort -> 'a1 -> 'a1 Map.t

  val remove : 'a1 t -> Equality.sort -> 'a1 Map.t

  val map : ('a1 -> 'a2) -> 'a1 Map.t -> 'a2 Map.t

  val mapi : (Map.key -> 'a1 -> 'a2) -> 'a1 Map.t -> 'a2 Map.t

  val raw_map2 :
    (Equality.sort -> 'a1 option -> 'a2 option -> 'a3 option) -> 'a1
    Map.Raw.tree -> 'a2 Map.Raw.tree -> 'a3 Map.Raw.tree

  val elements : 'a1 Map.t -> (Map.key * 'a1) list

  val fold : (Map.key -> 'a1 -> 'a2 -> 'a2) -> 'a1 Map.t -> 'a2 -> 'a2

  val all_t : (Equality.sort -> 'a1 -> bool) -> 'a1 Map.Raw.tree -> bool

  val has_t : (Equality.sort -> 'a1 -> bool) -> 'a1 Map.Raw.tree -> bool

  val incl_t :
    (Equality.sort -> 'a1 -> bool) -> (Equality.sort -> 'a1 -> 'a2 -> bool)
    -> 'a1 Map.Raw.tree -> 'a2 Map.Raw.tree -> bool

  val all : (Equality.sort -> 'a1 -> bool) -> 'a1 t -> bool

  val has : (Equality.sort -> 'a1 -> bool) -> 'a1 t -> bool

  val incl_def :
    (Equality.sort -> 'a1 -> bool) -> (Equality.sort -> 'a1 -> 'a2 -> bool)
    -> 'a1 Map.bst -> 'a2 Map.bst -> bool

  val incl :
    (Equality.sort -> 'a1 -> 'a2 -> bool) -> 'a1 Map.bst -> 'a2 Map.bst ->
    bool

  val in_codom : Equality.coq_type -> Equality.sort -> Equality.sort t -> bool

  val map2 :
    (Equality.sort -> 'a1 option -> 'a2 option -> 'a3 option) -> 'a1 t -> 'a2
    t -> 'a3 t

  val filter_map : (Equality.sort -> 'a1 -> 'a2 option) -> 'a1 t -> 'a2 t

  val is_emptyP : 'a1 t -> reflect

  val elementsP :
    Equality.coq_type -> (Equality.sort * Equality.sort) -> Equality.sort t
    -> reflect
 end

type sexpr =
| Sconst of coq_Z
| Svar of Var.var
| Sof_int of wsize * sexpr
| Sto_int of signedness * wsize * sexpr
| Sneg of op_kind * sexpr
| Sadd of op_kind * sexpr * sexpr
| Smul of op_kind * sexpr * sexpr
| Ssub of op_kind * sexpr * sexpr

val sexpr_beq : sexpr -> sexpr -> bool

val sexpr_eq_axiom : sexpr eq_axiom

val coq_HB_unnamed_factory_3 : sexpr Coq_hasDecEq.axioms_

val stack_alloc_sexpr__canonical__eqtype_Equality : Equality.coq_type

val is_const : sexpr -> coq_Z option

type symbolic_slice = { ss_ofs : sexpr; ss_len : sexpr }

val symbolic_slice_beq : symbolic_slice -> symbolic_slice -> bool

val symbolic_slice_eq_axiom : symbolic_slice eq_axiom

val coq_HB_unnamed_factory_5 : symbolic_slice Coq_hasDecEq.axioms_

val stack_alloc_symbolic_slice__canonical__eqtype_Equality : Equality.coq_type

type symbolic_zone = symbolic_slice list

type sub_region = { sr_region : region; sr_zone : symbolic_zone }

val sub_region_beq : sub_region -> sub_region -> bool

val sub_region_eq_axiom : sub_region eq_axiom

val coq_HB_unnamed_factory_7 : sub_region Coq_hasDecEq.axioms_

val stack_alloc_sub_region__canonical__eqtype_Equality : Equality.coq_type

type intervals = symbolic_slice list

val symbolic_slice_ble : symbolic_slice -> symbolic_slice -> bool option

val disjoint_slices : symbolic_slice -> symbolic_slice -> bool

val get_sub_interval : intervals -> Equality.sort -> bool

val add_sub_interval : intervals -> symbolic_slice -> intervals option

val remove_sub_interval : intervals -> Equality.sort -> intervals

type status =
| Valid
| Unknown
| Borrowed of intervals

type status_map = status Mvar.t

type region_map = { var_region : sub_region Mvar.t;
                    region_var : status_map Mr.t }

val empty_status_map : status Mvar.t

val empty : region_map

val get_sub_region : region_map -> var_i -> (pp_error_loc, sub_region) result

val get_status_map : status Mvar.t Mr.t -> region -> status_map

val get_status : status_map -> Equality.sort -> status

val get_var_status : status Mvar.t Mr.t -> region -> Equality.sort -> status

type concrete_slice = { cs_ofs : coq_Z; cs_len : coq_Z }

type ptr_kind_init =
| PIdirect of Var.var * concrete_slice * v_scope
| PIregptr of Var.var
| PIstkptr of Var.var * concrete_slice * Var.var

type ptr_kind =
| Pdirect of Var.var * coq_Z * wsize * concrete_slice * v_scope
| Pregptr of Var.var
| Pstkptr of Var.var * coq_Z * wsize * concrete_slice * Var.var

type vptr_kind =
| VKglob of (coq_Z * wsize)
| VKptr of ptr_kind

type pos_map = { vrip : Var.var; vrsp : Var.var; vxlen : Var.var;
                 globals : (coq_Z * wsize) Mvar.t; locals : ptr_kind Mvar.t;
                 vnew : SvExtra.Sv.t }

type param_info = { pp_ptr : Var.var; pp_writable : bool; pp_align : wsize }

val divide_z : coq_Z -> wsize -> bool

val divide : sexpr -> wsize -> bool

val divide_zone : symbolic_slice list -> wsize -> bool

val check_align :
  Equality.sort -> var_i -> sub_region -> wsize -> (pp_error_loc, unit) result

val check_writable : var_i -> region -> (pp_error_loc, unit) result

val is_valid : status -> bool

val check_valid : var_i -> status -> (pp_error_loc, unit) result

val split_last : symbolic_slice list -> symbolic_slice list * symbolic_slice

val sub_zone_at_ofs : symbolic_zone -> sexpr -> sexpr -> symbolic_slice list

val sub_region_at_ofs : sub_region -> sexpr -> sexpr -> sub_region

val get_sub_status : status -> Equality.sort -> bool

val sub_region_status_at_ofs :
  var_i -> sub_region -> status -> Equality.sort -> Equality.sort ->
  sub_region * status

val get_suffix : symbolic_zone -> symbolic_zone -> symbolic_zone option option

val fill_status : status -> symbolic_slice -> status

val clear_status : status -> symbolic_zone -> status option

val clear_status_map_aux :
  region_map -> symbolic_zone -> Equality.sort -> status -> status option

val clear_status_map :
  region_map -> symbolic_zone -> status_map -> status Mvar.t

val set_clear_status : region_map -> sub_region -> status_map Mr.Map.t

val set_clear_pure : region_map -> sub_region -> region_map

val set_clear :
  region_map -> var_i -> sub_region -> (pp_error_loc, region_map) result

val is_unknown : status -> bool

val set_status : status Mvar.t -> Equality.sort -> status -> status Mvar.Map.t

val set_word_status :
  region_map -> sub_region -> Equality.sort -> status -> status_map Mr.Map.t

val set_word_pure :
  region_map -> sub_region -> Equality.sort -> status -> region_map

val set_word :
  region_map -> Equality.sort -> sub_region -> var_i -> status -> wsize ->
  (pp_error_loc, region_map) result

val set_move_status :
  status Mvar.t Mr.t -> Equality.sort -> region -> status -> status Mvar.t
  Mr.Map.t

val set_move :
  region_map -> Equality.sort -> sub_region -> status -> region_map

val insert_status :
  Var.var -> status -> Equality.sort -> Equality.sort -> status -> status

val set_move_sub :
  region_map -> region -> Equality.sort -> status -> Equality.sort ->
  Equality.sort -> status -> region_map

val zone_of_cs : concrete_slice -> symbolic_zone

val sub_region_stkptr : slot -> wsize -> concrete_slice -> sub_region

val set_stack_ptr :
  region_map -> slot -> wsize -> concrete_slice -> Var.var -> region_map

val check_stack_ptr :
  status Mvar.t Mr.t -> slot -> wsize -> concrete_slice -> Equality.sort ->
  bool

val sub_region_full : Var.var -> region -> sub_region

val sub_region_glob : slot -> wsize -> sub_region

val get_sub_region_status :
  region_map -> var_i -> (pp_error_loc, sub_region * status) result

val get_gsub_region_status :
  region_map -> var_i -> vptr_kind -> (pp_error_loc, sub_region * status)
  result

type table = { bindings : sexpr Mvar.t; counter : Uint63.t;
               vars : SvExtra.Sv.t }

val table_fresh_var :
  (Var.var -> Uint63.t -> Var.var) -> table -> Var.var -> (pp_error_loc,
  table * sexpr) result

val table_get_var :
  (Var.var -> Uint63.t -> Var.var) -> table -> Equality.sort ->
  (pp_error_loc, table * sexpr) result

val merge_table : table -> table -> table

val symbolic_of_pexpr :
  (Var.var -> Uint63.t -> Var.var) -> table -> pexpr -> (table * sexpr)
  option cexec

val get_symbolic_of_pexpr :
  (Var.var -> Uint63.t -> Var.var) -> table -> pexpr -> (pp_error_loc,
  table * sexpr) result

val remove_binding : table -> Equality.sort -> table

val remove_binding_lval : table -> lval -> table

val remove_binding_lvals : table -> lval list -> table

val table_set_var : table -> Equality.sort -> sexpr -> table

val update_table : table -> lval -> sexpr option -> table

val assert_check : bool -> bool -> 'a1 -> ('a1, unit) result

val clone :
  (v_kind -> Uint63.t -> string -> atype -> Ident.Ident.ident) -> Var.var ->
  Uint63.t -> Var.var

val mul : coq_PointerData -> pexpr -> pexpr -> pexpr

val mk_ofs : coq_PointerData -> arr_access -> wsize -> pexpr -> coq_Z -> pexpr

val mk_ofs_int : arr_access -> wsize -> sexpr -> sexpr

val get_global : pos_map -> var_i -> (pp_error_loc, coq_Z * wsize) result

val get_local : pos_map -> Var.var -> ptr_kind option

val get_var_kind : pos_map -> gvar -> (pp_error_loc, vptr_kind option) result

val check_diff : pos_map -> var_i -> (pp_error_loc, unit) result

val check_var : pos_map -> var_i -> (pp_error_loc, unit) result

val with_var : var_i -> Var.var -> var_i

val base_ptr : pos_map -> v_scope -> Var.var

val addr_from_pk :
  pos_map -> var_i -> ptr_kind -> (pp_error_loc, var_i * coq_Z) result

val addr_from_vpk :
  pos_map -> var_i -> vptr_kind -> (pp_error_loc, var_i * coq_Z) result

val bad_arg_number : pp_error_loc

val not_trivially_incorrect : arr_access -> wsize -> pexpr -> coq_Z -> bool

val alloc_e :
  coq_PointerData -> pos_map -> region_map -> pexpr -> atype ->
  (pp_error_loc, pexpr) result

val alloc_es :
  coq_PointerData -> pos_map -> region_map -> pexpr list -> atype list ->
  (pp_error_loc, pexpr list) result

val sub_region_direct :
  slot -> wsize -> concrete_slice -> Equality.sort -> sub_region

val sub_region_stack : slot -> wsize -> concrete_slice -> sub_region

val alloc_lval :
  coq_PointerData -> pos_map -> region_map -> lval -> atype -> (pp_error_loc,
  region_map * lval) result

val nop : 'a1 asmOp -> 'a1 instr_r

val is_nop :
  region_map -> Equality.sort -> sub_region -> slot -> wsize ->
  concrete_slice -> Equality.sort -> bool

val get_addr :
  'a1 asmOp -> 'a1 stack_alloc_params -> var_i -> lval -> assgn_tag ->
  mov_kind -> pexpr -> pexpr -> (pp_error_loc, 'a1 instr_r) result

val is_stack_ptr :
  vptr_kind -> ((((Var.var * coq_Z) * wsize) * concrete_slice) * Var.var)
  option

val addr_from_vpk_pexpr :
  coq_PointerData -> pos_map -> status Mvar.t Mr.t -> var_i -> vptr_kind ->
  (pp_error_loc, pexpr * coq_Z) result

val mk_mov : vptr_kind -> mov_kind

val regions_are_not_equal :
  (sub_region -> pp_error) -> string -> var_i -> sub_region -> sub_region ->
  pp_error_loc

val alloc_array_move :
  coq_PointerData -> 'a1 asmOp -> 'a1 stack_alloc_params -> (v_kind ->
  Uint63.t -> string -> atype -> Ident.Ident.ident) -> (sub_region ->
  pp_error) -> pos_map -> table -> region_map -> lval -> assgn_tag -> pexpr
  -> (pp_error_loc, (table * region_map) * 'a1 instr_r) result

val is_protect_ptr_fail :
  'a1 asmOp -> lval list -> 'a1 sopn -> pexpr list ->
  ((lval * pexpr) * pexpr) option

val lower_protect_ptr_fail :
  coq_PointerData -> 'a1 asmOp -> 'a1 sh_params -> instr_info -> lval list ->
  assgn_tag -> pexpr list -> 'a1 instr_r cexec

val alloc_protect_ptr :
  coq_PointerData -> coq_MSFsize -> 'a1 asmOp -> 'a1 sh_params -> pos_map ->
  region_map -> instr_info -> lval -> assgn_tag -> pexpr -> pexpr ->
  (pp_error_loc, region_map * 'a1 instr_r) result

val alloc_array_move_init :
  coq_PointerData -> 'a1 asmOp -> 'a1 stack_alloc_params -> (v_kind ->
  Uint63.t -> string -> atype -> Ident.Ident.ident) -> (sub_region ->
  pp_error) -> pos_map -> table -> region_map -> lval -> assgn_tag -> pexpr
  -> (pp_error_loc, (table * region_map) * 'a1 instr_r) result

val bad_lval_number : pp_error_loc

val alloc_lvals :
  coq_PointerData -> pos_map -> region_map -> lval list -> atype list ->
  (pp_error_loc, region_map * lval list) result

val incl_interval : intervals -> intervals -> bool

val incl_status : status -> status -> bool

val incl_status_map : status_map -> status_map -> bool

val incl : region_map -> region_map -> bool

val typecheck : sexpr -> (pp_error_loc, atype) result

val typecheck_slice : symbolic_slice -> bool

val merge_interval : intervals -> intervals -> intervals option

val read_e_rec : SvExtra.Sv.t -> sexpr -> SvExtra.Sv.t

val read_e : sexpr -> SvExtra.Sv.t

val read_slice : symbolic_slice -> SvExtra.Sv.t

val merge_status :
  SvExtra.Sv.t -> Var.var -> status option -> status option -> status option

val merge_status_map :
  SvExtra.Sv.t -> region -> status_map option -> status_map option -> status
  Mvar.t option

val merge : SvExtra.Sv.t -> region_map -> region_map -> region_map

val incl_table : table -> table -> bool

val loop2 :
  'a1 asmOp -> instr_info -> (table -> region_map ->
  (((table * region_map) * (table * region_map)) * ((pexpr * 'a1 instr list
  list) * 'a1 instr list list)) cexec) -> nat -> table -> region_map ->
  (pp_error_loc, (table * region_map) * ((pexpr * 'a1 instr list list) * 'a1
  instr list list)) result

type stk_alloc_oracle_t = { sao_align : wsize; sao_size : coq_Z;
                            sao_ioff : coq_Z; sao_extra_size : coq_Z;
                            sao_max_size : coq_Z; sao_max_call_depth : 
                            coq_Z; sao_params : param_info option list;
                            sao_return : nat option list;
                            sao_slots : ((Var.var * wsize) * coq_Z) list;
                            sao_alloc : (Var.var * ptr_kind_init) list;
                            sao_to_save : (Var.var * coq_Z) list;
                            sao_rsp : saved_stack;
                            sao_return_address : return_address_location }

val sao_frame_size : stk_alloc_oracle_t -> coq_Z

val get_Pvar : pexpr -> (pp_error_loc, gvar) result

val alloc_call_arg_aux :
  pos_map -> region_map -> region_map -> param_info option -> pexpr ->
  (pp_error_loc, region_map * ((bool * sub_region) option * pexpr)) result

val alloc_call_args_aux :
  pos_map -> region_map -> param_info option list -> pexpr list ->
  (pp_error_loc, region_map * ((bool * sub_region) option * pexpr) list)
  result

val disjoint_zones : Equality.sort list -> Equality.sort list -> bool

val disj_sub_regions : sub_region -> sub_region -> bool

val check_all_disj :
  sub_region list -> sub_region list -> ((bool * sub_region) option * pexpr)
  list -> bool

val alloc_call_args :
  pos_map -> region_map -> funname -> param_info option list -> pexpr list ->
  (pp_error_loc, region_map * ((bool * sub_region) option * pexpr) list)
  result

val check_lval_reg_call : pos_map -> lval -> (pp_error_loc, unit) result

val get_regptr : pos_map -> var_i -> (pp_error_loc, var_i) result

val alloc_lval_call :
  coq_PointerData -> pos_map -> ((bool * sub_region) option * pexpr) list ->
  region_map -> lval -> nat option -> (pp_error_loc, region_map * lval) result

val alloc_call_res :
  coq_PointerData -> pos_map -> region_map -> ((bool * sub_region)
  option * pexpr) list -> nat option list -> lval list -> (pp_error_loc,
  region_map * lval list) result

val alloc_call :
  bool -> coq_PointerData -> 'a1 asmOp -> pos_map -> (funname ->
  stk_alloc_oracle_t) -> stk_alloc_oracle_t -> region_map -> lval list ->
  funname -> pexpr list -> (pp_error_loc, region_map * 'a1 instr_r) result

val alloc_syscall :
  coq_PointerData -> 'a1 asmOp -> 'a1 stack_alloc_params -> pos_map ->
  instr_info -> region_map -> lval list ->
  (Wsize.wsize * BinNums.positive) Syscall_t.syscall_t -> pexpr list ->
  (pp_error_loc, region_map * 'a1 instr list) result

val is_swap_array : 'a1 asmOp -> 'a1 sopn -> bool

val alloc_array_swap :
  'a1 asmOp -> 'a1 stack_alloc_params -> pos_map -> region_map -> lval list
  -> assgn_tag -> pexpr list -> (pp_error_loc, region_map * 'a1 instr_r)
  result

val is_declassify_array : 'a1 asmOp -> 'a1 sopn -> bool

val alloc_declassify_array :
  coq_PointerData -> 'a1 asmOp -> pos_map -> region_map -> pexpr list ->
  (pp_error_loc, 'a1 instr_r) result

val alloc_i :
  bool -> coq_PointerData -> coq_MSFsize -> 'a1 asmOp -> 'a1 sh_params -> 'a1
  stack_alloc_params -> ('a1 asm_op_t -> bool) -> (v_kind -> Uint63.t ->
  string -> atype -> Ident.Ident.ident) -> (sub_region -> pp_error) ->
  pos_map -> (funname -> stk_alloc_oracle_t) -> 'a1 _uprog ->
  stk_alloc_oracle_t -> (table * region_map) -> 'a1 instr ->
  ((table * region_map) * 'a1 instr list) cexec

val init_stack_layout :
  (coq_Z * wsize) Mvar.t -> stk_alloc_oracle_t -> (pp_error_loc,
  (coq_Z * wsize) Mvar.t) result

val add_alloc :
  coq_PointerData -> (coq_Z * wsize) Mvar.t -> (coq_Z * wsize) Mvar.t ->
  (Var.var * ptr_kind_init) -> ((ptr_kind
  Mvar.t * region_map) * SvExtra.Sv.t) -> (pp_error_loc, (ptr_kind
  Mvar.Map.t * region_map) * SvExtra.Sv.t) result

val init_local_map :
  coq_PointerData -> SvExtra.Sv.elt -> SvExtra.Sv.elt -> SvExtra.Sv.elt ->
  (coq_Z * wsize) Mvar.t -> (coq_Z * wsize) Mvar.t -> stk_alloc_oracle_t ->
  (pp_error_loc, (ptr_kind Mvar.t * region_map) * SvExtra.Sv.t) result

val check_result :
  pos_map -> region_map -> Equality.sort option list -> var_i list -> nat
  option -> var_i -> (pp_error_loc, var_i) result

val check_all_writable_regions_returned :
  sub_region option list -> nat option list -> bool

val check_results :
  pos_map -> region_map -> Equality.sort option list -> var_i list -> nat
  option list -> var_i list -> (pp_error_loc, var_i list) result

val init_param :
  coq_PointerData -> (coq_Z * wsize) Mvar.t -> (coq_Z * wsize) Mvar.t ->
  ((SvExtra.Sv.t * ptr_kind Mvar.t) * region_map) -> param_info option ->
  var_i -> (pp_error_loc, ((SvExtra.Sv.t * ptr_kind
  Mvar.Map.t) * region_map) * (sub_region option * var_i)) result

val init_params :
  coq_PointerData -> (coq_Z * wsize) Mvar.t -> (coq_Z * wsize) Mvar.t ->
  SvExtra.Sv.t -> ptr_kind Mvar.t -> region_map -> param_info option list ->
  var_i list -> (pp_error_loc, ((SvExtra.Sv.t * ptr_kind
  Mvar.t) * region_map) * (sub_region option * var_i) list) result

val fresh_reg :
  (v_kind -> Uint63.t -> string -> atype -> Ident.Ident.ident) -> string ->
  atype -> Ident.Ident.ident

val alloc_fd_aux :
  bool -> coq_PointerData -> coq_MSFsize -> 'a1 asmOp -> 'a1 sh_params -> 'a1
  stack_alloc_params -> ('a1 asm_op_t -> bool) -> (v_kind -> Uint63.t ->
  string -> atype -> Ident.Ident.ident) -> (sub_region -> pp_error) -> 'a1
  _uprog -> sprog_extra -> (coq_Z * wsize) Mvar.t -> (funname ->
  stk_alloc_oracle_t) -> stk_alloc_oracle_t -> ('a1, unit) _fundef -> 'a1
  _ufundef cexec

val alloc_fd :
  bool -> coq_PointerData -> coq_MSFsize -> 'a1 asmOp -> 'a1 sh_params -> 'a1
  stack_alloc_params -> ('a1 asm_op_t -> bool) -> (v_kind -> Uint63.t ->
  string -> atype -> Ident.Ident.ident) -> (sub_region -> pp_error) -> 'a1
  _uprog -> sprog_extra -> (coq_Z * wsize) Mvar.t -> (funname ->
  stk_alloc_oracle_t) -> funname -> ('a1, unit) _fundef -> (pp_error_loc, 'a1
  sfundef) result

val ptake : positive -> 'a1 list -> 'a1 list -> ('a1 list * 'a1 list) option

val ztake : coq_Z -> 'a1 list -> ('a1 list * 'a1 list) option

val check_glob :
  GRing.ComRing.sort list -> glob_value -> (pp_error_loc, unit) result

val size_glob : glob_value -> coq_Z

val init_map :
  ((Var.var * wsize) * coq_Z) list -> GRing.ComRing.sort list -> glob_decl
  list -> (coq_Z * wsize) Mvar.t cexec

val alloc_prog :
  bool -> coq_PointerData -> coq_MSFsize -> 'a1 asmOp -> 'a1 sh_params -> 'a1
  stack_alloc_params -> ('a1 asm_op_t -> bool) -> (v_kind -> Uint63.t ->
  string -> atype -> Ident.Ident.ident) -> (sub_region -> pp_error) ->
  Ident.Ident.ident -> Ident.Ident.ident -> GRing.ComRing.sort list ->
  ((Var.var * wsize) * coq_Z) list -> (funname -> stk_alloc_oracle_t) -> 'a1
  _uprog -> 'a1 _sprog cexec

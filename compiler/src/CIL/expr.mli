open BinInt
open BinNums
open Bool
open Datatypes
open Div
open EqbOK
open Eqb_core_defs
open Eqtype
open Global
open Memory_model
open Seq
open Sopn
open Ssralg
open Ssrbool
open Ssrfun
open Type
open Utils0
open Var0
open Warray_
open Word0
open Word_ssrZ
open Wsize


type __ = Obj.t

type cmp_kind =
| Cmp_int
| Cmp_w of signedness * wsize

type is_cmp_kind =
| Coq_is_Cmp_int
| Coq_is_Cmp_w of signedness * is_signedness * wsize * is_wsize

val cmp_kind_tag : cmp_kind -> positive

val is_cmp_kind_inhab : cmp_kind -> is_cmp_kind

val is_cmp_kind_functor : cmp_kind -> is_cmp_kind -> is_cmp_kind

type box_cmp_kind_Cmp_int =
| Box_cmp_kind_Cmp_int

type box_cmp_kind_Cmp_w = { coq_Box_cmp_kind_Cmp_w_0 : signedness;
                            coq_Box_cmp_kind_Cmp_w_1 : wsize }

val coq_Box_cmp_kind_Cmp_w_0 : box_cmp_kind_Cmp_w -> signedness

val coq_Box_cmp_kind_Cmp_w_1 : box_cmp_kind_Cmp_w -> wsize

type cmp_kind_fields_t = __

val cmp_kind_fields : cmp_kind -> cmp_kind_fields_t

val cmp_kind_construct : positive -> cmp_kind_fields_t -> cmp_kind option

val cmp_kind_induction :
  'a1 -> (signedness -> is_signedness -> wsize -> is_wsize -> 'a1) ->
  cmp_kind -> is_cmp_kind -> 'a1

val cmp_kind_eqb_fields :
  (cmp_kind -> cmp_kind -> bool) -> positive -> cmp_kind_fields_t ->
  cmp_kind_fields_t -> bool

val cmp_kind_eqb : cmp_kind -> cmp_kind -> bool

val cmp_kind_eqb_OK : cmp_kind -> cmp_kind -> reflect

val cmp_kind_eqb_OK_sumbool : cmp_kind -> cmp_kind -> bool

type op_kind =
| Op_int
| Op_w of wsize

type is_op_kind =
| Coq_is_Op_int
| Coq_is_Op_w of wsize * is_wsize

val op_kind_tag : op_kind -> positive

val is_op_kind_inhab : op_kind -> is_op_kind

val is_op_kind_functor : op_kind -> is_op_kind -> is_op_kind

type box_op_kind_Op_int =
| Box_op_kind_Op_int

type box_op_kind_Op_w =
  wsize
  (* singleton inductive, whose constructor was Box_op_kind_Op_w *)

val coq_Box_op_kind_Op_w_0 : box_op_kind_Op_w -> wsize

type op_kind_fields_t = __

val op_kind_fields : op_kind -> op_kind_fields_t

val op_kind_construct : positive -> op_kind_fields_t -> op_kind option

val op_kind_induction :
  'a1 -> (wsize -> is_wsize -> 'a1) -> op_kind -> is_op_kind -> 'a1

val op_kind_eqb_fields :
  (op_kind -> op_kind -> bool) -> positive -> op_kind_fields_t ->
  op_kind_fields_t -> bool

val op_kind_eqb : op_kind -> op_kind -> bool

val op_kind_eqb_OK : op_kind -> op_kind -> reflect

val op_kind_eqb_OK_sumbool : op_kind -> op_kind -> bool

type wiop1 =
| WIwint_of_int of wsize
| WIint_of_wint of wsize
| WIword_of_wint of wsize
| WIwint_of_word of wsize
| WIwint_ext of wsize * wsize
| WIneg of wsize

type is_wiop1 =
| Coq_is_WIwint_of_int of wsize * is_wsize
| Coq_is_WIint_of_wint of wsize * is_wsize
| Coq_is_WIword_of_wint of wsize * is_wsize
| Coq_is_WIwint_of_word of wsize * is_wsize
| Coq_is_WIwint_ext of wsize * is_wsize * wsize * is_wsize
| Coq_is_WIneg of wsize * is_wsize

val wiop1_tag : wiop1 -> positive

val is_wiop1_inhab : wiop1 -> is_wiop1

val is_wiop1_functor : wiop1 -> is_wiop1 -> is_wiop1

type box_wiop1_WIwint_of_int =
  wsize
  (* singleton inductive, whose constructor was Box_wiop1_WIwint_of_int *)

val coq_Box_wiop1_WIwint_of_int_0 : box_wiop1_WIwint_of_int -> wsize

type box_wiop1_WIwint_ext = { coq_Box_wiop1_WIwint_ext_0 : wsize;
                              coq_Box_wiop1_WIwint_ext_1 : wsize }

val coq_Box_wiop1_WIwint_ext_0 : box_wiop1_WIwint_ext -> wsize

val coq_Box_wiop1_WIwint_ext_1 : box_wiop1_WIwint_ext -> wsize

type wiop1_fields_t = __

val wiop1_fields : wiop1 -> wiop1_fields_t

val wiop1_construct : positive -> wiop1_fields_t -> wiop1 option

val wiop1_induction :
  (wsize -> is_wsize -> 'a1) -> (wsize -> is_wsize -> 'a1) -> (wsize ->
  is_wsize -> 'a1) -> (wsize -> is_wsize -> 'a1) -> (wsize -> is_wsize ->
  wsize -> is_wsize -> 'a1) -> (wsize -> is_wsize -> 'a1) -> wiop1 ->
  is_wiop1 -> 'a1

val wiop1_eqb_fields :
  (wiop1 -> wiop1 -> bool) -> positive -> wiop1_fields_t -> wiop1_fields_t ->
  bool

val wiop1_eqb : wiop1 -> wiop1 -> bool

val wiop1_eqb_OK : wiop1 -> wiop1 -> reflect

val wiop1_eqb_OK_sumbool : wiop1 -> wiop1 -> bool

type sop1 =
| Oword_of_int of wsize
| Oint_of_word of signedness * wsize
| Osignext of wsize * wsize
| Ozeroext of wsize * wsize
| Onot
| Olnot of wsize
| Oneg of op_kind
| Owi1 of signedness * wiop1

type is_sop1 =
| Coq_is_Oword_of_int of wsize * is_wsize
| Coq_is_Oint_of_word of signedness * is_signedness * wsize * is_wsize
| Coq_is_Osignext of wsize * is_wsize * wsize * is_wsize
| Coq_is_Ozeroext of wsize * is_wsize * wsize * is_wsize
| Coq_is_Onot
| Coq_is_Olnot of wsize * is_wsize
| Coq_is_Oneg of op_kind * is_op_kind
| Coq_is_Owi1 of signedness * is_signedness * wiop1 * is_wiop1

val sop1_tag : sop1 -> positive

val is_sop1_inhab : sop1 -> is_sop1

val is_sop1_functor : sop1 -> is_sop1 -> is_sop1

type box_sop1_Oword_of_int =
  wsize
  (* singleton inductive, whose constructor was Box_sop1_Oword_of_int *)

val coq_Box_sop1_Oword_of_int_0 : box_sop1_Oword_of_int -> wsize

type box_sop1_Oint_of_word = { coq_Box_sop1_Oint_of_word_0 : signedness;
                               coq_Box_sop1_Oint_of_word_1 : wsize }

val coq_Box_sop1_Oint_of_word_0 : box_sop1_Oint_of_word -> signedness

val coq_Box_sop1_Oint_of_word_1 : box_sop1_Oint_of_word -> wsize

type box_sop1_Osignext = { coq_Box_sop1_Osignext_0 : wsize;
                           coq_Box_sop1_Osignext_1 : wsize }

val coq_Box_sop1_Osignext_0 : box_sop1_Osignext -> wsize

val coq_Box_sop1_Osignext_1 : box_sop1_Osignext -> wsize

type box_sop1_Onot =
| Box_sop1_Onot

type box_sop1_Oneg =
  op_kind
  (* singleton inductive, whose constructor was Box_sop1_Oneg *)

val coq_Box_sop1_Oneg_0 : box_sop1_Oneg -> op_kind

type box_sop1_Owi1 = { coq_Box_sop1_Owi1_0 : signedness;
                       coq_Box_sop1_Owi1_1 : wiop1 }

val coq_Box_sop1_Owi1_0 : box_sop1_Owi1 -> signedness

val coq_Box_sop1_Owi1_1 : box_sop1_Owi1 -> wiop1

type sop1_fields_t = __

val sop1_fields : sop1 -> sop1_fields_t

val sop1_construct : positive -> sop1_fields_t -> sop1 option

val sop1_induction :
  (wsize -> is_wsize -> 'a1) -> (signedness -> is_signedness -> wsize ->
  is_wsize -> 'a1) -> (wsize -> is_wsize -> wsize -> is_wsize -> 'a1) ->
  (wsize -> is_wsize -> wsize -> is_wsize -> 'a1) -> 'a1 -> (wsize ->
  is_wsize -> 'a1) -> (op_kind -> is_op_kind -> 'a1) -> (signedness ->
  is_signedness -> wiop1 -> is_wiop1 -> 'a1) -> sop1 -> is_sop1 -> 'a1

val sop1_eqb_fields :
  (sop1 -> sop1 -> bool) -> positive -> sop1_fields_t -> sop1_fields_t -> bool

val sop1_eqb : sop1 -> sop1 -> bool

val sop1_eqb_OK : sop1 -> sop1 -> reflect

val sop1_eqb_OK_sumbool : sop1 -> sop1 -> bool

val uint_of_word : wsize -> sop1

val sint_of_word : wsize -> sop1

type wiop2 =
| WIadd
| WImul
| WIsub
| WIdiv
| WImod
| WIshl
| WIshr
| WIeq
| WIneq
| WIlt
| WIle
| WIgt
| WIge

type is_wiop2 =
| Coq_is_WIadd
| Coq_is_WImul
| Coq_is_WIsub
| Coq_is_WIdiv
| Coq_is_WImod
| Coq_is_WIshl
| Coq_is_WIshr
| Coq_is_WIeq
| Coq_is_WIneq
| Coq_is_WIlt
| Coq_is_WIle
| Coq_is_WIgt
| Coq_is_WIge

val wiop2_tag : wiop2 -> positive

val is_wiop2_inhab : wiop2 -> is_wiop2

val is_wiop2_functor : wiop2 -> is_wiop2 -> is_wiop2

type box_wiop2_WIadd =
| Box_wiop2_WIadd

type wiop2_fields_t = __

val wiop2_fields : wiop2 -> wiop2_fields_t

val wiop2_construct : positive -> wiop2_fields_t -> wiop2 option

val wiop2_induction :
  'a1 -> 'a1 -> 'a1 -> 'a1 -> 'a1 -> 'a1 -> 'a1 -> 'a1 -> 'a1 -> 'a1 -> 'a1
  -> 'a1 -> 'a1 -> wiop2 -> is_wiop2 -> 'a1

val wiop2_eqb_fields :
  (wiop2 -> wiop2 -> bool) -> positive -> wiop2_fields_t -> wiop2_fields_t ->
  bool

val wiop2_eqb : wiop2 -> wiop2 -> bool

val wiop2_eqb_OK : wiop2 -> wiop2 -> reflect

val wiop2_eqb_OK_sumbool : wiop2 -> wiop2 -> bool

type sop2 =
| Obeq
| Oand
| Oor
| Oadd of op_kind
| Omul of op_kind
| Osub of op_kind
| Odiv of signedness * op_kind
| Omod of signedness * op_kind
| Oland of wsize
| Olor of wsize
| Olxor of wsize
| Olsr of wsize
| Olsl of op_kind
| Oasr of op_kind
| Oror of wsize
| Orol of wsize
| Oeq of op_kind
| Oneq of op_kind
| Olt of cmp_kind
| Ole of cmp_kind
| Ogt of cmp_kind
| Oge of cmp_kind
| Ovadd of velem * wsize
| Ovsub of velem * wsize
| Ovmul of velem * wsize
| Ovlsr of velem * wsize
| Ovlsl of velem * wsize
| Ovasr of velem * wsize
| Owi2 of signedness * wsize * wiop2

type is_sop2 =
| Coq_is_Obeq
| Coq_is_Oand
| Coq_is_Oor
| Coq_is_Oadd of op_kind * is_op_kind
| Coq_is_Omul of op_kind * is_op_kind
| Coq_is_Osub of op_kind * is_op_kind
| Coq_is_Odiv of signedness * is_signedness * op_kind * is_op_kind
| Coq_is_Omod of signedness * is_signedness * op_kind * is_op_kind
| Coq_is_Oland of wsize * is_wsize
| Coq_is_Olor of wsize * is_wsize
| Coq_is_Olxor of wsize * is_wsize
| Coq_is_Olsr of wsize * is_wsize
| Coq_is_Olsl of op_kind * is_op_kind
| Coq_is_Oasr of op_kind * is_op_kind
| Coq_is_Oror of wsize * is_wsize
| Coq_is_Orol of wsize * is_wsize
| Coq_is_Oeq of op_kind * is_op_kind
| Coq_is_Oneq of op_kind * is_op_kind
| Coq_is_Olt of cmp_kind * is_cmp_kind
| Coq_is_Ole of cmp_kind * is_cmp_kind
| Coq_is_Ogt of cmp_kind * is_cmp_kind
| Coq_is_Oge of cmp_kind * is_cmp_kind
| Coq_is_Ovadd of velem * is_velem * wsize * is_wsize
| Coq_is_Ovsub of velem * is_velem * wsize * is_wsize
| Coq_is_Ovmul of velem * is_velem * wsize * is_wsize
| Coq_is_Ovlsr of velem * is_velem * wsize * is_wsize
| Coq_is_Ovlsl of velem * is_velem * wsize * is_wsize
| Coq_is_Ovasr of velem * is_velem * wsize * is_wsize
| Coq_is_Owi2 of signedness * is_signedness * wsize * is_wsize * wiop2
   * is_wiop2

val sop2_tag : sop2 -> positive

val is_sop2_inhab : sop2 -> is_sop2

val is_sop2_functor : sop2 -> is_sop2 -> is_sop2

type box_sop2_Obeq =
| Box_sop2_Obeq

type box_sop2_Oadd =
  op_kind
  (* singleton inductive, whose constructor was Box_sop2_Oadd *)

val coq_Box_sop2_Oadd_0 : box_sop2_Oadd -> op_kind

type box_sop2_Odiv = { coq_Box_sop2_Odiv_0 : signedness;
                       coq_Box_sop2_Odiv_1 : op_kind }

val coq_Box_sop2_Odiv_0 : box_sop2_Odiv -> signedness

val coq_Box_sop2_Odiv_1 : box_sop2_Odiv -> op_kind

type box_sop2_Oland =
  wsize
  (* singleton inductive, whose constructor was Box_sop2_Oland *)

val coq_Box_sop2_Oland_0 : box_sop2_Oland -> wsize

type box_sop2_Olt =
  cmp_kind
  (* singleton inductive, whose constructor was Box_sop2_Olt *)

val coq_Box_sop2_Olt_0 : box_sop2_Olt -> cmp_kind

type box_sop2_Ovadd = { coq_Box_sop2_Ovadd_0 : velem;
                        coq_Box_sop2_Ovadd_1 : wsize }

val coq_Box_sop2_Ovadd_0 : box_sop2_Ovadd -> velem

val coq_Box_sop2_Ovadd_1 : box_sop2_Ovadd -> wsize

type box_sop2_Owi2 = { coq_Box_sop2_Owi2_0 : signedness;
                       coq_Box_sop2_Owi2_1 : wsize;
                       coq_Box_sop2_Owi2_2 : wiop2 }

val coq_Box_sop2_Owi2_0 : box_sop2_Owi2 -> signedness

val coq_Box_sop2_Owi2_1 : box_sop2_Owi2 -> wsize

val coq_Box_sop2_Owi2_2 : box_sop2_Owi2 -> wiop2

type sop2_fields_t = __

val sop2_fields : sop2 -> sop2_fields_t

val sop2_construct : positive -> sop2_fields_t -> sop2 option

val sop2_induction :
  'a1 -> 'a1 -> 'a1 -> (op_kind -> is_op_kind -> 'a1) -> (op_kind ->
  is_op_kind -> 'a1) -> (op_kind -> is_op_kind -> 'a1) -> (signedness ->
  is_signedness -> op_kind -> is_op_kind -> 'a1) -> (signedness ->
  is_signedness -> op_kind -> is_op_kind -> 'a1) -> (wsize -> is_wsize ->
  'a1) -> (wsize -> is_wsize -> 'a1) -> (wsize -> is_wsize -> 'a1) -> (wsize
  -> is_wsize -> 'a1) -> (op_kind -> is_op_kind -> 'a1) -> (op_kind ->
  is_op_kind -> 'a1) -> (wsize -> is_wsize -> 'a1) -> (wsize -> is_wsize ->
  'a1) -> (op_kind -> is_op_kind -> 'a1) -> (op_kind -> is_op_kind -> 'a1) ->
  (cmp_kind -> is_cmp_kind -> 'a1) -> (cmp_kind -> is_cmp_kind -> 'a1) ->
  (cmp_kind -> is_cmp_kind -> 'a1) -> (cmp_kind -> is_cmp_kind -> 'a1) ->
  (velem -> is_velem -> wsize -> is_wsize -> 'a1) -> (velem -> is_velem ->
  wsize -> is_wsize -> 'a1) -> (velem -> is_velem -> wsize -> is_wsize ->
  'a1) -> (velem -> is_velem -> wsize -> is_wsize -> 'a1) -> (velem ->
  is_velem -> wsize -> is_wsize -> 'a1) -> (velem -> is_velem -> wsize ->
  is_wsize -> 'a1) -> (signedness -> is_signedness -> wsize -> is_wsize ->
  wiop2 -> is_wiop2 -> 'a1) -> sop2 -> is_sop2 -> 'a1

val sop2_eqb_fields :
  (sop2 -> sop2 -> bool) -> positive -> sop2_fields_t -> sop2_fields_t -> bool

val sop2_eqb : sop2 -> sop2 -> bool

val sop2_eqb_OK : sop2 -> sop2 -> reflect

val sop2_eqb_OK_sumbool : sop2 -> sop2 -> bool

type combine_flags =
| CF_LT of signedness
| CF_LE of signedness
| CF_EQ
| CF_NEQ
| CF_GE of signedness
| CF_GT of signedness

type is_combine_flags =
| Coq_is_CF_LT of signedness * is_signedness
| Coq_is_CF_LE of signedness * is_signedness
| Coq_is_CF_EQ
| Coq_is_CF_NEQ
| Coq_is_CF_GE of signedness * is_signedness
| Coq_is_CF_GT of signedness * is_signedness

val combine_flags_tag : combine_flags -> positive

val is_combine_flags_inhab : combine_flags -> is_combine_flags

val is_combine_flags_functor :
  combine_flags -> is_combine_flags -> is_combine_flags

type box_combine_flags_CF_LT =
  signedness
  (* singleton inductive, whose constructor was Box_combine_flags_CF_LT *)

val coq_Box_combine_flags_CF_LT_0 : box_combine_flags_CF_LT -> signedness

type box_combine_flags_CF_EQ =
| Box_combine_flags_CF_EQ

type combine_flags_fields_t = __

val combine_flags_fields : combine_flags -> combine_flags_fields_t

val combine_flags_construct :
  positive -> combine_flags_fields_t -> combine_flags option

val combine_flags_induction :
  (signedness -> is_signedness -> 'a1) -> (signedness -> is_signedness ->
  'a1) -> 'a1 -> 'a1 -> (signedness -> is_signedness -> 'a1) -> (signedness
  -> is_signedness -> 'a1) -> combine_flags -> is_combine_flags -> 'a1

val combine_flags_eqb_fields :
  (combine_flags -> combine_flags -> bool) -> positive ->
  combine_flags_fields_t -> combine_flags_fields_t -> bool

val combine_flags_eqb : combine_flags -> combine_flags -> bool

val combine_flags_eqb_OK : combine_flags -> combine_flags -> reflect

val combine_flags_eqb_OK_sumbool : combine_flags -> combine_flags -> bool

type opN =
| Opack of wsize * pelem
| Ocombine_flags of combine_flags

type is_opN =
| Coq_is_Opack of wsize * is_wsize * pelem * is_pelem
| Coq_is_Ocombine_flags of combine_flags * is_combine_flags

val opN_tag : opN -> positive

val is_opN_inhab : opN -> is_opN

val is_opN_functor : opN -> is_opN -> is_opN

type box_opN_Opack = { coq_Box_opN_Opack_0 : wsize;
                       coq_Box_opN_Opack_1 : pelem }

val coq_Box_opN_Opack_0 : box_opN_Opack -> wsize

val coq_Box_opN_Opack_1 : box_opN_Opack -> pelem

type box_opN_Ocombine_flags =
  combine_flags
  (* singleton inductive, whose constructor was Box_opN_Ocombine_flags *)

val coq_Box_opN_Ocombine_flags_0 : box_opN_Ocombine_flags -> combine_flags

type opN_fields_t = __

val opN_fields : opN -> opN_fields_t

val opN_construct : positive -> opN_fields_t -> opN option

val opN_induction :
  (wsize -> is_wsize -> pelem -> is_pelem -> 'a1) -> (combine_flags ->
  is_combine_flags -> 'a1) -> opN -> is_opN -> 'a1

val opN_eqb_fields :
  (opN -> opN -> bool) -> positive -> opN_fields_t -> opN_fields_t -> bool

val opN_eqb : opN -> opN -> bool

val opN_eqb_OK : opN -> opN -> reflect

val opN_eqb_OK_sumbool : opN -> opN -> bool

val coq_HB_unnamed_factory_1 : op_kind Coq_hasDecEq.axioms_

val expr_op_kind__canonical__eqtype_Equality : Equality.coq_type

val coq_HB_unnamed_factory_3 : sop1 Coq_hasDecEq.axioms_

val expr_sop1__canonical__eqtype_Equality : Equality.coq_type

val coq_HB_unnamed_factory_5 : sop2 Coq_hasDecEq.axioms_

val expr_sop2__canonical__eqtype_Equality : Equality.coq_type

val coq_HB_unnamed_factory_7 : opN Coq_hasDecEq.axioms_

val expr_opN__canonical__eqtype_Equality : Equality.coq_type

val etype_of_wiop1 :
  signedness -> wiop1 -> 'a1 extended_type * 'a1 extended_type

val type_of_wiop1 : wiop1 -> atype * atype

val type_of_opk : op_kind -> atype

val etype_of_opk : op_kind -> 'a1 extended_type

val etype_of_op1 : sop1 -> 'a1 extended_type * 'a1 extended_type

val type_of_op1 : sop1 -> atype * atype

val etype_of_wiop2 :
  signedness -> wsize -> wiop2 -> ('a1 extended_type * 'a1
  extended_type) * 'a1 extended_type

val type_of_wiop2 : wsize -> wiop2 -> (atype * atype) * atype

val opk8 : op_kind -> op_kind

val opk_of_cmpk : cmp_kind -> op_kind

val etype_of_op2 :
  sop2 -> ('a1 extended_type * 'a1 extended_type) * 'a1 extended_type

val type_of_op2 : sop2 -> (atype * atype) * atype

val tin_combine_flags : atype list

val type_of_opN : opN -> atype list * atype

module type TAG =
 sig
  type t

  val witness : t
 end

module VarInfo :
 TAG

type var_info = Location.t

val dummy_var_info : var_info

type var_i = { v_var : Var.var; v_info : var_info }

val v_var : var_i -> Var.var

val v_info : var_i -> var_info

val mk_var_i : Var.var -> var_i

type v_scope =
| Slocal
| Sglob

type is_v_scope =
| Coq_is_Slocal
| Coq_is_Sglob

val v_scope_tag : v_scope -> positive

val is_v_scope_inhab : v_scope -> is_v_scope

val is_v_scope_functor : v_scope -> is_v_scope -> is_v_scope

type box_v_scope_Slocal =
| Box_v_scope_Slocal

type v_scope_fields_t = __

val v_scope_fields : v_scope -> v_scope_fields_t

val v_scope_construct : positive -> v_scope_fields_t -> v_scope option

val v_scope_induction : 'a1 -> 'a1 -> v_scope -> is_v_scope -> 'a1

val v_scope_eqb_fields :
  (v_scope -> v_scope -> bool) -> positive -> v_scope_fields_t ->
  v_scope_fields_t -> bool

val v_scope_eqb : v_scope -> v_scope -> bool

val v_scope_eqb_OK : v_scope -> v_scope -> reflect

val v_scope_eqb_OK_sumbool : v_scope -> v_scope -> bool

val coq_HB_unnamed_factory_9 : v_scope Coq_hasDecEq.axioms_

val expr_v_scope__canonical__eqtype_Equality : Equality.coq_type

type gvar = { gv : var_i; gs : v_scope }

val gv : gvar -> var_i

val gs : gvar -> v_scope

val mk_gvar : var_i -> gvar

val mk_lvar : var_i -> gvar

val is_lvar : gvar -> bool

val is_glob : gvar -> bool

type pexpr =
| Pconst of coq_Z
| Pbool of bool
| Parr_init of wsize * positive
| Pvar of gvar
| Pget of aligned * arr_access * wsize * gvar * pexpr
| Psub of arr_access * wsize * positive * gvar * pexpr
| Pload of aligned * wsize * pexpr
| Papp1 of sop1 * pexpr
| Papp2 of sop2 * pexpr * pexpr
| PappN of opN * pexpr list
| Pif of atype * pexpr * pexpr * pexpr

val coq_Plvar : var_i -> pexpr

val enot : pexpr -> pexpr

val eor : pexpr -> pexpr -> pexpr

val eand : pexpr -> pexpr -> pexpr

val eeq : pexpr -> pexpr -> pexpr

val eneq : pexpr -> pexpr -> pexpr

val eaddw : wsize -> pexpr -> pexpr -> pexpr

val cf_of_condition : sop2 -> (combine_flags * wsize) option

val pexpr_of_cf : combine_flags -> var_info -> Var.var list -> pexpr

type lval =
| Lnone of var_info * atype
| Lvar of var_i
| Lmem of aligned * wsize * var_info * pexpr
| Laset of aligned * arr_access * wsize * var_i * pexpr
| Lasub of arr_access * wsize * positive * var_i * pexpr

val get_pvar : pexpr -> Var.var exec

val get_lvar : lval -> Var.var exec

val coq_Lnone_b : var_info -> lval

val var_info_of_lval : lval -> var_info

type dir =
| UpTo
| DownTo

type is_dir =
| Coq_is_UpTo
| Coq_is_DownTo

val dir_tag : dir -> positive

val is_dir_inhab : dir -> is_dir

val is_dir_functor : dir -> is_dir -> is_dir

type box_dir_UpTo =
| Box_dir_UpTo

type dir_fields_t = __

val dir_fields : dir -> dir_fields_t

val dir_construct : positive -> dir_fields_t -> dir option

val dir_induction : 'a1 -> 'a1 -> dir -> is_dir -> 'a1

val dir_eqb_fields :
  (dir -> dir -> bool) -> positive -> dir_fields_t -> dir_fields_t -> bool

val dir_eqb : dir -> dir -> bool

val dir_eqb_OK : dir -> dir -> reflect

val dir_eqb_OK_sumbool : dir -> dir -> bool

val coq_HB_unnamed_factory_11 : dir Coq_hasDecEq.axioms_

val expr_dir__canonical__eqtype_Equality : Equality.coq_type

type range = (dir * pexpr) * pexpr

val wrange : dir -> coq_Z -> coq_Z -> coq_Z list

module type InstrInfoT =
 sig
  type t

  val witness : t

  val with_location : t -> t

  val is_inline : t -> bool

  val var_info_of_ii : t -> var_info
 end

module InstrInfo :
 InstrInfoT

type instr_info = IInfo.t

val dummy_instr_info : instr_info

val ii_with_location : instr_info -> instr_info

val ii_is_inline : instr_info -> bool

val var_info_of_ii : instr_info -> var_info

type assgn_tag =
| AT_none
| AT_keep
| AT_rename
| AT_inline
| AT_phinode

type is_assgn_tag =
| Coq_is_AT_none
| Coq_is_AT_keep
| Coq_is_AT_rename
| Coq_is_AT_inline
| Coq_is_AT_phinode

val assgn_tag_tag : assgn_tag -> positive

val is_assgn_tag_inhab : assgn_tag -> is_assgn_tag

val is_assgn_tag_functor : assgn_tag -> is_assgn_tag -> is_assgn_tag

type box_assgn_tag_AT_none =
| Box_assgn_tag_AT_none

type assgn_tag_fields_t = __

val assgn_tag_fields : assgn_tag -> assgn_tag_fields_t

val assgn_tag_construct : positive -> assgn_tag_fields_t -> assgn_tag option

val assgn_tag_induction :
  'a1 -> 'a1 -> 'a1 -> 'a1 -> 'a1 -> assgn_tag -> is_assgn_tag -> 'a1

val assgn_tag_eqb_fields :
  (assgn_tag -> assgn_tag -> bool) -> positive -> assgn_tag_fields_t ->
  assgn_tag_fields_t -> bool

val assgn_tag_eqb : assgn_tag -> assgn_tag -> bool

val assgn_tag_eqb_OK : assgn_tag -> assgn_tag -> reflect

val assgn_tag_eqb_OK_sumbool : assgn_tag -> assgn_tag -> bool

val coq_HB_unnamed_factory_13 : assgn_tag Coq_hasDecEq.axioms_

val expr_assgn_tag__canonical__eqtype_Equality : Equality.coq_type

type align =
| Align
| NoAlign

type 'asm_op instr_r =
| Cassgn of lval * assgn_tag * atype * pexpr
| Copn of lval list * assgn_tag * 'asm_op sopn * pexpr list
| Csyscall of lval list
   * (Wsize.wsize * BinNums.positive) Syscall_t.syscall_t * pexpr list
| Cif of pexpr * 'asm_op instr list * 'asm_op instr list
| Cfor of var_i * range * 'asm_op instr list
| Cwhile of align * 'asm_op instr list * pexpr * instr_info
   * 'asm_op instr list
| Ccall of lval list * funname * pexpr list
and 'asm_op instr =
| MkI of instr_info * 'asm_op instr_r

val cmd_rect_aux :
  'a1 asmOp -> 'a3 -> ('a1 instr -> 'a1 instr list -> 'a2 -> 'a3 -> 'a3) ->
  ('a1 instr -> 'a2) -> 'a1 instr list -> 'a3

val instr_Rect :
  'a1 asmOp -> ('a1 instr_r -> instr_info -> 'a2 -> 'a3) -> 'a4 -> ('a1 instr
  -> 'a1 instr list -> 'a3 -> 'a4 -> 'a4) -> (lval -> assgn_tag -> atype ->
  pexpr -> 'a2) -> (lval list -> assgn_tag -> 'a1 sopn -> pexpr list -> 'a2)
  -> (lval list -> (Wsize.wsize * BinNums.positive) Syscall_t.syscall_t ->
  pexpr list -> 'a2) -> (pexpr -> 'a1 instr list -> 'a1 instr list -> 'a4 ->
  'a4 -> 'a2) -> (var_i -> dir -> pexpr -> pexpr -> 'a1 instr list -> 'a4 ->
  'a2) -> (align -> 'a1 instr list -> pexpr -> instr_info -> 'a1 instr list
  -> 'a4 -> 'a4 -> 'a2) -> (lval list -> funname -> pexpr list -> 'a2) -> 'a1
  instr -> 'a3

val instr_r_Rect :
  'a1 asmOp -> ('a1 instr_r -> instr_info -> 'a2 -> 'a3) -> 'a4 -> ('a1 instr
  -> 'a1 instr list -> 'a3 -> 'a4 -> 'a4) -> (lval -> assgn_tag -> atype ->
  pexpr -> 'a2) -> (lval list -> assgn_tag -> 'a1 sopn -> pexpr list -> 'a2)
  -> (lval list -> (Wsize.wsize * BinNums.positive) Syscall_t.syscall_t ->
  pexpr list -> 'a2) -> (pexpr -> 'a1 instr list -> 'a1 instr list -> 'a4 ->
  'a4 -> 'a2) -> (var_i -> dir -> pexpr -> pexpr -> 'a1 instr list -> 'a4 ->
  'a2) -> (align -> 'a1 instr list -> pexpr -> instr_info -> 'a1 instr list
  -> 'a4 -> 'a4 -> 'a2) -> (lval list -> funname -> pexpr list -> 'a2) -> 'a1
  instr_r -> 'a2

val cmd_rect :
  'a1 asmOp -> ('a1 instr_r -> instr_info -> 'a2 -> 'a3) -> 'a4 -> ('a1 instr
  -> 'a1 instr list -> 'a3 -> 'a4 -> 'a4) -> (lval -> assgn_tag -> atype ->
  pexpr -> 'a2) -> (lval list -> assgn_tag -> 'a1 sopn -> pexpr list -> 'a2)
  -> (lval list -> (Wsize.wsize * BinNums.positive) Syscall_t.syscall_t ->
  pexpr list -> 'a2) -> (pexpr -> 'a1 instr list -> 'a1 instr list -> 'a4 ->
  'a4 -> 'a2) -> (var_i -> dir -> pexpr -> pexpr -> 'a1 instr list -> 'a4 ->
  'a2) -> (align -> 'a1 instr list -> pexpr -> instr_info -> 'a1 instr list
  -> 'a4 -> 'a4 -> 'a2) -> (lval list -> funname -> pexpr list -> 'a2) -> 'a1
  instr list -> 'a4

module type FunInfoT =
 sig
  type t

  val witness : t

  val entry_info : t -> instr_info

  val ret_info : t -> instr_info
 end

module FunInfo :
 FunInfoT

type fun_info = FInfo.t

val entry_info_of_fun_info : fun_info -> instr_info

val ret_info_of_fun_info : fun_info -> instr_info

type progT =
| Build_progT

type extra_fun_t = __

type extra_prog_t = __

type extra_val_t = __

type ('asm_op, 'extra_fun_t) _fundef = { f_info : fun_info;
                                         f_tyin : atype list;
                                         f_params : var_i list;
                                         f_body : 'asm_op instr list;
                                         f_tyout : atype list;
                                         f_res : var_i list;
                                         f_extra : 'extra_fun_t }

val f_info : 'a1 asmOp -> ('a1, 'a2) _fundef -> fun_info

val f_tyin : 'a1 asmOp -> ('a1, 'a2) _fundef -> atype list

val f_params : 'a1 asmOp -> ('a1, 'a2) _fundef -> var_i list

val f_body : 'a1 asmOp -> ('a1, 'a2) _fundef -> 'a1 instr list

val f_tyout : 'a1 asmOp -> ('a1, 'a2) _fundef -> atype list

val f_res : 'a1 asmOp -> ('a1, 'a2) _fundef -> var_i list

val f_extra : 'a1 asmOp -> ('a1, 'a2) _fundef -> 'a2

type ('asm_op, 'extra_fun_t) _fun_decl =
  funname * ('asm_op, 'extra_fun_t) _fundef

type ('asm_op, 'extra_fun_t, 'extra_prog_t) _prog = { p_funcs : ('asm_op,
                                                                'extra_fun_t)
                                                                _fun_decl list;
                                                      p_globs : glob_decl list;
                                                      p_extra : 'extra_prog_t }

val p_funcs : 'a1 asmOp -> ('a1, 'a2, 'a3) _prog -> ('a1, 'a2) _fun_decl list

val p_globs : 'a1 asmOp -> ('a1, 'a2, 'a3) _prog -> glob_decl list

val p_extra : 'a1 asmOp -> ('a1, 'a2, 'a3) _prog -> 'a3

type 'asm_op fundef = ('asm_op, extra_fun_t) _fundef

type function_signature = atype list * atype list

val signature_of_fundef :
  'a1 asmOp -> progT -> 'a1 fundef -> function_signature

type 'asm_op fun_decl = funname * 'asm_op fundef

type 'asm_op prog = ('asm_op, extra_fun_t, extra_prog_t) _prog

val coq_Build_prog :
  'a1 asmOp -> progT -> ('a1, extra_fun_t) _fun_decl list -> glob_decl list
  -> extra_prog_t -> 'a1 prog

val progUnit : progT

type 'asm_op ufundef = 'asm_op fundef

type 'asm_op ufun_decl = 'asm_op fun_decl

type 'asm_op ufun_decls = 'asm_op fun_decl list

type 'asm_op uprog = 'asm_op prog

type 'asm_op _ufundef = ('asm_op, unit) _fundef

type 'asm_op _ufun_decl = ('asm_op, unit) _fun_decl

type 'asm_op _ufun_decls = ('asm_op, unit) _fun_decl list

type 'asm_op _uprog = ('asm_op, unit, unit) _prog

val to_uprog : 'a1 asmOp -> 'a1 _uprog -> 'a1 uprog

type saved_stack =
| SavedStackNone
| SavedStackReg of Var.var
| SavedStackStk of coq_Z

val saved_stack_beq : saved_stack -> saved_stack -> bool

val saved_stack_eq_axiom : saved_stack eq_axiom

val coq_HB_unnamed_factory_15 : saved_stack Coq_hasDecEq.axioms_

val expr_saved_stack__canonical__eqtype_Equality : Equality.coq_type

type return_address_location =
| RAnone
| RAreg of Var.var * Var.var option
| RAstack of Var.var option * Var.var option * coq_Z * Var.var option

val is_RAnone : return_address_location -> bool

val is_RAstack : return_address_location -> bool

val return_address_location_beq :
  return_address_location -> return_address_location -> bool

val return_address_location_eq_axiom : return_address_location eq_axiom

val coq_HB_unnamed_factory_17 : return_address_location Coq_hasDecEq.axioms_

val expr_return_address_location__canonical__eqtype_Equality :
  Equality.coq_type

type mask_kind =
  | MKinput
  | MKoutput
  | MKrnd
  | MKother

type mask_array_info = {
  mai_kind  : mask_kind;
  mai_base  : string;
  mai_len   : coq_Z;
  mai_align : wsize;
  mai_ofs   : coq_Z;
}


type share_info = string * int
type randomness_info = int

type skey = var_i * int
type rkey = var_i * int

module SKeyOrd : Map.OrderedType with type t = skey
module RKeyOrd : Map.OrderedType with type t = rkey

module ShareMap : Map.S with type key = skey
module RandomnessMap : Map.S with type key = rkey

type smap = share_info ShareMap.t
type rmap = randomness_info RandomnessMap.t

type mask_layout = mask_array_info list

type stk_fun_extra = { sf_align : wsize; sf_stk_sz : coq_Z;
                       sf_stk_ioff : coq_Z; sf_stk_extra_sz : coq_Z;
                       sf_stk_max : coq_Z; sf_max_call_depth : coq_Z;
                       sf_to_save : (Var.var * coq_Z) list;
                       sf_save_stack : saved_stack;
                       sf_return_address : return_address_location;
                       sf_align_args : wsize list;
                       sf_random_layout: rmap;
                       sf_masking_layout: smap;
                       sf_output_layout: smap}


val sf_masking_layout : stk_fun_extra -> smap   

val sf_output_layout : stk_fun_extra -> smap 

val sf_random_layout : stk_fun_extra -> rmap
(*
val sf_mask_layout : stk_fun_extra -> mask_layout*)

val sf_align : stk_fun_extra -> wsize

val sf_stk_sz : stk_fun_extra -> coq_Z

val sf_stk_ioff : stk_fun_extra -> coq_Z

val sf_stk_extra_sz : stk_fun_extra -> coq_Z

val sf_stk_max : stk_fun_extra -> coq_Z

val sf_max_call_depth : stk_fun_extra -> coq_Z

val sf_to_save : stk_fun_extra -> (Var.var * coq_Z) list

val sf_save_stack : stk_fun_extra -> saved_stack

val sf_return_address : stk_fun_extra -> return_address_location

val sf_align_args : stk_fun_extra -> wsize list

type sprog_extra = { sp_rsp : Ident.Ident.ident; sp_rip : Ident.Ident.ident;
                     sp_globs : GRing.ComRing.sort list;
                     sp_glob_names : ((Var.var * wsize) * coq_Z) list }

val sp_rsp : sprog_extra -> Ident.Ident.ident

val sp_rip : sprog_extra -> Ident.Ident.ident

val sp_globs : sprog_extra -> GRing.ComRing.sort list

val sp_glob_names : sprog_extra -> ((Var.var * wsize) * coq_Z) list

val progStack : coq_PointerData -> progT

type 'asm_op sfundef = 'asm_op fundef

type 'asm_op sfun_decl = 'asm_op fun_decl

type 'asm_op sfun_decls = 'asm_op fun_decl list

type 'asm_op sprog = 'asm_op prog

type 'asm_op _sfundef = ('asm_op, stk_fun_extra) _fundef

type 'asm_op _sfun_decl = ('asm_op, stk_fun_extra) _fun_decl

type 'asm_op _sfun_decls = ('asm_op, stk_fun_extra) _fun_decl list

type 'asm_op _sprog = ('asm_op, stk_fun_extra, sprog_extra) _prog

val to_sprog : coq_PointerData -> 'a1 asmOp -> 'a1 _sprog -> 'a1 sprog

val with_body :
  'a1 asmOp -> ('a1, 'a2) _fundef -> 'a1 instr list -> ('a1, 'a2) _fundef

val swith_extra :
  coq_PointerData -> 'a1 asmOp -> coq_PointerData -> 'a1 ufundef ->
  extra_fun_t -> 'a1 sfundef

val is_const : pexpr -> coq_Z option

val is_bool : pexpr -> bool option

val is_Papp2 : pexpr -> ((sop2 * pexpr) * pexpr) option

val is_Pload : pexpr -> bool

val is_load : pexpr -> bool

val is_array_init : pexpr -> bool

val cast_w : wsize -> pexpr -> pexpr

val cast_ptr : coq_PointerData -> pexpr -> pexpr

val cast_const : coq_PointerData -> coq_Z -> pexpr

val eword_of_int : wsize -> coq_Z -> pexpr

val wconst : wsize -> GRing.ComRing.sort -> pexpr

val is_wconst : wsize -> pexpr -> GRing.ComRing.sort option

val is_wconst_of_size : Equality.sort -> pexpr -> coq_Z option

val vrv_rec : SvExtra.Sv.t -> lval -> SvExtra.Sv.t

val vrvs_rec : SvExtra.Sv.t -> lval list -> SvExtra.Sv.t

val vrv : lval -> SvExtra.Sv.t

val vrvs : lval list -> SvExtra.Sv.t

val lv_write_mem : lval -> bool

val write_i_rec : 'a1 asmOp -> SvExtra.Sv.t -> 'a1 instr_r -> SvExtra.Sv.t

val write_I_rec : 'a1 asmOp -> SvExtra.Sv.t -> 'a1 instr -> SvExtra.Sv.t

val write_i : 'a1 asmOp -> 'a1 instr_r -> SvExtra.Sv.t

val write_I : 'a1 asmOp -> 'a1 instr -> SvExtra.Sv.t

val write_c_rec : 'a1 asmOp -> SvExtra.Sv.t -> 'a1 instr list -> SvExtra.Sv.t

val write_c : 'a1 asmOp -> 'a1 instr list -> SvExtra.Sv.t

val use_mem : pexpr -> bool

val read_gvar : gvar -> SvExtra.Sv.t

val read_e_rec : SvExtra.Sv.t -> pexpr -> SvExtra.Sv.t

val read_e : pexpr -> SvExtra.Sv.t

val read_es_rec : SvExtra.Sv.t -> pexpr list -> SvExtra.Sv.t

val read_es : pexpr list -> SvExtra.Sv.t

val read_rv_rec : SvExtra.Sv.t -> lval -> SvExtra.Sv.t

val read_rv : lval -> SvExtra.Sv.t

val read_rvs_rec : SvExtra.Sv.t -> lval list -> SvExtra.Sv.t

val read_rvs : lval list -> SvExtra.Sv.t

val read_i_rec : 'a1 asmOp -> SvExtra.Sv.t -> 'a1 instr_r -> SvExtra.Sv.t

val read_I_rec : 'a1 asmOp -> SvExtra.Sv.t -> 'a1 instr -> SvExtra.Sv.t

val read_c_rec : 'a1 asmOp -> SvExtra.Sv.t -> 'a1 instr list -> SvExtra.Sv.t

val read_i : 'a1 asmOp -> 'a1 instr_r -> SvExtra.Sv.t

val read_I : 'a1 asmOp -> 'a1 instr -> SvExtra.Sv.t

val read_c : 'a1 asmOp -> 'a1 instr list -> SvExtra.Sv.t

val vars_I : 'a1 asmOp -> 'a1 instr -> SvExtra.Sv.t

val vars_c : 'a1 asmOp -> 'a1 instr list -> SvExtra.Sv.t

val vars_lval : lval -> SvExtra.Sv.t

val vars_lvals : lval list -> SvExtra.Sv.t

val vars_l : var_i list -> SvExtra.Sv.t

val vars_fd : 'a1 asmOp -> progT -> 'a1 fundef -> SvExtra.Sv.t

val vars_p : 'a1 asmOp -> progT -> 'a1 fun_decl list -> SvExtra.Sv.t

val eq_gvar : gvar -> gvar -> bool

val eq_expr : pexpr -> pexpr -> bool

val to_lvals : Var.var list -> lval list

val is_false : pexpr -> bool

val is_zero : Equality.sort -> pexpr -> bool

val instr_of_copn_args :
  'a1 asmOp -> assgn_tag -> ((lval list * 'a1 sopn) * pexpr list) -> 'a1
  instr_r

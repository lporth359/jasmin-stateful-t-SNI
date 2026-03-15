open BinNums
open Bool
open Eqb_core_defs
open Eqtype
open Utils0
open Wsize

type __ = Obj.t

type slh_op =
| SLHinit
| SLHupdate
| SLHmove
| SLHprotect of wsize
| SLHprotect_ptr of wsize * positive
| SLHprotect_ptr_fail of wsize * positive

val slh_op_tag : slh_op -> positive

type box_slh_op_SLHinit =
| Box_slh_op_SLHinit

type box_slh_op_SLHprotect_ptr = { coq_Box_slh_op_SLHprotect_ptr_0 : 
                                   wsize;
                                   coq_Box_slh_op_SLHprotect_ptr_1 : 
                                   positive }

type slh_op_fields_t = __

val slh_op_fields : slh_op -> slh_op_fields_t

val slh_op_eqb_fields :
  (slh_op -> slh_op -> bool) -> positive -> slh_op_fields_t ->
  slh_op_fields_t -> bool

val slh_op_eqb : slh_op -> slh_op -> bool

val slh_op_eqb_OK : slh_op -> slh_op -> reflect

val coq_HB_unnamed_factory_1 : slh_op Coq_hasDecEq.axioms_

val slh_ops_slh_op__canonical__eqtype_Equality : Equality.coq_type

val is_protect_ptr : slh_op -> (wsize * positive) option

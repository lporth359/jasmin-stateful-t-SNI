open BinNums
open Bool
open Eqb_core_defs
open Eqtype
open Std
open Type
open Utils0
open Wsize

type __ = Obj.t

type spill_op =
| Spill
| Unspill

val spill_op_tag : spill_op -> positive

type box_spill_op_Spill =
| Box_spill_op_Spill

type spill_op_fields_t = __

val spill_op_fields : spill_op -> spill_op_fields_t

val spill_op_eqb_fields :
  (spill_op -> spill_op -> bool) -> positive -> spill_op_fields_t ->
  spill_op_fields_t -> bool

val spill_op_eqb : spill_op -> spill_op -> bool

type pseudo_operator =
| Ospill of spill_op * atype list
| Ocopy of wsize * positive
| Odeclassify of atype
| Odeclassify_mem of positive
| Onop
| Omulu of wsize
| Oaddcarry of wsize
| Osubcarry of wsize
| Oswap of atype

val pseudo_operator_tag : pseudo_operator -> positive

type box_pseudo_operator_Ospill = { coq_Box_pseudo_operator_Ospill_0 : 
                                    spill_op;
                                    coq_Box_pseudo_operator_Ospill_1 : 
                                    atype list }

type box_pseudo_operator_Ocopy = { coq_Box_pseudo_operator_Ocopy_0 : 
                                   wsize;
                                   coq_Box_pseudo_operator_Ocopy_1 : 
                                   positive }

type box_pseudo_operator_Onop =
| Box_pseudo_operator_Onop

type pseudo_operator_fields_t = __

val pseudo_operator_fields : pseudo_operator -> pseudo_operator_fields_t

val pseudo_operator_eqb_fields :
  (pseudo_operator -> pseudo_operator -> bool) -> positive ->
  pseudo_operator_fields_t -> pseudo_operator_fields_t -> bool

val pseudo_operator_eqb : pseudo_operator -> pseudo_operator -> bool

val pseudo_operator_eqb_OK : pseudo_operator -> pseudo_operator -> reflect

val eqTC_pseudo_operator : pseudo_operator eqTypeC

val pseudo_operator_eqType : Equality.coq_type

val string_of_pseudo_operator : pseudo_operator -> string

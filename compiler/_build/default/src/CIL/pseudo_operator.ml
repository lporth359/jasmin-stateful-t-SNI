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

(** val spill_op_tag : spill_op -> positive **)

let spill_op_tag = function
| Spill -> Coq_xH
| Unspill -> Coq_xO Coq_xH

type box_spill_op_Spill =
| Box_spill_op_Spill

type spill_op_fields_t = __

(** val spill_op_fields : spill_op -> spill_op_fields_t **)

let spill_op_fields _ =
  Obj.magic Box_spill_op_Spill

(** val spill_op_eqb_fields :
    (spill_op -> spill_op -> bool) -> positive -> spill_op_fields_t ->
    spill_op_fields_t -> bool **)

let spill_op_eqb_fields _ _ _ _ =
  true

(** val spill_op_eqb : spill_op -> spill_op -> bool **)

let spill_op_eqb x1 x2 =
  eqb_body spill_op_tag spill_op_fields
    (Obj.magic spill_op_eqb_fields (fun _ _ -> true)) (spill_op_tag x1)
    Box_spill_op_Spill x2

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

(** val pseudo_operator_tag : pseudo_operator -> positive **)

let pseudo_operator_tag = function
| Ospill (_, _) -> Coq_xH
| Ocopy (_, _) -> Coq_xO Coq_xH
| Odeclassify _ -> Coq_xI Coq_xH
| Odeclassify_mem _ -> Coq_xO (Coq_xO Coq_xH)
| Onop -> Coq_xI (Coq_xO Coq_xH)
| Omulu _ -> Coq_xO (Coq_xI Coq_xH)
| Oaddcarry _ -> Coq_xI (Coq_xI Coq_xH)
| Osubcarry _ -> Coq_xO (Coq_xO (Coq_xO Coq_xH))
| Oswap _ -> Coq_xI (Coq_xO (Coq_xO Coq_xH))

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

(** val pseudo_operator_fields :
    pseudo_operator -> pseudo_operator_fields_t **)

let pseudo_operator_fields = function
| Ospill (h, h0) ->
  Obj.magic { coq_Box_pseudo_operator_Ospill_0 = h;
    coq_Box_pseudo_operator_Ospill_1 = h0 }
| Ocopy (h, h0) ->
  Obj.magic { coq_Box_pseudo_operator_Ocopy_0 = h;
    coq_Box_pseudo_operator_Ocopy_1 = h0 }
| Odeclassify h -> Obj.magic h
| Odeclassify_mem h -> Obj.magic h
| Onop -> Obj.magic Box_pseudo_operator_Onop
| Omulu h -> Obj.magic h
| Oaddcarry h -> Obj.magic h
| Osubcarry h -> Obj.magic h
| Oswap h -> Obj.magic h

(** val pseudo_operator_eqb_fields :
    (pseudo_operator -> pseudo_operator -> bool) -> positive ->
    pseudo_operator_fields_t -> pseudo_operator_fields_t -> bool **)

let pseudo_operator_eqb_fields _ x a b =
  match x with
  | Coq_xI x0 ->
    (match x0 with
     | Coq_xI _ -> (&&) (wsize_eqb (Obj.magic a) (Obj.magic b)) true
     | Coq_xO x1 ->
       (match x1 with
        | Coq_xO _ -> (&&) (atype_eqb (Obj.magic a) (Obj.magic b)) true
        | _ -> true)
     | Coq_xH -> (&&) (atype_eqb (Obj.magic a) (Obj.magic b)) true)
  | Coq_xO x0 ->
    (match x0 with
     | Coq_xI _ -> (&&) (wsize_eqb (Obj.magic a) (Obj.magic b)) true
     | Coq_xO x1 ->
       (match x1 with
        | Coq_xI _ -> true
        | Coq_xO _ -> (&&) (wsize_eqb (Obj.magic a) (Obj.magic b)) true
        | Coq_xH -> (&&) (positive_eqb (Obj.magic a) (Obj.magic b)) true)
     | Coq_xH ->
       let { coq_Box_pseudo_operator_Ocopy_0 = box_pseudo_operator_Ocopy_0;
         coq_Box_pseudo_operator_Ocopy_1 = box_pseudo_operator_Ocopy_1 } =
         Obj.magic a
       in
       let { coq_Box_pseudo_operator_Ocopy_0 = box_pseudo_operator_Ocopy_2;
         coq_Box_pseudo_operator_Ocopy_1 = box_pseudo_operator_Ocopy_3 } =
         Obj.magic b
       in
       (&&)
         (wsize_eqb box_pseudo_operator_Ocopy_0 box_pseudo_operator_Ocopy_2)
         ((&&)
           (positive_eqb box_pseudo_operator_Ocopy_1
             box_pseudo_operator_Ocopy_3) true))
  | Coq_xH ->
    let { coq_Box_pseudo_operator_Ospill_0 = box_pseudo_operator_Ospill_0;
      coq_Box_pseudo_operator_Ospill_1 = box_pseudo_operator_Ospill_1 } =
      Obj.magic a
    in
    let { coq_Box_pseudo_operator_Ospill_0 = box_pseudo_operator_Ospill_2;
      coq_Box_pseudo_operator_Ospill_1 = box_pseudo_operator_Ospill_3 } =
      Obj.magic b
    in
    (&&)
      (spill_op_eqb box_pseudo_operator_Ospill_0 box_pseudo_operator_Ospill_2)
      ((&&)
        (Prelude.list_eqb atype_eqb box_pseudo_operator_Ospill_1
          box_pseudo_operator_Ospill_3) true)

(** val pseudo_operator_eqb : pseudo_operator -> pseudo_operator -> bool **)

let pseudo_operator_eqb x1 x2 =
  match x1 with
  | Ospill (h, h0) ->
    eqb_body pseudo_operator_tag pseudo_operator_fields
      (Obj.magic pseudo_operator_eqb_fields (fun _ _ -> true))
      (pseudo_operator_tag (Ospill (h, h0)))
      { coq_Box_pseudo_operator_Ospill_0 = h;
      coq_Box_pseudo_operator_Ospill_1 = h0 } x2
  | Ocopy (h, h0) ->
    eqb_body pseudo_operator_tag pseudo_operator_fields
      (Obj.magic pseudo_operator_eqb_fields (fun _ _ -> true))
      (pseudo_operator_tag (Ocopy (h, h0)))
      { coq_Box_pseudo_operator_Ocopy_0 = h;
      coq_Box_pseudo_operator_Ocopy_1 = h0 } x2
  | Odeclassify h ->
    eqb_body pseudo_operator_tag pseudo_operator_fields
      (Obj.magic pseudo_operator_eqb_fields (fun _ _ -> true))
      (pseudo_operator_tag (Odeclassify h)) h x2
  | Odeclassify_mem h ->
    eqb_body pseudo_operator_tag pseudo_operator_fields
      (Obj.magic pseudo_operator_eqb_fields (fun _ _ -> true))
      (pseudo_operator_tag (Odeclassify_mem h)) h x2
  | Onop ->
    eqb_body pseudo_operator_tag pseudo_operator_fields
      (Obj.magic pseudo_operator_eqb_fields (fun _ _ -> true))
      (pseudo_operator_tag Onop) Box_pseudo_operator_Onop x2
  | Omulu h ->
    eqb_body pseudo_operator_tag pseudo_operator_fields
      (Obj.magic pseudo_operator_eqb_fields (fun _ _ -> true))
      (pseudo_operator_tag (Omulu h)) h x2
  | Oaddcarry h ->
    eqb_body pseudo_operator_tag pseudo_operator_fields
      (Obj.magic pseudo_operator_eqb_fields (fun _ _ -> true))
      (pseudo_operator_tag (Oaddcarry h)) h x2
  | Osubcarry h ->
    eqb_body pseudo_operator_tag pseudo_operator_fields
      (Obj.magic pseudo_operator_eqb_fields (fun _ _ -> true))
      (pseudo_operator_tag (Osubcarry h)) h x2
  | Oswap h ->
    eqb_body pseudo_operator_tag pseudo_operator_fields
      (Obj.magic pseudo_operator_eqb_fields (fun _ _ -> true))
      (pseudo_operator_tag (Oswap h)) h x2

(** val pseudo_operator_eqb_OK :
    pseudo_operator -> pseudo_operator -> reflect **)

let pseudo_operator_eqb_OK =
  iffP2 pseudo_operator_eqb

(** val eqTC_pseudo_operator : pseudo_operator eqTypeC **)

let eqTC_pseudo_operator =
  { beq = pseudo_operator_eqb; ceqP = pseudo_operator_eqb_OK }

(** val pseudo_operator_eqType : Equality.coq_type **)

let pseudo_operator_eqType =
  ceqT_eqType eqTC_pseudo_operator

(** val string_of_pseudo_operator : pseudo_operator -> string **)

let string_of_pseudo_operator = function
| Ospill (s, _) -> (match s with
                    | Spill -> "spill"
                    | Unspill -> "unspill")
| Ocopy (ws, _) -> pp_sz "copy" ws ()
| Odeclassify _ -> "declassify"
| Odeclassify_mem _ -> "declassify_mem"
| Onop -> "nop"
| Omulu ws -> pp_sz "mulu" ws ()
| Oaddcarry ws -> pp_sz "adc" ws ()
| Osubcarry ws -> pp_sz "sbb" ws ()
| Oswap _ -> "swap"

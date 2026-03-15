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

(** val cmp_kind_tag : cmp_kind -> positive **)

let cmp_kind_tag = function
| Cmp_int -> Coq_xH
| Cmp_w (_, _) -> Coq_xO Coq_xH

(** val is_cmp_kind_inhab : cmp_kind -> is_cmp_kind **)

let is_cmp_kind_inhab = function
| Cmp_int -> Coq_is_Cmp_int
| Cmp_w (h, h0) ->
  Coq_is_Cmp_w (h, (is_signedness_inhab h), h0, (is_wsize_inhab h0))

(** val is_cmp_kind_functor : cmp_kind -> is_cmp_kind -> is_cmp_kind **)

let rec is_cmp_kind_functor _ x =
  x

type box_cmp_kind_Cmp_int =
| Box_cmp_kind_Cmp_int

type box_cmp_kind_Cmp_w = { coq_Box_cmp_kind_Cmp_w_0 : signedness;
                            coq_Box_cmp_kind_Cmp_w_1 : wsize }

(** val coq_Box_cmp_kind_Cmp_w_0 : box_cmp_kind_Cmp_w -> signedness **)

let coq_Box_cmp_kind_Cmp_w_0 record =
  record.coq_Box_cmp_kind_Cmp_w_0

(** val coq_Box_cmp_kind_Cmp_w_1 : box_cmp_kind_Cmp_w -> wsize **)

let coq_Box_cmp_kind_Cmp_w_1 record =
  record.coq_Box_cmp_kind_Cmp_w_1

type cmp_kind_fields_t = __

(** val cmp_kind_fields : cmp_kind -> cmp_kind_fields_t **)

let cmp_kind_fields = function
| Cmp_int -> Obj.magic Box_cmp_kind_Cmp_int
| Cmp_w (h, h0) ->
  Obj.magic { coq_Box_cmp_kind_Cmp_w_0 = h; coq_Box_cmp_kind_Cmp_w_1 = h0 }

(** val cmp_kind_construct :
    positive -> cmp_kind_fields_t -> cmp_kind option **)

let cmp_kind_construct p x =
  match p with
  | Coq_xI _ -> None
  | Coq_xO _ ->
    let { coq_Box_cmp_kind_Cmp_w_0 = box_cmp_kind_Cmp_w_0;
      coq_Box_cmp_kind_Cmp_w_1 = box_cmp_kind_Cmp_w_1 } = Obj.magic x
    in
    Some (Cmp_w (box_cmp_kind_Cmp_w_0, box_cmp_kind_Cmp_w_1))
  | Coq_xH -> Some Cmp_int

(** val cmp_kind_induction :
    'a1 -> (signedness -> is_signedness -> wsize -> is_wsize -> 'a1) ->
    cmp_kind -> is_cmp_kind -> 'a1 **)

let cmp_kind_induction his_Cmp_int his_Cmp_w _ = function
| Coq_is_Cmp_int -> his_Cmp_int
| Coq_is_Cmp_w (x0, p_, x1, p_0) -> his_Cmp_w x0 p_ x1 p_0

(** val cmp_kind_eqb_fields :
    (cmp_kind -> cmp_kind -> bool) -> positive -> cmp_kind_fields_t ->
    cmp_kind_fields_t -> bool **)

let cmp_kind_eqb_fields _ x x0 x1 =
  match x with
  | Coq_xO _ ->
    let { coq_Box_cmp_kind_Cmp_w_0 = box_cmp_kind_Cmp_w_0;
      coq_Box_cmp_kind_Cmp_w_1 = box_cmp_kind_Cmp_w_1 } = Obj.magic x0
    in
    let { coq_Box_cmp_kind_Cmp_w_0 = box_cmp_kind_Cmp_w_2;
      coq_Box_cmp_kind_Cmp_w_1 = box_cmp_kind_Cmp_w_3 } = Obj.magic x1
    in
    (&&) (signedness_eqb box_cmp_kind_Cmp_w_0 box_cmp_kind_Cmp_w_2)
      ((&&) (wsize_eqb box_cmp_kind_Cmp_w_1 box_cmp_kind_Cmp_w_3) true)
  | _ -> true

(** val cmp_kind_eqb : cmp_kind -> cmp_kind -> bool **)

let cmp_kind_eqb x1 x2 =
  match x1 with
  | Cmp_int ->
    eqb_body cmp_kind_tag cmp_kind_fields
      (Obj.magic cmp_kind_eqb_fields (fun _ _ -> true))
      (cmp_kind_tag Cmp_int) Box_cmp_kind_Cmp_int x2
  | Cmp_w (h, h0) ->
    eqb_body cmp_kind_tag cmp_kind_fields
      (Obj.magic cmp_kind_eqb_fields (fun _ _ -> true))
      (cmp_kind_tag (Cmp_w (h, h0))) { coq_Box_cmp_kind_Cmp_w_0 = h;
      coq_Box_cmp_kind_Cmp_w_1 = h0 } x2

(** val cmp_kind_eqb_OK : cmp_kind -> cmp_kind -> reflect **)

let cmp_kind_eqb_OK =
  iffP2 cmp_kind_eqb

(** val cmp_kind_eqb_OK_sumbool : cmp_kind -> cmp_kind -> bool **)

let cmp_kind_eqb_OK_sumbool =
  reflect_dec cmp_kind_eqb cmp_kind_eqb_OK

type op_kind =
| Op_int
| Op_w of wsize

type is_op_kind =
| Coq_is_Op_int
| Coq_is_Op_w of wsize * is_wsize

(** val op_kind_tag : op_kind -> positive **)

let op_kind_tag = function
| Op_int -> Coq_xH
| Op_w _ -> Coq_xO Coq_xH

(** val is_op_kind_inhab : op_kind -> is_op_kind **)

let is_op_kind_inhab = function
| Op_int -> Coq_is_Op_int
| Op_w h -> Coq_is_Op_w (h, (is_wsize_inhab h))

(** val is_op_kind_functor : op_kind -> is_op_kind -> is_op_kind **)

let rec is_op_kind_functor _ x =
  x

type box_op_kind_Op_int =
| Box_op_kind_Op_int

type box_op_kind_Op_w =
  wsize
  (* singleton inductive, whose constructor was Box_op_kind_Op_w *)

(** val coq_Box_op_kind_Op_w_0 : box_op_kind_Op_w -> wsize **)

let coq_Box_op_kind_Op_w_0 record =
  record

type op_kind_fields_t = __

(** val op_kind_fields : op_kind -> op_kind_fields_t **)

let op_kind_fields = function
| Op_int -> Obj.magic Box_op_kind_Op_int
| Op_w h -> Obj.magic h

(** val op_kind_construct : positive -> op_kind_fields_t -> op_kind option **)

let op_kind_construct p x =
  match p with
  | Coq_xI _ -> None
  | Coq_xO _ -> Some (Op_w (Obj.magic x))
  | Coq_xH -> Some Op_int

(** val op_kind_induction :
    'a1 -> (wsize -> is_wsize -> 'a1) -> op_kind -> is_op_kind -> 'a1 **)

let op_kind_induction his_Op_int his_Op_w _ = function
| Coq_is_Op_int -> his_Op_int
| Coq_is_Op_w (x0, p_) -> his_Op_w x0 p_

(** val op_kind_eqb_fields :
    (op_kind -> op_kind -> bool) -> positive -> op_kind_fields_t ->
    op_kind_fields_t -> bool **)

let op_kind_eqb_fields _ x x0 x1 =
  match x with
  | Coq_xO _ -> (&&) (wsize_eqb (Obj.magic x0) (Obj.magic x1)) true
  | _ -> true

(** val op_kind_eqb : op_kind -> op_kind -> bool **)

let op_kind_eqb x1 x2 =
  match x1 with
  | Op_int ->
    eqb_body op_kind_tag op_kind_fields
      (Obj.magic op_kind_eqb_fields (fun _ _ -> true)) (op_kind_tag Op_int)
      Box_op_kind_Op_int x2
  | Op_w h ->
    eqb_body op_kind_tag op_kind_fields
      (Obj.magic op_kind_eqb_fields (fun _ _ -> true)) (op_kind_tag (Op_w h))
      h x2

(** val op_kind_eqb_OK : op_kind -> op_kind -> reflect **)

let op_kind_eqb_OK =
  iffP2 op_kind_eqb

(** val op_kind_eqb_OK_sumbool : op_kind -> op_kind -> bool **)

let op_kind_eqb_OK_sumbool =
  reflect_dec op_kind_eqb op_kind_eqb_OK

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

(** val wiop1_tag : wiop1 -> positive **)

let wiop1_tag = function
| WIwint_of_int _ -> Coq_xH
| WIint_of_wint _ -> Coq_xO Coq_xH
| WIword_of_wint _ -> Coq_xI Coq_xH
| WIwint_of_word _ -> Coq_xO (Coq_xO Coq_xH)
| WIwint_ext (_, _) -> Coq_xI (Coq_xO Coq_xH)
| WIneg _ -> Coq_xO (Coq_xI Coq_xH)

(** val is_wiop1_inhab : wiop1 -> is_wiop1 **)

let is_wiop1_inhab = function
| WIwint_of_int h -> Coq_is_WIwint_of_int (h, (is_wsize_inhab h))
| WIint_of_wint h -> Coq_is_WIint_of_wint (h, (is_wsize_inhab h))
| WIword_of_wint h -> Coq_is_WIword_of_wint (h, (is_wsize_inhab h))
| WIwint_of_word h -> Coq_is_WIwint_of_word (h, (is_wsize_inhab h))
| WIwint_ext (h, h0) ->
  Coq_is_WIwint_ext (h, (is_wsize_inhab h), h0, (is_wsize_inhab h0))
| WIneg h -> Coq_is_WIneg (h, (is_wsize_inhab h))

(** val is_wiop1_functor : wiop1 -> is_wiop1 -> is_wiop1 **)

let rec is_wiop1_functor _ x =
  x

type box_wiop1_WIwint_of_int =
  wsize
  (* singleton inductive, whose constructor was Box_wiop1_WIwint_of_int *)

(** val coq_Box_wiop1_WIwint_of_int_0 : box_wiop1_WIwint_of_int -> wsize **)

let coq_Box_wiop1_WIwint_of_int_0 record =
  record

type box_wiop1_WIwint_ext = { coq_Box_wiop1_WIwint_ext_0 : wsize;
                              coq_Box_wiop1_WIwint_ext_1 : wsize }

(** val coq_Box_wiop1_WIwint_ext_0 : box_wiop1_WIwint_ext -> wsize **)

let coq_Box_wiop1_WIwint_ext_0 record =
  record.coq_Box_wiop1_WIwint_ext_0

(** val coq_Box_wiop1_WIwint_ext_1 : box_wiop1_WIwint_ext -> wsize **)

let coq_Box_wiop1_WIwint_ext_1 record =
  record.coq_Box_wiop1_WIwint_ext_1

type wiop1_fields_t = __

(** val wiop1_fields : wiop1 -> wiop1_fields_t **)

let wiop1_fields = function
| WIwint_of_int h -> Obj.magic h
| WIint_of_wint h -> Obj.magic h
| WIword_of_wint h -> Obj.magic h
| WIwint_of_word h -> Obj.magic h
| WIwint_ext (h, h0) ->
  Obj.magic { coq_Box_wiop1_WIwint_ext_0 = h; coq_Box_wiop1_WIwint_ext_1 =
    h0 }
| WIneg h -> Obj.magic h

(** val wiop1_construct : positive -> wiop1_fields_t -> wiop1 option **)

let wiop1_construct p x =
  match p with
  | Coq_xI x0 ->
    (match x0 with
     | Coq_xI _ -> None
     | Coq_xO _ ->
       let { coq_Box_wiop1_WIwint_ext_0 = box_wiop1_WIwint_ext_0;
         coq_Box_wiop1_WIwint_ext_1 = box_wiop1_WIwint_ext_1 } = Obj.magic x
       in
       Some (WIwint_ext (box_wiop1_WIwint_ext_0, box_wiop1_WIwint_ext_1))
     | Coq_xH -> Some (WIword_of_wint (Obj.magic x)))
  | Coq_xO x0 ->
    (match x0 with
     | Coq_xI _ -> Some (WIneg (Obj.magic x))
     | Coq_xO _ -> Some (WIwint_of_word (Obj.magic x))
     | Coq_xH -> Some (WIint_of_wint (Obj.magic x)))
  | Coq_xH -> Some (WIwint_of_int (Obj.magic x))

(** val wiop1_induction :
    (wsize -> is_wsize -> 'a1) -> (wsize -> is_wsize -> 'a1) -> (wsize ->
    is_wsize -> 'a1) -> (wsize -> is_wsize -> 'a1) -> (wsize -> is_wsize ->
    wsize -> is_wsize -> 'a1) -> (wsize -> is_wsize -> 'a1) -> wiop1 ->
    is_wiop1 -> 'a1 **)

let wiop1_induction his_WIwint_of_int his_WIint_of_wint his_WIword_of_wint his_WIwint_of_word his_WIwint_ext his_WIneg _ = function
| Coq_is_WIwint_of_int (x0, p_) -> his_WIwint_of_int x0 p_
| Coq_is_WIint_of_wint (x0, p_) -> his_WIint_of_wint x0 p_
| Coq_is_WIword_of_wint (x0, p_) -> his_WIword_of_wint x0 p_
| Coq_is_WIwint_of_word (x0, p_) -> his_WIwint_of_word x0 p_
| Coq_is_WIwint_ext (x0, p_, x1, p_0) -> his_WIwint_ext x0 p_ x1 p_0
| Coq_is_WIneg (x0, p_) -> his_WIneg x0 p_

(** val wiop1_eqb_fields :
    (wiop1 -> wiop1 -> bool) -> positive -> wiop1_fields_t -> wiop1_fields_t
    -> bool **)

let wiop1_eqb_fields _ x x0 x1 =
  match x with
  | Coq_xI x2 ->
    (match x2 with
     | Coq_xI _ -> true
     | Coq_xO _ ->
       let { coq_Box_wiop1_WIwint_ext_0 = box_wiop1_WIwint_ext_0;
         coq_Box_wiop1_WIwint_ext_1 = box_wiop1_WIwint_ext_1 } = Obj.magic x0
       in
       let { coq_Box_wiop1_WIwint_ext_0 = box_wiop1_WIwint_ext_2;
         coq_Box_wiop1_WIwint_ext_1 = box_wiop1_WIwint_ext_3 } = Obj.magic x1
       in
       (&&) (wsize_eqb box_wiop1_WIwint_ext_0 box_wiop1_WIwint_ext_2)
         ((&&) (wsize_eqb box_wiop1_WIwint_ext_1 box_wiop1_WIwint_ext_3) true)
     | Coq_xH -> (&&) (wsize_eqb (Obj.magic x0) (Obj.magic x1)) true)
  | _ -> (&&) (wsize_eqb (Obj.magic x0) (Obj.magic x1)) true

(** val wiop1_eqb : wiop1 -> wiop1 -> bool **)

let wiop1_eqb x1 x2 =
  match x1 with
  | WIwint_of_int h ->
    eqb_body wiop1_tag wiop1_fields
      (Obj.magic wiop1_eqb_fields (fun _ _ -> true))
      (wiop1_tag (WIwint_of_int h)) h x2
  | WIint_of_wint h ->
    eqb_body wiop1_tag wiop1_fields
      (Obj.magic wiop1_eqb_fields (fun _ _ -> true))
      (wiop1_tag (WIint_of_wint h)) h x2
  | WIword_of_wint h ->
    eqb_body wiop1_tag wiop1_fields
      (Obj.magic wiop1_eqb_fields (fun _ _ -> true))
      (wiop1_tag (WIword_of_wint h)) h x2
  | WIwint_of_word h ->
    eqb_body wiop1_tag wiop1_fields
      (Obj.magic wiop1_eqb_fields (fun _ _ -> true))
      (wiop1_tag (WIwint_of_word h)) h x2
  | WIwint_ext (h, h0) ->
    eqb_body wiop1_tag wiop1_fields
      (Obj.magic wiop1_eqb_fields (fun _ _ -> true))
      (wiop1_tag (WIwint_ext (h, h0))) { coq_Box_wiop1_WIwint_ext_0 = h;
      coq_Box_wiop1_WIwint_ext_1 = h0 } x2
  | WIneg h ->
    eqb_body wiop1_tag wiop1_fields
      (Obj.magic wiop1_eqb_fields (fun _ _ -> true)) (wiop1_tag (WIneg h)) h
      x2

(** val wiop1_eqb_OK : wiop1 -> wiop1 -> reflect **)

let wiop1_eqb_OK =
  iffP2 wiop1_eqb

(** val wiop1_eqb_OK_sumbool : wiop1 -> wiop1 -> bool **)

let wiop1_eqb_OK_sumbool =
  reflect_dec wiop1_eqb wiop1_eqb_OK

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

(** val sop1_tag : sop1 -> positive **)

let sop1_tag = function
| Oword_of_int _ -> Coq_xH
| Oint_of_word (_, _) -> Coq_xO Coq_xH
| Osignext (_, _) -> Coq_xI Coq_xH
| Ozeroext (_, _) -> Coq_xO (Coq_xO Coq_xH)
| Onot -> Coq_xI (Coq_xO Coq_xH)
| Olnot _ -> Coq_xO (Coq_xI Coq_xH)
| Oneg _ -> Coq_xI (Coq_xI Coq_xH)
| Owi1 (_, _) -> Coq_xO (Coq_xO (Coq_xO Coq_xH))

(** val is_sop1_inhab : sop1 -> is_sop1 **)

let is_sop1_inhab = function
| Oword_of_int h -> Coq_is_Oword_of_int (h, (is_wsize_inhab h))
| Oint_of_word (h, h0) ->
  Coq_is_Oint_of_word (h, (is_signedness_inhab h), h0, (is_wsize_inhab h0))
| Osignext (h, h0) ->
  Coq_is_Osignext (h, (is_wsize_inhab h), h0, (is_wsize_inhab h0))
| Ozeroext (h, h0) ->
  Coq_is_Ozeroext (h, (is_wsize_inhab h), h0, (is_wsize_inhab h0))
| Onot -> Coq_is_Onot
| Olnot h -> Coq_is_Olnot (h, (is_wsize_inhab h))
| Oneg h -> Coq_is_Oneg (h, (is_op_kind_inhab h))
| Owi1 (h, h0) ->
  Coq_is_Owi1 (h, (is_signedness_inhab h), h0, (is_wiop1_inhab h0))

(** val is_sop1_functor : sop1 -> is_sop1 -> is_sop1 **)

let rec is_sop1_functor _ x =
  x

type box_sop1_Oword_of_int =
  wsize
  (* singleton inductive, whose constructor was Box_sop1_Oword_of_int *)

(** val coq_Box_sop1_Oword_of_int_0 : box_sop1_Oword_of_int -> wsize **)

let coq_Box_sop1_Oword_of_int_0 record =
  record

type box_sop1_Oint_of_word = { coq_Box_sop1_Oint_of_word_0 : signedness;
                               coq_Box_sop1_Oint_of_word_1 : wsize }

(** val coq_Box_sop1_Oint_of_word_0 : box_sop1_Oint_of_word -> signedness **)

let coq_Box_sop1_Oint_of_word_0 record =
  record.coq_Box_sop1_Oint_of_word_0

(** val coq_Box_sop1_Oint_of_word_1 : box_sop1_Oint_of_word -> wsize **)

let coq_Box_sop1_Oint_of_word_1 record =
  record.coq_Box_sop1_Oint_of_word_1

type box_sop1_Osignext = { coq_Box_sop1_Osignext_0 : wsize;
                           coq_Box_sop1_Osignext_1 : wsize }

(** val coq_Box_sop1_Osignext_0 : box_sop1_Osignext -> wsize **)

let coq_Box_sop1_Osignext_0 record =
  record.coq_Box_sop1_Osignext_0

(** val coq_Box_sop1_Osignext_1 : box_sop1_Osignext -> wsize **)

let coq_Box_sop1_Osignext_1 record =
  record.coq_Box_sop1_Osignext_1

type box_sop1_Onot =
| Box_sop1_Onot

type box_sop1_Oneg =
  op_kind
  (* singleton inductive, whose constructor was Box_sop1_Oneg *)

(** val coq_Box_sop1_Oneg_0 : box_sop1_Oneg -> op_kind **)

let coq_Box_sop1_Oneg_0 record =
  record

type box_sop1_Owi1 = { coq_Box_sop1_Owi1_0 : signedness;
                       coq_Box_sop1_Owi1_1 : wiop1 }

(** val coq_Box_sop1_Owi1_0 : box_sop1_Owi1 -> signedness **)

let coq_Box_sop1_Owi1_0 record =
  record.coq_Box_sop1_Owi1_0

(** val coq_Box_sop1_Owi1_1 : box_sop1_Owi1 -> wiop1 **)

let coq_Box_sop1_Owi1_1 record =
  record.coq_Box_sop1_Owi1_1

type sop1_fields_t = __

(** val sop1_fields : sop1 -> sop1_fields_t **)

let sop1_fields = function
| Oword_of_int h -> Obj.magic h
| Oint_of_word (h, h0) ->
  Obj.magic { coq_Box_sop1_Oint_of_word_0 = h; coq_Box_sop1_Oint_of_word_1 =
    h0 }
| Osignext (h, h0) ->
  Obj.magic { coq_Box_sop1_Osignext_0 = h; coq_Box_sop1_Osignext_1 = h0 }
| Ozeroext (h, h0) ->
  Obj.magic { coq_Box_sop1_Osignext_0 = h; coq_Box_sop1_Osignext_1 = h0 }
| Onot -> Obj.magic Box_sop1_Onot
| Olnot h -> Obj.magic h
| Oneg h -> Obj.magic h
| Owi1 (h, h0) ->
  Obj.magic { coq_Box_sop1_Owi1_0 = h; coq_Box_sop1_Owi1_1 = h0 }

(** val sop1_construct : positive -> sop1_fields_t -> sop1 option **)

let sop1_construct p b =
  match p with
  | Coq_xI x ->
    (match x with
     | Coq_xI _ -> Some (Oneg (Obj.magic b))
     | Coq_xO _ -> Some Onot
     | Coq_xH ->
       let { coq_Box_sop1_Osignext_0 = box_sop1_Osignext_0;
         coq_Box_sop1_Osignext_1 = box_sop1_Osignext_1 } = Obj.magic b
       in
       Some (Osignext (box_sop1_Osignext_0, box_sop1_Osignext_1)))
  | Coq_xO x ->
    (match x with
     | Coq_xI _ -> Some (Olnot (Obj.magic b))
     | Coq_xO x0 ->
       (match x0 with
        | Coq_xI _ -> None
        | Coq_xO _ ->
          let { coq_Box_sop1_Owi1_0 = box_sop1_Owi1_0; coq_Box_sop1_Owi1_1 =
            box_sop1_Owi1_1 } = Obj.magic b
          in
          Some (Owi1 (box_sop1_Owi1_0, box_sop1_Owi1_1))
        | Coq_xH ->
          let { coq_Box_sop1_Osignext_0 = box_sop1_Osignext_0;
            coq_Box_sop1_Osignext_1 = box_sop1_Osignext_1 } = Obj.magic b
          in
          Some (Ozeroext (box_sop1_Osignext_0, box_sop1_Osignext_1)))
     | Coq_xH ->
       let { coq_Box_sop1_Oint_of_word_0 = box_sop1_Oint_of_word_0;
         coq_Box_sop1_Oint_of_word_1 = box_sop1_Oint_of_word_1 } = Obj.magic b
       in
       Some (Oint_of_word (box_sop1_Oint_of_word_0, box_sop1_Oint_of_word_1)))
  | Coq_xH -> Some (Oword_of_int (Obj.magic b))

(** val sop1_induction :
    (wsize -> is_wsize -> 'a1) -> (signedness -> is_signedness -> wsize ->
    is_wsize -> 'a1) -> (wsize -> is_wsize -> wsize -> is_wsize -> 'a1) ->
    (wsize -> is_wsize -> wsize -> is_wsize -> 'a1) -> 'a1 -> (wsize ->
    is_wsize -> 'a1) -> (op_kind -> is_op_kind -> 'a1) -> (signedness ->
    is_signedness -> wiop1 -> is_wiop1 -> 'a1) -> sop1 -> is_sop1 -> 'a1 **)

let sop1_induction his_Oword_of_int his_Oint_of_word his_Osignext his_Ozeroext his_Onot his_Olnot his_Oneg his_Owi1 _ = function
| Coq_is_Oword_of_int (x0, p_) -> his_Oword_of_int x0 p_
| Coq_is_Oint_of_word (x0, p_, x1, p_0) -> his_Oint_of_word x0 p_ x1 p_0
| Coq_is_Osignext (x0, p_, x1, p_0) -> his_Osignext x0 p_ x1 p_0
| Coq_is_Ozeroext (x0, p_, x1, p_0) -> his_Ozeroext x0 p_ x1 p_0
| Coq_is_Onot -> his_Onot
| Coq_is_Olnot (x0, p_) -> his_Olnot x0 p_
| Coq_is_Oneg (x0, p_) -> his_Oneg x0 p_
| Coq_is_Owi1 (x0, p_, x1, p_0) -> his_Owi1 x0 p_ x1 p_0

(** val sop1_eqb_fields :
    (sop1 -> sop1 -> bool) -> positive -> sop1_fields_t -> sop1_fields_t ->
    bool **)

let sop1_eqb_fields _ x a b =
  match x with
  | Coq_xI x0 ->
    (match x0 with
     | Coq_xI _ -> (&&) (op_kind_eqb (Obj.magic a) (Obj.magic b)) true
     | Coq_xO _ -> true
     | Coq_xH ->
       let { coq_Box_sop1_Osignext_0 = box_sop1_Osignext_0;
         coq_Box_sop1_Osignext_1 = box_sop1_Osignext_1 } = Obj.magic a
       in
       let { coq_Box_sop1_Osignext_0 = box_sop1_Osignext_2;
         coq_Box_sop1_Osignext_1 = box_sop1_Osignext_3 } = Obj.magic b
       in
       (&&) (wsize_eqb box_sop1_Osignext_0 box_sop1_Osignext_2)
         ((&&) (wsize_eqb box_sop1_Osignext_1 box_sop1_Osignext_3) true))
  | Coq_xO x0 ->
    (match x0 with
     | Coq_xI _ -> (&&) (wsize_eqb (Obj.magic a) (Obj.magic b)) true
     | Coq_xO x1 ->
       (match x1 with
        | Coq_xI _ -> true
        | Coq_xO _ ->
          let { coq_Box_sop1_Owi1_0 = box_sop1_Owi1_0; coq_Box_sop1_Owi1_1 =
            box_sop1_Owi1_1 } = Obj.magic a
          in
          let { coq_Box_sop1_Owi1_0 = box_sop1_Owi1_2; coq_Box_sop1_Owi1_1 =
            box_sop1_Owi1_3 } = Obj.magic b
          in
          (&&) (signedness_eqb box_sop1_Owi1_0 box_sop1_Owi1_2)
            ((&&) (wiop1_eqb box_sop1_Owi1_1 box_sop1_Owi1_3) true)
        | Coq_xH ->
          let { coq_Box_sop1_Osignext_0 = box_sop1_Osignext_0;
            coq_Box_sop1_Osignext_1 = box_sop1_Osignext_1 } = Obj.magic a
          in
          let { coq_Box_sop1_Osignext_0 = box_sop1_Osignext_2;
            coq_Box_sop1_Osignext_1 = box_sop1_Osignext_3 } = Obj.magic b
          in
          (&&) (wsize_eqb box_sop1_Osignext_0 box_sop1_Osignext_2)
            ((&&) (wsize_eqb box_sop1_Osignext_1 box_sop1_Osignext_3) true))
     | Coq_xH ->
       let { coq_Box_sop1_Oint_of_word_0 = box_sop1_Oint_of_word_0;
         coq_Box_sop1_Oint_of_word_1 = box_sop1_Oint_of_word_1 } = Obj.magic a
       in
       let { coq_Box_sop1_Oint_of_word_0 = box_sop1_Oint_of_word_2;
         coq_Box_sop1_Oint_of_word_1 = box_sop1_Oint_of_word_3 } = Obj.magic b
       in
       (&&) (signedness_eqb box_sop1_Oint_of_word_0 box_sop1_Oint_of_word_2)
         ((&&) (wsize_eqb box_sop1_Oint_of_word_1 box_sop1_Oint_of_word_3)
           true))
  | Coq_xH -> (&&) (wsize_eqb (Obj.magic a) (Obj.magic b)) true

(** val sop1_eqb : sop1 -> sop1 -> bool **)

let sop1_eqb x1 x2 =
  match x1 with
  | Oword_of_int h ->
    eqb_body sop1_tag sop1_fields
      (Obj.magic sop1_eqb_fields (fun _ _ -> true))
      (sop1_tag (Oword_of_int h)) h x2
  | Oint_of_word (h, h0) ->
    eqb_body sop1_tag sop1_fields
      (Obj.magic sop1_eqb_fields (fun _ _ -> true))
      (sop1_tag (Oint_of_word (h, h0))) { coq_Box_sop1_Oint_of_word_0 = h;
      coq_Box_sop1_Oint_of_word_1 = h0 } x2
  | Osignext (h, h0) ->
    eqb_body sop1_tag sop1_fields
      (Obj.magic sop1_eqb_fields (fun _ _ -> true))
      (sop1_tag (Osignext (h, h0))) { coq_Box_sop1_Osignext_0 = h;
      coq_Box_sop1_Osignext_1 = h0 } x2
  | Ozeroext (h, h0) ->
    eqb_body sop1_tag sop1_fields
      (Obj.magic sop1_eqb_fields (fun _ _ -> true))
      (sop1_tag (Ozeroext (h, h0))) { coq_Box_sop1_Osignext_0 = h;
      coq_Box_sop1_Osignext_1 = h0 } x2
  | Onot ->
    eqb_body sop1_tag sop1_fields
      (Obj.magic sop1_eqb_fields (fun _ _ -> true)) (sop1_tag Onot)
      Box_sop1_Onot x2
  | Olnot h ->
    eqb_body sop1_tag sop1_fields
      (Obj.magic sop1_eqb_fields (fun _ _ -> true)) (sop1_tag (Olnot h)) h x2
  | Oneg h ->
    eqb_body sop1_tag sop1_fields
      (Obj.magic sop1_eqb_fields (fun _ _ -> true)) (sop1_tag (Oneg h)) h x2
  | Owi1 (h, h0) ->
    eqb_body sop1_tag sop1_fields
      (Obj.magic sop1_eqb_fields (fun _ _ -> true)) (sop1_tag (Owi1 (h, h0)))
      { coq_Box_sop1_Owi1_0 = h; coq_Box_sop1_Owi1_1 = h0 } x2

(** val sop1_eqb_OK : sop1 -> sop1 -> reflect **)

let sop1_eqb_OK =
  iffP2 sop1_eqb

(** val sop1_eqb_OK_sumbool : sop1 -> sop1 -> bool **)

let sop1_eqb_OK_sumbool =
  reflect_dec sop1_eqb sop1_eqb_OK

(** val uint_of_word : wsize -> sop1 **)

let uint_of_word ws =
  Oint_of_word (Unsigned, ws)

(** val sint_of_word : wsize -> sop1 **)

let sint_of_word ws =
  Oint_of_word (Signed, ws)

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

(** val wiop2_tag : wiop2 -> positive **)

let wiop2_tag = function
| WIadd -> Coq_xH
| WImul -> Coq_xO Coq_xH
| WIsub -> Coq_xI Coq_xH
| WIdiv -> Coq_xO (Coq_xO Coq_xH)
| WImod -> Coq_xI (Coq_xO Coq_xH)
| WIshl -> Coq_xO (Coq_xI Coq_xH)
| WIshr -> Coq_xI (Coq_xI Coq_xH)
| WIeq -> Coq_xO (Coq_xO (Coq_xO Coq_xH))
| WIneq -> Coq_xI (Coq_xO (Coq_xO Coq_xH))
| WIlt -> Coq_xO (Coq_xI (Coq_xO Coq_xH))
| WIle -> Coq_xI (Coq_xI (Coq_xO Coq_xH))
| WIgt -> Coq_xO (Coq_xO (Coq_xI Coq_xH))
| WIge -> Coq_xI (Coq_xO (Coq_xI Coq_xH))

(** val is_wiop2_inhab : wiop2 -> is_wiop2 **)

let is_wiop2_inhab = function
| WIadd -> Coq_is_WIadd
| WImul -> Coq_is_WImul
| WIsub -> Coq_is_WIsub
| WIdiv -> Coq_is_WIdiv
| WImod -> Coq_is_WImod
| WIshl -> Coq_is_WIshl
| WIshr -> Coq_is_WIshr
| WIeq -> Coq_is_WIeq
| WIneq -> Coq_is_WIneq
| WIlt -> Coq_is_WIlt
| WIle -> Coq_is_WIle
| WIgt -> Coq_is_WIgt
| WIge -> Coq_is_WIge

(** val is_wiop2_functor : wiop2 -> is_wiop2 -> is_wiop2 **)

let rec is_wiop2_functor _ x =
  x

type box_wiop2_WIadd =
| Box_wiop2_WIadd

type wiop2_fields_t = __

(** val wiop2_fields : wiop2 -> wiop2_fields_t **)

let wiop2_fields _ =
  Obj.magic Box_wiop2_WIadd

(** val wiop2_construct : positive -> wiop2_fields_t -> wiop2 option **)

let wiop2_construct p _ =
  match p with
  | Coq_xI x ->
    (match x with
     | Coq_xI x0 ->
       (match x0 with
        | Coq_xI _ -> None
        | Coq_xO _ -> Some WIle
        | Coq_xH -> Some WIshr)
     | Coq_xO x0 ->
       (match x0 with
        | Coq_xI _ -> Some WIge
        | Coq_xO _ -> Some WIneq
        | Coq_xH -> Some WImod)
     | Coq_xH -> Some WIsub)
  | Coq_xO x ->
    (match x with
     | Coq_xI x0 ->
       (match x0 with
        | Coq_xI _ -> None
        | Coq_xO _ -> Some WIlt
        | Coq_xH -> Some WIshl)
     | Coq_xO x0 ->
       (match x0 with
        | Coq_xI _ -> Some WIgt
        | Coq_xO _ -> Some WIeq
        | Coq_xH -> Some WIdiv)
     | Coq_xH -> Some WImul)
  | Coq_xH -> Some WIadd

(** val wiop2_induction :
    'a1 -> 'a1 -> 'a1 -> 'a1 -> 'a1 -> 'a1 -> 'a1 -> 'a1 -> 'a1 -> 'a1 -> 'a1
    -> 'a1 -> 'a1 -> wiop2 -> is_wiop2 -> 'a1 **)

let wiop2_induction his_WIadd his_WImul his_WIsub his_WIdiv his_WImod his_WIshl his_WIshr his_WIeq his_WIneq his_WIlt his_WIle his_WIgt his_WIge _ = function
| Coq_is_WIadd -> his_WIadd
| Coq_is_WImul -> his_WImul
| Coq_is_WIsub -> his_WIsub
| Coq_is_WIdiv -> his_WIdiv
| Coq_is_WImod -> his_WImod
| Coq_is_WIshl -> his_WIshl
| Coq_is_WIshr -> his_WIshr
| Coq_is_WIeq -> his_WIeq
| Coq_is_WIneq -> his_WIneq
| Coq_is_WIlt -> his_WIlt
| Coq_is_WIle -> his_WIle
| Coq_is_WIgt -> his_WIgt
| Coq_is_WIge -> his_WIge

(** val wiop2_eqb_fields :
    (wiop2 -> wiop2 -> bool) -> positive -> wiop2_fields_t -> wiop2_fields_t
    -> bool **)

let wiop2_eqb_fields _ _ _ _ =
  true

(** val wiop2_eqb : wiop2 -> wiop2 -> bool **)

let wiop2_eqb x1 x2 =
  eqb_body wiop2_tag wiop2_fields
    (Obj.magic wiop2_eqb_fields (fun _ _ -> true)) (wiop2_tag x1)
    Box_wiop2_WIadd x2

(** val wiop2_eqb_OK : wiop2 -> wiop2 -> reflect **)

let wiop2_eqb_OK =
  iffP2 wiop2_eqb

(** val wiop2_eqb_OK_sumbool : wiop2 -> wiop2 -> bool **)

let wiop2_eqb_OK_sumbool =
  reflect_dec wiop2_eqb wiop2_eqb_OK

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

(** val sop2_tag : sop2 -> positive **)

let sop2_tag = function
| Obeq -> Coq_xH
| Oand -> Coq_xO Coq_xH
| Oor -> Coq_xI Coq_xH
| Oadd _ -> Coq_xO (Coq_xO Coq_xH)
| Omul _ -> Coq_xI (Coq_xO Coq_xH)
| Osub _ -> Coq_xO (Coq_xI Coq_xH)
| Odiv (_, _) -> Coq_xI (Coq_xI Coq_xH)
| Omod (_, _) -> Coq_xO (Coq_xO (Coq_xO Coq_xH))
| Oland _ -> Coq_xI (Coq_xO (Coq_xO Coq_xH))
| Olor _ -> Coq_xO (Coq_xI (Coq_xO Coq_xH))
| Olxor _ -> Coq_xI (Coq_xI (Coq_xO Coq_xH))
| Olsr _ -> Coq_xO (Coq_xO (Coq_xI Coq_xH))
| Olsl _ -> Coq_xI (Coq_xO (Coq_xI Coq_xH))
| Oasr _ -> Coq_xO (Coq_xI (Coq_xI Coq_xH))
| Oror _ -> Coq_xI (Coq_xI (Coq_xI Coq_xH))
| Orol _ -> Coq_xO (Coq_xO (Coq_xO (Coq_xO Coq_xH)))
| Oeq _ -> Coq_xI (Coq_xO (Coq_xO (Coq_xO Coq_xH)))
| Oneq _ -> Coq_xO (Coq_xI (Coq_xO (Coq_xO Coq_xH)))
| Olt _ -> Coq_xI (Coq_xI (Coq_xO (Coq_xO Coq_xH)))
| Ole _ -> Coq_xO (Coq_xO (Coq_xI (Coq_xO Coq_xH)))
| Ogt _ -> Coq_xI (Coq_xO (Coq_xI (Coq_xO Coq_xH)))
| Oge _ -> Coq_xO (Coq_xI (Coq_xI (Coq_xO Coq_xH)))
| Ovadd (_, _) -> Coq_xI (Coq_xI (Coq_xI (Coq_xO Coq_xH)))
| Ovsub (_, _) -> Coq_xO (Coq_xO (Coq_xO (Coq_xI Coq_xH)))
| Ovmul (_, _) -> Coq_xI (Coq_xO (Coq_xO (Coq_xI Coq_xH)))
| Ovlsr (_, _) -> Coq_xO (Coq_xI (Coq_xO (Coq_xI Coq_xH)))
| Ovlsl (_, _) -> Coq_xI (Coq_xI (Coq_xO (Coq_xI Coq_xH)))
| Ovasr (_, _) -> Coq_xO (Coq_xO (Coq_xI (Coq_xI Coq_xH)))
| Owi2 (_, _, _) -> Coq_xI (Coq_xO (Coq_xI (Coq_xI Coq_xH)))

(** val is_sop2_inhab : sop2 -> is_sop2 **)

let is_sop2_inhab = function
| Obeq -> Coq_is_Obeq
| Oand -> Coq_is_Oand
| Oor -> Coq_is_Oor
| Oadd h -> Coq_is_Oadd (h, (is_op_kind_inhab h))
| Omul h -> Coq_is_Omul (h, (is_op_kind_inhab h))
| Osub h -> Coq_is_Osub (h, (is_op_kind_inhab h))
| Odiv (h, h0) ->
  Coq_is_Odiv (h, (is_signedness_inhab h), h0, (is_op_kind_inhab h0))
| Omod (h, h0) ->
  Coq_is_Omod (h, (is_signedness_inhab h), h0, (is_op_kind_inhab h0))
| Oland h -> Coq_is_Oland (h, (is_wsize_inhab h))
| Olor h -> Coq_is_Olor (h, (is_wsize_inhab h))
| Olxor h -> Coq_is_Olxor (h, (is_wsize_inhab h))
| Olsr h -> Coq_is_Olsr (h, (is_wsize_inhab h))
| Olsl h -> Coq_is_Olsl (h, (is_op_kind_inhab h))
| Oasr h -> Coq_is_Oasr (h, (is_op_kind_inhab h))
| Oror h -> Coq_is_Oror (h, (is_wsize_inhab h))
| Orol h -> Coq_is_Orol (h, (is_wsize_inhab h))
| Oeq h -> Coq_is_Oeq (h, (is_op_kind_inhab h))
| Oneq h -> Coq_is_Oneq (h, (is_op_kind_inhab h))
| Olt h -> Coq_is_Olt (h, (is_cmp_kind_inhab h))
| Ole h -> Coq_is_Ole (h, (is_cmp_kind_inhab h))
| Ogt h -> Coq_is_Ogt (h, (is_cmp_kind_inhab h))
| Oge h -> Coq_is_Oge (h, (is_cmp_kind_inhab h))
| Ovadd (h, h0) ->
  Coq_is_Ovadd (h, (is_velem_inhab h), h0, (is_wsize_inhab h0))
| Ovsub (h, h0) ->
  Coq_is_Ovsub (h, (is_velem_inhab h), h0, (is_wsize_inhab h0))
| Ovmul (h, h0) ->
  Coq_is_Ovmul (h, (is_velem_inhab h), h0, (is_wsize_inhab h0))
| Ovlsr (h, h0) ->
  Coq_is_Ovlsr (h, (is_velem_inhab h), h0, (is_wsize_inhab h0))
| Ovlsl (h, h0) ->
  Coq_is_Ovlsl (h, (is_velem_inhab h), h0, (is_wsize_inhab h0))
| Ovasr (h, h0) ->
  Coq_is_Ovasr (h, (is_velem_inhab h), h0, (is_wsize_inhab h0))
| Owi2 (h, h0, h1) ->
  Coq_is_Owi2 (h, (is_signedness_inhab h), h0, (is_wsize_inhab h0), h1,
    (is_wiop2_inhab h1))

(** val is_sop2_functor : sop2 -> is_sop2 -> is_sop2 **)

let rec is_sop2_functor _ x =
  x

type box_sop2_Obeq =
| Box_sop2_Obeq

type box_sop2_Oadd =
  op_kind
  (* singleton inductive, whose constructor was Box_sop2_Oadd *)

(** val coq_Box_sop2_Oadd_0 : box_sop2_Oadd -> op_kind **)

let coq_Box_sop2_Oadd_0 record =
  record

type box_sop2_Odiv = { coq_Box_sop2_Odiv_0 : signedness;
                       coq_Box_sop2_Odiv_1 : op_kind }

(** val coq_Box_sop2_Odiv_0 : box_sop2_Odiv -> signedness **)

let coq_Box_sop2_Odiv_0 record =
  record.coq_Box_sop2_Odiv_0

(** val coq_Box_sop2_Odiv_1 : box_sop2_Odiv -> op_kind **)

let coq_Box_sop2_Odiv_1 record =
  record.coq_Box_sop2_Odiv_1

type box_sop2_Oland =
  wsize
  (* singleton inductive, whose constructor was Box_sop2_Oland *)

(** val coq_Box_sop2_Oland_0 : box_sop2_Oland -> wsize **)

let coq_Box_sop2_Oland_0 record =
  record

type box_sop2_Olt =
  cmp_kind
  (* singleton inductive, whose constructor was Box_sop2_Olt *)

(** val coq_Box_sop2_Olt_0 : box_sop2_Olt -> cmp_kind **)

let coq_Box_sop2_Olt_0 record =
  record

type box_sop2_Ovadd = { coq_Box_sop2_Ovadd_0 : velem;
                        coq_Box_sop2_Ovadd_1 : wsize }

(** val coq_Box_sop2_Ovadd_0 : box_sop2_Ovadd -> velem **)

let coq_Box_sop2_Ovadd_0 record =
  record.coq_Box_sop2_Ovadd_0

(** val coq_Box_sop2_Ovadd_1 : box_sop2_Ovadd -> wsize **)

let coq_Box_sop2_Ovadd_1 record =
  record.coq_Box_sop2_Ovadd_1

type box_sop2_Owi2 = { coq_Box_sop2_Owi2_0 : signedness;
                       coq_Box_sop2_Owi2_1 : wsize;
                       coq_Box_sop2_Owi2_2 : wiop2 }

(** val coq_Box_sop2_Owi2_0 : box_sop2_Owi2 -> signedness **)

let coq_Box_sop2_Owi2_0 record =
  record.coq_Box_sop2_Owi2_0

(** val coq_Box_sop2_Owi2_1 : box_sop2_Owi2 -> wsize **)

let coq_Box_sop2_Owi2_1 record =
  record.coq_Box_sop2_Owi2_1

(** val coq_Box_sop2_Owi2_2 : box_sop2_Owi2 -> wiop2 **)

let coq_Box_sop2_Owi2_2 record =
  record.coq_Box_sop2_Owi2_2

type sop2_fields_t = __

(** val sop2_fields : sop2 -> sop2_fields_t **)

let sop2_fields = function
| Oadd h -> Obj.magic h
| Omul h -> Obj.magic h
| Osub h -> Obj.magic h
| Odiv (h, h0) ->
  Obj.magic { coq_Box_sop2_Odiv_0 = h; coq_Box_sop2_Odiv_1 = h0 }
| Omod (h, h0) ->
  Obj.magic { coq_Box_sop2_Odiv_0 = h; coq_Box_sop2_Odiv_1 = h0 }
| Oland h -> Obj.magic h
| Olor h -> Obj.magic h
| Olxor h -> Obj.magic h
| Olsr h -> Obj.magic h
| Olsl h -> Obj.magic h
| Oasr h -> Obj.magic h
| Oror h -> Obj.magic h
| Orol h -> Obj.magic h
| Oeq h -> Obj.magic h
| Oneq h -> Obj.magic h
| Olt h -> Obj.magic h
| Ole h -> Obj.magic h
| Ogt h -> Obj.magic h
| Oge h -> Obj.magic h
| Ovadd (h, h0) ->
  Obj.magic { coq_Box_sop2_Ovadd_0 = h; coq_Box_sop2_Ovadd_1 = h0 }
| Ovsub (h, h0) ->
  Obj.magic { coq_Box_sop2_Ovadd_0 = h; coq_Box_sop2_Ovadd_1 = h0 }
| Ovmul (h, h0) ->
  Obj.magic { coq_Box_sop2_Ovadd_0 = h; coq_Box_sop2_Ovadd_1 = h0 }
| Ovlsr (h, h0) ->
  Obj.magic { coq_Box_sop2_Ovadd_0 = h; coq_Box_sop2_Ovadd_1 = h0 }
| Ovlsl (h, h0) ->
  Obj.magic { coq_Box_sop2_Ovadd_0 = h; coq_Box_sop2_Ovadd_1 = h0 }
| Ovasr (h, h0) ->
  Obj.magic { coq_Box_sop2_Ovadd_0 = h; coq_Box_sop2_Ovadd_1 = h0 }
| Owi2 (h, h0, h1) ->
  Obj.magic { coq_Box_sop2_Owi2_0 = h; coq_Box_sop2_Owi2_1 = h0;
    coq_Box_sop2_Owi2_2 = h1 }
| _ -> Obj.magic Box_sop2_Obeq

(** val sop2_construct : positive -> sop2_fields_t -> sop2 option **)

let sop2_construct p x =
  match p with
  | Coq_xI x0 ->
    (match x0 with
     | Coq_xI x1 ->
       (match x1 with
        | Coq_xI x2 ->
          (match x2 with
           | Coq_xI _ -> None
           | Coq_xO _ ->
             let { coq_Box_sop2_Ovadd_0 = box_sop2_Ovadd_0;
               coq_Box_sop2_Ovadd_1 = box_sop2_Ovadd_1 } = Obj.magic x
             in
             Some (Ovadd (box_sop2_Ovadd_0, box_sop2_Ovadd_1))
           | Coq_xH -> Some (Oror (Obj.magic x)))
        | Coq_xO x2 ->
          (match x2 with
           | Coq_xI _ ->
             let { coq_Box_sop2_Ovadd_0 = box_sop2_Ovadd_0;
               coq_Box_sop2_Ovadd_1 = box_sop2_Ovadd_1 } = Obj.magic x
             in
             Some (Ovlsl (box_sop2_Ovadd_0, box_sop2_Ovadd_1))
           | Coq_xO _ -> Some (Olt (Obj.magic x))
           | Coq_xH -> Some (Olxor (Obj.magic x)))
        | Coq_xH ->
          let { coq_Box_sop2_Odiv_0 = box_sop2_Odiv_0; coq_Box_sop2_Odiv_1 =
            box_sop2_Odiv_1 } = Obj.magic x
          in
          Some (Odiv (box_sop2_Odiv_0, box_sop2_Odiv_1)))
     | Coq_xO x1 ->
       (match x1 with
        | Coq_xI x2 ->
          (match x2 with
           | Coq_xI _ ->
             let { coq_Box_sop2_Owi2_0 = box_sop2_Owi2_0;
               coq_Box_sop2_Owi2_1 = box_sop2_Owi2_1; coq_Box_sop2_Owi2_2 =
               box_sop2_Owi2_2 } = Obj.magic x
             in
             Some (Owi2 (box_sop2_Owi2_0, box_sop2_Owi2_1, box_sop2_Owi2_2))
           | Coq_xO _ -> Some (Ogt (Obj.magic x))
           | Coq_xH -> Some (Olsl (Obj.magic x)))
        | Coq_xO x2 ->
          (match x2 with
           | Coq_xI _ ->
             let { coq_Box_sop2_Ovadd_0 = box_sop2_Ovadd_0;
               coq_Box_sop2_Ovadd_1 = box_sop2_Ovadd_1 } = Obj.magic x
             in
             Some (Ovmul (box_sop2_Ovadd_0, box_sop2_Ovadd_1))
           | Coq_xO _ -> Some (Oeq (Obj.magic x))
           | Coq_xH -> Some (Oland (Obj.magic x)))
        | Coq_xH -> Some (Omul (Obj.magic x)))
     | Coq_xH -> Some Oor)
  | Coq_xO x0 ->
    (match x0 with
     | Coq_xI x1 ->
       (match x1 with
        | Coq_xI x2 ->
          (match x2 with
           | Coq_xI _ -> None
           | Coq_xO _ -> Some (Oge (Obj.magic x))
           | Coq_xH -> Some (Oasr (Obj.magic x)))
        | Coq_xO x2 ->
          (match x2 with
           | Coq_xI _ ->
             let { coq_Box_sop2_Ovadd_0 = box_sop2_Ovadd_0;
               coq_Box_sop2_Ovadd_1 = box_sop2_Ovadd_1 } = Obj.magic x
             in
             Some (Ovlsr (box_sop2_Ovadd_0, box_sop2_Ovadd_1))
           | Coq_xO _ -> Some (Oneq (Obj.magic x))
           | Coq_xH -> Some (Olor (Obj.magic x)))
        | Coq_xH -> Some (Osub (Obj.magic x)))
     | Coq_xO x1 ->
       (match x1 with
        | Coq_xI x2 ->
          (match x2 with
           | Coq_xI _ ->
             let { coq_Box_sop2_Ovadd_0 = box_sop2_Ovadd_0;
               coq_Box_sop2_Ovadd_1 = box_sop2_Ovadd_1 } = Obj.magic x
             in
             Some (Ovasr (box_sop2_Ovadd_0, box_sop2_Ovadd_1))
           | Coq_xO _ -> Some (Ole (Obj.magic x))
           | Coq_xH -> Some (Olsr (Obj.magic x)))
        | Coq_xO x2 ->
          (match x2 with
           | Coq_xI _ ->
             let { coq_Box_sop2_Ovadd_0 = box_sop2_Ovadd_0;
               coq_Box_sop2_Ovadd_1 = box_sop2_Ovadd_1 } = Obj.magic x
             in
             Some (Ovsub (box_sop2_Ovadd_0, box_sop2_Ovadd_1))
           | Coq_xO _ -> Some (Orol (Obj.magic x))
           | Coq_xH ->
             let { coq_Box_sop2_Odiv_0 = box_sop2_Odiv_0;
               coq_Box_sop2_Odiv_1 = box_sop2_Odiv_1 } = Obj.magic x
             in
             Some (Omod (box_sop2_Odiv_0, box_sop2_Odiv_1)))
        | Coq_xH -> Some (Oadd (Obj.magic x)))
     | Coq_xH -> Some Oand)
  | Coq_xH -> Some Obeq

(** val sop2_induction :
    'a1 -> 'a1 -> 'a1 -> (op_kind -> is_op_kind -> 'a1) -> (op_kind ->
    is_op_kind -> 'a1) -> (op_kind -> is_op_kind -> 'a1) -> (signedness ->
    is_signedness -> op_kind -> is_op_kind -> 'a1) -> (signedness ->
    is_signedness -> op_kind -> is_op_kind -> 'a1) -> (wsize -> is_wsize ->
    'a1) -> (wsize -> is_wsize -> 'a1) -> (wsize -> is_wsize -> 'a1) ->
    (wsize -> is_wsize -> 'a1) -> (op_kind -> is_op_kind -> 'a1) -> (op_kind
    -> is_op_kind -> 'a1) -> (wsize -> is_wsize -> 'a1) -> (wsize -> is_wsize
    -> 'a1) -> (op_kind -> is_op_kind -> 'a1) -> (op_kind -> is_op_kind ->
    'a1) -> (cmp_kind -> is_cmp_kind -> 'a1) -> (cmp_kind -> is_cmp_kind ->
    'a1) -> (cmp_kind -> is_cmp_kind -> 'a1) -> (cmp_kind -> is_cmp_kind ->
    'a1) -> (velem -> is_velem -> wsize -> is_wsize -> 'a1) -> (velem ->
    is_velem -> wsize -> is_wsize -> 'a1) -> (velem -> is_velem -> wsize ->
    is_wsize -> 'a1) -> (velem -> is_velem -> wsize -> is_wsize -> 'a1) ->
    (velem -> is_velem -> wsize -> is_wsize -> 'a1) -> (velem -> is_velem ->
    wsize -> is_wsize -> 'a1) -> (signedness -> is_signedness -> wsize ->
    is_wsize -> wiop2 -> is_wiop2 -> 'a1) -> sop2 -> is_sop2 -> 'a1 **)

let sop2_induction his_Obeq his_Oand his_Oor his_Oadd his_Omul his_Osub his_Odiv his_Omod his_Oland his_Olor his_Olxor his_Olsr his_Olsl his_Oasr his_Oror his_Orol his_Oeq his_Oneq his_Olt his_Ole his_Ogt his_Oge his_Ovadd his_Ovsub his_Ovmul his_Ovlsr his_Ovlsl his_Ovasr his_Owi2 _ = function
| Coq_is_Obeq -> his_Obeq
| Coq_is_Oand -> his_Oand
| Coq_is_Oor -> his_Oor
| Coq_is_Oadd (x0, p_) -> his_Oadd x0 p_
| Coq_is_Omul (x0, p_) -> his_Omul x0 p_
| Coq_is_Osub (x0, p_) -> his_Osub x0 p_
| Coq_is_Odiv (x0, p_, x1, p_0) -> his_Odiv x0 p_ x1 p_0
| Coq_is_Omod (x0, p_, x1, p_0) -> his_Omod x0 p_ x1 p_0
| Coq_is_Oland (x0, p_) -> his_Oland x0 p_
| Coq_is_Olor (x0, p_) -> his_Olor x0 p_
| Coq_is_Olxor (x0, p_) -> his_Olxor x0 p_
| Coq_is_Olsr (x0, p_) -> his_Olsr x0 p_
| Coq_is_Olsl (x0, p_) -> his_Olsl x0 p_
| Coq_is_Oasr (x0, p_) -> his_Oasr x0 p_
| Coq_is_Oror (x0, p_) -> his_Oror x0 p_
| Coq_is_Orol (x0, p_) -> his_Orol x0 p_
| Coq_is_Oeq (x0, p_) -> his_Oeq x0 p_
| Coq_is_Oneq (x0, p_) -> his_Oneq x0 p_
| Coq_is_Olt (x0, p_) -> his_Olt x0 p_
| Coq_is_Ole (x0, p_) -> his_Ole x0 p_
| Coq_is_Ogt (x0, p_) -> his_Ogt x0 p_
| Coq_is_Oge (x0, p_) -> his_Oge x0 p_
| Coq_is_Ovadd (x0, p_, x1, p_0) -> his_Ovadd x0 p_ x1 p_0
| Coq_is_Ovsub (x0, p_, x1, p_0) -> his_Ovsub x0 p_ x1 p_0
| Coq_is_Ovmul (x0, p_, x1, p_0) -> his_Ovmul x0 p_ x1 p_0
| Coq_is_Ovlsr (x0, p_, x1, p_0) -> his_Ovlsr x0 p_ x1 p_0
| Coq_is_Ovlsl (x0, p_, x1, p_0) -> his_Ovlsl x0 p_ x1 p_0
| Coq_is_Ovasr (x0, p_, x1, p_0) -> his_Ovasr x0 p_ x1 p_0
| Coq_is_Owi2 (x0, p_, x1, p_0, x2, p_1) -> his_Owi2 x0 p_ x1 p_0 x2 p_1

(** val sop2_eqb_fields :
    (sop2 -> sop2 -> bool) -> positive -> sop2_fields_t -> sop2_fields_t ->
    bool **)

let sop2_eqb_fields _ x x0 x1 =
  match x with
  | Coq_xI x2 ->
    (match x2 with
     | Coq_xI x3 ->
       (match x3 with
        | Coq_xI x4 ->
          (match x4 with
           | Coq_xI _ -> true
           | Coq_xO _ ->
             let { coq_Box_sop2_Ovadd_0 = box_sop2_Ovadd_0;
               coq_Box_sop2_Ovadd_1 = box_sop2_Ovadd_1 } = Obj.magic x0
             in
             let { coq_Box_sop2_Ovadd_0 = box_sop2_Ovadd_2;
               coq_Box_sop2_Ovadd_1 = box_sop2_Ovadd_3 } = Obj.magic x1
             in
             (&&) (velem_eqb box_sop2_Ovadd_0 box_sop2_Ovadd_2)
               ((&&) (wsize_eqb box_sop2_Ovadd_1 box_sop2_Ovadd_3) true)
           | Coq_xH -> (&&) (wsize_eqb (Obj.magic x0) (Obj.magic x1)) true)
        | Coq_xO x4 ->
          (match x4 with
           | Coq_xI _ ->
             let { coq_Box_sop2_Ovadd_0 = box_sop2_Ovadd_0;
               coq_Box_sop2_Ovadd_1 = box_sop2_Ovadd_1 } = Obj.magic x0
             in
             let { coq_Box_sop2_Ovadd_0 = box_sop2_Ovadd_2;
               coq_Box_sop2_Ovadd_1 = box_sop2_Ovadd_3 } = Obj.magic x1
             in
             (&&) (velem_eqb box_sop2_Ovadd_0 box_sop2_Ovadd_2)
               ((&&) (wsize_eqb box_sop2_Ovadd_1 box_sop2_Ovadd_3) true)
           | Coq_xO _ ->
             (&&) (cmp_kind_eqb (Obj.magic x0) (Obj.magic x1)) true
           | Coq_xH -> (&&) (wsize_eqb (Obj.magic x0) (Obj.magic x1)) true)
        | Coq_xH ->
          let { coq_Box_sop2_Odiv_0 = box_sop2_Odiv_0; coq_Box_sop2_Odiv_1 =
            box_sop2_Odiv_1 } = Obj.magic x0
          in
          let { coq_Box_sop2_Odiv_0 = box_sop2_Odiv_2; coq_Box_sop2_Odiv_1 =
            box_sop2_Odiv_3 } = Obj.magic x1
          in
          (&&) (signedness_eqb box_sop2_Odiv_0 box_sop2_Odiv_2)
            ((&&) (op_kind_eqb box_sop2_Odiv_1 box_sop2_Odiv_3) true))
     | Coq_xO x3 ->
       (match x3 with
        | Coq_xI x4 ->
          (match x4 with
           | Coq_xI _ ->
             let { coq_Box_sop2_Owi2_0 = box_sop2_Owi2_0;
               coq_Box_sop2_Owi2_1 = box_sop2_Owi2_1; coq_Box_sop2_Owi2_2 =
               box_sop2_Owi2_2 } = Obj.magic x0
             in
             let { coq_Box_sop2_Owi2_0 = box_sop2_Owi2_3;
               coq_Box_sop2_Owi2_1 = box_sop2_Owi2_4; coq_Box_sop2_Owi2_2 =
               box_sop2_Owi2_5 } = Obj.magic x1
             in
             (&&) (signedness_eqb box_sop2_Owi2_0 box_sop2_Owi2_3)
               ((&&) (wsize_eqb box_sop2_Owi2_1 box_sop2_Owi2_4)
                 ((&&) (wiop2_eqb box_sop2_Owi2_2 box_sop2_Owi2_5) true))
           | Coq_xO _ ->
             (&&) (cmp_kind_eqb (Obj.magic x0) (Obj.magic x1)) true
           | Coq_xH -> (&&) (op_kind_eqb (Obj.magic x0) (Obj.magic x1)) true)
        | Coq_xO x4 ->
          (match x4 with
           | Coq_xI _ ->
             let { coq_Box_sop2_Ovadd_0 = box_sop2_Ovadd_0;
               coq_Box_sop2_Ovadd_1 = box_sop2_Ovadd_1 } = Obj.magic x0
             in
             let { coq_Box_sop2_Ovadd_0 = box_sop2_Ovadd_2;
               coq_Box_sop2_Ovadd_1 = box_sop2_Ovadd_3 } = Obj.magic x1
             in
             (&&) (velem_eqb box_sop2_Ovadd_0 box_sop2_Ovadd_2)
               ((&&) (wsize_eqb box_sop2_Ovadd_1 box_sop2_Ovadd_3) true)
           | Coq_xO _ -> (&&) (op_kind_eqb (Obj.magic x0) (Obj.magic x1)) true
           | Coq_xH -> (&&) (wsize_eqb (Obj.magic x0) (Obj.magic x1)) true)
        | Coq_xH -> (&&) (op_kind_eqb (Obj.magic x0) (Obj.magic x1)) true)
     | Coq_xH -> true)
  | Coq_xO x2 ->
    (match x2 with
     | Coq_xI x3 ->
       (match x3 with
        | Coq_xI x4 ->
          (match x4 with
           | Coq_xI _ -> true
           | Coq_xO _ ->
             (&&) (cmp_kind_eqb (Obj.magic x0) (Obj.magic x1)) true
           | Coq_xH -> (&&) (op_kind_eqb (Obj.magic x0) (Obj.magic x1)) true)
        | Coq_xO x4 ->
          (match x4 with
           | Coq_xI _ ->
             let { coq_Box_sop2_Ovadd_0 = box_sop2_Ovadd_0;
               coq_Box_sop2_Ovadd_1 = box_sop2_Ovadd_1 } = Obj.magic x0
             in
             let { coq_Box_sop2_Ovadd_0 = box_sop2_Ovadd_2;
               coq_Box_sop2_Ovadd_1 = box_sop2_Ovadd_3 } = Obj.magic x1
             in
             (&&) (velem_eqb box_sop2_Ovadd_0 box_sop2_Ovadd_2)
               ((&&) (wsize_eqb box_sop2_Ovadd_1 box_sop2_Ovadd_3) true)
           | Coq_xO _ -> (&&) (op_kind_eqb (Obj.magic x0) (Obj.magic x1)) true
           | Coq_xH -> (&&) (wsize_eqb (Obj.magic x0) (Obj.magic x1)) true)
        | Coq_xH -> (&&) (op_kind_eqb (Obj.magic x0) (Obj.magic x1)) true)
     | Coq_xO x3 ->
       (match x3 with
        | Coq_xI x4 ->
          (match x4 with
           | Coq_xI _ ->
             let { coq_Box_sop2_Ovadd_0 = box_sop2_Ovadd_0;
               coq_Box_sop2_Ovadd_1 = box_sop2_Ovadd_1 } = Obj.magic x0
             in
             let { coq_Box_sop2_Ovadd_0 = box_sop2_Ovadd_2;
               coq_Box_sop2_Ovadd_1 = box_sop2_Ovadd_3 } = Obj.magic x1
             in
             (&&) (velem_eqb box_sop2_Ovadd_0 box_sop2_Ovadd_2)
               ((&&) (wsize_eqb box_sop2_Ovadd_1 box_sop2_Ovadd_3) true)
           | Coq_xO _ ->
             (&&) (cmp_kind_eqb (Obj.magic x0) (Obj.magic x1)) true
           | Coq_xH -> (&&) (wsize_eqb (Obj.magic x0) (Obj.magic x1)) true)
        | Coq_xO x4 ->
          (match x4 with
           | Coq_xI _ ->
             let { coq_Box_sop2_Ovadd_0 = box_sop2_Ovadd_0;
               coq_Box_sop2_Ovadd_1 = box_sop2_Ovadd_1 } = Obj.magic x0
             in
             let { coq_Box_sop2_Ovadd_0 = box_sop2_Ovadd_2;
               coq_Box_sop2_Ovadd_1 = box_sop2_Ovadd_3 } = Obj.magic x1
             in
             (&&) (velem_eqb box_sop2_Ovadd_0 box_sop2_Ovadd_2)
               ((&&) (wsize_eqb box_sop2_Ovadd_1 box_sop2_Ovadd_3) true)
           | Coq_xO _ -> (&&) (wsize_eqb (Obj.magic x0) (Obj.magic x1)) true
           | Coq_xH ->
             let { coq_Box_sop2_Odiv_0 = box_sop2_Odiv_0;
               coq_Box_sop2_Odiv_1 = box_sop2_Odiv_1 } = Obj.magic x0
             in
             let { coq_Box_sop2_Odiv_0 = box_sop2_Odiv_2;
               coq_Box_sop2_Odiv_1 = box_sop2_Odiv_3 } = Obj.magic x1
             in
             (&&) (signedness_eqb box_sop2_Odiv_0 box_sop2_Odiv_2)
               ((&&) (op_kind_eqb box_sop2_Odiv_1 box_sop2_Odiv_3) true))
        | Coq_xH -> (&&) (op_kind_eqb (Obj.magic x0) (Obj.magic x1)) true)
     | Coq_xH -> true)
  | Coq_xH -> true

(** val sop2_eqb : sop2 -> sop2 -> bool **)

let sop2_eqb x1 x2 =
  match x1 with
  | Oadd h ->
    eqb_body sop2_tag sop2_fields
      (Obj.magic sop2_eqb_fields (fun _ _ -> true)) (sop2_tag (Oadd h)) h x2
  | Omul h ->
    eqb_body sop2_tag sop2_fields
      (Obj.magic sop2_eqb_fields (fun _ _ -> true)) (sop2_tag (Omul h)) h x2
  | Osub h ->
    eqb_body sop2_tag sop2_fields
      (Obj.magic sop2_eqb_fields (fun _ _ -> true)) (sop2_tag (Osub h)) h x2
  | Odiv (h, h0) ->
    eqb_body sop2_tag sop2_fields
      (Obj.magic sop2_eqb_fields (fun _ _ -> true)) (sop2_tag (Odiv (h, h0)))
      { coq_Box_sop2_Odiv_0 = h; coq_Box_sop2_Odiv_1 = h0 } x2
  | Omod (h, h0) ->
    eqb_body sop2_tag sop2_fields
      (Obj.magic sop2_eqb_fields (fun _ _ -> true)) (sop2_tag (Omod (h, h0)))
      { coq_Box_sop2_Odiv_0 = h; coq_Box_sop2_Odiv_1 = h0 } x2
  | Oland h ->
    eqb_body sop2_tag sop2_fields
      (Obj.magic sop2_eqb_fields (fun _ _ -> true)) (sop2_tag (Oland h)) h x2
  | Olor h ->
    eqb_body sop2_tag sop2_fields
      (Obj.magic sop2_eqb_fields (fun _ _ -> true)) (sop2_tag (Olor h)) h x2
  | Olxor h ->
    eqb_body sop2_tag sop2_fields
      (Obj.magic sop2_eqb_fields (fun _ _ -> true)) (sop2_tag (Olxor h)) h x2
  | Olsr h ->
    eqb_body sop2_tag sop2_fields
      (Obj.magic sop2_eqb_fields (fun _ _ -> true)) (sop2_tag (Olsr h)) h x2
  | Olsl h ->
    eqb_body sop2_tag sop2_fields
      (Obj.magic sop2_eqb_fields (fun _ _ -> true)) (sop2_tag (Olsl h)) h x2
  | Oasr h ->
    eqb_body sop2_tag sop2_fields
      (Obj.magic sop2_eqb_fields (fun _ _ -> true)) (sop2_tag (Oasr h)) h x2
  | Oror h ->
    eqb_body sop2_tag sop2_fields
      (Obj.magic sop2_eqb_fields (fun _ _ -> true)) (sop2_tag (Oror h)) h x2
  | Orol h ->
    eqb_body sop2_tag sop2_fields
      (Obj.magic sop2_eqb_fields (fun _ _ -> true)) (sop2_tag (Orol h)) h x2
  | Oeq h ->
    eqb_body sop2_tag sop2_fields
      (Obj.magic sop2_eqb_fields (fun _ _ -> true)) (sop2_tag (Oeq h)) h x2
  | Oneq h ->
    eqb_body sop2_tag sop2_fields
      (Obj.magic sop2_eqb_fields (fun _ _ -> true)) (sop2_tag (Oneq h)) h x2
  | Olt h ->
    eqb_body sop2_tag sop2_fields
      (Obj.magic sop2_eqb_fields (fun _ _ -> true)) (sop2_tag (Olt h)) h x2
  | Ole h ->
    eqb_body sop2_tag sop2_fields
      (Obj.magic sop2_eqb_fields (fun _ _ -> true)) (sop2_tag (Ole h)) h x2
  | Ogt h ->
    eqb_body sop2_tag sop2_fields
      (Obj.magic sop2_eqb_fields (fun _ _ -> true)) (sop2_tag (Ogt h)) h x2
  | Oge h ->
    eqb_body sop2_tag sop2_fields
      (Obj.magic sop2_eqb_fields (fun _ _ -> true)) (sop2_tag (Oge h)) h x2
  | Ovadd (h, h0) ->
    eqb_body sop2_tag sop2_fields
      (Obj.magic sop2_eqb_fields (fun _ _ -> true))
      (sop2_tag (Ovadd (h, h0))) { coq_Box_sop2_Ovadd_0 = h;
      coq_Box_sop2_Ovadd_1 = h0 } x2
  | Ovsub (h, h0) ->
    eqb_body sop2_tag sop2_fields
      (Obj.magic sop2_eqb_fields (fun _ _ -> true))
      (sop2_tag (Ovsub (h, h0))) { coq_Box_sop2_Ovadd_0 = h;
      coq_Box_sop2_Ovadd_1 = h0 } x2
  | Ovmul (h, h0) ->
    eqb_body sop2_tag sop2_fields
      (Obj.magic sop2_eqb_fields (fun _ _ -> true))
      (sop2_tag (Ovmul (h, h0))) { coq_Box_sop2_Ovadd_0 = h;
      coq_Box_sop2_Ovadd_1 = h0 } x2
  | Ovlsr (h, h0) ->
    eqb_body sop2_tag sop2_fields
      (Obj.magic sop2_eqb_fields (fun _ _ -> true))
      (sop2_tag (Ovlsr (h, h0))) { coq_Box_sop2_Ovadd_0 = h;
      coq_Box_sop2_Ovadd_1 = h0 } x2
  | Ovlsl (h, h0) ->
    eqb_body sop2_tag sop2_fields
      (Obj.magic sop2_eqb_fields (fun _ _ -> true))
      (sop2_tag (Ovlsl (h, h0))) { coq_Box_sop2_Ovadd_0 = h;
      coq_Box_sop2_Ovadd_1 = h0 } x2
  | Ovasr (h, h0) ->
    eqb_body sop2_tag sop2_fields
      (Obj.magic sop2_eqb_fields (fun _ _ -> true))
      (sop2_tag (Ovasr (h, h0))) { coq_Box_sop2_Ovadd_0 = h;
      coq_Box_sop2_Ovadd_1 = h0 } x2
  | Owi2 (h, h0, h1) ->
    eqb_body sop2_tag sop2_fields
      (Obj.magic sop2_eqb_fields (fun _ _ -> true))
      (sop2_tag (Owi2 (h, h0, h1))) { coq_Box_sop2_Owi2_0 = h;
      coq_Box_sop2_Owi2_1 = h0; coq_Box_sop2_Owi2_2 = h1 } x2
  | x ->
    eqb_body sop2_tag sop2_fields
      (Obj.magic sop2_eqb_fields (fun _ _ -> true)) (sop2_tag x)
      Box_sop2_Obeq x2

(** val sop2_eqb_OK : sop2 -> sop2 -> reflect **)

let sop2_eqb_OK =
  iffP2 sop2_eqb

(** val sop2_eqb_OK_sumbool : sop2 -> sop2 -> bool **)

let sop2_eqb_OK_sumbool =
  reflect_dec sop2_eqb sop2_eqb_OK

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

(** val combine_flags_tag : combine_flags -> positive **)

let combine_flags_tag = function
| CF_LT _ -> Coq_xH
| CF_LE _ -> Coq_xO Coq_xH
| CF_EQ -> Coq_xI Coq_xH
| CF_NEQ -> Coq_xO (Coq_xO Coq_xH)
| CF_GE _ -> Coq_xI (Coq_xO Coq_xH)
| CF_GT _ -> Coq_xO (Coq_xI Coq_xH)

(** val is_combine_flags_inhab : combine_flags -> is_combine_flags **)

let is_combine_flags_inhab = function
| CF_LT h -> Coq_is_CF_LT (h, (is_signedness_inhab h))
| CF_LE h -> Coq_is_CF_LE (h, (is_signedness_inhab h))
| CF_EQ -> Coq_is_CF_EQ
| CF_NEQ -> Coq_is_CF_NEQ
| CF_GE h -> Coq_is_CF_GE (h, (is_signedness_inhab h))
| CF_GT h -> Coq_is_CF_GT (h, (is_signedness_inhab h))

(** val is_combine_flags_functor :
    combine_flags -> is_combine_flags -> is_combine_flags **)

let rec is_combine_flags_functor _ x =
  x

type box_combine_flags_CF_LT =
  signedness
  (* singleton inductive, whose constructor was Box_combine_flags_CF_LT *)

(** val coq_Box_combine_flags_CF_LT_0 :
    box_combine_flags_CF_LT -> signedness **)

let coq_Box_combine_flags_CF_LT_0 record =
  record

type box_combine_flags_CF_EQ =
| Box_combine_flags_CF_EQ

type combine_flags_fields_t = __

(** val combine_flags_fields : combine_flags -> combine_flags_fields_t **)

let combine_flags_fields = function
| CF_LT h -> Obj.magic h
| CF_LE h -> Obj.magic h
| CF_GE h -> Obj.magic h
| CF_GT h -> Obj.magic h
| _ -> Obj.magic Box_combine_flags_CF_EQ

(** val combine_flags_construct :
    positive -> combine_flags_fields_t -> combine_flags option **)

let combine_flags_construct p x =
  match p with
  | Coq_xI x0 ->
    (match x0 with
     | Coq_xI _ -> None
     | Coq_xO _ -> Some (CF_GE (Obj.magic x))
     | Coq_xH -> Some CF_EQ)
  | Coq_xO x0 ->
    (match x0 with
     | Coq_xI _ -> Some (CF_GT (Obj.magic x))
     | Coq_xO _ -> Some CF_NEQ
     | Coq_xH -> Some (CF_LE (Obj.magic x)))
  | Coq_xH -> Some (CF_LT (Obj.magic x))

(** val combine_flags_induction :
    (signedness -> is_signedness -> 'a1) -> (signedness -> is_signedness ->
    'a1) -> 'a1 -> 'a1 -> (signedness -> is_signedness -> 'a1) -> (signedness
    -> is_signedness -> 'a1) -> combine_flags -> is_combine_flags -> 'a1 **)

let combine_flags_induction his_CF_LT his_CF_LE his_CF_EQ his_CF_NEQ his_CF_GE his_CF_GT _ = function
| Coq_is_CF_LT (x0, p_) -> his_CF_LT x0 p_
| Coq_is_CF_LE (x0, p_) -> his_CF_LE x0 p_
| Coq_is_CF_EQ -> his_CF_EQ
| Coq_is_CF_NEQ -> his_CF_NEQ
| Coq_is_CF_GE (x0, p_) -> his_CF_GE x0 p_
| Coq_is_CF_GT (x0, p_) -> his_CF_GT x0 p_

(** val combine_flags_eqb_fields :
    (combine_flags -> combine_flags -> bool) -> positive ->
    combine_flags_fields_t -> combine_flags_fields_t -> bool **)

let combine_flags_eqb_fields _ x x0 x1 =
  match x with
  | Coq_xI x2 ->
    (match x2 with
     | Coq_xO _ -> (&&) (signedness_eqb (Obj.magic x0) (Obj.magic x1)) true
     | _ -> true)
  | Coq_xO x2 ->
    (match x2 with
     | Coq_xO _ -> true
     | _ -> (&&) (signedness_eqb (Obj.magic x0) (Obj.magic x1)) true)
  | Coq_xH -> (&&) (signedness_eqb (Obj.magic x0) (Obj.magic x1)) true

(** val combine_flags_eqb : combine_flags -> combine_flags -> bool **)

let combine_flags_eqb x1 x2 =
  match x1 with
  | CF_LT h ->
    eqb_body combine_flags_tag combine_flags_fields
      (Obj.magic combine_flags_eqb_fields (fun _ _ -> true))
      (combine_flags_tag (CF_LT h)) h x2
  | CF_LE h ->
    eqb_body combine_flags_tag combine_flags_fields
      (Obj.magic combine_flags_eqb_fields (fun _ _ -> true))
      (combine_flags_tag (CF_LE h)) h x2
  | CF_GE h ->
    eqb_body combine_flags_tag combine_flags_fields
      (Obj.magic combine_flags_eqb_fields (fun _ _ -> true))
      (combine_flags_tag (CF_GE h)) h x2
  | CF_GT h ->
    eqb_body combine_flags_tag combine_flags_fields
      (Obj.magic combine_flags_eqb_fields (fun _ _ -> true))
      (combine_flags_tag (CF_GT h)) h x2
  | x ->
    eqb_body combine_flags_tag combine_flags_fields
      (Obj.magic combine_flags_eqb_fields (fun _ _ -> true))
      (combine_flags_tag x) Box_combine_flags_CF_EQ x2

(** val combine_flags_eqb_OK : combine_flags -> combine_flags -> reflect **)

let combine_flags_eqb_OK =
  iffP2 combine_flags_eqb

(** val combine_flags_eqb_OK_sumbool :
    combine_flags -> combine_flags -> bool **)

let combine_flags_eqb_OK_sumbool =
  reflect_dec combine_flags_eqb combine_flags_eqb_OK

type opN =
| Opack of wsize * pelem
| Ocombine_flags of combine_flags

type is_opN =
| Coq_is_Opack of wsize * is_wsize * pelem * is_pelem
| Coq_is_Ocombine_flags of combine_flags * is_combine_flags

(** val opN_tag : opN -> positive **)

let opN_tag = function
| Opack (_, _) -> Coq_xH
| Ocombine_flags _ -> Coq_xO Coq_xH

(** val is_opN_inhab : opN -> is_opN **)

let is_opN_inhab = function
| Opack (h, h0) ->
  Coq_is_Opack (h, (is_wsize_inhab h), h0, (is_pelem_inhab h0))
| Ocombine_flags h -> Coq_is_Ocombine_flags (h, (is_combine_flags_inhab h))

(** val is_opN_functor : opN -> is_opN -> is_opN **)

let rec is_opN_functor _ x =
  x

type box_opN_Opack = { coq_Box_opN_Opack_0 : wsize;
                       coq_Box_opN_Opack_1 : pelem }

(** val coq_Box_opN_Opack_0 : box_opN_Opack -> wsize **)

let coq_Box_opN_Opack_0 record =
  record.coq_Box_opN_Opack_0

(** val coq_Box_opN_Opack_1 : box_opN_Opack -> pelem **)

let coq_Box_opN_Opack_1 record =
  record.coq_Box_opN_Opack_1

type box_opN_Ocombine_flags =
  combine_flags
  (* singleton inductive, whose constructor was Box_opN_Ocombine_flags *)

(** val coq_Box_opN_Ocombine_flags_0 :
    box_opN_Ocombine_flags -> combine_flags **)

let coq_Box_opN_Ocombine_flags_0 record =
  record

type opN_fields_t = __

(** val opN_fields : opN -> opN_fields_t **)

let opN_fields = function
| Opack (h, h0) ->
  Obj.magic { coq_Box_opN_Opack_0 = h; coq_Box_opN_Opack_1 = h0 }
| Ocombine_flags h -> Obj.magic h

(** val opN_construct : positive -> opN_fields_t -> opN option **)

let opN_construct p x =
  match p with
  | Coq_xI _ -> None
  | Coq_xO _ -> Some (Ocombine_flags (Obj.magic x))
  | Coq_xH ->
    let { coq_Box_opN_Opack_0 = box_opN_Opack_0; coq_Box_opN_Opack_1 =
      box_opN_Opack_1 } = Obj.magic x
    in
    Some (Opack (box_opN_Opack_0, box_opN_Opack_1))

(** val opN_induction :
    (wsize -> is_wsize -> pelem -> is_pelem -> 'a1) -> (combine_flags ->
    is_combine_flags -> 'a1) -> opN -> is_opN -> 'a1 **)

let opN_induction his_Opack his_Ocombine_flags _ = function
| Coq_is_Opack (x0, p_, x1, p_0) -> his_Opack x0 p_ x1 p_0
| Coq_is_Ocombine_flags (x0, p_) -> his_Ocombine_flags x0 p_

(** val opN_eqb_fields :
    (opN -> opN -> bool) -> positive -> opN_fields_t -> opN_fields_t -> bool **)

let opN_eqb_fields _ x x0 x1 =
  match x with
  | Coq_xI _ -> true
  | Coq_xO _ -> (&&) (combine_flags_eqb (Obj.magic x0) (Obj.magic x1)) true
  | Coq_xH ->
    let { coq_Box_opN_Opack_0 = box_opN_Opack_0; coq_Box_opN_Opack_1 =
      box_opN_Opack_1 } = Obj.magic x0
    in
    let { coq_Box_opN_Opack_0 = box_opN_Opack_2; coq_Box_opN_Opack_1 =
      box_opN_Opack_3 } = Obj.magic x1
    in
    (&&) (wsize_eqb box_opN_Opack_0 box_opN_Opack_2)
      ((&&) (pelem_eqb box_opN_Opack_1 box_opN_Opack_3) true)

(** val opN_eqb : opN -> opN -> bool **)

let opN_eqb x1 x2 =
  match x1 with
  | Opack (h, h0) ->
    eqb_body opN_tag opN_fields (Obj.magic opN_eqb_fields (fun _ _ -> true))
      (opN_tag (Opack (h, h0))) { coq_Box_opN_Opack_0 = h;
      coq_Box_opN_Opack_1 = h0 } x2
  | Ocombine_flags h ->
    eqb_body opN_tag opN_fields (Obj.magic opN_eqb_fields (fun _ _ -> true))
      (opN_tag (Ocombine_flags h)) h x2

(** val opN_eqb_OK : opN -> opN -> reflect **)

let opN_eqb_OK =
  iffP2 opN_eqb

(** val opN_eqb_OK_sumbool : opN -> opN -> bool **)

let opN_eqb_OK_sumbool =
  reflect_dec opN_eqb opN_eqb_OK

(** val coq_HB_unnamed_factory_1 : op_kind Coq_hasDecEq.axioms_ **)

let coq_HB_unnamed_factory_1 =
  { Coq_hasDecEq.eq_op = op_kind_eqb; Coq_hasDecEq.eqP = op_kind_eqb_OK }

(** val expr_op_kind__canonical__eqtype_Equality : Equality.coq_type **)

let expr_op_kind__canonical__eqtype_Equality =
  Obj.magic coq_HB_unnamed_factory_1

(** val coq_HB_unnamed_factory_3 : sop1 Coq_hasDecEq.axioms_ **)

let coq_HB_unnamed_factory_3 =
  { Coq_hasDecEq.eq_op = sop1_eqb; Coq_hasDecEq.eqP = sop1_eqb_OK }

(** val expr_sop1__canonical__eqtype_Equality : Equality.coq_type **)

let expr_sop1__canonical__eqtype_Equality =
  Obj.magic coq_HB_unnamed_factory_3

(** val coq_HB_unnamed_factory_5 : sop2 Coq_hasDecEq.axioms_ **)

let coq_HB_unnamed_factory_5 =
  { Coq_hasDecEq.eq_op = sop2_eqb; Coq_hasDecEq.eqP = sop2_eqb_OK }

(** val expr_sop2__canonical__eqtype_Equality : Equality.coq_type **)

let expr_sop2__canonical__eqtype_Equality =
  Obj.magic coq_HB_unnamed_factory_5

(** val coq_HB_unnamed_factory_7 : opN Coq_hasDecEq.axioms_ **)

let coq_HB_unnamed_factory_7 =
  { Coq_hasDecEq.eq_op = opN_eqb; Coq_hasDecEq.eqP = opN_eqb_OK }

(** val expr_opN__canonical__eqtype_Equality : Equality.coq_type **)

let expr_opN__canonical__eqtype_Equality =
  Obj.magic coq_HB_unnamed_factory_7

(** val etype_of_wiop1 :
    signedness -> wiop1 -> 'a1 extended_type * 'a1 extended_type **)

let etype_of_wiop1 s = function
| WIwint_of_int sz -> (tint, (twint s sz))
| WIint_of_wint sz -> ((twint s sz), tint)
| WIword_of_wint sz -> ((twint s sz), (tword sz))
| WIwint_of_word sz -> ((tword sz), (twint s sz))
| WIwint_ext (szo, szi) -> ((twint s szi), (twint s szo))
| WIneg sz -> ((twint s sz), (twint s sz))

(** val type_of_wiop1 : wiop1 -> atype * atype **)

let type_of_wiop1 = function
| WIwint_of_int sz -> (Coq_aint, (Coq_aword sz))
| WIint_of_wint sz -> ((Coq_aword sz), Coq_aint)
| WIword_of_wint sz -> ((Coq_aword sz), (Coq_aword sz))
| WIwint_of_word sz -> ((Coq_aword sz), (Coq_aword sz))
| WIwint_ext (szo, szi) -> ((Coq_aword szi), (Coq_aword szo))
| WIneg sz -> ((Coq_aword sz), (Coq_aword sz))

(** val type_of_opk : op_kind -> atype **)

let type_of_opk = function
| Op_int -> Coq_aint
| Op_w sz -> Coq_aword sz

(** val etype_of_opk : op_kind -> 'a1 extended_type **)

let etype_of_opk = function
| Op_int -> tint
| Op_w sz -> tword sz

(** val etype_of_op1 : sop1 -> 'a1 extended_type * 'a1 extended_type **)

let etype_of_op1 = function
| Oword_of_int sz -> (tint, (tword sz))
| Oint_of_word (_, sz) -> ((tword sz), tint)
| Osignext (szo, szi) -> ((tword szi), (tword szo))
| Ozeroext (szo, szi) -> ((tword szi), (tword szo))
| Onot -> (tbool, tbool)
| Olnot sz -> ((tword sz), (tword sz))
| Oneg k -> let t0 = etype_of_opk k in (t0, t0)
| Owi1 (s, o0) -> etype_of_wiop1 s o0

(** val type_of_op1 : sop1 -> atype * atype **)

let type_of_op1 = function
| Oword_of_int sz -> (Coq_aint, (Coq_aword sz))
| Oint_of_word (_, sz) -> ((Coq_aword sz), Coq_aint)
| Osignext (szo, szi) -> ((Coq_aword szi), (Coq_aword szo))
| Ozeroext (szo, szi) -> ((Coq_aword szi), (Coq_aword szo))
| Onot -> (Coq_abool, Coq_abool)
| Olnot sz -> ((Coq_aword sz), (Coq_aword sz))
| Oneg k -> let t0 = type_of_opk k in (t0, t0)
| Owi1 (_, o0) -> type_of_wiop1 o0

(** val etype_of_wiop2 :
    signedness -> wsize -> wiop2 -> ('a1 extended_type * 'a1
    extended_type) * 'a1 extended_type **)

let etype_of_wiop2 s sz = function
| WIadd -> let t0 = twint s sz in ((t0, t0), t0)
| WImul -> let t0 = twint s sz in ((t0, t0), t0)
| WIsub -> let t0 = twint s sz in ((t0, t0), t0)
| WIdiv -> let t0 = twint s sz in ((t0, t0), t0)
| WImod -> let t0 = twint s sz in ((t0, t0), t0)
| WIshl -> let t0 = twint s sz in let tu8 = tuint U8 in ((t0, tu8), t0)
| WIshr -> let t0 = twint s sz in let tu8 = tuint U8 in ((t0, tu8), t0)
| _ -> let t0 = twint s sz in ((t0, t0), tbool)

(** val type_of_wiop2 : wsize -> wiop2 -> (atype * atype) * atype **)

let type_of_wiop2 sz = function
| WIadd -> let t0 = Coq_aword sz in ((t0, t0), t0)
| WImul -> let t0 = Coq_aword sz in ((t0, t0), t0)
| WIsub -> let t0 = Coq_aword sz in ((t0, t0), t0)
| WIdiv -> let t0 = Coq_aword sz in ((t0, t0), t0)
| WImod -> let t0 = Coq_aword sz in ((t0, t0), t0)
| WIshl -> let t0 = Coq_aword sz in let tu8 = Coq_aword U8 in ((t0, tu8), t0)
| WIshr -> let t0 = Coq_aword sz in let tu8 = Coq_aword U8 in ((t0, tu8), t0)
| _ -> let t0 = Coq_aword sz in ((t0, t0), Coq_abool)

(** val opk8 : op_kind -> op_kind **)

let opk8 = function
| Op_int -> Op_int
| Op_w _ -> Op_w U8

(** val opk_of_cmpk : cmp_kind -> op_kind **)

let opk_of_cmpk = function
| Cmp_int -> Op_int
| Cmp_w (_, sz) -> Op_w sz

(** val etype_of_op2 :
    sop2 -> ('a1 extended_type * 'a1 extended_type) * 'a1 extended_type **)

let etype_of_op2 = function
| Oadd k -> let t0 = etype_of_opk k in ((t0, t0), t0)
| Omul k -> let t0 = etype_of_opk k in ((t0, t0), t0)
| Osub k -> let t0 = etype_of_opk k in ((t0, t0), t0)
| Odiv (_, k) -> let t0 = etype_of_opk k in ((t0, t0), t0)
| Omod (_, k) -> let t0 = etype_of_opk k in ((t0, t0), t0)
| Oland s -> let t0 = tword s in ((t0, t0), t0)
| Olor s -> let t0 = tword s in ((t0, t0), t0)
| Olxor s -> let t0 = tword s in ((t0, t0), t0)
| Olsr s -> let t0 = tword s in ((t0, (tword U8)), t0)
| Olsl k ->
  let t1 = etype_of_opk k in let t2 = etype_of_opk (opk8 k) in ((t1, t2), t1)
| Oasr k ->
  let t1 = etype_of_opk k in let t2 = etype_of_opk (opk8 k) in ((t1, t2), t1)
| Oror s -> let t0 = tword s in ((t0, (tword U8)), t0)
| Orol s -> let t0 = tword s in ((t0, (tword U8)), t0)
| Oeq k -> let t0 = etype_of_opk k in ((t0, t0), tbool)
| Oneq k -> let t0 = etype_of_opk k in ((t0, t0), tbool)
| Olt k -> let t0 = etype_of_opk (opk_of_cmpk k) in ((t0, t0), tbool)
| Ole k -> let t0 = etype_of_opk (opk_of_cmpk k) in ((t0, t0), tbool)
| Ogt k -> let t0 = etype_of_opk (opk_of_cmpk k) in ((t0, t0), tbool)
| Oge k -> let t0 = etype_of_opk (opk_of_cmpk k) in ((t0, t0), tbool)
| Ovadd (_, s) -> let t0 = tword s in ((t0, t0), t0)
| Ovsub (_, s) -> let t0 = tword s in ((t0, t0), t0)
| Ovmul (_, s) -> let t0 = tword s in ((t0, t0), t0)
| Ovlsr (_, s) -> let t0 = tword s in ((t0, (tword U128)), t0)
| Ovlsl (_, s) -> let t0 = tword s in ((t0, (tword U128)), t0)
| Ovasr (_, s) -> let t0 = tword s in ((t0, (tword U128)), t0)
| Owi2 (s, sz, o0) -> etype_of_wiop2 s sz o0
| _ -> ((tbool, tbool), tbool)

(** val type_of_op2 : sop2 -> (atype * atype) * atype **)

let type_of_op2 = function
| Oadd k -> let t0 = type_of_opk k in ((t0, t0), t0)
| Omul k -> let t0 = type_of_opk k in ((t0, t0), t0)
| Osub k -> let t0 = type_of_opk k in ((t0, t0), t0)
| Odiv (_, k) -> let t0 = type_of_opk k in ((t0, t0), t0)
| Omod (_, k) -> let t0 = type_of_opk k in ((t0, t0), t0)
| Oland s -> let t0 = Coq_aword s in ((t0, t0), t0)
| Olor s -> let t0 = Coq_aword s in ((t0, t0), t0)
| Olxor s -> let t0 = Coq_aword s in ((t0, t0), t0)
| Olsr s -> let t0 = Coq_aword s in ((t0, (Coq_aword U8)), t0)
| Olsl k ->
  let t1 = type_of_opk k in let t2 = type_of_opk (opk8 k) in ((t1, t2), t1)
| Oasr k ->
  let t1 = type_of_opk k in let t2 = type_of_opk (opk8 k) in ((t1, t2), t1)
| Oror s -> let t0 = Coq_aword s in ((t0, (Coq_aword U8)), t0)
| Orol s -> let t0 = Coq_aword s in ((t0, (Coq_aword U8)), t0)
| Oeq k -> let t0 = type_of_opk k in ((t0, t0), Coq_abool)
| Oneq k -> let t0 = type_of_opk k in ((t0, t0), Coq_abool)
| Olt k -> let t0 = type_of_opk (opk_of_cmpk k) in ((t0, t0), Coq_abool)
| Ole k -> let t0 = type_of_opk (opk_of_cmpk k) in ((t0, t0), Coq_abool)
| Ogt k -> let t0 = type_of_opk (opk_of_cmpk k) in ((t0, t0), Coq_abool)
| Oge k -> let t0 = type_of_opk (opk_of_cmpk k) in ((t0, t0), Coq_abool)
| Ovadd (_, s) -> let t0 = Coq_aword s in ((t0, t0), t0)
| Ovsub (_, s) -> let t0 = Coq_aword s in ((t0, t0), t0)
| Ovmul (_, s) -> let t0 = Coq_aword s in ((t0, t0), t0)
| Ovlsr (_, s) -> let t0 = Coq_aword s in ((t0, (Coq_aword U128)), t0)
| Ovlsl (_, s) -> let t0 = Coq_aword s in ((t0, (Coq_aword U128)), t0)
| Ovasr (_, s) -> let t0 = Coq_aword s in ((t0, (Coq_aword U128)), t0)
| Owi2 (_, sz, o0) -> type_of_wiop2 sz o0
| _ -> ((Coq_abool, Coq_abool), Coq_abool)

(** val tin_combine_flags : atype list **)

let tin_combine_flags =
  Coq_abool :: (Coq_abool :: (Coq_abool :: (Coq_abool :: [])))

(** val type_of_opN : opN -> atype list * atype **)

let type_of_opN = function
| Opack (ws, p) ->
  let n = divn (nat_of_wsize ws) (nat_of_pelem p) in
  ((nseq n Coq_aint), (Coq_aword ws))
| Ocombine_flags _ -> (tin_combine_flags, Coq_abool)

module type TAG =
 sig
  type t

  val witness : t
 end

module VarInfo =
 struct
  type t = Location.t

  (** val witness : t **)

  let witness = Location._dummy
 end

type var_info = Location.t

(** val dummy_var_info : var_info **)

let dummy_var_info =
  VarInfo.witness

type var_i = { v_var : Var.var; v_info : var_info }

(** val v_var : var_i -> Var.var **)

let v_var v =
  v.v_var

(** val v_info : var_i -> var_info **)

let v_info v =
  v.v_info

(** val mk_var_i : Var.var -> var_i **)

let mk_var_i x =
  { v_var = x; v_info = dummy_var_info }

type v_scope =
| Slocal
| Sglob

type is_v_scope =
| Coq_is_Slocal
| Coq_is_Sglob

(** val v_scope_tag : v_scope -> positive **)

let v_scope_tag = function
| Slocal -> Coq_xH
| Sglob -> Coq_xO Coq_xH

(** val is_v_scope_inhab : v_scope -> is_v_scope **)

let is_v_scope_inhab = function
| Slocal -> Coq_is_Slocal
| Sglob -> Coq_is_Sglob

(** val is_v_scope_functor : v_scope -> is_v_scope -> is_v_scope **)

let rec is_v_scope_functor _ x =
  x

type box_v_scope_Slocal =
| Box_v_scope_Slocal

type v_scope_fields_t = __

(** val v_scope_fields : v_scope -> v_scope_fields_t **)

let v_scope_fields _ =
  Obj.magic Box_v_scope_Slocal

(** val v_scope_construct : positive -> v_scope_fields_t -> v_scope option **)

let v_scope_construct p _ =
  match p with
  | Coq_xI _ -> None
  | Coq_xO _ -> Some Sglob
  | Coq_xH -> Some Slocal

(** val v_scope_induction : 'a1 -> 'a1 -> v_scope -> is_v_scope -> 'a1 **)

let v_scope_induction his_Slocal his_Sglob _ = function
| Coq_is_Slocal -> his_Slocal
| Coq_is_Sglob -> his_Sglob

(** val v_scope_eqb_fields :
    (v_scope -> v_scope -> bool) -> positive -> v_scope_fields_t ->
    v_scope_fields_t -> bool **)

let v_scope_eqb_fields _ _ _ _ =
  true

(** val v_scope_eqb : v_scope -> v_scope -> bool **)

let v_scope_eqb x1 x2 =
  eqb_body v_scope_tag v_scope_fields
    (Obj.magic v_scope_eqb_fields (fun _ _ -> true)) (v_scope_tag x1)
    Box_v_scope_Slocal x2

(** val v_scope_eqb_OK : v_scope -> v_scope -> reflect **)

let v_scope_eqb_OK =
  iffP2 v_scope_eqb

(** val v_scope_eqb_OK_sumbool : v_scope -> v_scope -> bool **)

let v_scope_eqb_OK_sumbool =
  reflect_dec v_scope_eqb v_scope_eqb_OK

(** val coq_HB_unnamed_factory_9 : v_scope Coq_hasDecEq.axioms_ **)

let coq_HB_unnamed_factory_9 =
  { Coq_hasDecEq.eq_op = v_scope_eqb; Coq_hasDecEq.eqP = v_scope_eqb_OK }

(** val expr_v_scope__canonical__eqtype_Equality : Equality.coq_type **)

let expr_v_scope__canonical__eqtype_Equality =
  Obj.magic coq_HB_unnamed_factory_9

type gvar = { gv : var_i; gs : v_scope }

(** val gv : gvar -> var_i **)

let gv g =
  g.gv

(** val gs : gvar -> v_scope **)

let gs g =
  g.gs

(** val mk_gvar : var_i -> gvar **)

let mk_gvar x =
  { gv = x; gs = Sglob }

(** val mk_lvar : var_i -> gvar **)

let mk_lvar x =
  { gv = x; gs = Slocal }

(** val is_lvar : gvar -> bool **)

let is_lvar x =
  eq_op expr_v_scope__canonical__eqtype_Equality (Obj.magic x.gs)
    (Obj.magic Slocal)

(** val is_glob : gvar -> bool **)

let is_glob x =
  eq_op expr_v_scope__canonical__eqtype_Equality (Obj.magic x.gs)
    (Obj.magic Sglob)

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

(** val coq_Plvar : var_i -> pexpr **)

let coq_Plvar x =
  Pvar (mk_lvar x)

(** val enot : pexpr -> pexpr **)

let enot e =
  Papp1 (Onot, e)

(** val eor : pexpr -> pexpr -> pexpr **)

let eor e1 e2 =
  Papp2 (Oor, e1, e2)

(** val eand : pexpr -> pexpr -> pexpr **)

let eand e1 e2 =
  Papp2 (Oand, e1, e2)

(** val eeq : pexpr -> pexpr -> pexpr **)

let eeq e1 e2 =
  Papp2 (Obeq, e1, e2)

(** val eneq : pexpr -> pexpr -> pexpr **)

let eneq e1 e2 =
  enot (eeq e1 e2)

(** val eaddw : wsize -> pexpr -> pexpr -> pexpr **)

let eaddw w e1 e2 =
  Papp2 ((Oadd (Op_w w)), e1, e2)

(** val cf_of_condition : sop2 -> (combine_flags * wsize) option **)

let cf_of_condition = function
| Oeq o -> (match o with
            | Op_int -> None
            | Op_w ws -> Some (CF_EQ, ws))
| Oneq o -> (match o with
             | Op_int -> None
             | Op_w ws -> Some (CF_NEQ, ws))
| Olt c ->
  (match c with
   | Cmp_int -> None
   | Cmp_w (s, ws) -> Some ((CF_LT s), ws))
| Ole c ->
  (match c with
   | Cmp_int -> None
   | Cmp_w (s, ws) -> Some ((CF_LE s), ws))
| Ogt c ->
  (match c with
   | Cmp_int -> None
   | Cmp_w (s, ws) -> Some ((CF_GT s), ws))
| Oge c ->
  (match c with
   | Cmp_int -> None
   | Cmp_w (s, ws) -> Some ((CF_GE s), ws))
| _ -> None

(** val pexpr_of_cf : combine_flags -> var_info -> Var.var list -> pexpr **)

let pexpr_of_cf cf vi flags =
  let eflags = map (fun x -> coq_Plvar { v_var = x; v_info = vi }) flags in
  PappN ((Ocombine_flags cf), eflags)

type lval =
| Lnone of var_info * atype
| Lvar of var_i
| Lmem of aligned * wsize * var_info * pexpr
| Laset of aligned * arr_access * wsize * var_i * pexpr
| Lasub of arr_access * wsize * positive * var_i * pexpr

(** val get_pvar : pexpr -> Var.var exec **)

let get_pvar = function
| Pvar g ->
  let { gv = x; gs = gs0 } = g in
  (match gs0 with
   | Slocal -> Ok x.v_var
   | Sglob -> type_error)
| _ -> type_error

(** val get_lvar : lval -> Var.var exec **)

let get_lvar = function
| Lvar x0 -> Ok x0.v_var
| _ -> type_error

(** val coq_Lnone_b : var_info -> lval **)

let coq_Lnone_b vi =
  Lnone (vi, Coq_abool)

(** val var_info_of_lval : lval -> var_info **)

let var_info_of_lval = function
| Lnone (i, _) -> i
| Lvar x0 -> x0.v_info
| Lmem (_, _, i, _) -> i
| Laset (_, _, _, x0, _) -> x0.v_info
| Lasub (_, _, _, x0, _) -> x0.v_info

type dir =
| UpTo
| DownTo

type is_dir =
| Coq_is_UpTo
| Coq_is_DownTo

(** val dir_tag : dir -> positive **)

let dir_tag = function
| UpTo -> Coq_xH
| DownTo -> Coq_xO Coq_xH

(** val is_dir_inhab : dir -> is_dir **)

let is_dir_inhab = function
| UpTo -> Coq_is_UpTo
| DownTo -> Coq_is_DownTo

(** val is_dir_functor : dir -> is_dir -> is_dir **)

let rec is_dir_functor _ x =
  x

type box_dir_UpTo =
| Box_dir_UpTo

type dir_fields_t = __

(** val dir_fields : dir -> dir_fields_t **)

let dir_fields _ =
  Obj.magic Box_dir_UpTo

(** val dir_construct : positive -> dir_fields_t -> dir option **)

let dir_construct p _ =
  match p with
  | Coq_xI _ -> None
  | Coq_xO _ -> Some DownTo
  | Coq_xH -> Some UpTo

(** val dir_induction : 'a1 -> 'a1 -> dir -> is_dir -> 'a1 **)

let dir_induction his_UpTo his_DownTo _ = function
| Coq_is_UpTo -> his_UpTo
| Coq_is_DownTo -> his_DownTo

(** val dir_eqb_fields :
    (dir -> dir -> bool) -> positive -> dir_fields_t -> dir_fields_t -> bool **)

let dir_eqb_fields _ _ _ _ =
  true

(** val dir_eqb : dir -> dir -> bool **)

let dir_eqb x1 x2 =
  eqb_body dir_tag dir_fields (Obj.magic dir_eqb_fields (fun _ _ -> true))
    (dir_tag x1) Box_dir_UpTo x2

(** val dir_eqb_OK : dir -> dir -> reflect **)

let dir_eqb_OK =
  iffP2 dir_eqb

(** val dir_eqb_OK_sumbool : dir -> dir -> bool **)

let dir_eqb_OK_sumbool =
  reflect_dec dir_eqb dir_eqb_OK

(** val coq_HB_unnamed_factory_11 : dir Coq_hasDecEq.axioms_ **)

let coq_HB_unnamed_factory_11 =
  { Coq_hasDecEq.eq_op = dir_eqb; Coq_hasDecEq.eqP = dir_eqb_OK }

(** val expr_dir__canonical__eqtype_Equality : Equality.coq_type **)

let expr_dir__canonical__eqtype_Equality =
  Obj.magic coq_HB_unnamed_factory_11

type range = (dir * pexpr) * pexpr

(** val wrange : dir -> coq_Z -> coq_Z -> coq_Z list **)

let wrange d n1 n2 =
  let n = Z.to_nat (Z.sub n2 n1) in
  (match d with
   | UpTo -> map (fun i -> Z.add n1 (Z.of_nat i)) (iota O n)
   | DownTo -> map (fun i -> Z.sub n2 (Z.of_nat i)) (iota O n))

module type InstrInfoT =
 sig
  type t

  val witness : t

  val with_location : t -> t

  val is_inline : t -> bool

  val var_info_of_ii : t -> var_info
 end

module InstrInfo =
 struct
  type t = IInfo.t

  (** val witness : t **)

  let witness = IInfo.dummy

  (** val with_location : t -> t **)

  let with_location = IInfo.with_location

  (** val is_inline : t -> bool **)

  let is_inline = IInfo.is_inline

  (** val var_info_of_ii : t -> var_info **)

  let var_info_of_ii = IInfo.var_info_of_ii
 end

type instr_info = IInfo.t

(** val dummy_instr_info : instr_info **)

let dummy_instr_info =
  InstrInfo.witness

(** val ii_with_location : instr_info -> instr_info **)

let ii_with_location =
  InstrInfo.with_location

(** val ii_is_inline : instr_info -> bool **)

let ii_is_inline =
  InstrInfo.is_inline

(** val var_info_of_ii : instr_info -> var_info **)

let var_info_of_ii =
  InstrInfo.var_info_of_ii

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

(** val assgn_tag_tag : assgn_tag -> positive **)

let assgn_tag_tag = function
| AT_none -> Coq_xH
| AT_keep -> Coq_xO Coq_xH
| AT_rename -> Coq_xI Coq_xH
| AT_inline -> Coq_xO (Coq_xO Coq_xH)
| AT_phinode -> Coq_xI (Coq_xO Coq_xH)

(** val is_assgn_tag_inhab : assgn_tag -> is_assgn_tag **)

let is_assgn_tag_inhab = function
| AT_none -> Coq_is_AT_none
| AT_keep -> Coq_is_AT_keep
| AT_rename -> Coq_is_AT_rename
| AT_inline -> Coq_is_AT_inline
| AT_phinode -> Coq_is_AT_phinode

(** val is_assgn_tag_functor : assgn_tag -> is_assgn_tag -> is_assgn_tag **)

let rec is_assgn_tag_functor _ x =
  x

type box_assgn_tag_AT_none =
| Box_assgn_tag_AT_none

type assgn_tag_fields_t = __

(** val assgn_tag_fields : assgn_tag -> assgn_tag_fields_t **)

let assgn_tag_fields _ =
  Obj.magic Box_assgn_tag_AT_none

(** val assgn_tag_construct :
    positive -> assgn_tag_fields_t -> assgn_tag option **)

let assgn_tag_construct p _ =
  match p with
  | Coq_xI x ->
    (match x with
     | Coq_xI _ -> None
     | Coq_xO _ -> Some AT_phinode
     | Coq_xH -> Some AT_rename)
  | Coq_xO x ->
    (match x with
     | Coq_xI _ -> None
     | Coq_xO _ -> Some AT_inline
     | Coq_xH -> Some AT_keep)
  | Coq_xH -> Some AT_none

(** val assgn_tag_induction :
    'a1 -> 'a1 -> 'a1 -> 'a1 -> 'a1 -> assgn_tag -> is_assgn_tag -> 'a1 **)

let assgn_tag_induction his_AT_none his_AT_keep his_AT_rename his_AT_inline his_AT_phinode _ = function
| Coq_is_AT_none -> his_AT_none
| Coq_is_AT_keep -> his_AT_keep
| Coq_is_AT_rename -> his_AT_rename
| Coq_is_AT_inline -> his_AT_inline
| Coq_is_AT_phinode -> his_AT_phinode

(** val assgn_tag_eqb_fields :
    (assgn_tag -> assgn_tag -> bool) -> positive -> assgn_tag_fields_t ->
    assgn_tag_fields_t -> bool **)

let assgn_tag_eqb_fields _ _ _ _ =
  true

(** val assgn_tag_eqb : assgn_tag -> assgn_tag -> bool **)

let assgn_tag_eqb x1 x2 =
  eqb_body assgn_tag_tag assgn_tag_fields
    (Obj.magic assgn_tag_eqb_fields (fun _ _ -> true)) (assgn_tag_tag x1)
    Box_assgn_tag_AT_none x2

(** val assgn_tag_eqb_OK : assgn_tag -> assgn_tag -> reflect **)

let assgn_tag_eqb_OK =
  iffP2 assgn_tag_eqb

(** val assgn_tag_eqb_OK_sumbool : assgn_tag -> assgn_tag -> bool **)

let assgn_tag_eqb_OK_sumbool =
  reflect_dec assgn_tag_eqb assgn_tag_eqb_OK

(** val coq_HB_unnamed_factory_13 : assgn_tag Coq_hasDecEq.axioms_ **)

let coq_HB_unnamed_factory_13 =
  { Coq_hasDecEq.eq_op = assgn_tag_eqb; Coq_hasDecEq.eqP = assgn_tag_eqb_OK }

(** val expr_assgn_tag__canonical__eqtype_Equality : Equality.coq_type **)

let expr_assgn_tag__canonical__eqtype_Equality =
  Obj.magic coq_HB_unnamed_factory_13

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

(** val cmd_rect_aux :
    'a1 asmOp -> 'a3 -> ('a1 instr -> 'a1 instr list -> 'a2 -> 'a3 -> 'a3) ->
    ('a1 instr -> 'a2) -> 'a1 instr list -> 'a3 **)

let rec cmd_rect_aux asmop hnil hcons instr_rect = function
| [] -> hnil
| i :: c0 ->
  hcons i c0 (instr_rect i) (cmd_rect_aux asmop hnil hcons instr_rect c0)

(** val instr_Rect :
    'a1 asmOp -> ('a1 instr_r -> instr_info -> 'a2 -> 'a3) -> 'a4 -> ('a1
    instr -> 'a1 instr list -> 'a3 -> 'a4 -> 'a4) -> (lval -> assgn_tag ->
    atype -> pexpr -> 'a2) -> (lval list -> assgn_tag -> 'a1 sopn -> pexpr
    list -> 'a2) -> (lval list ->
    (Wsize.wsize * BinNums.positive) Syscall_t.syscall_t -> pexpr list ->
    'a2) -> (pexpr -> 'a1 instr list -> 'a1 instr list -> 'a4 -> 'a4 -> 'a2)
    -> (var_i -> dir -> pexpr -> pexpr -> 'a1 instr list -> 'a4 -> 'a2) ->
    (align -> 'a1 instr list -> pexpr -> instr_info -> 'a1 instr list -> 'a4
    -> 'a4 -> 'a2) -> (lval list -> funname -> pexpr list -> 'a2) -> 'a1
    instr -> 'a3 **)

let instr_Rect asmop hmk hnil hcons hasgn hopn hsyscall hif hfor hwhile hcall =
  let rec instr_Rect0 = function
  | MkI (ii, i0) -> hmk i0 ii (instr_r_Rect0 i0)
  and instr_r_Rect0 = function
  | Cassgn (x, tg, ty, e) -> hasgn x tg ty e
  | Copn (xs, t0, o, es) -> hopn xs t0 o es
  | Csyscall (xs, o, es) -> hsyscall xs o es
  | Cif (e, c1, c2) ->
    hif e c1 c2 (cmd_rect_aux asmop hnil hcons instr_Rect0 c1)
      (cmd_rect_aux asmop hnil hcons instr_Rect0 c2)
  | Cfor (i0, r, c) ->
    let (p, hi) = r in
    let (dir0, lo) = p in
    hfor i0 dir0 lo hi c (cmd_rect_aux asmop hnil hcons instr_Rect0 c)
  | Cwhile (a, c, e, info, c') ->
    hwhile a c e info c' (cmd_rect_aux asmop hnil hcons instr_Rect0 c)
      (cmd_rect_aux asmop hnil hcons instr_Rect0 c')
  | Ccall (xs, f, es) -> hcall xs f es
  in instr_Rect0

(** val instr_r_Rect :
    'a1 asmOp -> ('a1 instr_r -> instr_info -> 'a2 -> 'a3) -> 'a4 -> ('a1
    instr -> 'a1 instr list -> 'a3 -> 'a4 -> 'a4) -> (lval -> assgn_tag ->
    atype -> pexpr -> 'a2) -> (lval list -> assgn_tag -> 'a1 sopn -> pexpr
    list -> 'a2) -> (lval list ->
    (Wsize.wsize * BinNums.positive) Syscall_t.syscall_t -> pexpr list ->
    'a2) -> (pexpr -> 'a1 instr list -> 'a1 instr list -> 'a4 -> 'a4 -> 'a2)
    -> (var_i -> dir -> pexpr -> pexpr -> 'a1 instr list -> 'a4 -> 'a2) ->
    (align -> 'a1 instr list -> pexpr -> instr_info -> 'a1 instr list -> 'a4
    -> 'a4 -> 'a2) -> (lval list -> funname -> pexpr list -> 'a2) -> 'a1
    instr_r -> 'a2 **)

let instr_r_Rect asmop hmk hnil hcons hasgn hopn hsyscall hif hfor hwhile hcall =
  let rec instr_Rect0 = function
  | MkI (ii, i0) -> hmk i0 ii (instr_r_Rect0 i0)
  and instr_r_Rect0 = function
  | Cassgn (x, tg, ty, e) -> hasgn x tg ty e
  | Copn (xs, t0, o, es) -> hopn xs t0 o es
  | Csyscall (xs, o, es) -> hsyscall xs o es
  | Cif (e, c1, c2) ->
    hif e c1 c2 (cmd_rect_aux asmop hnil hcons instr_Rect0 c1)
      (cmd_rect_aux asmop hnil hcons instr_Rect0 c2)
  | Cfor (i0, r, c) ->
    let (p, hi) = r in
    let (dir0, lo) = p in
    hfor i0 dir0 lo hi c (cmd_rect_aux asmop hnil hcons instr_Rect0 c)
  | Cwhile (a, c, e, info, c') ->
    hwhile a c e info c' (cmd_rect_aux asmop hnil hcons instr_Rect0 c)
      (cmd_rect_aux asmop hnil hcons instr_Rect0 c')
  | Ccall (xs, f, es) -> hcall xs f es
  in instr_r_Rect0

(** val cmd_rect :
    'a1 asmOp -> ('a1 instr_r -> instr_info -> 'a2 -> 'a3) -> 'a4 -> ('a1
    instr -> 'a1 instr list -> 'a3 -> 'a4 -> 'a4) -> (lval -> assgn_tag ->
    atype -> pexpr -> 'a2) -> (lval list -> assgn_tag -> 'a1 sopn -> pexpr
    list -> 'a2) -> (lval list ->
    (Wsize.wsize * BinNums.positive) Syscall_t.syscall_t -> pexpr list ->
    'a2) -> (pexpr -> 'a1 instr list -> 'a1 instr list -> 'a4 -> 'a4 -> 'a2)
    -> (var_i -> dir -> pexpr -> pexpr -> 'a1 instr list -> 'a4 -> 'a2) ->
    (align -> 'a1 instr list -> pexpr -> instr_info -> 'a1 instr list -> 'a4
    -> 'a4 -> 'a2) -> (lval list -> funname -> pexpr list -> 'a2) -> 'a1
    instr list -> 'a4 **)

let cmd_rect asmop hmk hnil hcons hasgn hopn hsyscall hif hfor hwhile hcall =
  cmd_rect_aux asmop hnil hcons
    (instr_Rect asmop hmk hnil hcons hasgn hopn hsyscall hif hfor hwhile
      hcall)

module type FunInfoT =
 sig
  type t

  val witness : t

  val entry_info : t -> instr_info

  val ret_info : t -> instr_info
 end

module FunInfo =
 struct
  type t = positive

  (** val witness : t **)

  let witness =
    Coq_xH

  (** val entry_info : t -> instr_info **)

  let entry_info _ =
    dummy_instr_info

  (** val ret_info : t -> instr_info **)

  let ret_info _ =
    dummy_instr_info
 end

type fun_info = FInfo.t

(** val entry_info_of_fun_info : fun_info -> instr_info **)

let entry_info_of_fun_info = FInfo.entry_info

(** val ret_info_of_fun_info : fun_info -> instr_info **)

let ret_info_of_fun_info = FInfo.ret_info

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

(** val f_info : 'a1 asmOp -> ('a1, 'a2) _fundef -> fun_info **)

let f_info _ x =
  x.f_info

(** val f_tyin : 'a1 asmOp -> ('a1, 'a2) _fundef -> atype list **)

let f_tyin _ x =
  x.f_tyin

(** val f_params : 'a1 asmOp -> ('a1, 'a2) _fundef -> var_i list **)

let f_params _ x =
  x.f_params

(** val f_body : 'a1 asmOp -> ('a1, 'a2) _fundef -> 'a1 instr list **)

let f_body _ x =
  x.f_body

(** val f_tyout : 'a1 asmOp -> ('a1, 'a2) _fundef -> atype list **)

let f_tyout _ x =
  x.f_tyout

(** val f_res : 'a1 asmOp -> ('a1, 'a2) _fundef -> var_i list **)

let f_res _ x =
  x.f_res

(** val f_extra : 'a1 asmOp -> ('a1, 'a2) _fundef -> 'a2 **)

let f_extra _ x =
  x.f_extra

type ('asm_op, 'extra_fun_t) _fun_decl =
  funname * ('asm_op, 'extra_fun_t) _fundef

type ('asm_op, 'extra_fun_t, 'extra_prog_t) _prog = { p_funcs : ('asm_op,
                                                                'extra_fun_t)
                                                                _fun_decl list;
                                                      p_globs : glob_decl list;
                                                      p_extra : 'extra_prog_t }

(** val p_funcs :
    'a1 asmOp -> ('a1, 'a2, 'a3) _prog -> ('a1, 'a2) _fun_decl list **)

let p_funcs _ x =
  x.p_funcs

(** val p_globs : 'a1 asmOp -> ('a1, 'a2, 'a3) _prog -> glob_decl list **)

let p_globs _ x =
  x.p_globs

(** val p_extra : 'a1 asmOp -> ('a1, 'a2, 'a3) _prog -> 'a3 **)

let p_extra _ x =
  x.p_extra

type 'asm_op fundef = ('asm_op, extra_fun_t) _fundef

type function_signature = atype list * atype list

(** val signature_of_fundef :
    'a1 asmOp -> progT -> 'a1 fundef -> function_signature **)

let signature_of_fundef _ _ fd =
  (fd.f_tyin, fd.f_tyout)

type 'asm_op fun_decl = funname * 'asm_op fundef

type 'asm_op prog = ('asm_op, extra_fun_t, extra_prog_t) _prog

(** val coq_Build_prog :
    'a1 asmOp -> progT -> ('a1, extra_fun_t) _fun_decl list -> glob_decl list
    -> extra_prog_t -> 'a1 prog **)

let coq_Build_prog _ _ p_funcs0 p_globs0 p_extra0 =
  { p_funcs = p_funcs0; p_globs = p_globs0; p_extra = p_extra0 }

(** val progUnit : progT **)

let progUnit =
  Build_progT

type 'asm_op ufundef = 'asm_op fundef

type 'asm_op ufun_decl = 'asm_op fun_decl

type 'asm_op ufun_decls = 'asm_op fun_decl list

type 'asm_op uprog = 'asm_op prog

type 'asm_op _ufundef = ('asm_op, unit) _fundef

type 'asm_op _ufun_decl = ('asm_op, unit) _fun_decl

type 'asm_op _ufun_decls = ('asm_op, unit) _fun_decl list

type 'asm_op _uprog = ('asm_op, unit, unit) _prog

(** val to_uprog : 'a1 asmOp -> 'a1 _uprog -> 'a1 uprog **)

let to_uprog _ p =
  Obj.magic p

type saved_stack =
| SavedStackNone
| SavedStackReg of Var.var
| SavedStackStk of coq_Z

(** val saved_stack_beq : saved_stack -> saved_stack -> bool **)

let saved_stack_beq x y =
  match x with
  | SavedStackNone -> (match y with
                       | SavedStackNone -> true
                       | _ -> false)
  | SavedStackReg v1 ->
    (match y with
     | SavedStackReg v2 ->
       eq_op Var.coq_MvMake_var__canonical__eqtype_Equality (Obj.magic v1)
         (Obj.magic v2)
     | _ -> false)
  | SavedStackStk z1 ->
    (match y with
     | SavedStackStk z2 ->
       eq_op coq_BinNums_Z__canonical__eqtype_Equality (Obj.magic z1)
         (Obj.magic z2)
     | _ -> false)

(** val saved_stack_eq_axiom : saved_stack eq_axiom **)

let saved_stack_eq_axiom __top_assumption_ =
  let _evar_0_ = fun __top_assumption_0 ->
    let _evar_0_ = ReflectT in
    let _evar_0_0 = fun _ -> ReflectF in
    let _evar_0_1 = fun _ -> ReflectF in
    (match __top_assumption_0 with
     | SavedStackNone -> _evar_0_
     | SavedStackReg v -> _evar_0_0 v
     | SavedStackStk z -> _evar_0_1 z)
  in
  let _evar_0_0 = fun v1 __top_assumption_0 ->
    let _evar_0_0 = ReflectF in
    let _evar_0_1 = fun v2 ->
      iffP (eq_op Var.coq_MvMake_var__canonical__eqtype_Equality v1 v2)
        (eqP Var.coq_MvMake_var__canonical__eqtype_Equality v1 v2)
    in
    let _evar_0_2 = fun _ -> ReflectF in
    (match __top_assumption_0 with
     | SavedStackNone -> _evar_0_0
     | SavedStackReg v -> Obj.magic _evar_0_1 v
     | SavedStackStk z -> _evar_0_2 z)
  in
  let _evar_0_1 = fun z1 __top_assumption_0 ->
    let _evar_0_1 = ReflectF in
    let _evar_0_2 = fun _ -> ReflectF in
    let _evar_0_3 = fun z2 ->
      iffP (eq_op coq_BinNums_Z__canonical__eqtype_Equality z1 z2)
        (eqP coq_BinNums_Z__canonical__eqtype_Equality z1 z2)
    in
    (match __top_assumption_0 with
     | SavedStackNone -> _evar_0_1
     | SavedStackReg v -> _evar_0_2 v
     | SavedStackStk z -> Obj.magic _evar_0_3 z)
  in
  (match __top_assumption_ with
   | SavedStackNone -> _evar_0_
   | SavedStackReg v -> Obj.magic _evar_0_0 v
   | SavedStackStk z -> Obj.magic _evar_0_1 z)

(** val coq_HB_unnamed_factory_15 : saved_stack Coq_hasDecEq.axioms_ **)

let coq_HB_unnamed_factory_15 =
  { Coq_hasDecEq.eq_op = saved_stack_beq; Coq_hasDecEq.eqP =
    saved_stack_eq_axiom }

(** val expr_saved_stack__canonical__eqtype_Equality : Equality.coq_type **)

let expr_saved_stack__canonical__eqtype_Equality =
  Obj.magic coq_HB_unnamed_factory_15

type return_address_location =
| RAnone
| RAreg of Var.var * Var.var option
| RAstack of Var.var option * Var.var option * coq_Z * Var.var option

(** val is_RAnone : return_address_location -> bool **)

let is_RAnone = function
| RAnone -> true
| _ -> false

(** val is_RAstack : return_address_location -> bool **)

let is_RAstack = function
| RAstack (_, _, _, _) -> true
| _ -> false

(** val return_address_location_beq :
    return_address_location -> return_address_location -> bool **)

let return_address_location_beq r1 r2 =
  match r1 with
  | RAnone -> (match r2 with
               | RAnone -> true
               | _ -> false)
  | RAreg (x1, o1) ->
    (match r2 with
     | RAreg (x2, o2) ->
       (&&)
         (eq_op Var.coq_MvMake_var__canonical__eqtype_Equality (Obj.magic x1)
           (Obj.magic x2))
         (eq_op
           (coq_Datatypes_option__canonical__eqtype_Equality
             Var.coq_MvMake_var__canonical__eqtype_Equality) (Obj.magic o1)
           (Obj.magic o2))
     | _ -> false)
  | RAstack (ra_call1, ra_return1, z1, o1) ->
    (match r2 with
     | RAstack (ra_call2, ra_return2, z2, o2) ->
       (&&)
         (eq_op
           (coq_Datatypes_option__canonical__eqtype_Equality
             Var.coq_MvMake_var__canonical__eqtype_Equality)
           (Obj.magic ra_call1) (Obj.magic ra_call2))
         ((&&)
           (eq_op
             (coq_Datatypes_option__canonical__eqtype_Equality
               Var.coq_MvMake_var__canonical__eqtype_Equality)
             (Obj.magic ra_return1) (Obj.magic ra_return2))
           ((&&)
             (eq_op coq_BinNums_Z__canonical__eqtype_Equality (Obj.magic z1)
               (Obj.magic z2))
             (eq_op
               (coq_Datatypes_option__canonical__eqtype_Equality
                 Var.coq_MvMake_var__canonical__eqtype_Equality)
               (Obj.magic o1) (Obj.magic o2))))
     | _ -> false)

(** val return_address_location_eq_axiom :
    return_address_location eq_axiom **)

let return_address_location_eq_axiom _top_assumption_ =
  let _evar_0_ = fun __top_assumption_ ->
    let _evar_0_ = ReflectT in
    let _evar_0_0 = fun _ _ -> ReflectF in
    let _evar_0_1 = fun _ _ _ _ -> ReflectF in
    (match __top_assumption_ with
     | RAnone -> _evar_0_
     | RAreg (v, o) -> _evar_0_0 v o
     | RAstack (o, o0, z, o1) -> _evar_0_1 o o0 z o1)
  in
  let _evar_0_0 = fun x1 o1 __top_assumption_ ->
    let _evar_0_0 = ReflectF in
    let _evar_0_1 = fun x2 o2 ->
      iffP
        ((&&) (eq_op Var.coq_MvMake_var__canonical__eqtype_Equality x1 x2)
          (eq_op
            (coq_Datatypes_option__canonical__eqtype_Equality
              Var.coq_MvMake_var__canonical__eqtype_Equality) o1 o2))
        (andP (eq_op Var.coq_MvMake_var__canonical__eqtype_Equality x1 x2)
          (eq_op
            (coq_Datatypes_option__canonical__eqtype_Equality
              Var.coq_MvMake_var__canonical__eqtype_Equality) o1 o2))
    in
    let _evar_0_2 = fun _ _ _ _ -> ReflectF in
    (match __top_assumption_ with
     | RAnone -> _evar_0_0
     | RAreg (v, o) -> Obj.magic _evar_0_1 v o
     | RAstack (o, o0, z, o2) -> _evar_0_2 o o0 z o2)
  in
  let _evar_0_1 = fun ra_call1 ra_return1 z1 o1 __top_assumption_ ->
    let _evar_0_1 = ReflectF in
    let _evar_0_2 = fun _ _ -> ReflectF in
    let _evar_0_3 = fun ra_call2 ra_return2 z2 o2 ->
      iffP
        ((&&)
          (eq_op
            (coq_Datatypes_option__canonical__eqtype_Equality
              Var.coq_MvMake_var__canonical__eqtype_Equality) ra_call1
            ra_call2)
          ((&&)
            (eq_op
              (coq_Datatypes_option__canonical__eqtype_Equality
                Var.coq_MvMake_var__canonical__eqtype_Equality) ra_return1
              ra_return2)
            ((&&) (eq_op coq_BinNums_Z__canonical__eqtype_Equality z1 z2)
              (eq_op
                (coq_Datatypes_option__canonical__eqtype_Equality
                  Var.coq_MvMake_var__canonical__eqtype_Equality) o1 o2))))
        (and4P
          (eq_op
            (coq_Datatypes_option__canonical__eqtype_Equality
              Var.coq_MvMake_var__canonical__eqtype_Equality) ra_call1
            ra_call2)
          (eq_op
            (coq_Datatypes_option__canonical__eqtype_Equality
              Var.coq_MvMake_var__canonical__eqtype_Equality) ra_return1
            ra_return2)
          (eq_op coq_BinNums_Z__canonical__eqtype_Equality z1 z2)
          (eq_op
            (coq_Datatypes_option__canonical__eqtype_Equality
              Var.coq_MvMake_var__canonical__eqtype_Equality) o1 o2))
    in
    (match __top_assumption_ with
     | RAnone -> _evar_0_1
     | RAreg (v, o) -> _evar_0_2 v o
     | RAstack (o, o0, z, o2) -> Obj.magic _evar_0_3 o o0 z o2)
  in
  (match _top_assumption_ with
   | RAnone -> _evar_0_
   | RAreg (v, o) -> Obj.magic _evar_0_0 v o
   | RAstack (o, o0, z, o1) -> Obj.magic _evar_0_1 o o0 z o1)

(** val coq_HB_unnamed_factory_17 :
    return_address_location Coq_hasDecEq.axioms_ **)

let coq_HB_unnamed_factory_17 =
  { Coq_hasDecEq.eq_op = return_address_location_beq; Coq_hasDecEq.eqP =
    return_address_location_eq_axiom }

(** val expr_return_address_location__canonical__eqtype_Equality :
    Equality.coq_type **)

let expr_return_address_location__canonical__eqtype_Equality =
  Obj.magic coq_HB_unnamed_factory_17

(* -------------------------------------------------------------------- *)
(* Masking / dependency metadata (PoC) carried through f_extra / sf_extra *)
(* -------------------------------------------------------------------- *)

type mask_kind =
  | MKinput  (* stack array named input* *)
  | MKoutput (* stack array named output* *)
  | MKrnd    (* stack array named rnd *)
  | MKother  (* anything else *)

type mask_array_info = {
  mai_kind  : mask_kind;
  mai_base  : string;  (* variable base name, e.g. "inputA" / "rnd" *)
  mai_len   : coq_Z;   (* total size in bytes, from size_of(vtype) *)
  mai_align : wsize;   (* alignment from stack slot *)
  mai_ofs   : coq_Z;   (* stack offset (oracle-provided) *)
}


type share_info = string * int  (* (sharename, share_index) *)
type skey = var_i * int     (* (base var, element index) *)


module SKeyOrd = struct
  type t = skey
  let compare ((v1 :var_i ),(i1:int)) ((v2: var_i),(i2:int)) = 
    match (Var0.Var.var_cmp) v1.v_var v2.v_var  with 
    | Eq ->   i1 - i2
    | Lt -> -1
    | Gt -> 1
end

type randomness_info = int  (* (sharename, share_index) *)
type rkey = var_i * int     (* (base var, element index) *)


module RKeyOrd = struct
  type t = rkey
  let compare ((v1 :var_i ),(i1:int)) ((v2: var_i),(i2:int)) = 
    match (Var0.Var.var_cmp) v1.v_var v2.v_var  with 
    | Eq -> i1 - i2
    | Lt -> -1
    | Gt -> 1

end

module ShareMap = Map.Make(SKeyOrd)

type smap = share_info ShareMap.t

module RandomnessMap = Map.Make(RKeyOrd)

type rmap = randomness_info RandomnessMap.t

type mask_layout = mask_array_info list





  
type stk_fun_extra = { sf_align : wsize; sf_stk_sz : coq_Z;
                       sf_stk_ioff : coq_Z; sf_stk_extra_sz : coq_Z;
                       sf_stk_max : coq_Z; sf_max_call_depth : coq_Z;
                       sf_to_save : (Var.var * coq_Z) list;
                       sf_save_stack : saved_stack;
                       sf_return_address : return_address_location;
                       sf_align_args : wsize list ;
                       sf_random_layout : rmap;
                       sf_masking_layout : smap;
                       sf_output_layout : smap}


(** val sf_mask_layout : stk_fun_extra -> mask_layout **)

let sf_masking_layout s =
  s.sf_masking_layout

let sf_output_layout s =
  s.sf_masking_layout

let sf_random_layout s =
  s.sf_random_layout
(*
let sf_mask_layout s =
  s.sf_mask_layout*)
(* -------------------------------------------------------------------- *)
(* Masking / dependency metadata (PoC) carried through f_extra / sf_extra *)
(* -------------------------------------------------------------------- *)



(** val sf_align : stk_fun_extra -> wsize **)


let sf_align s =
  s.sf_align

(** val sf_stk_sz : stk_fun_extra -> coq_Z **)

let sf_stk_sz s =
  s.sf_stk_sz

(** val sf_stk_ioff : stk_fun_extra -> coq_Z **)

let sf_stk_ioff s =
  s.sf_stk_ioff

(** val sf_stk_extra_sz : stk_fun_extra -> coq_Z **)

let sf_stk_extra_sz s =
  s.sf_stk_extra_sz

(** val sf_stk_max : stk_fun_extra -> coq_Z **)

let sf_stk_max s =
  s.sf_stk_max

(** val sf_max_call_depth : stk_fun_extra -> coq_Z **)

let sf_max_call_depth s =
  s.sf_max_call_depth

(** val sf_to_save : stk_fun_extra -> (Var.var * coq_Z) list **)

let sf_to_save s =
  s.sf_to_save

(** val sf_save_stack : stk_fun_extra -> saved_stack **)

let sf_save_stack s =
  s.sf_save_stack

(** val sf_return_address : stk_fun_extra -> return_address_location **)

let sf_return_address s =
  s.sf_return_address

(** val sf_align_args : stk_fun_extra -> wsize list **)

let sf_align_args s =
  s.sf_align_args

type sprog_extra = { sp_rsp : Ident.Ident.ident; sp_rip : Ident.Ident.ident;
                     sp_globs : GRing.ComRing.sort list;
                     sp_glob_names : ((Var.var * wsize) * coq_Z) list }

(** val sp_rsp : sprog_extra -> Ident.Ident.ident **)

let sp_rsp s =
  s.sp_rsp

(** val sp_rip : sprog_extra -> Ident.Ident.ident **)

let sp_rip s =
  s.sp_rip

(** val sp_globs : sprog_extra -> GRing.ComRing.sort list **)

let sp_globs s =
  s.sp_globs

(** val sp_glob_names : sprog_extra -> ((Var.var * wsize) * coq_Z) list **)

let sp_glob_names s =
  s.sp_glob_names

(** val progStack : coq_PointerData -> progT **)

let progStack _ =
  Build_progT

type 'asm_op sfundef = 'asm_op fundef

type 'asm_op sfun_decl = 'asm_op fun_decl

type 'asm_op sfun_decls = 'asm_op fun_decl list

type 'asm_op sprog = 'asm_op prog

type 'asm_op _sfundef = ('asm_op, stk_fun_extra) _fundef

type 'asm_op _sfun_decl = ('asm_op, stk_fun_extra) _fun_decl

type 'asm_op _sfun_decls = ('asm_op, stk_fun_extra) _fun_decl list

type 'asm_op _sprog = ('asm_op, stk_fun_extra, sprog_extra) _prog

(** val to_sprog : coq_PointerData -> 'a1 asmOp -> 'a1 _sprog -> 'a1 sprog **)

let to_sprog _ _ p =
  Obj.magic p

(** val with_body :
    'a1 asmOp -> ('a1, 'a2) _fundef -> 'a1 instr list -> ('a1, 'a2) _fundef **)

let with_body _ fd body =
  { f_info = fd.f_info; f_tyin = fd.f_tyin; f_params = fd.f_params; f_body =
    body; f_tyout = fd.f_tyout; f_res = fd.f_res; f_extra = fd.f_extra }

(** val swith_extra :
    coq_PointerData -> 'a1 asmOp -> coq_PointerData -> 'a1 ufundef ->
    extra_fun_t -> 'a1 sfundef **)

let swith_extra _ _ _ fd f_extra0 =
  { f_info = fd.f_info; f_tyin = fd.f_tyin; f_params = fd.f_params; f_body =
    fd.f_body; f_tyout = fd.f_tyout; f_res = fd.f_res; f_extra = f_extra0 }

(** val is_const : pexpr -> coq_Z option **)

let is_const = function
| Pconst n -> Some n
| _ -> None

(** val is_bool : pexpr -> bool option **)

let is_bool = function
| Pbool b -> Some b
| _ -> None

(** val is_Papp2 : pexpr -> ((sop2 * pexpr) * pexpr) option **)

let is_Papp2 = function
| Papp2 (op, e0, e1) -> Some ((op, e0), e1)
| _ -> None

(** val is_Pload : pexpr -> bool **)

let is_Pload = function
| Pload (_, _, _) -> true
| _ -> false

(** val is_load : pexpr -> bool **)

let is_load = function
| Pvar g ->
  let { gv = x; gs = gs0 } = g in
  (match gs0 with
   | Slocal -> is_var_in_memory x.v_var
   | Sglob -> true)
| Pget (_, _, _, _, _) -> true
| Pload (_, _, _) -> true
| _ -> false

(** val is_array_init : pexpr -> bool **)

let is_array_init = function
| Parr_init (_, _) -> true
| _ -> false

(** val cast_w : wsize -> pexpr -> pexpr **)

let rec cast_w ws e = match e with
| Papp1 (s, e') ->
  (match s with
   | Oint_of_word (sign, ws') ->
     (match sign with
      | Signed ->
        if eq_op wsize_wsize__canonical__eqtype_Equality (Obj.magic ws)
             (Obj.magic ws')
        then e'
        else Papp1 ((Oword_of_int ws), e)
      | Unsigned ->
        if cmp_le wsize_cmp ws ws' then e' else Papp1 ((Oword_of_int ws), e))
   | Oneg o ->
     (match o with
      | Op_int -> Papp1 ((Oneg (Op_w ws)), (cast_w ws e'))
      | Op_w _ -> Papp1 ((Oword_of_int ws), e))
   | _ -> Papp1 ((Oword_of_int ws), e))
| Papp2 (s, e1, e2) ->
  (match s with
   | Oadd o ->
     (match o with
      | Op_int -> Papp2 ((Oadd (Op_w ws)), (cast_w ws e1), (cast_w ws e2))
      | Op_w _ -> Papp1 ((Oword_of_int ws), e))
   | Omul o ->
     (match o with
      | Op_int -> Papp2 ((Omul (Op_w ws)), (cast_w ws e1), (cast_w ws e2))
      | Op_w _ -> Papp1 ((Oword_of_int ws), e))
   | Osub o ->
     (match o with
      | Op_int -> Papp2 ((Osub (Op_w ws)), (cast_w ws e1), (cast_w ws e2))
      | Op_w _ -> Papp1 ((Oword_of_int ws), e))
   | _ -> Papp1 ((Oword_of_int ws), e))
| _ -> Papp1 ((Oword_of_int ws), e)

(** val cast_ptr : coq_PointerData -> pexpr -> pexpr **)

let cast_ptr =
  cast_w

(** val cast_const : coq_PointerData -> coq_Z -> pexpr **)

let cast_const pd z =
  cast_ptr pd (Pconst z)

(** val eword_of_int : wsize -> coq_Z -> pexpr **)

let eword_of_int ws x =
  Papp1 ((Oword_of_int ws), (Pconst x))

(** val wconst : wsize -> GRing.ComRing.sort -> pexpr **)

let wconst sz n =
  Papp1 ((Oword_of_int sz), (Pconst (wunsigned sz n)))

(** val is_wconst : wsize -> pexpr -> GRing.ComRing.sort option **)

let is_wconst sz = function
| Papp1 (s, e0) ->
  (match s with
   | Oword_of_int sz' ->
     if cmp_le wsize_cmp sz sz'
     then Ssrfun.Option.bind (fun n -> Some (wrepr sz n)) (is_const e0)
     else None
   | _ -> None)
| _ -> None

(** val is_wconst_of_size : Equality.sort -> pexpr -> coq_Z option **)

let is_wconst_of_size sz = function
| Papp1 (s, p) ->
  (match s with
   | Oword_of_int sz' ->
     (match p with
      | Pconst z ->
        if eq_op wsize_wsize__canonical__eqtype_Equality (Obj.magic sz') sz
        then Some z
        else None
      | _ -> None)
   | _ -> None)
| _ -> None

(** val vrv_rec : SvExtra.Sv.t -> lval -> SvExtra.Sv.t **)

let vrv_rec s = function
| Lvar x -> SvExtra.Sv.add (Obj.magic x.v_var) s
| Laset (_, _, _, x, _) -> SvExtra.Sv.add (Obj.magic x.v_var) s
| Lasub (_, _, _, x, _) -> SvExtra.Sv.add (Obj.magic x.v_var) s
| _ -> s

(** val vrvs_rec : SvExtra.Sv.t -> lval list -> SvExtra.Sv.t **)

let vrvs_rec s rv =
  foldl vrv_rec s rv

(** val vrv : lval -> SvExtra.Sv.t **)

let vrv =
  vrv_rec SvExtra.Sv.empty

(** val vrvs : lval list -> SvExtra.Sv.t **)

let vrvs =
  vrvs_rec SvExtra.Sv.empty

(** val lv_write_mem : lval -> bool **)

let lv_write_mem = function
| Lmem (_, _, _, _) -> true
| _ -> false

(** val write_i_rec :
    'a1 asmOp -> SvExtra.Sv.t -> 'a1 instr_r -> SvExtra.Sv.t **)

let write_i_rec _ =
  let rec write_i_rec0 s = function
  | Cassgn (x, _, _, _) -> vrv_rec s x
  | Copn (xs, _, _, _) -> vrvs_rec s xs
  | Csyscall (xs, _, _) -> vrvs_rec s xs
  | Cif (_, c1, c2) -> foldl write_I_rec0 (foldl write_I_rec0 s c2) c1
  | Cfor (x, _, c) ->
    foldl write_I_rec0 (SvExtra.Sv.add (Obj.magic x.v_var) s) c
  | Cwhile (_, c, _, _, c') -> foldl write_I_rec0 (foldl write_I_rec0 s c') c
  | Ccall (x, _, _) -> vrvs_rec s x
  and write_I_rec0 s = function
  | MkI (_, i0) -> write_i_rec0 s i0
  in write_i_rec0

(** val write_I_rec :
    'a1 asmOp -> SvExtra.Sv.t -> 'a1 instr -> SvExtra.Sv.t **)

let write_I_rec _ =
  let rec write_i_rec0 s = function
  | Cassgn (x, _, _, _) -> vrv_rec s x
  | Copn (xs, _, _, _) -> vrvs_rec s xs
  | Csyscall (xs, _, _) -> vrvs_rec s xs
  | Cif (_, c1, c2) -> foldl write_I_rec0 (foldl write_I_rec0 s c2) c1
  | Cfor (x, _, c) ->
    foldl write_I_rec0 (SvExtra.Sv.add (Obj.magic x.v_var) s) c
  | Cwhile (_, c, _, _, c') -> foldl write_I_rec0 (foldl write_I_rec0 s c') c
  | Ccall (x, _, _) -> vrvs_rec s x
  and write_I_rec0 s = function
  | MkI (_, i0) -> write_i_rec0 s i0
  in write_I_rec0

(** val write_i : 'a1 asmOp -> 'a1 instr_r -> SvExtra.Sv.t **)

let write_i asmop i =
  write_i_rec asmop SvExtra.Sv.empty i

(** val write_I : 'a1 asmOp -> 'a1 instr -> SvExtra.Sv.t **)

let write_I asmop i =
  write_I_rec asmop SvExtra.Sv.empty i

(** val write_c_rec :
    'a1 asmOp -> SvExtra.Sv.t -> 'a1 instr list -> SvExtra.Sv.t **)

let write_c_rec asmop s c =
  foldl (write_I_rec asmop) s c

(** val write_c : 'a1 asmOp -> 'a1 instr list -> SvExtra.Sv.t **)

let write_c asmop c =
  write_c_rec asmop SvExtra.Sv.empty c

(** val use_mem : pexpr -> bool **)

let rec use_mem = function
| Pget (_, _, _, _, e0) -> use_mem e0
| Psub (_, _, _, _, e0) -> use_mem e0
| Pload (_, _, _) -> true
| Papp1 (_, e0) -> use_mem e0
| Papp2 (_, e1, e2) -> (||) (use_mem e1) (use_mem e2)
| PappN (_, es) -> has use_mem es
| Pif (_, e0, e1, e2) -> (||) ((||) (use_mem e0) (use_mem e1)) (use_mem e2)
| _ -> false

(** val read_gvar : gvar -> SvExtra.Sv.t **)

let read_gvar x =
  if is_lvar x
  then SvExtra.Sv.singleton (Obj.magic x.gv.v_var)
  else SvExtra.Sv.empty

(** val read_e_rec : SvExtra.Sv.t -> pexpr -> SvExtra.Sv.t **)

let rec read_e_rec s = function
| Pvar x -> SvExtra.Sv.union (read_gvar x) s
| Pget (_, _, _, x, e0) -> read_e_rec (SvExtra.Sv.union (read_gvar x) s) e0
| Psub (_, _, _, x, e0) -> read_e_rec (SvExtra.Sv.union (read_gvar x) s) e0
| Pload (_, _, e0) -> read_e_rec s e0
| Papp1 (_, e0) -> read_e_rec s e0
| Papp2 (_, e1, e2) -> read_e_rec (read_e_rec s e2) e1
| PappN (_, es) -> foldl read_e_rec s es
| Pif (_, t0, e1, e2) -> read_e_rec (read_e_rec (read_e_rec s e2) e1) t0
| _ -> s

(** val read_e : pexpr -> SvExtra.Sv.t **)

let read_e =
  read_e_rec SvExtra.Sv.empty

(** val read_es_rec : SvExtra.Sv.t -> pexpr list -> SvExtra.Sv.t **)

let read_es_rec =
  foldl read_e_rec

(** val read_es : pexpr list -> SvExtra.Sv.t **)

let read_es =
  read_es_rec SvExtra.Sv.empty

(** val read_rv_rec : SvExtra.Sv.t -> lval -> SvExtra.Sv.t **)

let read_rv_rec s = function
| Lmem (_, _, _, e) -> read_e_rec s e
| Laset (_, _, _, x, e) -> read_e_rec (SvExtra.Sv.add (Obj.magic x.v_var) s) e
| Lasub (_, _, _, x, e) -> read_e_rec (SvExtra.Sv.add (Obj.magic x.v_var) s) e
| _ -> s

(** val read_rv : lval -> SvExtra.Sv.t **)

let read_rv =
  read_rv_rec SvExtra.Sv.empty

(** val read_rvs_rec : SvExtra.Sv.t -> lval list -> SvExtra.Sv.t **)

let read_rvs_rec =
  foldl read_rv_rec

(** val read_rvs : lval list -> SvExtra.Sv.t **)

let read_rvs =
  read_rvs_rec SvExtra.Sv.empty

(** val read_i_rec :
    'a1 asmOp -> SvExtra.Sv.t -> 'a1 instr_r -> SvExtra.Sv.t **)

let read_i_rec _ =
  let rec read_i_rec0 s = function
  | Cassgn (x, _, _, e) -> read_rv_rec (read_e_rec s e) x
  | Copn (xs, _, _, es) -> read_es_rec (read_rvs_rec s xs) es
  | Csyscall (xs, _, es) -> read_es_rec (read_rvs_rec s xs) es
  | Cif (b, c1, c2) ->
    let s0 = foldl read_I_rec0 s c1 in
    let s1 = foldl read_I_rec0 s0 c2 in read_e_rec s1 b
  | Cfor (_, r, c) ->
    let (p, e2) = r in
    let (_, e1) = p in
    let s0 = foldl read_I_rec0 s c in read_e_rec (read_e_rec s0 e2) e1
  | Cwhile (_, c, e, _, c') ->
    let s0 = foldl read_I_rec0 s c in
    let s1 = foldl read_I_rec0 s0 c' in read_e_rec s1 e
  | Ccall (xs, _, es) -> read_es_rec (read_rvs_rec s xs) es
  and read_I_rec0 s = function
  | MkI (_, i0) -> read_i_rec0 s i0
  in read_i_rec0

(** val read_I_rec :
    'a1 asmOp -> SvExtra.Sv.t -> 'a1 instr -> SvExtra.Sv.t **)

let read_I_rec _ =
  let rec read_i_rec0 s = function
  | Cassgn (x, _, _, e) -> read_rv_rec (read_e_rec s e) x
  | Copn (xs, _, _, es) -> read_es_rec (read_rvs_rec s xs) es
  | Csyscall (xs, _, es) -> read_es_rec (read_rvs_rec s xs) es
  | Cif (b, c1, c2) ->
    let s0 = foldl read_I_rec0 s c1 in
    let s1 = foldl read_I_rec0 s0 c2 in read_e_rec s1 b
  | Cfor (_, r, c) ->
    let (p, e2) = r in
    let (_, e1) = p in
    let s0 = foldl read_I_rec0 s c in read_e_rec (read_e_rec s0 e2) e1
  | Cwhile (_, c, e, _, c') ->
    let s0 = foldl read_I_rec0 s c in
    let s1 = foldl read_I_rec0 s0 c' in read_e_rec s1 e
  | Ccall (xs, _, es) -> read_es_rec (read_rvs_rec s xs) es
  and read_I_rec0 s = function
  | MkI (_, i0) -> read_i_rec0 s i0
  in read_I_rec0

(** val read_c_rec :
    'a1 asmOp -> SvExtra.Sv.t -> 'a1 instr list -> SvExtra.Sv.t **)

let read_c_rec asmop =
  foldl (read_I_rec asmop)

(** val read_i : 'a1 asmOp -> 'a1 instr_r -> SvExtra.Sv.t **)

let read_i asmop =
  read_i_rec asmop SvExtra.Sv.empty

(** val read_I : 'a1 asmOp -> 'a1 instr -> SvExtra.Sv.t **)

let read_I asmop =
  read_I_rec asmop SvExtra.Sv.empty

(** val read_c : 'a1 asmOp -> 'a1 instr list -> SvExtra.Sv.t **)

let read_c asmop =
  read_c_rec asmop SvExtra.Sv.empty

(** val vars_I : 'a1 asmOp -> 'a1 instr -> SvExtra.Sv.t **)

let vars_I asmop i =
  SvExtra.Sv.union (read_I asmop i) (write_I asmop i)

(** val vars_c : 'a1 asmOp -> 'a1 instr list -> SvExtra.Sv.t **)

let vars_c asmop c =
  SvExtra.Sv.union (read_c asmop c) (write_c asmop c)

(** val vars_lval : lval -> SvExtra.Sv.t **)

let vars_lval l =
  SvExtra.Sv.union (read_rv l) (vrv l)

(** val vars_lvals : lval list -> SvExtra.Sv.t **)

let vars_lvals ls =
  SvExtra.Sv.union (read_rvs ls) (vrvs ls)

(** val vars_l : var_i list -> SvExtra.Sv.t **)

let rec vars_l = function
| [] -> SvExtra.Sv.empty
| h :: q -> SvExtra.Sv.add (Obj.magic h.v_var) (vars_l q)

(** val vars_fd : 'a1 asmOp -> progT -> 'a1 fundef -> SvExtra.Sv.t **)

let vars_fd asmop _ fd =
  SvExtra.Sv.union (vars_l fd.f_params)
    (SvExtra.Sv.union (vars_l fd.f_res) (vars_c asmop fd.f_body))

(** val vars_p : 'a1 asmOp -> progT -> 'a1 fun_decl list -> SvExtra.Sv.t **)

let vars_p asmop pT p =
  foldr (fun f x ->
    let (_, fd) = f in SvExtra.Sv.union x (vars_fd asmop pT fd))
    SvExtra.Sv.empty p

(** val eq_gvar : gvar -> gvar -> bool **)

let eq_gvar x x' =
  (&&)
    (eq_op expr_v_scope__canonical__eqtype_Equality (Obj.magic x.gs)
      (Obj.magic x'.gs))
    (eq_op Var.coq_MvMake_var__canonical__eqtype_Equality
      (Obj.magic x.gv.v_var) (Obj.magic x'.gv.v_var))

(** val eq_expr : pexpr -> pexpr -> bool **)

let rec eq_expr e e' =
  match e with
  | Pconst z ->
    (match e' with
     | Pconst z' ->
       eq_op coq_BinNums_Z__canonical__eqtype_Equality (Obj.magic z)
         (Obj.magic z')
     | _ -> false)
  | Pbool b ->
    (match e' with
     | Pbool b' ->
       eq_op coq_Datatypes_bool__canonical__eqtype_Equality (Obj.magic b)
         (Obj.magic b')
     | _ -> false)
  | Parr_init (ws, n) ->
    (match e' with
     | Parr_init (ws', n') ->
       (&&)
         (eq_op wsize_wsize__canonical__eqtype_Equality (Obj.magic ws)
           (Obj.magic ws'))
         (eq_op coq_BinNums_positive__canonical__eqtype_Equality
           (Obj.magic n) (Obj.magic n'))
     | _ -> false)
  | Pvar x -> (match e' with
               | Pvar x' -> eq_gvar x x'
               | _ -> false)
  | Pget (al, aa, w, x, e0) ->
    (match e' with
     | Pget (al', aa', w', x', e'0) ->
       (&&)
         ((&&)
           ((&&)
             ((&&)
               (eq_op memory_model_aligned__canonical__eqtype_Equality
                 (Obj.magic al) (Obj.magic al'))
               (eq_op warray__arr_access__canonical__eqtype_Equality
                 (Obj.magic aa) (Obj.magic aa')))
             (eq_op wsize_wsize__canonical__eqtype_Equality (Obj.magic w)
               (Obj.magic w'))) (eq_gvar x x')) (eq_expr e0 e'0)
     | _ -> false)
  | Psub (aa, w, len, x, e0) ->
    (match e' with
     | Psub (aa', w', len', x', e'0) ->
       (&&)
         ((&&)
           ((&&)
             ((&&)
               (eq_op warray__arr_access__canonical__eqtype_Equality
                 (Obj.magic aa) (Obj.magic aa'))
               (eq_op wsize_wsize__canonical__eqtype_Equality (Obj.magic w)
                 (Obj.magic w')))
             (eq_op coq_BinNums_positive__canonical__eqtype_Equality
               (Obj.magic len) (Obj.magic len'))) (eq_gvar x x'))
         (eq_expr e0 e'0)
     | _ -> false)
  | Pload (al, w, e0) ->
    (match e' with
     | Pload (al', w', e'0) ->
       (&&)
         ((&&)
           (eq_op memory_model_aligned__canonical__eqtype_Equality
             (Obj.magic al) (Obj.magic al'))
           (eq_op wsize_wsize__canonical__eqtype_Equality (Obj.magic w)
             (Obj.magic w'))) (eq_expr e0 e'0)
     | _ -> false)
  | Papp1 (o, e0) ->
    (match e' with
     | Papp1 (o', e'0) ->
       (&&)
         (eq_op expr_sop1__canonical__eqtype_Equality (Obj.magic o)
           (Obj.magic o')) (eq_expr e0 e'0)
     | _ -> false)
  | Papp2 (o, e1, e2) ->
    (match e' with
     | Papp2 (o', e1', e2') ->
       (&&)
         ((&&)
           (eq_op expr_sop2__canonical__eqtype_Equality (Obj.magic o)
             (Obj.magic o')) (eq_expr e1 e1')) (eq_expr e2 e2')
     | _ -> false)
  | PappN (o, es) ->
    (match e' with
     | PappN (o', es') ->
       (&&)
         (eq_op expr_opN__canonical__eqtype_Equality (Obj.magic o)
           (Obj.magic o')) (all2 eq_expr es es')
     | _ -> false)
  | Pif (t0, e0, e1, e2) ->
    (match e' with
     | Pif (t', e'0, e1', e2') ->
       (&&)
         ((&&)
           ((&&)
             (eq_op type_atype__canonical__eqtype_Equality (Obj.magic t0)
               (Obj.magic t')) (eq_expr e0 e'0)) (eq_expr e1 e1'))
         (eq_expr e2 e2')
     | _ -> false)

(** val to_lvals : Var.var list -> lval list **)

let to_lvals l =
  map (fun x -> Lvar (mk_var_i x)) l

(** val is_false : pexpr -> bool **)

let is_false = function
| Pbool b -> if b then false else true
| _ -> false

(** val is_zero : Equality.sort -> pexpr -> bool **)

let is_zero sz = function
| Papp1 (s, p) ->
  (match s with
   | Oword_of_int sz' ->
     (match p with
      | Pconst z ->
        (match z with
         | Z0 ->
           eq_op wsize_wsize__canonical__eqtype_Equality (Obj.magic sz') sz
         | _ -> false)
      | _ -> false)
   | _ -> false)
| _ -> false

(** val instr_of_copn_args :
    'a1 asmOp -> assgn_tag -> ((lval list * 'a1 sopn) * pexpr list) -> 'a1
    instr_r **)

let instr_of_copn_args _ tg args =
  Copn ((fst (fst args)), tg, (snd (fst args)), (snd args))

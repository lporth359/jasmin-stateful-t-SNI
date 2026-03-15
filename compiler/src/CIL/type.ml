open BinInt
open BinNums
open BinPos
open Bool
open Datatypes
open Eqb_core_defs
open Eqtype
open Utils0
open Word_ssrZ
open Wsize

type __ = Obj.t

type ltype =
| Coq_lbool
| Coq_lword of wsize

(** val ltype_tag : ltype -> positive **)

let ltype_tag = function
| Coq_lbool -> Coq_xH
| Coq_lword _ -> Coq_xO Coq_xH

type box_ltype_lbool =
| Box_ltype_lbool

type ltype_fields_t = __

(** val ltype_fields : ltype -> ltype_fields_t **)

let ltype_fields = function
| Coq_lbool -> Obj.magic Box_ltype_lbool
| Coq_lword h -> Obj.magic h

(** val ltype_eqb_fields :
    (ltype -> ltype -> bool) -> positive -> ltype_fields_t -> ltype_fields_t
    -> bool **)

let ltype_eqb_fields _ x x0 x1 =
  match x with
  | Coq_xO _ -> (&&) (wsize_eqb (Obj.magic x0) (Obj.magic x1)) true
  | _ -> true

(** val ltype_eqb : ltype -> ltype -> bool **)

let ltype_eqb x1 x2 =
  match x1 with
  | Coq_lbool ->
    eqb_body ltype_tag ltype_fields
      (Obj.magic ltype_eqb_fields (fun _ _ -> true)) (ltype_tag Coq_lbool)
      Box_ltype_lbool x2
  | Coq_lword h ->
    eqb_body ltype_tag ltype_fields
      (Obj.magic ltype_eqb_fields (fun _ _ -> true))
      (ltype_tag (Coq_lword h)) h x2

(** val ltype_eqb_OK : ltype -> ltype -> reflect **)

let ltype_eqb_OK =
  iffP2 ltype_eqb

type atype =
| Coq_abool
| Coq_aint
| Coq_aarr of wsize * positive
| Coq_aword of wsize

(** val atype_tag : atype -> positive **)

let atype_tag = function
| Coq_abool -> Coq_xH
| Coq_aint -> Coq_xO Coq_xH
| Coq_aarr (_, _) -> Coq_xI Coq_xH
| Coq_aword _ -> Coq_xO (Coq_xO Coq_xH)

type box_atype_abool =
| Box_atype_abool

type box_atype_aarr = { coq_Box_atype_aarr_0 : wsize;
                        coq_Box_atype_aarr_1 : positive }

type atype_fields_t = __

(** val atype_fields : atype -> atype_fields_t **)

let atype_fields = function
| Coq_aarr (h, h0) ->
  Obj.magic { coq_Box_atype_aarr_0 = h; coq_Box_atype_aarr_1 = h0 }
| Coq_aword h -> Obj.magic h
| _ -> Obj.magic Box_atype_abool

(** val atype_eqb_fields :
    (atype -> atype -> bool) -> positive -> atype_fields_t -> atype_fields_t
    -> bool **)

let atype_eqb_fields _ x a b =
  match x with
  | Coq_xI _ ->
    let { coq_Box_atype_aarr_0 = box_atype_aarr_0; coq_Box_atype_aarr_1 =
      box_atype_aarr_1 } = Obj.magic a
    in
    let { coq_Box_atype_aarr_0 = box_atype_aarr_2; coq_Box_atype_aarr_1 =
      box_atype_aarr_3 } = Obj.magic b
    in
    (&&) (wsize_eqb box_atype_aarr_0 box_atype_aarr_2)
      ((&&) (positive_eqb box_atype_aarr_1 box_atype_aarr_3) true)
  | Coq_xO x0 ->
    (match x0 with
     | Coq_xO _ -> (&&) (wsize_eqb (Obj.magic a) (Obj.magic b)) true
     | _ -> true)
  | Coq_xH -> true

(** val atype_eqb : atype -> atype -> bool **)

let atype_eqb x1 x2 =
  match x1 with
  | Coq_aarr (h, h0) ->
    eqb_body atype_tag atype_fields
      (Obj.magic atype_eqb_fields (fun _ _ -> true))
      (atype_tag (Coq_aarr (h, h0))) { coq_Box_atype_aarr_0 = h;
      coq_Box_atype_aarr_1 = h0 } x2
  | Coq_aword h ->
    eqb_body atype_tag atype_fields
      (Obj.magic atype_eqb_fields (fun _ _ -> true))
      (atype_tag (Coq_aword h)) h x2
  | x ->
    eqb_body atype_tag atype_fields
      (Obj.magic atype_eqb_fields (fun _ _ -> true)) (atype_tag x)
      Box_atype_abool x2

(** val atype_eqb_OK : atype -> atype -> reflect **)

let atype_eqb_OK =
  iffP2 atype_eqb

type ctype =
| Coq_cbool
| Coq_cint
| Coq_carr of positive
| Coq_cword of wsize

(** val ctype_tag : ctype -> positive **)

let ctype_tag = function
| Coq_cbool -> Coq_xH
| Coq_cint -> Coq_xO Coq_xH
| Coq_carr _ -> Coq_xI Coq_xH
| Coq_cword _ -> Coq_xO (Coq_xO Coq_xH)

type box_ctype_cbool =
| Box_ctype_cbool

type ctype_fields_t = __

(** val ctype_fields : ctype -> ctype_fields_t **)

let ctype_fields = function
| Coq_carr h -> Obj.magic h
| Coq_cword h -> Obj.magic h
| _ -> Obj.magic Box_ctype_cbool

(** val ctype_eqb_fields :
    (ctype -> ctype -> bool) -> positive -> ctype_fields_t -> ctype_fields_t
    -> bool **)

let ctype_eqb_fields _ x a b =
  match x with
  | Coq_xI _ -> (&&) (positive_eqb (Obj.magic a) (Obj.magic b)) true
  | Coq_xO x0 ->
    (match x0 with
     | Coq_xO _ -> (&&) (wsize_eqb (Obj.magic a) (Obj.magic b)) true
     | _ -> true)
  | Coq_xH -> true

(** val ctype_eqb : ctype -> ctype -> bool **)

let ctype_eqb x1 x2 =
  match x1 with
  | Coq_carr h ->
    eqb_body ctype_tag ctype_fields
      (Obj.magic ctype_eqb_fields (fun _ _ -> true)) (ctype_tag (Coq_carr h))
      h x2
  | Coq_cword h ->
    eqb_body ctype_tag ctype_fields
      (Obj.magic ctype_eqb_fields (fun _ _ -> true))
      (ctype_tag (Coq_cword h)) h x2
  | x ->
    eqb_body ctype_tag ctype_fields
      (Obj.magic ctype_eqb_fields (fun _ _ -> true)) (ctype_tag x)
      Box_ctype_cbool x2

(** val ctype_eqb_OK : ctype -> ctype -> reflect **)

let ctype_eqb_OK =
  iffP2 ctype_eqb

(** val atype_of_ltype : ltype -> atype **)

let atype_of_ltype = function
| Coq_lbool -> Coq_abool
| Coq_lword ws -> Coq_aword ws

(** val ltype_of_atype : atype -> ltype option **)

let ltype_of_atype = function
| Coq_abool -> Some Coq_lbool
| Coq_aword ws -> Some (Coq_lword ws)
| _ -> None

(** val coq_HB_unnamed_factory_1 : ltype Coq_hasDecEq.axioms_ **)

let coq_HB_unnamed_factory_1 =
  { Coq_hasDecEq.eq_op = ltype_eqb; Coq_hasDecEq.eqP = ltype_eqb_OK }

(** val type_ltype__canonical__eqtype_Equality : Equality.coq_type **)

let type_ltype__canonical__eqtype_Equality =
  Obj.magic coq_HB_unnamed_factory_1

(** val coq_HB_unnamed_factory_3 : atype Coq_hasDecEq.axioms_ **)

let coq_HB_unnamed_factory_3 =
  { Coq_hasDecEq.eq_op = atype_eqb; Coq_hasDecEq.eqP = atype_eqb_OK }

(** val type_atype__canonical__eqtype_Equality : Equality.coq_type **)

let type_atype__canonical__eqtype_Equality =
  Obj.magic coq_HB_unnamed_factory_3

(** val coq_HB_unnamed_factory_5 : ctype Coq_hasDecEq.axioms_ **)

let coq_HB_unnamed_factory_5 =
  { Coq_hasDecEq.eq_op = ctype_eqb; Coq_hasDecEq.eqP = ctype_eqb_OK }

(** val type_ctype__canonical__eqtype_Equality : Equality.coq_type **)

let type_ctype__canonical__eqtype_Equality =
  Obj.magic coq_HB_unnamed_factory_5

(** val atype_cmp : atype -> atype -> comparison **)

let atype_cmp t t' =
  match t with
  | Coq_abool -> (match t' with
                  | Coq_abool -> Eq
                  | _ -> Lt)
  | Coq_aint -> (match t' with
                 | Coq_abool -> Gt
                 | Coq_aint -> Eq
                 | _ -> Lt)
  | Coq_aarr (ws, n) ->
    (match t' with
     | Coq_aarr (ws', n') ->
       (match wsize_cmp ws ws' with
        | Eq -> Pos.compare n n'
        | x -> x)
     | _ -> Gt)
  | Coq_aword w ->
    (match t' with
     | Coq_aarr (_, _) -> Lt
     | Coq_aword w' -> wsize_cmp w w'
     | _ -> Gt)

module OtherDefs =
 struct
  (** val is_aarr : atype -> bool **)

  let is_aarr = function
  | Coq_aarr (_, _) -> true
  | _ -> false

  (** val is_word_type : atype -> wsize option **)

  let is_word_type = function
  | Coq_aword sz -> Some sz
  | _ -> None

  (** val is_cword : ctype -> bool **)

  let is_cword = function
  | Coq_cword _ -> true
  | _ -> false
 end

(** val arr_size : wsize -> positive -> coq_Z **)

let arr_size ws len =
  Z.mul (wsize_size ws) (Zpos len)

(** val eval_atype : atype -> ctype **)

let eval_atype = function
| Coq_abool -> Coq_cbool
| Coq_aint -> Coq_cint
| Coq_aarr (ws, len) -> Coq_carr (Z.to_pos (arr_size ws len))
| Coq_aword ws -> Coq_cword ws

(** val eval_ltype : ltype -> ctype **)

let eval_ltype = function
| Coq_lbool -> Coq_cbool
| Coq_lword ws -> Coq_cword ws

(** val convertible : atype -> atype -> bool **)

let convertible t t' =
  match t with
  | Coq_aarr (ws, n) ->
    (match t' with
     | Coq_aarr (ws', n') ->
       eq_op coq_BinNums_Z__canonical__eqtype_Equality
         (Obj.magic arr_size ws n) (Obj.magic arr_size ws' n')
     | _ -> false)
  | _ ->
    eq_op type_atype__canonical__eqtype_Equality (Obj.magic t) (Obj.magic t')

(** val subatype : atype -> atype -> bool **)

let subatype t t' =
  match t with
  | Coq_aword w ->
    (match t' with
     | Coq_aword w' -> cmp_le wsize_cmp w w'
     | _ -> false)
  | _ -> convertible t t'

(** val subctype : ctype -> ctype -> bool **)

let subctype t t' =
  match t with
  | Coq_cword w ->
    (match t' with
     | Coq_cword w' -> cmp_le wsize_cmp w w'
     | _ -> false)
  | _ ->
    eq_op type_ctype__canonical__eqtype_Equality (Obj.magic t) (Obj.magic t')

type 'len extended_type =
| ETbool
| ETint
| ETarr of wsize * 'len
| ETword of signedness option * wsize

(** val tbool : 'a1 extended_type **)

let tbool =
  ETbool

(** val tint : 'a1 extended_type **)

let tint =
  ETint

(** val tarr : wsize -> 'a1 -> 'a1 extended_type **)

let tarr ws l =
  ETarr (ws, l)

(** val tword : wsize -> 'a1 extended_type **)

let tword ws =
  ETword (None, ws)

(** val twint : signedness -> wsize -> 'a1 extended_type **)

let twint s ws =
  ETword ((Some s), ws)

(** val tuint : wsize -> 'a1 extended_type **)

let tuint ws =
  twint Unsigned ws

(** val to_atype : positive extended_type -> atype **)

let to_atype = function
| ETbool -> Coq_abool
| ETint -> Coq_aint
| ETarr (ws, l) -> Coq_aarr (ws, l)
| ETword (_, ws) -> Coq_aword ws

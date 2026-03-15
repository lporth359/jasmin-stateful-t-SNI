open BinNums
open Bool
open Datatypes
open Eqb_core_defs
open Param1

type __ = Obj.t

module Prelude =
 struct
  (** val bool_tag : bool -> positive **)

  let bool_tag = function
  | true -> Coq_xH
  | false -> Coq_xO Coq_xH

  type box_bool_true =
  | Box_bool_true

  type bool_fields_t = __

  (** val bool_fields : bool -> bool_fields_t **)

  let bool_fields _ =
    Obj.magic Box_bool_true

  (** val bool_eqb_fields :
      (bool -> bool -> bool) -> positive -> bool_fields_t -> bool_fields_t ->
      bool **)

  let bool_eqb_fields _ _ _ _ =
    true

  (** val bool_eqb : bool -> bool -> bool **)

  let bool_eqb x1 x2 =
    if x1
    then eqb_body bool_tag bool_fields
           (Obj.magic bool_eqb_fields (fun _ _ -> true)) (bool_tag true)
           Box_bool_true x2
    else eqb_body bool_tag bool_fields
           (Obj.magic bool_eqb_fields (fun _ _ -> true)) (bool_tag false)
           Box_bool_true x2

  type ('a, 'pA) is_option =
  | Coq_is_Some of 'a * 'pA
  | Coq_is_None

  (** val option_tag : 'a1 option -> positive **)

  let option_tag = function
  | Some _ -> Coq_xH
  | None -> Coq_xO Coq_xH

  (** val is_option_inhab :
      ('a1, 'a2) full -> 'a1 option -> ('a1, 'a2) is_option **)

  let is_option_inhab h = function
  | Some h0 -> Coq_is_Some (h0, (h h0))
  | None -> Coq_is_None

  type 'a box_option_None =
  | Box_option_None

  type 'p option_fields_t = __

  (** val option_fields : 'a1 option -> 'a1 option_fields_t **)

  let option_fields = function
  | Some h -> Obj.magic h
  | None -> Obj.magic Box_option_None

  (** val option_eqb_fields :
      ('a1 -> 'a1 -> bool) -> ('a1 option -> 'a1 option -> bool) -> positive
      -> 'a1 option_fields_t -> 'a1 option_fields_t -> bool **)

  let option_eqb_fields eqp _ x x0 x1 =
    match x with
    | Coq_xH -> (&&) (eqp (Obj.magic x0) (Obj.magic x1)) true
    | _ -> true

  (** val option_eqb :
      ('a1 -> 'a1 -> bool) -> 'a1 option -> 'a1 option -> bool **)

  let option_eqb eqx x1 x2 =
    match x1 with
    | Some h ->
      eqb_body option_tag option_fields
        (Obj.magic option_eqb_fields eqx (fun _ _ -> true))
        (option_tag (Some h)) h x2
    | None ->
      eqb_body option_tag option_fields
        (Obj.magic option_eqb_fields eqx (fun _ _ -> true)) (option_tag None)
        Box_option_None x2

  (** val list_tag : 'a1 list -> positive **)

  let list_tag = function
  | [] -> Coq_xH
  | _ :: _ -> Coq_xO Coq_xH

  type 'a box_list_nil =
  | Box_list_nil

  type 'a box_list_cons = { coq_Box_list_cons_0 : 'a;
                            coq_Box_list_cons_1 : 'a list }

  type 'p list_fields_t = __

  (** val list_fields : 'a1 list -> 'a1 list_fields_t **)

  let list_fields = function
  | [] -> Obj.magic Box_list_nil
  | h :: h0 -> Obj.magic { coq_Box_list_cons_0 = h; coq_Box_list_cons_1 = h0 }

  (** val list_eqb_fields :
      ('a1 -> 'a1 -> bool) -> ('a1 list -> 'a1 list -> bool) -> positive ->
      'a1 list_fields_t -> 'a1 list_fields_t -> bool **)

  let list_eqb_fields eqp rec0 x x0 x1 =
    match x with
    | Coq_xO _ ->
      let { coq_Box_list_cons_0 = box_list_cons_0; coq_Box_list_cons_1 =
        box_list_cons_1 } = Obj.magic x0
      in
      let { coq_Box_list_cons_0 = box_list_cons_2; coq_Box_list_cons_1 =
        box_list_cons_3 } = Obj.magic x1
      in
      (&&) (eqp box_list_cons_0 box_list_cons_2)
        ((&&) (rec0 box_list_cons_1 box_list_cons_3) true)
    | _ -> true

  (** val list_eqb : ('a1 -> 'a1 -> bool) -> 'a1 list -> 'a1 list -> bool **)

  let rec list_eqb eqx x1 x2 =
    match x1 with
    | [] ->
      eqb_body list_tag list_fields
        (Obj.magic list_eqb_fields eqx (list_eqb eqx)) (list_tag [])
        Box_list_nil x2
    | h :: h0 ->
      eqb_body list_tag list_fields
        (Obj.magic list_eqb_fields eqx (list_eqb eqx)) (list_tag (h :: h0))
        { coq_Box_list_cons_0 = h; coq_Box_list_cons_1 = h0 } x2

  (** val comparison_tag : comparison -> positive **)

  let comparison_tag = function
  | Eq -> Coq_xH
  | Lt -> Coq_xO Coq_xH
  | Gt -> Coq_xI Coq_xH

  type box_comparison_Eq =
  | Box_comparison_Eq

  type comparison_fields_t = __

  (** val comparison_fields : comparison -> comparison_fields_t **)

  let comparison_fields _ =
    Obj.magic Box_comparison_Eq

  (** val comparison_eqb_fields :
      (comparison -> comparison -> bool) -> positive -> comparison_fields_t
      -> comparison_fields_t -> bool **)

  let comparison_eqb_fields _ _ _ _ =
    true

  (** val comparison_eqb : comparison -> comparison -> bool **)

  let comparison_eqb x1 x2 =
    eqb_body comparison_tag comparison_fields
      (Obj.magic comparison_eqb_fields (fun _ _ -> true)) (comparison_tag x1)
      Box_comparison_Eq x2

  (** val comparison_eqb_OK : comparison -> comparison -> reflect **)

  let comparison_eqb_OK =
    iffP2 comparison_eqb
 end

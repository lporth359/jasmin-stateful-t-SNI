open BinNums
open Bool
open EqbOK
open Eqb_core_defs
open Eqtype

type __ = Obj.t

type stack_zero_strategy =
| SZSloop
| SZSloopSCT
| SZSunrolled

(** val stack_zero_strategy_rect :
    'a1 -> 'a1 -> 'a1 -> stack_zero_strategy -> 'a1 **)

let stack_zero_strategy_rect f f0 f1 = function
| SZSloop -> f
| SZSloopSCT -> f0
| SZSunrolled -> f1

(** val stack_zero_strategy_rec :
    'a1 -> 'a1 -> 'a1 -> stack_zero_strategy -> 'a1 **)

let stack_zero_strategy_rec f f0 f1 = function
| SZSloop -> f
| SZSloopSCT -> f0
| SZSunrolled -> f1

type is_stack_zero_strategy =
| Coq_is_SZSloop
| Coq_is_SZSloopSCT
| Coq_is_SZSunrolled

(** val is_stack_zero_strategy_rect :
    'a1 -> 'a1 -> 'a1 -> stack_zero_strategy -> is_stack_zero_strategy -> 'a1 **)

let is_stack_zero_strategy_rect f f0 f1 _ = function
| Coq_is_SZSloop -> f
| Coq_is_SZSloopSCT -> f0
| Coq_is_SZSunrolled -> f1

(** val is_stack_zero_strategy_rec :
    'a1 -> 'a1 -> 'a1 -> stack_zero_strategy -> is_stack_zero_strategy -> 'a1 **)

let is_stack_zero_strategy_rec f f0 f1 _ = function
| Coq_is_SZSloop -> f
| Coq_is_SZSloopSCT -> f0
| Coq_is_SZSunrolled -> f1

(** val stack_zero_strategy_tag : stack_zero_strategy -> positive **)

let stack_zero_strategy_tag = function
| SZSloop -> Coq_xH
| SZSloopSCT -> Coq_xO Coq_xH
| SZSunrolled -> Coq_xI Coq_xH

(** val is_stack_zero_strategy_inhab :
    stack_zero_strategy -> is_stack_zero_strategy **)

let is_stack_zero_strategy_inhab = function
| SZSloop -> Coq_is_SZSloop
| SZSloopSCT -> Coq_is_SZSloopSCT
| SZSunrolled -> Coq_is_SZSunrolled

(** val is_stack_zero_strategy_functor :
    stack_zero_strategy -> is_stack_zero_strategy -> is_stack_zero_strategy **)

let rec is_stack_zero_strategy_functor _ x =
  x

type box_stack_zero_strategy_SZSloop =
| Box_stack_zero_strategy_SZSloop

type stack_zero_strategy_fields_t = __

(** val stack_zero_strategy_fields :
    stack_zero_strategy -> stack_zero_strategy_fields_t **)

let stack_zero_strategy_fields _ =
  Obj.magic Box_stack_zero_strategy_SZSloop

(** val stack_zero_strategy_construct :
    positive -> stack_zero_strategy_fields_t -> stack_zero_strategy option **)

let stack_zero_strategy_construct p _ =
  match p with
  | Coq_xI _ -> Some SZSunrolled
  | Coq_xO _ -> Some SZSloopSCT
  | Coq_xH -> Some SZSloop

(** val stack_zero_strategy_induction :
    'a1 -> 'a1 -> 'a1 -> stack_zero_strategy -> is_stack_zero_strategy -> 'a1 **)

let stack_zero_strategy_induction his_SZSloop his_SZSloopSCT his_SZSunrolled _ = function
| Coq_is_SZSloop -> his_SZSloop
| Coq_is_SZSloopSCT -> his_SZSloopSCT
| Coq_is_SZSunrolled -> his_SZSunrolled

(** val stack_zero_strategy_eqb_fields :
    (stack_zero_strategy -> stack_zero_strategy -> bool) -> positive ->
    stack_zero_strategy_fields_t -> stack_zero_strategy_fields_t -> bool **)

let stack_zero_strategy_eqb_fields _ _ _ _ =
  true

(** val stack_zero_strategy_eqb :
    stack_zero_strategy -> stack_zero_strategy -> bool **)

let stack_zero_strategy_eqb x1 x2 =
  eqb_body stack_zero_strategy_tag stack_zero_strategy_fields
    (Obj.magic stack_zero_strategy_eqb_fields (fun _ _ -> true))
    (stack_zero_strategy_tag x1) Box_stack_zero_strategy_SZSloop x2

(** val stack_zero_strategy_eqb_OK :
    stack_zero_strategy -> stack_zero_strategy -> reflect **)

let stack_zero_strategy_eqb_OK =
  iffP2 stack_zero_strategy_eqb

(** val stack_zero_strategy_eqb_OK_sumbool :
    stack_zero_strategy -> stack_zero_strategy -> bool **)

let stack_zero_strategy_eqb_OK_sumbool =
  reflect_dec stack_zero_strategy_eqb stack_zero_strategy_eqb_OK

(** val stack_zero_strategy_list : stack_zero_strategy list **)

let stack_zero_strategy_list =
  SZSloop :: (SZSloopSCT :: (SZSunrolled :: []))

(** val coq_HB_unnamed_factory_1 :
    stack_zero_strategy Coq_hasDecEq.axioms_ **)

let coq_HB_unnamed_factory_1 =
  { Coq_hasDecEq.eq_op = stack_zero_strategy_eqb; Coq_hasDecEq.eqP =
    stack_zero_strategy_eqb_OK }

(** val stack_zero_strategy_stack_zero_strategy__canonical__eqtype_Equality :
    Equality.coq_type **)

let stack_zero_strategy_stack_zero_strategy__canonical__eqtype_Equality =
  Obj.magic coq_HB_unnamed_factory_1

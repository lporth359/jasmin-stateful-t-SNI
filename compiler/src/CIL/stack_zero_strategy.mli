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

val stack_zero_strategy_rect : 'a1 -> 'a1 -> 'a1 -> stack_zero_strategy -> 'a1

val stack_zero_strategy_rec : 'a1 -> 'a1 -> 'a1 -> stack_zero_strategy -> 'a1

type is_stack_zero_strategy =
| Coq_is_SZSloop
| Coq_is_SZSloopSCT
| Coq_is_SZSunrolled

val is_stack_zero_strategy_rect :
  'a1 -> 'a1 -> 'a1 -> stack_zero_strategy -> is_stack_zero_strategy -> 'a1

val is_stack_zero_strategy_rec :
  'a1 -> 'a1 -> 'a1 -> stack_zero_strategy -> is_stack_zero_strategy -> 'a1

val stack_zero_strategy_tag : stack_zero_strategy -> positive

val is_stack_zero_strategy_inhab :
  stack_zero_strategy -> is_stack_zero_strategy

val is_stack_zero_strategy_functor :
  stack_zero_strategy -> is_stack_zero_strategy -> is_stack_zero_strategy

type box_stack_zero_strategy_SZSloop =
| Box_stack_zero_strategy_SZSloop

type stack_zero_strategy_fields_t = __

val stack_zero_strategy_fields :
  stack_zero_strategy -> stack_zero_strategy_fields_t

val stack_zero_strategy_construct :
  positive -> stack_zero_strategy_fields_t -> stack_zero_strategy option

val stack_zero_strategy_induction :
  'a1 -> 'a1 -> 'a1 -> stack_zero_strategy -> is_stack_zero_strategy -> 'a1

val stack_zero_strategy_eqb_fields :
  (stack_zero_strategy -> stack_zero_strategy -> bool) -> positive ->
  stack_zero_strategy_fields_t -> stack_zero_strategy_fields_t -> bool

val stack_zero_strategy_eqb :
  stack_zero_strategy -> stack_zero_strategy -> bool

val stack_zero_strategy_eqb_OK :
  stack_zero_strategy -> stack_zero_strategy -> reflect

val stack_zero_strategy_eqb_OK_sumbool :
  stack_zero_strategy -> stack_zero_strategy -> bool

val stack_zero_strategy_list : stack_zero_strategy list

val coq_HB_unnamed_factory_1 : stack_zero_strategy Coq_hasDecEq.axioms_

val stack_zero_strategy_stack_zero_strategy__canonical__eqtype_Equality :
  Equality.coq_type

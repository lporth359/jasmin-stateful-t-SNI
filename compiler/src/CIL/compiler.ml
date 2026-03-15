open BinInt
open BinNums
open Bool
open Datatypes
open List0
open Allocation
open Arch_decl
open Arch_extra
open Arch_params
open Array_copy
open Array_expansion
open Array_init
open Asm_gen
open Compiler_util
open Constant_prop
open Dead_calls
open Dead_code
open EqbOK
open Eqb_core_defs
open Eqtype
open Expr
open Flag_combination
open Global
open Inline
open Insert_renaming
open Linear
open Linearization
open Load_constants_in_cond
open Lower_spill
open Lowering
open MakeReferenceArguments
open Merge_varmaps
open Post_unrolling_check
open Propagate_inline
open Remove_globals
open Sem_type
open Seq
open Slh_lowering
open Sopn
open Ssralg
open Ssrbool
open Ssrfun
open Ssrnat
open Stack_alloc
open Stack_zero_strategy
open Stack_zeroization
open Tunneling
open Type
open Unrolling
open Utils0
open Var0
open Wint_word
open Wsize


type __ = Obj.t

(** val pp_s : string -> pp_error **)

let pp_s x =
  PPEstring x

(** val unroll :
    coq_MSFsize -> 'a1 asmOp -> coq_FlagCombinationParams -> ('a1 asm_op_t ->
    bool) -> nat -> 'a1 uprog -> 'a1 uprog cexec **)

let unroll msfsz asmop fcp is_move_op =
  let postprocess = fun p ->
    let p0 = const_prop_prog fcp msfsz asmop progUnit p in
    dead_code_prog asmop is_move_op progUnit p0 false
  in
  let rec unroll0 n p =
    match n with
    | O -> Error (loop_iterator "unrolling")
    | S n' ->
      let (p', repeat) = unroll_prog asmop progUnit p in
      if repeat
      then (match postprocess p' with
            | Ok x -> unroll0 n' x
            | Error s -> Error s)
      else Ok p
  in unroll0

(** val unroll_loop :
    coq_MSFsize -> 'a1 asmOp -> coq_FlagCombinationParams -> ('a1 asm_op_t ->
    bool) -> 'a1 uprog -> (pp_error_loc, 'a1 uprog) result **)

let unroll_loop msfsz asmop fcp is_move_op =
  let postprocess = fun p ->
    let p0 = const_prop_prog fcp msfsz asmop progUnit p in
    dead_code_prog asmop is_move_op progUnit p0 false
  in
  (fun p ->
  match postprocess p with
  | Ok x -> unroll msfsz asmop fcp is_move_op Loop.nb x
  | Error s -> Error s)

type compiler_step =
| Typing
| ParamsExpansion
| InsertRenaming
| WintWord
| ArrayCopy
| AddArrInit
| LowerSpill
| Inlining
| RemoveUnusedFunction
| Unrolling
| Splitting
| Renaming
| RemovePhiNodes
| DeadCode_Renaming
| RemoveArrInit
| MakeRefArguments
| RegArrayExpansion
| RemoveGlobal
| LoadConstantsInCond
| LowerInstruction
| PropagateInline
| SLHLowering
| LowerAddressing
| StackAllocation
| RemoveReturn
| RegAllocation
| DeadCode_RegAllocation
| Linearization
| StackZeroization
| Tunneling
| Assembly

(** val compiler_step_rect :
    'a1 -> 'a1 -> 'a1 -> 'a1 -> 'a1 -> 'a1 -> 'a1 -> 'a1 -> 'a1 -> 'a1 -> 'a1
    -> 'a1 -> 'a1 -> 'a1 -> 'a1 -> 'a1 -> 'a1 -> 'a1 -> 'a1 -> 'a1 -> 'a1 ->
    'a1 -> 'a1 -> 'a1 -> 'a1 -> 'a1 -> 'a1 -> 'a1 -> 'a1 -> 'a1 -> 'a1 ->
    compiler_step -> 'a1 **)

let compiler_step_rect f f0 f1 f2 f3 f4 f5 f6 f7 f8 f9 f10 f11 f12 f13 f14 f15 f16 f17 f18 f19 f20 f21 f22 f23 f24 f25 f26 f27 f28 f29 = function
| Typing -> f
| ParamsExpansion -> f0
| InsertRenaming -> f1
| WintWord -> f2
| ArrayCopy -> f3
| AddArrInit -> f4
| LowerSpill -> f5
| Inlining -> f6
| RemoveUnusedFunction -> f7
| Unrolling -> f8
| Splitting -> f9
| Renaming -> f10
| RemovePhiNodes -> f11
| DeadCode_Renaming -> f12
| RemoveArrInit -> f13
| MakeRefArguments -> f14
| RegArrayExpansion -> f15
| RemoveGlobal -> f16
| LoadConstantsInCond -> f17
| LowerInstruction -> f18
| PropagateInline -> f19
| SLHLowering -> f20
| LowerAddressing -> f21
| StackAllocation -> f22
| RemoveReturn -> f23
| RegAllocation -> f24
| DeadCode_RegAllocation -> f25
| Linearization -> f26
| StackZeroization -> f27
| Tunneling -> f28
| Assembly -> f29

(** val compiler_step_rec :
    'a1 -> 'a1 -> 'a1 -> 'a1 -> 'a1 -> 'a1 -> 'a1 -> 'a1 -> 'a1 -> 'a1 -> 'a1
    -> 'a1 -> 'a1 -> 'a1 -> 'a1 -> 'a1 -> 'a1 -> 'a1 -> 'a1 -> 'a1 -> 'a1 ->
    'a1 -> 'a1 -> 'a1 -> 'a1 -> 'a1 -> 'a1 -> 'a1 -> 'a1 -> 'a1 -> 'a1 ->
    compiler_step -> 'a1 **)

let compiler_step_rec f f0 f1 f2 f3 f4 f5 f6 f7 f8 f9 f10 f11 f12 f13 f14 f15 f16 f17 f18 f19 f20 f21 f22 f23 f24 f25 f26 f27 f28 f29 = function
| Typing -> f
| ParamsExpansion -> f0
| InsertRenaming -> f1
| WintWord -> f2
| ArrayCopy -> f3
| AddArrInit -> f4
| LowerSpill -> f5
| Inlining -> f6
| RemoveUnusedFunction -> f7
| Unrolling -> f8
| Splitting -> f9
| Renaming -> f10
| RemovePhiNodes -> f11
| DeadCode_Renaming -> f12
| RemoveArrInit -> f13
| MakeRefArguments -> f14
| RegArrayExpansion -> f15
| RemoveGlobal -> f16
| LoadConstantsInCond -> f17
| LowerInstruction -> f18
| PropagateInline -> f19
| SLHLowering -> f20
| LowerAddressing -> f21
| StackAllocation -> f22
| RemoveReturn -> f23
| RegAllocation -> f24
| DeadCode_RegAllocation -> f25
| Linearization -> f26
| StackZeroization -> f27
| Tunneling -> f28
| Assembly -> f29

type is_compiler_step =
| Coq_is_Typing
| Coq_is_ParamsExpansion
| Coq_is_InsertRenaming
| Coq_is_WintWord
| Coq_is_ArrayCopy
| Coq_is_AddArrInit
| Coq_is_LowerSpill
| Coq_is_Inlining
| Coq_is_RemoveUnusedFunction
| Coq_is_Unrolling
| Coq_is_Splitting
| Coq_is_Renaming
| Coq_is_RemovePhiNodes
| Coq_is_DeadCode_Renaming
| Coq_is_RemoveArrInit
| Coq_is_MakeRefArguments
| Coq_is_RegArrayExpansion
| Coq_is_RemoveGlobal
| Coq_is_LoadConstantsInCond
| Coq_is_LowerInstruction
| Coq_is_PropagateInline
| Coq_is_SLHLowering
| Coq_is_LowerAddressing
| Coq_is_StackAllocation
| Coq_is_RemoveReturn
| Coq_is_RegAllocation
| Coq_is_DeadCode_RegAllocation
| Coq_is_Linearization
| Coq_is_StackZeroization
| Coq_is_Tunneling
| Coq_is_Assembly

(** val is_compiler_step_rect :
    'a1 -> 'a1 -> 'a1 -> 'a1 -> 'a1 -> 'a1 -> 'a1 -> 'a1 -> 'a1 -> 'a1 -> 'a1
    -> 'a1 -> 'a1 -> 'a1 -> 'a1 -> 'a1 -> 'a1 -> 'a1 -> 'a1 -> 'a1 -> 'a1 ->
    'a1 -> 'a1 -> 'a1 -> 'a1 -> 'a1 -> 'a1 -> 'a1 -> 'a1 -> 'a1 -> 'a1 ->
    compiler_step -> is_compiler_step -> 'a1 **)

let is_compiler_step_rect f f0 f1 f2 f3 f4 f5 f6 f7 f8 f9 f10 f11 f12 f13 f14 f15 f16 f17 f18 f19 f20 f21 f22 f23 f24 f25 f26 f27 f28 f29 _ = function
| Coq_is_Typing -> f
| Coq_is_ParamsExpansion -> f0
| Coq_is_InsertRenaming -> f1
| Coq_is_WintWord -> f2
| Coq_is_ArrayCopy -> f3
| Coq_is_AddArrInit -> f4
| Coq_is_LowerSpill -> f5
| Coq_is_Inlining -> f6
| Coq_is_RemoveUnusedFunction -> f7
| Coq_is_Unrolling -> f8
| Coq_is_Splitting -> f9
| Coq_is_Renaming -> f10
| Coq_is_RemovePhiNodes -> f11
| Coq_is_DeadCode_Renaming -> f12
| Coq_is_RemoveArrInit -> f13
| Coq_is_MakeRefArguments -> f14
| Coq_is_RegArrayExpansion -> f15
| Coq_is_RemoveGlobal -> f16
| Coq_is_LoadConstantsInCond -> f17
| Coq_is_LowerInstruction -> f18
| Coq_is_PropagateInline -> f19
| Coq_is_SLHLowering -> f20
| Coq_is_LowerAddressing -> f21
| Coq_is_StackAllocation -> f22
| Coq_is_RemoveReturn -> f23
| Coq_is_RegAllocation -> f24
| Coq_is_DeadCode_RegAllocation -> f25
| Coq_is_Linearization -> f26
| Coq_is_StackZeroization -> f27
| Coq_is_Tunneling -> f28
| Coq_is_Assembly -> f29

(** val is_compiler_step_rec :
    'a1 -> 'a1 -> 'a1 -> 'a1 -> 'a1 -> 'a1 -> 'a1 -> 'a1 -> 'a1 -> 'a1 -> 'a1
    -> 'a1 -> 'a1 -> 'a1 -> 'a1 -> 'a1 -> 'a1 -> 'a1 -> 'a1 -> 'a1 -> 'a1 ->
    'a1 -> 'a1 -> 'a1 -> 'a1 -> 'a1 -> 'a1 -> 'a1 -> 'a1 -> 'a1 -> 'a1 ->
    compiler_step -> is_compiler_step -> 'a1 **)

let is_compiler_step_rec f f0 f1 f2 f3 f4 f5 f6 f7 f8 f9 f10 f11 f12 f13 f14 f15 f16 f17 f18 f19 f20 f21 f22 f23 f24 f25 f26 f27 f28 f29 _ = function
| Coq_is_Typing -> f
| Coq_is_ParamsExpansion -> f0
| Coq_is_InsertRenaming -> f1
| Coq_is_WintWord -> f2
| Coq_is_ArrayCopy -> f3
| Coq_is_AddArrInit -> f4
| Coq_is_LowerSpill -> f5
| Coq_is_Inlining -> f6
| Coq_is_RemoveUnusedFunction -> f7
| Coq_is_Unrolling -> f8
| Coq_is_Splitting -> f9
| Coq_is_Renaming -> f10
| Coq_is_RemovePhiNodes -> f11
| Coq_is_DeadCode_Renaming -> f12
| Coq_is_RemoveArrInit -> f13
| Coq_is_MakeRefArguments -> f14
| Coq_is_RegArrayExpansion -> f15
| Coq_is_RemoveGlobal -> f16
| Coq_is_LoadConstantsInCond -> f17
| Coq_is_LowerInstruction -> f18
| Coq_is_PropagateInline -> f19
| Coq_is_SLHLowering -> f20
| Coq_is_LowerAddressing -> f21
| Coq_is_StackAllocation -> f22
| Coq_is_RemoveReturn -> f23
| Coq_is_RegAllocation -> f24
| Coq_is_DeadCode_RegAllocation -> f25
| Coq_is_Linearization -> f26
| Coq_is_StackZeroization -> f27
| Coq_is_Tunneling -> f28
| Coq_is_Assembly -> f29

(** val compiler_step_tag : compiler_step -> positive **)

let compiler_step_tag = function
| Typing -> Coq_xH
| ParamsExpansion -> Coq_xO Coq_xH
| InsertRenaming -> Coq_xI Coq_xH
| WintWord -> Coq_xO (Coq_xO Coq_xH)
| ArrayCopy -> Coq_xI (Coq_xO Coq_xH)
| AddArrInit -> Coq_xO (Coq_xI Coq_xH)
| LowerSpill -> Coq_xI (Coq_xI Coq_xH)
| Inlining -> Coq_xO (Coq_xO (Coq_xO Coq_xH))
| RemoveUnusedFunction -> Coq_xI (Coq_xO (Coq_xO Coq_xH))
| Unrolling -> Coq_xO (Coq_xI (Coq_xO Coq_xH))
| Splitting -> Coq_xI (Coq_xI (Coq_xO Coq_xH))
| Renaming -> Coq_xO (Coq_xO (Coq_xI Coq_xH))
| RemovePhiNodes -> Coq_xI (Coq_xO (Coq_xI Coq_xH))
| DeadCode_Renaming -> Coq_xO (Coq_xI (Coq_xI Coq_xH))
| RemoveArrInit -> Coq_xI (Coq_xI (Coq_xI Coq_xH))
| MakeRefArguments -> Coq_xO (Coq_xO (Coq_xO (Coq_xO Coq_xH)))
| RegArrayExpansion -> Coq_xI (Coq_xO (Coq_xO (Coq_xO Coq_xH)))
| RemoveGlobal -> Coq_xO (Coq_xI (Coq_xO (Coq_xO Coq_xH)))
| LoadConstantsInCond -> Coq_xI (Coq_xI (Coq_xO (Coq_xO Coq_xH)))
| LowerInstruction -> Coq_xO (Coq_xO (Coq_xI (Coq_xO Coq_xH)))
| PropagateInline -> Coq_xI (Coq_xO (Coq_xI (Coq_xO Coq_xH)))
| SLHLowering -> Coq_xO (Coq_xI (Coq_xI (Coq_xO Coq_xH)))
| LowerAddressing -> Coq_xI (Coq_xI (Coq_xI (Coq_xO Coq_xH)))
| StackAllocation -> Coq_xO (Coq_xO (Coq_xO (Coq_xI Coq_xH)))
| RemoveReturn -> Coq_xI (Coq_xO (Coq_xO (Coq_xI Coq_xH)))
| RegAllocation -> Coq_xO (Coq_xI (Coq_xO (Coq_xI Coq_xH)))
| DeadCode_RegAllocation -> Coq_xI (Coq_xI (Coq_xO (Coq_xI Coq_xH)))
| Linearization -> Coq_xO (Coq_xO (Coq_xI (Coq_xI Coq_xH)))
| StackZeroization -> Coq_xI (Coq_xO (Coq_xI (Coq_xI Coq_xH)))
| Tunneling -> Coq_xO (Coq_xI (Coq_xI (Coq_xI Coq_xH)))
| Assembly -> Coq_xI (Coq_xI (Coq_xI (Coq_xI Coq_xH)))

(** val is_compiler_step_inhab : compiler_step -> is_compiler_step **)

let is_compiler_step_inhab = function
| Typing -> Coq_is_Typing
| ParamsExpansion -> Coq_is_ParamsExpansion
| InsertRenaming -> Coq_is_InsertRenaming
| WintWord -> Coq_is_WintWord
| ArrayCopy -> Coq_is_ArrayCopy
| AddArrInit -> Coq_is_AddArrInit
| LowerSpill -> Coq_is_LowerSpill
| Inlining -> Coq_is_Inlining
| RemoveUnusedFunction -> Coq_is_RemoveUnusedFunction
| Unrolling -> Coq_is_Unrolling
| Splitting -> Coq_is_Splitting
| Renaming -> Coq_is_Renaming
| RemovePhiNodes -> Coq_is_RemovePhiNodes
| DeadCode_Renaming -> Coq_is_DeadCode_Renaming
| RemoveArrInit -> Coq_is_RemoveArrInit
| MakeRefArguments -> Coq_is_MakeRefArguments
| RegArrayExpansion -> Coq_is_RegArrayExpansion
| RemoveGlobal -> Coq_is_RemoveGlobal
| LoadConstantsInCond -> Coq_is_LoadConstantsInCond
| LowerInstruction -> Coq_is_LowerInstruction
| PropagateInline -> Coq_is_PropagateInline
| SLHLowering -> Coq_is_SLHLowering
| LowerAddressing -> Coq_is_LowerAddressing
| StackAllocation -> Coq_is_StackAllocation
| RemoveReturn -> Coq_is_RemoveReturn
| RegAllocation -> Coq_is_RegAllocation
| DeadCode_RegAllocation -> Coq_is_DeadCode_RegAllocation
| Linearization -> Coq_is_Linearization
| StackZeroization -> Coq_is_StackZeroization
| Tunneling -> Coq_is_Tunneling
| Assembly -> Coq_is_Assembly

(** val is_compiler_step_functor :
    compiler_step -> is_compiler_step -> is_compiler_step **)

let rec is_compiler_step_functor _ x =
  x

type box_compiler_step_Typing =
| Box_compiler_step_Typing

type compiler_step_fields_t = __

(** val compiler_step_fields : compiler_step -> compiler_step_fields_t **)

let compiler_step_fields _ =
  Obj.magic Box_compiler_step_Typing

(** val compiler_step_construct :
    positive -> compiler_step_fields_t -> compiler_step option **)

let compiler_step_construct p _ =
  match p with
  | Coq_xI x ->
    (match x with
     | Coq_xI x0 ->
       (match x0 with
        | Coq_xI x1 ->
          (match x1 with
           | Coq_xI _ -> Some Assembly
           | Coq_xO _ -> Some LowerAddressing
           | Coq_xH -> Some RemoveArrInit)
        | Coq_xO x1 ->
          (match x1 with
           | Coq_xI _ -> Some DeadCode_RegAllocation
           | Coq_xO _ -> Some LoadConstantsInCond
           | Coq_xH -> Some Splitting)
        | Coq_xH -> Some LowerSpill)
     | Coq_xO x0 ->
       (match x0 with
        | Coq_xI x1 ->
          (match x1 with
           | Coq_xI _ -> Some StackZeroization
           | Coq_xO _ -> Some PropagateInline
           | Coq_xH -> Some RemovePhiNodes)
        | Coq_xO x1 ->
          (match x1 with
           | Coq_xI _ -> Some RemoveReturn
           | Coq_xO _ -> Some RegArrayExpansion
           | Coq_xH -> Some RemoveUnusedFunction)
        | Coq_xH -> Some ArrayCopy)
     | Coq_xH -> Some InsertRenaming)
  | Coq_xO x ->
    (match x with
     | Coq_xI x0 ->
       (match x0 with
        | Coq_xI x1 ->
          (match x1 with
           | Coq_xI _ -> Some Tunneling
           | Coq_xO _ -> Some SLHLowering
           | Coq_xH -> Some DeadCode_Renaming)
        | Coq_xO x1 ->
          (match x1 with
           | Coq_xI _ -> Some RegAllocation
           | Coq_xO _ -> Some RemoveGlobal
           | Coq_xH -> Some Unrolling)
        | Coq_xH -> Some AddArrInit)
     | Coq_xO x0 ->
       (match x0 with
        | Coq_xI x1 ->
          (match x1 with
           | Coq_xI _ -> Some Linearization
           | Coq_xO _ -> Some LowerInstruction
           | Coq_xH -> Some Renaming)
        | Coq_xO x1 ->
          (match x1 with
           | Coq_xI _ -> Some StackAllocation
           | Coq_xO _ -> Some MakeRefArguments
           | Coq_xH -> Some Inlining)
        | Coq_xH -> Some WintWord)
     | Coq_xH -> Some ParamsExpansion)
  | Coq_xH -> Some Typing

(** val compiler_step_induction :
    'a1 -> 'a1 -> 'a1 -> 'a1 -> 'a1 -> 'a1 -> 'a1 -> 'a1 -> 'a1 -> 'a1 -> 'a1
    -> 'a1 -> 'a1 -> 'a1 -> 'a1 -> 'a1 -> 'a1 -> 'a1 -> 'a1 -> 'a1 -> 'a1 ->
    'a1 -> 'a1 -> 'a1 -> 'a1 -> 'a1 -> 'a1 -> 'a1 -> 'a1 -> 'a1 -> 'a1 ->
    compiler_step -> is_compiler_step -> 'a1 **)

let compiler_step_induction his_Typing his_ParamsExpansion his_InsertRenaming his_WintWord his_ArrayCopy his_AddArrInit his_LowerSpill his_Inlining his_RemoveUnusedFunction his_Unrolling his_Splitting his_Renaming his_RemovePhiNodes his_DeadCode_Renaming his_RemoveArrInit his_MakeRefArguments his_RegArrayExpansion his_RemoveGlobal his_LoadConstantsInCond his_LowerInstruction his_PropagateInline his_SLHLowering his_LowerAddressing his_StackAllocation his_RemoveReturn his_RegAllocation his_DeadCode_RegAllocation his_Linearization his_StackZeroization his_Tunneling his_Assembly _ = function
| Coq_is_Typing -> his_Typing
| Coq_is_ParamsExpansion -> his_ParamsExpansion
| Coq_is_InsertRenaming -> his_InsertRenaming
| Coq_is_WintWord -> his_WintWord
| Coq_is_ArrayCopy -> his_ArrayCopy
| Coq_is_AddArrInit -> his_AddArrInit
| Coq_is_LowerSpill -> his_LowerSpill
| Coq_is_Inlining -> his_Inlining
| Coq_is_RemoveUnusedFunction -> his_RemoveUnusedFunction
| Coq_is_Unrolling -> his_Unrolling
| Coq_is_Splitting -> his_Splitting
| Coq_is_Renaming -> his_Renaming
| Coq_is_RemovePhiNodes -> his_RemovePhiNodes
| Coq_is_DeadCode_Renaming -> his_DeadCode_Renaming
| Coq_is_RemoveArrInit -> his_RemoveArrInit
| Coq_is_MakeRefArguments -> his_MakeRefArguments
| Coq_is_RegArrayExpansion -> his_RegArrayExpansion
| Coq_is_RemoveGlobal -> his_RemoveGlobal
| Coq_is_LoadConstantsInCond -> his_LoadConstantsInCond
| Coq_is_LowerInstruction -> his_LowerInstruction
| Coq_is_PropagateInline -> his_PropagateInline
| Coq_is_SLHLowering -> his_SLHLowering
| Coq_is_LowerAddressing -> his_LowerAddressing
| Coq_is_StackAllocation -> his_StackAllocation
| Coq_is_RemoveReturn -> his_RemoveReturn
| Coq_is_RegAllocation -> his_RegAllocation
| Coq_is_DeadCode_RegAllocation -> his_DeadCode_RegAllocation
| Coq_is_Linearization -> his_Linearization
| Coq_is_StackZeroization -> his_StackZeroization
| Coq_is_Tunneling -> his_Tunneling
| Coq_is_Assembly -> his_Assembly

(** val compiler_step_eqb_fields :
    (compiler_step -> compiler_step -> bool) -> positive ->
    compiler_step_fields_t -> compiler_step_fields_t -> bool **)

let compiler_step_eqb_fields _ _ _ _ =
  true

(** val compiler_step_eqb : compiler_step -> compiler_step -> bool **)

let compiler_step_eqb x1 x2 =
  eqb_body compiler_step_tag compiler_step_fields
    (Obj.magic compiler_step_eqb_fields (fun _ _ -> true))
    (compiler_step_tag x1) Box_compiler_step_Typing x2

(** val compiler_step_eqb_OK : compiler_step -> compiler_step -> reflect **)

let compiler_step_eqb_OK =
  iffP2 compiler_step_eqb

(** val compiler_step_eqb_OK_sumbool :
    compiler_step -> compiler_step -> bool **)

let compiler_step_eqb_OK_sumbool =
  reflect_dec compiler_step_eqb compiler_step_eqb_OK

(** val compiler_step_list : compiler_step list **)

let compiler_step_list =
  Typing :: (ParamsExpansion :: (InsertRenaming :: (WintWord :: (ArrayCopy :: (AddArrInit :: (LowerSpill :: (Inlining :: (RemoveUnusedFunction :: (Unrolling :: (Splitting :: (Renaming :: (RemovePhiNodes :: (DeadCode_Renaming :: (RemoveArrInit :: (MakeRefArguments :: (RegArrayExpansion :: (RemoveGlobal :: (LoadConstantsInCond :: (LowerInstruction :: (PropagateInline :: (SLHLowering :: (LowerAddressing :: (StackAllocation :: (RemoveReturn :: (RegAllocation :: (DeadCode_RegAllocation :: (Linearization :: (StackZeroization :: (Tunneling :: (Assembly :: []))))))))))))))))))))))))))))))

(** val coq_HB_unnamed_factory_1 : compiler_step Coq_hasDecEq.axioms_ **)

let coq_HB_unnamed_factory_1 =
  { Coq_hasDecEq.eq_op = compiler_step_eqb; Coq_hasDecEq.eqP =
    compiler_step_eqb_OK }

(** val compiler_compiler_step__canonical__eqtype_Equality :
    Equality.coq_type **)

let compiler_compiler_step__canonical__eqtype_Equality =
  Obj.magic coq_HB_unnamed_factory_1

type stack_alloc_oracles = { ao_globals : GRing.ComRing.sort list;
                             ao_global_alloc : ((Var.var * wsize) * coq_Z)
                                               list;
                             ao_stack_alloc : (funname -> stk_alloc_oracle_t) }

(** val ao_globals : stack_alloc_oracles -> GRing.ComRing.sort list **)

let ao_globals s =
  s.ao_globals

(** val ao_global_alloc :
    stack_alloc_oracles -> ((Var.var * wsize) * coq_Z) list **)

let ao_global_alloc s =
  s.ao_global_alloc

(** val ao_stack_alloc :
    stack_alloc_oracles -> funname -> stk_alloc_oracle_t **)

let ao_stack_alloc s =
  s.ao_stack_alloc

type ('asm_op, 'lowering_options) compiler_params = { rename_fd : (instr_info
                                                                  -> funname
                                                                  -> 'asm_op
                                                                  _ufundef ->
                                                                  'asm_op
                                                                  _ufundef);
                                                      expand_fd : (funname ->
                                                                  'asm_op
                                                                  _ufundef ->
                                                                  expand_info);
                                                      split_live_ranges_fd : 
                                                      (funname -> 'asm_op
                                                      _ufundef -> 'asm_op
                                                      _ufundef);
                                                      renaming_fd : (funname
                                                                    ->
                                                                    'asm_op
                                                                    _ufundef
                                                                    ->
                                                                    'asm_op
                                                                    _ufundef);
                                                      remove_phi_nodes_fd : 
                                                      (funname -> 'asm_op
                                                      _ufundef -> 'asm_op
                                                      _ufundef);
                                                      stack_register_symbol : 
                                                      Ident.Ident.ident;
                                                      global_static_data_symbol : 
                                                      Ident.Ident.ident;
                                                      stackalloc : ('asm_op
                                                                   _uprog ->
                                                                   stack_alloc_oracles);
                                                      removereturn : 
                                                      ('asm_op _sprog ->
                                                      funname -> bool list
                                                      option);
                                                      regalloc : ('asm_op
                                                                 _sfun_decl
                                                                 list ->
                                                                 'asm_op
                                                                 _sfun_decl
                                                                 list);
                                                      remove_wint_annot : 
                                                      (funname -> 'asm_op
                                                      _ufundef -> 'asm_op
                                                      _ufundef);
                                                      print_uprog : (compiler_step
                                                                    ->
                                                                    'asm_op
                                                                    _uprog ->
                                                                    'asm_op
                                                                    _uprog);
                                                      print_sprog : (compiler_step
                                                                    ->
                                                                    'asm_op
                                                                    _sprog ->
                                                                    'asm_op
                                                                    _sprog);
                                                      print_linear : 
                                                      (compiler_step ->
                                                      'asm_op lprog ->
                                                      'asm_op lprog);
                                                      refresh_instr_info : 
                                                      (funname -> 'asm_op
                                                      _ufundef -> 'asm_op
                                                      _ufundef);
                                                      warning : (instr_info
                                                                ->
                                                                warning_msg
                                                                -> instr_info);
                                                      lowering_opt : 
                                                      'lowering_options;
                                                      insert_renaming : 
                                                      (fun_info -> bool);
                                                      fresh_id : (glob_decl
                                                                 list ->
                                                                 Var.var ->
                                                                 Ident.Ident.ident);
                                                      fresh_var_ident : 
                                                      (v_kind -> instr_info
                                                      -> Uint63.t -> string
                                                      -> atype ->
                                                      Ident.Ident.ident);
                                                      slh_info : ('asm_op
                                                                 _uprog ->
                                                                 funname ->
                                                                 slh_t
                                                                 list * slh_t
                                                                 list);
                                                      stack_zero_info : 
                                                      (funname ->
                                                      (stack_zero_strategy * wsize
                                                      option) option);
                                                      dead_vars_ufd : 
                                                      ('asm_op _ufun_decl ->
                                                      instr_info ->
                                                      SvExtra.Sv.t);
                                                      dead_vars_sfd : 
                                                      ('asm_op _sfun_decl ->
                                                      instr_info ->
                                                      SvExtra.Sv.t);
                                                      pp_sr : (sub_region ->
                                                              pp_error);
                                                      security_transform_sprog : ('asm_op
                                                                    _sprog ->
                                                                    'asm_op
                                                                    _sprog);
                                                            }


let security_transform_sprog _ c = 
    c.security_transform_sprog

(** val rename_fd :
    'a1 asmOp -> ('a1, 'a2) compiler_params -> instr_info -> funname -> 'a1
    _ufundef -> 'a1 _ufundef **)

let rename_fd _ c =
  c.rename_fd

(** val expand_fd :
    'a1 asmOp -> ('a1, 'a2) compiler_params -> funname -> 'a1 _ufundef ->
    expand_info **)

let expand_fd _ c =
  c.expand_fd

(** val split_live_ranges_fd :
    'a1 asmOp -> ('a1, 'a2) compiler_params -> funname -> 'a1 _ufundef -> 'a1
    _ufundef **)

let split_live_ranges_fd _ c =
  c.split_live_ranges_fd

(** val renaming_fd :
    'a1 asmOp -> ('a1, 'a2) compiler_params -> funname -> 'a1 _ufundef -> 'a1
    _ufundef **)

let renaming_fd _ c =
  c.renaming_fd

(** val remove_phi_nodes_fd :
    'a1 asmOp -> ('a1, 'a2) compiler_params -> funname -> 'a1 _ufundef -> 'a1
    _ufundef **)

let remove_phi_nodes_fd _ c =
  c.remove_phi_nodes_fd

(** val stack_register_symbol :
    'a1 asmOp -> ('a1, 'a2) compiler_params -> Ident.Ident.ident **)

let stack_register_symbol _ c =
  c.stack_register_symbol

(** val global_static_data_symbol :
    'a1 asmOp -> ('a1, 'a2) compiler_params -> Ident.Ident.ident **)

let global_static_data_symbol _ c =
  c.global_static_data_symbol

(** val stackalloc :
    'a1 asmOp -> ('a1, 'a2) compiler_params -> 'a1 _uprog ->
    stack_alloc_oracles **)

let stackalloc _ c =
  c.stackalloc

(** val removereturn :
    'a1 asmOp -> ('a1, 'a2) compiler_params -> 'a1 _sprog -> funname -> bool
    list option **)

let removereturn _ c =
  c.removereturn

(** val regalloc :
    'a1 asmOp -> ('a1, 'a2) compiler_params -> 'a1 _sfun_decl list -> 'a1
    _sfun_decl list **)

let regalloc _ c =
  c.regalloc

(** val remove_wint_annot :
    'a1 asmOp -> ('a1, 'a2) compiler_params -> funname -> 'a1 _ufundef -> 'a1
    _ufundef **)

let remove_wint_annot _ c =
  c.remove_wint_annot

(** val print_uprog :
    'a1 asmOp -> ('a1, 'a2) compiler_params -> compiler_step -> 'a1 _uprog ->
    'a1 _uprog **)

let print_uprog _ c =
  c.print_uprog

(** val print_sprog :
    'a1 asmOp -> ('a1, 'a2) compiler_params -> compiler_step -> 'a1 _sprog ->
    'a1 _sprog **)

let print_sprog _ c =
  c.print_sprog

(** val print_linear :
    'a1 asmOp -> ('a1, 'a2) compiler_params -> compiler_step -> 'a1 lprog ->
    'a1 lprog **)

let print_linear _ c =
  c.print_linear

(** val refresh_instr_info :
    'a1 asmOp -> ('a1, 'a2) compiler_params -> funname -> 'a1 _ufundef -> 'a1
    _ufundef **)

let refresh_instr_info _ c =
  c.refresh_instr_info

(** val warning :
    'a1 asmOp -> ('a1, 'a2) compiler_params -> instr_info -> warning_msg ->
    instr_info **)

let warning _ c =
  c.warning

(** val lowering_opt : 'a1 asmOp -> ('a1, 'a2) compiler_params -> 'a2 **)

let lowering_opt _ c =
  c.lowering_opt

(** val insert_renaming :
    'a1 asmOp -> ('a1, 'a2) compiler_params -> fun_info -> bool **)

let insert_renaming _ c =
  c.insert_renaming

(** val fresh_id :
    'a1 asmOp -> ('a1, 'a2) compiler_params -> glob_decl list -> Var.var ->
    Ident.Ident.ident **)

let fresh_id _ c =
  c.fresh_id

(** val fresh_var_ident :
    'a1 asmOp -> ('a1, 'a2) compiler_params -> v_kind -> instr_info ->
    Uint63.t -> string -> atype -> Ident.Ident.ident **)

let fresh_var_ident _ c =
  c.fresh_var_ident

(** val slh_info :
    'a1 asmOp -> ('a1, 'a2) compiler_params -> 'a1 _uprog -> funname -> slh_t
    list * slh_t list **)

let slh_info _ c =
  c.slh_info

(** val stack_zero_info :
    'a1 asmOp -> ('a1, 'a2) compiler_params -> funname ->
    (stack_zero_strategy * wsize option) option **)

let stack_zero_info _ c =
  c.stack_zero_info

(** val dead_vars_ufd :
    'a1 asmOp -> ('a1, 'a2) compiler_params -> 'a1 _ufun_decl -> instr_info
    -> SvExtra.Sv.t **)

let dead_vars_ufd _ c =
  c.dead_vars_ufd

(** val dead_vars_sfd :
    'a1 asmOp -> ('a1, 'a2) compiler_params -> 'a1 _sfun_decl -> instr_info
    -> SvExtra.Sv.t **)

let dead_vars_sfd _ c =
  c.dead_vars_sfd

(** val pp_sr :
    'a1 asmOp -> ('a1, 'a2) compiler_params -> sub_region -> pp_error **)

let pp_sr _ c =
  c.pp_sr

(** val split_live_ranges_prog :
    ('a1, 'a2, 'a3, 'a4, 'a5, 'a6, 'a7) asm_extra -> (('a1, 'a2, 'a3, 'a4,
    'a5, 'a6, 'a7) extended_op, 'a8) compiler_params -> ('a1, 'a2, 'a3, 'a4,
    'a5, 'a6, 'a7) extended_op _uprog -> ('a1, 'a2, 'a3, 'a4, 'a5, 'a6, 'a7)
    extended_op _uprog **)

let split_live_ranges_prog asm_e cparams p =
  Obj.magic map_prog_name (asm_opI asm_e) progUnit
    cparams.split_live_ranges_fd p

(** val renaming_prog :
    ('a1, 'a2, 'a3, 'a4, 'a5, 'a6, 'a7) asm_extra -> (('a1, 'a2, 'a3, 'a4,
    'a5, 'a6, 'a7) extended_op, 'a8) compiler_params -> ('a1, 'a2, 'a3, 'a4,
    'a5, 'a6, 'a7) extended_op _uprog -> ('a1, 'a2, 'a3, 'a4, 'a5, 'a6, 'a7)
    extended_op _uprog **)

let renaming_prog asm_e cparams p =
  Obj.magic map_prog_name (asm_opI asm_e) progUnit cparams.renaming_fd p

(** val remove_phi_nodes_prog :
    ('a1, 'a2, 'a3, 'a4, 'a5, 'a6, 'a7) asm_extra -> (('a1, 'a2, 'a3, 'a4,
    'a5, 'a6, 'a7) extended_op, 'a8) compiler_params -> ('a1, 'a2, 'a3, 'a4,
    'a5, 'a6, 'a7) extended_op _uprog -> ('a1, 'a2, 'a3, 'a4, 'a5, 'a6, 'a7)
    extended_op _uprog **)

let remove_phi_nodes_prog asm_e cparams p =
  Obj.magic map_prog_name (asm_opI asm_e) progUnit
    cparams.remove_phi_nodes_fd p

(** val var_tmp :
    ('a1, 'a2, 'a3, 'a4, 'a5, 'a6, 'a7) asm_extra -> ('a1, 'a2, 'a3, 'a4,
    'a5, 'a6, 'a7, 'a8) architecture_params -> Var.var **)

let var_tmp asm_e aparams =
  { Var.vtype = (Coq_aword (arch_pd asm_e._asm._arch_decl)); Var.vname =
    aparams.ap_lip.lip_tmp }

(** val var_tmp2 :
    ('a1, 'a2, 'a3, 'a4, 'a5, 'a6, 'a7) asm_extra -> ('a1, 'a2, 'a3, 'a4,
    'a5, 'a6, 'a7, 'a8) architecture_params -> Var.var **)

let var_tmp2 asm_e aparams =
  { Var.vtype = (Coq_aword (arch_pd asm_e._asm._arch_decl)); Var.vname =
    aparams.ap_lip.lip_tmp2 }

(** val var_tmps :
    ('a1, 'a2, 'a3, 'a4, 'a5, 'a6, 'a7) asm_extra -> ('a1, 'a2, 'a3, 'a4,
    'a5, 'a6, 'a7, 'a8) architecture_params -> SvExtra.Sv.t **)

let var_tmps asm_e aparams =
  SvExtra.Sv.add (Obj.magic var_tmp2 asm_e aparams)
    (SvExtra.Sv.singleton (Obj.magic var_tmp asm_e aparams))

(** val live_range_splitting :
    ('a1, 'a2, 'a3, 'a4, 'a5, 'a6, 'a7) asm_extra -> ('a1, 'a2, 'a3, 'a4,
    'a5, 'a6, 'a7, 'a8) architecture_params -> (('a1, 'a2, 'a3, 'a4, 'a5,
    'a6, 'a7) extended_op, 'a8) compiler_params -> ('a1, 'a2, 'a3, 'a4, 'a5,
    'a6, 'a7) extended_op uprog -> ('a1, 'a2, 'a3, 'a4, 'a5, 'a6, 'a7)
    extended_op uprog cexec **)

let live_range_splitting asm_e aparams cparams p =
  let pv = split_live_ranges_prog asm_e cparams (Obj.magic p) in
  let pv0 = cparams.print_uprog Splitting pv in
  let pv1 = renaming_prog asm_e cparams pv0 in
  let pv2 = cparams.print_uprog Renaming pv1 in
  let pv3 = remove_phi_nodes_prog asm_e cparams pv2 in
  let pv4 = cparams.print_uprog RemovePhiNodes pv3 in
  let pv5 =
    map_prog_name (asm_opI asm_e) progUnit
      (Obj.magic cparams.refresh_instr_info) (Obj.magic pv4)
  in
  (match check_uprog withsubword (asm_opI asm_e)
           (Obj.magic cparams.dead_vars_ufd) p.p_extra p.p_funcs pv5.p_extra
           pv5.p_funcs with
   | Ok _ ->
     (match dead_code_prog (asm_opI asm_e) aparams.ap_is_move_op progUnit pv5
              false with
      | Ok x ->
        let p0 = cparams.print_uprog DeadCode_Renaming (Obj.magic x) in
        Ok (Obj.magic p0)
      | Error s -> Error s)
   | Error s -> Error s)

(** val inlining :
    ('a1, 'a2, 'a3, 'a4, 'a5, 'a6, 'a7) asm_extra -> (('a1, 'a2, 'a3, 'a4,
    'a5, 'a6, 'a7) extended_op, 'a8) compiler_params -> funname list -> ('a1,
    'a2, 'a3, 'a4, 'a5, 'a6, 'a7) extended_op uprog -> ('a1, 'a2, 'a3, 'a4,
    'a5, 'a6, 'a7) extended_op uprog cexec **)

let inlining asm_e cparams to_keep p =
  match inline_prog_err withsubword (asm_opI asm_e)
          (Obj.magic cparams.rename_fd) (Obj.magic cparams.dead_vars_ufd) p with
  | Ok x ->
    let p0 = cparams.print_uprog Inlining (Obj.magic x) in
    (match dead_calls_err_seq (asm_opI asm_e) progUnit to_keep (Obj.magic p0) with
     | Ok x0 ->
       let p1 = cparams.print_uprog RemoveUnusedFunction (Obj.magic x0) in
       Ok (Obj.magic p1)
     | Error s -> Error s)
  | Error s -> Error s

(** val compiler_first_part :
    ('a1, 'a2, 'a3, 'a4, 'a5, 'a6, 'a7) asm_extra -> ('a1, 'a2, 'a3, 'a4,
    'a5, 'a6, 'a7, 'a8) architecture_params -> (('a1, 'a2, 'a3, 'a4, 'a5,
    'a6, 'a7) extended_op, 'a8) compiler_params -> funname list -> ('a1, 'a2,
    'a3, 'a4, 'a5, 'a6, 'a7) extended_op uprog -> ('a1, 'a2, 'a3, 'a4, 'a5,
    'a6, 'a7) extended_op uprog cexec **)

let compiler_first_part asm_e aparams cparams to_keep p =
  match wi2w_prog (asm_opI asm_e) withsubword
          (Obj.magic cparams.remove_wint_annot)
          (Obj.magic cparams.dead_vars_ufd) p with
  | Ok x ->
    let p0 = cparams.print_uprog WintWord (Obj.magic x) in
    let p1 =
      insert_renaming_prog (asm_opI asm_e) progUnit cparams.insert_renaming
        (Obj.magic p0)
    in
    let p2 = cparams.print_uprog InsertRenaming (Obj.magic p1) in
    (match array_copy_prog (asm_opI asm_e) (fun k ->
             cparams.fresh_var_ident k dummy_instr_info (Uint63.of_int (0)))
             progUnit (Obj.magic p2) with
     | Ok x0 ->
       let p3 = cparams.print_uprog ArrayCopy (Obj.magic x0) in
       let p4 = add_init_prog (asm_opI asm_e) progUnit (Obj.magic p3) in
       let p5 = cparams.print_uprog AddArrInit (Obj.magic p4) in
       (match spill_prog (asm_opI asm_e) cparams.fresh_var_ident progUnit
                (Obj.magic p5) with
        | Ok x1 ->
          let p6 = cparams.print_uprog LowerSpill (Obj.magic x1) in
          (match inlining asm_e cparams to_keep (Obj.magic p6) with
           | Ok x2 ->
             (match unroll_loop (arch_msfsz asm_e._asm._arch_decl)
                      (asm_opI asm_e) asm_e._asm._arch_decl.ad_fcp
                      aparams.ap_is_move_op x2 with
              | Ok x3 ->
                (match check_no_for_loop (asm_opI asm_e) x3 with
                 | Ok _ ->
                   (match check_no_inline_instr (asm_opI asm_e) x3 with
                    | Ok _ ->
                      let p7 = cparams.print_uprog Unrolling (Obj.magic x3) in
                      (match dead_calls_err_seq (asm_opI asm_e) progUnit
                               to_keep (Obj.magic p7) with
                       | Ok x4 ->
                         let p8 =
                           cparams.print_uprog RemoveUnusedFunction
                             (Obj.magic x4)
                         in
                         (match live_range_splitting asm_e aparams cparams
                                  (Obj.magic p8) with
                          | Ok x5 ->
                            let pr =
                              remove_init_prog (asm_opI asm_e) is_reg_array
                                progUnit x5
                            in
                            let pr0 =
                              cparams.print_uprog RemoveArrInit (Obj.magic pr)
                            in
                            (match makereference_prog (asm_opI asm_e)
                                     (cparams.fresh_var_ident (Reg (Normal,
                                       (Pointer Writable)))) (Obj.magic pr0) with
                             | Ok x6 ->
                               let pa =
                                 cparams.print_uprog MakeRefArguments
                                   (Obj.magic x6)
                               in
                               (match expand_prog (asm_opI asm_e)
                                        (Obj.magic cparams.expand_fd) to_keep
                                        (Obj.magic pa) with
                                | Ok x7 ->
                                  let pe =
                                    cparams.print_uprog RegArrayExpansion
                                      (Obj.magic x7)
                                  in
                                  (match live_range_splitting asm_e aparams
                                           cparams (Obj.magic pe) with
                                   | Ok x8 ->
                                     (match remove_glob_prog (asm_opI asm_e)
                                              cparams.fresh_id x8 with
                                      | Ok x9 ->
                                        let pg =
                                          cparams.print_uprog RemoveGlobal
                                            (Obj.magic x9)
                                        in
                                        (match load_constants_prog
                                                 (asm_opI asm_e) progUnit
                                                 (cparams.fresh_var_ident
                                                   (Reg (Normal, Direct)))
                                                 aparams.ap_plp (Obj.magic pg) with
                                         | Ok x10 ->
                                           let pp =
                                             cparams.print_uprog
                                               LoadConstantsInCond
                                               (Obj.magic x10)
                                           in
                                           if aparams.ap_lop.lop_fvars_correct
                                                (cparams.fresh_var_ident (Reg
                                                  (Normal, Direct))
                                                  dummy_instr_info
                                                  (Uint63.of_int (0)))
                                                progUnit
                                                (Obj.magic pp).p_funcs
                                           then let p9 =
                                                  lower_prog (asm_opI asm_e)
                                                    aparams.ap_lop.lop_lower_i
                                                    cparams.lowering_opt
                                                    cparams.warning
                                                    (cparams.fresh_var_ident
                                                      (Reg (Normal, Direct))
                                                      dummy_instr_info
                                                      (Uint63.of_int (0)))
                                                    progUnit (Obj.magic pp)
                                                in
                                                let p10 =
                                                  cparams.print_uprog
                                                    LowerInstruction
                                                    (Obj.magic p9)
                                                in
                                                (match pi_prog
                                                         (asm_opI asm_e)
                                                         asm_e._asm._arch_decl.ad_fcp
                                                         progUnit
                                                         (Obj.magic p10) with
                                                 | Ok x11 ->
                                                   let p11 =
                                                     cparams.print_uprog
                                                       PropagateInline
                                                       (Obj.magic x11)
                                                   in
                                                   (match lower_slh_prog
                                                            (asm_opI asm_e)
                                                            (arch_msfsz
                                                              asm_e._asm._arch_decl)
                                                            asm_e._asm._arch_decl.ad_fcp
                                                            progUnit
                                                            aparams.ap_shp
                                                            (cparams.slh_info
                                                              p11) to_keep
                                                            (Obj.magic p11) with
                                                    | Ok x12 ->
                                                      let p12 =
                                                        cparams.print_uprog
                                                          SLHLowering
                                                          (Obj.magic x12)
                                                      in
                                                      Ok (Obj.magic p12)
                                                    | Error s -> Error s)
                                                 | Error s -> Error s)
                                           else let s =
                                                  pp_internal_error_s
                                                    "lowering"
                                                    "lowering check fails"
                                                in
                                                Error s
                                         | Error s -> Error s)
                                      | Error s -> Error s)
                                   | Error s -> Error s)
                                | Error s -> Error s)
                             | Error s -> Error s)
                          | Error s -> Error s)
                       | Error s -> Error s)
                    | Error s -> Error s)
                 | Error s -> Error s)
              | Error s -> Error s)
           | Error s -> Error s)
        | Error s -> Error s)
     | Error s -> Error s)
  | Error s -> Error s

(** val compiler_third_part :
    ('a1, 'a2, 'a3, 'a4, 'a5, 'a6, 'a7) asm_extra -> ('a1, 'a2, 'a3, 'a4,
    'a5, 'a6, 'a7, 'a8) architecture_params -> (('a1, 'a2, 'a3, 'a4, 'a5,
    'a6, 'a7) extended_op, 'a8) compiler_params -> (funname -> nat option
    list option) -> ('a1, 'a2, 'a3, 'a4, 'a5, 'a6, 'a7) extended_op sprog ->
    ('a1, 'a2, 'a3, 'a4, 'a5, 'a6, 'a7) extended_op sprog cexec **)

let compiler_third_part asm_e aparams cparams returned_params ps =
  let rminfo = cparams.removereturn (Obj.magic ps) in
  let rminfo0 = fun fn ->
    match returned_params fn with
    | Some l ->
      let l' =
        List0.map (fun i -> match i with
                            | Some _ -> false
                            | None -> true) l
      in
      if all (fun b -> b) l' then None else Some l'
    | None -> rminfo fn
  in
  (match dead_code_prog_tokeep (asm_opI asm_e) aparams.ap_is_move_op false
           rminfo0 (progStack (arch_pd asm_e._asm._arch_decl)) ps with
   | Ok x ->
     let pr = cparams.print_sprog RemoveReturn (Obj.magic x) in
     let pa = { p_funcs = (cparams.regalloc pr.p_funcs); p_globs =
       pr.p_globs; p_extra = pr.p_extra }
     in
     let pa0 = cparams.print_sprog RegAllocation pa in
     (match check_sprog withsubword (asm_opI asm_e)
              (arch_pd asm_e._asm._arch_decl)
              (Obj.magic cparams.dead_vars_sfd) (Obj.magic pr).p_extra
              (Obj.magic pr).p_funcs (Obj.magic pa0).p_extra
              (Obj.magic pa0).p_funcs with
      | Ok _ ->
        (match dead_code_prog (asm_opI asm_e) aparams.ap_is_move_op
                 (progStack (arch_pd asm_e._asm._arch_decl)) (Obj.magic pa0)
                 true with
         | Ok x0 ->
           let pd = cparams.print_sprog DeadCode_RegAllocation (Obj.magic x0)
           in

          (*Here the securitytransformationpath is pluged in*)
          (*let pd = Securitytransformationpath.security_transform_sprog (asm_opI asm_e) pd in*)
          let pd = cparams.security_transform_sprog pd in
          
           Ok (Obj.magic pd)
         | Error s -> Error s)
      | Error s -> Error s)
   | Error s -> Error s)

(** val wptr_status : var_i -> bool option **)

let wptr_status x =
  match Ident.Ident.id_kind (Var.vname x.v_var) with
  | Reg p ->
    let (_, r0) = p in
    (match r0 with
     | Direct -> None
     | Pointer writable ->
       Some (match writable with
             | Constant -> false
             | Writable -> true))
  | _ -> None

(** val allNone : 'a1 option list -> bool **)

let allNone m =
  all (fun a -> match a with
                | Some _ -> false
                | None -> true) m

(** val check_wf_ptr :
    ('a1, 'a2, 'a3, 'a4, 'a5, 'a6, 'a7) asm_extra -> funname list -> ('a1,
    'a2, 'a3, 'a4, 'a5, 'a6, 'a7) extended_op uprog -> (funname ->
    stk_alloc_oracle_t) -> unit cexec **)

let check_wf_ptr _ entries p ao =
  match allM (fun fn ->
          match get_fundef p.p_funcs fn with
          | Some fd ->
            if all2 (fun x pi ->
                 eq_op
                   (coq_Datatypes_option__canonical__eqtype_Equality
                     coq_Datatypes_bool__canonical__eqtype_Equality)
                   (Obj.magic wptr_status x)
                   (Obj.magic Ssrfun.Option.map (fun p0 -> p0.pp_writable) pi))
                 fd.f_params (ao fn).sao_params
            then Ok ()
            else Error
                   (pp_at_fi fd.f_info
                     (pp_at_fn fn
                       (Stack_alloc.E.stk_ierror_no_var
                         "inconsistent wptr_status")))
          | None -> Ok ()) entries with
  | Ok _ ->
    allM (fun fn ->
      match get_fundef p.p_funcs fn with
      | Some fd ->
        let n =
          find (fun x ->
            negb
              (eq_op
                (coq_Datatypes_option__canonical__eqtype_Equality
                  coq_Datatypes_bool__canonical__eqtype_Equality)
                (Obj.magic wptr_status x) (Obj.magic (Some true))))
            fd.f_params
        in
        if (&&)
             (eq_op
               (coq_Datatypes_list__canonical__eqtype_Equality
                 (coq_Datatypes_option__canonical__eqtype_Equality
                   coq_Datatypes_nat__canonical__eqtype_Equality))
               (Obj.magic take n (ao fn).sao_return)
               (Obj.magic map (fun x -> Some x) (iota O n)))
             (allNone (drop n (ao fn).sao_return))
        then Ok ()
        else Error
               (pp_at_fi fd.f_info
                 (pp_at_fn fn
                   (Stack_alloc.E.stk_error_no_var_gen false
                     (pp_nobox
                       ((pp_s
                          "the ordering of the arguments or the results is not correct.") :: (PPEbreak :: (
                       (pp_s
                         "The reg mut ptr must come first in the arguments") :: (
                       (pp_s
                         " and be returned first, in the same order, in the results.") :: []))))))))
      | None -> Ok ()) entries
  | Error s -> Error s

(** val compiler_front_end :
    ('a1, 'a2, 'a3, 'a4, 'a5, 'a6, 'a7) asm_extra -> ('a1, 'a2, 'a3, 'a4,
    'a5, 'a6, 'a7, 'a8) architecture_params -> (('a1, 'a2, 'a3, 'a4, 'a5,
    'a6, 'a7) extended_op, 'a8) compiler_params -> funname list -> ('a1, 'a2,
    'a3, 'a4, 'a5, 'a6, 'a7) extended_op uprog -> ('a1, 'a2, 'a3, 'a4, 'a5,
    'a6, 'a7) extended_op sprog cexec **)

let compiler_front_end asm_e aparams cparams entries p =
  match compiler_first_part asm_e aparams cparams entries p with
  | Ok x ->
    let ao = cparams.stackalloc (Obj.magic x) in
    (match check_wf_ptr asm_e entries p ao.ao_stack_alloc with
     | Ok _ ->
       (match alloc_prog true (arch_pd asm_e._asm._arch_decl)
                (arch_msfsz asm_e._asm._arch_decl) (asm_opI asm_e)
                aparams.ap_shp aparams.ap_sap aparams.ap_is_move_op
                (fun vk -> cparams.fresh_var_ident vk dummy_instr_info)
                cparams.pp_sr cparams.global_static_data_symbol
                cparams.stack_register_symbol ao.ao_globals
                ao.ao_global_alloc ao.ao_stack_alloc (Obj.magic x) with
        | Ok x0 ->
          let ps = cparams.print_sprog StackAllocation x0 in
          (match aparams.ap_lap
                   (cparams.fresh_var_ident (Reg (Normal, Direct))
                     dummy_instr_info (Uint63.of_int (0))) ps with
           | Ok x1 ->
             let ps0 = cparams.print_sprog LowerAddressing x1 in
             let returned_params = fun fn ->
               if in_mem fn
                    (mem (seq_predType funname_eqType) (Obj.magic entries))
               then Some (ao.ao_stack_alloc (Obj.magic fn)).sao_return
               else None
             in
             (match compiler_third_part asm_e aparams cparams
                      (Obj.magic returned_params) (Obj.magic ps0) with
              | Ok x2 -> Ok x2
              | Error s -> Error s)
           | Error s -> Error s)
        | Error s -> Error s)
     | Error s -> Error s)
  | Error s -> Error s

(** val check_export :
    ('a1, 'a2, 'a3, 'a4, 'a5, 'a6, 'a7) asm_extra -> funname list -> ('a1,
    'a2, 'a3, 'a4, 'a5, 'a6, 'a7) extended_op sprog -> unit cexec **)

let check_export _ entries p =
  allM (fun fn ->
    match get_fundef p.p_funcs fn with
    | Some fd ->
      if is_RAnone (Obj.magic fd).f_extra.sf_return_address
      then Ok ()
      else Error
             (pp_at_fn fn
               (Merge_varmaps.E.gen_error true None
                 (pp_s "export function expects a return address")))
    | None ->
      Error
        (pp_at_fn fn
          (Merge_varmaps.E.gen_error true None
            (pp_s "unknown export function")))) entries

(** val compiler_back_end :
    ('a1, 'a2, 'a3, 'a4, 'a5, 'a6, 'a7) asm_extra -> ('a1, 'a2, 'a3, 'a4,
    'a5) calling_convention -> ('a1, 'a2, 'a3, 'a4, 'a5, 'a6, 'a7, 'a8)
    architecture_params -> (('a1, 'a2, 'a3, 'a4, 'a5, 'a6, 'a7) extended_op,
    'a8) compiler_params -> funname list -> ('a1, 'a2, 'a3, 'a4, 'a5, 'a6,
    'a7) extended_op sprog -> (pp_error_loc, ('a1, 'a2, 'a3, 'a4, 'a5, 'a6,
    'a7) extended_op lprog) result **)

let compiler_back_end asm_e call_conv aparams cparams entries pd =
  match check_export asm_e entries pd with
  | Ok _ ->
    (match check (arch_pd asm_e._asm._arch_decl) (asm_opI asm_e)
             (ovm_i asm_e._asm._arch_decl asm_e._atoI call_conv) pd
             (var_tmps asm_e aparams) with
     | Ok _ ->
       (match linear_prog (arch_pd asm_e._asm._arch_decl) (asm_opI asm_e)
                aparams.ap_lip pd with
        | Ok x ->
          let pl = cparams.print_linear Linearization x in
          
          (*let pl = Pass2.schedule_lprog pl in*)
          let szs_of_fn = fun fn ->
            match cparams.stack_zero_info fn with
            | Some p ->
              let (szs, ows) = p in
              let ws =
                match ows with
                | Some ws -> ws
                | None ->
                  (match get_fundef pl.lp_funcs fn with
                   | Some lfd -> lfd.lfd_align
                   | None -> U8)
              in
              Some (szs, ws)
            | None -> None
          in
          (match stack_zeroization_lprog (arch_pd asm_e._asm._arch_decl)
                   (asm_opI asm_e)
                   (ovm_i asm_e._asm._arch_decl asm_e._atoI call_conv)
                   aparams.ap_szp szs_of_fn pl with
           | Ok x0 ->
             let pl0 = cparams.print_linear StackZeroization x0 in
             (match tunnel_program (asm_opI asm_e) pl0 with
              | Ok x1 -> let pl1 = cparams.print_linear Tunneling x1 in Ok pl1
              | Error s -> Error s)
           | Error s -> Error s)
        | Error s -> Error s)
     | Error s -> Error s)
  | Error s -> Error s

(** val compiler_back_end_to_asm :
    ('a1, 'a2, 'a3, 'a4, 'a5, 'a6, 'a7) asm_extra -> ('a1, 'a2, 'a3, 'a4,
    'a5) calling_convention -> ('a1, 'a2, 'a3, 'a4, 'a5, 'a6, 'a7, 'a8)
    architecture_params -> (('a1, 'a2, 'a3, 'a4, 'a5, 'a6, 'a7) extended_op,
    'a8) compiler_params -> funname list -> ('a1, 'a2, 'a3, 'a4, 'a5, 'a6,
    'a7) extended_op sprog -> (pp_error_loc, ('a1, 'a2, 'a3, 'a4, 'a5, 'a6)
    asm_prog) result **)

let compiler_back_end_to_asm asm_e call_conv aparams cparams entries p =
  match compiler_back_end asm_e call_conv aparams cparams entries p with
  | Ok x -> assemble_prog asm_e call_conv aparams.ap_agp x
  | Error s -> Error s

(** val compile_prog_to_asm :
    ('a1, 'a2, 'a3, 'a4, 'a5, 'a6, 'a7) asm_extra -> ('a1, 'a2, 'a3, 'a4,
    'a5) calling_convention -> ('a1, 'a2, 'a3, 'a4, 'a5, 'a6, 'a7, 'a8)
    architecture_params -> (('a1, 'a2, 'a3, 'a4, 'a5, 'a6, 'a7) extended_op,
    'a8) compiler_params -> funname list -> ('a1, 'a2, 'a3, 'a4, 'a5, 'a6,
    'a7) extended_op uprog -> ('a1, 'a2, 'a3, 'a4, 'a5, 'a6) asm_prog cexec **)

let compile_prog_to_asm asm_e call_conv aparams cparams entries p =
  match compiler_front_end asm_e aparams cparams entries p with
  | Ok x -> compiler_back_end_to_asm asm_e call_conv aparams cparams entries x
  | Error s -> Error s

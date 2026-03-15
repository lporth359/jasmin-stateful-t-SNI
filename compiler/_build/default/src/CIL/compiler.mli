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

val pp_s : string -> pp_error

val unroll :
  coq_MSFsize -> 'a1 asmOp -> coq_FlagCombinationParams -> ('a1 asm_op_t ->
  bool) -> nat -> 'a1 uprog -> 'a1 uprog cexec

val unroll_loop :
  coq_MSFsize -> 'a1 asmOp -> coq_FlagCombinationParams -> ('a1 asm_op_t ->
  bool) -> 'a1 uprog -> (pp_error_loc, 'a1 uprog) result

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

val compiler_step_rect :
  'a1 -> 'a1 -> 'a1 -> 'a1 -> 'a1 -> 'a1 -> 'a1 -> 'a1 -> 'a1 -> 'a1 -> 'a1
  -> 'a1 -> 'a1 -> 'a1 -> 'a1 -> 'a1 -> 'a1 -> 'a1 -> 'a1 -> 'a1 -> 'a1 ->
  'a1 -> 'a1 -> 'a1 -> 'a1 -> 'a1 -> 'a1 -> 'a1 -> 'a1 -> 'a1 -> 'a1 ->
  compiler_step -> 'a1

val compiler_step_rec :
  'a1 -> 'a1 -> 'a1 -> 'a1 -> 'a1 -> 'a1 -> 'a1 -> 'a1 -> 'a1 -> 'a1 -> 'a1
  -> 'a1 -> 'a1 -> 'a1 -> 'a1 -> 'a1 -> 'a1 -> 'a1 -> 'a1 -> 'a1 -> 'a1 ->
  'a1 -> 'a1 -> 'a1 -> 'a1 -> 'a1 -> 'a1 -> 'a1 -> 'a1 -> 'a1 -> 'a1 ->
  compiler_step -> 'a1

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

val is_compiler_step_rect :
  'a1 -> 'a1 -> 'a1 -> 'a1 -> 'a1 -> 'a1 -> 'a1 -> 'a1 -> 'a1 -> 'a1 -> 'a1
  -> 'a1 -> 'a1 -> 'a1 -> 'a1 -> 'a1 -> 'a1 -> 'a1 -> 'a1 -> 'a1 -> 'a1 ->
  'a1 -> 'a1 -> 'a1 -> 'a1 -> 'a1 -> 'a1 -> 'a1 -> 'a1 -> 'a1 -> 'a1 ->
  compiler_step -> is_compiler_step -> 'a1

val is_compiler_step_rec :
  'a1 -> 'a1 -> 'a1 -> 'a1 -> 'a1 -> 'a1 -> 'a1 -> 'a1 -> 'a1 -> 'a1 -> 'a1
  -> 'a1 -> 'a1 -> 'a1 -> 'a1 -> 'a1 -> 'a1 -> 'a1 -> 'a1 -> 'a1 -> 'a1 ->
  'a1 -> 'a1 -> 'a1 -> 'a1 -> 'a1 -> 'a1 -> 'a1 -> 'a1 -> 'a1 -> 'a1 ->
  compiler_step -> is_compiler_step -> 'a1

val compiler_step_tag : compiler_step -> positive

val is_compiler_step_inhab : compiler_step -> is_compiler_step

val is_compiler_step_functor :
  compiler_step -> is_compiler_step -> is_compiler_step

type box_compiler_step_Typing =
| Box_compiler_step_Typing

type compiler_step_fields_t = __

val compiler_step_fields : compiler_step -> compiler_step_fields_t

val compiler_step_construct :
  positive -> compiler_step_fields_t -> compiler_step option

val compiler_step_induction :
  'a1 -> 'a1 -> 'a1 -> 'a1 -> 'a1 -> 'a1 -> 'a1 -> 'a1 -> 'a1 -> 'a1 -> 'a1
  -> 'a1 -> 'a1 -> 'a1 -> 'a1 -> 'a1 -> 'a1 -> 'a1 -> 'a1 -> 'a1 -> 'a1 ->
  'a1 -> 'a1 -> 'a1 -> 'a1 -> 'a1 -> 'a1 -> 'a1 -> 'a1 -> 'a1 -> 'a1 ->
  compiler_step -> is_compiler_step -> 'a1

val compiler_step_eqb_fields :
  (compiler_step -> compiler_step -> bool) -> positive ->
  compiler_step_fields_t -> compiler_step_fields_t -> bool

val compiler_step_eqb : compiler_step -> compiler_step -> bool

val compiler_step_eqb_OK : compiler_step -> compiler_step -> reflect

val compiler_step_eqb_OK_sumbool : compiler_step -> compiler_step -> bool

val compiler_step_list : compiler_step list

val coq_HB_unnamed_factory_1 : compiler_step Coq_hasDecEq.axioms_

val compiler_compiler_step__canonical__eqtype_Equality : Equality.coq_type

type stack_alloc_oracles = { ao_globals : GRing.ComRing.sort list;
                             ao_global_alloc : ((Var.var * wsize) * coq_Z)
                                               list;
                             ao_stack_alloc : (funname -> stk_alloc_oracle_t) }

val ao_globals : stack_alloc_oracles -> GRing.ComRing.sort list

val ao_global_alloc : stack_alloc_oracles -> ((Var.var * wsize) * coq_Z) list

val ao_stack_alloc : stack_alloc_oracles -> funname -> stk_alloc_oracle_t

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
                                                      security_transform_sprog : ('asm_op _sprog ->
                                                             'asm_op _sprog) 
                                                                 }


val security_transform_sprog : 'a1 asmOp -> ('a1, 'a2) compiler_params ->  'a1 _sprog ->
  'a1 _sprog

val rename_fd :
  'a1 asmOp -> ('a1, 'a2) compiler_params -> instr_info -> funname -> 'a1
  _ufundef -> 'a1 _ufundef

val expand_fd :
  'a1 asmOp -> ('a1, 'a2) compiler_params -> funname -> 'a1 _ufundef ->
  expand_info

val split_live_ranges_fd :
  'a1 asmOp -> ('a1, 'a2) compiler_params -> funname -> 'a1 _ufundef -> 'a1
  _ufundef

val renaming_fd :
  'a1 asmOp -> ('a1, 'a2) compiler_params -> funname -> 'a1 _ufundef -> 'a1
  _ufundef

val remove_phi_nodes_fd :
  'a1 asmOp -> ('a1, 'a2) compiler_params -> funname -> 'a1 _ufundef -> 'a1
  _ufundef

val stack_register_symbol :
  'a1 asmOp -> ('a1, 'a2) compiler_params -> Ident.Ident.ident

val global_static_data_symbol :
  'a1 asmOp -> ('a1, 'a2) compiler_params -> Ident.Ident.ident

val stackalloc :
  'a1 asmOp -> ('a1, 'a2) compiler_params -> 'a1 _uprog -> stack_alloc_oracles

val removereturn :
  'a1 asmOp -> ('a1, 'a2) compiler_params -> 'a1 _sprog -> funname -> bool
  list option

val regalloc :
  'a1 asmOp -> ('a1, 'a2) compiler_params -> 'a1 _sfun_decl list -> 'a1
  _sfun_decl list

val remove_wint_annot :
  'a1 asmOp -> ('a1, 'a2) compiler_params -> funname -> 'a1 _ufundef -> 'a1
  _ufundef

val print_uprog :
  'a1 asmOp -> ('a1, 'a2) compiler_params -> compiler_step -> 'a1 _uprog ->
  'a1 _uprog

val print_sprog :
  'a1 asmOp -> ('a1, 'a2) compiler_params -> compiler_step -> 'a1 _sprog ->
  'a1 _sprog

val print_linear :
  'a1 asmOp -> ('a1, 'a2) compiler_params -> compiler_step -> 'a1 lprog ->
  'a1 lprog

val refresh_instr_info :
  'a1 asmOp -> ('a1, 'a2) compiler_params -> funname -> 'a1 _ufundef -> 'a1
  _ufundef

val warning :
  'a1 asmOp -> ('a1, 'a2) compiler_params -> instr_info -> warning_msg ->
  instr_info

val lowering_opt : 'a1 asmOp -> ('a1, 'a2) compiler_params -> 'a2

val insert_renaming :
  'a1 asmOp -> ('a1, 'a2) compiler_params -> fun_info -> bool

val fresh_id :
  'a1 asmOp -> ('a1, 'a2) compiler_params -> glob_decl list -> Var.var ->
  Ident.Ident.ident

val fresh_var_ident :
  'a1 asmOp -> ('a1, 'a2) compiler_params -> v_kind -> instr_info -> Uint63.t
  -> string -> atype -> Ident.Ident.ident

val slh_info :
  'a1 asmOp -> ('a1, 'a2) compiler_params -> 'a1 _uprog -> funname -> slh_t
  list * slh_t list

val stack_zero_info :
  'a1 asmOp -> ('a1, 'a2) compiler_params -> funname ->
  (stack_zero_strategy * wsize option) option

val dead_vars_ufd :
  'a1 asmOp -> ('a1, 'a2) compiler_params -> 'a1 _ufun_decl -> instr_info ->
  SvExtra.Sv.t

val dead_vars_sfd :
  'a1 asmOp -> ('a1, 'a2) compiler_params -> 'a1 _sfun_decl -> instr_info ->
  SvExtra.Sv.t

val pp_sr : 'a1 asmOp -> ('a1, 'a2) compiler_params -> sub_region -> pp_error

val split_live_ranges_prog :
  ('a1, 'a2, 'a3, 'a4, 'a5, 'a6, 'a7) asm_extra -> (('a1, 'a2, 'a3, 'a4, 'a5,
  'a6, 'a7) extended_op, 'a8) compiler_params -> ('a1, 'a2, 'a3, 'a4, 'a5,
  'a6, 'a7) extended_op _uprog -> ('a1, 'a2, 'a3, 'a4, 'a5, 'a6, 'a7)
  extended_op _uprog

val renaming_prog :
  ('a1, 'a2, 'a3, 'a4, 'a5, 'a6, 'a7) asm_extra -> (('a1, 'a2, 'a3, 'a4, 'a5,
  'a6, 'a7) extended_op, 'a8) compiler_params -> ('a1, 'a2, 'a3, 'a4, 'a5,
  'a6, 'a7) extended_op _uprog -> ('a1, 'a2, 'a3, 'a4, 'a5, 'a6, 'a7)
  extended_op _uprog

val remove_phi_nodes_prog :
  ('a1, 'a2, 'a3, 'a4, 'a5, 'a6, 'a7) asm_extra -> (('a1, 'a2, 'a3, 'a4, 'a5,
  'a6, 'a7) extended_op, 'a8) compiler_params -> ('a1, 'a2, 'a3, 'a4, 'a5,
  'a6, 'a7) extended_op _uprog -> ('a1, 'a2, 'a3, 'a4, 'a5, 'a6, 'a7)
  extended_op _uprog

val var_tmp :
  ('a1, 'a2, 'a3, 'a4, 'a5, 'a6, 'a7) asm_extra -> ('a1, 'a2, 'a3, 'a4, 'a5,
  'a6, 'a7, 'a8) architecture_params -> Var.var

val var_tmp2 :
  ('a1, 'a2, 'a3, 'a4, 'a5, 'a6, 'a7) asm_extra -> ('a1, 'a2, 'a3, 'a4, 'a5,
  'a6, 'a7, 'a8) architecture_params -> Var.var

val var_tmps :
  ('a1, 'a2, 'a3, 'a4, 'a5, 'a6, 'a7) asm_extra -> ('a1, 'a2, 'a3, 'a4, 'a5,
  'a6, 'a7, 'a8) architecture_params -> SvExtra.Sv.t

val live_range_splitting :
  ('a1, 'a2, 'a3, 'a4, 'a5, 'a6, 'a7) asm_extra -> ('a1, 'a2, 'a3, 'a4, 'a5,
  'a6, 'a7, 'a8) architecture_params -> (('a1, 'a2, 'a3, 'a4, 'a5, 'a6, 'a7)
  extended_op, 'a8) compiler_params -> ('a1, 'a2, 'a3, 'a4, 'a5, 'a6, 'a7)
  extended_op uprog -> ('a1, 'a2, 'a3, 'a4, 'a5, 'a6, 'a7) extended_op uprog
  cexec

val inlining :
  ('a1, 'a2, 'a3, 'a4, 'a5, 'a6, 'a7) asm_extra -> (('a1, 'a2, 'a3, 'a4, 'a5,
  'a6, 'a7) extended_op, 'a8) compiler_params -> funname list -> ('a1, 'a2,
  'a3, 'a4, 'a5, 'a6, 'a7) extended_op uprog -> ('a1, 'a2, 'a3, 'a4, 'a5,
  'a6, 'a7) extended_op uprog cexec

val compiler_first_part :
  ('a1, 'a2, 'a3, 'a4, 'a5, 'a6, 'a7) asm_extra -> ('a1, 'a2, 'a3, 'a4, 'a5,
  'a6, 'a7, 'a8) architecture_params -> (('a1, 'a2, 'a3, 'a4, 'a5, 'a6, 'a7)
  extended_op, 'a8) compiler_params -> funname list -> ('a1, 'a2, 'a3, 'a4,
  'a5, 'a6, 'a7) extended_op uprog -> ('a1, 'a2, 'a3, 'a4, 'a5, 'a6, 'a7)
  extended_op uprog cexec

val compiler_third_part :
  ('a1, 'a2, 'a3, 'a4, 'a5, 'a6, 'a7) asm_extra -> ('a1, 'a2, 'a3, 'a4, 'a5,
  'a6, 'a7, 'a8) architecture_params -> (('a1, 'a2, 'a3, 'a4, 'a5, 'a6, 'a7)
  extended_op, 'a8) compiler_params -> (funname -> nat option list option) ->
  ('a1, 'a2, 'a3, 'a4, 'a5, 'a6, 'a7) extended_op sprog -> ('a1, 'a2, 'a3,
  'a4, 'a5, 'a6, 'a7) extended_op sprog cexec

val wptr_status : var_i -> bool option

val allNone : 'a1 option list -> bool

val check_wf_ptr :
  ('a1, 'a2, 'a3, 'a4, 'a5, 'a6, 'a7) asm_extra -> funname list -> ('a1, 'a2,
  'a3, 'a4, 'a5, 'a6, 'a7) extended_op uprog -> (funname ->
  stk_alloc_oracle_t) -> unit cexec

val compiler_front_end :
  ('a1, 'a2, 'a3, 'a4, 'a5, 'a6, 'a7) asm_extra -> ('a1, 'a2, 'a3, 'a4, 'a5,
  'a6, 'a7, 'a8) architecture_params -> (('a1, 'a2, 'a3, 'a4, 'a5, 'a6, 'a7)
  extended_op, 'a8) compiler_params -> funname list -> ('a1, 'a2, 'a3, 'a4,
  'a5, 'a6, 'a7) extended_op uprog -> ('a1, 'a2, 'a3, 'a4, 'a5, 'a6, 'a7)
  extended_op sprog cexec

val check_export :
  ('a1, 'a2, 'a3, 'a4, 'a5, 'a6, 'a7) asm_extra -> funname list -> ('a1, 'a2,
  'a3, 'a4, 'a5, 'a6, 'a7) extended_op sprog -> unit cexec

val compiler_back_end :
  ('a1, 'a2, 'a3, 'a4, 'a5, 'a6, 'a7) asm_extra -> ('a1, 'a2, 'a3, 'a4, 'a5)
  calling_convention -> ('a1, 'a2, 'a3, 'a4, 'a5, 'a6, 'a7, 'a8)
  architecture_params -> (('a1, 'a2, 'a3, 'a4, 'a5, 'a6, 'a7) extended_op,
  'a8) compiler_params -> funname list -> ('a1, 'a2, 'a3, 'a4, 'a5, 'a6, 'a7)
  extended_op sprog -> (pp_error_loc, ('a1, 'a2, 'a3, 'a4, 'a5, 'a6, 'a7)
  extended_op lprog) result

val compiler_back_end_to_asm :
  ('a1, 'a2, 'a3, 'a4, 'a5, 'a6, 'a7) asm_extra -> ('a1, 'a2, 'a3, 'a4, 'a5)
  calling_convention -> ('a1, 'a2, 'a3, 'a4, 'a5, 'a6, 'a7, 'a8)
  architecture_params -> (('a1, 'a2, 'a3, 'a4, 'a5, 'a6, 'a7) extended_op,
  'a8) compiler_params -> funname list -> ('a1, 'a2, 'a3, 'a4, 'a5, 'a6, 'a7)
  extended_op sprog -> (pp_error_loc, ('a1, 'a2, 'a3, 'a4, 'a5, 'a6)
  asm_prog) result

val compile_prog_to_asm :
  ('a1, 'a2, 'a3, 'a4, 'a5, 'a6, 'a7) asm_extra -> ('a1, 'a2, 'a3, 'a4, 'a5)
  calling_convention -> ('a1, 'a2, 'a3, 'a4, 'a5, 'a6, 'a7, 'a8)
  architecture_params -> (('a1, 'a2, 'a3, 'a4, 'a5, 'a6, 'a7) extended_op,
  'a8) compiler_params -> funname list -> ('a1, 'a2, 'a3, 'a4, 'a5, 'a6, 'a7)
  extended_op uprog -> ('a1, 'a2, 'a3, 'a4, 'a5, 'a6) asm_prog cexec

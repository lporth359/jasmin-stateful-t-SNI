open Arch_extra
open Asm_gen
open Compiler_util
open Expr
open Linearization
open Lowering
open Slh_lowering
open Sopn
open Stack_alloc_params
open Stack_zeroization
open Type

type ('asm_op, 'lowering_options) lowering_params = { lop_lower_i : ('lowering_options
                                                                    ->
                                                                    (instr_info
                                                                    ->
                                                                    warning_msg
                                                                    ->
                                                                    instr_info)
                                                                    ->
                                                                    fresh_vars
                                                                    ->
                                                                    'asm_op
                                                                    instr ->
                                                                    'asm_op
                                                                    instr
                                                                    list);
                                                      lop_fvars_correct : 
                                                      (fresh_vars -> progT ->
                                                      'asm_op fun_decl list
                                                      -> bool) }

type ('reg, 'regx, 'xreg, 'rflag, 'cond, 'asm_op, 'extra_op) lower_addressing_params =
  (string -> atype -> Ident.Ident.ident) -> ('reg, 'regx, 'xreg, 'rflag,
  'cond, 'asm_op, 'extra_op) extended_op _sprog -> ('reg, 'regx, 'xreg,
  'rflag, 'cond, 'asm_op, 'extra_op) extended_op _sprog cexec
  (* singleton inductive, whose constructor was Build_lower_addressing_params *)

type ('reg, 'regx, 'xreg, 'rflag, 'cond, 'asm_op, 'extra_op, 'lowering_options) architecture_params = { 
ap_sap : ('reg, 'regx, 'xreg, 'rflag, 'cond, 'asm_op, 'extra_op) extended_op
         stack_alloc_params;
ap_lip : ('reg, 'regx, 'xreg, 'rflag, 'cond, 'asm_op, 'extra_op) extended_op
         linearization_params; ap_plp : bool;
ap_lop : (('reg, 'regx, 'xreg, 'rflag, 'cond, 'asm_op, 'extra_op)
         extended_op, 'lowering_options) lowering_params;
ap_shp : ('reg, 'regx, 'xreg, 'rflag, 'cond, 'asm_op, 'extra_op) extended_op
         sh_params;
ap_lap : ('reg, 'regx, 'xreg, 'rflag, 'cond, 'asm_op, 'extra_op)
         lower_addressing_params;
ap_agp : ('reg, 'regx, 'xreg, 'rflag, 'cond, 'asm_op, 'extra_op)
         asm_gen_params;
ap_szp : ('reg, 'regx, 'xreg, 'rflag, 'cond, 'asm_op, 'extra_op) extended_op
         stack_zeroization_params;
ap_is_move_op : (('reg, 'regx, 'xreg, 'rflag, 'cond, 'asm_op, 'extra_op)
                extended_op asm_op_t -> bool) }

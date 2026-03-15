open BinPos
open Bool
open Datatypes
open Arch_decl
open Compiler_util
open Eqtype
open Expr
open Fexpr
open Sem_type
open Seq
open Sopn
open Type
open Utils0
open Var0
open Wsize
open Xseq

type 't coq_ToIdent = { to_ident : ('t -> Ident.Ident.ident);
                        of_ident : (Ident.Ident.ident -> 't option) }

val to_ident :
  ltype -> 'a1 coq_ToString -> 'a1 coq_ToIdent -> 'a1 -> Ident.Ident.ident

val of_ident :
  ltype -> 'a1 coq_ToString -> 'a1 coq_ToIdent -> Ident.Ident.ident -> 'a1
  option

val to_var : ltype -> 'a1 coq_ToString -> 'a1 coq_ToIdent -> 'a1 -> Var.var

val of_var :
  ltype -> 'a1 coq_ToString -> 'a1 coq_ToIdent -> Var.var -> 'a1 option

module type MkToIdent_T =
 sig
  val mk :
    ltype -> 'a1 coq_ToString -> (string -> Ident.Ident.ident) ->
    (pp_error_loc, 'a1 coq_ToIdent) result
 end

module MkToIdent :
 MkToIdent_T

type ('reg, 'regx, 'xreg, 'rflag, 'cond) arch_toIdent = { toI_r : 'reg
                                                                  coq_ToIdent;
                                                          toI_rx : 'regx
                                                                   coq_ToIdent;
                                                          toI_x : 'xreg
                                                                  coq_ToIdent;
                                                          toI_f : 'rflag
                                                                  coq_ToIdent }

val toI_r :
  ('a1, 'a2, 'a3, 'a4, 'a5) arch_decl -> ('a1, 'a2, 'a3, 'a4, 'a5)
  arch_toIdent -> 'a1 coq_ToIdent

val toI_rx :
  ('a1, 'a2, 'a3, 'a4, 'a5) arch_decl -> ('a1, 'a2, 'a3, 'a4, 'a5)
  arch_toIdent -> 'a2 coq_ToIdent

val toI_x :
  ('a1, 'a2, 'a3, 'a4, 'a5) arch_decl -> ('a1, 'a2, 'a3, 'a4, 'a5)
  arch_toIdent -> 'a3 coq_ToIdent

val toI_f :
  ('a1, 'a2, 'a3, 'a4, 'a5) arch_decl -> ('a1, 'a2, 'a3, 'a4, 'a5)
  arch_toIdent -> 'a4 coq_ToIdent

module type AToIdent_T =
 sig
  val mk :
    ('a1, 'a2, 'a3, 'a4, 'a5) arch_decl -> (reg_kind -> ltype -> string ->
    Ident.Ident.ident) -> (pp_error_loc, ('a1, 'a2, 'a3, 'a4, 'a5)
    arch_toIdent) result
 end

module MkAToIdent :
 AToIdent_T

val var_of_implicit_arg :
  ('a1, 'a2, 'a3, 'a4, 'a5) arch_decl -> ('a1, 'a2, 'a3, 'a4, 'a5)
  arch_toIdent -> ('a1, 'a2, 'a3, 'a4, 'a5) implicit_arg -> Var.var

val sopn_constrained_register :
  ('a1, 'a2, 'a3, 'a4, 'a5) arch_decl -> ('a1, 'a2, 'a3, 'a4, 'a5)
  arch_toIdent -> ('a1, 'a2, 'a3, 'a4, 'a5)
  Arch_decl.arg_constrained_register -> arg_constrained_register

val sopn_arg_desc :
  ('a1, 'a2, 'a3, 'a4, 'a5) arch_decl -> ('a1, 'a2, 'a3, 'a4, 'a5)
  arch_toIdent -> ('a1, 'a2, 'a3, 'a4, 'a5) Arch_decl.arg_desc -> arg_desc

type ('reg, 'regx, 'xreg, 'rflag, 'cond, 'asm_op, 'extra_op) asm_extra = { 
_asm : ('reg, 'regx, 'xreg, 'rflag, 'cond, 'asm_op) asm;
_atoI : ('reg, 'regx, 'xreg, 'rflag, 'cond) arch_toIdent;
_extra : 'extra_op asmOp;
to_asm : (instr_info -> 'extra_op -> lexpr list -> rexpr list -> ((('reg,
         'regx, 'xreg, 'rflag, 'cond, 'asm_op) asm_op_msb_t * lexpr
         list) * rexpr list) list cexec) }

val _asm :
  ('a1, 'a2, 'a3, 'a4, 'a5, 'a6, 'a7) asm_extra -> ('a1, 'a2, 'a3, 'a4, 'a5,
  'a6) asm

val _atoI :
  ('a1, 'a2, 'a3, 'a4, 'a5, 'a6, 'a7) asm_extra -> ('a1, 'a2, 'a3, 'a4, 'a5)
  arch_toIdent

val _extra : ('a1, 'a2, 'a3, 'a4, 'a5, 'a6, 'a7) asm_extra -> 'a7 asmOp

val to_asm :
  ('a1, 'a2, 'a3, 'a4, 'a5, 'a6, 'a7) asm_extra -> instr_info -> 'a7 -> lexpr
  list -> rexpr list -> ((('a1, 'a2, 'a3, 'a4, 'a5, 'a6) asm_op_msb_t * lexpr
  list) * rexpr list) list cexec

type ('reg, 'regx, 'xreg, 'rflag, 'cond, 'asm_op, 'extra_op) extra_op_t =
  'extra_op

type ('reg, 'regx, 'xreg, 'rflag, 'cond, 'asm_op, 'extra_op) extended_op =
| BaseOp of ('reg, 'regx, 'xreg, 'rflag, 'cond, 'asm_op) asm_op_msb_t
| ExtOp of ('reg, 'regx, 'xreg, 'rflag, 'cond, 'asm_op, 'extra_op) extra_op_t

val extended_op_beq :
  ('a1, 'a2, 'a3, 'a4, 'a5, 'a6, 'a7) asm_extra -> ('a1, 'a2, 'a3, 'a4, 'a5,
  'a6, 'a7) extended_op -> ('a1, 'a2, 'a3, 'a4, 'a5, 'a6, 'a7) extended_op ->
  bool

val extended_op_eq_axiom :
  ('a1, 'a2, 'a3, 'a4, 'a5, 'a6, 'a7) asm_extra -> ('a1, 'a2, 'a3, 'a4, 'a5,
  'a6, 'a7) extended_op eq_axiom

val coq_HB_unnamed_factory_1 :
  ('a1, 'a2, 'a3, 'a4, 'a5, 'a6, 'a7) asm_extra -> ('a1, 'a2, 'a3, 'a4, 'a5,
  'a6, 'a7) extended_op Coq_hasDecEq.axioms_

val arch_extra_extended_op__canonical__eqtype_Equality :
  ('a1, 'a2, 'a3, 'a4, 'a5, 'a6, 'a7) asm_extra -> Equality.coq_type

val wsize_eq_dec : wsize -> wsize -> bool

val ctype_eq_dec : ctype -> ctype -> bool

val semi_to_atype :
  ltype list -> ltype list -> sem_tuple exec sem_prod -> sem_tuple exec
  sem_prod

val get_instr_desc :
  ('a1, 'a2, 'a3, 'a4, 'a5, 'a6, 'a7) asm_extra -> ('a1, 'a2, 'a3, 'a4, 'a5,
  'a6, 'a7) extended_op -> instruction_desc

val sopn_prim_string_base :
  ('a1, 'a2, 'a3, 'a4, 'a5, 'a6, 'a7) asm_extra -> (string * 'a6
  prim_constructor) list -> (string * ('a1, 'a2, 'a3, 'a4, 'a5, 'a6, 'a7)
  extended_op prim_constructor) list

val sopn_prim_string_extra :
  ('a1, 'a2, 'a3, 'a4, 'a5, 'a6, 'a7) asm_extra -> (string * 'a7
  prim_constructor) list -> (string * ('a1, 'a2, 'a3, 'a4, 'a5, 'a6, 'a7)
  extended_op prim_constructor) list

val get_prime_op :
  ('a1, 'a2, 'a3, 'a4, 'a5, 'a6, 'a7) asm_extra -> (string * ('a1, 'a2, 'a3,
  'a4, 'a5, 'a6, 'a7) extended_op prim_constructor) list

val eqTC_extended_op :
  ('a1, 'a2, 'a3, 'a4, 'a5, 'a6, 'a7) asm_extra -> ('a1, 'a2, 'a3, 'a4, 'a5,
  'a6, 'a7) extended_op eqTypeC

val asm_opI :
  ('a1, 'a2, 'a3, 'a4, 'a5, 'a6, 'a7) asm_extra -> ('a1, 'a2, 'a3, 'a4, 'a5,
  'a6, 'a7) extended_op asmOp

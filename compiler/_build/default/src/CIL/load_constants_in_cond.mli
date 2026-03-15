open BinInt
open BinNums
open Datatypes
open Compiler_util
open Expr
open Seq
open Sopn
open Type
open Utils0
open Var0
open Wsize

module E :
 sig
  val pass : string

  val load_constants_ref_error : instr_info -> string -> pp_error_loc
 end

val fresh_word :
  (instr_info -> Uint63.t -> string -> atype -> Ident.Ident.ident) ->
  instr_info -> Uint63.t -> wsize -> var_i

val process_constant :
  'a1 asmOp -> (instr_info -> Uint63.t -> string -> atype ->
  Ident.Ident.ident) -> instr_info -> Uint63.t -> wsize -> pexpr -> ('a1
  instr_r list * pexpr) * SvExtra.Sv.t

val process_condition :
  'a1 asmOp -> (instr_info -> Uint63.t -> string -> atype ->
  Ident.Ident.ident) -> SvExtra.Sv.t -> instr_info -> pexpr -> ('a1 instr_r
  list * pexpr) cexec

val load_constants_c :
  'a1 asmOp -> ('a1 instr -> 'a1 instr list cexec) -> 'a1 instr list ->
  (pp_error_loc, 'a1 instr list) result

val load_constants_i :
  'a1 asmOp -> (instr_info -> Uint63.t -> string -> atype ->
  Ident.Ident.ident) -> SvExtra.Sv.t -> 'a1 instr -> (pp_error_loc, 'a1 instr
  list) result

val load_constants_fd :
  'a1 asmOp -> progT -> (instr_info -> Uint63.t -> string -> atype ->
  Ident.Ident.ident) -> 'a1 fundef -> (pp_error_loc, ('a1, extra_fun_t)
  _fundef) result

val load_constants_prog :
  'a1 asmOp -> progT -> (instr_info -> Uint63.t -> string -> atype ->
  Ident.Ident.ident) -> bool -> ('a1, extra_fun_t, extra_prog_t) _prog -> 'a1
  prog cexec

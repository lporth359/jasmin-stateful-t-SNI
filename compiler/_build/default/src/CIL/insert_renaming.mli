open Compiler_util
open Eqtype
open Expr
open Seq
open Sopn
open Type
open Var0

val rename_var_r : 'a1 asmOp -> var_i -> 'a1 instr_r

val rename_var : 'a1 asmOp -> instr_info -> var_i -> 'a1 instr

val rename_vars : 'a1 asmOp -> instr_info -> var_i list -> 'a1 instr list

val insert_renaming_body : 'a1 asmOp -> progT -> 'a1 fundef -> 'a1 instr list

val should_insert_renaming :
  'a1 asmOp -> progT -> (fun_info -> bool) -> 'a1 fundef -> bool

val insert_renaming_fd :
  'a1 asmOp -> progT -> (fun_info -> bool) -> 'a1 fundef -> 'a1 fundef

val insert_renaming_prog :
  'a1 asmOp -> progT -> (fun_info -> bool) -> 'a1 prog -> 'a1 prog

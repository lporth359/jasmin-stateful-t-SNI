open BinInt
open BinNums
open Datatypes
open Expr
open Fexpr
open Label
open Sopn
open Ssralg
open Type
open Var0
open Wsize

type 'asm_op linstr_r =
| Lopn of lexpr list * 'asm_op sopn * rexpr list
| Lsyscall of (Wsize.wsize * BinNums.positive) Syscall_t.syscall_t
| Lcall of var_i option * remote_label
| Lret
| Lalign
| Llabel of label_kind * label
| Lgoto of remote_label
| Ligoto of rexpr
| LstoreLabel of Var.var * label
| Lcond of fexpr * label

type 'asm_op linstr = { li_ii : instr_info; li_i : 'asm_op linstr_r }

type 'asm_op lcmd = 'asm_op linstr list

type 'asm_op lfundef = { lfd_info : fun_info; lfd_align : wsize;
                         lfd_tyin : ltype list; lfd_arg : var_i list;
                         lfd_body : 'asm_op lcmd; lfd_tyout : ltype list;
                         lfd_res : var_i list; lfd_export : bool;
                         lfd_callee_saved : Var.var list;
                         lfd_stk_max : coq_Z; lfd_frame_size : coq_Z;
                         lfd_align_args : wsize list }

val lfd_total_stack : 'a1 asmOp -> 'a1 lfundef -> coq_Z

type 'asm_op lprog = { lp_rip : Ident.Ident.ident;
                       lp_rsp : Ident.Ident.ident;
                       lp_globs : GRing.ComRing.sort list;
                       lp_glob_names : ((Var.var * wsize) * coq_Z) list;
                       lp_funcs : (funname * 'asm_op lfundef) list }

val li_of_fopn_args :
  'a1 asmOp -> instr_info -> ((lexpr list * 'a1 sopn) * rexpr list) -> 'a1
  linstr

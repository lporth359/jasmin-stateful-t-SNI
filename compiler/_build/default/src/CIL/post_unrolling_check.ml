open Datatypes
open Compiler_util
open Expr
open Sopn
open Utils0
open Var0

module E =
 struct
  (** val pass : string **)

  let pass =
    "loop unrolling"

  (** val for_loop_remains : pp_error_loc **)

  let for_loop_remains =
    { pel_msg = (PPEstring "for loops remain"); pel_fn = None; pel_fi = None;
      pel_ii = None; pel_vi = None; pel_pass = (Some pass); pel_internal =
      false }

  (** val inline_instr_remains : pp_error_loc **)

  let inline_instr_remains =
    { pel_msg = (PPEstring
      "\226\128\156inline\226\128\157-annotated instructions remain");
      pel_fn = None; pel_fi = None; pel_ii = None; pel_vi = None; pel_pass =
      (Some pass); pel_internal = false }
 end

(** val check_no_for_loop_cmd :
    'a1 asmOp -> ('a1 instr -> unit cexec) -> 'a1 instr list ->
    (pp_error_loc, unit) result **)

let check_no_for_loop_cmd _ =
  allM

(** val check_no_for_loop_instr : 'a1 asmOp -> 'a1 instr -> unit cexec **)

let check_no_for_loop_instr asmop =
  let rec check_no_for_loop_instr_r = function
  | Cassgn (_, _, _, _) -> Ok ()
  | Copn (_, _, _, _) -> Ok ()
  | Csyscall (_, _, _) -> Ok ()
  | Cif (_, c, c') ->
    (match check_no_for_loop_cmd asmop check_no_for_loop_instr0 c with
     | Ok _ -> check_no_for_loop_cmd asmop check_no_for_loop_instr0 c'
     | Error s -> Error s)
  | Cfor (_, _, _) -> Error E.for_loop_remains
  | Cwhile (_, c, _, _, c') ->
    (match check_no_for_loop_cmd asmop check_no_for_loop_instr0 c with
     | Ok _ -> check_no_for_loop_cmd asmop check_no_for_loop_instr0 c'
     | Error s -> Error s)
  | Ccall (_, _, _) -> Ok ()
  and check_no_for_loop_instr0 = function
  | MkI (ii, i0) -> add_iinfo ii (check_no_for_loop_instr_r i0)
  in check_no_for_loop_instr0

(** val check_no_for_loop_fd :
    'a1 asmOp -> (funname * 'a1 ufundef) -> unit cexec **)

let check_no_for_loop_fd asmop = function
| (fn, fd) ->
  add_funname fn
    (add_finfo fd.f_info
      (check_no_for_loop_cmd asmop (check_no_for_loop_instr asmop) fd.f_body))

(** val check_no_for_loop :
    'a1 asmOp -> 'a1 uprog -> (pp_error_loc, unit) result **)

let check_no_for_loop asmop p =
  allM (check_no_for_loop_fd asmop) p.p_funcs

(** val check_no_inline_instr_cmd :
    'a1 asmOp -> ('a1 instr -> unit cexec) -> 'a1 instr list ->
    (pp_error_loc, unit) result **)

let check_no_inline_instr_cmd _ =
  allM

(** val check_no_inline_instr_instr : 'a1 asmOp -> 'a1 instr -> unit cexec **)

let check_no_inline_instr_instr asmop =
  let rec check_no_inline_instr_instr_r = function
  | Cassgn (_, _, _, _) -> Ok ()
  | Copn (_, _, _, _) -> Ok ()
  | Csyscall (_, _, _) -> Ok ()
  | Cif (_, c, c') ->
    (match check_no_inline_instr_cmd asmop check_no_inline_instr_instr0 c with
     | Ok _ -> check_no_inline_instr_cmd asmop check_no_inline_instr_instr0 c'
     | Error s -> Error s)
  | Cwhile (_, c, _, _, c') ->
    (match check_no_inline_instr_cmd asmop check_no_inline_instr_instr0 c with
     | Ok _ -> check_no_inline_instr_cmd asmop check_no_inline_instr_instr0 c'
     | Error s -> Error s)
  | _ -> Ok ()
  and check_no_inline_instr_instr0 = function
  | MkI (ii, i0) ->
    add_iinfo ii
      (if negb (ii_is_inline ii)
       then check_no_inline_instr_instr_r i0
       else Error E.inline_instr_remains)
  in check_no_inline_instr_instr0

(** val check_no_inline_instr_fd :
    'a1 asmOp -> (funname * 'a1 ufundef) -> unit cexec **)

let check_no_inline_instr_fd asmop = function
| (fn, fd) ->
  add_funname fn
    (add_finfo fd.f_info
      (check_no_inline_instr_cmd asmop (check_no_inline_instr_instr asmop)
        fd.f_body))

(** val check_no_inline_instr :
    'a1 asmOp -> 'a1 uprog -> (pp_error_loc, unit) result **)

let check_no_inline_instr asmop p =
  allM (check_no_inline_instr_fd asmop) p.p_funcs

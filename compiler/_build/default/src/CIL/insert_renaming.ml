open Compiler_util
open Eqtype
open Expr
open Seq
open Sopn
open Type
open Var0

(** val rename_var_r : 'a1 asmOp -> var_i -> 'a1 instr_r **)

let rename_var_r _ x =
  Cassgn ((Lvar x), AT_none, (Var.vtype x.v_var), (Pvar (mk_lvar x)))

(** val rename_var : 'a1 asmOp -> instr_info -> var_i -> 'a1 instr **)

let rename_var asmop ii x =
  MkI (ii, (rename_var_r asmop x))

(** val rename_vars :
    'a1 asmOp -> instr_info -> var_i list -> 'a1 instr list **)

let rename_vars asmop ii =
  map (rename_var asmop ii)

(** val insert_renaming_body :
    'a1 asmOp -> progT -> 'a1 fundef -> 'a1 instr list **)

let insert_renaming_body asmop _ fd =
  cat (rename_vars asmop (entry_info_of_fun_info fd.f_info) fd.f_params)
    (cat fd.f_body
      (rename_vars asmop (ret_info_of_fun_info fd.f_info) fd.f_res))

(** val should_insert_renaming :
    'a1 asmOp -> progT -> (fun_info -> bool) -> 'a1 fundef -> bool **)

let should_insert_renaming _ _ insert_renaming_p fd =
  let well_typed = fun xs tys ->
    eq_op
      (coq_Datatypes_list__canonical__eqtype_Equality
        type_atype__canonical__eqtype_Equality) tys
      (Obj.magic map (fun x -> Var.vtype x.v_var) xs)
  in
  (&&) (insert_renaming_p fd.f_info)
    ((&&) (Obj.magic well_typed fd.f_params fd.f_tyin)
      (Obj.magic well_typed fd.f_res fd.f_tyout))

(** val insert_renaming_fd :
    'a1 asmOp -> progT -> (fun_info -> bool) -> 'a1 fundef -> 'a1 fundef **)

let insert_renaming_fd asmop pT insert_renaming_p fd =
  if should_insert_renaming asmop pT insert_renaming_p fd
  then with_body asmop fd (insert_renaming_body asmop pT fd)
  else fd

(** val insert_renaming_prog :
    'a1 asmOp -> progT -> (fun_info -> bool) -> 'a1 prog -> 'a1 prog **)

let insert_renaming_prog asmop pT insert_renaming_p p =
  map_prog asmop pT (insert_renaming_fd asmop pT insert_renaming_p) p

open Datatypes
open Uint0
open Compiler_util
open Eqtype
open Expr
open Pseudo_operator
open Seq
open Sopn
open Ssrnat
open Syscall
open Type
open Utils0
open Var0

module E =
 struct
  (** val pass : string **)

  let pass =
    "make reference arguments"

  (** val make_ref_error : instr_info -> string -> pp_error_loc **)

  let make_ref_error =
    pp_internal_error_s_at pass
 end

(** val with_id :
    (instr_info -> Uint63.t -> string -> atype -> Ident.Ident.ident) ->
    var_info -> instr_info -> Uint63.t -> string -> atype -> var_i **)

let with_id fresh_reg_ptr vi ii ctr id ty =
  { v_var = { Var.vtype = ty; Var.vname = (fresh_reg_ptr ii ctr id ty) };
    v_info = vi }

(** val is_reg_ptr_expr :
    (instr_info -> Uint63.t -> string -> atype -> Ident.Ident.ident) -> bool
    -> instr_info -> Uint63.t -> string -> atype -> pexpr -> var_i option **)

let is_reg_ptr_expr fresh_reg_ptr doit ii ctr id ty = function
| Pvar x' ->
  if (&&) doit
       ((&&) (OtherDefs.is_aarr (Var.vtype x'.gv.v_var))
         ((&&) (convertible ty (Var.vtype x'.gv.v_var))
           ((||) (is_glob x') (negb (is_reg_ptr x'.gv.v_var)))))
  then Some (with_id fresh_reg_ptr x'.gv.v_info ii ctr id ty)
  else None
| Psub (_, ws, len, x', _) ->
  if (&&) doit (convertible ty (Coq_aarr (ws, len)))
  then Some (with_id fresh_reg_ptr x'.gv.v_info ii ctr id ty)
  else None
| _ -> None

(** val is_reg_ptr_lval :
    (instr_info -> Uint63.t -> string -> atype -> Ident.Ident.ident) -> bool
    -> instr_info -> Uint63.t -> string -> atype -> lval -> var_i option **)

let is_reg_ptr_lval fresh_reg_ptr doit ii ctr id ty = function
| Lvar x' ->
  if (&&) doit (negb (is_reg_ptr x'.v_var))
  then Some (with_id fresh_reg_ptr x'.v_info ii ctr id ty)
  else None
| Lasub (_, _, _, x', _) ->
  if doit then Some (with_id fresh_reg_ptr x'.v_info ii ctr id ty) else None
| _ -> None

(** val make_prologue :
    'a1 asmOp -> (instr_info -> Uint63.t -> string -> atype ->
    Ident.Ident.ident) -> instr_info -> SvExtra.Sv.t -> Uint63.t ->
    ((bool * string) * atype) list -> pexpr list -> (pp_error_loc, 'a1 instr
    list * pexpr list) result **)

let rec make_prologue asmop fresh_reg_ptr ii x ctr xtys es =
  match xtys with
  | [] ->
    (match es with
     | [] -> Ok ([], [])
     | _ :: _ -> Error (E.make_ref_error ii "assert false (prologue)"))
  | y :: xtys0 ->
    let (y0, ty) = y in
    let (doit, id) = y0 in
    (match es with
     | [] -> Error (E.make_ref_error ii "assert false (prologue)")
     | e :: es0 ->
       (match is_reg_ptr_expr fresh_reg_ptr doit ii ctr id ty e with
        | Some y1 ->
          if negb (SvExtra.Sv.mem (Obj.magic y1.v_var) x)
          then (match make_prologue asmop fresh_reg_ptr ii
                        (SvExtra.Sv.add (Obj.magic y1.v_var) x) (succ ctr)
                        xtys0 es0 with
                | Ok x0 ->
                  let (p, es') = x0 in
                  Ok (((MkI (ii, (Cassgn ((Lvar y1), AT_rename, ty,
                  e)))) :: p), ((coq_Plvar y1) :: es'))
                | Error s -> Error s)
          else let s = E.make_ref_error ii "bad fresh id (prologue)" in
               Error s
        | None ->
          (match make_prologue asmop fresh_reg_ptr ii x ctr xtys0 es0 with
           | Ok x0 -> let (p, es') = x0 in Ok (p, (e :: es'))
           | Error s -> Error s)))

type pseudo_instr =
| PI_lv of lval
| PI_i of lval * atype * var_i

(** val make_pseudo_epilogue :
    (instr_info -> Uint63.t -> string -> atype -> Ident.Ident.ident) ->
    instr_info -> SvExtra.Sv.t -> Uint63.t -> ((bool * string) * atype) list
    -> lval list -> (pp_error_loc, pseudo_instr list) result **)

let rec make_pseudo_epilogue fresh_reg_ptr ii x ctr xtys rs =
  match xtys with
  | [] ->
    (match rs with
     | [] -> Ok []
     | _ :: _ -> Error (E.make_ref_error ii "assert false (epilogue)"))
  | y :: xtys0 ->
    let (y0, ty) = y in
    let (doit, id) = y0 in
    (match rs with
     | [] -> Error (E.make_ref_error ii "assert false (epilogue)")
     | r :: rs0 ->
       (match is_reg_ptr_lval fresh_reg_ptr doit ii ctr id ty r with
        | Some y1 ->
          if negb (SvExtra.Sv.mem (Obj.magic y1.v_var) x)
          then (match make_pseudo_epilogue fresh_reg_ptr ii x (succ ctr)
                        xtys0 rs0 with
                | Ok x0 ->
                  Ok ((PI_lv (Lvar y1)) :: ((PI_i (r, ty, y1)) :: x0))
                | Error s -> Error s)
          else let s = E.make_ref_error ii "bad fresh id (epilogue)" in
               Error s
        | None ->
          (match make_pseudo_epilogue fresh_reg_ptr ii x ctr xtys0 rs0 with
           | Ok x0 -> Ok ((PI_lv r) :: x0)
           | Error s -> Error s)))

(** val mk_ep_i :
    'a1 asmOp -> instr_info -> lval -> atype -> var_i -> 'a1 instr **)

let mk_ep_i _ ii r ty y =
  MkI (ii, (Cassgn (r, AT_rename, ty, (coq_Plvar y))))

(** val wf_lv : lval -> bool **)

let wf_lv = function
| Lvar _ -> true
| Lasub (_, _, _, _, e) -> negb (use_mem e)
| _ -> false

(** val swapable :
    'a1 asmOp -> instr_info -> pseudo_instr list -> (pp_error_loc, lval
    list * 'a1 instr list) result **)

let rec swapable asmop ii = function
| [] -> Ok ([], [])
| p0 :: pis0 ->
  (match p0 with
   | PI_lv lv ->
     (match swapable asmop ii pis0 with
      | Ok x -> let (lvs, ep) = x in Ok ((lv :: lvs), ep)
      | Error s -> Error s)
   | PI_i (r, ty, y) ->
     (match swapable asmop ii pis0 with
      | Ok x ->
        let (lvs, ep) = x in
        let i = mk_ep_i asmop ii r ty y in
        if SvExtra.disjoint (read_rvs lvs) (write_I asmop i)
        then if SvExtra.disjoint (vrvs lvs)
                  (SvExtra.Sv.union (write_I asmop i) (read_I asmop i))
             then if wf_lv r
                  then Ok (lvs, (i :: ep))
                  else let s = E.make_ref_error ii "cannot swap 3" in Error s
             else let s = E.make_ref_error ii "cannot swap 2" in Error s
        else let s = E.make_ref_error ii "cannot swap 1" in Error s
      | Error s -> Error s))

(** val make_epilogue :
    'a1 asmOp -> (instr_info -> Uint63.t -> string -> atype ->
    Ident.Ident.ident) -> instr_info -> SvExtra.Sv.t ->
    ((bool * string) * atype) list -> lval list -> (pp_error_loc, lval
    list * 'a1 instr list) result **)

let make_epilogue asmop fresh_reg_ptr ii x xtys rs =
  match make_pseudo_epilogue fresh_reg_ptr ii x (Uint63.of_int (0)) xtys rs with
  | Ok x0 -> swapable asmop ii x0
  | Error s -> Error s

(** val update_c :
    'a1 asmOp -> ('a1 instr -> 'a1 instr list cexec) -> 'a1 instr list ->
    (pp_error_loc, 'a1 instr list) result **)

let update_c _ update_i0 c =
  match mapM update_i0 c with
  | Ok x -> Ok (flatten x)
  | Error s -> Error s

(** val mk_info : var_i -> atype -> (bool * string) * atype **)

let mk_info x ty =
  (((is_reg_ptr x.v_var), (Ident.Ident.id_name (Var.vname x.v_var))), ty)

(** val get_sig :
    'a1 asmOp -> 'a1 uprog -> instr_info -> funname -> (pp_error_loc,
    ((bool * string) * atype) list * ((bool * string) * atype) list) result **)

let get_sig _ p ii fn =
  match get_fundef p.p_funcs fn with
  | Some fd ->
    Ok ((map2 mk_info fd.f_params fd.f_tyin),
      (map2 mk_info fd.f_res fd.f_tyout))
  | None -> Error (E.make_ref_error ii "unknown function")

(** val get_syscall_sig :
    (Wsize.wsize * BinNums.positive) Syscall_t.syscall_t ->
    ((bool * string) * atype) list * ((bool * string) * atype) list **)

let get_syscall_sig o =
  ((map (fun ty -> (((OtherDefs.is_aarr ty), "__p__"), ty))
     (syscall_sig_u o).scs_tin),
    (map (fun ty -> (((OtherDefs.is_aarr ty), "__p__"), ty))
      (syscall_sig_u o).scs_tout))

(** val is_swap_op : 'a1 asmOp -> 'a1 sopn -> atype option **)

let is_swap_op _ = function
| Opseudo_op p0 ->
  (match p0 with
   | Oswap ty -> (match ty with
                  | Coq_aarr (_, _) -> Some ty
                  | _ -> None)
   | _ -> None)
| _ -> None

(** val update_i :
    'a1 asmOp -> (instr_info -> Uint63.t -> string -> atype ->
    Ident.Ident.ident) -> 'a1 uprog -> SvExtra.Sv.t -> 'a1 instr -> 'a1 instr
    list cexec **)

let rec update_i asmop fresh_reg_ptr p x i = match i with
| MkI (ii, ir) ->
  (match ir with
   | Cassgn (_, _, _, _) -> Ok (i :: [])
   | Copn (xs, tg, op, es) ->
     (match is_swap_op asmop op with
      | Some ty ->
        let sig0 = ((true, "__swap__"), ty) in
        let sig1 = sig0 :: (sig0 :: []) in
        (match make_prologue asmop fresh_reg_ptr ii x (Uint63.of_int (0))
                 sig1 es with
         | Ok x0 ->
           let (prologue, es0) = x0 in
           (match make_epilogue asmop fresh_reg_ptr ii x sig1 xs with
            | Ok x1 ->
              let (xs0, epilogue) = x1 in
              let tg0 =
                if (&&)
                     (eq_op coq_Datatypes_nat__canonical__eqtype_Equality
                       (Obj.magic size prologue) (Obj.magic (S (S O))))
                     (eq_op coq_Datatypes_nat__canonical__eqtype_Equality
                       (Obj.magic size epilogue) (Obj.magic (S (S O))))
                then AT_inline
                else tg
              in
              Ok
              (cat prologue ((MkI (ii, (Copn (xs0, tg0, (Opseudo_op (Oswap
                ty)), es0)))) :: epilogue))
            | Error s -> Error s)
         | Error s -> Error s)
      | None -> Ok (i :: []))
   | Csyscall (xs, o, es) ->
     let (params, returns) = get_syscall_sig o in
     (match make_prologue asmop fresh_reg_ptr ii x (Uint63.of_int (0)) params
              es with
      | Ok x0 ->
        let (prologue, es0) = x0 in
        (match make_epilogue asmop fresh_reg_ptr ii x returns xs with
         | Ok x1 ->
           let (xs0, epilogue) = x1 in
           Ok
           (cat prologue ((MkI (ii, (Csyscall (xs0, o, es0)))) :: epilogue))
         | Error s -> Error s)
      | Error s -> Error s)
   | Cif (b, c1, c2) ->
     (match update_c asmop (update_i asmop fresh_reg_ptr p x) c1 with
      | Ok x0 ->
        (match update_c asmop (update_i asmop fresh_reg_ptr p x) c2 with
         | Ok x1 -> Ok ((MkI (ii, (Cif (b, x0, x1)))) :: [])
         | Error s -> Error s)
      | Error s -> Error s)
   | Cfor (x0, r, c) ->
     (match update_c asmop (update_i asmop fresh_reg_ptr p x) c with
      | Ok x1 -> Ok ((MkI (ii, (Cfor (x0, r, x1)))) :: [])
      | Error s -> Error s)
   | Cwhile (a, c, e, info, c') ->
     (match update_c asmop (update_i asmop fresh_reg_ptr p x) c with
      | Ok x0 ->
        (match update_c asmop (update_i asmop fresh_reg_ptr p x) c' with
         | Ok x1 -> Ok ((MkI (ii, (Cwhile (a, x0, e, info, x1)))) :: [])
         | Error s -> Error s)
      | Error s -> Error s)
   | Ccall (xs, fn, es) ->
     (match get_sig asmop p ii fn with
      | Ok x0 ->
        let (params, returns) = x0 in
        (match make_prologue asmop fresh_reg_ptr ii x (Uint63.of_int (0))
                 params es with
         | Ok x1 ->
           let (prologue, es0) = x1 in
           (match make_epilogue asmop fresh_reg_ptr ii x returns xs with
            | Ok x2 ->
              let (xs0, epilogue) = x2 in
              Ok
              (cat prologue ((MkI (ii, (Ccall (xs0, fn, es0)))) :: epilogue))
            | Error s -> Error s)
         | Error s -> Error s)
      | Error s -> Error s))

(** val update_fd :
    'a1 asmOp -> (instr_info -> Uint63.t -> string -> atype ->
    Ident.Ident.ident) -> 'a1 uprog -> 'a1 ufundef -> (pp_error_loc, ('a1,
    extra_fun_t) _fundef) result **)

let update_fd asmop fresh_reg_ptr p fd =
  let body = fd.f_body in
  let write = write_c asmop body in
  let read = read_c asmop body in
  let returns = read_es (map coq_Plvar fd.f_res) in
  let x = SvExtra.Sv.union returns (SvExtra.Sv.union write read) in
  (match update_c asmop (update_i asmop fresh_reg_ptr p x) body with
   | Ok x0 -> Ok (with_body asmop fd x0)
   | Error s -> Error s)

(** val makereference_prog :
    'a1 asmOp -> (instr_info -> Uint63.t -> string -> atype ->
    Ident.Ident.ident) -> 'a1 uprog -> 'a1 uprog cexec **)

let makereference_prog asmop fresh_reg_ptr p =
  match map_cfprog_gen (fun x -> x.f_info) (update_fd asmop fresh_reg_ptr p)
          p.p_funcs with
  | Ok x -> Ok { p_funcs = x; p_globs = p.p_globs; p_extra = p.p_extra }
  | Error s -> Error s

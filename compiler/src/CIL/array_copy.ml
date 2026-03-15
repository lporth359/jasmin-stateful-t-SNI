open BinNums
open Datatypes
open Compiler_util
open Expr
open Memory_model
open Pseudo_operator
open Seq
open Sopn
open Type
open Utils0
open Var0
open Warray_
open Wsize

module E =
 struct
  (** val pass : string **)

  let pass =
    "array copy"

  (** val error : pp_error_loc **)

  let error =
    pp_internal_error_s pass "fresh variables are not fresh ..."
 end

(** val direct_copy :
    'a1 asmOp -> wsize -> var_i -> gvar -> pexpr -> 'a1 instr_r list **)

let direct_copy _ ws x y i =
  (Cassgn ((Laset (Aligned, AAscale, ws, x, i)), AT_none, (Coq_aword ws),
    (Pget (Aligned, AAscale, ws, y, i)))) :: []

(** val tmp_var :
    (v_kind -> string -> atype -> Ident.Ident.ident) -> wsize -> Var.var **)

let tmp_var fresh_var_ident =
  let fresh_temporary = fun ws ->
    fresh_var_ident (Reg (Normal, Direct)) "tmp" (Coq_aword ws)
  in
  (fun ws -> { Var.vtype = (Coq_aword ws); Var.vname = (fresh_temporary ws) })

(** val indirect_copy :
    'a1 asmOp -> (v_kind -> string -> atype -> Ident.Ident.ident) -> wsize ->
    var_i -> gvar -> pexpr -> 'a1 instr_r list **)

let indirect_copy _ fresh_var_ident ws x y i =
  let tmp = { v_var = (tmp_var fresh_var_ident ws); v_info = x.v_info } in
  (Cassgn ((Lvar tmp), AT_none, (Coq_aword ws), (Pget (Aligned, AAscale, ws,
  y, i)))) :: ((Cassgn ((Laset (Aligned, AAscale, ws, x, i)), AT_none,
  (Coq_aword ws), (Pvar (mk_lvar tmp)))) :: [])

(** val needs_temporary : Var.var -> Var.var -> bool **)

let needs_temporary x y =
  (&&) (is_var_in_memory x) (is_var_in_memory y)

(** val array_copy :
    'a1 asmOp -> (v_kind -> string -> atype -> Ident.Ident.ident) ->
    instr_info -> var_i -> wsize -> positive -> gvar -> 'a1 instr list **)

let array_copy asmop fresh_var_ident =
  let fresh_counter = fresh_var_ident Inline "i__copy" Coq_aint in
  (fun ii x ws n y ->
  let i = { v_var = { Var.vtype = Coq_aint; Var.vname = fresh_counter };
    v_info = x.v_info }
  in
  let ei = Pvar (mk_lvar i) in
  let pre =
    if (||) (eq_gvar (mk_lvar x) y) (is_ptr x.v_var)
    then Copn ([], AT_none, (sopn_nop asmop), [])
    else Cassgn ((Lvar x), AT_none, (Coq_aarr (ws, n)), (Parr_init (ws, n)))
  in
  (MkI (ii, pre)) :: ((MkI (ii, (Cfor (i, ((UpTo, (Pconst Z0)), (Pconst (Zpos
  n))),
  (map (fun i0 -> MkI (ii, i0))
    (if needs_temporary x.v_var y.gv.v_var
     then indirect_copy asmop fresh_var_ident ws x y ei
     else direct_copy asmop ws x y ei)))))) :: []))

(** val array_copy_c :
    'a1 asmOp -> SvExtra.Sv.t -> (SvExtra.Sv.t -> 'a1 instr -> 'a1 instr list
    cexec) -> 'a1 instr list -> 'a1 instr list cexec **)

let array_copy_c _ v array_copy_i0 c =
  match mapM (array_copy_i0 v) c with
  | Ok x -> Ok (flatten x)
  | Error s -> Error s

(** val is_copy : 'a1 asmOp -> 'a1 sopn -> (wsize * positive) option **)

let is_copy _ = function
| Opseudo_op p0 -> (match p0 with
                    | Ocopy (ws, p) -> Some (ws, p)
                    | _ -> None)
| _ -> None

(** val get_source :
    'a1 asmOp -> (v_kind -> string -> atype -> Ident.Ident.ident) ->
    SvExtra.Sv.t -> instr_info -> pexpr list -> (gvar * 'a1 instr list) cexec **)

let get_source _ fresh_var_ident v ii = function
| [] ->
  Error (pp_internal_error_s_at E.pass ii "copy should have a single source")
| e :: l ->
  (match l with
   | [] ->
     (match e with
      | Pvar x -> Ok (x, [])
      | Psub (_, ws, len, x, _) ->
        let ty = Coq_aarr (ws, len) in
        let y_name =
          fresh_var_ident (Ident.Ident.id_kind (Var.vname x.gv.v_var)) "src"
            ty
        in
        let y_var = { v_var = { Var.vtype = ty; Var.vname = y_name };
          v_info = (var_info_of_ii ii) }
        in
        if negb (SvExtra.Sv.mem (Obj.magic y_var.v_var) v)
        then let y = { gv = y_var; gs = Slocal } in
             Ok (y, ((MkI (ii, (Cassgn ((Lvar y_var), AT_rename, ty,
             e)))) :: []))
        else let s = pp_internal_error_s_at E.pass ii "fresh source not fresh"
             in
             Error s
      | _ ->
        Error (pp_internal_error_s_at E.pass ii "unexpected source for copy "))
   | _ :: _ ->
     Error
       (pp_internal_error_s_at E.pass ii "copy should have a single source"))

(** val get_target :
    'a1 asmOp -> (v_kind -> string -> atype -> Ident.Ident.ident) ->
    SvExtra.Sv.t -> instr_info -> lval list -> (var_i * 'a1 instr list) cexec **)

let get_target _ fresh_var_ident v ii = function
| [] ->
  Error
    (pp_internal_error_s_at E.pass ii "copy should have a single destination")
| d :: l ->
  (match l with
   | [] ->
     (match d with
      | Lvar x -> Ok (x, [])
      | Lasub (_, ws, len, x, _) ->
        let ty = Coq_aarr (ws, len) in
        let x_name =
          fresh_var_ident (Ident.Ident.id_kind (Var.vname x.v_var)) "dst" ty
        in
        let x_var = { v_var = { Var.vtype = ty; Var.vname = x_name };
          v_info = (var_info_of_ii ii) }
        in
        if negb (SvExtra.Sv.mem (Obj.magic x_var.v_var) v)
        then let x0 = { gv = x_var; gs = Slocal } in
             Ok (x_var, ((MkI (ii, (Cassgn (d, AT_rename, ty, (Pvar
             x0))))) :: []))
        else let s =
               pp_internal_error_s_at E.pass ii "fresh destination not fresh"
             in
             Error s
      | _ ->
        Error
          (pp_internal_error_s_at E.pass ii
            "unexpected destination for copy "))
   | _ :: _ ->
     Error
       (pp_internal_error_s_at E.pass ii
         "copy should have a single destination"))

(** val array_copy_i :
    'a1 asmOp -> (v_kind -> string -> atype -> Ident.Ident.ident) ->
    SvExtra.Sv.t -> 'a1 instr -> 'a1 instr list cexec **)

let rec array_copy_i asmop fresh_var_ident v i = match i with
| MkI (ii, id) ->
  (match id with
   | Copn (xs, _, o, es) ->
     (match is_copy asmop o with
      | Some p ->
        let (ws, n) = p in
        (match get_source asmop fresh_var_ident v ii es with
         | Ok x ->
           let (y, pre) = x in
           (match get_target asmop fresh_var_ident v ii xs with
            | Ok x0 ->
              let (x1, post) = x0 in
              if convertible (Var.vtype x1.v_var) (Coq_aarr (ws, n))
              then Ok
                     (cat pre
                       (cat (array_copy asmop fresh_var_ident ii x1 ws n y)
                         post))
              else let s =
                     pp_internal_error_s_at E.pass ii "bad type for copy"
                   in
                   Error s
            | Error s -> Error s)
         | Error s -> Error s)
      | None -> Ok (i :: []))
   | Cif (e, c1, c2) ->
     (match array_copy_c asmop v (array_copy_i asmop fresh_var_ident) c1 with
      | Ok x ->
        (match array_copy_c asmop v (array_copy_i asmop fresh_var_ident) c2 with
         | Ok x0 -> Ok ((MkI (ii, (Cif (e, x, x0)))) :: [])
         | Error s -> Error s)
      | Error s -> Error s)
   | Cfor (i0, r, c) ->
     (match array_copy_c asmop v (array_copy_i asmop fresh_var_ident) c with
      | Ok x -> Ok ((MkI (ii, (Cfor (i0, r, x)))) :: [])
      | Error s -> Error s)
   | Cwhile (a, c1, e, info, c2) ->
     (match array_copy_c asmop v (array_copy_i asmop fresh_var_ident) c1 with
      | Ok x ->
        (match array_copy_c asmop v (array_copy_i asmop fresh_var_ident) c2 with
         | Ok x0 -> Ok ((MkI (ii, (Cwhile (a, x, e, info, x0)))) :: [])
         | Error s -> Error s)
      | Error s -> Error s)
   | _ -> Ok (i :: []))

(** val array_copy_fd :
    'a1 asmOp -> (v_kind -> string -> atype -> Ident.Ident.ident) -> progT ->
    SvExtra.Sv.t -> 'a1 fundef -> (pp_error_loc, ('a1, extra_fun_t) _fundef)
    result **)

let array_copy_fd asmop fresh_var_ident _ v f =
  let { f_info = fi; f_tyin = tyin; f_params = params; f_body = c; f_tyout =
    tyout; f_res = res; f_extra = ev } = f
  in
  (match array_copy_c asmop v (array_copy_i asmop fresh_var_ident) c with
   | Ok x ->
     Ok { f_info = fi; f_tyin = tyin; f_params = params; f_body = x;
       f_tyout = tyout; f_res = res; f_extra = ev }
   | Error s -> Error s)

(** val array_copy_prog :
    'a1 asmOp -> (v_kind -> string -> atype -> Ident.Ident.ident) -> progT ->
    'a1 prog -> (pp_error_loc, ('a1, extra_fun_t, extra_prog_t) _prog) result **)

let array_copy_prog asmop fresh_var_ident =
  let fresh_counter = fresh_var_ident Inline "i__copy" Coq_aint in
  (fun pT p ->
  let v = vars_p asmop pT p.p_funcs in
  let fresh =
    SvExtra.Sv.add
      (Obj.magic { Var.vtype = Coq_aint; Var.vname = fresh_counter })
      (SvExtra.sv_of_list (Obj.magic tmp_var fresh_var_ident) wsizes)
  in
  if SvExtra.disjoint fresh v
  then (match map_cfprog_gen (fun x -> x.f_info)
                (array_copy_fd asmop fresh_var_ident pT v) p.p_funcs with
        | Ok x -> Ok { p_funcs = x; p_globs = p.p_globs; p_extra = p.p_extra }
        | Error s -> Error s)
  else Error E.error)

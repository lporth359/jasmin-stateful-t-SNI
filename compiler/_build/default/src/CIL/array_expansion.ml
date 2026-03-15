open BinInt
open BinNums
open Datatypes
open Compiler_util
open Eqtype
open Expr
open Memory_model
open Seq
open Sopn
open Ssrbool
open Type
open Utils0
open Var0
open Warray_
open Word_ssrZ
open Wsize

module E =
 struct
  (** val pass : string **)

  let pass =
    "array expansion"

  (** val reg_error : var_i -> string -> pp_error_loc **)

  let reg_error x msg =
    { pel_msg =
      (pp_box ((PPEstring "cannot expand variable") :: ((PPEvar
        x.v_var) :: ((PPEstring msg) :: [])))); pel_fn = None; pel_fi = None;
      pel_ii = None; pel_vi = (Some x.v_info); pel_pass = (Some pass);
      pel_internal = false }

  (** val reg_error_expr : pexpr -> string -> pp_error_loc **)

  let reg_error_expr e msg =
    { pel_msg =
      (pp_nobox ((PPEstring
        "cannot expand expression") :: (PPEbreak :: ((PPEstring
        "  ") :: ((PPEexpr e) :: (PPEbreak :: ((PPEstring msg) :: [])))))));
      pel_fn = None; pel_fi = None; pel_ii = None; pel_vi = None; pel_pass =
      (Some pass); pel_internal = false }

  (** val reg_ierror : var_i -> string -> pp_error_loc **)

  let reg_ierror x msg =
    { pel_msg =
      (pp_box ((PPEstring
        msg) :: ((pp_nobox ((PPEstring "(") :: ((PPEvar
                   x.v_var) :: ((PPEstring ")") :: [])))) :: []))); pel_fn =
      None; pel_fi = None; pel_ii = None; pel_vi = (Some x.v_info);
      pel_pass = (Some pass); pel_internal = true }

  (** val length_mismatch : pp_error_loc **)

  let length_mismatch =
    pp_internal_error_s pass "length mismatch"

  (** val reg_ierror_no_var : string -> pp_error_loc **)

  let reg_ierror_no_var =
    pp_internal_error_s pass
 end

type varr_info = { vi_v : Var.var; vi_s : wsize; vi_n : Ident.Ident.ident list }

type expand_info = { vars : Var.var list; arrs : varr_info list;
                     finfo : fun_info }

type array_info = { ai_ty : wsize; ai_len : coq_Z; ai_elems : Var.var list }

type t = { svars : SvExtra.Sv.t; sarrs : array_info Mvar.t }

type expd_t = ((wsize * coq_Z) option list * (wsize * coq_Z) option list) Mf.t

(** val init_elems :
    SvExtra.Sv.elt -> (SvExtra.Sv.t * coq_Z) -> (pp_error_loc,
    SvExtra.Sv.t * coq_Z) result **)

let init_elems xi = function
| (sv, i) ->
  if negb (SvExtra.Sv.mem xi sv)
  then Ok ((SvExtra.Sv.add xi sv), (Z.add i (Zpos Coq_xH)))
  else let s = E.reg_ierror_no_var "init_elems" in Error s

(** val init_array_info :
    varr_info -> (SvExtra.Sv.t * array_info Mvar.t) -> (pp_error_loc,
    SvExtra.Sv.t * array_info Mvar.Map.t) result **)

let init_array_info x = function
| (sv, m) ->
  let ty = Coq_aword x.vi_s in
  if negb (SvExtra.Sv.mem (Obj.magic x.vi_v) sv)
  then let vars0 = map (fun id -> { Var.vtype = ty; Var.vname = id }) x.vi_n
       in
       (match foldM (Obj.magic init_elems) (sv, Z0) vars0 with
        | Ok x0 ->
          let (sv0, len) = x0 in
          if (&&) (Z.ltb Z0 len)
               (convertible (Var.vtype x.vi_v) (Coq_aarr (x.vi_s,
                 (Z.to_pos len))))
          then Ok (sv0,
                 (Mvar.set m (Obj.magic x.vi_v) { ai_ty = x.vi_s; ai_len =
                   len; ai_elems = vars0 }))
          else let s = E.reg_ierror_no_var "init_array_info" in Error s
        | Error s -> Error s)
  else let s = E.reg_ierror_no_var "init_array_info" in Error s

(** val init_map : expand_info -> (pp_error_loc, t * fun_info) result **)

let init_map fi =
  let svars0 = SvExtra.sv_of_list (fun x -> Obj.magic x) fi.vars in
  (match foldM init_array_info (svars0, Mvar.empty) fi.arrs with
   | Ok x -> Ok ({ svars = svars0; sarrs = (snd x) }, fi.finfo)
   | Error s -> Error s)

(** val check_gvar : t -> gvar -> bool **)

let check_gvar m x =
  (||) (negb (is_lvar x)) (SvExtra.Sv.mem (Obj.magic x.gv.v_var) m.svars)

(** val expand_e : t -> pexpr -> pexpr cexec **)

let rec expand_e m e = match e with
| Pvar x ->
  if check_gvar m x
  then Ok e
  else let s =
         E.reg_error x.gv
           "(the array cannot be manipulated alone, you need to access its cells instead)"
       in
       Error s
| Pget (al, aa, ws, x, e1) ->
  if check_gvar m x
  then (match expand_e m e1 with
        | Ok x0 -> Ok (Pget (al, aa, ws, x, x0))
        | Error s -> Error s)
  else let x0 = x.gv in
       (match Mvar.get m.sarrs (Obj.magic x0.v_var) with
        | Some ai ->
          (match is_const e1 with
           | Some i ->
             if eq_op wsize_wsize__canonical__eqtype_Equality
                  (Obj.magic ai.ai_ty) (Obj.magic ws)
             then if eq_op memory_model_aligned__canonical__eqtype_Equality
                       (Obj.magic al) (Obj.magic Aligned)
                  then if eq_op
                            warray__arr_access__canonical__eqtype_Equality
                            (Obj.magic aa) (Obj.magic AAscale)
                       then if (&&) (Z.leb Z0 i) (Z.ltb i ai.ai_len)
                            then let v = znth x0.v_var ai.ai_elems i in
                                 Ok (Pvar
                                 (mk_lvar { v_var = v; v_info = x0.v_info }))
                            else let s =
                                   E.reg_error x0 "(index out of bounds)"
                                 in
                                 Error s
                       else let s =
                              E.reg_error x0
                                "(the default scale must be used)"
                            in
                            Error s
                  else let s = E.reg_error x0 "(alignement must be enforced)"
                       in
                       Error s
             else let s = E.reg_error x0 "(the default scale must be used)" in
                  Error s
           | None -> Error (E.reg_error x0 "(the index is not a constant)"))
        | None -> Error (E.reg_error x0 "(the index is not a constant)"))
| Psub (aa, ws, len, x, e1) ->
  if check_gvar m x
  then (match expand_e m e1 with
        | Ok x0 -> Ok (Psub (aa, ws, len, x, x0))
        | Error s -> Error s)
  else let s = E.reg_error x.gv "(sub-reg arrays are not allowed)" in Error s
| Pload (al, ws, e1) ->
  (match expand_e m e1 with
   | Ok x -> Ok (Pload (al, ws, x))
   | Error s -> Error s)
| Papp1 (o, e1) ->
  (match expand_e m e1 with
   | Ok x -> Ok (Papp1 (o, x))
   | Error s -> Error s)
| Papp2 (o, e1, e2) ->
  (match expand_e m e1 with
   | Ok x ->
     (match expand_e m e2 with
      | Ok x0 -> Ok (Papp2 (o, x, x0))
      | Error s -> Error s)
   | Error s -> Error s)
| PappN (o, es) ->
  (match mapM (expand_e m) es with
   | Ok x -> Ok (PappN (o, x))
   | Error s -> Error s)
| Pif (ty, e1, e2, e3) ->
  (match expand_e m e1 with
   | Ok x ->
     (match expand_e m e2 with
      | Ok x0 ->
        (match expand_e m e3 with
         | Ok x1 -> Ok (Pif (ty, x, x0, x1))
         | Error s -> Error s)
      | Error s -> Error s)
   | Error s -> Error s)
| _ -> Ok e

(** val expand_lv : t -> lval -> (pp_error_loc, lval) result **)

let expand_lv m x = match x with
| Lnone (_, _) -> Ok x
| Lvar x0 ->
  if SvExtra.Sv.mem (Obj.magic x0.v_var) m.svars
  then Ok (Lvar x0)
  else let s =
         E.reg_error x0
           "(the array cannot be manipulated alone, you need to access its cells instead)"
       in
       Error s
| Lmem (al, ws, vi, e) ->
  (match expand_e m e with
   | Ok x0 -> Ok (Lmem (al, ws, vi, x0))
   | Error s -> Error s)
| Laset (al, aa, ws, x0, e) ->
  if SvExtra.Sv.mem (Obj.magic x0.v_var) m.svars
  then (match expand_e m e with
        | Ok x1 -> Ok (Laset (al, aa, ws, x0, x1))
        | Error s -> Error s)
  else (match Mvar.get m.sarrs (Obj.magic x0.v_var) with
        | Some ai ->
          (match is_const e with
           | Some i ->
             if eq_op wsize_wsize__canonical__eqtype_Equality
                  (Obj.magic ai.ai_ty) (Obj.magic ws)
             then if eq_op memory_model_aligned__canonical__eqtype_Equality
                       (Obj.magic al) (Obj.magic Aligned)
                  then if eq_op
                            warray__arr_access__canonical__eqtype_Equality
                            (Obj.magic aa) (Obj.magic AAscale)
                       then if (&&) (Z.leb Z0 i) (Z.ltb i ai.ai_len)
                            then let v = znth x0.v_var ai.ai_elems i in
                                 Ok (Lvar { v_var = v; v_info = x0.v_info })
                            else let s =
                                   E.reg_error x0 "(index out of bounds)"
                                 in
                                 Error s
                       else let s =
                              E.reg_error x0
                                "(the default scale must be used)"
                            in
                            Error s
                  else let s = E.reg_error x0 "(alignement must be enforced)"
                       in
                       Error s
             else let s = E.reg_error x0 "(the default scale must be used)" in
                  Error s
           | None -> Error (E.reg_error x0 "(the index is not a constant)"))
        | None -> Error (E.reg_error x0 "(the index is not a constant)"))
| Lasub (aa, ws, len, x0, e) ->
  if SvExtra.Sv.mem (Obj.magic x0.v_var) m.svars
  then (match expand_e m e with
        | Ok x1 -> Ok (Lasub (aa, ws, len, x0, x1))
        | Error s -> Error s)
  else let s = E.reg_error x0 "(sub-reg arrays are not allowed)" in Error s

(** val expand_es : t -> pexpr list -> (pp_error_loc, pexpr list) result **)

let expand_es m =
  mapM (expand_e m)

(** val expand_lvs : t -> lval list -> (pp_error_loc, lval list) result **)

let expand_lvs m =
  mapM (expand_lv m)

(** val expand_param :
    t -> (Equality.sort * Equality.sort) option -> pexpr -> pexpr list cexec **)

let expand_param m ex e =
  match ex with
  | Some y ->
    let (ws, len) = y in
    (match e with
     | Pvar x ->
       (match o2r (E.reg_error x.gv "(not a reg array)")
                (Mvar.get m.sarrs (Obj.magic x.gv.v_var)) with
        | Ok x0 ->
          if (&&)
               (eq_op wsize_wsize__canonical__eqtype_Equality ws
                 (Obj.magic x0.ai_ty))
               ((&&)
                 (eq_op coq_BinNums_Z__canonical__eqtype_Equality len
                   (Obj.magic x0.ai_len)) (is_lvar x))
          then let vi = x.gv.v_info in
               Ok
               (map (fun v -> Pvar (mk_lvar { v_var = v; v_info = vi }))
                 x0.ai_elems)
          else let s = E.reg_error x.gv "(type mismatch)" in Error s
        | Error s -> Error s)
     | Psub (aa, ws', len', x, e0) ->
       if eq_op warray__arr_access__canonical__eqtype_Equality (Obj.magic aa)
            (Obj.magic AAscale)
       then (match o2r (E.reg_error x.gv "(the index is not a constant)")
                     (is_const e0) with
             | Ok x0 ->
               (match o2r (E.reg_error x.gv "(not a reg array)")
                        (Mvar.get m.sarrs (Obj.magic x.gv.v_var)) with
                | Ok x1 ->
                  if (&&)
                       (eq_op wsize_wsize__canonical__eqtype_Equality ws
                         (Obj.magic x1.ai_ty))
                       ((&&)
                         (eq_op wsize_wsize__canonical__eqtype_Equality
                           (Obj.magic ws') ws)
                         ((&&)
                           (eq_op coq_BinNums_Z__canonical__eqtype_Equality
                             len (Obj.magic (Zpos len'))) (is_lvar x)))
                  then let elems =
                         take (Z.to_nat (Obj.magic len))
                           (drop (Z.to_nat x0) x1.ai_elems)
                       in
                       let vi = x.gv.v_info in
                       Ok
                       (map (fun v -> Pvar
                         (mk_lvar { v_var = v; v_info = vi })) elems)
                  else let s = E.reg_error x.gv "(type mismatch)" in Error s
                | Error s -> Error s)
             | Error s -> Error s)
       else let s = E.reg_error x.gv "(the default scale must be used)" in
            Error s
     | _ ->
       Error
         (E.reg_error_expr e
           "Only variables and sub-arrays can be expanded in function arguments."))
  | None -> Result.map (fun x -> x :: []) (expand_e m e)

(** val expand_return :
    t -> (wsize * coq_Z) option -> lval -> (pp_error_loc, lval list) result **)

let expand_return m ex x =
  match ex with
  | Some y ->
    let (ws, len) = y in
    (match x with
     | Lnone (v, _) -> Ok (nseq (Z.to_nat len) (Lnone (v, (Coq_aword ws))))
     | Lvar x0 ->
       (match o2r (E.reg_error x0 "(not a reg array)")
                (Mvar.get m.sarrs (Obj.magic x0.v_var)) with
        | Ok x1 ->
          if (&&)
               (eq_op wsize_wsize__canonical__eqtype_Equality (Obj.magic ws)
                 (Obj.magic x1.ai_ty))
               (eq_op coq_BinNums_Z__canonical__eqtype_Equality
                 (Obj.magic len) (Obj.magic x1.ai_len))
          then let vi = x0.v_info in
               Ok (map (fun v -> Lvar { v_var = v; v_info = vi }) x1.ai_elems)
          else let s = E.reg_error x0 "(type mismatch)" in Error s
        | Error s -> Error s)
     | Lasub (aa, ws', len', x0, e) ->
       if eq_op warray__arr_access__canonical__eqtype_Equality (Obj.magic aa)
            (Obj.magic AAscale)
       then (match o2r (E.reg_error x0 "(the index is not a constant)")
                     (is_const e) with
             | Ok x1 ->
               (match o2r (E.reg_error x0 "(not a reg array)")
                        (Mvar.get m.sarrs (Obj.magic x0.v_var)) with
                | Ok x2 ->
                  if (&&)
                       (eq_op wsize_wsize__canonical__eqtype_Equality
                         (Obj.magic ws) (Obj.magic x2.ai_ty))
                       ((&&)
                         (eq_op wsize_wsize__canonical__eqtype_Equality
                           (Obj.magic ws') (Obj.magic ws))
                         (eq_op coq_BinNums_Z__canonical__eqtype_Equality
                           (Obj.magic len) (Obj.magic (Zpos len'))))
                  then let vi = x0.v_info in
                       let elems =
                         take (Z.to_nat len) (drop (Z.to_nat x1) x2.ai_elems)
                       in
                       Ok
                       (map (fun v -> Lvar { v_var = v; v_info = vi }) elems)
                  else let s = E.reg_error x0 "(type mismatch)" in Error s
                | Error s -> Error s)
             | Error s -> Error s)
       else let s = E.reg_error x0 "(the default scale must be used)" in
            Error s
     | _ ->
       Error
         (E.reg_ierror_no_var
           "only variables/sub-arrays/_ can be expanded in function return"))
  | None -> Result.map (fun x0 -> x0 :: []) (expand_lv m x)

(** val expand_i :
    'a1 asmOp -> expd_t -> t -> 'a1 instr -> 'a1 instr cexec **)

let rec expand_i asmop fsigs m = function
| MkI (ii, ir) ->
  (match ir with
   | Cassgn (x, tag, ty, e) ->
     (match add_iinfo ii (expand_lv m x) with
      | Ok x0 ->
        (match add_iinfo ii (expand_e m e) with
         | Ok x1 -> Ok (MkI (ii, (Cassgn (x0, tag, ty, x1))))
         | Error s -> Error s)
      | Error s -> Error s)
   | Copn (xs, tag, o, es) ->
     (match add_iinfo ii (expand_lvs m xs) with
      | Ok x ->
        (match add_iinfo ii (expand_es m es) with
         | Ok x0 -> Ok (MkI (ii, (Copn (x, tag, o, x0))))
         | Error s -> Error s)
      | Error s -> Error s)
   | Csyscall (xs, o, es) ->
     (match add_iinfo ii (expand_lvs m xs) with
      | Ok x ->
        (match add_iinfo ii (expand_es m es) with
         | Ok x0 -> Ok (MkI (ii, (Csyscall (x, o, x0))))
         | Error s -> Error s)
      | Error s -> Error s)
   | Cif (b, c1, c2) ->
     (match add_iinfo ii (expand_e m b) with
      | Ok x ->
        (match mapM (expand_i asmop fsigs m) c1 with
         | Ok x0 ->
           (match mapM (expand_i asmop fsigs m) c2 with
            | Ok x1 -> Ok (MkI (ii, (Cif (x, x0, x1))))
            | Error s -> Error s)
         | Error s -> Error s)
      | Error s -> Error s)
   | Cfor (x, r, c) ->
     let (p, e2) = r in
     let (dir, e1) = p in
     (match add_iinfo ii
              (if SvExtra.Sv.mem (Obj.magic x.v_var) m.svars
               then Ok ()
               else Error
                      (E.reg_ierror x "reg array as a variable of a for loop")) with
      | Ok _ ->
        (match add_iinfo ii (expand_e m e1) with
         | Ok x0 ->
           (match add_iinfo ii (expand_e m e2) with
            | Ok x1 ->
              (match mapM (expand_i asmop fsigs m) c with
               | Ok x2 -> Ok (MkI (ii, (Cfor (x, ((dir, x0), x1), x2))))
               | Error s -> Error s)
            | Error s -> Error s)
         | Error s -> Error s)
      | Error s -> Error s)
   | Cwhile (a, c, e, info, c') ->
     (match add_iinfo ii (expand_e m e) with
      | Ok x ->
        (match mapM (expand_i asmop fsigs m) c with
         | Ok x0 ->
           (match mapM (expand_i asmop fsigs m) c' with
            | Ok x1 -> Ok (MkI (ii, (Cwhile (a, x0, x, info, x1))))
            | Error s -> Error s)
         | Error s -> Error s)
      | Error s -> Error s)
   | Ccall (xs, fn, es) ->
     (match Mf.get fsigs (Obj.magic fn) with
      | Some p ->
        let (expdin, expdout) = p in
        (match add_iinfo ii
                 (Result.map flatten
                   (mapM2 E.length_mismatch (expand_return m) expdout xs)) with
         | Ok x ->
           (match add_iinfo ii
                    (Result.map flatten
                      (mapM2 E.length_mismatch (Obj.magic expand_param m)
                        expdin es)) with
            | Ok x0 -> Ok (MkI (ii, (Ccall (x, fn, x0))))
            | Error s -> Error s)
         | Error s -> Error s)
      | None -> Error (E.reg_ierror_no_var "function not found")))

(** val expand_tyv :
    t -> bool -> string -> atype -> var_i -> (pp_error_loc, (atype
    list * var_i list) * (wsize * coq_Z) option) result **)

let expand_tyv m b s ty v =
  match Mvar.get m.sarrs (Obj.magic v.v_var) with
  | Some ai ->
    if b
    then let vi = v.v_info in
         let vvars = map (fun v' -> { v_var = v'; v_info = vi }) ai.ai_elems
         in
         let vtypes = map Var.vtype ai.ai_elems in
         Ok ((vtypes, vvars), (Some (ai.ai_ty, ai.ai_len)))
    else let s0 =
           E.reg_error v
             ((^) "(reg arrays are not allowed in "
               ((^) s " of export functions)"))
         in
         Error s0
  | None ->
    if SvExtra.Sv.mem (Obj.magic v.v_var) m.svars
    then Ok (((ty :: []), (v :: [])), None)
    else let s0 =
           E.reg_ierror v
             "there should be an invariant ensuring this never happens in array_expansion_proof"
         in
         Error s0

(** val expand_fsig :
    'a1 asmOp -> (funname -> 'a1 ufundef -> expand_info) -> funname list ->
    funname -> 'a1 ufundef -> (pp_error_loc, (('a1, extra_fun_t)
    _fundef * t) * ((wsize * coq_Z) option list * (wsize * coq_Z) option
    list)) result **)

let expand_fsig _ fi entries fname fd =
  match init_map (fi fname fd) with
  | Ok x ->
    let { f_info = _; f_tyin = tyin; f_params = params; f_body = c; f_tyout =
      tyout; f_res = res; f_extra = ef } = fd
    in
    let (m, fi0) = x in
    let exp =
      negb
        (in_mem (Obj.magic fname)
          (mem (seq_predType funname_eqType) (Obj.magic entries)))
    in
    (match mapM2 E.length_mismatch (expand_tyv m exp "the parameters") tyin
             params with
     | Ok x0 ->
       let tyin0 = map (fun x1 -> fst (fst x1)) x0 in
       let params0 = map (fun x1 -> snd (fst x1)) x0 in
       let ins = map snd x0 in
       (match mapM2 E.length_mismatch (expand_tyv m exp "the return type")
                tyout res with
        | Ok x1 ->
          let tyout0 = map (fun x2 -> fst (fst x2)) x1 in
          let res0 = map (fun x2 -> snd (fst x2)) x1 in
          let outs = map snd x1 in
          Ok (({ f_info = fi0; f_tyin = (flatten tyin0); f_params =
          (flatten params0); f_body = c; f_tyout = (flatten tyout0); f_res =
          (flatten res0); f_extra = ef }, m), (ins, outs))
        | Error s -> Error s)
     | Error s -> Error s)
  | Error s -> Error s

(** val expand_fbody :
    'a1 asmOp -> expd_t -> funname -> ('a1 ufundef * t) -> (pp_error_loc,
    ('a1, extra_fun_t) _fundef) result **)

let expand_fbody asmop fsigs _ = function
| (fd, m) ->
  let { f_info = fi; f_tyin = tyin; f_params = params; f_body = c; f_tyout =
    tyout; f_res = res; f_extra = ef } = fd
  in
  (match mapM (expand_i asmop fsigs m) c with
   | Ok x ->
     Ok { f_info = fi; f_tyin = tyin; f_params = params; f_body = x;
       f_tyout = tyout; f_res = res; f_extra = ef }
   | Error s -> Error s)

(** val expand_prog :
    'a1 asmOp -> (funname -> 'a1 ufundef -> expand_info) -> funname list ->
    'a1 uprog -> 'a1 uprog cexec **)

let expand_prog asmop fi entries p =
  

  match map_cfprog_name_gen (fun x -> x.f_info)
          (expand_fsig asmop fi entries) p.p_funcs with
  | Ok x ->
    let fsigs =
      foldr (fun x0 y -> Mf.set y (fst (Obj.magic x0)) (snd (snd x0)))
        Mf.empty x
    in
    (match map_cfprog_name_gen (fun x0 -> (fst (fst x0)).f_info)
             (fun fn x0 -> expand_fbody asmop fsigs fn (fst x0)) x with
     | Ok x0 -> Ok { p_funcs = x0; p_globs = p.p_globs; p_extra = p.p_extra }
     | Error s -> Error s)
  | Error s -> Error s

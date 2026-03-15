open BinNums
open Datatypes
open Compiler_util
open Eqtype
open Expr
open Pseudo_operator
open Seq
open Sopn
open Ssrfun
open Ssrnat
open Syscall
open Type
open Utils0
open Var0
open Word0
open Word_ssrZ
open Wsize

module E =
 struct
  (** val pass : string **)

  let pass =
    "wint_to_int"

  (** val ierror_s : string -> pp_error_loc **)

  let ierror_s =
    pp_internal_error_s pass

  (** val ierror : pp_error -> pp_error_loc **)

  let ierror =
    pp_internal_error pass

  (** val ierror_e : pexpr -> pp_error_loc **)

  let ierror_e e =
    ierror
      (pp_nobox ((PPEstring "ill typed expression ") :: ((PPEexpr e) :: [])))

  (** val ierror_lv : lval -> pp_error_loc **)

  let ierror_lv lv =
    ierror
      (pp_nobox ((PPEstring "ill typed left value ") :: ((PPElval lv) :: [])))
 end

(** val is_wi1 : sop1 -> (signedness * wiop1) option **)

let is_wi1 = function
| Owi1 (s, op) -> Some (s, op)
| _ -> None

(** val is_wi2 : sop2 -> (signedness * wiop2) option **)

let is_wi2 = function
| Owi2 (s, _, op) -> Some (s, op)
| _ -> None

(** val wi2i_op2 : sop2 -> sop2 **)

let wi2i_op2 o =
  match is_wi2 o with
  | Some p ->
    let (s, op) = p in
    (match op with
     | WIadd -> Oadd Op_int
     | WImul -> Omul Op_int
     | WIsub -> Osub Op_int
     | WIdiv -> Odiv (s, Op_int)
     | WImod -> Omod (s, Op_int)
     | WIshl -> Olsl Op_int
     | WIshr -> Oasr Op_int
     | WIeq -> Oeq Op_int
     | WIneq -> Oneq Op_int
     | WIlt -> Olt Cmp_int
     | WIle -> Ole Cmp_int
     | WIgt -> Ogt Cmp_int
     | WIge -> Oge Cmp_int)
  | None -> o

(** val esubtype :
    positive extended_type -> positive extended_type -> bool **)

let esubtype ty1 ty2 =
  match ty1 with
  | ETbool -> (match ty2 with
               | ETbool -> true
               | _ -> false)
  | ETint -> (match ty2 with
              | ETint -> true
              | _ -> false)
  | ETarr (ws, l) ->
    (match ty2 with
     | ETarr (ws', l') ->
       eq_op coq_BinNums_Z__canonical__eqtype_Equality
         (Obj.magic arr_size ws l) (Obj.magic arr_size ws' l')
     | _ -> false)
  | ETword (o, w) ->
    (match o with
     | Some sg ->
       (match ty2 with
        | ETword (o0, w') ->
          (match o0 with
           | Some sg' ->
             (&&)
               (eq_op wsize_signedness__canonical__eqtype_Equality
                 (Obj.magic sg) (Obj.magic sg'))
               (eq_op wsize_wsize__canonical__eqtype_Equality (Obj.magic w)
                 (Obj.magic w'))
           | None -> false)
        | _ -> false)
     | None ->
       (match ty2 with
        | ETword (o0, w') ->
          (match o0 with
           | Some _ -> false
           | None -> cmp_le wsize_cmp w w')
        | _ -> false))

(** val wi2i_op1_e : sop1 -> pexpr -> pexpr **)

let wi2i_op1_e o e =
  match is_wi1 o with
  | Some p ->
    let (s, o0) = p in
    (match o0 with
     | WIword_of_wint ws -> Papp1 ((Owi1 (s, (WIwint_of_int ws))), e)
     | WIwint_of_word ws -> Papp1 ((Oint_of_word (s, ws)), e)
     | WIwint_ext (szo, szi) ->
       if cmp_le wsize_cmp szi szo
       then e
       else Papp1 ((Oint_of_word (s, szo)), (Papp1
              ((signed (Ozeroext (szo, szi)) (Osignext (szo, szi)) s), (Papp1
              ((Oword_of_int szi), e)))))
     | WIneg _ -> Papp1 ((Oneg Op_int), e)
     | _ -> e)
  | None -> Papp1 (o, e)

(** val wi2i_op2_e : sop2 -> pexpr -> pexpr -> pexpr **)

let wi2i_op2_e o e1 e2 =
  let o0 = wi2i_op2 o in Papp2 (o0, e1, e2)

(** val to_etype : signedness option -> atype -> positive extended_type **)

let to_etype sg = function
| Coq_abool -> tbool
| Coq_aint -> tint
| Coq_aarr (ws, l) -> tarr ws l
| Coq_aword ws -> ETword (sg, ws)

(** val sign_of_var :
    (Var.var -> (signedness * Var.var) option) -> Var.var -> signedness option **)

let sign_of_var m x =
  Ssrfun.Option.map fst (m x)

(** val etype_of_var :
    (Var.var -> (signedness * Var.var) option) -> Var.var -> positive
    extended_type **)

let etype_of_var m x =
  to_etype (sign_of_var m x) (Var.vtype x)

(** val sign_of_gvar :
    (Var.var -> (signedness * Var.var) option) -> gvar -> signedness option **)

let sign_of_gvar m x =
  if is_lvar x then sign_of_var m x.gv.v_var else None

(** val etype_of_gvar :
    (Var.var -> (signedness * Var.var) option) -> gvar -> positive
    extended_type **)

let etype_of_gvar m x =
  to_etype (sign_of_gvar m x) (Var.vtype x.gv.v_var)

(** val sign_of_etype : positive extended_type -> signedness option **)

let sign_of_etype = function
| ETword (o, _) -> o
| _ -> None

(** val etype_of_expr :
    (Var.var -> (signedness * Var.var) option) -> pexpr -> positive
    extended_type **)

let rec etype_of_expr m = function
| Pconst _ -> tint
| Pbool _ -> tbool
| Parr_init (ws, len) -> tarr ws len
| Pvar x -> etype_of_gvar m x
| Pget (_, _, ws, _, _) -> tword ws
| Psub (_, ws, len, _, _) -> tarr ws len
| Pload (_, ws, _) -> tword ws
| Papp1 (o, _) -> snd (etype_of_op1 o)
| Papp2 (o, _, _) -> snd (etype_of_op2 o)
| PappN (o, _) -> to_etype None (snd (type_of_opN o))
| Pif (ty, _, e2, _) -> to_etype (sign_of_etype (etype_of_expr m e2)) ty

(** val sign_of_expr :
    (Var.var -> (signedness * Var.var) option) -> pexpr -> signedness option **)

let sign_of_expr m e =
  sign_of_etype (etype_of_expr m e)

(** val wi2i_var :
    (Var.var -> (signedness * Var.var) option) -> Var.var -> Var.var **)

let wi2i_var m x =
  match m x with
  | Some p -> let (_, xi) = p in xi
  | None -> x

(** val in_FV_var : SvExtra.Sv.t -> Var.var -> bool **)

let in_FV_var fV x =
  SvExtra.Sv.mem (Obj.magic x) fV

(** val wi2i_vari :
    (Var.var -> (signedness * Var.var) option) -> SvExtra.Sv.t -> var_i ->
    (pp_error_loc, var_i) result **)

let wi2i_vari m fV x =
  if in_FV_var fV x.v_var
  then Ok { v_var = (wi2i_var m x.v_var); v_info = x.v_info }
  else let s = E.ierror_e (coq_Plvar x) in Error s

(** val wi2i_gvar :
    (Var.var -> (signedness * Var.var) option) -> SvExtra.Sv.t -> gvar ->
    (pp_error_loc, gvar) result **)

let wi2i_gvar m fV x =
  if is_lvar x
  then (match wi2i_vari m fV x.gv with
        | Ok x0 -> Ok (mk_lvar x0)
        | Error s -> Error s)
  else Ok x

(** val wi2i_type : signedness option -> atype -> atype **)

let wi2i_type sg ty =
  if eq_op
       (coq_Datatypes_option__canonical__eqtype_Equality
         wsize_signedness__canonical__eqtype_Equality) (Obj.magic sg)
       (Obj.magic None)
  then ty
  else Coq_aint

(** val wi2i_e :
    (Var.var -> (signedness * Var.var) option) -> SvExtra.Sv.t -> pexpr ->
    pexpr cexec **)

let rec wi2i_e m fV e0 = match e0 with
| Pvar x ->
  (match wi2i_gvar m fV x with
   | Ok x0 -> Ok (Pvar x0)
   | Error s -> Error s)
| Pget (al, aa, ws, x, e) ->
  (match wi2i_gvar m fV x with
   | Ok x0 ->
     (match wi2i_e m fV e with
      | Ok x1 -> Ok (Pget (al, aa, ws, x0, x1))
      | Error s -> Error s)
   | Error s -> Error s)
| Psub (al, ws, len, x, e) ->
  (match wi2i_gvar m fV x with
   | Ok x0 ->
     (match wi2i_e m fV e with
      | Ok x1 -> Ok (Psub (al, ws, len, x0, x1))
      | Error s -> Error s)
   | Error s -> Error s)
| Pload (al, ws, e) ->
  if eq_op
       (coq_Datatypes_option__canonical__eqtype_Equality
         wsize_signedness__canonical__eqtype_Equality)
       (Obj.magic sign_of_expr m e) (Obj.magic None)
  then (match wi2i_e m fV e with
        | Ok x -> Ok (Pload (al, ws, x))
        | Error s -> Error s)
  else let s = E.ierror_e e0 in Error s
| Papp1 (o, e) ->
  if esubtype (fst (etype_of_op1 o)) (etype_of_expr m e)
  then (match wi2i_e m fV e with
        | Ok x -> Ok (wi2i_op1_e o x)
        | Error s -> Error s)
  else let s = E.ierror_e e0 in Error s
| Papp2 (o, e1, e2) ->
  let ty = etype_of_op2 o in
  if (&&) (esubtype (fst (fst ty)) (etype_of_expr m e1))
       (esubtype (snd (fst ty)) (etype_of_expr m e2))
  then (match wi2i_e m fV e1 with
        | Ok x ->
          (match wi2i_e m fV e2 with
           | Ok x0 -> Ok (wi2i_op2_e o x x0)
           | Error s -> Error s)
        | Error s -> Error s)
  else let s = E.ierror_e e0 in Error s
| PappN (o, es) ->
  if all (fun e ->
       eq_op
         (coq_Datatypes_option__canonical__eqtype_Equality
           wsize_signedness__canonical__eqtype_Equality)
         (Obj.magic sign_of_expr m e) (Obj.magic None)) es
  then (match mapM (wi2i_e m fV) es with
        | Ok x -> Ok (PappN (o, x))
        | Error s -> Error s)
  else let s = E.ierror_e e0 in Error s
| Pif (ty, e1, e2, e3) ->
  let ety = etype_of_expr m e0 in
  if (&&) (esubtype ety (etype_of_expr m e2))
       (esubtype ety (etype_of_expr m e3))
  then let ty0 = wi2i_type (sign_of_expr m e2) ty in
       (match wi2i_e m fV e1 with
        | Ok x ->
          (match wi2i_e m fV e2 with
           | Ok x0 ->
             (match wi2i_e m fV e3 with
              | Ok x1 -> Ok (Pif (ty0, x, x0, x1))
              | Error s -> Error s)
           | Error s -> Error s)
        | Error s -> Error s)
  else let s = E.ierror_e e0 in Error s
| _ -> Ok e0

(** val wi2i_lvar :
    (Var.var -> (signedness * Var.var) option) -> SvExtra.Sv.t -> positive
    extended_type -> var_i -> var_i cexec **)

let wi2i_lvar m fV ety x =
  if esubtype (etype_of_var m x.v_var) ety
  then wi2i_vari m fV x
  else let s = E.ierror_lv (Lvar x) in Error s

(** val wi2i_lv :
    (Var.var -> (signedness * Var.var) option) -> SvExtra.Sv.t -> positive
    extended_type -> lval -> lval cexec **)

let wi2i_lv m fV ety lv =
  let s = sign_of_etype ety in
  (match lv with
   | Lnone (vi, ty) -> Ok (Lnone (vi, (wi2i_type s ty)))
   | Lvar x ->
     (match wi2i_lvar m fV ety x with
      | Ok x0 -> Ok (Lvar x0)
      | Error s0 -> Error s0)
   | Lmem (al, ws, vi, e) ->
     if (&&)
          (eq_op
            (coq_Datatypes_option__canonical__eqtype_Equality
              wsize_signedness__canonical__eqtype_Equality)
            (Obj.magic sign_of_expr m e) (Obj.magic None))
          (eq_op
            (coq_Datatypes_option__canonical__eqtype_Equality
              wsize_signedness__canonical__eqtype_Equality) (Obj.magic s)
            (Obj.magic None))
     then (match wi2i_e m fV e with
           | Ok x -> Ok (Lmem (al, ws, vi, x))
           | Error s0 -> Error s0)
     else let s0 = E.ierror_lv lv in Error s0
   | Laset (al, aa, ws, x, e) ->
     if (&&) (in_FV_var fV x.v_var)
          ((&&)
            (eq_op
              (coq_Datatypes_option__canonical__eqtype_Equality
                wsize_signedness__canonical__eqtype_Equality)
              (Obj.magic sign_of_expr m e) (Obj.magic None))
            (eq_op
              (coq_Datatypes_option__canonical__eqtype_Equality
                wsize_signedness__canonical__eqtype_Equality) (Obj.magic s)
              (Obj.magic None)))
     then (match wi2i_e m fV e with
           | Ok x0 -> Ok (Laset (al, aa, ws, x, x0))
           | Error s0 -> Error s0)
     else let s0 = E.ierror_lv lv in Error s0
   | Lasub (aa, ws, len, x, e) ->
     if (&&) (in_FV_var fV x.v_var)
          ((&&)
            (eq_op
              (coq_Datatypes_option__canonical__eqtype_Equality
                wsize_signedness__canonical__eqtype_Equality)
              (Obj.magic sign_of_expr m e) (Obj.magic None))
            (eq_op
              (coq_Datatypes_option__canonical__eqtype_Equality
                wsize_signedness__canonical__eqtype_Equality) (Obj.magic s)
              (Obj.magic None)))
     then (match wi2i_e m fV e with
           | Ok x0 -> Ok (Lasub (aa, ws, len, x, x0))
           | Error s0 -> Error s0)
     else let s0 = E.ierror_lv lv in Error s0)

(** val get_sig :
    (funname -> (positive extended_type list * positive extended_type list)
    option) -> funname -> (pp_error_loc, positive extended_type
    list * positive extended_type list) result **)

let get_sig sigs f =
  match sigs f with
  | Some sig0 -> Ok sig0
  | None -> Error (E.ierror_s "unknown function")

(** val wi2i_ir :
    'a1 asmOp -> coq_PointerData -> coq_MSFsize -> (Var.var ->
    (signedness * Var.var) option) -> SvExtra.Sv.t -> (funname -> (positive
    extended_type list * positive extended_type list) option) -> 'a1 instr_r
    -> 'a1 instr_r cexec **)

let wi2i_ir asmop pd msfsz m fV sigs =
  let rec wi2i_ir0 = function
  | Cassgn (x, tag, ty, e) ->
    let ety = etype_of_expr m e in
    let sg = sign_of_etype ety in
    let tyr = to_etype sg ty in
    if esubtype tyr ety
    then (match wi2i_lv m fV tyr x with
          | Ok x0 ->
            (match wi2i_e m fV e with
             | Ok x1 ->
               let ty0 = wi2i_type sg ty in Ok (Cassgn (x0, tag, ty0, x1))
             | Error s -> Error s)
          | Error s -> Error s)
    else let s = E.ierror_s "invalid type in assigned" in Error s
  | Copn (xs, t0, o, es) ->
    (match o with
     | Opseudo_op p ->
       (match p with
        | Ospill (k, tys) ->
          let etys = map (etype_of_expr m) es in
          let tys' =
            map2 (fun ety ty -> to_etype (sign_of_etype ety) ty) etys tys
          in
          if eq_op coq_Datatypes_nat__canonical__eqtype_Equality
               (Obj.magic size tys) (Obj.magic size es)
          then if all2 esubtype tys' etys
               then let tys0 =
                      map2 (fun ty e -> wi2i_type (sign_of_expr m e) ty) tys
                        es
                    in
                    (match mapM (wi2i_e m fV) es with
                     | Ok x ->
                       Ok (Copn ([], t0, (Opseudo_op (Ospill (k, tys0))), x))
                     | Error s -> Error s)
               else let s = E.ierror_s "ill typed spill (arguments)" in
                    Error s
          else let s = E.ierror_s "ill typed spill" in Error s
        | Ocopy (_, _) ->
          if all (fun e ->
               eq_op
                 (coq_Datatypes_option__canonical__eqtype_Equality
                   wsize_signedness__canonical__eqtype_Equality)
                 (Obj.magic sign_of_expr m e) (Obj.magic None)) es
          then (match mapM (wi2i_e m fV) es with
                | Ok x ->
                  let xtys = map (to_etype None) (sopn_tout pd msfsz asmop o)
                  in
                  (match mapM2 (E.ierror_s "invalid dest in Copn")
                           (wi2i_lv m fV) xtys xs with
                   | Ok x0 -> Ok (Copn (x0, t0, o, x))
                   | Error s -> Error s)
                | Error s -> Error s)
          else let s = E.ierror_s "invalid expr in Copn" in Error s
        | Odeclassify ty ->
          (match es with
           | [] -> Error (E.ierror_s "ill-typed declassify")
           | e :: l ->
             (match l with
              | [] ->
                (match wi2i_e m fV e with
                 | Ok x ->
                   let ty0 = wi2i_type (sign_of_expr m x) ty in
                   Ok (Copn ([], t0, (Opseudo_op (Odeclassify ty0)),
                   (x :: [])))
                 | Error s -> Error s)
              | _ :: _ -> Error (E.ierror_s "ill-typed declassify")))
        | Odeclassify_mem _ ->
          if all (fun e ->
               eq_op
                 (coq_Datatypes_option__canonical__eqtype_Equality
                   wsize_signedness__canonical__eqtype_Equality)
                 (Obj.magic sign_of_expr m e) (Obj.magic None)) es
          then (match mapM (wi2i_e m fV) es with
                | Ok x ->
                  let xtys = map (to_etype None) (sopn_tout pd msfsz asmop o)
                  in
                  (match mapM2 (E.ierror_s "invalid dest in Copn")
                           (wi2i_lv m fV) xtys xs with
                   | Ok x0 -> Ok (Copn (x0, t0, o, x))
                   | Error s -> Error s)
                | Error s -> Error s)
          else let s = E.ierror_s "invalid expr in Copn" in Error s
        | Onop ->
          if all (fun e ->
               eq_op
                 (coq_Datatypes_option__canonical__eqtype_Equality
                   wsize_signedness__canonical__eqtype_Equality)
                 (Obj.magic sign_of_expr m e) (Obj.magic None)) es
          then (match mapM (wi2i_e m fV) es with
                | Ok x ->
                  let xtys = map (to_etype None) (sopn_tout pd msfsz asmop o)
                  in
                  (match mapM2 (E.ierror_s "invalid dest in Copn")
                           (wi2i_lv m fV) xtys xs with
                   | Ok x0 -> Ok (Copn (x0, t0, o, x))
                   | Error s -> Error s)
                | Error s -> Error s)
          else let s = E.ierror_s "invalid expr in Copn" in Error s
        | Omulu _ ->
          if all (fun e ->
               eq_op
                 (coq_Datatypes_option__canonical__eqtype_Equality
                   wsize_signedness__canonical__eqtype_Equality)
                 (Obj.magic sign_of_expr m e) (Obj.magic None)) es
          then (match mapM (wi2i_e m fV) es with
                | Ok x ->
                  let xtys = map (to_etype None) (sopn_tout pd msfsz asmop o)
                  in
                  (match mapM2 (E.ierror_s "invalid dest in Copn")
                           (wi2i_lv m fV) xtys xs with
                   | Ok x0 -> Ok (Copn (x0, t0, o, x))
                   | Error s -> Error s)
                | Error s -> Error s)
          else let s = E.ierror_s "invalid expr in Copn" in Error s
        | Oaddcarry _ ->
          if all (fun e ->
               eq_op
                 (coq_Datatypes_option__canonical__eqtype_Equality
                   wsize_signedness__canonical__eqtype_Equality)
                 (Obj.magic sign_of_expr m e) (Obj.magic None)) es
          then (match mapM (wi2i_e m fV) es with
                | Ok x ->
                  let xtys = map (to_etype None) (sopn_tout pd msfsz asmop o)
                  in
                  (match mapM2 (E.ierror_s "invalid dest in Copn")
                           (wi2i_lv m fV) xtys xs with
                   | Ok x0 -> Ok (Copn (x0, t0, o, x))
                   | Error s -> Error s)
                | Error s -> Error s)
          else let s = E.ierror_s "invalid expr in Copn" in Error s
        | Osubcarry _ ->
          if all (fun e ->
               eq_op
                 (coq_Datatypes_option__canonical__eqtype_Equality
                   wsize_signedness__canonical__eqtype_Equality)
                 (Obj.magic sign_of_expr m e) (Obj.magic None)) es
          then (match mapM (wi2i_e m fV) es with
                | Ok x ->
                  let xtys = map (to_etype None) (sopn_tout pd msfsz asmop o)
                  in
                  (match mapM2 (E.ierror_s "invalid dest in Copn")
                           (wi2i_lv m fV) xtys xs with
                   | Ok x0 -> Ok (Copn (x0, t0, o, x))
                   | Error s -> Error s)
                | Error s -> Error s)
          else let s = E.ierror_s "invalid expr in Copn" in Error s
        | Oswap ty ->
          (match es with
           | [] -> Error (E.ierror_s "ill-typed swap")
           | e1 :: l ->
             (match l with
              | [] -> Error (E.ierror_s "ill-typed swap")
              | _ :: l0 ->
                (match l0 with
                 | [] ->
                   let ety = etype_of_expr m e1 in
                   let sig0 = ety :: (ety :: []) in
                   if all2 (fun ety0 e -> esubtype ety0 (etype_of_expr m e))
                        sig0 es
                   then (match mapM (wi2i_e m fV) es with
                         | Ok x ->
                           (match mapM2 (E.ierror_s "invalid dest in swap")
                                    (wi2i_lv m fV) sig0 xs with
                            | Ok x0 ->
                              let ty0 = wi2i_type (sign_of_expr m e1) ty in
                              Ok (Copn (x0, t0, (Opseudo_op (Oswap ty0)), x))
                            | Error s -> Error s)
                         | Error s -> Error s)
                   else let s = E.ierror_s "invalid args in swap" in Error s
                 | _ :: _ -> Error (E.ierror_s "ill-typed swap")))))
     | Oslh _ ->
       if all (fun e ->
            eq_op
              (coq_Datatypes_option__canonical__eqtype_Equality
                wsize_signedness__canonical__eqtype_Equality)
              (Obj.magic sign_of_expr m e) (Obj.magic None)) es
       then (match mapM (wi2i_e m fV) es with
             | Ok x ->
               let xtys = map (to_etype None) (sopn_tout pd msfsz asmop o) in
               (match mapM2 (E.ierror_s "invalid dest in Copn")
                        (wi2i_lv m fV) xtys xs with
                | Ok x0 -> Ok (Copn (x0, t0, o, x))
                | Error s -> Error s)
             | Error s -> Error s)
       else let s = E.ierror_s "invalid expr in Copn" in Error s
     | Oasm _ ->
       if all (fun e ->
            eq_op
              (coq_Datatypes_option__canonical__eqtype_Equality
                wsize_signedness__canonical__eqtype_Equality)
              (Obj.magic sign_of_expr m e) (Obj.magic None)) es
       then (match mapM (wi2i_e m fV) es with
             | Ok x ->
               let xtys = map (to_etype None) (sopn_tout pd msfsz asmop o) in
               (match mapM2 (E.ierror_s "invalid dest in Copn")
                        (wi2i_lv m fV) xtys xs with
                | Ok x0 -> Ok (Copn (x0, t0, o, x))
                | Error s -> Error s)
             | Error s -> Error s)
       else let s = E.ierror_s "invalid expr in Copn" in Error s)
  | Csyscall (xs, o, es) ->
    if all (fun e ->
         eq_op
           (coq_Datatypes_option__canonical__eqtype_Equality
             wsize_signedness__canonical__eqtype_Equality)
           (Obj.magic sign_of_expr m e) (Obj.magic None)) es
    then (match mapM (wi2i_e m fV) es with
          | Ok x ->
            let xtys = map (to_etype None) (syscall_sig_u o).scs_tout in
            (match mapM2 (E.ierror_s "invalid dest in Csyscall")
                     (wi2i_lv m fV) xtys xs with
             | Ok x0 -> Ok (Csyscall (x0, o, x))
             | Error s -> Error s)
          | Error s -> Error s)
    else let s = E.ierror_s "invalid args in Csyscall" in Error s
  | Cif (b, c1, c2) ->
    (match wi2i_e m fV b with
     | Ok x ->
       (match mapM wi2i_i0 c1 with
        | Ok x0 ->
          (match mapM wi2i_i0 c2 with
           | Ok x1 -> Ok (Cif (x, x0, x1))
           | Error s -> Error s)
        | Error s -> Error s)
     | Error s -> Error s)
  | Cfor (x, r, c) ->
    let (p, e2) = r in
    let (dir, e1) = p in
    if in_FV_var fV x.v_var
    then (match wi2i_e m fV e1 with
          | Ok x0 ->
            (match wi2i_e m fV e2 with
             | Ok x1 ->
               (match mapM wi2i_i0 c with
                | Ok x2 -> Ok (Cfor (x, ((dir, x0), x1), x2))
                | Error s -> Error s)
             | Error s -> Error s)
          | Error s -> Error s)
    else let s = E.ierror_s "invalid loop counter" in Error s
  | Cwhile (a, c, e, info, c') ->
    (match wi2i_e m fV e with
     | Ok x ->
       (match mapM wi2i_i0 c with
        | Ok x0 ->
          (match mapM wi2i_i0 c' with
           | Ok x1 -> Ok (Cwhile (a, x0, x, info, x1))
           | Error s -> Error s)
        | Error s -> Error s)
     | Error s -> Error s)
  | Ccall (xs, f, es) ->
    (match get_sig sigs f with
     | Ok x ->
       if all2 (fun ety e -> esubtype ety (etype_of_expr m e)) (fst x) es
       then (match mapM2 (E.ierror_s "bad xs length in Ccall") (wi2i_lv m fV)
                     (snd x) xs with
             | Ok x0 ->
               (match mapM (wi2i_e m fV) es with
                | Ok x1 -> Ok (Ccall (x0, f, x1))
                | Error s -> Error s)
             | Error s -> Error s)
       else let s = E.ierror_s "invalid args in Ccall" in Error s
     | Error s -> Error s)
  and wi2i_i0 = function
  | MkI (ii, ir) ->
    (match add_iinfo ii (wi2i_ir0 ir) with
     | Ok x -> Ok (MkI (ii, x))
     | Error s -> Error s)
  in wi2i_ir0

(** val wi2i_i :
    'a1 asmOp -> coq_PointerData -> coq_MSFsize -> (Var.var ->
    (signedness * Var.var) option) -> SvExtra.Sv.t -> (funname -> (positive
    extended_type list * positive extended_type list) option) -> 'a1 instr ->
    'a1 instr cexec **)

let wi2i_i asmop pd msfsz m fV sigs =
  let rec wi2i_ir0 = function
  | Cassgn (x, tag, ty, e) ->
    let ety = etype_of_expr m e in
    let sg = sign_of_etype ety in
    let tyr = to_etype sg ty in
    if esubtype tyr ety
    then (match wi2i_lv m fV tyr x with
          | Ok x0 ->
            (match wi2i_e m fV e with
             | Ok x1 ->
               let ty0 = wi2i_type sg ty in Ok (Cassgn (x0, tag, ty0, x1))
             | Error s -> Error s)
          | Error s -> Error s)
    else let s = E.ierror_s "invalid type in assigned" in Error s
  | Copn (xs, t0, o, es) ->
    (match o with
     | Opseudo_op p ->
       (match p with
        | Ospill (k, tys) ->
          let etys = map (etype_of_expr m) es in
          let tys' =
            map2 (fun ety ty -> to_etype (sign_of_etype ety) ty) etys tys
          in
          if eq_op coq_Datatypes_nat__canonical__eqtype_Equality
               (Obj.magic size tys) (Obj.magic size es)
          then if all2 esubtype tys' etys
               then let tys0 =
                      map2 (fun ty e -> wi2i_type (sign_of_expr m e) ty) tys
                        es
                    in
                    (match mapM (wi2i_e m fV) es with
                     | Ok x ->
                       Ok (Copn ([], t0, (Opseudo_op (Ospill (k, tys0))), x))
                     | Error s -> Error s)
               else let s = E.ierror_s "ill typed spill (arguments)" in
                    Error s
          else let s = E.ierror_s "ill typed spill" in Error s
        | Ocopy (_, _) ->
          if all (fun e ->
               eq_op
                 (coq_Datatypes_option__canonical__eqtype_Equality
                   wsize_signedness__canonical__eqtype_Equality)
                 (Obj.magic sign_of_expr m e) (Obj.magic None)) es
          then (match mapM (wi2i_e m fV) es with
                | Ok x ->
                  let xtys = map (to_etype None) (sopn_tout pd msfsz asmop o)
                  in
                  (match mapM2 (E.ierror_s "invalid dest in Copn")
                           (wi2i_lv m fV) xtys xs with
                   | Ok x0 -> Ok (Copn (x0, t0, o, x))
                   | Error s -> Error s)
                | Error s -> Error s)
          else let s = E.ierror_s "invalid expr in Copn" in Error s
        | Odeclassify ty ->
          (match es with
           | [] -> Error (E.ierror_s "ill-typed declassify")
           | e :: l ->
             (match l with
              | [] ->
                (match wi2i_e m fV e with
                 | Ok x ->
                   let ty0 = wi2i_type (sign_of_expr m x) ty in
                   Ok (Copn ([], t0, (Opseudo_op (Odeclassify ty0)),
                   (x :: [])))
                 | Error s -> Error s)
              | _ :: _ -> Error (E.ierror_s "ill-typed declassify")))
        | Odeclassify_mem _ ->
          if all (fun e ->
               eq_op
                 (coq_Datatypes_option__canonical__eqtype_Equality
                   wsize_signedness__canonical__eqtype_Equality)
                 (Obj.magic sign_of_expr m e) (Obj.magic None)) es
          then (match mapM (wi2i_e m fV) es with
                | Ok x ->
                  let xtys = map (to_etype None) (sopn_tout pd msfsz asmop o)
                  in
                  (match mapM2 (E.ierror_s "invalid dest in Copn")
                           (wi2i_lv m fV) xtys xs with
                   | Ok x0 -> Ok (Copn (x0, t0, o, x))
                   | Error s -> Error s)
                | Error s -> Error s)
          else let s = E.ierror_s "invalid expr in Copn" in Error s
        | Onop ->
          if all (fun e ->
               eq_op
                 (coq_Datatypes_option__canonical__eqtype_Equality
                   wsize_signedness__canonical__eqtype_Equality)
                 (Obj.magic sign_of_expr m e) (Obj.magic None)) es
          then (match mapM (wi2i_e m fV) es with
                | Ok x ->
                  let xtys = map (to_etype None) (sopn_tout pd msfsz asmop o)
                  in
                  (match mapM2 (E.ierror_s "invalid dest in Copn")
                           (wi2i_lv m fV) xtys xs with
                   | Ok x0 -> Ok (Copn (x0, t0, o, x))
                   | Error s -> Error s)
                | Error s -> Error s)
          else let s = E.ierror_s "invalid expr in Copn" in Error s
        | Omulu _ ->
          if all (fun e ->
               eq_op
                 (coq_Datatypes_option__canonical__eqtype_Equality
                   wsize_signedness__canonical__eqtype_Equality)
                 (Obj.magic sign_of_expr m e) (Obj.magic None)) es
          then (match mapM (wi2i_e m fV) es with
                | Ok x ->
                  let xtys = map (to_etype None) (sopn_tout pd msfsz asmop o)
                  in
                  (match mapM2 (E.ierror_s "invalid dest in Copn")
                           (wi2i_lv m fV) xtys xs with
                   | Ok x0 -> Ok (Copn (x0, t0, o, x))
                   | Error s -> Error s)
                | Error s -> Error s)
          else let s = E.ierror_s "invalid expr in Copn" in Error s
        | Oaddcarry _ ->
          if all (fun e ->
               eq_op
                 (coq_Datatypes_option__canonical__eqtype_Equality
                   wsize_signedness__canonical__eqtype_Equality)
                 (Obj.magic sign_of_expr m e) (Obj.magic None)) es
          then (match mapM (wi2i_e m fV) es with
                | Ok x ->
                  let xtys = map (to_etype None) (sopn_tout pd msfsz asmop o)
                  in
                  (match mapM2 (E.ierror_s "invalid dest in Copn")
                           (wi2i_lv m fV) xtys xs with
                   | Ok x0 -> Ok (Copn (x0, t0, o, x))
                   | Error s -> Error s)
                | Error s -> Error s)
          else let s = E.ierror_s "invalid expr in Copn" in Error s
        | Osubcarry _ ->
          if all (fun e ->
               eq_op
                 (coq_Datatypes_option__canonical__eqtype_Equality
                   wsize_signedness__canonical__eqtype_Equality)
                 (Obj.magic sign_of_expr m e) (Obj.magic None)) es
          then (match mapM (wi2i_e m fV) es with
                | Ok x ->
                  let xtys = map (to_etype None) (sopn_tout pd msfsz asmop o)
                  in
                  (match mapM2 (E.ierror_s "invalid dest in Copn")
                           (wi2i_lv m fV) xtys xs with
                   | Ok x0 -> Ok (Copn (x0, t0, o, x))
                   | Error s -> Error s)
                | Error s -> Error s)
          else let s = E.ierror_s "invalid expr in Copn" in Error s
        | Oswap ty ->
          (match es with
           | [] -> Error (E.ierror_s "ill-typed swap")
           | e1 :: l ->
             (match l with
              | [] -> Error (E.ierror_s "ill-typed swap")
              | _ :: l0 ->
                (match l0 with
                 | [] ->
                   let ety = etype_of_expr m e1 in
                   let sig0 = ety :: (ety :: []) in
                   if all2 (fun ety0 e -> esubtype ety0 (etype_of_expr m e))
                        sig0 es
                   then (match mapM (wi2i_e m fV) es with
                         | Ok x ->
                           (match mapM2 (E.ierror_s "invalid dest in swap")
                                    (wi2i_lv m fV) sig0 xs with
                            | Ok x0 ->
                              let ty0 = wi2i_type (sign_of_expr m e1) ty in
                              Ok (Copn (x0, t0, (Opseudo_op (Oswap ty0)), x))
                            | Error s -> Error s)
                         | Error s -> Error s)
                   else let s = E.ierror_s "invalid args in swap" in Error s
                 | _ :: _ -> Error (E.ierror_s "ill-typed swap")))))
     | Oslh _ ->
       if all (fun e ->
            eq_op
              (coq_Datatypes_option__canonical__eqtype_Equality
                wsize_signedness__canonical__eqtype_Equality)
              (Obj.magic sign_of_expr m e) (Obj.magic None)) es
       then (match mapM (wi2i_e m fV) es with
             | Ok x ->
               let xtys = map (to_etype None) (sopn_tout pd msfsz asmop o) in
               (match mapM2 (E.ierror_s "invalid dest in Copn")
                        (wi2i_lv m fV) xtys xs with
                | Ok x0 -> Ok (Copn (x0, t0, o, x))
                | Error s -> Error s)
             | Error s -> Error s)
       else let s = E.ierror_s "invalid expr in Copn" in Error s
     | Oasm _ ->
       if all (fun e ->
            eq_op
              (coq_Datatypes_option__canonical__eqtype_Equality
                wsize_signedness__canonical__eqtype_Equality)
              (Obj.magic sign_of_expr m e) (Obj.magic None)) es
       then (match mapM (wi2i_e m fV) es with
             | Ok x ->
               let xtys = map (to_etype None) (sopn_tout pd msfsz asmop o) in
               (match mapM2 (E.ierror_s "invalid dest in Copn")
                        (wi2i_lv m fV) xtys xs with
                | Ok x0 -> Ok (Copn (x0, t0, o, x))
                | Error s -> Error s)
             | Error s -> Error s)
       else let s = E.ierror_s "invalid expr in Copn" in Error s)
  | Csyscall (xs, o, es) ->
    if all (fun e ->
         eq_op
           (coq_Datatypes_option__canonical__eqtype_Equality
             wsize_signedness__canonical__eqtype_Equality)
           (Obj.magic sign_of_expr m e) (Obj.magic None)) es
    then (match mapM (wi2i_e m fV) es with
          | Ok x ->
            let xtys = map (to_etype None) (syscall_sig_u o).scs_tout in
            (match mapM2 (E.ierror_s "invalid dest in Csyscall")
                     (wi2i_lv m fV) xtys xs with
             | Ok x0 -> Ok (Csyscall (x0, o, x))
             | Error s -> Error s)
          | Error s -> Error s)
    else let s = E.ierror_s "invalid args in Csyscall" in Error s
  | Cif (b, c1, c2) ->
    (match wi2i_e m fV b with
     | Ok x ->
       (match mapM wi2i_i0 c1 with
        | Ok x0 ->
          (match mapM wi2i_i0 c2 with
           | Ok x1 -> Ok (Cif (x, x0, x1))
           | Error s -> Error s)
        | Error s -> Error s)
     | Error s -> Error s)
  | Cfor (x, r, c) ->
    let (p, e2) = r in
    let (dir, e1) = p in
    if in_FV_var fV x.v_var
    then (match wi2i_e m fV e1 with
          | Ok x0 ->
            (match wi2i_e m fV e2 with
             | Ok x1 ->
               (match mapM wi2i_i0 c with
                | Ok x2 -> Ok (Cfor (x, ((dir, x0), x1), x2))
                | Error s -> Error s)
             | Error s -> Error s)
          | Error s -> Error s)
    else let s = E.ierror_s "invalid loop counter" in Error s
  | Cwhile (a, c, e, info, c') ->
    (match wi2i_e m fV e with
     | Ok x ->
       (match mapM wi2i_i0 c with
        | Ok x0 ->
          (match mapM wi2i_i0 c' with
           | Ok x1 -> Ok (Cwhile (a, x0, x, info, x1))
           | Error s -> Error s)
        | Error s -> Error s)
     | Error s -> Error s)
  | Ccall (xs, f, es) ->
    (match get_sig sigs f with
     | Ok x ->
       if all2 (fun ety e -> esubtype ety (etype_of_expr m e)) (fst x) es
       then (match mapM2 (E.ierror_s "bad xs length in Ccall") (wi2i_lv m fV)
                     (snd x) xs with
             | Ok x0 ->
               (match mapM (wi2i_e m fV) es with
                | Ok x1 -> Ok (Ccall (x0, f, x1))
                | Error s -> Error s)
             | Error s -> Error s)
       else let s = E.ierror_s "invalid args in Ccall" in Error s
     | Error s -> Error s)
  and wi2i_i0 = function
  | MkI (ii, ir) ->
    (match add_iinfo ii (wi2i_ir0 ir) with
     | Ok x -> Ok (MkI (ii, x))
     | Error s -> Error s)
  in wi2i_i0

(** val wi2i_fun :
    'a1 asmOp -> coq_PointerData -> coq_MSFsize -> (Var.var ->
    (signedness * Var.var) option) -> SvExtra.Sv.t -> (funname -> (positive
    extended_type list * positive extended_type list) option) -> funname ->
    'a1 fundef -> ('a1, extra_fun_t) _fundef cexec **)

let wi2i_fun asmop pd msfsz m fV sigs fn f =
  add_funname fn
    (match get_sig sigs fn with
     | Ok x ->
       let { f_info = ii; f_tyin = _; f_params = p; f_body = c; f_tyout = _;
         f_res = r; f_extra = ev } = f
       in
       (match mapM2 (E.ierror_s "bad params in fun") (wi2i_lvar m fV) 
                (fst x) p with
        | Ok x0 ->
          (match mapM (wi2i_i asmop pd msfsz m fV sigs) c with
           | Ok x1 ->
             (match mapM2 (E.ierror_s "bad return in fun") (fun ety x2 ->
                      if esubtype ety (etype_of_var m x2.v_var)
                      then wi2i_vari m fV x2
                      else let s = E.ierror_e (coq_Plvar x2) in Error s)
                      (snd x) r with
              | Ok x2 ->
                let mk =
                  map (fun ety ->
                    wi2i_type (sign_of_etype ety) (to_atype ety))
                in
                let tin = mk (fst x) in
                let tout = mk (snd x) in
                Ok { f_info = ii; f_tyin = tin; f_params = x0; f_body = x1;
                f_tyout = tout; f_res = x2; f_extra = ev }
              | Error s -> Error s)
           | Error s -> Error s)
        | Error s -> Error s)
     | Error s -> Error s)

(** val build_sig :
    'a1 asmOp -> (Var.var -> (signedness * Var.var) option) -> (funname * 'a1
    fundef) -> funname * (positive extended_type list * positive
    extended_type list) **)

let build_sig _ m fd =
  let { f_info = _; f_tyin = si; f_params = p; f_body = _; f_tyout = so;
    f_res = r; f_extra = _ } = snd fd
  in
  let mk = map2 (fun x ty -> to_etype (sign_of_var m x.v_var) ty) in
  ((fst fd), ((mk p si), (mk r so)))

(** val build_info :
    (Var.var -> (signedness * Var.var) option) -> SvExtra.Sv.t ->
    (pp_error_loc, Equality.sort -> (signedness * Var.var) option) result **)

let build_info info fv =
  match foldM (fun x fvm ->
          match Obj.magic info x with
          | Some p ->
            let (s, xi) = p in
            (match OtherDefs.is_word_type (Var.vtype (Obj.magic x)) with
             | Some _ ->
               if (&&) (convertible (Var.vtype (Obj.magic xi)) Coq_aint)
                    (negb (SvExtra.Sv.mem xi (fst fvm)))
               then Ok ((SvExtra.Sv.add xi (fst fvm)),
                      (Mvar.set (snd fvm) x (s, xi)))
               else let s0 = E.ierror_s "invalid info" in Error s0
             | None -> let s0 = E.ierror_s "invalid info" in Error s0)
          | None -> Ok fvm) (fv, Mvar.empty) (SvExtra.Sv.elements fv) with
  | Ok x -> Ok (Mvar.get (snd (Obj.magic x)))
  | Error s -> Error s

(** val wi2i_prog :
    'a1 asmOp -> coq_PointerData -> coq_MSFsize -> (Var.var ->
    (signedness * Var.var) option) -> 'a1 _uprog -> 'a1 _uprog cexec **)

let wi2i_prog asmop pd msfsz info p =
  let fV = vars_p asmop progUnit (Obj.magic p).p_funcs in
  (match build_info info fV with
   | Ok x ->
     let sigs = map (Obj.magic build_sig asmop info) p.p_funcs in
     (match map_cfprog_name_gen (fun x0 -> x0.f_info)
              (Obj.magic wi2i_fun asmop pd msfsz x fV (get_fundef sigs))
              p.p_funcs with
      | Ok x0 -> Ok { p_funcs = x0; p_globs = p.p_globs; p_extra = p.p_extra }
      | Error s -> Error s)
   | Error s -> Error s)

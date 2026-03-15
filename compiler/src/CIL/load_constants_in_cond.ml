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

module E =
 struct
  (** val pass : string **)

  let pass =
    "load constants in conditions"

  (** val load_constants_ref_error : instr_info -> string -> pp_error_loc **)

  let load_constants_ref_error =
    pp_internal_error_s_at pass
 end

(** val fresh_word :
    (instr_info -> Uint63.t -> string -> atype -> Ident.Ident.ident) ->
    instr_info -> Uint63.t -> wsize -> var_i **)

let fresh_word fresh_reg ii n ws =
  { v_var = { Var.vtype = (Coq_aword ws); Var.vname =
    (fresh_reg ii n "__tmp__" (Coq_aword ws)) }; v_info = dummy_var_info }

(** val process_constant :
    'a1 asmOp -> (instr_info -> Uint63.t -> string -> atype ->
    Ident.Ident.ident) -> instr_info -> Uint63.t -> wsize -> pexpr -> ('a1
    instr_r list * pexpr) * SvExtra.Sv.t **)

let process_constant _ fresh_reg ii n ws e =
  match is_wconst_of_size (Obj.magic ws) e with
  | Some z ->
    if negb (Z.eqb z Z0)
    then let x = fresh_word fresh_reg ii n ws in
         ((((Cassgn ((Lvar x), AT_rename, (Coq_aword ws), e)) :: []), (Pvar
         (mk_lvar x))), (SvExtra.Sv.singleton (Obj.magic x.v_var)))
    else (([], e), SvExtra.Sv.empty)
  | None -> (([], e), SvExtra.Sv.empty)

(** val process_condition :
    'a1 asmOp -> (instr_info -> Uint63.t -> string -> atype ->
    Ident.Ident.ident) -> SvExtra.Sv.t -> instr_info -> pexpr -> ('a1 instr_r
    list * pexpr) cexec **)

let process_condition asmop fresh_reg x ii e =
  match is_Papp2 e with
  | Some p ->
    let (p0, e2) = p in
    let (op, e1) = p0 in
    (match cf_of_condition op with
     | Some p1 ->
       let (_, ws) = p1 in
       let (p2, s1) =
         process_constant asmop fresh_reg ii (Uint63.of_int (0)) ws e1
       in
       let (c1, e3) = p2 in
       let (p3, s2) =
         process_constant asmop fresh_reg ii (Uint63.of_int (1)) ws e2
       in
       let (c2, e4) = p3 in
       if SvExtra.disjoint s1 x
       then if SvExtra.disjoint s2 x
            then if SvExtra.disjoint s1 s2
                 then Ok ((cat c1 c2), (Papp2 (op, e3, e4)))
                 else let s =
                        E.load_constants_ref_error ii
                          "bad fresh id (disjoint)"
                      in
                      Error s
            else let s = E.load_constants_ref_error ii "bad fresh id (2)" in
                 Error s
       else let s = E.load_constants_ref_error ii "bad fresh id (1)" in
            Error s
     | None -> Ok ([], e))
  | None -> Ok ([], e)

(** val load_constants_c :
    'a1 asmOp -> ('a1 instr -> 'a1 instr list cexec) -> 'a1 instr list ->
    (pp_error_loc, 'a1 instr list) result **)

let load_constants_c _ load_constants_i0 c =
  match mapM load_constants_i0 c with
  | Ok x -> Ok (flatten x)
  | Error s -> Error s

(** val load_constants_i :
    'a1 asmOp -> (instr_info -> Uint63.t -> string -> atype ->
    Ident.Ident.ident) -> SvExtra.Sv.t -> 'a1 instr -> (pp_error_loc, 'a1
    instr list) result **)

let rec load_constants_i asmop fresh_reg x i = match i with
| MkI (ii, ir) ->
  (match ir with
   | Cif (e, c1, c2) ->
     (match process_condition asmop fresh_reg x ii e with
      | Ok x0 ->
        let (c, e0) = x0 in
        (match load_constants_c asmop (load_constants_i asmop fresh_reg x) c1 with
         | Ok x1 ->
           (match load_constants_c asmop (load_constants_i asmop fresh_reg x)
                    c2 with
            | Ok x2 ->
              Ok
                (map (fun x3 -> MkI (ii, x3))
                  (cat c ((Cif (e0, x1, x2)) :: [])))
            | Error s -> Error s)
         | Error s -> Error s)
      | Error s -> Error s)
   | Cfor (x0, r, c) ->
     (match load_constants_c asmop (load_constants_i asmop fresh_reg x) c with
      | Ok x1 -> Ok ((MkI (ii, (Cfor (x0, r, x1)))) :: [])
      | Error s -> Error s)
   | Cwhile (a, c1, e, info, c2) ->
     (match process_condition asmop fresh_reg x info e with
      | Ok x0 ->
        let (c, e0) = x0 in
        (match load_constants_c asmop (load_constants_i asmop fresh_reg x) c1 with
         | Ok x1 ->
           (match load_constants_c asmop (load_constants_i asmop fresh_reg x)
                    c2 with
            | Ok x2 ->
              Ok ((MkI (ii, (Cwhile (a,
                (cat x1 (map (fun x3 -> MkI (info, x3)) c)), e0, info,
                x2)))) :: [])
            | Error s -> Error s)
         | Error s -> Error s)
      | Error s -> Error s)
   | _ -> Ok (i :: []))

(** val load_constants_fd :
    'a1 asmOp -> progT -> (instr_info -> Uint63.t -> string -> atype ->
    Ident.Ident.ident) -> 'a1 fundef -> (pp_error_loc, ('a1, extra_fun_t)
    _fundef) result **)

let load_constants_fd asmop _ fresh_reg fd =
  let body = fd.f_body in
  let write = write_c asmop body in
  let read = read_c asmop body in
  let returns = read_es (map coq_Plvar fd.f_res) in
  let x = SvExtra.Sv.union returns (SvExtra.Sv.union write read) in
  (match load_constants_c asmop (load_constants_i asmop fresh_reg x) body with
   | Ok x0 -> Ok (with_body asmop fd x0)
   | Error s -> Error s)

(** val load_constants_prog :
    'a1 asmOp -> progT -> (instr_info -> Uint63.t -> string -> atype ->
    Ident.Ident.ident) -> bool -> ('a1, extra_fun_t, extra_prog_t) _prog ->
    'a1 prog cexec **)

let load_constants_prog asmop pT fresh_reg doit p =
  if doit
  then (match map_cfprog_gen (fun x -> x.f_info)
                (load_constants_fd asmop pT fresh_reg) p.p_funcs with
        | Ok x -> Ok { p_funcs = x; p_globs = p.p_globs; p_extra = p.p_extra }
        | Error s -> Error s)
  else Ok p

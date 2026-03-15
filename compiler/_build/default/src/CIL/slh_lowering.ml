open BinNums
open Datatypes
open Compiler_util
open Constant_prop
open Expr
open Flag_combination
open Seq
open Slh_ops
open Sopn
open Type
open Utils0
open Var0
open Wsize

module E =
 struct
  (** val pass : string **)

  let pass =
    "speculative execution lowering"

  (** val pp_user_error :
      instr_info option -> var_info option -> pp_error -> pp_error_loc **)

  let pp_user_error ii vi pp =
    { pel_msg =
      (pp_vbox (pp :: ((PPEstring
        "Did you run the speculative constant time checker first?") :: [])));
      pel_fn = None; pel_fi = None; pel_ii = ii; pel_vi = vi; pel_pass =
      (Some pass); pel_internal = false }

  (** val cond_not_found :
      instr_info -> pexpr option -> pexpr -> pp_error_loc **)

  let cond_not_found ii oe e =
    let pp_oe =
      match oe with
      | Some e0 -> (PPEstring "known expression") :: ((PPEexpr e0) :: [])
      | None -> (PPEstring "no condition are known") :: []
    in
    pp_user_error (Some ii) None
      (pp_vbox
        ((pp_hov ((PPEstring "Not able to prove that") :: ((PPEexpr
           e) :: ((PPEstring "evaluate to true,") :: [])))) :: ((pp_hov pp_oe) :: [])))

  (** val lvar_variable : instr_info -> pp_error_loc **)

  let lvar_variable ii =
    pp_user_error (Some ii) None (PPEstring
      "misspeculation flag should be stored into register")

  (** val expr_variable : instr_info -> pexpr -> pp_error_loc **)

  let expr_variable ii e =
    pp_user_error (Some ii) None
      (pp_vbox ((PPEstring
        "only register allowed for misspeculation flag:") :: ((PPEexpr
        e) :: [])))

  (** val msf_not_found_r : var_i -> SvExtra.Sv.t -> pp_error_loc **)

  let msf_not_found_r x known =
    pp_user_error None (Some x.v_info)
      (pp_vbox
        ((pp_box ((PPEstring "Variable") :: ((PPEvar x.v_var) :: ((PPEstring
           "is not a misspeculation flag") :: [])))) :: ((pp_box ((PPEstring
                                                           "Known are") :: (
                                                           (pp_Sv known) :: []))) :: [])))

  (** val msf_not_found :
      instr_info -> var_i -> SvExtra.Sv.t -> pp_error_loc **)

  let msf_not_found ii x known =
    pp_at_ii ii (msf_not_found_r x known)

  (** val invalid_nb_args : pp_error_loc **)

  let invalid_nb_args =
    pp_internal_error_s pass "invalid number of arguments"

  (** val invalid_nb_lvals : pp_error_loc **)

  let invalid_nb_lvals =
    pp_internal_error_s pass "invalid number of left values"

  (** val cond_uses_mem : instr_info -> pexpr -> pp_error_loc **)

  let cond_uses_mem ii e =
    pp_user_error (Some ii) None
      (pp_vbox ((PPEstring "Condition has a memory access:") :: ((PPEexpr
        e) :: [])))

  (** val lowering_failed : instr_info -> pp_error_loc **)

  let lowering_failed ii =
    pp_user_error (Some ii) None (PPEstring
      "The architecture does not provides protection for selective speculative load hardening")

  (** val invalid_type_for_msf : instr_info -> pp_error_loc **)

  let invalid_type_for_msf ii =
    pp_user_error (Some ii) None (PPEstring "Invalid type for msf variable")
 end

module Env =
 struct
  type t = { cond : pexpr option; msf_vars : SvExtra.Sv.t }

  (** val cond : t -> pexpr option **)

  let cond t0 =
    t0.cond

  (** val msf_vars : t -> SvExtra.Sv.t **)

  let msf_vars t0 =
    t0.msf_vars

  (** val restrict_cond : pexpr option -> SvExtra.Sv.t -> pexpr option **)

  let restrict_cond oc xs =
    match oc with
    | Some c -> if negb (SvExtra.disjoint (read_e c) xs) then None else oc
    | None -> oc

  (** val empty : t **)

  let empty =
    { cond = None; msf_vars = SvExtra.Sv.empty }

  (** val initial : Var.var option -> t **)

  let initial ox =
    { cond = None; msf_vars = (SvExtra.sv_of_option (Obj.magic ox)) }

  (** val update_cond : coq_FlagCombinationParams -> t -> pexpr -> t **)

  let update_cond fcparams env c =
    let c0 = empty_const_prop_e fcparams c in
    { cond = (Some c0); msf_vars = env.msf_vars }

  (** val meet : t -> t -> t **)

  let meet env0 env1 =
    let c =
      match env0.cond with
      | Some c0 ->
        (match env1.cond with
         | Some c1 -> if eq_expr c0 c1 then Some c0 else None
         | None -> None)
      | None -> None
    in
    { cond = c; msf_vars = (SvExtra.Sv.inter env0.msf_vars env1.msf_vars) }

  (** val le : t -> t -> bool **)

  let le env0 env1 =
    let bcond =
      match env0.cond with
      | Some c0 ->
        (match env1.cond with
         | Some c1 -> eq_expr c0 c1
         | None -> false)
      | None -> true
    in
    (&&) bcond (SvExtra.Sv.subset env0.msf_vars env1.msf_vars)

  (** val is_msf_var : t -> Var.var -> bool **)

  let is_msf_var env x =
    SvExtra.Sv.mem (Obj.magic x) env.msf_vars

  (** val is_cond : coq_FlagCombinationParams -> t -> pexpr -> bool **)

  let is_cond fcparams env c =
    (||) (eq_expr c (Pbool true))
      (let c0 = empty_const_prop_e fcparams c in
       match env.cond with
       | Some c' -> eq_expr c0 c'
       | None -> false)

  (** val after_SLHmove : t -> Var.var option -> t **)

  let after_SLHmove env ox =
    let s = SvExtra.sv_of_option (Obj.magic ox) in
    { cond = (restrict_cond env.cond s); msf_vars =
    (SvExtra.Sv.union s env.msf_vars) }

  (** val after_assign_var : t -> Var.var -> t **)

  let after_assign_var env x =
    { cond = (restrict_cond env.cond (SvExtra.Sv.singleton (Obj.magic x)));
      msf_vars = (SvExtra.Sv.remove (Obj.magic x) env.msf_vars) }

  (** val after_assign_vars : t -> SvExtra.Sv.t -> t **)

  let after_assign_vars env xs =
    { cond = (restrict_cond env.cond xs); msf_vars =
      (SvExtra.Sv.diff env.msf_vars xs) }
 end

(** val check_e_msf :
    instr_info -> Env.t -> pexpr -> (pp_error_loc, unit) result **)

let check_e_msf ii env e = match e with
| Pvar msf ->
  if (&&) (Env.is_msf_var env msf.gv.v_var) (is_lvar msf)
  then Ok ()
  else Error (E.msf_not_found ii msf.gv env.Env.msf_vars)
| _ -> Error (E.expr_variable ii e)

(** val check_lv :
    instr_info -> lval -> (pp_error_loc, Var.var option) result **)

let check_lv ii = function
| Lnone (_, _) -> Ok None
| Lvar x -> Ok (Some x.v_var)
| _ -> Error (E.lvar_variable ii)

(** val check_lv_msf :
    coq_MSFsize -> instr_info -> lval -> (pp_error_loc, Var.var option) result **)

let check_lv_msf msfsz ii lv =
  match check_lv ii lv with
  | Ok x ->
    if match x with
       | Some x0 -> convertible (Var.vtype x0) (Coq_aword msfsz)
       | None -> true
    then Ok x
    else let s = E.invalid_type_for_msf ii in Error s
  | Error s -> Error s

(** val check_slho :
    coq_MSFsize -> coq_FlagCombinationParams -> instr_info -> lval list ->
    slh_op -> pexpr list -> Env.t -> Env.t cexec **)

let check_slho msfsz fcparams ii lvs slho es env =
  match slho with
  | SLHinit ->
    (match check_lv_msf msfsz ii
             (nth (Lnone (dummy_var_info, Coq_aint)) lvs O) with
     | Ok x -> Ok (Env.initial x)
     | Error s -> Error s)
  | SLHupdate ->
    let e = nth (Pconst Z0) es O in
    if Env.is_cond fcparams env e
    then (match check_e_msf ii env (nth (Pconst Z0) es (S O)) with
          | Ok _ ->
            (match check_lv_msf msfsz ii
                     (nth (Lnone (dummy_var_info, Coq_aint)) lvs O) with
             | Ok x -> Ok (Env.after_SLHmove env x)
             | Error s -> Error s)
          | Error s -> Error s)
    else let s = E.cond_not_found ii env.Env.cond e in Error s
  | SLHmove ->
    (match check_e_msf ii env (nth (Pconst Z0) es O) with
     | Ok _ ->
       (match check_lv_msf msfsz ii
                (nth (Lnone (dummy_var_info, Coq_aint)) lvs O) with
        | Ok x -> Ok (Env.after_SLHmove env x)
        | Error s -> Error s)
     | Error s -> Error s)
  | SLHprotect_ptr_fail (_, _) ->
    Ok
      (Env.after_assign_vars env
        (vrv (nth (Lnone (dummy_var_info, Coq_aint)) lvs O)))
  | _ ->
    (match check_e_msf ii env (nth (Pconst Z0) es (S O)) with
     | Ok _ ->
       Ok
         (Env.after_assign_vars env
           (vrv (nth (Lnone (dummy_var_info, Coq_aint)) lvs O)))
     | Error s -> Error s)

type slh_t =
| Slh_None
| Slh_msf

(** val check_f_arg :
    instr_info -> Env.t -> pexpr -> slh_t -> (pp_error_loc, unit) result **)

let check_f_arg ii env e = function
| Slh_None -> Ok ()
| Slh_msf -> check_e_msf ii env e

(** val check_f_args :
    instr_info -> Env.t -> pexpr list -> slh_t list -> (pp_error_loc, unit
    list) result **)

let check_f_args ii env es tys =
  mapM2 E.invalid_nb_args (check_f_arg ii env) es tys

(** val check_f_lv :
    coq_MSFsize -> instr_info -> Env.t -> lval -> slh_t -> (pp_error_loc,
    Env.t) result **)

let check_f_lv msfsz ii env lv = function
| Slh_None -> Ok (Env.after_assign_vars env (vrv lv))
| Slh_msf ->
  (match check_lv_msf msfsz ii lv with
   | Ok x -> Ok (Env.after_SLHmove env x)
   | Error s -> Error s)

(** val check_f_lvs :
    coq_MSFsize -> instr_info -> Env.t -> lval list -> slh_t list ->
    (pp_error_loc, Env.t) result **)

let rec check_f_lvs msfsz ii env lvs tys =
  match lvs with
  | [] -> (match tys with
           | [] -> Ok env
           | _ :: _ -> Error E.invalid_nb_lvals)
  | lv :: lvs0 ->
    (match tys with
     | [] -> Error E.invalid_nb_lvals
     | ty :: tys0 ->
       (match check_f_lv msfsz ii env lv ty with
        | Ok x -> check_f_lvs msfsz ii x lvs0 tys0
        | Error s -> Error s))

(** val init_fun_env :
    coq_MSFsize -> Env.t -> var_i list -> atype list -> slh_t list ->
    (pp_error_loc, Env.t) result **)

let rec init_fun_env msfsz env xs ttys tys =
  match xs with
  | [] ->
    (match ttys with
     | [] ->
       (match tys with
        | [] -> Ok env
        | _ :: _ -> Error (pp_internal_error_s E.pass "invalid params (nb)"))
     | _ :: _ -> Error (pp_internal_error_s E.pass "invalid params (nb)"))
  | x :: xs0 ->
    (match ttys with
     | [] -> Error (pp_internal_error_s E.pass "invalid params (nb)")
     | t0 :: ttys0 ->
       (match tys with
        | [] -> Error (pp_internal_error_s E.pass "invalid params (nb)")
        | ty :: tys0 ->
          (match match ty with
                 | Slh_None -> Ok (Env.after_assign_var env x.v_var)
                 | Slh_msf ->
                   if (&&)
                        (convertible (Var.vtype x.v_var) (Coq_aword msfsz))
                        (convertible t0 (Coq_aword msfsz))
                   then Ok (Env.after_SLHmove env (Some x.v_var))
                   else let s =
                          pp_internal_error_s E.pass "invalid params (type)"
                        in
                        Error s with
           | Ok x0 -> init_fun_env msfsz x0 xs0 ttys0 tys0
           | Error s -> Error s)))

(** val check_res :
    coq_MSFsize -> Env.t -> var_i list -> atype list -> slh_t list ->
    (pp_error_loc, unit) result **)

let rec check_res msfsz env xs ttys tys =
  match xs with
  | [] ->
    (match ttys with
     | [] ->
       (match tys with
        | [] -> Ok ()
        | _ :: _ -> Error (pp_internal_error_s E.pass "invalid return (nb)"))
     | _ :: _ -> Error (pp_internal_error_s E.pass "invalid return (nb)"))
  | x :: xs0 ->
    (match ttys with
     | [] -> Error (pp_internal_error_s E.pass "invalid return (nb)")
     | t0 :: ttys0 ->
       (match tys with
        | [] -> Error (pp_internal_error_s E.pass "invalid return (nb)")
        | ty :: tys0 ->
          if match ty with
             | Slh_None -> true
             | Slh_msf -> Env.is_msf_var env x.v_var
          then if match ty with
                  | Slh_None -> true
                  | Slh_msf -> convertible t0 (Coq_aword msfsz)
               then check_res msfsz env xs0 ttys0 tys0
               else let s = pp_internal_error_s E.pass "invalid return (type)"
                    in
                    Error s
          else let s = E.msf_not_found_r x env.Env.msf_vars in Error s))

(** val check_for :
    instr_info -> Var.var -> (Env.t -> Env.t cexec) -> nat -> Env.t -> Env.t
    cexec **)

let rec check_for ii x check_c fuel env =
  match fuel with
  | O -> Error (ii_loop_iterator E.pass ii)
  | S n ->
    (match check_c (Env.after_assign_var env x) with
     | Ok x0 ->
       if Env.le env x0
       then Ok env
       else check_for ii x check_c n (Env.meet env x0)
     | Error s -> Error s)

(** val check_while :
    coq_FlagCombinationParams -> instr_info -> pexpr -> (Env.t -> Env.t
    cexec) -> (Env.t -> Env.t cexec) -> nat -> Env.t -> Env.t cexec **)

let rec check_while fcparams ii cond0 check_c0 check_c1 fuel env =
  match fuel with
  | O -> Error (ii_loop_iterator E.pass ii)
  | S n ->
    (match check_c0 env with
     | Ok x ->
       (match check_c1 (Env.update_cond fcparams x cond0) with
        | Ok x0 ->
          if Env.le env x0
          then Ok (Env.update_cond fcparams x (enot cond0))
          else check_while fcparams ii cond0 check_c0 check_c1 n
                 (Env.meet env x0)
        | Error s -> Error s)
     | Error s -> Error s)

type 'asm_op sh_params =
  lval list -> slh_op -> pexpr list -> ((lval list * 'asm_op sopn) * pexpr
  list) option
  (* singleton inductive, whose constructor was Build_sh_params *)

(** val check_i :
    'a1 asmOp -> coq_MSFsize -> coq_FlagCombinationParams -> (funname ->
    slh_t list * slh_t list) -> 'a1 instr -> Env.t -> Env.t cexec **)

let rec check_i asmop msfsz fcparams fun_info i env =
  let MkI (ii, ir) = i in
  (match ir with
   | Cassgn (lv, _, _, _) -> Ok (Env.after_assign_vars env (vrv lv))
   | Copn (lvs, _, op, es) ->
     (match is_Oslh asmop op with
      | Some slho -> check_slho msfsz fcparams ii lvs slho es env
      | None -> Ok (Env.after_assign_vars env (vrvs lvs)))
   | Csyscall (_, _, _) -> Ok Env.empty
   | Cif (cond0, c0, c1) ->
     if negb (use_mem cond0)
     then (match let env0 = Env.update_cond fcparams env cond0 in
                 foldM (check_i asmop msfsz fcparams fun_info) env0 c0 with
           | Ok x ->
             (match let env0 = Env.update_cond fcparams env (enot cond0) in
                    foldM (check_i asmop msfsz fcparams fun_info) env0 c1 with
              | Ok x0 -> Ok (Env.meet x x0)
              | Error s -> Error s)
           | Error s -> Error s)
     else let s = E.cond_uses_mem ii cond0 in Error s
   | Cfor (x, _, c) ->
     check_for ii x.v_var (fun env0 ->
       foldM (check_i asmop msfsz fcparams fun_info) env0 c) Loop.nb env
   | Cwhile (_, c0, cond0, _, c1) ->
     if negb (use_mem cond0)
     then check_while fcparams ii cond0 (fun env0 ->
            foldM (check_i asmop msfsz fcparams fun_info) env0 c0)
            (fun env0 ->
            foldM (check_i asmop msfsz fcparams fun_info) env0 c1) Loop.nb env
     else let s = E.cond_uses_mem ii cond0 in Error s
   | Ccall (xs, fn, es) ->
     let (in_t, out_t) = fun_info fn in
     (match check_f_args ii env es in_t with
      | Ok _ -> check_f_lvs msfsz ii env xs out_t
      | Error s -> Error s))

(** val check_cmd :
    'a1 asmOp -> coq_MSFsize -> coq_FlagCombinationParams -> (funname ->
    slh_t list * slh_t list) -> Env.t -> 'a1 instr list -> Env.t cexec **)

let check_cmd asmop msfsz fcparams fun_info env c =
  foldM (check_i asmop msfsz fcparams fun_info) env c

(** val check_fd :
    'a1 asmOp -> coq_MSFsize -> coq_FlagCombinationParams -> progT ->
    (funname -> slh_t list * slh_t list) -> funname -> 'a1 fundef -> unit
    cexec **)

let check_fd asmop msfsz fcparams _ fun_info fn fd =
  let (in_t, out_t) = fun_info fn in
  (match init_fun_env msfsz Env.empty fd.f_params fd.f_tyin in_t with
   | Ok x ->
     (match check_cmd asmop msfsz fcparams fun_info x fd.f_body with
      | Ok x0 ->
        (match check_res msfsz x0 fd.f_res fd.f_tyout out_t with
         | Ok _ -> Ok ()
         | Error s -> Error s)
      | Error s -> Error s)
   | Error s -> Error s)

(** val lower_slho :
    'a1 asmOp -> 'a1 sh_params -> instr_info -> lval list -> assgn_tag ->
    slh_op -> pexpr list -> 'a1 instr_r cexec **)

let lower_slho asmop shparams ii lvs tg slho es =
  match match is_protect_ptr slho with
        | Some p0 ->
          let (ws, p) = p0 in
          Ok ((lvs, (Oslh (SLHprotect_ptr_fail (ws, p)))), es)
        | None -> o2r (E.lowering_failed ii) (shparams lvs slho es) with
  | Ok x -> Ok (instr_of_copn_args asmop tg x)
  | Error s -> Error s

(** val lower_i :
    'a1 asmOp -> 'a1 sh_params -> 'a1 instr -> 'a1 instr cexec **)

let rec lower_i asmop shparams i =
  let lower_cmd0 = fun c -> mapM (lower_i asmop shparams) c in
  let MkI (ii, ir) = i in
  (match match ir with
         | Copn (lvs, tg, op, es) ->
           (match is_Oslh asmop op with
            | Some slho -> lower_slho asmop shparams ii lvs tg slho es
            | None -> Ok ir)
         | Cif (b, c0, c1) ->
           (match lower_cmd0 c0 with
            | Ok x ->
              (match lower_cmd0 c1 with
               | Ok x0 -> Ok (Cif (b, x, x0))
               | Error s -> Error s)
            | Error s -> Error s)
         | Cfor (x, r, c) ->
           (match lower_cmd0 c with
            | Ok x0 -> Ok (Cfor (x, r, x0))
            | Error s -> Error s)
         | Cwhile (al, c0, b, info, c1) ->
           (match lower_cmd0 c0 with
            | Ok x ->
              (match lower_cmd0 c1 with
               | Ok x0 -> Ok (Cwhile (al, x, b, info, x0))
               | Error s -> Error s)
            | Error s -> Error s)
         | _ -> Ok ir with
   | Ok x -> Ok (MkI (ii, x))
   | Error s -> Error s)

(** val lower_cmd :
    'a1 asmOp -> 'a1 sh_params -> 'a1 instr list -> 'a1 instr list cexec **)

let lower_cmd asmop shparams c =
  mapM (lower_i asmop shparams) c

(** val lower_fd :
    'a1 asmOp -> coq_MSFsize -> coq_FlagCombinationParams -> progT -> 'a1
    sh_params -> (funname -> slh_t list * slh_t list) -> funname -> 'a1
    fundef -> (pp_error_loc, ('a1, extra_fun_t) _fundef) result **)

let lower_fd asmop msfsz fcparams pT shparams fun_info fn fd =
  match check_fd asmop msfsz fcparams pT fun_info fn fd with
  | Ok _ ->
    let { f_info = ii; f_tyin = si; f_params = p; f_body = c; f_tyout = so;
      f_res = r; f_extra = ev } = fd
    in
    (match lower_cmd asmop shparams c with
     | Ok x ->
       Ok { f_info = ii; f_tyin = si; f_params = p; f_body = x; f_tyout = so;
         f_res = r; f_extra = ev }
     | Error s -> Error s)
  | Error s -> Error s

(** val is_slh_none : slh_t -> bool **)

let is_slh_none = function
| Slh_None -> true
| Slh_msf -> false

(** val lower_slh_prog :
    'a1 asmOp -> coq_MSFsize -> coq_FlagCombinationParams -> progT -> 'a1
    sh_params -> (funname -> slh_t list * slh_t list) -> funname list -> 'a1
    prog -> 'a1 prog cexec **)

let lower_slh_prog asmop msfsz fcparams pT shparams fun_info entries p =
  if all (fun f -> all is_slh_none (fst (fun_info f))) entries
  then (match map_cfprog_name_gen (fun x -> x.f_info)
                (lower_fd asmop msfsz fcparams pT shparams fun_info) p.p_funcs with
        | Ok x -> Ok { p_funcs = x; p_globs = p.p_globs; p_extra = p.p_extra }
        | Error s -> Error s)
  else let s =
         E.pp_user_error None None (PPEstring
           "export function should not take a misspeculation flag as input")
       in
       Error s

open Datatypes
open Compiler_util
open Eqtype
open Expr
open One_varmap
open Seq
open Sopn
open Ssrfun
open Type
open Utils0
open Var0
open Wsize

module E =
 struct
  (** val pass_name : string **)

  let pass_name =
    "one-varmap checker"

  (** val gen_error :
      bool -> instr_info option -> pp_error -> pp_error_loc **)

  let gen_error internal ii msg =
    { pel_msg = msg; pel_fn = None; pel_fi = None; pel_ii = ii; pel_vi =
      None; pel_pass = (Some pass_name); pel_internal = internal }

  (** val internal_error : instr_info -> string -> pp_error_loc **)

  let internal_error ii msg =
    gen_error true (Some ii) (PPEstring msg)

  (** val ii_loop_iterator : instr_info -> pp_error_loc **)

  let ii_loop_iterator =
    ii_loop_iterator pass_name
 end

(** val writefun_ra :
    coq_PointerData -> 'a1 asmOp -> one_varmap_info -> 'a1 sprog ->
    SvExtra.Sv.t -> (funname -> SvExtra.Sv.t) -> funname -> SvExtra.Sv.t **)

let writefun_ra _ asmop ovm_i p var_tmp writefun fn =
  let ra =
    match get_fundef p.p_funcs fn with
    | Some fd ->
      SvExtra.Sv.union (ra_undef asmop ovm_i (Obj.magic fd) var_tmp)
        (ra_vm_return (Obj.magic fd).f_extra)
    | None -> SvExtra.Sv.empty
  in
  SvExtra.Sv.union (writefun fn) ra

(** val writefun_ra_call :
    coq_PointerData -> 'a1 asmOp -> one_varmap_info -> 'a1 sprog ->
    SvExtra.Sv.t -> (funname -> SvExtra.Sv.t) -> funname -> SvExtra.Sv.t **)

let writefun_ra_call pd asmop ovm_i p var_tmp writefun fn =
  SvExtra.Sv.union (writefun_ra pd asmop ovm_i p var_tmp writefun fn)
    (fd_tmp_call pd asmop p fn)

(** val write_I_rec :
    coq_PointerData -> 'a1 asmOp -> one_varmap_info -> 'a1 sprog ->
    SvExtra.Sv.t -> (funname -> SvExtra.Sv.t) -> SvExtra.Sv.t -> 'a1 instr ->
    SvExtra.Sv.t **)

let write_I_rec pd asmop ovm_i p var_tmp writefun =
  let rec write_i_rec s = function
  | Cassgn (x, _, _, _) -> vrv_rec s x
  | Copn (xs, _, _, _) -> vrvs_rec s xs
  | Csyscall (_, o, _) ->
    vrvs_rec (SvExtra.Sv.union s (syscall_kill ovm_i))
      (to_lvals (ovm_i.syscall_sig o).scs_vout)
  | Cif (_, c1, c2) -> foldl write_I_rec0 (foldl write_I_rec0 s c2) c1
  | Cfor (x, _, c) ->
    foldl write_I_rec0 (SvExtra.Sv.add (Obj.magic x.v_var) s) c
  | Cwhile (_, c, _, _, c') -> foldl write_I_rec0 (foldl write_I_rec0 s c') c
  | Ccall (_, fn, _) ->
    SvExtra.Sv.union s (writefun_ra_call pd asmop ovm_i p var_tmp writefun fn)
  and write_I_rec0 s = function
  | MkI (_, i0) -> write_i_rec s i0
  in write_I_rec0

(** val write_c_rec :
    coq_PointerData -> 'a1 asmOp -> one_varmap_info -> 'a1 sprog ->
    SvExtra.Sv.t -> (funname -> SvExtra.Sv.t) -> SvExtra.Sv.t -> 'a1 instr
    list -> SvExtra.Sv.t **)

let write_c_rec pd asmop ovm_i p var_tmp writefun =
  foldl (write_I_rec pd asmop ovm_i p var_tmp writefun)

(** val write_c :
    coq_PointerData -> 'a1 asmOp -> one_varmap_info -> 'a1 sprog ->
    SvExtra.Sv.t -> (funname -> SvExtra.Sv.t) -> 'a1 instr list ->
    SvExtra.Sv.t **)

let write_c pd asmop ovm_i p var_tmp writefun =
  write_c_rec pd asmop ovm_i p var_tmp writefun SvExtra.Sv.empty

(** val write_fd :
    coq_PointerData -> 'a1 asmOp -> one_varmap_info -> 'a1 sprog ->
    SvExtra.Sv.t -> (funname -> SvExtra.Sv.t) -> 'a1 sfundef -> SvExtra.Sv.t **)

let write_fd pd asmop ovm_i p var_tmp writefun fd =
  write_c pd asmop ovm_i p var_tmp writefun fd.f_body

(** val get_wmap : SvExtra.Sv.t Mf.t -> funname -> SvExtra.Sv.t **)

let get_wmap wmap fn =
  Ssrfun.Option.default SvExtra.Sv.empty (Mf.get wmap (Obj.magic fn))

(** val mk_wmap :
    coq_PointerData -> 'a1 asmOp -> one_varmap_info -> 'a1 sprog ->
    SvExtra.Sv.t -> SvExtra.Sv.t Mf.t **)

let mk_wmap pd asmop ovm_i p var_tmp =
  foldr (fun pat wmap ->
    let (f, fd) = pat in
    let w = write_fd pd asmop ovm_i p var_tmp (get_wmap wmap) fd in
    Mf.set wmap (Obj.magic f) w) Mf.empty p.p_funcs

(** val check_wmap :
    coq_PointerData -> 'a1 asmOp -> one_varmap_info -> 'a1 sprog ->
    SvExtra.Sv.t -> SvExtra.Sv.t Mf.t -> bool **)

let check_wmap pd asmop ovm_i p var_tmp wmap =
  all (fun pat ->
    let (f, fd) = pat in
    SvExtra.Sv.subset (write_fd pd asmop ovm_i p var_tmp (get_wmap wmap) fd)
      (get_wmap wmap f)) p.p_funcs

(** val check_fv :
    instr_info -> SvExtra.Sv.t -> SvExtra.Sv.t -> (pp_error_loc, unit) result **)

let check_fv ii d r =
  let i = SvExtra.Sv.inter d r in
  if SvExtra.Sv.is_empty i
  then Ok ()
  else Error
         (E.gen_error true (Some ii)
           (pp_hov ((PPEstring
             "modified expression :") :: (map (Obj.magic (fun x -> PPEvar x))
                                           (SvExtra.Sv.elements i)))))

(** val check_e :
    instr_info -> SvExtra.Sv.t -> pexpr -> (pp_error_loc, unit) result **)

let check_e ii d e =
  check_fv ii d (read_e e)

(** val check_es :
    instr_info -> SvExtra.Sv.t -> pexpr list -> (pp_error_loc, unit) result **)

let check_es ii d es =
  foldM (fun e _ -> check_e ii d e) () es

(** val check_c :
    'a1 asmOp -> (SvExtra.Sv.t -> 'a1 instr -> SvExtra.Sv.t cexec) ->
    SvExtra.Sv.t -> 'a1 instr list -> (pp_error_loc, SvExtra.Sv.t) result **)

let rec check_c asmop check_i0 d = function
| [] -> Ok d
| i :: c' ->
  (match check_i0 d i with
   | Ok x -> check_c asmop check_i0 x c'
   | Error s -> Error s)

(** val wloop :
    'a1 asmOp -> (SvExtra.Sv.t -> 'a1 instr -> SvExtra.Sv.t cexec) ->
    instr_info -> 'a1 instr list -> SvExtra.Sv.t -> 'a1 instr list -> nat ->
    SvExtra.Sv.t -> (pp_error_loc, SvExtra.Sv.t) result **)

let rec wloop asmop check_i0 ii c1 efv c2 n d =
  match n with
  | O -> Error (E.ii_loop_iterator ii)
  | S n' ->
    (match check_c asmop check_i0 d c1 with
     | Ok x ->
       (match check_fv ii x efv with
        | Ok _ ->
          (match check_c asmop check_i0 x c2 with
           | Ok x0 ->
             if SvExtra.Sv.subset x0 d
             then Ok x
             else wloop asmop check_i0 ii c1 efv c2 n' (SvExtra.Sv.union x0 d)
           | Error s -> Error s)
        | Error s -> Error s)
     | Error s -> Error s)

(** val check_lv :
    instr_info -> SvExtra.Sv.t -> lval -> (pp_error_loc, SvExtra.Sv.t) result **)

let check_lv ii d x =
  match check_fv ii d (read_rv x) with
  | Ok _ -> Ok (SvExtra.Sv.diff d (vrv x))
  | Error s -> Error s

(** val check_lvs :
    instr_info -> SvExtra.Sv.t -> lval list -> (pp_error_loc, SvExtra.Sv.t)
    result **)

let check_lvs ii d xs =
  foldM (fun x d0 -> check_lv ii d0 x) d xs

(** val check_i :
    coq_PointerData -> 'a1 asmOp -> one_varmap_info -> 'a1 sprog ->
    SvExtra.Sv.t -> (funname -> SvExtra.Sv.t) -> wsize -> SvExtra.Sv.t -> 'a1
    instr -> SvExtra.Sv.t cexec **)

let check_i pd asmop ovm_i p var_tmp writefun =
  let rec check_i0 sz d = function
  | MkI (ii, ir) -> check_ir sz ii d ir
  and check_ir sz ii d = function
  | Cassgn (x, _, _, e) ->
    (match check_e ii d e with
     | Ok _ -> check_lv ii d x
     | Error s -> Error s)
  | Copn (xs, _, _, es) ->
    (match check_es ii d es with
     | Ok _ -> check_lvs ii d xs
     | Error s -> Error s)
  | Csyscall (xs, o, es) ->
    let osig = ovm_i.syscall_sig o in
    let o_params = osig.scs_vin in
    let o_res = osig.scs_vout in
    (match check_es ii d es with
     | Ok _ ->
       if all2 (fun e a ->
            match e with
            | Pvar g ->
              let { gv = v; gs = gs0 } = g in
              (match gs0 with
               | Slocal ->
                 eq_op Var.coq_MvMake_var__canonical__eqtype_Equality
                   (Obj.magic v.v_var) (Obj.magic a)
               | Sglob -> false)
            | _ -> false) es o_params
       then if all2 (fun x r ->
                 match x with
                 | Lvar v ->
                   eq_op Var.coq_MvMake_var__canonical__eqtype_Equality
                     (Obj.magic v.v_var) (Obj.magic r)
                 | _ -> false) xs o_res
            then let w = syscall_kill ovm_i in
                 Ok
                 (SvExtra.Sv.diff (SvExtra.Sv.union d w)
                   (vrvs (to_lvals (ovm_i.syscall_sig o).scs_vout)))
            else let s = E.internal_error ii "bad syscall dests" in Error s
       else let s = E.internal_error ii "bad syscall args" in Error s
     | Error s -> Error s)
  | Cif (b, c1, c2) ->
    (match check_e ii d b with
     | Ok _ ->
       (match check_c asmop (check_i0 sz) d c1 with
        | Ok x ->
          (match check_c asmop (check_i0 sz) d c2 with
           | Ok x0 -> Ok (SvExtra.Sv.union x x0)
           | Error s -> Error s)
        | Error s -> Error s)
     | Error s -> Error s)
  | Cfor (_, _, _) ->
    Error (E.internal_error ii "for loop should be unrolled")
  | Cwhile (_, c, e, _, c') ->
    if is_false e
    then check_c asmop (check_i0 sz) d c
    else wloop asmop (check_i0 sz) ii c (read_e e) c' Loop.nb d
  | Ccall (xs, fn, es) ->
    (match get_fundef p.p_funcs fn with
     | Some fd ->
       let tmp = tmp_call (Obj.magic fd).f_extra in
       (match check_es ii (SvExtra.Sv.union d tmp) es with
        | Ok _ ->
          if cmp_le wsize_cmp (Obj.magic fd).f_extra.sf_align sz
          then if all2 (fun e a ->
                    match e with
                    | Pvar g ->
                      let { gv = v; gs = gs0 } = g in
                      (match gs0 with
                       | Slocal ->
                         eq_op Var.coq_MvMake_var__canonical__eqtype_Equality
                           (Obj.magic v.v_var) (Obj.magic a.v_var)
                       | Sglob -> false)
                    | _ -> false) es fd.f_params
               then if all2 (fun x r ->
                         match x with
                         | Lvar v ->
                           eq_op
                             Var.coq_MvMake_var__canonical__eqtype_Equality
                             (Obj.magic v.v_var) (Obj.magic r.v_var)
                         | _ -> false) xs fd.f_res
                    then let w =
                           writefun_ra pd asmop ovm_i p var_tmp writefun fn
                         in
                         let res =
                           SvExtra.sv_of_list (Obj.magic (fun v -> v.v_var))
                             fd.f_res
                         in
                         if SvExtra.disjoint tmp res
                         then Ok
                                (SvExtra.Sv.union
                                  (SvExtra.Sv.diff (SvExtra.Sv.union d w) res)
                                  tmp)
                         else let s =
                                E.internal_error ii "tmp call write dests"
                              in
                              Error s
                    else let s = E.internal_error ii "bad call dests" in
                         Error s
               else let s = E.internal_error ii "bad call args" in Error s
          else let s = E.internal_error ii "alignment constraints error" in
               Error s
        | Error s -> Error s)
     | None -> Error (E.internal_error ii "call to unknown function"))
  in check_i0

(** val check_fd :
    coq_PointerData -> 'a1 asmOp -> one_varmap_info -> 'a1 sprog ->
    SvExtra.Sv.t -> (funname -> SvExtra.Sv.t) -> funname -> 'a1 sfundef ->
    (pp_error_loc, unit) result **)

let check_fd pd asmop ovm_i p var_tmp =
  let magic_variables0 = magic_variables pd asmop p in
  (fun writefun ->
  let check_preserved_register = fun w j name r ->
    if convertible (Var.vtype r) (Coq_aword pd)
    then if negb (SvExtra.Sv.mem (Obj.magic r) w)
         then if negb (SvExtra.Sv.mem (Obj.magic r) j)
              then Ok ()
              else Error
                     (E.gen_error true None
                       (pp_box ((PPEstring
                         "the function depends on its") :: ((PPEstring
                         name) :: ((PPEvar r) :: [])))))
         else let s =
                E.gen_error true None
                  (pp_box ((PPEstring
                    "the function writes its") :: ((PPEstring
                    name) :: ((PPEvar r) :: []))))
              in
              Error s
    else let s =
           E.gen_error true None
             (pp_box ((PPEstring "bad register type for") :: ((PPEstring
               name) :: ((PPEvar r) :: []))))
         in
         Error s
  in
  (fun fn fd ->
  let params = SvExtra.sv_of_list (Obj.magic (fun v -> v.v_var)) fd.f_params
  in
  let dI =
    SvExtra.Sv.inter params (ra_undef asmop ovm_i (Obj.magic fd) var_tmp)
  in
  (match check_c asmop
           (check_i pd asmop ovm_i p var_tmp writefun
             (Obj.magic fd).f_extra.sf_align) dI fd.f_body with
   | Ok x ->
     let dF = SvExtra.Sv.union (ra_vm_return (Obj.magic fd).f_extra) x in
     let res = SvExtra.sv_of_list (Obj.magic (fun v -> v.v_var)) fd.f_res in
     let w' = writefun_ra pd asmop ovm_i p var_tmp writefun fn in
     if SvExtra.disjoint dF res
     then if SvExtra.disjoint params magic_variables0
          then if negb
                    (SvExtra.Sv.mem
                      (Obj.magic
                        (mk_var_i { Var.vtype = (Coq_aword pd); Var.vname =
                          (Obj.magic p).p_extra.sp_rsp }).v_var) res)
               then if SvExtra.disjoint w' magic_variables0
                    then let w = writefun fn in
                         let j = SvExtra.Sv.union magic_variables0 params in
                         let e = fd.f_extra in
                         (match match (Obj.magic e).sf_save_stack with
                                | SavedStackReg r ->
                                  check_preserved_register w j
                                    "saved stack pointer" r
                                | _ -> Ok () with
                          | Ok _ ->
                            if SvExtra.disjoint magic_variables0
                                 (tmp_call (Obj.magic e))
                            then (match (Obj.magic e).sf_return_address with
                                  | RAnone ->
                                    let to_save =
                                      SvExtra.sv_of_list (Obj.magic fst)
                                        (Obj.magic fd).f_extra.sf_to_save
                                    in
                                    if SvExtra.disjoint to_save res
                                    then if SvExtra.Sv.subset
                                              (SvExtra.Sv.inter
                                                ovm_i.callee_saved w') to_save
                                         then if all (fun x0 ->
                                                   match Var.vtype x0.v_var with
                                                   | Coq_aword _ -> true
                                                   | _ -> false) fd.f_params
                                              then Ok ()
                                              else Error
                                                     (E.gen_error true None
                                                       (PPEstring
                                                       "the export function has non-word arguments"))
                                         else let s =
                                                E.gen_error true None
                                                  (PPEstring
                                                  "the function kills some callee-saved registers")
                                              in
                                              Error s
                                    else let s =
                                           E.gen_error true None (PPEstring
                                             "the function returns a callee-saved register")
                                         in
                                         Error s
                                  | RAreg (ra, _) ->
                                    check_preserved_register w j
                                      "return address" ra
                                  | RAstack (ra_call, ra_return, _, _) ->
                                    (match ra_call with
                                     | Some r ->
                                       if convertible (Var.vtype r)
                                            (Coq_aword pd)
                                       then (match ra_return with
                                             | Some r0 ->
                                               if convertible (Var.vtype r0)
                                                    (Coq_aword pd)
                                               then Ok ()
                                               else Error
                                                      (E.gen_error true None
                                                        (pp_box ((PPEstring
                                                          "bad register type for") :: ((PPEstring
                                                          "return address (return)") :: ((PPEvar
                                                          r0) :: [])))))
                                             | None -> Ok ())
                                       else let s =
                                              E.gen_error true None
                                                (pp_box ((PPEstring
                                                  "bad register type for") :: ((PPEstring
                                                  "return address (call)") :: ((PPEvar
                                                  r) :: []))))
                                            in
                                            Error s
                                     | None ->
                                       (match ra_return with
                                        | Some r ->
                                          if convertible (Var.vtype r)
                                               (Coq_aword pd)
                                          then Ok ()
                                          else Error
                                                 (E.gen_error true None
                                                   (pp_box ((PPEstring
                                                     "bad register type for") :: ((PPEstring
                                                     "return address (return)") :: ((PPEvar
                                                     r) :: [])))))
                                        | None -> Ok ())))
                            else let s =
                                   E.gen_error true None (PPEstring
                                     "not (disjoint magic_variables tmp_call)")
                                 in
                                 Error s
                          | Error s -> Error s)
                    else let s =
                           E.gen_error true None (PPEstring
                             "the function writes to RSP or global-data")
                         in
                         Error s
               else let s =
                      E.gen_error true None (PPEstring
                        "the function returns RSP")
                    in
                    Error s
          else let s =
                 E.gen_error true None (PPEstring
                   "the function has RSP or global-data as parameter")
               in
               Error s
     else let s =
            E.gen_error true None (PPEstring
              "not able to ensure equality of the result")
          in
          Error s
   | Error s -> Error s)))

(** val check_prog :
    coq_PointerData -> 'a1 asmOp -> one_varmap_info -> 'a1 sprog ->
    SvExtra.Sv.t -> (funname -> SvExtra.Sv.t) -> (pp_error_loc,
    (funname * unit) list) result **)

let check_prog pd asmop ovm_i p var_tmp writefun =
  map_cfprog_name_gen (fun x -> x.f_info)
    (check_fd pd asmop ovm_i p var_tmp writefun) p.p_funcs

(** val check :
    coq_PointerData -> 'a1 asmOp -> one_varmap_info -> 'a1 sprog ->
    SvExtra.Sv.t -> (pp_error_loc, unit) result **)

let check pd asmop ovm_i p var_tmp =
  let magic_variables0 = magic_variables pd asmop p in
  let wmap = mk_wmap pd asmop ovm_i p var_tmp in
  if check_wmap pd asmop ovm_i p var_tmp wmap
  then if negb
            (eq_op Ident.ident_eqType
              (Obj.magic (Obj.magic p).p_extra.sp_rip)
              (Obj.magic (Obj.magic p).p_extra.sp_rsp))
       then if SvExtra.disjoint var_tmp magic_variables0
            then (match check_prog pd asmop ovm_i p var_tmp (get_wmap wmap) with
                  | Ok _ -> Ok ()
                  | Error s -> Error s)
            else let s =
                   E.gen_error true None (PPEstring
                     "RAX clashes with RSP or RIP")
                 in
                 Error s
       else let s = E.gen_error true None (PPEstring "rip and rsp clash") in
            Error s
  else let s = E.gen_error true None (PPEstring "invalid wmap") in Error s

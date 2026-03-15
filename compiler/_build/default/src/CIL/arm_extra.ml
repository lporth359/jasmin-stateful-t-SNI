open BinNums
open Bool
open Datatypes
open Arch_decl
open Arch_extra
open Arch_utils
open Arm
open Arm_decl
open Arm_instr_decl
open Arm_params_core
open Compiler_util
open EqbOK
open Eqb_core_defs
open Eqtype
open Expr
open Fexpr
open Sem_type
open Seq
open Sopn
open Ssralg
open Type
open Utils0
open Var0
open Word0
open Wsize

type __ = Obj.t

type arm_extra_op =
| Oarm_swap of wsize
| Oarm_add_large_imm
| Osmart_li of wsize
| Osmart_li_cc of wsize

(** val arm_extra_op_rect :
    (wsize -> 'a1) -> 'a1 -> (wsize -> 'a1) -> (wsize -> 'a1) -> arm_extra_op
    -> 'a1 **)

let arm_extra_op_rect f f0 f1 f2 = function
| Oarm_swap w -> f w
| Oarm_add_large_imm -> f0
| Osmart_li w -> f1 w
| Osmart_li_cc w -> f2 w

(** val arm_extra_op_rec :
    (wsize -> 'a1) -> 'a1 -> (wsize -> 'a1) -> (wsize -> 'a1) -> arm_extra_op
    -> 'a1 **)

let arm_extra_op_rec f f0 f1 f2 = function
| Oarm_swap w -> f w
| Oarm_add_large_imm -> f0
| Osmart_li w -> f1 w
| Osmart_li_cc w -> f2 w

type is_arm_extra_op =
| Coq_is_Oarm_swap of wsize * is_wsize
| Coq_is_Oarm_add_large_imm
| Coq_is_Osmart_li of wsize * is_wsize
| Coq_is_Osmart_li_cc of wsize * is_wsize

(** val is_arm_extra_op_rect :
    (wsize -> is_wsize -> 'a1) -> 'a1 -> (wsize -> is_wsize -> 'a1) -> (wsize
    -> is_wsize -> 'a1) -> arm_extra_op -> is_arm_extra_op -> 'a1 **)

let is_arm_extra_op_rect f f0 f1 f2 _ = function
| Coq_is_Oarm_swap (w, p_) -> f w p_
| Coq_is_Oarm_add_large_imm -> f0
| Coq_is_Osmart_li (w, p_) -> f1 w p_
| Coq_is_Osmart_li_cc (w, p_) -> f2 w p_

(** val is_arm_extra_op_rec :
    (wsize -> is_wsize -> 'a1) -> 'a1 -> (wsize -> is_wsize -> 'a1) -> (wsize
    -> is_wsize -> 'a1) -> arm_extra_op -> is_arm_extra_op -> 'a1 **)

let is_arm_extra_op_rec f f0 f1 f2 _ = function
| Coq_is_Oarm_swap (w, p_) -> f w p_
| Coq_is_Oarm_add_large_imm -> f0
| Coq_is_Osmart_li (w, p_) -> f1 w p_
| Coq_is_Osmart_li_cc (w, p_) -> f2 w p_

(** val arm_extra_op_tag : arm_extra_op -> positive **)

let arm_extra_op_tag = function
| Oarm_swap _ -> Coq_xH
| Oarm_add_large_imm -> Coq_xO Coq_xH
| Osmart_li _ -> Coq_xI Coq_xH
| Osmart_li_cc _ -> Coq_xO (Coq_xO Coq_xH)

(** val is_arm_extra_op_inhab : arm_extra_op -> is_arm_extra_op **)

let is_arm_extra_op_inhab = function
| Oarm_swap h -> Coq_is_Oarm_swap (h, (is_wsize_inhab h))
| Oarm_add_large_imm -> Coq_is_Oarm_add_large_imm
| Osmart_li h -> Coq_is_Osmart_li (h, (is_wsize_inhab h))
| Osmart_li_cc h -> Coq_is_Osmart_li_cc (h, (is_wsize_inhab h))

(** val is_arm_extra_op_functor :
    arm_extra_op -> is_arm_extra_op -> is_arm_extra_op **)

let rec is_arm_extra_op_functor _ x =
  x

type box_arm_extra_op_Oarm_swap =
  wsize
  (* singleton inductive, whose constructor was Box_arm_extra_op_Oarm_swap *)

(** val coq_Box_arm_extra_op_Oarm_swap_0 :
    box_arm_extra_op_Oarm_swap -> wsize **)

let coq_Box_arm_extra_op_Oarm_swap_0 record =
  record

type box_arm_extra_op_Oarm_add_large_imm =
| Box_arm_extra_op_Oarm_add_large_imm

type arm_extra_op_fields_t = __

(** val arm_extra_op_fields : arm_extra_op -> arm_extra_op_fields_t **)

let arm_extra_op_fields = function
| Oarm_swap h -> Obj.magic h
| Oarm_add_large_imm -> Obj.magic Box_arm_extra_op_Oarm_add_large_imm
| Osmart_li h -> Obj.magic h
| Osmart_li_cc h -> Obj.magic h

(** val arm_extra_op_construct :
    positive -> arm_extra_op_fields_t -> arm_extra_op option **)

let arm_extra_op_construct p b =
  match p with
  | Coq_xI _ -> Some (Osmart_li (Obj.magic b))
  | Coq_xO x ->
    (match x with
     | Coq_xI _ -> None
     | Coq_xO _ -> Some (Osmart_li_cc (Obj.magic b))
     | Coq_xH -> Some Oarm_add_large_imm)
  | Coq_xH -> Some (Oarm_swap (Obj.magic b))

(** val arm_extra_op_induction :
    (wsize -> is_wsize -> 'a1) -> 'a1 -> (wsize -> is_wsize -> 'a1) -> (wsize
    -> is_wsize -> 'a1) -> arm_extra_op -> is_arm_extra_op -> 'a1 **)

let arm_extra_op_induction his_Oarm_swap his_Oarm_add_large_imm his_Osmart_li his_Osmart_li_cc _ = function
| Coq_is_Oarm_swap (x0, p_) -> his_Oarm_swap x0 p_
| Coq_is_Oarm_add_large_imm -> his_Oarm_add_large_imm
| Coq_is_Osmart_li (x0, p_) -> his_Osmart_li x0 p_
| Coq_is_Osmart_li_cc (x0, p_) -> his_Osmart_li_cc x0 p_

(** val arm_extra_op_eqb_fields :
    (arm_extra_op -> arm_extra_op -> bool) -> positive ->
    arm_extra_op_fields_t -> arm_extra_op_fields_t -> bool **)

let arm_extra_op_eqb_fields _ x a b =
  match x with
  | Coq_xO x0 ->
    (match x0 with
     | Coq_xO _ -> (&&) (wsize_eqb (Obj.magic a) (Obj.magic b)) true
     | _ -> true)
  | _ -> (&&) (wsize_eqb (Obj.magic a) (Obj.magic b)) true

(** val arm_extra_op_eqb : arm_extra_op -> arm_extra_op -> bool **)

let arm_extra_op_eqb x1 x2 =
  match x1 with
  | Oarm_swap h ->
    eqb_body arm_extra_op_tag arm_extra_op_fields
      (Obj.magic arm_extra_op_eqb_fields (fun _ _ -> true))
      (arm_extra_op_tag (Oarm_swap h)) h x2
  | Oarm_add_large_imm ->
    eqb_body arm_extra_op_tag arm_extra_op_fields
      (Obj.magic arm_extra_op_eqb_fields (fun _ _ -> true))
      (arm_extra_op_tag Oarm_add_large_imm)
      Box_arm_extra_op_Oarm_add_large_imm x2
  | Osmart_li h ->
    eqb_body arm_extra_op_tag arm_extra_op_fields
      (Obj.magic arm_extra_op_eqb_fields (fun _ _ -> true))
      (arm_extra_op_tag (Osmart_li h)) h x2
  | Osmart_li_cc h ->
    eqb_body arm_extra_op_tag arm_extra_op_fields
      (Obj.magic arm_extra_op_eqb_fields (fun _ _ -> true))
      (arm_extra_op_tag (Osmart_li_cc h)) h x2

(** val arm_extra_op_eqb_OK : arm_extra_op -> arm_extra_op -> reflect **)

let arm_extra_op_eqb_OK =
  iffP2 arm_extra_op_eqb

(** val arm_extra_op_eqb_OK_sumbool : arm_extra_op -> arm_extra_op -> bool **)

let arm_extra_op_eqb_OK_sumbool =
  reflect_dec arm_extra_op_eqb arm_extra_op_eqb_OK

(** val coq_HB_unnamed_factory_1 : arm_extra_op Coq_hasDecEq.axioms_ **)

let coq_HB_unnamed_factory_1 =
  { Coq_hasDecEq.eq_op = arm_extra_op_eqb; Coq_hasDecEq.eqP =
    arm_extra_op_eqb_OK }

(** val arm_extra_arm_extra_op__canonical__eqtype_Equality :
    Equality.coq_type **)

let arm_extra_arm_extra_op__canonical__eqtype_Equality =
  Obj.magic coq_HB_unnamed_factory_1

(** val eqTC_arm_extra_op : arm_extra_op eqTypeC **)

let eqTC_arm_extra_op =
  { beq = arm_extra_op_eqb; ceqP = arm_extra_op_eqb_OK }

(** val coq_Oarm_add_large_imm_instr : instruction_desc **)

let coq_Oarm_add_large_imm_instr =
  let ty = Coq_aword arm_reg_size in
  let cty = eval_atype ty in
  let ctin = cty :: (cty :: []) in
  let semi0 = fun x y ->
    GRing.add
      (GRing.ComRing.Exports.coq_GRing_ComRing__to__GRing_Nmodule
        (word arm_reg_size)) x y
  in
  { str = (fun _ -> "add_large_imm"); tin = (ty :: (ty :: [])); i_in =
  ((ADExplicit ((S O), ACR_any)) :: ((ADExplicit ((S (S O)),
  ACR_any)) :: [])); tout = (ty :: []); i_out = ((ADExplicit (O,
  ACR_any)) :: []); conflicts = (((APout O), (APin O)) :: []); semi =
  (sem_prod_ok ctin (Obj.magic semi0)); i_valid = true; i_safe = [] }

(** val smart_li_instr : wsize -> instruction_desc **)

let smart_li_instr ws =
  { str = (pp_sz "smart_li" ws); tin = ((Coq_aword ws) :: []); i_in =
    ((ADExplicit (O, ACR_any)) :: []); tout = ((Coq_aword ws) :: []); i_out =
    ((ADExplicit ((S O), ACR_any)) :: []); conflicts = []; semi =
    (sem_prod_ok (map eval_atype ((Coq_aword ws) :: []))
      (Obj.magic (fun x -> x))); i_valid = true; i_safe = [] }

(** val smart_li_instr_cc : wsize -> instruction_desc **)

let smart_li_instr_cc ws =
  { str = (pp_sz "smart_li_cc" ws); tin = ((Coq_aword
    ws) :: (Coq_abool :: ((Coq_aword ws) :: []))); i_in = ((ADExplicit (O,
    ACR_any)) :: ((ADExplicit ((S (S O)), ACR_any)) :: ((ADExplicit ((S O),
    ACR_any)) :: []))); tout = ((Coq_aword ws) :: []); i_out = ((ADExplicit
    ((S O), ACR_any)) :: []); conflicts = []; semi =
    (sem_prod_ok
      (map eval_atype ((Coq_aword ws) :: (Coq_abool :: ((Coq_aword
        ws) :: [])))) (Obj.magic (fun x b y -> if b then x else y)));
    i_valid = true; i_safe = [] }

(** val get_instr_desc : arm_extra_op -> instruction_desc **)

let get_instr_desc = function
| Oarm_swap sz -> coq_Oswap_instr (Coq_aword sz)
| Oarm_add_large_imm -> coq_Oarm_add_large_imm_instr
| Osmart_li ws -> smart_li_instr ws
| Osmart_li_cc ws -> smart_li_instr_cc ws

(** val arm_extra_op_decl : arm_extra_op asmOp **)

let arm_extra_op_decl =
  { _eqT = eqTC_arm_extra_op; asm_op_instr = get_instr_desc; prim_string =
    [] }

module E =
 struct
  (** val pass_name : string **)

  let pass_name =
    "asmgen"

  (** val internal_error : instr_info -> string -> pp_error_loc **)

  let internal_error ii msg =
    { pel_msg = (PPEstring msg); pel_fn = None; pel_fi = None; pel_ii = (Some
      ii); pel_vi = None; pel_pass = (Some pass_name); pel_internal = true }

  (** val error : instr_info -> string -> pp_error_loc **)

  let error ii msg =
    { pel_msg = (PPEstring msg); pel_fn = None; pel_fi = None; pel_ii = (Some
      ii); pel_vi = None; pel_pass = (Some pass_name); pel_internal = false }

  (** val li_condition_modified : instr_info -> pp_error_loc **)

  let li_condition_modified ii =
    error ii
      "assignment needs to be split but condition is modified by assignment"
 end

(** val asm_args_of_opn_args :
    ARMFopn_core.opn_args list -> (((register, empty, empty, rflag, condt,
    arm_op) asm_op_msb_t * lexpr list) * rexpr list) list **)

let asm_args_of_opn_args =
  map (fun pat ->
    let (y, res) = pat in let (les, aop) = y in (((None, aop), les), res))

(** val uncons : instr_info -> 'a1 list -> ('a1 * 'a1 list) cexec **)

let uncons ii = function
| [] -> Error (E.internal_error ii "invalid uncons")
| x :: xs0 -> Ok (x, xs0)

(** val uncons_LLvar :
    instr_info -> lexpr list -> (var_i * lexpr list) cexec **)

let uncons_LLvar ii = function
| [] -> Error (E.internal_error ii "invalid lvals")
| l :: les0 ->
  (match l with
   | Store (_, _, _) -> Error (E.internal_error ii "invalid lvals")
   | LLvar x -> Ok (x, les0))

(** val uncons_rvar :
    instr_info -> rexpr list -> (var_i * rexpr list) cexec **)

let uncons_rvar ii = function
| [] -> Error (E.internal_error ii "invalid arguments")
| r :: res0 ->
  (match r with
   | Load (_, _, _) -> Error (E.internal_error ii "invalid arguments")
   | Rexpr f ->
     (match f with
      | Fvar x -> Ok (x, res0)
      | _ -> Error (E.internal_error ii "invalid arguments")))

(** val uncons_wconst :
    instr_info -> rexpr list -> (coq_Z * rexpr list) cexec **)

let uncons_wconst ii = function
| [] -> Error (E.internal_error ii "invalid arguments")
| r :: res' ->
  (match r with
   | Load (_, _, _) -> Error (E.internal_error ii "invalid arguments")
   | Rexpr f ->
     (match f with
      | Fapp1 (s, f0) ->
        (match s with
         | Oword_of_int _ ->
           (match f0 with
            | Fconst imm -> Ok (imm, res')
            | _ -> Error (E.internal_error ii "invalid arguments"))
         | _ -> Error (E.internal_error ii "invalid arguments"))
      | _ -> Error (E.internal_error ii "invalid arguments")))

(** val smart_li_args :
    instr_info -> wsize -> lexpr list -> rexpr list -> (pp_error_loc,
    (var_i * coq_Z) * rexpr list) result **)

let smart_li_args ii ws les res =
  if eq_op wsize_wsize__canonical__eqtype_Equality (Obj.magic ws)
       (Obj.magic arm_decl.reg_size)
  then (match uncons_LLvar ii les with
        | Ok x ->
          let (x0, les0) = x in
          if convertible (Var.vtype x0.v_var) (Coq_aword ws)
          then if nilp les0
               then (match uncons_wconst ii res with
                     | Ok x1 -> let (imm, res0) = x1 in Ok ((x0, imm), res0)
                     | Error s -> Error s)
               else let s = E.internal_error ii "invalid lvals" in Error s
          else let s = E.internal_error ii "invalid type" in Error s
        | Error s -> Error s)
  else let s =
         E.error ii
           "smart immediate assignment is only valid for u32 variables"
       in
       Error s

(** val assemble_smart_li :
    instr_info -> wsize -> lexpr list -> rexpr list -> (pp_error_loc,
    (((register, empty, empty, rflag, condt, arm_op) asm_op_msb_t * lexpr
    list) * rexpr list) list) result **)

let assemble_smart_li ii ws les res =
  match smart_li_args ii ws les res with
  | Ok x ->
    let (y, _) = x in
    let (x0, imm) = y in Ok (asm_args_of_opn_args (ARMFopn_core.li x0 imm))
  | Error s -> Error s

(** val assemble_smart_li_cc :
    instr_info -> wsize -> lexpr list -> rexpr list -> (((register, empty,
    empty, rflag, condt, arm_op) asm_op_msb_t * lexpr list) * rexpr list)
    list cexec **)

let assemble_smart_li_cc ii ws les res =
  match smart_li_args ii ws les res with
  | Ok x ->
    let (y, res0) = x in
    let (x0, imm) = y in
    (match uncons ii res0 with
     | Ok x1 ->
       let (cond, res1) = x1 in
       if negb (SvExtra.Sv.mem (Obj.magic x0.v_var) (free_vars_r cond))
       then (match uncons_rvar ii res1 with
             | Ok x2 ->
               let (oldx, _) = x2 in
               let mk = fun pat ->
                 let (y0, res2) = pat in
                 let (les0, y1) = y0 in
                 let ARM_op (mn, opts) = y1 in
                 let opts0 = set_is_conditional opts in
                 Ok (((None, (ARM_op (mn, opts0))), les0),
                 (cat res2 (cond :: ((rvar oldx) :: []))))
               in
               mapM mk (ARMFopn_core.li x0 imm)
             | Error s -> Error s)
       else let s = E.li_condition_modified ii in Error s
     | Error s -> Error s)
  | Error s -> Error s

(** val assemble_extra :
    instr_info -> arm_extra_op -> lexpr list -> rexpr list -> (((register,
    empty, empty, rflag, condt, arm_op) asm_op_msb_t * lexpr list) * rexpr
    list) list cexec **)

let assemble_extra ii o outx inx =
  match o with
  | Oarm_swap sz ->
    if eq_op wsize_wsize__canonical__eqtype_Equality (Obj.magic sz)
         (Obj.magic U32)
    then (match outx with
          | [] ->
            Error
              (E.error ii
                "only register is accepted on source and destination of the swap instruction on arm")
          | l :: l0 ->
            (match l with
             | Store (_, _, _) ->
               Error
                 (E.error ii
                   "only register is accepted on source and destination of the swap instruction on arm")
             | LLvar x ->
               (match l0 with
                | [] ->
                  Error
                    (E.error ii
                      "only register is accepted on source and destination of the swap instruction on arm")
                | l1 :: l2 ->
                  (match l1 with
                   | Store (_, _, _) ->
                     Error
                       (E.error ii
                         "only register is accepted on source and destination of the swap instruction on arm")
                   | LLvar y ->
                     (match l2 with
                      | [] ->
                        (match inx with
                         | [] ->
                           Error
                             (E.error ii
                               "only register is accepted on source and destination of the swap instruction on arm")
                         | r :: l3 ->
                           (match r with
                            | Load (_, _, _) ->
                              Error
                                (E.error ii
                                  "only register is accepted on source and destination of the swap instruction on arm")
                            | Rexpr f ->
                              (match f with
                               | Fvar z ->
                                 (match l3 with
                                  | [] ->
                                    Error
                                      (E.error ii
                                        "only register is accepted on source and destination of the swap instruction on arm")
                                  | r0 :: l4 ->
                                    (match r0 with
                                     | Load (_, _, _) ->
                                       Error
                                         (E.error ii
                                           "only register is accepted on source and destination of the swap instruction on arm")
                                     | Rexpr f0 ->
                                       (match f0 with
                                        | Fvar w ->
                                          (match l4 with
                                           | [] ->
                                             if negb
                                                  (eq_op
                                                    Var.coq_MvMake_var__canonical__eqtype_Equality
                                                    (Obj.magic x.v_var)
                                                    (Obj.magic w.v_var))
                                             then if negb
                                                       (eq_op
                                                         Var.coq_MvMake_var__canonical__eqtype_Equality
                                                         (Obj.magic y.v_var)
                                                         (Obj.magic x.v_var))
                                                  then if all (fun x0 ->
                                                            convertible
                                                              (Var.vtype
                                                                x0.v_var)
                                                              (Coq_aword U32))
                                                            (x :: (y :: (z :: (w :: []))))
                                                       then Ok ((((None,
                                                              (ARM_op (EOR,
                                                              default_opts))),
                                                              ((LLvar
                                                              x) :: [])),
                                                              ((Rexpr (Fvar
                                                              z)) :: ((Rexpr
                                                              (Fvar
                                                              w)) :: []))) :: ((((None,
                                                              (ARM_op (EOR,
                                                              default_opts))),
                                                              ((LLvar
                                                              y) :: [])),
                                                              ((Rexpr (Fvar
                                                              x)) :: ((Rexpr
                                                              (Fvar
                                                              w)) :: []))) :: ((((None,
                                                              (ARM_op (EOR,
                                                              default_opts))),
                                                              ((LLvar
                                                              x) :: [])),
                                                              ((Rexpr (Fvar
                                                              x)) :: ((Rexpr
                                                              (Fvar
                                                              y)) :: []))) :: [])))
                                                       else let s =
                                                              E.error ii
                                                                "arm swap only valid for register of type u32"
                                                            in
                                                            Error s
                                                  else let s =
                                                         E.internal_error ii
                                                           "bad arm swap : y = x"
                                                       in
                                                       Error s
                                             else let s =
                                                    E.internal_error ii
                                                      "bad arm swap : x = w"
                                                  in
                                                  Error s
                                           | _ :: _ ->
                                             Error
                                               (E.error ii
                                                 "only register is accepted on source and destination of the swap instruction on arm"))
                                        | _ ->
                                          Error
                                            (E.error ii
                                              "only register is accepted on source and destination of the swap instruction on arm"))))
                               | _ ->
                                 Error
                                   (E.error ii
                                     "only register is accepted on source and destination of the swap instruction on arm"))))
                      | _ :: _ ->
                        Error
                          (E.error ii
                            "only register is accepted on source and destination of the swap instruction on arm"))))))
    else Error (E.error ii "arm swap only valid for register of type u32")
  | Oarm_add_large_imm ->
    (match outx with
     | [] ->
       Error
         (E.internal_error ii "bad arm_add_large_imm: invalid args or dests")
     | l :: l0 ->
       (match l with
        | Store (_, _, _) ->
          Error
            (E.internal_error ii
              "bad arm_add_large_imm: invalid args or dests")
        | LLvar x ->
          (match l0 with
           | [] ->
             (match inx with
              | [] ->
                Error
                  (E.internal_error ii
                    "bad arm_add_large_imm: invalid args or dests")
              | r :: l1 ->
                (match r with
                 | Load (_, _, _) ->
                   Error
                     (E.internal_error ii
                       "bad arm_add_large_imm: invalid args or dests")
                 | Rexpr f ->
                   (match f with
                    | Fvar y ->
                      (match l1 with
                       | [] ->
                         Error
                           (E.internal_error ii
                             "bad arm_add_large_imm: invalid args or dests")
                       | r0 :: l2 ->
                         (match r0 with
                          | Load (_, _, _) ->
                            Error
                              (E.internal_error ii
                                "bad arm_add_large_imm: invalid args or dests")
                          | Rexpr f0 ->
                            (match f0 with
                             | Fapp1 (s, f1) ->
                               (match s with
                                | Oword_of_int _ ->
                                  (match f1 with
                                   | Fconst imm ->
                                     (match l2 with
                                      | [] ->
                                        if negb
                                             (eq_op
                                               Var.coq_MvMake_var__canonical__eqtype_Equality
                                               (Obj.magic x.v_var)
                                               (Obj.magic y.v_var))
                                        then if all (fun x0 ->
                                                  convertible
                                                    (Var.vtype x0.v_var)
                                                    (Coq_aword U32))
                                                  (x :: (y :: []))
                                             then Ok
                                                    (asm_args_of_opn_args
                                                      (ARMFopn_core.smart_addi
                                                        x y imm))
                                             else let s0 =
                                                    E.error ii
                                                      "arm swap only valid for register of type u32"
                                                  in
                                                  Error s0
                                        else let s0 =
                                               E.internal_error ii
                                                 "bad arm_add_large_imm: invalid register"
                                             in
                                             Error s0
                                      | _ :: _ ->
                                        Error
                                          (E.internal_error ii
                                            "bad arm_add_large_imm: invalid args or dests"))
                                   | _ ->
                                     Error
                                       (E.internal_error ii
                                         "bad arm_add_large_imm: invalid args or dests"))
                                | _ ->
                                  Error
                                    (E.internal_error ii
                                      "bad arm_add_large_imm: invalid args or dests"))
                             | _ ->
                               Error
                                 (E.internal_error ii
                                   "bad arm_add_large_imm: invalid args or dests"))))
                    | _ ->
                      Error
                        (E.internal_error ii
                          "bad arm_add_large_imm: invalid args or dests"))))
           | _ :: _ ->
             Error
               (E.internal_error ii
                 "bad arm_add_large_imm: invalid args or dests"))))
  | Osmart_li ws -> assemble_smart_li ii ws outx inx
  | Osmart_li_cc ws -> assemble_smart_li_cc ii ws outx inx

(** val arm_extra :
    (register, empty, empty, rflag, condt) arch_toIdent -> (register, empty,
    empty, rflag, condt, arm_op, arm_extra_op) asm_extra **)

let arm_extra atoI =
  { _asm = arm; _atoI = atoI; _extra = arm_extra_op_decl; to_asm =
    assemble_extra }

type arm_extended_op =
  (register, empty, empty, rflag, condt, arm_op, arm_extra_op) extended_op

(** val coq_Oarm :
    (register, empty, empty, rflag, condt) arch_toIdent -> arm_op ->
    arm_extended_op sopn **)

let coq_Oarm _ o =
  Oasm (BaseOp (None, o))

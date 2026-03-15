open BinNums
open Bool
open Datatypes
open Arch_decl
open Arch_extra
open Arch_utils
open Compiler_util
open EqbOK
open Eqb_core_defs
open Eqtype
open Expr
open Fexpr
open Riscv
open Riscv_decl
open Riscv_instr_decl
open Riscv_params_core
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

type riscv_extra_op =
| SWAP of wsize
| Oriscv_add_large_imm

(** val riscv_extra_op_rect :
    (wsize -> 'a1) -> 'a1 -> riscv_extra_op -> 'a1 **)

let riscv_extra_op_rect f f0 = function
| SWAP w -> f w
| Oriscv_add_large_imm -> f0

(** val riscv_extra_op_rec :
    (wsize -> 'a1) -> 'a1 -> riscv_extra_op -> 'a1 **)

let riscv_extra_op_rec f f0 = function
| SWAP w -> f w
| Oriscv_add_large_imm -> f0

type is_riscv_extra_op =
| Coq_is_SWAP of wsize * is_wsize
| Coq_is_Oriscv_add_large_imm

(** val is_riscv_extra_op_rect :
    (wsize -> is_wsize -> 'a1) -> 'a1 -> riscv_extra_op -> is_riscv_extra_op
    -> 'a1 **)

let is_riscv_extra_op_rect f f0 _ = function
| Coq_is_SWAP (w, p_) -> f w p_
| Coq_is_Oriscv_add_large_imm -> f0

(** val is_riscv_extra_op_rec :
    (wsize -> is_wsize -> 'a1) -> 'a1 -> riscv_extra_op -> is_riscv_extra_op
    -> 'a1 **)

let is_riscv_extra_op_rec f f0 _ = function
| Coq_is_SWAP (w, p_) -> f w p_
| Coq_is_Oriscv_add_large_imm -> f0

(** val riscv_extra_op_tag : riscv_extra_op -> positive **)

let riscv_extra_op_tag = function
| SWAP _ -> Coq_xH
| Oriscv_add_large_imm -> Coq_xO Coq_xH

(** val is_riscv_extra_op_inhab : riscv_extra_op -> is_riscv_extra_op **)

let is_riscv_extra_op_inhab = function
| SWAP h -> Coq_is_SWAP (h, (is_wsize_inhab h))
| Oriscv_add_large_imm -> Coq_is_Oriscv_add_large_imm

(** val is_riscv_extra_op_functor :
    riscv_extra_op -> is_riscv_extra_op -> is_riscv_extra_op **)

let rec is_riscv_extra_op_functor _ x =
  x

type box_riscv_extra_op_SWAP =
  wsize
  (* singleton inductive, whose constructor was Box_riscv_extra_op_SWAP *)

(** val coq_Box_riscv_extra_op_SWAP_0 : box_riscv_extra_op_SWAP -> wsize **)

let coq_Box_riscv_extra_op_SWAP_0 record =
  record

type box_riscv_extra_op_Oriscv_add_large_imm =
| Box_riscv_extra_op_Oriscv_add_large_imm

type riscv_extra_op_fields_t = __

(** val riscv_extra_op_fields : riscv_extra_op -> riscv_extra_op_fields_t **)

let riscv_extra_op_fields = function
| SWAP h -> Obj.magic h
| Oriscv_add_large_imm -> Obj.magic Box_riscv_extra_op_Oriscv_add_large_imm

(** val riscv_extra_op_construct :
    positive -> riscv_extra_op_fields_t -> riscv_extra_op option **)

let riscv_extra_op_construct p x =
  match p with
  | Coq_xI _ -> None
  | Coq_xO _ -> Some Oriscv_add_large_imm
  | Coq_xH -> Some (SWAP (Obj.magic x))

(** val riscv_extra_op_induction :
    (wsize -> is_wsize -> 'a1) -> 'a1 -> riscv_extra_op -> is_riscv_extra_op
    -> 'a1 **)

let riscv_extra_op_induction his_SWAP his_Oriscv_add_large_imm _ = function
| Coq_is_SWAP (x0, p_) -> his_SWAP x0 p_
| Coq_is_Oriscv_add_large_imm -> his_Oriscv_add_large_imm

(** val riscv_extra_op_eqb_fields :
    (riscv_extra_op -> riscv_extra_op -> bool) -> positive ->
    riscv_extra_op_fields_t -> riscv_extra_op_fields_t -> bool **)

let riscv_extra_op_eqb_fields _ x x0 x1 =
  match x with
  | Coq_xH -> (&&) (wsize_eqb (Obj.magic x0) (Obj.magic x1)) true
  | _ -> true

(** val riscv_extra_op_eqb : riscv_extra_op -> riscv_extra_op -> bool **)

let riscv_extra_op_eqb x1 x2 =
  match x1 with
  | SWAP h ->
    eqb_body riscv_extra_op_tag riscv_extra_op_fields
      (Obj.magic riscv_extra_op_eqb_fields (fun _ _ -> true))
      (riscv_extra_op_tag (SWAP h)) h x2
  | Oriscv_add_large_imm ->
    eqb_body riscv_extra_op_tag riscv_extra_op_fields
      (Obj.magic riscv_extra_op_eqb_fields (fun _ _ -> true))
      (riscv_extra_op_tag Oriscv_add_large_imm)
      Box_riscv_extra_op_Oriscv_add_large_imm x2

(** val riscv_extra_op_eqb_OK :
    riscv_extra_op -> riscv_extra_op -> reflect **)

let riscv_extra_op_eqb_OK =
  iffP2 riscv_extra_op_eqb

(** val riscv_extra_op_eqb_OK_sumbool :
    riscv_extra_op -> riscv_extra_op -> bool **)

let riscv_extra_op_eqb_OK_sumbool =
  reflect_dec riscv_extra_op_eqb riscv_extra_op_eqb_OK

(** val coq_HB_unnamed_factory_1 : riscv_extra_op Coq_hasDecEq.axioms_ **)

let coq_HB_unnamed_factory_1 =
  { Coq_hasDecEq.eq_op = riscv_extra_op_eqb; Coq_hasDecEq.eqP =
    riscv_extra_op_eqb_OK }

(** val riscv_extra_riscv_extra_op__canonical__eqtype_Equality :
    Equality.coq_type **)

let riscv_extra_riscv_extra_op__canonical__eqtype_Equality =
  Obj.magic coq_HB_unnamed_factory_1

(** val eqTC_riscv_extra_op : riscv_extra_op eqTypeC **)

let eqTC_riscv_extra_op =
  { beq = riscv_extra_op_eqb; ceqP = riscv_extra_op_eqb_OK }

(** val coq_Oriscv_add_large_imm_instr : instruction_desc **)

let coq_Oriscv_add_large_imm_instr =
  let ty = Coq_aword riscv_reg_size in
  let cty = eval_atype ty in
  let ctin = cty :: (cty :: []) in
  let semi0 = fun x y ->
    GRing.add
      (GRing.ComRing.Exports.coq_GRing_ComRing__to__GRing_Nmodule
        (word riscv_reg_size)) x y
  in
  { str = (fun _ -> "add_large_imm"); tin = (ty :: (ty :: [])); i_in =
  ((ADExplicit ((S O), ACR_any)) :: ((ADExplicit ((S (S O)),
  ACR_any)) :: [])); tout = (ty :: []); i_out = ((ADExplicit (O,
  ACR_any)) :: []); conflicts = (((APout O), (APin O)) :: []); semi =
  (sem_prod_ok ctin (Obj.magic semi0)); i_valid = true; i_safe = [] }

(** val get_instr_desc : riscv_extra_op -> instruction_desc **)

let get_instr_desc = function
| SWAP ws -> coq_Oswap_instr (Coq_aword ws)
| Oriscv_add_large_imm -> coq_Oriscv_add_large_imm_instr

(** val riscv_extra_op_decl : riscv_extra_op asmOp **)

let riscv_extra_op_decl =
  { _eqT = eqTC_riscv_extra_op; asm_op_instr = get_instr_desc; prim_string =
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
 end

(** val asm_args_of_opn_args :
    RISCVFopn_core.opn_args list -> (((register, empty, empty, empty, condt,
    riscv_op) asm_op_msb_t * lexpr list) * rexpr list) list **)

let asm_args_of_opn_args =
  map (fun pat ->
    let (y, res) = pat in let (les, aop) = y in (((None, aop), les), res))

(** val assemble_extra :
    instr_info -> riscv_extra_op -> lexpr list -> rexpr list -> (((register,
    empty, empty, empty, condt, riscv_op) asm_op_msb_t * lexpr list) * rexpr
    list) list cexec **)

let assemble_extra ii o outx inx =
  match o with
  | SWAP sz ->
    if eq_op wsize_wsize__canonical__eqtype_Equality (Obj.magic sz)
         (Obj.magic U32)
    then (match outx with
          | [] ->
            Error
              (E.error ii
                "only register is accepted on source and destination of the swap instruction on RISC-V")
          | l :: l0 ->
            (match l with
             | Store (_, _, _) ->
               Error
                 (E.error ii
                   "only register is accepted on source and destination of the swap instruction on RISC-V")
             | LLvar x ->
               (match l0 with
                | [] ->
                  Error
                    (E.error ii
                      "only register is accepted on source and destination of the swap instruction on RISC-V")
                | l1 :: l2 ->
                  (match l1 with
                   | Store (_, _, _) ->
                     Error
                       (E.error ii
                         "only register is accepted on source and destination of the swap instruction on RISC-V")
                   | LLvar y ->
                     (match l2 with
                      | [] ->
                        (match inx with
                         | [] ->
                           Error
                             (E.error ii
                               "only register is accepted on source and destination of the swap instruction on RISC-V")
                         | r :: l3 ->
                           (match r with
                            | Load (_, _, _) ->
                              Error
                                (E.error ii
                                  "only register is accepted on source and destination of the swap instruction on RISC-V")
                            | Rexpr f ->
                              (match f with
                               | Fvar z ->
                                 (match l3 with
                                  | [] ->
                                    Error
                                      (E.error ii
                                        "only register is accepted on source and destination of the swap instruction on RISC-V")
                                  | r0 :: l4 ->
                                    (match r0 with
                                     | Load (_, _, _) ->
                                       Error
                                         (E.error ii
                                           "only register is accepted on source and destination of the swap instruction on RISC-V")
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
                                                              XOR), ((LLvar
                                                              x) :: [])),
                                                              ((Rexpr (Fvar
                                                              z)) :: ((Rexpr
                                                              (Fvar
                                                              w)) :: []))) :: ((((None,
                                                              XOR), ((LLvar
                                                              y) :: [])),
                                                              ((Rexpr (Fvar
                                                              x)) :: ((Rexpr
                                                              (Fvar
                                                              w)) :: []))) :: ((((None,
                                                              XOR), ((LLvar
                                                              x) :: [])),
                                                              ((Rexpr (Fvar
                                                              x)) :: ((Rexpr
                                                              (Fvar
                                                              y)) :: []))) :: [])))
                                                       else let s =
                                                              E.error ii
                                                                "RISC-V swap only valid for register of type u32"
                                                            in
                                                            Error s
                                                  else let s =
                                                         E.internal_error ii
                                                           "bad RISC-V swap : y = x"
                                                       in
                                                       Error s
                                             else let s =
                                                    E.internal_error ii
                                                      "bad RISC-V swap : x = w"
                                                  in
                                                  Error s
                                           | _ :: _ ->
                                             Error
                                               (E.error ii
                                                 "only register is accepted on source and destination of the swap instruction on RISC-V"))
                                        | _ ->
                                          Error
                                            (E.error ii
                                              "only register is accepted on source and destination of the swap instruction on RISC-V"))))
                               | _ ->
                                 Error
                                   (E.error ii
                                     "only register is accepted on source and destination of the swap instruction on RISC-V"))))
                      | _ :: _ ->
                        Error
                          (E.error ii
                            "only register is accepted on source and destination of the swap instruction on RISC-V"))))))
    else Error (E.error ii "RISC-V swap only valid for register of type u32")
  | Oriscv_add_large_imm ->
    (match outx with
     | [] ->
       Error
         (E.internal_error ii
           "bad riscv_add_large_imm: invalid args or dests")
     | l :: l0 ->
       (match l with
        | Store (_, _, _) ->
          Error
            (E.internal_error ii
              "bad riscv_add_large_imm: invalid args or dests")
        | LLvar x ->
          (match l0 with
           | [] ->
             (match inx with
              | [] ->
                Error
                  (E.internal_error ii
                    "bad riscv_add_large_imm: invalid args or dests")
              | r :: l1 ->
                (match r with
                 | Load (_, _, _) ->
                   Error
                     (E.internal_error ii
                       "bad riscv_add_large_imm: invalid args or dests")
                 | Rexpr f ->
                   (match f with
                    | Fvar y ->
                      (match l1 with
                       | [] ->
                         Error
                           (E.internal_error ii
                             "bad riscv_add_large_imm: invalid args or dests")
                       | r0 :: l2 ->
                         (match r0 with
                          | Load (_, _, _) ->
                            Error
                              (E.internal_error ii
                                "bad riscv_add_large_imm: invalid args or dests")
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
                                                      (RISCVFopn_core.smart_addi
                                                        x y imm))
                                             else let s0 =
                                                    E.error ii
                                                      "riscv_add_large_imm only valid for register of type u32"
                                                  in
                                                  Error s0
                                        else let s0 =
                                               E.internal_error ii
                                                 "bad riscv_add_large_imm: invalid register"
                                             in
                                             Error s0
                                      | _ :: _ ->
                                        Error
                                          (E.internal_error ii
                                            "bad riscv_add_large_imm: invalid args or dests"))
                                   | _ ->
                                     Error
                                       (E.internal_error ii
                                         "bad riscv_add_large_imm: invalid args or dests"))
                                | _ ->
                                  Error
                                    (E.internal_error ii
                                      "bad riscv_add_large_imm: invalid args or dests"))
                             | _ ->
                               Error
                                 (E.internal_error ii
                                   "bad riscv_add_large_imm: invalid args or dests"))))
                    | _ ->
                      Error
                        (E.internal_error ii
                          "bad riscv_add_large_imm: invalid args or dests"))))
           | _ :: _ ->
             Error
               (E.internal_error ii
                 "bad riscv_add_large_imm: invalid args or dests"))))

(** val riscv_extra :
    (register, empty, empty, empty, condt) arch_toIdent -> (register, empty,
    empty, empty, condt, riscv_op, riscv_extra_op) asm_extra **)

let riscv_extra atoI =
  { _asm = riscv; _atoI = atoI; _extra = riscv_extra_op_decl; to_asm =
    assemble_extra }

type riscv_extended_op =
  (register, empty, empty, empty, condt, riscv_op, riscv_extra_op) extended_op

(** val coq_Oriscv :
    (register, empty, empty, empty, condt) arch_toIdent -> riscv_op ->
    riscv_extended_op sopn **)

let coq_Oriscv _ o =
  Oasm (BaseOp (None, o))

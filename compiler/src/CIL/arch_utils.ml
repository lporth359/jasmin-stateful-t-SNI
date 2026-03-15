open Datatypes
open Arch_decl
open Sem_type
open Seq
open Ssrnat
open Type
open Utils0
open Wsize

type empty = |

(** val of_empty : empty -> 'a1 **)

let of_empty _ =
  assert false (* absurd case *)

(** val eqTC_empty : empty eqTypeC **)

let eqTC_empty =
  { beq = of_empty; ceqP = (fun _ _ -> assert false (* absurd case *)) }

(** val finTC_empty : empty finTypeC **)

let finTC_empty =
  { _eqC = eqTC_empty; cenum = [] }

(** val empty_toS : ltype -> empty coq_ToString **)

let empty_toS _ =
  { category = "empty"; _finC = finTC_empty; to_string = of_empty }

(** val ak_reg_reg : i_args_kinds **)

let ak_reg_reg =
  ((CAreg :: []) :: ((CAreg :: []) :: [])) :: []

(** val coq_CAimm_sz : wsize -> arg_kind **)

let coq_CAimm_sz sz =
  CAimm (CAimmC_none, sz)

(** val ak_reg_imm : ('a1, 'a2, 'a3, 'a4, 'a5) arch_decl -> i_args_kinds **)

let ak_reg_imm ad =
  ((CAreg :: []) :: (((coq_CAimm_sz ad.reg_size) :: []) :: [])) :: []

(** val ak_reg_addr : i_args_kinds **)

let ak_reg_addr =
  ((CAreg :: []) :: (((CAmem true) :: []) :: [])) :: []

(** val ak_reg_imm8_imm8 : i_args_kinds **)

let ak_reg_imm8_imm8 =
  ((CAreg :: []) :: (((coq_CAimm_sz U8) :: []) :: (((coq_CAimm_sz U8) :: []) :: []))) :: []

(** val ak_reg_reg_reg : i_args_kinds **)

let ak_reg_reg_reg =
  ((CAreg :: []) :: ((CAreg :: []) :: ((CAreg :: []) :: []))) :: []

(** val ak_reg_reg_imm8_imm8 : i_args_kinds **)

let ak_reg_reg_imm8_imm8 =
  ((CAreg :: []) :: ((CAreg :: []) :: (((coq_CAimm_sz U8) :: []) :: ((
    (coq_CAimm_sz U8) :: []) :: [])))) :: []

(** val ak_reg_reg_reg_reg : i_args_kinds **)

let ak_reg_reg_reg_reg =
  ((CAreg :: []) :: ((CAreg :: []) :: ((CAreg :: []) :: ((CAreg :: []) :: [])))) :: []

(** val behead_tuple :
    ltype list -> ltype list -> sem_tuple exec sem_prod -> sem_tuple exec
    sem_prod **)

let behead_tuple tin tout f =
  match tout with
  | [] ->
    sem_prod_app (map eval_ltype tin) f (fun x ->
      match x with
      | Ok _ -> Ok ()
      | Error s -> Error s)
  | _ :: tout' ->
    (match tout' with
     | [] ->
       sem_prod_app (map eval_ltype tin) f (fun x ->
         match x with
         | Ok _ -> Ok ()
         | Error s -> Error s)
     | _ :: _ ->
       sem_prod_app (map eval_ltype tin) f (fun x ->
         match x with
         | Ok x0 -> let (_, p) = x0 in Ok p
         | Error s -> Error s))

(** val semi_drop2 :
    ltype list -> ltype list -> sem_tuple exec sem_prod -> sem_tuple exec
    sem_prod **)

let semi_drop2 tin tout semi =
  behead_tuple tin (behead tout) (behead_tuple tin tout semi)

(** val semi_drop3 :
    ltype list -> ltype list -> sem_tuple exec sem_prod -> sem_tuple exec
    sem_prod **)

let semi_drop3 tin tout semi =
  behead_tuple tin (behead (behead tout))
    (behead_tuple tin (behead tout) (behead_tuple tin tout semi))

(** val semi_drop4 :
    ltype list -> ltype list -> sem_tuple exec sem_prod -> sem_tuple exec
    sem_prod **)

let semi_drop4 tin tout semi =
  behead_tuple tin (behead (behead (behead tout)))
    (behead_tuple tin (behead (behead tout))
      (behead_tuple tin (behead tout) (behead_tuple tin tout semi)))

(** val idt_drop2 :
    ('a1, 'a2, 'a3, 'a4, 'a5) arch_decl -> ('a1, 'a2, 'a3, 'a4, 'a5)
    instr_desc_t -> ('a1, 'a2, 'a3, 'a4, 'a5) instr_desc_t **)

let idt_drop2 _ idt =
  { id_valid = idt.id_valid; id_msb_flag = idt.id_msb_flag; id_tin =
    idt.id_tin; id_in = idt.id_in; id_tout =
    (iter (S (S O)) behead idt.id_tout); id_out =
    (iter (S (S O)) behead idt.id_out); id_semi =
    (semi_drop2 idt.id_tin idt.id_tout idt.id_semi); id_args_kinds =
    idt.id_args_kinds; id_nargs = idt.id_nargs; id_str_jas = idt.id_str_jas;
    id_safe = idt.id_safe; id_pp_asm = idt.id_pp_asm }

(** val idt_drop3 :
    ('a1, 'a2, 'a3, 'a4, 'a5) arch_decl -> ('a1, 'a2, 'a3, 'a4, 'a5)
    instr_desc_t -> ('a1, 'a2, 'a3, 'a4, 'a5) instr_desc_t **)

let idt_drop3 _ idt =
  { id_valid = idt.id_valid; id_msb_flag = idt.id_msb_flag; id_tin =
    idt.id_tin; id_in = idt.id_in; id_tout =
    (iter (S (S (S O))) behead idt.id_tout); id_out =
    (iter (S (S (S O))) behead idt.id_out); id_semi =
    (semi_drop3 idt.id_tin idt.id_tout idt.id_semi); id_args_kinds =
    idt.id_args_kinds; id_nargs = idt.id_nargs; id_str_jas = idt.id_str_jas;
    id_safe = idt.id_safe; id_pp_asm = idt.id_pp_asm }

(** val idt_drop4 :
    ('a1, 'a2, 'a3, 'a4, 'a5) arch_decl -> ('a1, 'a2, 'a3, 'a4, 'a5)
    instr_desc_t -> ('a1, 'a2, 'a3, 'a4, 'a5) instr_desc_t **)

let idt_drop4 _ idt =
  { id_valid = idt.id_valid; id_msb_flag = idt.id_msb_flag; id_tin =
    idt.id_tin; id_in = idt.id_in; id_tout =
    (iter (S (S (S (S O)))) behead idt.id_tout); id_out =
    (iter (S (S (S (S O)))) behead idt.id_out); id_semi =
    (semi_drop4 idt.id_tin idt.id_tout idt.id_semi); id_args_kinds =
    idt.id_args_kinds; id_nargs = idt.id_nargs; id_str_jas = idt.id_str_jas;
    id_safe = idt.id_safe; id_pp_asm = idt.id_pp_asm }

(** val rtuple_drop5th :
    ctype -> ctype -> ctype -> ctype -> ctype -> sem_tuple ->
    sem_ot * (sem_ot * (sem_ot * sem_ot)) **)

let rtuple_drop5th _ _ _ _ _ xs =
  let (x0, l) = Obj.magic xs in
  let (x1, p) = l in
  let (x2, p0) = p in let (x3, _) = p0 in (x0, (x1, (x2, x3)))

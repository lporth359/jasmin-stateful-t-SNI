open BinInt
open Datatypes
open Arch_decl
open Arch_extra
open Arch_utils
open Compiler_util
open Eqtype
open Expr
open Lea
open Memory_model
open Riscv_decl
open Riscv_extra
open Riscv_instr_decl
open Seq
open Ssrfun
open Type
open Utils0
open Var0
open Word0
open Wsize

module E =
 struct
  (** val pass_name : string **)

  let pass_name =
    "lower_addressing"

  (** val error : string -> pp_error_loc **)

  let error msg =
    { pel_msg = (PPEstring msg); pel_fn = None; pel_fi = None; pel_ii = None;
      pel_vi = None; pel_pass = (Some pass_name); pel_internal = true }
 end

(** val is_one_Lmem :
    lval list -> (((aligned * wsize) * var_info) * pexpr) option **)

let is_one_Lmem = function
| [] -> None
| y :: l ->
  (match y with
   | Lmem (al, ws, vi, e) ->
     (match l with
      | [] -> Some (((al, ws), vi), e)
      | _ :: _ -> None)
   | _ -> None)

(** val is_one_Pload : pexpr list -> ((aligned * wsize) * pexpr) option **)

let is_one_Pload = function
| [] -> None
| y :: l ->
  (match y with
   | Pload (al, ws, e) ->
     (match l with
      | [] -> Some ((al, ws), e)
      | _ :: _ -> None)
   | _ -> None)

(** val compute_addr :
    (register, empty, empty, empty, condt) arch_toIdent -> var_i -> pexpr ->
    (riscv_extended_op instr_r list * pexpr) option **)

let compute_addr atoI tmp e =
  match mk_lea (arch_pd riscv_decl) e with
  | Some lea ->
    (match lea.lea_base with
     | Some base ->
       (match lea.lea_offset with
        | Some off ->
          if eq_op Var.coq_MvMake_var__canonical__eqtype_Equality
               (Obj.magic tmp.v_var) (Obj.magic base.v_var)
          then None
          else (match Ssrfun.Option.map Z.of_nat
                        (shift_of_scale lea.lea_scale) with
                | Some shift ->
                  Some (((Copn (((Lvar tmp) :: []), AT_none,
                    (coq_Oriscv atoI SLLI), ((Pvar
                    (mk_lvar off)) :: ((wconst (arch_pd riscv_decl)
                                         (wrepr (arch_pd riscv_decl) shift)) :: [])))) :: ((Copn
                    (((Lvar tmp) :: []), AT_none, (coq_Oriscv atoI ADD),
                    ((Pvar (mk_lvar base)) :: ((Pvar
                    (mk_lvar tmp)) :: [])))) :: [])),
                    (eaddw (arch_pd riscv_decl) (coq_Plvar tmp)
                      (wconst (arch_pd riscv_decl)
                        (wrepr (arch_pd riscv_decl) lea.lea_disp))))
                | None -> None)
        | None -> None)
     | None -> None)
  | None -> None

(** val lower_addressing_i :
    (register, empty, empty, empty, condt) arch_toIdent -> var_i ->
    (register, empty, empty, empty, condt, riscv_op, riscv_extra_op)
    extended_op instr -> (register, empty, empty, empty, condt, riscv_op,
    riscv_extra_op) extended_op instr list **)

let rec lower_addressing_i atoI tmp i = match i with
| MkI (ii, ir) ->
  (match ir with
   | Copn (xs, t, o, es) ->
     (match is_one_Lmem xs with
      | Some p ->
        let (p0, e) = p in
        let (p1, vi) = p0 in
        let (al, ws) = p1 in
        (match compute_addr atoI tmp e with
         | Some p2 ->
           let (prelude, p3) = p2 in
           map (fun x -> MkI (ii, x))
             (cat prelude ((Copn (((Lmem (al, ws, vi, p3)) :: []), t, o,
               es)) :: []))
         | None -> i :: [])
      | None ->
        (match is_one_Pload es with
         | Some p ->
           let (p0, e) = p in
           let (al, ws) = p0 in
           (match compute_addr atoI tmp e with
            | Some p1 ->
              let (prelude, p2) = p1 in
              map (fun x -> MkI (ii, x))
                (cat prelude ((Copn (xs, t, o, ((Pload (al, ws,
                  p2)) :: []))) :: []))
            | None -> i :: [])
         | None -> i :: []))
   | Cif (b, c1, c2) ->
     let c3 = conc_map (lower_addressing_i atoI tmp) c1 in
     let c4 = conc_map (lower_addressing_i atoI tmp) c2 in
     (MkI (ii, (Cif (b, c3, c4)))) :: []
   | Cfor (x, r, c) ->
     let c0 = conc_map (lower_addressing_i atoI tmp) c in
     (MkI (ii, (Cfor (x, r, c0)))) :: []
   | Cwhile (a, c, e, info, c') ->
     let c0 = conc_map (lower_addressing_i atoI tmp) c in
     let c'0 = conc_map (lower_addressing_i atoI tmp) c' in
     (MkI (ii, (Cwhile (a, c0, e, info, c'0)))) :: []
   | _ -> i :: [])

(** val lower_addressing_c :
    (register, empty, empty, empty, condt) arch_toIdent -> var_i ->
    (register, empty, empty, empty, condt, riscv_op, riscv_extra_op)
    extended_op instr list -> (register, empty, empty, empty, condt,
    riscv_op, riscv_extra_op) extended_op instr list **)

let lower_addressing_c atoI tmp =
  conc_map (lower_addressing_i atoI tmp)

(** val lower_addressing_fd :
    (register, empty, empty, empty, condt) arch_toIdent -> progT -> var_i ->
    (register, empty, empty, empty, condt, riscv_op, riscv_extra_op)
    extended_op fundef -> (pp_error_loc, ((register, empty, empty, empty,
    condt, riscv_op, riscv_extra_op) extended_op, extra_fun_t) _fundef) result **)

let lower_addressing_fd atoI _ tmp f =
  let body = f.f_body in
  if negb
       (SvExtra.Sv.mem (Obj.magic tmp.v_var)
         (read_c (asm_opI (riscv_extra atoI)) body))
  then if negb (SvExtra.Sv.mem (Obj.magic tmp.v_var) (vars_l f.f_res))
       then Ok
              (with_body (asm_opI (riscv_extra atoI)) f
                (lower_addressing_c atoI tmp body))
       else let s = E.error "fresh variable not fresh (res)" in Error s
  else let s = E.error "fresh variable not fresh (body)" in Error s

(** val lower_addressing_prog :
    (register, empty, empty, empty, condt) arch_toIdent -> progT -> (string
    -> atype -> Ident.Ident.ident) -> (register, empty, empty, empty, condt,
    riscv_op, riscv_extra_op) extended_op prog -> (register, empty, empty,
    empty, condt, riscv_op, riscv_extra_op) extended_op prog cexec **)

let lower_addressing_prog atoI pT fresh_reg p =
  let tmp = { v_var = { Var.vtype = (Coq_aword (arch_pd riscv_decl));
    Var.vname = (fresh_reg "__tmp__" (Coq_aword (arch_pd riscv_decl))) };
    v_info = dummy_var_info }
  in
  (match map_cfprog_gen (fun x -> x.f_info) (lower_addressing_fd atoI pT tmp)
           p.p_funcs with
   | Ok x -> Ok { p_funcs = x; p_globs = p.p_globs; p_extra = p.p_extra }
   | Error s -> Error s)

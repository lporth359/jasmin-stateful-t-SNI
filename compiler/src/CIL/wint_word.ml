open Allocation
open Compiler_util
open Expr
open Sem_type
open Seq
open Sopn
open Utils0
open Var0
open Wsize

(** val wi2w_wiop1 : signedness -> wiop1 -> pexpr -> pexpr **)

let wi2w_wiop1 s o e =
  match o with
  | WIwint_of_int sz -> Papp1 ((Oword_of_int sz), e)
  | WIint_of_wint sz -> Papp1 ((Oint_of_word (s, sz)), e)
  | WIwint_ext (sz1, sz2) ->
    let o0 =
      match s with
      | Signed -> Osignext (sz1, sz2)
      | Unsigned -> Ozeroext (sz1, sz2)
    in
    Papp1 (o0, e)
  | WIneg sz -> Papp1 ((Oneg (Op_w sz)), e)
  | _ -> e

(** val wi2w_op1 : sop1 -> pexpr -> pexpr **)

let wi2w_op1 o e =
  match o with
  | Owi1 (s, o0) -> wi2w_wiop1 s o0 e
  | _ -> Papp1 (o, e)

(** val wi2w_wiop2 : signedness -> wsize -> wiop2 -> sop2 **)

let wi2w_wiop2 s sz = function
| WIadd -> Oadd (Op_w sz)
| WImul -> Omul (Op_w sz)
| WIsub -> Osub (Op_w sz)
| WIdiv -> Odiv (s, (Op_w sz))
| WImod -> Omod (s, (Op_w sz))
| WIshl -> Olsl (Op_w sz)
| WIshr -> (match s with
            | Signed -> Oasr (Op_w sz)
            | Unsigned -> Olsr sz)
| WIeq -> Oeq (Op_w sz)
| WIneq -> Oneq (Op_w sz)
| WIlt -> Olt (Cmp_w (s, sz))
| WIle -> Ole (Cmp_w (s, sz))
| WIgt -> Ogt (Cmp_w (s, sz))
| WIge -> Oge (Cmp_w (s, sz))

(** val wi2w_op2 : sop2 -> sop2 **)

let wi2w_op2 o = match o with
| Owi2 (s, sz, o0) -> wi2w_wiop2 s sz o0
| _ -> o

(** val wi2w_e : pexpr -> pexpr **)

let rec wi2w_e e = match e with
| Pget (al, aa, ws, x, e0) -> Pget (al, aa, ws, x, (wi2w_e e0))
| Psub (al, ws, len, x, e0) -> Psub (al, ws, len, x, (wi2w_e e0))
| Pload (al, ws, e0) -> Pload (al, ws, (wi2w_e e0))
| Papp1 (o, e0) -> wi2w_op1 o (wi2w_e e0)
| Papp2 (o, e1, e2) -> Papp2 ((wi2w_op2 o), (wi2w_e e1), (wi2w_e e2))
| PappN (o, es) -> PappN (o, (map wi2w_e es))
| Pif (ty, e1, e2, e3) -> Pif (ty, (wi2w_e e1), (wi2w_e e2), (wi2w_e e3))
| _ -> e

(** val wi2w_lv : lval -> lval **)

let wi2w_lv = function
| Lmem (al, ws, x0, e) -> Lmem (al, ws, x0, (wi2w_e e))
| Laset (al, aa, ws, vi, e) -> Laset (al, aa, ws, vi, (wi2w_e e))
| Lasub (aa, ws, len, x0, e) -> Lasub (aa, ws, len, x0, (wi2w_e e))
| x0 -> x0

(** val wi2w_i : 'a1 asmOp -> 'a1 instr -> 'a1 instr **)

let wi2w_i _ =
  let rec wi2w_ir = function
  | Cassgn (x, tag, ty, e) -> Cassgn ((wi2w_lv x), tag, ty, (wi2w_e e))
  | Copn (xs, t0, o, es) -> Copn ((map wi2w_lv xs), t0, o, (map wi2w_e es))
  | Csyscall (xs, o, es) -> Csyscall ((map wi2w_lv xs), o, (map wi2w_e es))
  | Cif (b, c1, c2) -> Cif ((wi2w_e b), (map wi2w_i0 c1), (map wi2w_i0 c2))
  | Cfor (x, r, c) ->
    let (p, e2) = r in
    let (dir, e1) = p in
    Cfor (x, ((dir, (wi2w_e e1)), (wi2w_e e2)), (map wi2w_i0 c))
  | Cwhile (a, c, e, info, c') ->
    Cwhile (a, (map wi2w_i0 c), (wi2w_e e), info, (map wi2w_i0 c'))
  | Ccall (xs, f, es) -> Ccall ((map wi2w_lv xs), f, (map wi2w_e es))
  and wi2w_i0 = function
  | MkI (ii, ir) -> MkI (ii, (wi2w_ir ir))
  in wi2w_i0

(** val wi2w_fun : 'a1 asmOp -> ('a1, 'a2) _fundef -> ('a1, 'a2) _fundef **)

let wi2w_fun asmop f =
  with_body asmop f (map (wi2w_i asmop) f.f_body)

(** val wi2w_prog_internal : 'a1 asmOp -> progT -> 'a1 prog -> 'a1 prog **)

let wi2w_prog_internal asmop pT p =
  map_prog asmop pT (wi2w_fun asmop) p

(** val wi2w_prog :
    'a1 asmOp -> coq_WithSubWord -> (funname -> 'a1 fundef -> 'a1 fundef) ->
    ('a1 fun_decl -> instr_info -> SvExtra.Sv.t) -> 'a1 prog ->
    (pp_error_loc, 'a1 prog) result **)

let wi2w_prog asmop wsw remove_wint_annot dead_vars_fd p =
  let p0 = wi2w_prog_internal asmop progUnit p in
  let pv = map_prog_name asmop progUnit remove_wint_annot p0 in
  (match check_uprog wsw asmop dead_vars_fd p0.p_extra p0.p_funcs pv.p_extra
           pv.p_funcs with
   | Ok _ -> Ok pv
   | Error s -> Error s)

open BinPos
open Bool
open Datatypes
open Arch_decl
open Compiler_util
open Eqtype
open Expr
open Fexpr
open Sem_type
open Seq
open Sopn
open Type
open Utils0
open Var0
open Wsize
open Xseq

let __ = let rec f _ = Obj.repr f in Obj.repr f

type 't coq_ToIdent = { to_ident : ('t -> Ident.Ident.ident);
                        of_ident : (Ident.Ident.ident -> 't option) }

(** val to_ident :
    ltype -> 'a1 coq_ToString -> 'a1 coq_ToIdent -> 'a1 -> Ident.Ident.ident **)

let to_ident _ _ toIdent =
  toIdent.to_ident

(** val of_ident :
    ltype -> 'a1 coq_ToString -> 'a1 coq_ToIdent -> Ident.Ident.ident -> 'a1
    option **)

let of_ident _ _ toIdent =
  toIdent.of_ident

(** val to_var :
    ltype -> 'a1 coq_ToString -> 'a1 coq_ToIdent -> 'a1 -> Var.var **)

let to_var t tS toI r =
  { Var.vtype = (atype_of_ltype (rtype t tS)); Var.vname = (toI.to_ident r) }

(** val of_var :
    ltype -> 'a1 coq_ToString -> 'a1 coq_ToIdent -> Var.var -> 'a1 option **)

let of_var t tS toI v =
  if eq_op type_atype__canonical__eqtype_Equality (Obj.magic Var.vtype v)
       (Obj.magic atype_of_ltype (rtype t tS))
  then toI.of_ident (Var.vname v)
  else None

module type MkToIdent_T =
 sig
  val mk :
    ltype -> 'a1 coq_ToString -> (string -> Ident.Ident.ident) ->
    (pp_error_loc, 'a1 coq_ToIdent) result
 end

module MkToIdent =
 struct
  (** val mk :
      ltype -> 'a1 coq_ToString -> (string -> Ident.Ident.ident) ->
      (pp_error_loc, 'a1 coq_ToIdent) result **)

  let mk _ tS mk_id =
    let rid = map (fun r -> (r, (mk_id (tS.to_string r)))) tS._finC.cenum in
    let t_eqType = ceqT_eqType tS._finC._eqC in
    let to_ident0 = fun r ->
      match assoc t_eqType (Obj.magic rid) r with
      | Some id -> id
      | None -> assert false (* absurd case *)
    in
    let ids = unzip2 rid in
    let rtbl =
      foldr (fun pat t ->
        let (r, id) = pat in Ident.Ident.Mid.set t (Obj.magic id) r)
        Ident.Ident.Mid.empty rid
    in
    let of_ident0 = fun x -> Ident.Ident.Mid.get rtbl x in
    if uniq Ident.ident_eqType (Obj.magic ids)
    then Ok { to_ident = (Obj.magic to_ident0); of_ident =
           (Obj.magic of_ident0) }
    else Error (pp_internal_error_s "to_ident generation" tS.category)
 end

type ('reg, 'regx, 'xreg, 'rflag, 'cond) arch_toIdent = { toI_r : 'reg
                                                                  coq_ToIdent;
                                                          toI_rx : 'regx
                                                                   coq_ToIdent;
                                                          toI_x : 'xreg
                                                                  coq_ToIdent;
                                                          toI_f : 'rflag
                                                                  coq_ToIdent }

(** val toI_r :
    ('a1, 'a2, 'a3, 'a4, 'a5) arch_decl -> ('a1, 'a2, 'a3, 'a4, 'a5)
    arch_toIdent -> 'a1 coq_ToIdent **)

let toI_r _ arch_toIdent0 =
  arch_toIdent0.toI_r

(** val toI_rx :
    ('a1, 'a2, 'a3, 'a4, 'a5) arch_decl -> ('a1, 'a2, 'a3, 'a4, 'a5)
    arch_toIdent -> 'a2 coq_ToIdent **)

let toI_rx _ arch_toIdent0 =
  arch_toIdent0.toI_rx

(** val toI_x :
    ('a1, 'a2, 'a3, 'a4, 'a5) arch_decl -> ('a1, 'a2, 'a3, 'a4, 'a5)
    arch_toIdent -> 'a3 coq_ToIdent **)

let toI_x _ arch_toIdent0 =
  arch_toIdent0.toI_x

(** val toI_f :
    ('a1, 'a2, 'a3, 'a4, 'a5) arch_decl -> ('a1, 'a2, 'a3, 'a4, 'a5)
    arch_toIdent -> 'a4 coq_ToIdent **)

let toI_f _ arch_toIdent0 =
  arch_toIdent0.toI_f

module type AToIdent_T =
 sig
  val mk :
    ('a1, 'a2, 'a3, 'a4, 'a5) arch_decl -> (reg_kind -> ltype -> string ->
    Ident.Ident.ident) -> (pp_error_loc, ('a1, 'a2, 'a3, 'a4, 'a5)
    arch_toIdent) result
 end

module MkAToIdent =
 struct
  (** val _inj_toI_reg_regx :
      ('a1, 'a2, 'a3, 'a4, 'a5) arch_decl -> 'a1 coq_ToIdent -> 'a2
      coq_ToIdent -> bool **)

  let _inj_toI_reg_regx arch rtI rxtI =
    all (fun r ->
      all (fun rx ->
        negb
          (eq_op Ident.ident_eqType (Obj.magic rtI.to_ident r)
            (Obj.magic rxtI.to_ident rx))) arch.toS_rx._finC.cenum)
      arch.toS_r._finC.cenum

  (** val mk :
      ('a1, 'a2, 'a3, 'a4, 'a5) arch_decl -> (reg_kind -> ltype -> string ->
      Ident.Ident.ident) -> (pp_error_loc, ('a1, 'a2, 'a3, 'a4, 'a5)
      arch_toIdent) result **)

  let mk arch toid =
    match MkToIdent.mk (Coq_lword arch.reg_size) arch.toS_r
            (toid Normal (Coq_lword arch.reg_size)) with
    | Ok x ->
      (match MkToIdent.mk (Coq_lword arch.reg_size) arch.toS_rx
               (toid Extra (Coq_lword arch.reg_size)) with
       | Ok x0 ->
         (match MkToIdent.mk (Coq_lword arch.xreg_size) arch.toS_x
                  (toid Normal (Coq_lword arch.xreg_size)) with
          | Ok x1 ->
            (match MkToIdent.mk Coq_lbool arch.toS_f (toid Normal Coq_lbool) with
             | Ok x2 ->
               if _inj_toI_reg_regx arch x x0
               then Ok { toI_r = x; toI_rx = x0; toI_x = x1; toI_f = x2 }
               else Error
                      (pp_internal_error_s "arch_to_ident generation"
                        "inj_toI_reg_regx")
             | Error s -> Error s)
          | Error s -> Error s)
       | Error s -> Error s)
    | Error s -> Error s
 end

(** val var_of_implicit_arg :
    ('a1, 'a2, 'a3, 'a4, 'a5) arch_decl -> ('a1, 'a2, 'a3, 'a4, 'a5)
    arch_toIdent -> ('a1, 'a2, 'a3, 'a4, 'a5) implicit_arg -> Var.var **)

let var_of_implicit_arg arch atoI = function
| IArflag r -> to_var Coq_lbool arch.toS_f atoI.toI_f r
| IAreg r -> to_var (Coq_lword arch.reg_size) arch.toS_r atoI.toI_r r

(** val sopn_constrained_register :
    ('a1, 'a2, 'a3, 'a4, 'a5) arch_decl -> ('a1, 'a2, 'a3, 'a4, 'a5)
    arch_toIdent -> ('a1, 'a2, 'a3, 'a4, 'a5)
    Arch_decl.arg_constrained_register -> arg_constrained_register **)

let sopn_constrained_register arch atoI = function
| Arch_decl.ACR_any -> ACR_any
| Arch_decl.ACR_exact x ->
  ACR_exact (to_var (Coq_lword arch.reg_size) arch.toS_r atoI.toI_r x)
| ACR_vector x ->
  ACR_exact (to_var (Coq_lword arch.xreg_size) arch.toS_x atoI.toI_x x)
| Arch_decl.ACR_subset s ->
  ACR_subset (map (to_var (Coq_lword arch.reg_size) arch.toS_r atoI.toI_r) s)

(** val sopn_arg_desc :
    ('a1, 'a2, 'a3, 'a4, 'a5) arch_decl -> ('a1, 'a2, 'a3, 'a4, 'a5)
    arch_toIdent -> ('a1, 'a2, 'a3, 'a4, 'a5) Arch_decl.arg_desc -> arg_desc **)

let sopn_arg_desc arch atoI = function
| Arch_decl.ADImplicit ia -> ADImplicit (var_of_implicit_arg arch atoI ia)
| Arch_decl.ADExplicit (_, n, ox) ->
  ADExplicit (n, (sopn_constrained_register arch atoI ox))

type ('reg, 'regx, 'xreg, 'rflag, 'cond, 'asm_op, 'extra_op) asm_extra = { 
_asm : ('reg, 'regx, 'xreg, 'rflag, 'cond, 'asm_op) asm;
_atoI : ('reg, 'regx, 'xreg, 'rflag, 'cond) arch_toIdent;
_extra : 'extra_op asmOp;
to_asm : (instr_info -> 'extra_op -> lexpr list -> rexpr list -> ((('reg,
         'regx, 'xreg, 'rflag, 'cond, 'asm_op) asm_op_msb_t * lexpr
         list) * rexpr list) list cexec) }

(** val _asm :
    ('a1, 'a2, 'a3, 'a4, 'a5, 'a6, 'a7) asm_extra -> ('a1, 'a2, 'a3, 'a4,
    'a5, 'a6) asm **)

let _asm asm_extra0 =
  asm_extra0._asm

(** val _atoI :
    ('a1, 'a2, 'a3, 'a4, 'a5, 'a6, 'a7) asm_extra -> ('a1, 'a2, 'a3, 'a4,
    'a5) arch_toIdent **)

let _atoI asm_extra0 =
  asm_extra0._atoI

(** val _extra :
    ('a1, 'a2, 'a3, 'a4, 'a5, 'a6, 'a7) asm_extra -> 'a7 asmOp **)

let _extra asm_extra0 =
  asm_extra0._extra

(** val to_asm :
    ('a1, 'a2, 'a3, 'a4, 'a5, 'a6, 'a7) asm_extra -> instr_info -> 'a7 ->
    lexpr list -> rexpr list -> ((('a1, 'a2, 'a3, 'a4, 'a5, 'a6)
    asm_op_msb_t * lexpr list) * rexpr list) list cexec **)

let to_asm asm_extra0 =
  asm_extra0.to_asm

type ('reg, 'regx, 'xreg, 'rflag, 'cond, 'asm_op, 'extra_op) extra_op_t =
  'extra_op

type ('reg, 'regx, 'xreg, 'rflag, 'cond, 'asm_op, 'extra_op) extended_op =
| BaseOp of ('reg, 'regx, 'xreg, 'rflag, 'cond, 'asm_op) asm_op_msb_t
| ExtOp of ('reg, 'regx, 'xreg, 'rflag, 'cond, 'asm_op, 'extra_op) extra_op_t

(** val extended_op_beq :
    ('a1, 'a2, 'a3, 'a4, 'a5, 'a6, 'a7) asm_extra -> ('a1, 'a2, 'a3, 'a4,
    'a5, 'a6, 'a7) extended_op -> ('a1, 'a2, 'a3, 'a4, 'a5, 'a6, 'a7)
    extended_op -> bool **)

let extended_op_beq asm_e o1 o2 =
  match o1 with
  | BaseOp o3 ->
    (match o2 with
     | BaseOp o4 ->
       eq_op
         (coq_Datatypes_prod__canonical__eqtype_Equality
           (coq_Datatypes_option__canonical__eqtype_Equality
             wsize_wsize__canonical__eqtype_Equality)
           (ceqT_eqType asm_e._asm._asm_op_decl.Arch_decl._eqT))
         (Obj.magic o3) (Obj.magic o4)
     | ExtOp _ -> false)
  | ExtOp o3 ->
    (match o2 with
     | BaseOp _ -> false
     | ExtOp o4 ->
       eq_op (ceqT_eqType asm_e._extra._eqT) (Obj.magic o3) (Obj.magic o4))

(** val extended_op_eq_axiom :
    ('a1, 'a2, 'a3, 'a4, 'a5, 'a6, 'a7) asm_extra -> ('a1, 'a2, 'a3, 'a4,
    'a5, 'a6, 'a7) extended_op eq_axiom **)

let extended_op_eq_axiom asm_e _top_assumption_ =
  let _evar_0_ = fun o1 __top_assumption_ ->
    let _evar_0_ = fun o2 ->
      reflect_inj
        (coq_Datatypes_prod__canonical__eqtype_Equality
          (coq_Datatypes_option__canonical__eqtype_Equality
            wsize_wsize__canonical__eqtype_Equality)
          (ceqT_eqType asm_e._asm._asm_op_decl.Arch_decl._eqT))
        (Obj.magic (fun x -> BaseOp x)) o1 o2
        (eqP
          (coq_Datatypes_prod__canonical__eqtype_Equality
            (coq_Datatypes_option__canonical__eqtype_Equality
              wsize_wsize__canonical__eqtype_Equality)
            (ceqT_eqType asm_e._asm._asm_op_decl.Arch_decl._eqT)) o1 o2)
    in
    let _evar_0_0 = fun _ -> ReflectF in
    (match __top_assumption_ with
     | BaseOp a -> Obj.magic _evar_0_ a
     | ExtOp e -> _evar_0_0 e)
  in
  let _evar_0_0 = fun o1 __top_assumption_ ->
    let _evar_0_0 = fun _ -> ReflectF in
    let _evar_0_1 = fun o2 ->
      reflect_inj (ceqT_eqType asm_e._extra._eqT) (fun x -> ExtOp x) o1 o2
        (eqP (ceqT_eqType asm_e._extra._eqT) o1 o2)
    in
    (match __top_assumption_ with
     | BaseOp a -> _evar_0_0 a
     | ExtOp e -> _evar_0_1 e)
  in
  (match _top_assumption_ with
   | BaseOp a -> Obj.magic _evar_0_ a
   | ExtOp e -> Obj.magic _evar_0_0 e)

(** val coq_HB_unnamed_factory_1 :
    ('a1, 'a2, 'a3, 'a4, 'a5, 'a6, 'a7) asm_extra -> ('a1, 'a2, 'a3, 'a4,
    'a5, 'a6, 'a7) extended_op Coq_hasDecEq.axioms_ **)

let coq_HB_unnamed_factory_1 asm_e =
  { Coq_hasDecEq.eq_op = (extended_op_beq asm_e); Coq_hasDecEq.eqP =
    (extended_op_eq_axiom asm_e) }

(** val arch_extra_extended_op__canonical__eqtype_Equality :
    ('a1, 'a2, 'a3, 'a4, 'a5, 'a6, 'a7) asm_extra -> Equality.coq_type **)

let arch_extra_extended_op__canonical__eqtype_Equality asm_e =
  Obj.magic coq_HB_unnamed_factory_1 asm_e

(** val wsize_eq_dec : wsize -> wsize -> bool **)

let wsize_eq_dec ws1 ws2 =
  let _evar_0_ =
    let _evar_0_ = true in
    let _evar_0_0 = false in
    let _evar_0_1 = false in
    let _evar_0_2 = false in
    let _evar_0_3 = false in
    let _evar_0_4 = false in
    (match ws2 with
     | U8 -> _evar_0_
     | U16 -> _evar_0_0
     | U32 -> _evar_0_1
     | U64 -> _evar_0_2
     | U128 -> _evar_0_3
     | U256 -> _evar_0_4)
  in
  let _evar_0_0 =
    let _evar_0_0 = false in
    let _evar_0_1 = true in
    let _evar_0_2 = false in
    let _evar_0_3 = false in
    let _evar_0_4 = false in
    let _evar_0_5 = false in
    (match ws2 with
     | U8 -> _evar_0_0
     | U16 -> _evar_0_1
     | U32 -> _evar_0_2
     | U64 -> _evar_0_3
     | U128 -> _evar_0_4
     | U256 -> _evar_0_5)
  in
  let _evar_0_1 =
    let _evar_0_1 = false in
    let _evar_0_2 = false in
    let _evar_0_3 = true in
    let _evar_0_4 = false in
    let _evar_0_5 = false in
    let _evar_0_6 = false in
    (match ws2 with
     | U8 -> _evar_0_1
     | U16 -> _evar_0_2
     | U32 -> _evar_0_3
     | U64 -> _evar_0_4
     | U128 -> _evar_0_5
     | U256 -> _evar_0_6)
  in
  let _evar_0_2 =
    let _evar_0_2 = false in
    let _evar_0_3 = false in
    let _evar_0_4 = false in
    let _evar_0_5 = true in
    let _evar_0_6 = false in
    let _evar_0_7 = false in
    (match ws2 with
     | U8 -> _evar_0_2
     | U16 -> _evar_0_3
     | U32 -> _evar_0_4
     | U64 -> _evar_0_5
     | U128 -> _evar_0_6
     | U256 -> _evar_0_7)
  in
  let _evar_0_3 =
    let _evar_0_3 = false in
    let _evar_0_4 = false in
    let _evar_0_5 = false in
    let _evar_0_6 = false in
    let _evar_0_7 = true in
    let _evar_0_8 = false in
    (match ws2 with
     | U8 -> _evar_0_3
     | U16 -> _evar_0_4
     | U32 -> _evar_0_5
     | U64 -> _evar_0_6
     | U128 -> _evar_0_7
     | U256 -> _evar_0_8)
  in
  let _evar_0_4 =
    let _evar_0_4 = false in
    let _evar_0_5 = false in
    let _evar_0_6 = false in
    let _evar_0_7 = false in
    let _evar_0_8 = false in
    let _evar_0_9 = true in
    (match ws2 with
     | U8 -> _evar_0_4
     | U16 -> _evar_0_5
     | U32 -> _evar_0_6
     | U64 -> _evar_0_7
     | U128 -> _evar_0_8
     | U256 -> _evar_0_9)
  in
  (match ws1 with
   | U8 -> _evar_0_
   | U16 -> _evar_0_0
   | U32 -> _evar_0_1
   | U64 -> _evar_0_2
   | U128 -> _evar_0_3
   | U256 -> _evar_0_4)

(** val ctype_eq_dec : ctype -> ctype -> bool **)

let ctype_eq_dec ty1 ty2 =
  let _evar_0_ =
    let _evar_0_ = true in
    let _evar_0_0 = false in
    let _evar_0_1 = fun _ -> false in
    let _evar_0_2 = fun _ -> false in
    (match ty2 with
     | Coq_cbool -> _evar_0_
     | Coq_cint -> _evar_0_0
     | Coq_carr p -> _evar_0_1 p
     | Coq_cword w -> _evar_0_2 w)
  in
  let _evar_0_0 =
    let _evar_0_0 = false in
    let _evar_0_1 = true in
    let _evar_0_2 = fun _ -> false in
    let _evar_0_3 = fun _ -> false in
    (match ty2 with
     | Coq_cbool -> _evar_0_0
     | Coq_cint -> _evar_0_1
     | Coq_carr p -> _evar_0_2 p
     | Coq_cword w -> _evar_0_3 w)
  in
  let _evar_0_1 =
    let _evar_0_1 = fun _ -> false in
    let _evar_0_2 = fun _ -> false in
    let _evar_0_3 = fun p1 p2 ->
      let _evar_0_3 = fun _ -> true in
      let _evar_0_4 = fun _ -> false in
      if Pos.eq_dec p1 p2 then _evar_0_3 __ else _evar_0_4 __
    in
    let _evar_0_4 = fun _ _ -> false in
    (match ty2 with
     | Coq_cbool -> _evar_0_1
     | Coq_cint -> _evar_0_2
     | Coq_carr p -> _evar_0_3 p
     | Coq_cword w -> _evar_0_4 w)
  in
  let _evar_0_2 =
    let _evar_0_2 = fun _ -> false in
    let _evar_0_3 = fun _ -> false in
    let _evar_0_4 = fun _ _ -> false in
    let _evar_0_5 = fun ws1 ws2 ->
      let _evar_0_5 = fun _ -> true in
      let _evar_0_6 = fun _ -> false in
      if wsize_eq_dec ws1 ws2 then _evar_0_5 __ else _evar_0_6 __
    in
    (match ty2 with
     | Coq_cbool -> _evar_0_2
     | Coq_cint -> _evar_0_3
     | Coq_carr p -> _evar_0_4 p
     | Coq_cword w -> _evar_0_5 w)
  in
  (match ty1 with
   | Coq_cbool -> _evar_0_
   | Coq_cint -> _evar_0_0
   | Coq_carr p -> _evar_0_1 p
   | Coq_cword w -> _evar_0_2 w)

(** val semi_to_atype :
    ltype list -> ltype list -> sem_tuple exec sem_prod -> sem_tuple exec
    sem_prod **)

let semi_to_atype _ _ semi0 =
  semi0

(** val get_instr_desc :
    ('a1, 'a2, 'a3, 'a4, 'a5, 'a6, 'a7) asm_extra -> ('a1, 'a2, 'a3, 'a4,
    'a5, 'a6, 'a7) extended_op -> instruction_desc **)

let get_instr_desc asm_e = function
| BaseOp o0 ->
  let id = instr_desc asm_e._asm._arch_decl asm_e._asm._asm_op_decl o0 in
  { str = id.id_str_jas; tin = (map atype_of_ltype id.id_tin); i_in =
  (map (sopn_arg_desc asm_e._asm._arch_decl asm_e._atoI) id.id_in); tout =
  (map atype_of_ltype id.id_tout); i_out =
  (map (sopn_arg_desc asm_e._asm._arch_decl asm_e._atoI) id.id_out);
  conflicts = []; semi = (semi_to_atype id.id_tin id.id_tout id.id_semi);
  i_valid = id.id_valid; i_safe = id.id_safe }
| ExtOp o0 -> asm_e._extra.asm_op_instr o0

(** val sopn_prim_string_base :
    ('a1, 'a2, 'a3, 'a4, 'a5, 'a6, 'a7) asm_extra -> (string * 'a6
    prim_constructor) list -> (string * ('a1, 'a2, 'a3, 'a4, 'a5, 'a6, 'a7)
    extended_op prim_constructor) list **)

let sopn_prim_string_base _ o =
  let to_ex = fun o0 -> BaseOp (None, o0) in
  map (fun pat -> let (s, p) = pat in (s, (map_prim_constructor to_ex p))) o

(** val sopn_prim_string_extra :
    ('a1, 'a2, 'a3, 'a4, 'a5, 'a6, 'a7) asm_extra -> (string * 'a7
    prim_constructor) list -> (string * ('a1, 'a2, 'a3, 'a4, 'a5, 'a6, 'a7)
    extended_op prim_constructor) list **)

let sopn_prim_string_extra _ o =
  let to_ex = fun o0 -> ExtOp o0 in
  map (fun pat -> let (s, p) = pat in (s, (map_prim_constructor to_ex p))) o

(** val get_prime_op :
    ('a1, 'a2, 'a3, 'a4, 'a5, 'a6, 'a7) asm_extra -> (string * ('a1, 'a2,
    'a3, 'a4, 'a5, 'a6, 'a7) extended_op prim_constructor) list **)

let get_prime_op asm_e =
  cat
    (sopn_prim_string_base asm_e
      asm_e._asm._asm_op_decl.Arch_decl.prim_string)
    (sopn_prim_string_extra asm_e asm_e._extra.prim_string)

(** val eqTC_extended_op :
    ('a1, 'a2, 'a3, 'a4, 'a5, 'a6, 'a7) asm_extra -> ('a1, 'a2, 'a3, 'a4,
    'a5, 'a6, 'a7) extended_op eqTypeC **)

let eqTC_extended_op asm_e =
  { beq = (extended_op_beq asm_e); ceqP = (extended_op_eq_axiom asm_e) }

(** val asm_opI :
    ('a1, 'a2, 'a3, 'a4, 'a5, 'a6, 'a7) asm_extra -> ('a1, 'a2, 'a3, 'a4,
    'a5, 'a6, 'a7) extended_op asmOp **)

let asm_opI asm_e =
  { _eqT = (eqTC_extended_op asm_e); asm_op_instr = (get_instr_desc asm_e);
    prim_string = (get_prime_op asm_e) }

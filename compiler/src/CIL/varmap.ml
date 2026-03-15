open Datatypes
open Eqtype
open Sem_type
open Ssrfun
open Type
open Utils0
open Values
open Var0
open Word0
open Wsize

type __ = Obj.t
let __ = let rec f _ = Obj.repr f in Obj.repr f

(** val truncatable : coq_WithSubWord -> bool -> ctype -> value -> bool **)

let truncatable wsw wdb ty = function
| Vbool _ -> (match ty with
              | Coq_cbool -> true
              | _ -> false)
| Vint _ -> (match ty with
             | Coq_cint -> true
             | _ -> false)
| Varr (p, _) ->
  (match ty with
   | Coq_carr p' ->
     eq_op coq_BinNums_positive__canonical__eqtype_Equality (Obj.magic p)
       (Obj.magic p')
   | _ -> false)
| Vword (ws, _) ->
  (match ty with
   | Coq_cword ws' -> (||) (negb wdb) ((||) wsw (cmp_le wsize_cmp ws' ws))
   | _ -> false)
| Vundef t0 -> subctype t0 ty

(** val vm_truncate_val : coq_WithSubWord -> ctype -> value -> value **)

let vm_truncate_val wsw ty v = match v with
| Vbool _ -> (match ty with
              | Coq_cbool -> v
              | _ -> undef_addr ty)
| Vint _ -> (match ty with
             | Coq_cint -> v
             | _ -> undef_addr ty)
| Varr (p, _) ->
  (match ty with
   | Coq_carr p' ->
     if eq_op coq_BinNums_positive__canonical__eqtype_Equality (Obj.magic p)
          (Obj.magic p')
     then v
     else undef_addr ty
   | _ -> undef_addr ty)
| Vword (ws, w) ->
  (match ty with
   | Coq_cword ws' ->
     if (||) wsw (cmp_le wsize_cmp ws' ws)
     then if cmp_le wsize_cmp ws ws'
          then Vword (ws, w)
          else Vword (ws', (zero_extend ws' ws w))
     else undef_addr ty
   | _ -> undef_addr ty)
| Vundef _ -> undef_addr ty

module type VM =
 sig
  type t

  val init : coq_WithSubWord -> t

  val get : coq_WithSubWord -> t -> Var.var -> value

  val set : coq_WithSubWord -> t -> Var.var -> value -> t
 end

module Vm =
 struct
  type t_ =
    value Mvar.t
    (* singleton inductive, whose constructor was Build_t_ *)

  (** val data : coq_WithSubWord -> t_ -> value Mvar.t **)

  let data _ t0 =
    t0

  type t = t_

  (** val init : coq_WithSubWord -> t_ **)

  let init _ =
    Mvar.empty

  (** val get : coq_WithSubWord -> t -> Var.var -> value **)

  let get _ vm x =
    Ssrfun.Option.default (undef_addr (eval_atype (Var.vtype x)))
      (Mvar.get vm (Obj.magic x))

  (** val set : coq_WithSubWord -> t -> Var.var -> value -> t_ **)

  let set wsw vm x v =
    Mvar.set vm (Obj.magic x)
      (vm_truncate_val wsw (eval_atype (Var.vtype x)) v)

  (** val initP : __ **)

  let initP =
    __

  (** val getP : __ **)

  let getP =
    __

  (** val setP : __ **)

  let setP =
    __

  (** val setP_eq : __ **)

  let setP_eq =
    __

  (** val setP_neq : __ **)

  let setP_neq =
    __
 end

(** val set_var :
    coq_WithSubWord -> bool -> Vm.t -> Var.var -> value -> (error, Vm.t)
    result **)

let set_var wsw wdb vm x v =
  if coq_DB wdb v
  then if truncatable wsw wdb (eval_atype (Var.vtype x)) v
       then Ok (Vm.set wsw vm x v)
       else let s = ErrType in Error s
  else let s = ErrAddrUndef in Error s

(** val get_var :
    coq_WithSubWord -> bool -> Vm.t -> Var.var -> (error, value) result **)

let get_var wsw wdb vm x =
  let v = Vm.get wsw vm x in
  if (||) (negb wdb) (is_defined v)
  then Ok v
  else let s = ErrAddrUndef in Error s

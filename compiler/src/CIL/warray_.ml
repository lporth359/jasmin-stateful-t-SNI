open BinInt
open BinNums
open BinPos
open Bool
open Datatypes
open EqbOK
open Eqb_core_defs
open Eqtype
open Gen_map
open Memory_model
open Seq
open Ssralg
open Ssrbool
open Ssrfun
open Ssrnat
open Type
open Utils0
open Word0
open Word_ssrZ
open Wsize

type __ = Obj.t
let __ = let rec f _ = Obj.repr f in Obj.repr f

type arr_access =
| AAdirect
| AAscale

(** val arr_access_rect : 'a1 -> 'a1 -> arr_access -> 'a1 **)

let arr_access_rect f f0 = function
| AAdirect -> f
| AAscale -> f0

(** val arr_access_rec : 'a1 -> 'a1 -> arr_access -> 'a1 **)

let arr_access_rec f f0 = function
| AAdirect -> f
| AAscale -> f0

type is_arr_access =
| Coq_is_AAdirect
| Coq_is_AAscale

(** val is_arr_access_rect :
    'a1 -> 'a1 -> arr_access -> is_arr_access -> 'a1 **)

let is_arr_access_rect f f0 _ = function
| Coq_is_AAdirect -> f
| Coq_is_AAscale -> f0

(** val is_arr_access_rec :
    'a1 -> 'a1 -> arr_access -> is_arr_access -> 'a1 **)

let is_arr_access_rec f f0 _ = function
| Coq_is_AAdirect -> f
| Coq_is_AAscale -> f0

(** val arr_access_tag : arr_access -> positive **)

let arr_access_tag = function
| AAdirect -> Coq_xH
| AAscale -> Coq_xO Coq_xH

(** val is_arr_access_inhab : arr_access -> is_arr_access **)

let is_arr_access_inhab = function
| AAdirect -> Coq_is_AAdirect
| AAscale -> Coq_is_AAscale

(** val is_arr_access_functor :
    arr_access -> is_arr_access -> is_arr_access **)

let rec is_arr_access_functor _ x =
  x

type box_arr_access_AAdirect =
| Box_arr_access_AAdirect

type arr_access_fields_t = __

(** val arr_access_fields : arr_access -> arr_access_fields_t **)

let arr_access_fields _ =
  Obj.magic Box_arr_access_AAdirect

(** val arr_access_construct :
    positive -> arr_access_fields_t -> arr_access option **)

let arr_access_construct p _ =
  match p with
  | Coq_xI _ -> None
  | Coq_xO _ -> Some AAscale
  | Coq_xH -> Some AAdirect

(** val arr_access_induction :
    'a1 -> 'a1 -> arr_access -> is_arr_access -> 'a1 **)

let arr_access_induction his_AAdirect his_AAscale _ = function
| Coq_is_AAdirect -> his_AAdirect
| Coq_is_AAscale -> his_AAscale

(** val arr_access_eqb_fields :
    (arr_access -> arr_access -> bool) -> positive -> arr_access_fields_t ->
    arr_access_fields_t -> bool **)

let arr_access_eqb_fields _ _ _ _ =
  true

(** val arr_access_eqb : arr_access -> arr_access -> bool **)

let arr_access_eqb x1 x2 =
  eqb_body arr_access_tag arr_access_fields
    (Obj.magic arr_access_eqb_fields (fun _ _ -> true)) (arr_access_tag x1)
    Box_arr_access_AAdirect x2

(** val arr_access_eqb_OK : arr_access -> arr_access -> reflect **)

let arr_access_eqb_OK =
  iffP2 arr_access_eqb

(** val arr_access_eqb_OK_sumbool : arr_access -> arr_access -> bool **)

let arr_access_eqb_OK_sumbool =
  reflect_dec arr_access_eqb arr_access_eqb_OK

(** val coq_HB_unnamed_factory_1 : arr_access Coq_hasDecEq.axioms_ **)

let coq_HB_unnamed_factory_1 =
  { Coq_hasDecEq.eq_op = arr_access_eqb; Coq_hasDecEq.eqP =
    arr_access_eqb_OK }

(** val warray__arr_access__canonical__eqtype_Equality : Equality.coq_type **)

let warray__arr_access__canonical__eqtype_Equality =
  Obj.magic coq_HB_unnamed_factory_1

(** val mk_scale : arr_access -> wsize -> coq_Z **)

let mk_scale aa ws =
  match aa with
  | AAdirect -> Zpos Coq_xH
  | AAscale -> wsize_size ws

module WArray =
 struct
  type array =
    GRing.ComRing.sort Mz.t
    (* singleton inductive, whose constructor was Build_array *)

  (** val arr_data : positive -> array -> GRing.ComRing.sort Mz.t **)

  let arr_data _ a =
    a

  (** val empty : positive -> array **)

  let empty _ =
    Mz.empty

  (** val coq_PointerZ : pointer_op **)

  let coq_PointerZ =
    { add = (fun x y -> Obj.magic Z.add x y); sub = (fun x y ->
      Z.sub (Obj.magic x) (Obj.magic y)); p_to_z = (fun x -> Obj.magic x) }

  (** val in_bound : positive -> array -> coq_Z -> bool **)

  let in_bound s _ p =
    (&&) (Z.leb Z0 p) (Z.ltb p (Zpos s))

  (** val in_boundP : positive -> array -> coq_Z -> reflect **)

  let in_boundP s _ p =
    iffP ((&&) (Z.leb Z0 p) (Z.ltb p (Zpos s)))
      (andP (Z.leb Z0 p) (Z.ltb p (Zpos s)))

  (** val is_init : positive -> array -> coq_Z -> bool **)

  let is_init _ m i =
    match Mz.get m (Obj.magic i) with
    | Some _ -> true
    | None -> false

  (** val get8 :
      positive -> array -> coq_Z -> (error, GRing.Nmodule.sort) result **)

  let get8 s m i =
    if in_bound s m i
    then if is_init s m i
         then Ok
                (Ssrfun.Option.default
                  (GRing.zero
                    (GRing.ComRing.Exports.coq_GRing_ComRing__to__GRing_Nmodule
                      (word U8))) (Mz.get m (Obj.magic i)))
         else let s0 = ErrAddrUndef in Error s0
    else let s0 = ErrOob in Error s0

  (** val set8 :
      positive -> array -> coq_Z -> GRing.ComRing.sort -> (error, array)
      result **)

  let set8 s m i v =
    if in_bound s m i
    then Ok (Mz.set m (Obj.magic i) v)
    else let s0 = ErrOob in Error s0

  (** val valid8P :
      positive -> array -> coq_Z -> GRing.ComRing.sort -> reflect **)

  let valid8P s m p _ =
    let _evar_0_ = ReflectT in
    let _evar_0_0 = ReflectF in if in_bound s m p then _evar_0_ else _evar_0_0

  (** val array_CM : positive -> array coreMem **)

  let array_CM s =
    { get = (Obj.magic get8 s); set = (Obj.magic set8 s); valid8 =
      (Obj.magic in_bound s); Memory_model.valid8P = (Obj.magic valid8P s) }

  (** val in_range : positive -> coq_Z -> wsize -> bool **)

  let in_range s p ws =
    (&&) (Z.leb Z0 p) (Z.leb (Z.add p (wsize_size ws)) (Zpos s))

  (** val in_rangeP : positive -> coq_Z -> wsize -> reflect **)

  let in_rangeP s p ws =
    let _evar_0_ = fun _ -> ReflectT in
    let _evar_0_0 = fun _ -> ReflectF in
    (match andP (Z.leb Z0 p) (Z.leb (Z.add p (wsize_size ws)) (Zpos s)) with
     | ReflectT -> _evar_0_ __
     | ReflectF -> _evar_0_0 __)

  (** val get :
      positive -> aligned -> arr_access -> wsize -> array -> coq_Z ->
      GRing.ComRing.sort exec **)

  let get len al aa ws a p =
    CoreMem.read coq_BinNums_Z__canonical__eqtype_Equality coq_PointerZ
      (array_CM len) a al (Obj.magic Z.mul p (mk_scale aa ws)) ws

  (** val set :
      positive -> wsize -> array -> aligned -> arr_access -> coq_Z ->
      GRing.ComRing.sort -> array exec **)

  let set len ws a al aa p v =
    CoreMem.write coq_BinNums_Z__canonical__eqtype_Equality coq_PointerZ
      (array_CM len) a al (Obj.magic Z.mul p (mk_scale aa ws)) ws v

  (** val fcopy :
      wsize -> positive -> array -> array -> coq_Z -> coq_Z -> (error, array)
      result **)

  let fcopy ws len a t0 i j =
    foldM (fun i0 t1 ->
      match get len Aligned AAscale ws a i0 with
      | Ok x -> set len ws t1 Aligned AAscale i0 x
      | Error s -> Error s) t0 (ziota i j)

  (** val copy : wsize -> positive -> array -> (error, array) result **)

  let copy ws p a =
    fcopy ws (Z.to_pos (arr_size ws p)) a (empty (Z.to_pos (arr_size ws p)))
      Z0 (Zpos p)

  (** val fill : positive -> GRing.ComRing.sort list -> array exec **)

  let fill len l =
    if eq_op coq_Datatypes_nat__canonical__eqtype_Equality
         (Obj.magic Pos.to_nat len) (Obj.magic size l)
    then (match foldM (fun w pt ->
                  match set len U8 (snd pt) Aligned AAscale (fst pt) w with
                  | Ok x -> Ok ((Z.add (fst pt) (Zpos Coq_xH)), x)
                  | Error s -> Error s) (Z0, (empty len)) l with
          | Ok x -> Ok (snd x)
          | Error s -> Error s)
    else let s = ErrType in Error s

  (** val get_sub_data :
      arr_access -> wsize -> positive -> GRing.ComRing.sort Mz.t -> coq_Z ->
      GRing.ComRing.sort Mz.t **)

  let get_sub_data aa ws len a p =
    let size0 = arr_size ws len in
    let start = Z.mul p (mk_scale aa ws) in
    foldr (fun i data ->
      match Mz.get a (Obj.magic Z.add start i) with
      | Some w -> Mz.set data (Obj.magic i) w
      | None -> Mz.remove data (Obj.magic i)) Mz.empty (ziota Z0 size0)

  (** val get_sub :
      positive -> arr_access -> wsize -> positive -> array -> coq_Z -> array
      exec **)

  let get_sub lena aa ws len a p =
    let size0 = arr_size ws len in
    let start = Z.mul p (mk_scale aa ws) in
    if (&&) (Z.leb Z0 start) (Z.leb (Z.add start size0) (Zpos lena))
    then Ok (get_sub_data aa ws len a p)
    else Error ErrOob

  (** val set_sub_data :
      arr_access -> wsize -> positive -> GRing.ComRing.sort Mz.t -> coq_Z ->
      GRing.ComRing.sort Mz.t -> GRing.ComRing.sort Mz.t **)

  let set_sub_data aa ws len a p b =
    let size0 = arr_size ws len in
    let start = Z.mul p (mk_scale aa ws) in
    foldr (fun i data ->
      match Mz.get b (Obj.magic i) with
      | Some w -> Mz.set data (Obj.magic Z.add start i) w
      | None -> Mz.remove data (Obj.magic Z.add start i)) a (ziota Z0 size0)

  (** val set_sub :
      positive -> arr_access -> wsize -> positive -> array -> coq_Z -> array
      -> array exec **)

  let set_sub lena aa ws len a p b =
    let size0 = arr_size ws len in
    let start = Z.mul p (mk_scale aa ws) in
    if (&&) (Z.leb Z0 start) (Z.leb (Z.add start size0) (Zpos lena))
    then Ok (set_sub_data aa ws len a p b)
    else Error ErrOob

  (** val cast : positive -> positive -> array -> (error, array) result **)

  let cast len len' a =
    if eq_op coq_BinNums_positive__canonical__eqtype_Equality
         (Obj.magic len') (Obj.magic len)
    then Ok a
    else type_error

  (** val of_list : wsize -> GRing.ComRing.sort list -> array **)

  let of_list ws l =
    let do8 = fun mz w ->
      let (m, z) = mz in ((Mz.set m (Obj.magic z) w), (Z.succ z))
    in
    let dow = fun mz w -> foldl do8 mz (LE.encode ws w) in
    let (m, _) = foldl dow (Mz.empty, Z0) l in m
 end

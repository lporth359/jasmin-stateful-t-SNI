open BinInt
open BinNums
open Datatypes
open Prelude
open Div
open Eqtype
open Expr
open Flag_combination
open Sem_type
open Ssralg
open Type
open Utils0
open Word0
open Word_ssrZ
open Wsize

let __ = let rec f _ = Obj.repr f in Obj.repr f

(** val mk_sem_sop1 : ('a1 -> 'a2) -> 'a1 -> 'a2 exec **)

let mk_sem_sop1 o v1 =
  Ok (o v1)

(** val sem_wiop1_typed : signedness -> wiop1 -> sem_t -> sem_t exec **)

let sem_wiop1_typed sign = function
| WIwint_of_int sz -> Obj.magic wint_of_int sign sz
| WIint_of_wint sz -> mk_sem_sop1 (Obj.magic int_of_word sign sz)
| WIwint_ext (szo, szi) -> mk_sem_sop1 (sem_word_extend sign szo szi)
| WIneg sz -> (fun w -> wint_of_int sign sz (Z.opp (int_of_word sign sz w)))
| _ -> mk_sem_sop1 (fun w -> w)

(** val sem_sop1_typed : sop1 -> sem_t -> sem_t exec **)

let sem_sop1_typed = function
| Oword_of_int sz -> mk_sem_sop1 (Obj.magic wrepr sz)
| Oint_of_word (sign, sz) -> mk_sem_sop1 (Obj.magic int_of_word sign sz)
| Osignext (szo, szi) -> mk_sem_sop1 (sign_extend szo szi)
| Ozeroext (szo, szi) -> mk_sem_sop1 (zero_extend szo szi)
| Onot -> mk_sem_sop1 (Obj.magic negb)
| Olnot sz -> mk_sem_sop1 (wnot sz)
| Oneg o0 ->
  (match o0 with
   | Op_int -> mk_sem_sop1 (Obj.magic Z.opp)
   | Op_w sz ->
     mk_sem_sop1
       (GRing.opp
         (GRing.ComRing.Exports.coq_GRing_ComRing__to__GRing_Zmodule
           (word sz))))
| Owi1 (sign, o0) -> sem_wiop1_typed sign o0

(** val zlsl : coq_Z -> coq_Z -> coq_Z **)

let zlsl x i =
  if Z.leb Z0 i
  then Z.mul x (Z.pow (Zpos (Coq_xO Coq_xH)) i)
  else Z.div x (Z.pow (Zpos (Coq_xO Coq_xH)) (Z.opp i))

(** val zasr : coq_Z -> coq_Z -> coq_Z **)

let zasr x i =
  zlsl x (Z.opp i)

(** val sem_shift :
    (wsize -> GRing.ComRing.sort -> coq_Z -> GRing.ComRing.sort) -> wsize ->
    GRing.ComRing.sort -> GRing.ComRing.sort -> GRing.ComRing.sort **)

let sem_shift shift s v i =
  let i0 = wunsigned U8 i in shift s v i0

(** val sem_shr :
    wsize -> GRing.ComRing.sort -> GRing.ComRing.sort -> GRing.ComRing.sort **)

let sem_shr s =
  sem_shift wshr s

(** val sem_sar :
    wsize -> GRing.ComRing.sort -> GRing.ComRing.sort -> GRing.ComRing.sort **)

let sem_sar s =
  sem_shift wsar s

(** val sem_shl :
    wsize -> GRing.ComRing.sort -> GRing.ComRing.sort -> GRing.ComRing.sort **)

let sem_shl s =
  sem_shift wshl s

(** val sem_ror :
    wsize -> GRing.ComRing.sort -> GRing.ComRing.sort -> GRing.ComRing.sort **)

let sem_ror s =
  sem_shift wror s

(** val sem_rol :
    wsize -> GRing.ComRing.sort -> GRing.ComRing.sort -> GRing.ComRing.sort **)

let sem_rol s =
  sem_shift wrol s

(** val sem_vadd :
    velem -> wsize -> GRing.ComRing.sort -> GRing.ComRing.sort ->
    GRing.ComRing.sort **)

let sem_vadd ve ws =
  lift2_vec (wsize_of_velem ve)
    (GRing.add
      (GRing.ComRing.Exports.coq_GRing_ComRing__to__GRing_Nmodule
        (word (wsize_of_velem ve)))) ws

(** val sem_vsub :
    velem -> wsize -> GRing.ComRing.sort -> GRing.ComRing.sort ->
    GRing.ComRing.sort **)

let sem_vsub ve ws =
  lift2_vec (wsize_of_velem ve) (fun x y ->
    GRing.add
      (GRing.ComRing.Exports.coq_GRing_ComRing__to__GRing_Nmodule
        (word (wsize_of_velem ve))) x
      (GRing.opp
        (GRing.ComRing.Exports.coq_GRing_ComRing__to__GRing_Zmodule
          (word (wsize_of_velem ve))) y)) ws

(** val sem_vmul :
    velem -> wsize -> GRing.ComRing.sort -> GRing.ComRing.sort ->
    GRing.ComRing.sort **)

let sem_vmul ve ws =
  lift2_vec (wsize_of_velem ve)
    (GRing.mul
      (GRing.ComRing.Exports.coq_GRing_ComRing__to__GRing_SemiRing
        (word (wsize_of_velem ve)))) ws

(** val sem_vshr :
    velem -> wsize -> GRing.ComRing.sort -> GRing.ComRing.sort ->
    GRing.ComRing.sort **)

let sem_vshr ve ws v i =
  lift1_vec (wsize_of_velem ve) (fun x ->
    wshr (wsize_of_velem ve) x (wunsigned U128 i)) ws v

(** val sem_vsar :
    velem -> wsize -> GRing.ComRing.sort -> GRing.ComRing.sort ->
    GRing.ComRing.sort **)

let sem_vsar ve ws v i =
  lift1_vec (wsize_of_velem ve) (fun x ->
    wsar (wsize_of_velem ve) x (wunsigned U128 i)) ws v

(** val sem_vshl :
    velem -> wsize -> GRing.ComRing.sort -> GRing.ComRing.sort ->
    GRing.ComRing.sort **)

let sem_vshl ve ws v i =
  lift1_vec (wsize_of_velem ve) (fun x ->
    wshl (wsize_of_velem ve) x (wunsigned U128 i)) ws v

(** val mk_sem_divmod :
    signedness -> wsize -> (GRing.ComRing.sort -> GRing.ComRing.sort ->
    GRing.ComRing.sort) -> GRing.ComRing.sort -> GRing.ComRing.sort ->
    GRing.ComRing.sort exec **)

let mk_sem_divmod si sz o w1 w2 =
  if (||)
       (eq_op
         (GRing.ComRing.Exports.coq_GRing_ComRing__to__eqtype_Equality
           (word sz)) w2
         (GRing.zero
           (GRing.ComRing.Exports.coq_GRing_ComRing__to__GRing_Nmodule
             (word sz))))
       ((&&)
         (eq_op wsize_signedness__canonical__eqtype_Equality (Obj.magic si)
           (Obj.magic Signed))
         ((&&)
           (eq_op coq_BinNums_Z__canonical__eqtype_Equality
             (Obj.magic wsigned sz w1) (Obj.magic wmin_signed sz))
           (eq_op
             (GRing.ComRing.Exports.coq_GRing_ComRing__to__eqtype_Equality
               (word sz)) w2
             (GRing.opp
               (GRing.Ring.Exports.join_GRing_Ring_between_GRing_SemiRing_and_GRing_Zmodule
                 (GRing.ComRing.Exports.coq_GRing_ComRing__to__GRing_Ring
                   (word sz)))
               (GRing.one
                 (GRing.Ring.Exports.coq_GRing_Ring__to__GRing_SemiRing
                   (GRing.ComRing.Exports.coq_GRing_ComRing__to__GRing_Ring
                     (word sz))))))))
  then Error ErrArith
  else Ok (o w1 w2)

(** val mk_sem_sop2 : ('a1 -> 'a2 -> 'a3) -> 'a1 -> 'a2 -> 'a3 exec **)

let mk_sem_sop2 o v1 v2 =
  Ok (o v1 v2)

(** val mk_sem_wiop2 :
    signedness -> wsize -> (coq_Z -> coq_Z -> coq_Z) -> GRing.ComRing.sort ->
    GRing.ComRing.sort -> GRing.ComRing.sort exec **)

let mk_sem_wiop2 sign sz o w1 w2 =
  wint_of_int sign sz (o (int_of_word sign sz w1) (int_of_word sign sz w2))

(** val mk_sem_wishift :
    signedness -> wsize -> (coq_Z -> coq_Z -> coq_Z) -> GRing.ComRing.sort ->
    GRing.ComRing.sort -> GRing.ComRing.sort exec **)

let mk_sem_wishift sign sz o w1 w2 =
  wint_of_int sign sz
    (o (int_of_word sign sz w1) (int_of_word Unsigned U8 w2))

(** val mk_sem_wicmp :
    signedness -> wsize -> (coq_Z -> coq_Z -> bool) -> GRing.ComRing.sort ->
    GRing.ComRing.sort -> bool exec **)

let mk_sem_wicmp sign sz o w1 w2 =
  Ok (o (int_of_word sign sz w1) (int_of_word sign sz w2))

(** val sem_wiop2_typed :
    signedness -> wsize -> wiop2 -> sem_t -> sem_t -> sem_t exec **)

let sem_wiop2_typed sign sz = function
| WIadd -> mk_sem_wiop2 sign sz Z.add
| WImul -> mk_sem_wiop2 sign sz Z.mul
| WIsub -> mk_sem_wiop2 sign sz Z.sub
| WIdiv -> mk_sem_divmod sign sz (signed (wdiv sz) (wdivi sz) sign)
| WImod -> mk_sem_divmod sign sz (signed (wmod sz) (wmodi sz) sign)
| WIshl -> mk_sem_wishift sign sz zlsl
| WIshr -> mk_sem_wishift sign sz zasr
| WIeq -> Obj.magic mk_sem_wicmp sign sz Z.eqb
| WIneq -> Obj.magic mk_sem_wicmp sign sz (fun z1 z2 -> negb (Z.eqb z1 z2))
| WIlt -> Obj.magic mk_sem_wicmp sign sz Z.ltb
| WIle -> Obj.magic mk_sem_wicmp sign sz Z.leb
| WIgt -> Obj.magic mk_sem_wicmp sign sz Z.gtb
| WIge -> Obj.magic mk_sem_wicmp sign sz Z.geb

(** val sem_sop2_typed : sop2 -> sem_t -> sem_t -> sem_t exec **)

let sem_sop2_typed = function
| Obeq ->
  mk_sem_sop2
    (Obj.magic eq_op
      (reverse_coercion coq_Datatypes_bool__canonical__eqtype_Equality __))
| Oand -> mk_sem_sop2 (Obj.magic (&&))
| Oor -> mk_sem_sop2 (Obj.magic (||))
| Oadd o0 ->
  (match o0 with
   | Op_int -> mk_sem_sop2 (Obj.magic Z.add)
   | Op_w s ->
     mk_sem_sop2
       (GRing.add
         (GRing.ComRing.Exports.coq_GRing_ComRing__to__GRing_Nmodule (word s))))
| Omul o0 ->
  (match o0 with
   | Op_int -> mk_sem_sop2 (Obj.magic Z.mul)
   | Op_w s ->
     mk_sem_sop2
       (GRing.mul
         (GRing.ComRing.Exports.coq_GRing_ComRing__to__GRing_SemiRing
           (word s))))
| Osub o0 ->
  (match o0 with
   | Op_int -> mk_sem_sop2 (Obj.magic Z.sub)
   | Op_w s ->
     mk_sem_sop2 (fun x y ->
       GRing.add
         (GRing.Zmodule.Exports.coq_GRing_Zmodule__to__GRing_Nmodule
           (GRing.ComRing.Exports.coq_GRing_ComRing__to__GRing_Zmodule
             (word s))) x
         (GRing.opp
           (GRing.ComRing.Exports.coq_GRing_ComRing__to__GRing_Zmodule
             (word s)) y)))
| Odiv (u, o0) ->
  (match o0 with
   | Op_int -> mk_sem_sop2 (signed (Obj.magic Z.div) (Obj.magic Z.quot) u)
   | Op_w s -> mk_sem_divmod u s (signed (wdiv s) (wdivi s) u))
| Omod (u, o0) ->
  (match o0 with
   | Op_int -> mk_sem_sop2 (signed (Obj.magic Z.modulo) (Obj.magic Z.rem) u)
   | Op_w s -> mk_sem_divmod u s (signed (wmod s) (wmodi s) u))
| Oland s -> mk_sem_sop2 (wand s)
| Olor s -> mk_sem_sop2 (wor s)
| Olxor s -> mk_sem_sop2 (wxor s)
| Olsr s -> mk_sem_sop2 (sem_shr s)
| Olsl o0 ->
  (match o0 with
   | Op_int -> mk_sem_sop2 (Obj.magic zlsl)
   | Op_w s -> mk_sem_sop2 (sem_shl s))
| Oasr o0 ->
  (match o0 with
   | Op_int -> mk_sem_sop2 (Obj.magic zasr)
   | Op_w s -> mk_sem_sop2 (sem_sar s))
| Oror s -> mk_sem_sop2 (sem_ror s)
| Orol s -> mk_sem_sop2 (sem_rol s)
| Oeq o0 ->
  (match o0 with
   | Op_int -> mk_sem_sop2 (Obj.magic Z.eqb)
   | Op_w s ->
     mk_sem_sop2
       (Obj.magic eq_op
         (GRing.ComRing.Exports.coq_GRing_ComRing__to__eqtype_Equality
           (word s))))
| Oneq o0 ->
  (match o0 with
   | Op_int ->
     mk_sem_sop2 (fun x y ->
       Obj.magic negb (Z.eqb (Obj.magic x) (Obj.magic y)))
   | Op_w s ->
     mk_sem_sop2 (fun x y ->
       Obj.magic negb
         (eq_op
           (GRing.ComRing.Exports.coq_GRing_ComRing__to__eqtype_Equality
             (word s)) x y)))
| Olt c ->
  (match c with
   | Cmp_int -> mk_sem_sop2 (Obj.magic Z.ltb)
   | Cmp_w (u, s) -> mk_sem_sop2 (Obj.magic wlt s u))
| Ole c ->
  (match c with
   | Cmp_int -> mk_sem_sop2 (Obj.magic Z.leb)
   | Cmp_w (u, s) -> mk_sem_sop2 (Obj.magic wle s u))
| Ogt c ->
  (match c with
   | Cmp_int -> mk_sem_sop2 (Obj.magic Z.gtb)
   | Cmp_w (u, s) -> mk_sem_sop2 (fun x y -> Obj.magic wlt s u y x))
| Oge c ->
  (match c with
   | Cmp_int -> mk_sem_sop2 (Obj.magic Z.geb)
   | Cmp_w (u, s) -> mk_sem_sop2 (fun x y -> Obj.magic wle s u y x))
| Ovadd (ve, ws) -> mk_sem_sop2 (sem_vadd ve ws)
| Ovsub (ve, ws) -> mk_sem_sop2 (sem_vsub ve ws)
| Ovmul (ve, ws) -> mk_sem_sop2 (sem_vmul ve ws)
| Ovlsr (ve, ws) -> mk_sem_sop2 (sem_vshr ve ws)
| Ovlsl (ve, ws) -> mk_sem_sop2 (sem_vshl ve ws)
| Ovasr (ve, ws) -> mk_sem_sop2 (sem_vsar ve ws)
| Owi2 (s, sz, o0) -> sem_wiop2_typed s sz o0

(** val sem_combine_flags :
    coq_FlagCombinationParams -> combine_flags -> bool -> bool -> bool ->
    bool -> bool **)

let sem_combine_flags cfcd cf b0 b1 b2 b3 =
  cf_xsem cfcd negb (&&) (||) (fun x y ->
    eq_op coq_Datatypes_bool__canonical__eqtype_Equality (Obj.magic x)
      (Obj.magic y)) b0 b1 b2 b3 cf

(** val sem_opN_typed :
    coq_FlagCombinationParams -> opN -> sem_t exec sem_prod **)

let sem_opN_typed cfcd = function
| Opack (sz, pe) ->
  curry Coq_cint (divn (nat_of_wsize sz) (nat_of_pelem pe)) (fun vs -> Ok
    (wpack sz (nat_of_pelem pe) (Obj.magic vs)))
| Ocombine_flags cf ->
  Obj.magic (fun b0 b1 b2 b3 -> Ok (sem_combine_flags cfcd cf b0 b1 b2 b3))

open BinNums
open Datatypes
open Seq
open Ssralg
open Utils0
open Word0
open Wsize

(** val ch :
    GRing.ComRing.sort -> GRing.ComRing.sort -> GRing.ComRing.sort ->
    GRing.ComRing.sort **)

let ch e f g =
  wxor U32 (wand U32 e f) (wand U32 (wnot U32 e) g)

(** val maj :
    GRing.ComRing.sort -> GRing.ComRing.sort -> GRing.ComRing.sort ->
    GRing.ComRing.sort **)

let maj a b c =
  wxor U32 (wand U32 a b) (wxor U32 (wand U32 a c) (wand U32 b c))

(** val _UU03a3__UU2080_ : GRing.ComRing.sort -> GRing.ComRing.sort **)

let _UU03a3__UU2080_ a =
  wxor U32 (wror U32 a (Zpos (Coq_xO Coq_xH)))
    (wxor U32 (wror U32 a (Zpos (Coq_xI (Coq_xO (Coq_xI Coq_xH)))))
      (wror U32 a (Zpos (Coq_xO (Coq_xI (Coq_xI (Coq_xO Coq_xH)))))))

(** val _UU03a3__UU2081_ : GRing.ComRing.sort -> GRing.ComRing.sort **)

let _UU03a3__UU2081_ e =
  wxor U32 (wror U32 e (Zpos (Coq_xO (Coq_xI Coq_xH))))
    (wxor U32 (wror U32 e (Zpos (Coq_xI (Coq_xI (Coq_xO Coq_xH)))))
      (wror U32 e (Zpos (Coq_xI (Coq_xO (Coq_xO (Coq_xI Coq_xH)))))))

(** val _UU03c3__UU2080_ : GRing.ComRing.sort -> GRing.ComRing.sort **)

let _UU03c3__UU2080_ w =
  wxor U32 (wror U32 w (Zpos (Coq_xI (Coq_xI Coq_xH))))
    (wxor U32 (wror U32 w (Zpos (Coq_xO (Coq_xI (Coq_xO (Coq_xO Coq_xH))))))
      (wshr U32 w (Zpos (Coq_xI Coq_xH))))

(** val _UU03c3__UU2081_ : GRing.ComRing.sort -> GRing.ComRing.sort **)

let _UU03c3__UU2081_ w =
  wxor U32 (wror U32 w (Zpos (Coq_xI (Coq_xO (Coq_xO (Coq_xO Coq_xH))))))
    (wxor U32 (wror U32 w (Zpos (Coq_xI (Coq_xI (Coq_xO (Coq_xO Coq_xH))))))
      (wshr U32 w (Zpos (Coq_xO (Coq_xI (Coq_xO Coq_xH))))))

(** val sha256msg1 :
    GRing.ComRing.sort -> GRing.ComRing.sort -> GRing.ComRing.sort **)

let sha256msg1 m_UU2081_ m_UU2082_ =
  let s = split_vec U128 (nat_of_wsize (wsize_of_velem VE32)) m_UU2081_ in
  make_vec U32 U128
    (map2 (fun x y ->
      GRing.add
        (GRing.ComRing.Exports.coq_GRing_ComRing__to__GRing_Nmodule
          (word U32)) (Obj.magic x) (_UU03c3__UU2080_ y)) s
      (rcons (behead (Obj.magic s)) (zero_extend U32 U128 m_UU2082_)))

(** val sha256msg2 :
    GRing.ComRing.sort -> GRing.ComRing.sort -> GRing.ComRing.sort **)

let sha256msg2 m_UU2081_ m_UU2082_ =
  let src_UU2081_ =
    split_vec U128 (nat_of_wsize (wsize_of_velem VE32)) m_UU2081_
  in
  let src_UU2082_ =
    split_vec U128 (nat_of_wsize (wsize_of_velem VE32)) m_UU2082_
  in
  let w14 =
    nth
      (Obj.magic GRing.zero
        (GRing.ComRing.Exports.coq_GRing_ComRing__to__GRing_Nmodule
          (word U32))) src_UU2082_ (S (S O))
  in
  let w15 =
    nth
      (Obj.magic GRing.zero
        (GRing.ComRing.Exports.coq_GRing_ComRing__to__GRing_Nmodule
          (word U32))) src_UU2082_ (S (S (S O)))
  in
  let w16 =
    GRing.add
      (GRing.ComRing.Exports.coq_GRing_ComRing__to__GRing_Nmodule (word U32))
      (nth
        (GRing.zero
          (GRing.ComRing.Exports.coq_GRing_ComRing__to__GRing_Nmodule
            (word U32))) (Obj.magic src_UU2081_) O)
      (_UU03c3__UU2081_ (Obj.magic w14))
  in
  let w17 =
    GRing.add
      (GRing.ComRing.Exports.coq_GRing_ComRing__to__GRing_Nmodule (word U32))
      (nth
        (GRing.zero
          (GRing.ComRing.Exports.coq_GRing_ComRing__to__GRing_Nmodule
            (word U32))) (Obj.magic src_UU2081_) (S O))
      (_UU03c3__UU2081_ (Obj.magic w15))
  in
  let w18 =
    GRing.add
      (GRing.ComRing.Exports.coq_GRing_ComRing__to__GRing_Nmodule (word U32))
      (nth
        (GRing.zero
          (GRing.ComRing.Exports.coq_GRing_ComRing__to__GRing_Nmodule
            (word U32))) (Obj.magic src_UU2081_) (S (S O)))
      (_UU03c3__UU2081_ w16)
  in
  let w19 =
    GRing.add
      (GRing.ComRing.Exports.coq_GRing_ComRing__to__GRing_Nmodule (word U32))
      (nth
        (GRing.zero
          (GRing.ComRing.Exports.coq_GRing_ComRing__to__GRing_Nmodule
            (word U32))) (Obj.magic src_UU2081_) (S (S (S O))))
      (_UU03c3__UU2081_ w17)
  in
  make_vec U32 U128 (w16 :: (w17 :: (w18 :: (w19 :: []))))

(** val sha256rnds2 :
    GRing.ComRing.sort -> GRing.ComRing.sort -> GRing.ComRing.sort ->
    GRing.ComRing.sort **)

let sha256rnds2 x y z =
  let x0 = split_vec U128 (nat_of_wsize (wsize_of_velem VE32)) x in
  let y0 = split_vec U128 (nat_of_wsize (wsize_of_velem VE32)) y in
  let z0 = split_vec U128 (nat_of_wsize (wsize_of_velem VE32)) z in
  let p = (x0, y0) in
  let (l0, l1) = p in
  (match l0 with
   | [] ->
     GRing.zero
       (GRing.ComRing.Exports.coq_GRing_ComRing__to__GRing_Nmodule
         (word U128))
   | h_UU2080_ :: l2 ->
     (match l2 with
      | [] ->
        GRing.zero
          (GRing.ComRing.Exports.coq_GRing_ComRing__to__GRing_Nmodule
            (word U128))
      | g_UU2080_ :: l3 ->
        (match l3 with
         | [] ->
           GRing.zero
             (GRing.ComRing.Exports.coq_GRing_ComRing__to__GRing_Nmodule
               (word U128))
         | d_UU2080_ :: l4 ->
           (match l4 with
            | [] ->
              GRing.zero
                (GRing.ComRing.Exports.coq_GRing_ComRing__to__GRing_Nmodule
                  (word U128))
            | c_UU2080_ :: l5 ->
              (match l5 with
               | [] ->
                 (match l1 with
                  | [] ->
                    GRing.zero
                      (GRing.ComRing.Exports.coq_GRing_ComRing__to__GRing_Nmodule
                        (word U128))
                  | f_UU2080_ :: l6 ->
                    (match l6 with
                     | [] ->
                       GRing.zero
                         (GRing.ComRing.Exports.coq_GRing_ComRing__to__GRing_Nmodule
                           (word U128))
                     | e_UU2080_ :: l7 ->
                       (match l7 with
                        | [] ->
                          GRing.zero
                            (GRing.ComRing.Exports.coq_GRing_ComRing__to__GRing_Nmodule
                              (word U128))
                        | b_UU2080_ :: l8 ->
                          (match l8 with
                           | [] ->
                             GRing.zero
                               (GRing.ComRing.Exports.coq_GRing_ComRing__to__GRing_Nmodule
                                 (word U128))
                           | a_UU2080_ :: l9 ->
                             (match l9 with
                              | [] ->
                                (match z0 with
                                 | [] ->
                                   GRing.zero
                                     (GRing.ComRing.Exports.coq_GRing_ComRing__to__GRing_Nmodule
                                       (word U128))
                                 | wk_UU2080_ :: l10 ->
                                   (match l10 with
                                    | [] ->
                                      GRing.zero
                                        (GRing.ComRing.Exports.coq_GRing_ComRing__to__GRing_Nmodule
                                          (word U128))
                                    | wk_UU2081_ :: l11 ->
                                      (match l11 with
                                       | [] ->
                                         GRing.zero
                                           (GRing.ComRing.Exports.coq_GRing_ComRing__to__GRing_Nmodule
                                             (word U128))
                                       | _ :: l12 ->
                                         (match l12 with
                                          | [] ->
                                            GRing.zero
                                              (GRing.ComRing.Exports.coq_GRing_ComRing__to__GRing_Nmodule
                                                (word U128))
                                          | _ :: l13 ->
                                            (match l13 with
                                             | [] ->
                                               let t_UU2080_ =
                                                 GRing.add
                                                   (GRing.ComRing.Exports.coq_GRing_ComRing__to__GRing_Nmodule
                                                     (word U32))
                                                   (GRing.add
                                                     (GRing.ComRing.Exports.coq_GRing_ComRing__to__GRing_Nmodule
                                                       (word U32))
                                                     (GRing.add
                                                       (GRing.ComRing.Exports.coq_GRing_ComRing__to__GRing_Nmodule
                                                         (word U32))
                                                       (ch
                                                         (Obj.magic e_UU2080_)
                                                         (Obj.magic f_UU2080_)
                                                         (Obj.magic g_UU2080_))
                                                       (_UU03a3__UU2081_
                                                         (Obj.magic e_UU2080_)))
                                                     (Obj.magic wk_UU2080_))
                                                   (Obj.magic h_UU2080_)
                                               in
                                               let a_UU2081_ =
                                                 GRing.add
                                                   (GRing.ComRing.Exports.coq_GRing_ComRing__to__GRing_Nmodule
                                                     (word U32))
                                                   (GRing.add
                                                     (GRing.ComRing.Exports.coq_GRing_ComRing__to__GRing_Nmodule
                                                       (word U32)) t_UU2080_
                                                     (maj
                                                       (Obj.magic a_UU2080_)
                                                       (Obj.magic b_UU2080_)
                                                       (Obj.magic c_UU2080_)))
                                                   (_UU03a3__UU2080_
                                                     (Obj.magic a_UU2080_))
                                               in
                                               let e_UU2081_ =
                                                 GRing.add
                                                   (GRing.ComRing.Exports.coq_GRing_ComRing__to__GRing_Nmodule
                                                     (word U32)) t_UU2080_
                                                   (Obj.magic d_UU2080_)
                                               in
                                               let t_UU2081_ =
                                                 GRing.add
                                                   (GRing.ComRing.Exports.coq_GRing_ComRing__to__GRing_Nmodule
                                                     (word U32))
                                                   (GRing.add
                                                     (GRing.ComRing.Exports.coq_GRing_ComRing__to__GRing_Nmodule
                                                       (word U32))
                                                     (GRing.add
                                                       (GRing.ComRing.Exports.coq_GRing_ComRing__to__GRing_Nmodule
                                                         (word U32))
                                                       (ch e_UU2081_
                                                         (Obj.magic e_UU2080_)
                                                         (Obj.magic f_UU2080_))
                                                       (_UU03a3__UU2081_
                                                         e_UU2081_))
                                                     (Obj.magic wk_UU2081_))
                                                   (Obj.magic g_UU2080_)
                                               in
                                               let a_UU2082_ =
                                                 GRing.add
                                                   (GRing.ComRing.Exports.coq_GRing_ComRing__to__GRing_Nmodule
                                                     (word U32))
                                                   (GRing.add
                                                     (GRing.ComRing.Exports.coq_GRing_ComRing__to__GRing_Nmodule
                                                       (word U32)) t_UU2081_
                                                     (maj a_UU2081_
                                                       (Obj.magic a_UU2080_)
                                                       (Obj.magic b_UU2080_)))
                                                   (_UU03a3__UU2080_
                                                     a_UU2081_)
                                               in
                                               let e_UU2082_ =
                                                 GRing.add
                                                   (GRing.ComRing.Exports.coq_GRing_ComRing__to__GRing_Nmodule
                                                     (word U32)) t_UU2081_
                                                   (Obj.magic c_UU2080_)
                                               in
                                               make_vec U32 U128
                                                 (e_UU2081_ :: (e_UU2082_ :: (a_UU2081_ :: (a_UU2082_ :: []))))
                                             | _ :: _ ->
                                               GRing.zero
                                                 (GRing.ComRing.Exports.coq_GRing_ComRing__to__GRing_Nmodule
                                                   (word U128)))))))
                              | _ :: _ ->
                                GRing.zero
                                  (GRing.ComRing.Exports.coq_GRing_ComRing__to__GRing_Nmodule
                                    (word U128)))))))
               | _ :: _ ->
                 GRing.zero
                   (GRing.ComRing.Exports.coq_GRing_ComRing__to__GRing_Nmodule
                     (word U128)))))))

open BinInt
open BinNums
open Bool
open Datatypes
open Sumbool
open Eqtype
open Gen_map
open Memory_model
open Seq
open Ssralg
open Ssrfun
open Utils0
open Word0
open Wsize

type __ = Obj.t
let __ = let rec f _ = Obj.repr f in Obj.repr f

module MemoryI =
 struct
  (** val is_zalloc : 'a1 Mz.t -> coq_Z -> bool **)

  let is_zalloc m p =
    match Mz.get m (Obj.magic p) with
    | Some _ -> true
    | None -> false

  type frame = { frame_off : coq_Z; frame_size : coq_Z; frame_padding : coq_Z }

  (** val frame_off : frame -> coq_Z **)

  let frame_off f =
    f.frame_off

  (** val frame_size : frame -> coq_Z **)

  let frame_size f =
    f.frame_size

  (** val frame_padding : frame -> coq_Z **)

  let frame_padding f =
    f.frame_padding

  (** val footprint_of_frame : frame -> coq_Z **)

  let footprint_of_frame f =
    Z.add f.frame_size f.frame_padding

  (** val footprint_of_stack : frame list -> coq_Z **)

  let footprint_of_stack frames0 =
    foldr (fun f -> Z.add (footprint_of_frame f)) Z0 frames0

  (** val valid_frame : frame -> bool **)

  let valid_frame f =
    (&&) (Z.leb Z0 f.frame_off)
      ((&&) (Z.leb f.frame_off f.frame_size) (Z.leb Z0 f.frame_padding))

  type mem_ = { data : GRing.ComRing.sort Mz.t; alloc : unit Mz.t;
                stk_root : GRing.ComRing.sort;
                stk_limit : GRing.ComRing.sort; frames : frame list }

  (** val data : coq_PointerData -> mem_ -> GRing.ComRing.sort Mz.t **)

  let data _ m =
    m.data

  (** val alloc : coq_PointerData -> mem_ -> unit Mz.t **)

  let alloc _ m =
    m.alloc

  (** val stk_root : coq_PointerData -> mem_ -> GRing.ComRing.sort **)

  let stk_root _ m =
    m.stk_root

  (** val stk_limit : coq_PointerData -> mem_ -> GRing.ComRing.sort **)

  let stk_limit _ m =
    m.stk_limit

  (** val frames : coq_PointerData -> mem_ -> frame list **)

  let frames _ m =
    m.frames

  type mem = mem_

  (** val is_alloc : coq_PointerData -> mem -> GRing.ComRing.sort -> bool **)

  let is_alloc pd m p =
    is_zalloc m.alloc (wunsigned pd p)

  (** val is_init : coq_PointerData -> mem -> GRing.ComRing.sort -> bool **)

  let is_init pd m p =
    is_zalloc m.data (wunsigned pd p)

  (** val get :
      coq_PointerData -> mem -> GRing.ComRing.sort -> (error,
      GRing.Nmodule.sort) result **)

  let get pd m p =
    if (&&) (is_alloc pd m p) (is_init pd m p)
    then Ok
           (Ssrfun.Option.default
             (GRing.zero
               (GRing.ComRing.Exports.coq_GRing_ComRing__to__GRing_Nmodule
                 (word U8))) (Mz.get m.data (Obj.magic wunsigned pd p)))
    else let s = ErrAddrInvalid in Error s

  (** val set :
      coq_PointerData -> mem -> GRing.ComRing.sort -> GRing.ComRing.sort ->
      (error, mem_) result **)

  let set pd m p w =
    if is_alloc pd m p
    then Ok { data = (Mz.set m.data (Obj.magic wunsigned pd p) w); alloc =
           m.alloc; stk_root = m.stk_root; stk_limit = m.stk_limit; frames =
           m.frames }
    else let s = ErrAddrInvalid in Error s

  (** val is_allocP :
      coq_PointerData -> mem -> GRing.ComRing.sort -> GRing.ComRing.sort ->
      reflect **)

  let is_allocP pd m p _ =
    let _evar_0_ = ReflectT in
    let _evar_0_0 = ReflectF in
    if is_alloc pd m p then _evar_0_ else _evar_0_0

  (** val coq_CM : coq_PointerData -> mem coreMem **)

  let coq_CM pd =
    { Memory_model.get = (get pd); Memory_model.set = (set pd); valid8 =
      (is_alloc pd); valid8P = (is_allocP pd) }

  (** val top_stack : coq_PointerData -> mem -> Equality.sort **)

  let top_stack pd m =
    (coq_PointerW pd).add m.stk_root (Z.opp (footprint_of_stack m.frames))

  (** val set_alloc_aux :
      'a1 option -> 'a1 Mz.t -> coq_Z -> coq_Z -> 'a1 Mz.t **)

  let set_alloc_aux b m ptr sz =
    foldl (fun m0 k ->
      match b with
      | Some t0 -> Mz.set m0 (Obj.magic k) t0
      | None -> Mz.remove m0 (Obj.magic k)) m (ziota ptr sz)

  (** val set_alloc : bool -> unit Mz.t -> coq_Z -> coq_Z -> unit Mz.t **)

  let set_alloc b m ptr sz =
    set_alloc_aux (if b then Some () else None) m ptr sz

  (** val clear_data :
      GRing.ComRing.sort Mz.t -> coq_Z -> coq_Z -> GRing.ComRing.sort Mz.t **)

  let clear_data m ptr sz =
    set_alloc_aux None m ptr sz

  (** val stack_blocks_rec :
      coq_PointerData -> Equality.sort -> frame list ->
      Equality.sort * Equality.sort list **)

  let stack_blocks_rec pd stk_root0 frames0 =
    foldr (fun f pat ->
      let (p, blk) = pat in
      (((coq_PointerW pd).add p (Z.opp (footprint_of_frame f))),
      (((coq_PointerW pd).add p (Z.opp (footprint_of_frame f))) :: blk)))
      (stk_root0, []) frames0

  (** val stack_blocks :
      coq_PointerData -> Equality.sort -> frame list -> GRing.ComRing.sort
      list **)

  let stack_blocks pd stk_root0 frames0 =
    snd (stack_blocks_rec pd stk_root0 frames0)

  (** val stack_frames : coq_PointerData -> mem -> GRing.ComRing.sort list **)

  let stack_frames pd m =
    stack_blocks pd m.stk_root m.frames

  (** val alloc_stack :
      coq_PointerData -> mem -> wsize -> coq_Z -> coq_Z -> coq_Z -> mem exec **)

  let alloc_stack pd m ws sz ioff sz' =
    if sumbool_of_bool
         ((&&)
           (valid_frame { frame_off = ioff; frame_size = sz; frame_padding =
             (Z.sub
               ((coq_PointerW pd).sub (top_stack pd m)
                 (align_word pd ws
                   ((coq_PointerW pd).add (top_stack pd m)
                     (Z.opp (Z.add sz sz'))))) sz) })
           ((&&)
             (Z.leb
               (Z.add
                 (footprint_of_frame { frame_off = ioff; frame_size = sz;
                   frame_padding =
                   (Z.sub
                     ((coq_PointerW pd).sub (top_stack pd m)
                       (align_word pd ws
                         ((coq_PointerW pd).add (top_stack pd m)
                           (Z.opp (Z.add sz sz'))))) sz) })
                 (footprint_of_stack m.frames))
               (Z.sub (wunsigned pd m.stk_root) (wunsigned pd m.stk_limit)))
             (Z.leb sz'
               (Z.sub
                 ((coq_PointerW pd).sub (top_stack pd m)
                   (align_word pd ws
                     ((coq_PointerW pd).add (top_stack pd m)
                       (Z.opp (Z.add sz sz'))))) sz))))
    then Ok { data =
           (clear_data m.data
             (Z.add
               (Z.sub (wunsigned pd m.stk_root)
                 (Z.add
                   (footprint_of_frame { frame_off = ioff; frame_size = sz;
                     frame_padding =
                     (Z.sub
                       ((coq_PointerW pd).sub (top_stack pd m)
                         (align_word pd ws
                           ((coq_PointerW pd).add (top_stack pd m)
                             (Z.opp (Z.add sz sz'))))) sz) })
                   (footprint_of_stack m.frames))) ioff) (Z.sub sz ioff));
           alloc =
           (set_alloc true m.alloc
             (Z.add
               (Z.sub (wunsigned pd m.stk_root)
                 (Z.add
                   (footprint_of_frame { frame_off = ioff; frame_size = sz;
                     frame_padding =
                     (Z.sub
                       ((coq_PointerW pd).sub (top_stack pd m)
                         (align_word pd ws
                           ((coq_PointerW pd).add (top_stack pd m)
                             (Z.opp (Z.add sz sz'))))) sz) })
                   (footprint_of_stack m.frames))) ioff) (Z.sub sz ioff));
           stk_root = m.stk_root; stk_limit = m.stk_limit; frames =
           ({ frame_off = ioff; frame_size = sz; frame_padding =
           (Z.sub
             ((coq_PointerW pd).sub (top_stack pd m)
               (align_word pd ws
                 ((coq_PointerW pd).add (top_stack pd m)
                   (Z.opp (Z.add sz sz'))))) sz) } :: m.frames) }
    else Error ErrStack

  (** val free_stack : coq_PointerData -> mem -> mem **)

  let free_stack pd m =
    let sz =
      Ssrfun.Option.default Z0
        (Ssrfun.Option.map footprint_of_frame (ohead m.frames))
    in
    { data = m.data; alloc =
    (set_alloc false m.alloc
      (Z.sub (wunsigned pd m.stk_root) (footprint_of_stack m.frames)) sz);
    stk_root = m.stk_root; stk_limit = m.stk_limit; frames =
    (behead m.frames) }

  (** val init_mem_alloc :
      coq_PointerData -> (GRing.ComRing.sort * coq_Z) list -> unit Mz.t **)

  let init_mem_alloc pd s =
    foldl (fun a pz -> set_alloc true a (wunsigned pd (fst pz)) (snd pz))
      Mz.empty s

  (** val all_above :
      coq_PointerData -> (GRing.ComRing.sort * coq_Z) list ->
      GRing.ComRing.sort -> bool **)

  let all_above pd s stk =
    all (fun pat -> let (p, _) = pat in wlt pd Unsigned stk p) s

  (** val init_mem :
      coq_PointerData -> (GRing.ComRing.sort * coq_Z) list ->
      GRing.ComRing.sort -> mem exec **)

  let init_mem pd s stk =
    if sumbool_of_bool
         (is_align
           (GRing.ComRing.Exports.coq_GRing_ComRing__to__eqtype_Equality
             (word pd)) (coq_PointerW pd) stk U256)
    then if sumbool_of_bool (all_above pd s stk)
         then Ok { data = Mz.empty; alloc = (init_mem_alloc pd s); stk_root =
                stk; stk_limit =
                (GRing.zero
                  (GRing.ComRing.Exports.coq_GRing_ComRing__to__GRing_Nmodule
                    (word pd))); frames = [] }
         else Error ErrStack
    else Error ErrStack

  (** val coq_M : coq_PointerData -> mem memory **)

  let coq_M pd =
    { stack_root = (fun m -> m.stk_root); stack_limit = (fun m ->
      m.stk_limit); Memory_model.frames = (stack_frames pd);
      Memory_model.alloc_stack = (alloc_stack pd); Memory_model.free_stack =
      (free_stack pd); init = (init_mem pd) }

  (** val write_mem_stable : __ **)

  let write_mem_stable =
    __

  (** val alloc_stackP : __ **)

  let alloc_stackP =
    __

  (** val free_stackP : __ **)

  let free_stackP =
    __

  (** val alloc_stack_complete : __ **)

  let alloc_stack_complete =
    __
 end

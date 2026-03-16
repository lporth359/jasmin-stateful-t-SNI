(* securitytransformationpass.ml *)
open BinInt
open BinNums
open Bool
open Datatypes
open Ident
open List0
open Compiler_util
open Utils0
open Var0
open Expr
open Printer
open Sopn
open Wsize
open Sectrafo_util
open Sectrafo_util_clearing



let debug_security_pass : bool = false

let dbg (s : string) : unit =
  if debug_security_pass then (Printf.eprintf "[SecurityPass] %s\n%!" s) else ()




let rec coqZ_of_int (n : int) : coq_Z =
  if n <= 0 then Z0
  else Z.add (Zpos Coq_xH) (coqZ_of_int (n - 1))


type reg_id = var_i



(* --- share map types --- *)

type secret_id = Input of coq_Z
type share_id  = int

type share_info = { sid : secret_id; sh : share_id }

module ZOrd = struct
  type t = coq_Z
  let compare (a:t) (b:t) : int =
    match Z.compare a b with Lt -> -1 | Eq -> 0 | Gt -> 1
end

module ZMap = Map.Make(ZOrd)

type share_map = share_info ZMap.t  (* key: stack offset *)



(* Debug helpers*)

(* ---- coq_Z -> string (debug) ---- *)

let string_of_coq_positive (p : positive) : string =
  (* Build bits MSB->LSB by recursion *)
  let rec go p acc =
    match p with
    | Coq_xH -> '1' :: acc
    | Coq_xO p' -> go p' ('0' :: acc)
    | Coq_xI p' -> go p' ('1' :: acc)
  in
  let chars = go p [] in
  let len = List.length chars in
  let s = Bytes.create len in
  let rec fill i = function
    | [] -> ()
    | c :: tl ->
        Bytes.set s i c;
        fill (i + 1) tl
  in
  fill 0 chars;
  Bytes.unsafe_to_string s

let string_of_coq_Z (z : coq_Z) : string =
  match z with
  | Z0 -> "0"
  | Zpos p -> "0b" ^ string_of_coq_positive p
  | Zneg p -> "-0b" ^ string_of_coq_positive p

let debug_print_input_share_map fn (m : share_map) =
  Format.eprintf "Input share map for function:@.";
  ZMap.iter (fun ofs si ->
    match si.sid with
    | Input base_ofs ->
        Format.eprintf
          "  stack_ofs=%s -> Input(base=%s), share=%d@."
          (string_of_coq_Z ofs)
          (string_of_coq_Z base_ofs)
          si.sh
  ) m;
  Format.eprintf "@."

  let debug_probe_copn = false

let string_of_sopn_kind = function
  | Opseudo_op _ -> "Opseudo_op"
  | Oslh _ -> "Oslh"
  | Oasm _ -> "Oasm"



let debug_probe_body (body : 'asm_op instr list) =
  if not debug_probe_copn then () else
  List.iter (fun i ->
    match i with
    | MkI (_ii, ir) ->
      match ir with
      | Copn (outs, _tag, op, ins) ->
          Format.eprintf "Copn: kind=%s outs=%d ins=%d"
            (string_of_sopn_kind op) (List.length outs) (List.length ins);
          (match op with
           | Opseudo_op _pop ->
              Format.eprintf " pseudo"
           | Oslh slh ->
               (* if you have a printer; otherwise just say “slh” *)
               Format.eprintf " slh"
           | Oasm _ ->
               Format.eprintf " asm");
          Format.eprintf "@."
      | _ -> ()
  ) body

let debug_count_loads = false
let debug_loads = false

let count_pload_in_body (body : 'asm_op instr list) : int =
  let c = ref 0 in
  List.iter (fun i ->
    match i with
    | MkI (_, Cassgn (_lv, _tag, _ty, rhs)) ->
        (match rhs with Pload _ -> incr c | _ -> ())
    | _ -> ()
  ) body;
  !c


let debug_print_load_match ~(input_map:share_map) (r:reg_id) (ofs:coq_Z) =
  if not debug_loads then () else
  Format.eprintf "STACK LOAD matched: ofs=%s@." (string_of_coq_Z ofs);
  match ZMap.find_opt ofs input_map with
  | None ->
      Format.eprintf "  -> not an input share (will use mem dep)@."
  | Some si ->
      let Input base = si.sid in
      Format.eprintf "  -> input share: base=%s sh=%d@."
        (string_of_coq_Z base) si.sh


(* --- precompute per-function maps --- *)

module FnMap = Map.Make(struct
  type t = funname
  let compare = compare
end)


let debug_loads = false

let debug_print_load_match ~input_map r ofs =
  if not debug_loads then () else begin
    Format.eprintf "STACK LOAD matched: ofs=%s@." (string_of_coq_Z ofs);
    match ZMap.find_opt ofs input_map with
    | None ->
        Format.eprintf "  -> not an input share (will use mem dep)@."
    | Some si ->
        let Input base = si.sid in
        Format.eprintf "  -> input share: base=%s sh=%d@."
          (string_of_coq_Z base) si.sh
  end


let debug_stack_accesses = false
let debug_trafo = false

let debug_print_stack_access ~input_map kind ofs =
  if not debug_stack_accesses then () else begin
    Format.eprintf "%s stack access: ofs=%s" kind (string_of_coq_Z ofs);
    if ZMap.mem ofs input_map then
      Format.eprintf "  [HITS INPUT MAP]@."
    else
      Format.eprintf "  [not input]@."
  end


let rec peel (e : pexpr) : pexpr =
  match e with
  | Papp1 (_op, e1) -> peel e1
  | _ -> e

let rec offset_of_addr_expr (e : pexpr) : coq_Z option =
  match peel e with
  | Pconst z -> Some z

  | Papp2 (Oadd _, a, b) ->
      begin match offset_of_addr_expr a with
      | Some z -> Some z
      | None -> offset_of_addr_expr b
      end

  | Papp2 (Osub _, a, b) ->
      (* a - b : if we find const in b, negate it *)
      begin match offset_of_addr_expr b with
      | Some z -> Some (Z.opp z)
      | None -> offset_of_addr_expr a
      end

  | _ -> None


let offset_of_load_ins (e : pexpr) : coq_Z option =
  match e with
  | Pload (_al, _ws, addr) -> offset_of_addr_expr addr
  | _ -> None


  let match_load_ofs (i : 'asm_op instr) : (var_i * coq_Z) option =
  match i with
  | MkI (_, Copn (outs, _tag, op, ins)) ->
      begin match op, outs, ins with
      | Oasm _, [Lvar dst], [load_arg] ->
          begin match offset_of_load_ins load_arg with
          | Some ofs -> Some (dst, ofs)
          | None -> None
          end
      | _ -> None
      end
  | _ -> None


(* ---------- Helpers for coq_Z comparisons (Map/Set need int comparison) --- *)
let z_cmp (a : coq_Z) (b : coq_Z) : int =
  match Z.compare a b with
  | Lt -> -1
  | Eq -> 0
  | Gt -> 1


let rec peel2 (e : pexpr) : pexpr =
  match e with
  | Papp1 (_op, e1) -> peel e1
  | _ -> e

let rec offset_of_addr_expr2 (e : pexpr) : (var_i * coq_Z * wsize) option =
  match e with
    | Papp2 (Oadd (Op_w ws) , firstexpr, secondexpr) -> 
      begin match (firstexpr, secondexpr) with
      | ((Pvar x) , (Papp1 (_, (Pconst (y)) )))  -> Some (x.gv, y, ws)
      | _ -> None
      end
    | _ -> None

let offset_of_load_ins2 (e : pexpr) : (var_i * coq_Z * wsize) option =
  match e with
  | Pload (_, ws, addr) -> 
    begin match addr with
    | Papp2 ( _ , firstexpr, secondexpr) -> 
      begin match (firstexpr, secondexpr) with
      | ((Pvar x) , (Papp1 (_, (Pconst (y)) )))  -> Some (x.gv, y, ws)
      | _ -> None
      end
    | _ -> None 
    end
  | _ -> None


  let match_load_ofs2 (i : 'asm_op instr) (asmop) : (var_i * var_i * coq_Z * wsize) option =
  match i with
  | MkI (_, Copn (outs, _tag, op, ins)) ->
      begin match op, outs, ins with
      | Oasm o0, [Lvar dst], [load_arg] ->
          let op = ((asmop.asm_op_instr o0).str ()) in 
          begin match op with 
            | "LDR" ->  begin match offset_of_load_ins2 load_arg with
                         | Some ((src:var_i), ofs, (w:wsize)) -> Some (dst, src , ofs, w)
                         | None -> None
                        end
            |_ -> None 

          end   
      | _ -> None
      end
  | _ -> None



let is_sp_of (sp : var_i) (x : gvar) : bool =
  Stdlib.compare x.gv sp = 0

(* --- share map types --- *)




(*Maps and structs*)

type share_info2 = Expr.share_info  (* (sharename, share_index) *)
type skey = Expr.skey     (* (base var, element index) *)


module SKeyOrd = Expr.SKeyOrd

type randomness_info = Expr.randomness_info  (* (sharename, share_index) *)
type rkey = Expr.rkey     (* (base var, element index) *)


module RKeyOrd = Expr.RKeyOrd

module ShareMap = Expr.ShareMap

type smap = Expr.smap

module RandomnessMap = Expr.RandomnessMap

type rmap = Expr.rmap

type share_map2 = smap  (* key: stack offset *)




type share_id3 = share_info2

module ShareIdOrd2 = struct
  type t = share_id3
  let compare (b1, s1) (b2, s2) =
    let c = String.compare b1 b2 in
    if c <> 0 then c else Stdlib.compare s1 s2
end

module ShareSet2 = Set.Make(ShareIdOrd2)

type dep2 = ShareSet2.t

module IntSet = Set.Make(struct
  type t = int
  let compare = Stdlib.compare
end)

module MaskSet2 = Set.Make(IntSet)
type masking2 = IntSet.t


type dep2_m = 
  | Base of dep2 * masking2
  | Val of dep2_m * masking2


(* Compare two lists lexicographically using an element comparator *)
let rec compare_lists cmp xs ys =
  match xs, ys with
  | [], [] -> 0
  | [], _  -> -1
  | _ , [] -> 1
  | x::xs', y::ys' ->
      let c = cmp x y in
      if c <> 0 then c else compare_lists cmp xs' ys'

module Dep2Ord = struct
  type t = dep2
  let compare (a : t) (b : t) : int =
    compare_lists ShareIdOrd2.compare (ShareSet2.elements a) (ShareSet2.elements b)
end

module LeakSet2 = Set.Make(Dep2Ord)
type leak2_set = LeakSet2.t

let dep2_empty : dep2 = ShareSet2.empty
let dep2_union (a:dep2) (b:dep2) : dep2 = ShareSet2.union a b

let dep2_singleton (sid:share_id3) : dep2 = ShareSet2.singleton sid

let leak2_set_empty : leak2_set = LeakSet2.empty
let leak2_set_union (a:leak2_set) (b:leak2_set) : leak2_set = LeakSet2.union a b
let leak2_set_singleton (sid:dep2) : leak2_set = LeakSet2.singleton sid

let masking2_empty : masking2 = IntSet.empty
let masking2_union (a:masking2) (b:masking2) : masking2 = 
   IntSet.union (IntSet.diff a b) (IntSet.diff b a)
let masking2_singleton (sid) : masking2 = IntSet.singleton sid

let masking_contains a b :bool = 
    ((IntSet.cardinal a ) - IntSet.cardinal (IntSet.diff a b) = IntSet.cardinal b)

let dep2_union_with_masking (a:dep2) (b:dep2) (ma : masking2) (mb : masking2) : dep2 =
  if IntSet.cardinal (masking2_union ma mb)  > 0 
    then begin if IntSet.cardinal ma = 0 
      then a 
      else if IntSet.cardinal mb = 0 
        then b 
        else dep2_empty end 
  else ShareSet2.union a b
  


type reg2_key = var_i

module Reg2KeyOrd = struct
  type t = reg2_key
  let compare ((v1 :var_i )) ((v2: var_i)) = 
    match (Var0.Var.var_cmp) v1.v_var v2.v_var  with 
    | Eq ->   0
    | Lt -> -1
    | Gt -> 1
end

module RegMap2 = Map.Make(Reg2KeyOrd)


type reg2_deps = dep2 RegMap2.t
type reg2_masking = masking2 RegMap2.t


module OfsMap2 = Map.Make(struct
  type t = coq_Z
  let compare = z_cmp
end)

type mem2_deps = dep2 ShareMap.t

type share_init2 = bool ShareMap.t
type reg2_rnd = int RegMap2.t
type mem2_rnd = int ShareMap.t


type mem2_masking = masking2 ShareMap.t
let reg2_rnd_empty :reg2_rnd = RegMap2.empty
let reg2_masking_empty :reg2_masking = RegMap2.empty
let mem2_rnd_empty :mem2_rnd = ShareMap.empty

let mem2_masking_empty :mem2_masking = ShareMap.empty


type state = 
  | OPA 
  | OPB
  | OPR
  | OPW

type clearing = state list

type scrubvalue = 
 | None
 | Mem of skey
 | Reg of reg2_key

type scrubbing = scrubvalue list


(*type countermeasures is used to tag an instruction for insertion of countermeasures*)

type countermeasures = {
  clearOPA : bool;
  clearOPB : bool;
  clearOPR : bool;
  clearOPW : bool;
  scrub : scrubvalue
}

(*counter measure empty is tagged to an instruction, if no clearing is needed*)
let countermeasures_empty = {
  clearOPA = false;
  clearOPB = false;
  clearOPR = false;
  clearOPW = false;
  scrub = None;
}


type cm = 
| Scrub of scrubvalue
| Clear of state 
| None


type share = 
  | None
  | Some of share_id3

(*Datatype to build up observations as graph*)

module rec Obs : sig
  type t =
    | None
    | Share of share * masking2
    | Xor of ObsSet.t * masking2
    | And of ObsSet.t * masking2
    | Obs of ObsSet.t * masking2

  val compare : t -> t -> int
end = struct
  type t =
    | None
    | Share of share * masking2
    | Xor of ObsSet.t * masking2
    | And of ObsSet.t * masking2
    | Obs of ObsSet.t * masking2

  let compare = Stdlib.compare
end

and ObsSet : Set.S with type elt = Obs.t =
  Set.Make(Obs)



type observation = Obs.t

type rleak = {
  pos : int; 
  obs : observation; 
  cm : cm ;
  op : bool 
}

let observation_empty : observation = None

  (*helper functions for xor of observation represented as graphs*)
  let xor_list a b = 
    ObsSet.diff (ObsSet.union a b) (ObsSet.inter a b)
  let xor_element_list a b = 
    xor_list (ObsSet.singleton a) b
  let xor_elements a b  =
    xor_list (ObsSet.singleton a) (ObsSet.singleton b)
    
  (*Xor of to observation represented as grapg*)
  let xor (a : observation) (b: observation) : observation = match (a,b) with 

   | ((Xor (a,x)), (Xor (b,y))) -> Xor (xor_list a b , masking2_union x y)
   | ((Xor (a,x)), (Share (b,y))) -> Xor (xor_element_list (Share (b, masking2_empty)) a , masking2_union x y)
   | ((Xor (a,x)), (And (b,y))) -> Xor (xor_element_list (And (b, masking2_empty)) a , masking2_union x y)

   | ((And (a,x)), (Xor (b,y))) -> Xor (xor_element_list (And (a, masking2_empty)) b , masking2_union x y)
   | ((And (a,x)), (Share (None,y))) -> And (a, masking2_union x y)
   | ((And (a,x)), (Share (b,y))) -> Xor (xor_elements (And (a, masking2_empty)) (Share (b, masking2_empty)) , masking2_union x y)
   | ((And (a,x)), (And (b,y))) -> Xor (xor_elements (And (a, masking2_empty)) (And (b, masking2_empty)) , masking2_union x y)

   | ((Share(a,x)), (Xor (b,y))) -> Xor (xor_element_list (Share (a, masking2_empty)) b , masking2_union x y)
   | ((Share(a,x)), (Share (b,y))) -> Xor (xor_elements (Share (a, masking2_empty)) (Share (b, masking2_empty)) , masking2_union x y)
   | ((Share(None,x)), (And (b,y))) -> And (b, masking2_union x y)
   | ((Share(a,x)), (And (b,y))) -> Xor (xor_elements (Share (a, masking2_empty)) (And (b, masking2_empty)) , masking2_union x y)
   | (_,_) -> None  

   (*And of to observation represented as grapg*)
  let o_and (a : observation) (b: observation) : observation= match (a,b) with 

   | ((Obs _), (_)) -> None
   | ((_), (Obs _)) -> None
   | (And (sa,ma), And (sb,mb)) -> And (ObsSet.union sa sb, masking2_union ma mb)
   | (And (sa,ma), _) -> And (ObsSet.add b sa, ma)
   | (_, And (sb,mb)) -> And (ObsSet.add a sb, mb)
   | (_,_) -> And (ObsSet.union (ObsSet.singleton a) (ObsSet.singleton b) , masking2_empty)   

  type dep = {
  shares : ShareSet2.t;
  masks  : IntSet.t;
  }

  (*debug printing for observation type*)
  let print_dep_s d s = 
  ShareSet2.fold (fun (name, idx) acc ->
    Printf.sprintf "%s, %s%i " acc name idx;
  ) d s
  
  let check_obs_interceting a b = 
    ShareSet2.fold (fun (s1,_) acc -> if acc then acc else
      ShareSet2.fold (fun (s2, _) acc -> 
        if acc then acc else s1 = s2 
      ) b acc 
    ) a false
    
  let rec check_obs_security ((d,m) : dep2*masking2) (ds: (dep2 * masking2) list) = match ds with 
     | [] -> false
     | (x,y) :: rst -> 
       let em = IntSet.subset m y in   
       let es = ShareSet2.subset d x in  
       let eis = true in   
        if  (em && (es || not eis)) then true else check_obs_security (d,m) rst 
  let rec check_obs_security1 (d : dep2) (ds: dep2 list) = match ds with 
      | [] -> false
      | x :: rst -> 
        let e = ShareSet2.subset d x  in   
        if e then e else check_obs_security1 d rst 

  let rec check_obs_security2 (d : dep) (ds: dep list) = match ds with 
      | [] -> false
      | x :: rst -> 
        let e = ShareSet2.subset d.shares x.shares && IntSet.subset d.masks x.masks in   
        if e then e else check_obs_security2 d rst 


  let print_obs_shares s =
    let shs = if (ShareSet2.cardinal s = 0) then " ∅" 
      else ShareSet2.fold (fun (s,i) acc -> Printf.sprintf "%s %s%i" acc s i) s ""  in 
    Printf.sprintf "{%s }" shs
  let print_obs_masks_dep m =
    let msk = if (IntSet.cardinal m = 0) then " ∅" 
      else IntSet.fold (fun r acc -> Printf.sprintf "%s r(%i)" acc  r) m ""  in 
    Printf.sprintf "{%s }" msk 

  let print_obs_masks m =
    let msk = IntSet.fold (fun r acc -> Printf.sprintf "%s + r(%i)" acc  r) m ""  in 
    Printf.sprintf "%s" msk 
  let print_obs_dep dep = 
    Printf.printf "Printing of dep: \n Shares: %s Maskings: %s\n" 
      (print_obs_shares dep.shares) (print_obs_masks_dep dep.masks)

  let rec print_obs_s (o:observation) = match o with
    | None -> "∅"
    | Share (Some (s,i), m) -> Printf.sprintf "%s%i %s" s i (print_obs_masks m)
    | Share (None, m) -> print_obs_masks m 
    | Xor (x,m)-> Printf.sprintf "%s %s" (ObsSet.fold (fun o acc -> Printf.sprintf "%s + %s " acc (print_obs_s o) ) x "") (print_obs_masks m)
    | And (x,m) -> Printf.sprintf "%s %s" (ObsSet.fold (fun o acc -> Printf.sprintf "%s & %s " acc (print_obs_s o)) x "") (print_obs_masks m)
    | Obs (x,m) -> ObsSet.fold (fun o acc -> Printf.sprintf "%sObs = { %s %s } \n" acc (print_obs_s o) (print_obs_masks m)) x ""

  let print_obs (o: observation) = 
    Printf.printf "%s" (print_obs_s o)

  (*sis1 collects information used to calculate an observation to localy check if a observation is secure*)
  let rec sis1 (o:observation) =
  match o with
  | None ->
      {shares = ShareSet2.empty; masks = IntSet.empty}

  | Share (Some s,m) ->
      {shares = ShareSet2.singleton s; masks = m}

  | Share (None,m) ->
      {shares = ShareSet2.empty; masks = m}

  | Xor (set,m) ->
      ObsSet.fold
        (fun e acc ->
           let d = sis1 e in
           {
             shares = ShareSet2.union acc.shares d.shares;
             masks  = masking2_union acc.masks d.masks
           })
        set
        {shares = ShareSet2.empty; masks = m}

  | And (set,m) ->
      ObsSet.fold
        (fun e acc ->
           let d = sis1 e in
           {
             shares = ShareSet2.union acc.shares d.shares;
             masks  = IntSet.union acc.masks d.masks
           })
        set
        {shares = ShareSet2.empty; masks = m}

  | Obs (set,m) ->
      ObsSet.fold
        (fun e acc ->
           let d = sis1 e in
           {
             shares = ShareSet2.union acc.shares d.shares;
             masks  = IntSet.union acc.masks d.masks
           })
        set
        {shares = ShareSet2.empty; masks = m}

  
  type monomial = {
  shares : ShareSet2.t;
  masks  : IntSet.t;
  }

  let rec expand (o: observation) =
  match o with
  | None -> []

  | Share (Some s,m) ->
      [{shares = ShareSet2.singleton s; masks = m}]
  | Share (None,m) ->
      [{shares = ShareSet2.empty; masks = m}]

  | Xor (set,m) ->
      ObsSet.fold
        (fun e acc -> acc @ expand e)
        set
        []

  | And (set,m) ->
      ObsSet.fold
        (fun e acc ->
          let monos = expand e in
          match acc with
          | [] -> monos
          | _ ->
              List.concat (
                List.map (fun a ->
                  List.map (fun b ->
                    {
                      shares = ShareSet2.union a.shares b.shares;
                      masks  = IntSet.union a.masks b.masks;
                    }) monos)
                acc))
        set
        []

  | Obs (set,m) ->
      ObsSet.fold
        (fun e acc -> acc @ expand e)
        set
        []

  module IntMap = Map.Make(Stdlib.Int)
  module ObsSetKey = struct
  type t = ObsSet.t
  let compare = ObsSet.compare
  end
  module ObsSetMap = Map.Make(ObsSetKey) 
  
  (*Helper functions for eliminate masking*)
  let obs_dep_stop_ob (o: observation) = match o with
    | None -> true 
    | Share (_,y)  -> true
    | Xor (_,y) 
    | And (_,y) 
    | Obs (_,y)  -> if (IntSet.cardinal y > 0) then true else false
  let stop (obs: ObsSet.t) = 
    ObsSet.fold (fun o acc -> acc && obs_dep_stop_ob o) obs true
  let rec eliminate_masking_build_obs (obs: observation list) mi mb mmap = match obs,mi,mb with
     | [],[], mb -> ObsSet.empty
     | fo :: orest , mf :: mrest , fb :: brest -> begin  match fo with 
        | Share (x,m) -> let sh = ObsSetMap.fold (fun k v acc -> begin
      
          match acc with 
        | (Share (None,_) : observation)  -> acc
        | (Share (Some x,_) :observation) -> if (IntSet.subset v fb && ObsSet.mem fo k) 
            then begin 
              let mn = if IntSet.is_empty mf then mf else IntSet.union mf v in 
              (Share (None,mn)) end else acc 
        | _ -> acc end
        ) mmap (Share(x, mf):observation) in ObsSet.add sh (eliminate_masking_build_obs orest mrest brest mmap)
        | Xor (x,m) -> let oset = ObsSetMap.fold (fun k v acc -> 
            if IntSet.subset v m then ObsSet.diff acc k  else acc) mmap x in 
            ObsSet.add (Xor (oset, mf)) (eliminate_masking_build_obs orest mrest brest mmap)
      | _ -> eliminate_masking_build_obs orest mrest brest mmap end
     | _  -> ObsSet.empty

    let eliminate_masksing_buld_masking_obs mmap = 
      ObsSetMap.fold (fun key value acc -> 
        if ObsSet.is_empty key then IntSet.fold (fun i acc -> ObsSet.add (Share (None, IntSet.singleton i): observation) acc) value acc
        else ObsSet.add (Obs (key, value):observation) acc 
        ) mmap ObsSet.empty
    
    let eliminate_masking_group_masking bmap = 
    IntMap.fold(fun (key: int) (value: ObsSet.t) acc -> 
      match ObsSetMap.find_opt value acc with
        | Some x -> ObsSetMap.add value (IntSet.add key x) acc
        | None -> ObsSetMap.add value (IntSet.singleton key) acc
    ) bmap ObsSetMap.empty
    
  let rec eliminate_masking_collect_broken m (obs: observation list) mi mb acc = match obs, mi ,mb with 
      | [],[],[] -> acc
      | fo :: orst, fi :: irst , fb :: brst -> 
        if (IntSet.mem m fb && IntSet.is_empty fi)  
          then match fo with 
            | Share (Some x, _) -> begin match IntMap.find_opt m acc with 
                | Some e -> IntMap.add m (ObsSet.inter (ObsSet.singleton fo) e) (eliminate_masking_collect_broken m orst irst brst acc) 
                | None -> IntMap.add m (ObsSet.singleton fo) (eliminate_masking_collect_broken m orst irst brst acc) end 
            | Xor (x,_) -> begin match IntMap.find_opt m acc with 
                | Some e -> IntMap.add m (ObsSet.inter x e) (eliminate_masking_collect_broken m orst irst brst acc) 
                | None -> IntMap.add m x (eliminate_masking_collect_broken m orst irst brst acc) end 
            | _ -> acc
          else eliminate_masking_collect_broken m orst irst brst acc
      | _ -> acc 
  let rec eliiminate_masking_union b = match b with 
      | [] -> IntSet.empty
      | fst :: rst -> IntSet.union fst (eliiminate_masking_union rst) 

  let rec stop_eliminate (obs, i) = match i with 
      | [] -> true
      | fst :: rst -> if IntSet.cardinal fst = 0 then false else stop_eliminate (obs, rst) 
  
  let eliminate e = 
    let (obs, i, b) = ObsSet.fold(fun o (obs, i, b) -> 
      let e' = ObsSet.remove o e in 
      let mu = ObsSet.fold (fun x acc -> match x with 
        | Share (_,m) -> IntSet.union m acc
        | Xor (_,m) -> IntSet.union m acc
        | _ -> acc
      ) e' IntSet.empty in 
      let m = match o with 
        | Share (_,m) ->  m 
        | Xor (_,m) ->  m 
        | _ -> IntSet.empty 
      in 
      let mi = IntSet.diff m mu in
      let mb = IntSet.diff m mi in 
      (o::obs, mi::i, mb::b)) e ([] ,[], []) 
    in 

    if not (stop_eliminate (obs, i)) then begin
      let mu = eliiminate_masking_union b in 
      let bmap = IntSet.fold (fun m acc -> 
        eliminate_masking_collect_broken m obs i b acc      
      ) mu  IntMap.empty in 
      let mmap = eliminate_masking_group_masking bmap in 
      let mobs = eliminate_masksing_buld_masking_obs mmap in 
      let oobs = eliminate_masking_build_obs obs i b mmap in 
      ObsSet.union mobs oobs

    end else ObsSet.empty

  let rec prepare_elimination obs ein din : (ObsSet.t * ObsSet.t) = 
    ObsSet.fold (fun o (obs', e) ->  
          match o with
          | And (x,m) -> prepare_elimination x obs' e
          | Share (x, m) -> if (IntSet.cardinal m) = 0 then ((ObsSet.add o obs'),e) else (obs',ObsSet.add o e)
          | Xor (x, m) ->  
              if (IntSet.cardinal m) = 0
              then begin prepare_elimination x obs' e end 
              else begin (obs',ObsSet.add o e) end 
          | Obs (x,m) ->  (prepare_elimination x obs' e)
          | _ ->  (obs', e)
          
           ) obs (ein,din)

  (*eliminate maskings peels of maskings that are used by multiple obseravtion*)
  let rec eliminate_masking (obs: observation) : observation =
    match obs with 
    | Obs(x, m) -> 
      let (d, e) =  prepare_elimination x ObsSet.empty ObsSet.empty in 
      let e' = eliminate e in 
      let obs' = ObsSet.union d e' in 
      if stop obs' then (Obs (obs', masking2_empty)) 
      else eliminate_masking (Obs (obs', masking2_empty)) 
    | _ -> None  

  (*collects shares collects shares needed to simulate observation*)
  let collect_shares (obs:observation) : dep2 = 
    match obs with 
      | Obs (x,_) -> ObsSet.fold (fun o acc -> match o with 
        | Share (Some x, m) -> if IntSet.is_empty m then  ShareSet2.add x acc else acc 
        | _ -> acc
      ) x dep2_empty
      | _ -> dep2_empty

  (*function sis that calculates the shares needed for simulation of one or multiple observation*)
  let sis obs = 
    let obs' = eliminate_masking obs in
    collect_shares obs'                     


module RleakIdOrd = struct
  type t = rleak

  let cmp_s a b = match (a, b) with 
    | (OPA, OPA) -> 0
    | (OPB, OPB) -> 0
    | (OPR, OPR) -> 0
    | (OPW, OPW) -> 0 
    | (_,_) -> 1
  let cmp_sv a b = match (a,b) with
    | ((Mem x), (Mem y)) -> Expr.SKeyOrd.compare x y 
    | ((Reg x), (Reg y)) -> Reg2KeyOrd.compare x y 
    | (_,_) -> 1 

  let cmp_cm a b = match (a,b) with
    | ((Clear x), (Clear y)) -> cmp_s x y
    | ((Scrub x), (Scrub y)) -> cmp_sv x y 
    |(None, None) -> 0
    |(_,_) -> 1

  let rec cmp_masking (a : masking2 list) (b : masking2 list) = match (a , b) with 
    | ([], []) -> 0 
    | ([], _ :: _) -> -1
    | (_ :: _,[]) -> 1
    | ( fa:: rsta, fb:: rstb) -> let c = IntSet.compare fa fb in if c <> 0 then c else cmp_masking rsta rstb  

  let rec cmp_leak (a : dep2 list) (b : dep2 list) = match (a , b) with 
    | ([], []) -> 0 
    | ([], _ :: _) -> -1
    | (_ :: _,[]) -> 1
    | ( fa:: rsta, fb:: rstb) -> let c = ShareSet2.compare fa fb in if c <> 0 then c else cmp_leak rsta rstb
   

  let cmp_cms aarr barr =
    let an = Array.length aarr in 
    let bn = Array.length barr in
    let rec loop  i =
      if (i = an && i = bn) then 0 
      else if i = an then -1
      else if i = bn then 1
      else
      let c = cmp_cm aarr.(i) barr.(i) in
      if c <> 0 then c
      else loop (i + 1) in
    loop 0
  let compare (a : rleak) (b : rleak) =
    let c = Stdlib.compare a.pos b.pos in
    if c <> 0 then c 
    else let c =  Obs.compare a.obs b.obs in 
      if c <> 0 then c 
        else  let c = cmp_cm (a.cm) (b.cm) in
          if c <> 0 then c else Stdlib.compare a.op b.op 
end




module RleakSet = Set.Make(RleakIdOrd)

type rleakset = RleakSet.t

let rleakset_empty : rleakset = RleakSet.empty



type rleakMap = rleakset IntMap.t


type proc2_state = {
  opA : dep2;
  opAM : masking2;
  opAO : observation;  
  opB : dep2;
  opBM : masking2;
  opBO : observation; 
  opR : dep2;
  opRM : masking2;
  opRO : observation; 
  opW : dep2;
  opWM : masking2;
  opWO : observation; 
}

(*type leak2_set = *)
let state2_empty : proc2_state =
  { opA = dep2_empty; opB = dep2_empty; opR = dep2_empty; opW = dep2_empty; 
    opAM = masking2_empty; opBM = masking2_empty; opRM = masking2_empty; opWM = masking2_empty;
    opAO = observation_empty; opBO = observation_empty; opRO = observation_empty; opWO = observation_empty; }


(*enviroment state used to build up state and leakages accross instruction*)    
type env2 = {
  rdep : reg2_deps;
  mdep : mem2_deps;
  robs : observation RegMap2.t;
  mobs : observation ShareMap.t; 
  st   : proc2_state;
  si : share_init2 ;
  regrnd: reg2_rnd; 
  memrnd : mem2_rnd;
  regmasking : reg2_masking; 
  memmasking : mem2_masking;
  srleak : rleakset;
  rleak : rleakset

}



module Securitytransformation (Arch: Arch_full.Arch) = struct

(*Debug functions*)

let debug_rdep = false
let debug_mdep = false
let debug_memmasking = false

let debug_regrnd = false

let debug_state = false

let debug_regmasking = false

let print_scrubvalue (scrubbing : scrubvalue) = match scrubbing with
  | None -> print_string ("None")
  | Mem (x,i) -> print_string (x.v_var.vname.v_name); print_string (" "); print_int(i)
  | Reg x -> print_string(x.v_var.vname.v_name)


let rec print_scrubbing (scrubbings : scrubbing) =
  match scrubbings with 
  | [] -> print_string ""
  | x :: rest -> print_scrubvalue x; print_scrubbing rest

let rec print_scrubbing_list (scrubbings : scrubbing list) =
  match scrubbings with 
  | [] -> print_string ""
  | x :: rest -> print_scrubbing x; print_scrubbing_list rest

let print_clear (state : state) = match state with 
  | OPA -> "opA"
  | OPB -> "opB"
  | OPW -> "opW"
  | OPR -> "opR"

let print_state (state : state) = match state with 
  | OPA -> print_string "opA "
  | OPB -> print_string "opB "
  | OPW -> print_string "opW "
  | OPR -> print_string "opR "
  

let rec print_clearig (clearing:clearing) =  match clearing with 
  | [] -> print_string("")
  | x :: rest -> print_state x; print_clearig rest

let print_scrub (s : scrubvalue) = match s with
  | None -> "None"
  | Mem (x,i) -> Printf.sprintf "Mem (%s, %d)" x.v_var.vname.v_name i
  | Reg x -> Printf.sprintf "Reg (%s)" x.v_var.vname.v_name

let print_cm (cm : cm) = match cm with
| Clear x -> print_clear x 
| Scrub x -> print_scrub x 
| None -> "None"
let print_countermeasure (cm : countermeasures) = 
  Printf.printf "{opA = %b, opB = %b, opR = %b, opW = %b, scrub = %s}\n" 
    cm.clearOPA cm.clearOPB cm.clearOPR cm.clearOPW (print_scrub cm.scrub)

let print_countermeasurres (cm : countermeasures array) =
  let n = Array.length cm in 
  for idx = 0 to n-1 do
    let cm = cm.(idx) in 
      print_countermeasure cm
    done 

let print_dep d = 
  ShareSet2.iter (fun (name, idx) ->
    print_string(name); print_int(idx); print_string(", ");
  ) d
let print_masking m =
  IntSet.iter (fun i ->
    print_int i; print_string " ") m  

let env_empty : env2 = {  rdep = RegMap2.empty; 
                          mdep = ShareMap.empty; 
                          robs = RegMap2.empty; 
                          mobs = ShareMap.empty;
                          st = state2_empty; 
                          si = ShareMap.empty; 
                          regrnd = reg2_rnd_empty; 
                          memrnd = mem2_rnd_empty; 
                          regmasking = reg2_masking_empty;
                          memmasking = mem2_masking_empty;
                          srleak = RleakSet.empty;
                          rleak = RleakSet.empty}

(*Helper functions getter and setter to load and store enviroment variable*)

let get_reg2 (e:env2) (r:var_i) : dep2 =
  let k = r in
  match RegMap2.find_opt k e.rdep with Some d -> d | None -> dep2_empty

let get_si2 (e:env2) ((var_i, ofs):skey) : bool =
  match ShareMap.find_opt (var_i, ofs) e.si with Some d -> d | None -> true

let set_si2 (e:env2) ((var_i, ofs):skey) : env2 =
  { e with si = ShareMap.add (var_i, ofs) true e.si }

let set_reg2 (e:env2) (r:var_i) (d:dep2) : env2 =
  { e with rdep = RegMap2.add r d e.rdep }

let get_mem2 (e:env2) ((var_i, ofs):skey) : dep2 =
  match ShareMap.find_opt (var_i, ofs) e.mdep with Some d -> d | None -> dep2_empty

let set_mem2 (e:env2) ((var_i, ofs):skey) (d:dep2) : env2 =
  { e with mdep = ShareMap.add (var_i, ofs) d e.mdep }

let get_mem2_rnd (e:env2) ((var_i, ofs):skey) : int =
  match ShareMap.find_opt (var_i, ofs) e.memrnd with Some d -> d | None -> -1

let set_mem2_rnd (e:env2) ((var_i, ofs):skey) (d:int) : env2 =
  { e with memrnd = ShareMap.add (var_i, ofs) d e.memrnd }

let get_reg2_rnd (e:env2) (r:var_i) : int =
  match RegMap2.find_opt r e.regrnd with Some d -> d | None -> -1
let set_reg2_rnd (e:env2) (var_i) (d:int) : env2 =
  { e with regrnd = RegMap2.add var_i d e.regrnd }  

let get_mem2_obs (e:env2) ((var_i, ofs):skey) : observation =
  match ShareMap.find_opt (var_i, ofs) e.mobs with Some o -> o | None -> None

let set_mem2_obs (e:env2) ((var_i, ofs):skey) (o:observation) : env2 =
  { e with mobs = ShareMap.add (var_i, ofs) o e.mobs }

  let get_reg2_obs (e:env2) (r:var_i) : observation =
  match RegMap2.find_opt r e.robs with Some o -> o | None -> None

let set_reg2_obs (e:env2) (var_i) (o: observation) : env2 =
  { e with robs = RegMap2.add var_i o e.robs }  

let get_reg2_masking (e:env2) (r:var_i) : masking2 =
  match RegMap2.find_opt r e.regmasking with Some d -> d | None -> masking2_empty

let set_reg2_masking (e:env2) (var_i) (d:masking2) : env2 =
  { e with regmasking = RegMap2.add var_i d e.regmasking } 

let get_mem2_masking (e:env2) (k,i) : masking2 =
  match ShareMap.find_opt (k,i) e.memmasking with Some d -> d | None -> masking2_empty

let set_mem2_masking (e:env2) (k,i) (d:masking2) : env2 =
  { e with memmasking = ShareMap.add (k,i) d e.memmasking } 

let add_rleak (e:env2) (pos : int) (o:observation) (cm : cm) (op: bool) : env2 =

  let rm = e.rleak in 

  let (rleak :rleak) = {pos = pos; cm = cm; obs = o; op = op} in 

  let rm = RleakSet.add rleak rm in 

  { e with rleak = rm }

  let add_srleak (e:env2) (pos : int) (o:observation) (cm : cm) (op: bool) : env2 =

  let rm = e.srleak in 

  let (rleak :rleak) = {pos = pos; cm = cm; obs = o; op = op} in 

  let rm = RleakSet.add rleak rm in 

  { e with srleak = rm }

let add_rleak_output omap (e:env2)  =
  let rm = e.rleak in 
  let rm =  Expr.ShareMap.fold (fun skey si rm ->
        let o = get_mem2_obs e skey in 
        let (rleak :rleak) = {pos = max_int; cm = None; obs = o;  op = true} in 
        RleakSet.add rleak rm 
    ) omap rm
  in
  rm 


(*Helper functions getter and setter above*)


(* Initializeation of the memory dependencies and masking from the annotations input share and randomness map*)
let init2_env_from_input_smap_rmap (smap : smap) (rmap : rmap) : env2 =
  let mdep =
    Expr.ShareMap.fold (fun skey si acc ->
        ShareMap.add skey (dep2_singleton si ) acc
    ) smap ShareMap.empty
  in

  let mem_rnd  =
    Expr.RandomnessMap.fold (fun key si acc ->
        ShareMap.add key si acc
    ) rmap ShareMap.empty
  in
  let mem_masking  =
    Expr.RandomnessMap.fold (fun key si acc ->
        ShareMap.add key (IntSet.singleton si) acc
    ) rmap ShareMap.empty
  in
  let mem_obs = Expr.RandomnessMap.fold (fun key si acc ->
        ShareMap.add key ((Share (None, IntSet.add si IntSet.empty )):observation) acc
    ) rmap ShareMap.empty 
  in
  let mem_obs = Expr.ShareMap.fold (fun key (s,i) acc ->
        ShareMap.add key ((Share (Some (s,i), masking2_empty )):observation) acc
    ) smap mem_obs 
  in

  let si =
    Expr.ShareMap.fold (fun skey si acc ->
        ShareMap.add skey false acc
    ) smap ShareMap.empty
  in
  { rdep = RegMap2.empty; mdep; 
    st = state2_empty; 
    si; regrnd = reg2_rnd_empty; 
    memrnd = mem_rnd; 
    regmasking = reg2_masking_empty; 
    memmasking = mem_masking; 
    srleak = RleakSet.empty;
    rleak = RleakSet.empty; 
    robs = RegMap2.empty;
    mobs = mem_obs}


(* ---------- Per-instruction analysis results ------------------------------ *)

type analysis_results2 = {
  reg_after   : reg2_deps array;     (* full RegDep map AFTER instr i *)
  mem_after   : mem2_deps array;     (* full MemDep map AFTER instr i *)
  state_after : proc2_state array;   (* opA/opB/opR/opW deps AFTER instr i *)
  leak_at     : leak2_set array;          (* leakage deps AT instr i (computed pre-step) *)
  masking_at : masking2 array;              (* masking at instr i (computed pre-step) *)
  countermeasures_at : countermeasures array
}




(*has conflict checks on input dep a dependency set, if dep contains multiple input shares from the same input variable
  Returns true, if there are multiple shares of the same input variable, else false.*)

module StringMap = Map.Make(String)

let rec has_conflict dep stringmap : bool= 
  match dep with 
  | [] -> false
  | (x,i) :: rst -> 
    match (StringMap.find_opt x stringmap) with
      | None -> let stringmap = StringMap.add x i stringmap in has_conflict rst stringmap
      | Some idx -> if i <> idx then true else has_conflict rst stringmap   


let has_conflicting_share_indices (dep : dep2) : bool =
  has_conflict (ShareSet2.elements dep) StringMap.empty

let rec has_conflict_t dep stringmap t : bool= 
  match dep with 
  | [] -> false
  | (x,i) :: rst -> 
    match (StringMap.find_opt x stringmap) with
      | None -> let intset = IntSet.singleton 0 in let stringmap = StringMap.add x intset stringmap in has_conflict_t rst stringmap t
      | Some idx -> let idx = IntSet.add i idx in  
          if t < (IntSet.cardinal idx) then true else has_conflict_t rst (StringMap.add x idx stringmap) t   


let has_conflicting_share_indices_t (dep : dep2) (t :int) : bool =
  
  has_conflict_t (ShareSet2.elements dep) StringMap.empty t 

(*Helper for Leakage and State buildup*)

(* ---------- Step + leakage  ------------ *)

let z_to_int_opt (z : coq_Z) : int option =
  if Sectrafo_util.coqz_fits_int z then (Sectrafo_util.coqz_to_int_opt z) else None

(*Builds the stateful leakage of i if i matches load*)
let build_leak_load (e:env2) (dst:var_i) (src:var_i) (ofs:coq_Z) (w:wsize) (pos : int) : env2 *  leak2_set * countermeasures =
  let modul = match w with
  | U8 -> 1
  | U16 -> 2
  | U32 -> 4
  | U64 -> 8
  | U128 -> 16
  | U256 -> 32
  in
  let ofsint = match z_to_int_opt ofs with
  | Some x ->  x
  | None ->  0
  in 

  let os = get_mem2_obs e  (src, ofsint/ modul) in 
  let oaddr = get_reg2_obs e src in 
  let od = get_reg2_obs e dst in 
  let oa = e.st.opAO in 
  let ob = e.st.opBO in
  let oR = e.st.opRO in 

  (*Build the stateful leakages of the operation LOAD according to the Leakage Operand and the Transient Leakage effects*)

  let loadOP1 : observation= Obs (ObsSet.add oaddr (ObsSet.add ob (ObsSet.singleton oa)), masking2_empty) in

  let loadOPA : observation = Obs (ObsSet.add oaddr (ObsSet.singleton oa), masking2_empty) in

  let loadOPB : observation = Obs (ObsSet.add od (ObsSet.singleton ob), masking2_empty) in

  let loadOPMem : observation = Obs (ObsSet.add os (ObsSet.singleton oR), masking2_empty) in

  let loadTrans : observation = Obs (ObsSet.add od (ObsSet.singleton os), masking2_empty) in

  (*Reduce the stateful leakages of the operation LOAD to the shares and random values needed to simulate the leakages*)

  let sisLOP1 = sis loadOP1 in 
  
  let sisLOPA = sis loadOPA in 
  
  let sisLOPB = sis loadOPB in 
  
  let sisLOPMem = sis loadOPMem in 
  
  let sisLTrans = sis loadTrans in 

  (*Reduce the stateful leakages of the operation LOAD to the random values used in the calculation of the leakages*)

  let rLOP1 = (sis1 loadOP1).masks in 
  
  let rLOPA = (sis1 loadOPA).masks in 
  
  let rLOPB = (sis1 loadOPB).masks in 
  
  let rLOPMem = (sis1 loadOPMem).masks in 
  
  let rLTrans = (sis1 loadTrans).masks in 

  (*Reduce the stateless leakages of the operation LOAD to the shares and random values needed to simulate the leakages*)

  let sisOPA = sis oa in
  let sisOPB = sis ob in
  let sisOPR = sis oR in
  let sisOPS = sis os in
  let sisOPD = sis od in

  (*Reduce the stateless leakages of the operation LOAD to the random values used in the calculation of the leakages*)

  let rOPA = (sis1 oa).masks in
  let rOPB = (sis1 ob).masks in
  let rOPR = (sis1 oR).masks in
  let rOPS = (sis1 os).masks in
  let rOPD = (sis1 od).masks in  
  
  let srl = [(sisOPA, rOPA); (sisOPB, rOPB); (sisOPR,rOPR); (sisOPS,rOPS); (sisOPD, rOPD)] in

  (*Determine if stateful leakages can be simulated by the same values of one of the staless leakages*)

  let sLOP1 = check_obs_security (sisLOP1,rLOP1) srl in
  let sLOPA = check_obs_security (sisLOPA,rLOPA) srl in
  let sLOPB = check_obs_security (sisLOPB,rLOPB) srl in
  let sLOPMem = check_obs_security (sisLOPMem,rLOPMem) srl in
  let sLTrans = check_obs_security (sisLTrans,rLTrans) srl in

  (*Build up observation set containing all stateful observation*)

  let e =  add_rleak e pos loadOP1 (Clear OPA) false in 
  let e = add_rleak e pos loadOPA (Clear OPA) false in
  let e = add_rleak e pos loadOPB (Clear OPB) false in
  let e = add_rleak e pos loadOPMem (Clear OPR) false in  
  let e = add_rleak e pos loadTrans (Scrub (Reg dst)) false in  

  (*Build up operation set containing all stateful observation that are in question of being secure*)

  let e = if not sLOP1 then (add_srleak e pos loadOP1 (Clear OPA) false) else e in 
  let e = if not sLOPA then (add_srleak e pos loadOPA (Clear OPA) false) else e in
  let e = if not sLOPB then (add_srleak e pos loadOPB (Clear OPB) false) else e in
  let e = if not sLOPMem then (add_srleak e pos loadOPMem (Clear OPR) false) else e in  
  let e = if not sLTrans then (add_srleak e pos loadTrans (Scrub (Reg dst)) false) else e in  
  

  let scrub = if (not sLTrans) then (Reg dst) else None in 

  let countermeasure = {clearOPA = not (sLOP1 && sLOPA); 
                        clearOPB = not (sLOP1 && sLOPB); 
                        clearOPR = not (sLOPMem); 
                        clearOPW = false; 
                        scrub = scrub } in 




  (*dep2_union old_dst (dep2_union mem (dep2_union e.st.opR e.st.opA)*)
  let dD = get_reg2 e dst in 
  let dS = get_mem2 e (src, ofsint / modul) in 
  let dA = e.st.opA in
  let dB = e.st.opB in
  let dR = e.st.opR in


  let mD = get_reg2_masking e dst in 
  let mS = get_mem2_masking e (src, ofsint / modul) in 
  let mA = e.st.opAM in
  let mB = e.st.opBM in
  let mR = e.st.opRM in

  let cD = IntSet.cardinal mD in 
  let cS = IntSet.cardinal mS in 
  (*let cA = IntSet.cardinal mA in*)
  let cB = IntSet.cardinal mB in
  let cR = IntSet.cardinal mR in
  

  let leak = let c = IntSet.cardinal (masking2_union (masking2_union mB mS) mA)  in 

                if c > 0 then
                  let c1 = IntSet.cardinal (masking2_union mB mS) in 
                  if c1 > 0 then 
                    let c2 = IntSet.cardinal (masking2_union mA mB) in
                    if c2 > 0 then 
                      let c3 = IntSet.cardinal (masking2_union mA mS) in
                      if c3 > 0 then  dep2_empty 
                      else let dep = dep2_union dA dS in dep
                    else dep2_union dA dB  
                  else dep2_union dB dS 
                else dep2_union (dep2_union dS dB) dA in 


  let rev_opB = let c = IntSet.cardinal (masking2_union mB mD ) in 
                if c > 0 then begin
                  let dD' = if cD > 0 then dep2_empty else dD in 
                  let dB' = if cB > 0 then dep2_empty else dB in 
                  dep2_union dD' dB'
                 end
                else dep2_union dB dD in 

  let rev_opR = let c = IntSet.cardinal (masking2_union mR mS ) in 
                if c > 0 then begin
                  let dS' = if cS > 0 then dep2_empty else dS in 
                  let dR' = if cR > 0 then dep2_empty else dR in 
                  dep2_union dS' dR'
                 end
                else dep2_union dS dR in 
    

  let leak_at = leak2_set_empty in 
  
  
  let leak_at = LeakSet2.add leak leak_at in

  let leak_at = LeakSet2.add rev_opB leak_at in

  let leak_at = LeakSet2.add rev_opR leak_at in


  
  (e, leak_at, countermeasure)
  
  
(*Builds the resedue state after execution of i if i matches load*)
let build_state_load (e:env2) (dst:var_i) (ofs:coq_Z) (src:var_i) (w: wsize): env2 =
  
  let divisor = match w with
  | U8 -> 1
  | U16 -> 2
  | U32 -> 4
  | U64 -> 8
  | U128 -> 16
  | U256 -> 32
  in
  let ofsint = match z_to_int_opt ofs with
  | Some x -> x
  | _ -> 0
  in 

  let mem = get_mem2 e (src, ofsint / divisor) in
  let rnd = get_mem2_rnd e (src, ofsint / divisor) in 

  let om = get_mem2_obs e (src, ofsint / divisor) in
  let os = get_reg2_obs e src in 

  let e1 = set_reg2 e dst mem in
  let e1 = set_reg2_rnd e1 dst rnd in 
  let e1 = set_reg2_obs e1 dst om in 

  let masking = get_mem2_masking e1 (src,ofsint/divisor) in 
  let e1 = set_reg2_masking e1 dst masking in 


  { e1 with st = { e1.st with opR = mem; opA = mem; opB = dep2_empty;
                     opAM = masking; opRM = masking; opBM = masking2_empty;
                     opAO = om; opBO = os; opRO = om}}


(*Builds the stateful leakage of i if i matches xor or and*)
let build_leak_binop (e:env2) (dst:var_i) (v1:var_i) (v2:var_i) (opt:string) (pos : int) : env2 * leak2_set * countermeasures =

  let d1 = get_reg2 e v1 in
  let d2 = get_reg2 e v2 in

  let m1 = get_reg2_masking e v1 in
  let m2 = get_reg2_masking e v2 in 

  let c = IntSet.cardinal (masking2_union m1 m2) in
  let c1 = IntSet.cardinal m1 in
  let c2 = IntSet.cardinal m2 in 

  let d1,d2 = if c > 0 then begin 

                let d1 = if c1 > 0 then dep2_empty else d1 in
                let d2 = if c2 > 0 then dep2_empty else d2 in 
               d1, d2 
              end 
              else d1, d2 in 


  let leak_at = leak2_set_empty in 

  let dep = dep2_union d2 (dep2_union d1 (dep2_union e.st.opB e.st.opA)) in  
  let leak_at = LeakSet2.add dep leak_at in

  let dep = dep2_union d1 d2 in  
  let leak_at = LeakSet2.add dep leak_at in

  let dep = dep2_union e.st.opA d1 in  
  let leak_at = LeakSet2.add dep leak_at in

  let dep = dep2_union e.st.opB d2 in  
  let leak_at = LeakSet2.add dep leak_at in
  
  let od = get_reg2_obs e dst in
  let o1 = get_reg2_obs e v1 in 
  let o2 = get_reg2_obs e v2 in 
  let oa = e.st.opAO in 
  let ob = e.st.opBO in
  

  (*Build the stateful leakages of the operation EOR and AND according to the Leakage Operand and the Transient Leakage effects*)

  let binCR : observation = match opt with 
    | "AND" -> o_and o1 o2 
    | "EOR" -> xor o1 o2
    | _ -> None in 

  let binTrans : observation = Obs (ObsSet.add od (ObsSet.singleton binCR), masking2_empty) in
  
  let binOP : observation = Obs (ObsSet.add o2 (ObsSet.add o1 (ObsSet.add ob (ObsSet.singleton oa))), masking2_empty) in

  let binOPA : observation = Obs (ObsSet.add o1 (ObsSet.singleton oa), masking2_empty) in

  let binOPB : observation = Obs (ObsSet.add o2 (ObsSet.singleton ob), masking2_empty) in

  (*Reduce the stateful leakages of the operation EOR and AND to the shares and random values needed to simulate the leakages*)

  let sisBTrans = sis binTrans in 
  
  let sisBOP = sis binOP in 
  
  let sisBOPA = sis binOPA in 
  
  let sisBOPB = sis binOPB in
  
  (*Reduce the stateful leakages of the operation EOR and AND to the random values needed to calculate the leakages*)

  let rBTrans = (sis1 binTrans).masks in 
  
  let rBOP = (sis1 binOP).masks in 
  
  let rBOPA = (sis1 binOPA).masks in 
  
  let rBOPB = (sis1 binOPB).masks in
  

  (*Reduce the stateless leakages of the operation EOR and AND to the shares and random values needed to simulate the leakages*)
  
  let sisOP = sis (Obs (ObsSet.add o1 (ObsSet.singleton o2), masking2_empty)) in 
  let sisOPA = sis oa in
  let sisOPB = sis ob in
  let sisCR = sis binCR in

  (*Reduce the stateless leakages of the operation EOR and AND to the shares and random values needed to simulate the leakages*)
  
  let rOP = (sis1 (Obs (ObsSet.add o1 (ObsSet.singleton o2), masking2_empty))).masks in 
  let rOPA = (sis1 oa).masks in
  let rOPB = (sis1 ob).masks in
  let rCR = (sis1 binCR).masks in

  (*let sl = [sisOP; sisCR; sisOPA; sisOPB] in*) 
  let srl = [(sisOP,rOP); (sisCR,rCR); (sisOPA,rOPA); (sisOPB,rOPB)] in 

  (*Determine if stateful leakages can be simulated by the same values of one of the staless leakages*)

  let sBTrans = check_obs_security (sisBTrans,rBTrans) srl in
  let sBOP = check_obs_security (sisBOP,rBOP) srl in
  let sBOPA = check_obs_security (sisBOPA,rBOPA) srl in
  let sBOPB = check_obs_security (sisBOPB,rBOPB) srl in

  (*Build up operation set containing all stateful observation *)

  let e = add_rleak e pos binTrans (Scrub (Reg dst)) false in 
  let e = add_rleak e pos binOP (Clear OPA) false in
  let e = add_rleak e pos binOPA (Clear OPA) false in
  let e = add_rleak e pos binOPB (Clear OPB) false in

  (*Build up operation set containing all stateful observation that are in question of being secure*)

  let e = if not sBTrans then (add_srleak e pos binTrans (Scrub (Reg dst)) false) else e in 
  let e = if not sBOP then (add_srleak e pos binOP (Clear OPA) false) else e in
  let e = if not sBOPA then (add_srleak e pos binOPA (Clear OPA) false) else e in
  let e = if not sBOPB then (add_srleak e pos binOPB (Clear OPB) false) else e in    

  let scrub = if (not sBTrans) then (Reg dst) else None in 

  let countermeasure = {clearOPA = not (sBOP && sBOPA); 
                        clearOPB = not (sBOP && sBOPB); 
                        clearOPR = false; 
                        clearOPW = false; 
                        scrub = scrub } in 

  (*let countermeasure = {countermeasures_empty with clearOPA = true; clearOPB = true} in *)

  
  (e, leak_at, countermeasure)
  
(*Builds the resedue state after execution of i if i matches xor or and*)
let build_state_binop (e:env2) (dst:var_i) (v1:var_i) (v2:var_i) (opt:string) : env2 =

  let d1 = get_reg2 e v1 in
  let d2 = get_reg2 e v2 in

  let m1 = get_reg2_masking e v1 in
  let m2 = get_reg2_masking e v2 in 

  let c1 = IntSet.cardinal m1 in
  let c2 = IntSet.cardinal m2 in 

  let o1 = get_reg2_obs e v1 in
  let o2 = get_reg2_obs e v2 in 


  let e1 = {e with st = {e.st with opA = d1; opB = d2; opAM = m1; opBM = m2; opAO = o1; opBO = o2}} in 


  let e1 = match opt with
  | "EOR" -> 
    set_reg2_obs e1 v1 (xor o1 o2)  
  | "AND" -> 
    set_reg2_obs e1 v1 (o_and o1 o2)
  | _ -> e1 in 



  let e1 = if (c1 >= 0 || c2 >= 0 ) then begin 
    let e1 = match opt with
    | "EOR" -> set_reg2_masking e1 dst (masking2_union m1 m2)
    | "AND" -> set_reg2_masking e1 dst masking2_empty
    | _ -> e1 in 
    let e1 = match opt with
        | "EOR" -> set_reg2 e1 dst (dep2_union d1 d2)
        | "AND" -> 
            if (c1 > 0 && c2 > 0) then begin let e1 = set_reg2 e1 dst dep2_empty in e1 end else 
            if (c1 > 0) then begin let e1 = set_reg2 e1 dst (dep2_union dep2_empty d2) in e1 end else 
            if (c2 > 0) then begin let e1 = set_reg2 e1 dst (dep2_union dep2_empty d1) in e1 end else begin
              let e1 = set_reg2 e1 dst (dep2_union d1 d2) in e1   end
        | _ -> e1 in
    e1
  end
  else e1 in 

  { e1 with st = { e1.st with opA = d1; opB = d2; opAM = m1; opBM = m2 } }


  (*Builds the stateful leakage of i if i matches move*)
  let build_leak_move (e:env2) (dst:var_i) (src:var_i) (pos : int) : env2 * leak2_set * countermeasures =
  let old_dst = get_reg2 e dst in
  let dsrc = get_reg2 e src in

  let leak_at = leak2_set_empty in 

  let dep = dep2_union old_dst (dep2_union dsrc (dep2_union e.st.opA e.st.opB)) in  
  let leak_at = LeakSet2.add dep leak_at in

  let dep = dep2_union e.st.opB dsrc in  
  let leak_at = LeakSet2.add dep leak_at in

  let dep = dep2_union e.st.opA old_dst in  
  let leak_at = LeakSet2.add dep leak_at in


  let od = get_reg2_obs e dst in
  let os = get_reg2_obs e src in 
  let oa = e.st.opAO in 
  let ob = e.st.opBO in
  

  (*Build the stateful leakages of the operation MOVE according to the Leakage Operand and the Transient Leakage effects*)

  let movCR : observation = os in 

  let movTrans : observation = Obs (ObsSet.add od (ObsSet.singleton os), masking2_empty) in
  
  let movOP : observation = Obs (ObsSet.add od (ObsSet.add os (ObsSet.add ob (ObsSet.singleton oa))), masking2_empty) in

  (*Reduce the stateful leakages of the operation EOR and AND to the shares and random values needed to simulate the leakages*)

  let sisMCR = sis movCR in

  let sisMTrans = sis movTrans in 
  
  let sisMOP = sis movOP in 
  
  
  (*Reduce the stateful leakages of the operation MOVE to the random values needed to calculate the leakages*)

  let rMCR = (sis1 movCR).masks in 

  let rMTrans = (sis1 movTrans).masks in 
  
  let rMOP = (sis1 movOP).masks in 
  

  (*Reduce the stateless leakages of the operation MOVE to the shares and random values needed to simulate the leakages*)
  
  let sisOP = sis (Obs (ObsSet.add os (ObsSet.singleton od), masking2_empty)) in 
  let sisOPA = sis oa in
  let sisOPB = sis ob in
  let sisCR = sis os in

  (*Reduce the stateless leakages of the operation MOVE to the shares and random values needed to simulate the leakages*)
  
  let rOP = (sis1 (Obs (ObsSet.add os (ObsSet.singleton od), masking2_empty))).masks in 
  let rOPA = (sis1 oa).masks in
  let rOPB = (sis1 ob).masks in
  let rCR = (sis1 os).masks in

  (*let sl = [sisOP; sisCR; sisOPA; sisOPB] in*) 
  let srl = [(sisOP,rOP); (sisCR,rCR); (sisOPA,rOPA); (sisOPB,rOPB)] in 

  (*Determine if stateful leakages can be simulated by the same values of one of the staless leakages*)

  let sMCR = check_obs_security (sisMCR,rMCR) srl in
  let sMTrans = check_obs_security (sisMTrans,rMTrans) srl in
  let sMOP = check_obs_security (sisMOP,rMOP) srl in

  (*Build up operation set containing all stateful observation *)
  let e = add_rleak e pos movCR None false in
  let e = add_rleak e pos movTrans (Scrub (Reg dst)) false in 
  let e = add_rleak e pos movOP (Clear OPA) false in

  (*Build up operation set containing all stateful observation that are in question of being secure*)

  let e = if not sMCR then (add_srleak e pos movCR None false) else e in
  let e = if not sMTrans then (add_srleak e pos movTrans (Scrub (Reg dst)) false) else e in 
  let e = if not sMOP then (add_srleak e pos movOP (Clear OPA) false) else e in

  let scrub = if (not sMTrans) then (Reg dst) else None in 

  let countermeasure = {clearOPA = not (sMOP); 
                        clearOPB = not (sMOP); 
                        clearOPR = false; 
                        clearOPW = false; 
                        scrub = scrub } in

  
  (e, leak_at, countermeasure)

(*Builds the resedue state after execution of i if i matches move*)
let build_state_move (e:env2) (dst:var_i) (src:var_i) : env2 =
  let dsrc = get_reg2 e src in
  let rnd = get_reg2_rnd e src in 
  
  let m1 = get_reg2_masking e src in
  let m2 = get_reg2_masking e dst in 

  let od = get_reg2_obs e dst in
  let os = get_reg2_obs e src in

  let e1 = set_reg2_obs e dst os in 

  let e1 = set_reg2 e1 dst dsrc in
  let e1 = set_reg2_rnd e1 dst rnd in

  { e1 with st = { e1.st with opA = (get_reg2 e src); opB = dsrc; opAM = m2; opBM = m1; opAO = od; opBO = os  } }

(*Builds the stateful leakage of i if i matches store*)
let build_leak_store (e:env2) (dst:var_i) (ofs:coq_Z) (src:var_i) (w: wsize) (pos : int) : env2 * leak2_set * countermeasures =

  let dsrc = get_reg2 e src in
  let divisor = match w with
  | U8 -> 1
  | U16 -> 2
  | U32 -> 4
  | U64 -> 8
  | U128 -> 16
  | U256 -> 32
  in
  let ofsint = match z_to_int_opt ofs with
  | Some x -> x
  | _ -> 0
  in 

  let old_mem = get_mem2 e (dst, ofsint / divisor) in

  (*dep2_union dsrc (dep2_union old_mem (dep2_union e.st.opW e.st.opB))*)
  
  let leak_at = leak2_set_empty in 

  let dep = dep2_union old_mem (dep2_union e.st.opB e.st.opA) in  
  let leak_at = LeakSet2.add dep leak_at in

  let dep = dep2_union e.st.opB dsrc in  
  let leak_at = LeakSet2.add dep leak_at in

  let dep = dep2_union e.st.opA old_mem in  
  let leak_at = LeakSet2.add dep leak_at in

  let dep = dep2_union e.st.opW dsrc in  
  let leak_at = LeakSet2.add dep leak_at in
  

  let od = get_mem2_obs e  (dst, ofsint/ divisor) in 
  let oaddr = get_reg2_obs e dst in 
  let os = get_reg2_obs e src in 
  let oa = e.st.opAO in 
  let ob = e.st.opBO in
  let ow = e.st.opWO in 

  (*Build the stateful leakages of the operation STORE according to the Leakage Operand and the Transient Leakage effects*)

  let storeOP1 : observation= Obs (ObsSet.add oaddr (ObsSet.add ob (ObsSet.singleton oa)), masking2_empty) in

  let storeOPA : observation = Obs (ObsSet.add oaddr (ObsSet.singleton oa), masking2_empty) in

  let storeOPB : observation = Obs (ObsSet.add os (ObsSet.singleton ob), masking2_empty) in

  let storeOPMem : observation = Obs (ObsSet.add os (ObsSet.singleton ow), masking2_empty) in

  let storeTrans : observation = Obs (ObsSet.add od (ObsSet.singleton os), masking2_empty) in

  (*Reduce the stateful leakages of the operation STORE to the shares and random values needed to simulate the leakages*)

  let sisSOP1 = sis storeOP1 in 
  
  let sisSOPA = sis storeOPA in 
  
  let sisSOPB = sis storeOPB in 
  
  let sisSOPMem = sis storeOPMem in 
  
  let sisSTrans = sis storeTrans in 

  (*Reduce the stateful leakages of the operation STORE to the random values needed to calculate the leakages*)

  let rSOP1 = (sis1 storeOP1).masks in 
  
  let rSOPA = (sis1 storeOPA).masks in 
  
  let rSOPB = (sis1 storeOPB).masks in 
  
  let rSOPMem = (sis1 storeOPMem).masks in 
  
  let rSTrans = (sis1 storeTrans).masks in 

  (*Reduce the stateless leakages of the operation STORE to the shares and random values needed to simulate the leakages*)

  let sisOPA = sis oa in
  let sisOPB = sis ob in
  let sisOPW = sis ow in
  let sisOPS = sis os in
  let sisOPD = sis od in

  (*Reduce the stateless leakages of the operation STORE to the random values needed to calculate the leakages*)

  let rOPA = (sis1 oa).masks in
  let rOPB = (sis1 ob).masks in
  let rOPW = (sis1 ow).masks in
  let rOPS = (sis1 os).masks in
  let rOPD = (sis1 od).masks in  

  (*let sl = [sisOPA; sisOPB; sisOPW; sisOPS; sisOPD] in*) 
  let srl = [(sisOPA,rOPA); (sisOPB,rOPB); (sisOPW,rOPW); (sisOPS,rOPS); (sisOPD,rOPD)] in 

  (*Determine if stateful leakages can be simulated by the same values of one of the staless leakages*)

  let sSOP1 = check_obs_security (sisSOP1,rSOP1) srl in
  let sSOPA = check_obs_security (sisSOPA,rSOPA) srl in
  let sSOPB = check_obs_security (sisSOPB,rSOPB) srl in
  let sSOPMem = check_obs_security (sisSOPMem,rSOPMem) srl in
  let sSTrans = check_obs_security (sisSTrans,rSTrans) srl in

  (*Build up operation set containing all stateful*)

  let e = add_rleak e pos storeOP1 (Clear OPA) false in 
  let e = add_rleak e pos storeOPA (Clear OPA) false in
  let e = add_rleak e pos storeOPB (Clear OPB) false in
  let e = add_rleak e pos storeOPMem (Clear OPW) false in  
  let e = add_rleak e pos storeOP1 (Scrub (Mem (dst, ofsint /divisor))) false in

  (*Build up operation set containing all stateful observation that are in question of being secure*)

  let e = if not sSOP1 then (add_srleak e pos storeOP1 (Clear OPA) false) else e in 
  let e = if not sSOPA then (add_srleak e pos storeOPA (Clear OPA) false) else e in
  let e = if not sSOPB then (add_srleak e pos storeOPB (Clear OPB) false) else e in
  let e = if not sSOPMem then (add_srleak e pos storeOPMem (Clear OPW) false) else e in  
  let e = if not sSTrans then (add_srleak e pos storeOP1 (Scrub (Mem (dst, ofsint /divisor))) false) else e in  

  let scrub = if (not sSTrans) then (Mem (dst, ofsint /divisor)) else None in 

  let countermeasure = {clearOPA = not (sSOP1 && sSOPA); 
                        clearOPB = not (sSOP1 && sSOPB); 
                        clearOPR = false; 
                        clearOPW = not (sSOPMem); 
                        scrub = scrub } in 

  (*let countermeasure = {countermeasures_empty with clearOPA = true; clearOPB = true; clearOPR = true; scrub = (Mem (dst, ofsint / divisor))} in *)

  
  (e, leak_at, countermeasure)
  


(*Builds the resedue state after execution of i if i matches store*)
let build_state_store (e:env2) (dst : var_i) (ofs:coq_Z) (src:var_i) (w:wsize) : env2 =

    let dsrc = get_reg2 e src in
    let rnd = get_reg2_rnd e src in 
    let divisior = match w with
      | U8 -> 1
      | U16 -> 2
      | U32 -> 4
      | U64 -> 8
      | U128 -> 16
      | U256 -> 32
    in
    let ofsint = match z_to_int_opt ofs with
      | Some x -> x
      | _ -> 0
    in 

    let os = get_reg2_obs e src in 
    let od = get_reg2_obs e dst in 

    let e1 = set_mem2_obs e (dst, ofsint/divisior) os in 

    let e1 = set_mem2 e1 (dst, ofsint /divisior) dsrc in
    let e1 = set_mem2_rnd e1 (dst, ofsint/divisior) rnd in

    let mask = get_reg2_masking e1 src in 
    let e1 =  set_mem2_masking e1 (dst, (ofsint/divisior)) mask in  

    { e1 with st = { e1.st with opW = dsrc; opB = dsrc; opA = dep2_empty;
                     opBM = mask; opWM = mask; opAM = masking2_empty;
                     opAO = od; opBO = os; opWO = os} }
  




(* Match Operations: Load, Mov, Store, And, XOR*)
let var_of_pexpr2 (e : pexpr) : var_i option =
  match e with
  | Pvar gv -> Some gv.gv
  | _ -> None



let offset_of_lval_mem2 (lv : lval) : (var_i * coq_Z * wsize) option =
  match lv with
  | Lmem (_al, _ws, _vi, addr) -> offset_of_addr_expr2 addr
  | _ -> None

(*helper function that matches instruction i to instruction type xor or and*)
let match_binop_regs2 (i : 'asm_op instr) (asmop): (var_i * var_i * var_i * string) option =
  match i with
  | MkI (_, Copn (outs, _tag, op, ins)) ->
      begin match op, outs, ins with
      | Oasm o0, [Lvar dst], [e1; e2] ->
          let opname = ((asmop.asm_op_instr o0).str ()) in 
          begin match var_of_pexpr2 e1, var_of_pexpr2 e2 with
          | Some v1, Some v2 -> Some (dst, v1, v2 ,opname)
          | _ -> None
          end
      | _ -> None
      end
  | _ -> None

(*helper function that matches instruction i to instruction type move*)
let match_mov_reg2 (i : 'asm_op instr) : (var_i * var_i) option =
  match i with
  | MkI (_, Copn (outs, _tag, op, ins)) ->
      begin match op, outs, ins with
      | Oasm _, [Lvar dst], [e1] ->
          begin match var_of_pexpr2 e1 with
          | Some src -> Some (dst, src)
          | None -> None
          end
      | _ -> None
      end
  | _ -> None

(*helper function that matches instruction i to instruction type store*)
let match_store_sp_ofs2 asmop (i : 'asm_op instr) : (var_i* coq_Z * var_i * wsize) option =
  match i with
  | MkI (_, Copn (outs, _tag, op, ins)) ->
      begin match op, outs, ins with
      | Oasm o0, [lv], [src_e] ->
          let opname = ((asmop.asm_op_instr o0).str ()) in 
          
          begin match opname with
          | "STR" -> 
            begin match offset_of_lval_mem2 lv, var_of_pexpr2 src_e with
            | Some (dst, ofs, w), Some src -> Some (dst, ofs, src, w)
            | _ -> None
            end
          | _ -> None end
          
      | _ -> None
      end
  | _ -> None

(*Build Leak is part of Transform and builds the stateful leakage of i*) 
let build_leak asmop (e:env2) (i:'asm_op instr) (pos : int) : env2 * leak2_set * countermeasures =  
  match match_load_ofs2 i asmop with
  | Some (dst, src, ofs, w) -> build_leak_load e dst src ofs w pos
  | None ->
    match match_store_sp_ofs2 asmop i with
    | Some ((dst:var_i), (ofs:coq_Z), (src:var_i), (w: wsize)) -> build_leak_store e dst ofs src w pos
    | None ->
      match match_binop_regs2 i asmop with
      | Some (dst, v1, v2, op ) -> build_leak_binop e dst v1 v2 op pos
      | None ->
        match match_mov_reg2 i with
        | Some (dst, src) -> build_leak_move e dst src pos
        | None -> (e, leak2_set_empty, countermeasures_empty)

(*Build state is part of Transform and builds the resedue state after execution of i*)
let build_state asmop (e:env2) (i:'asm_op instr) : env2 =
  match match_load_ofs2 i asmop with
  | Some (dst, src, ofs, w) -> build_state_load e dst ofs src w
  | None ->
    match match_store_sp_ofs2 asmop i with
    | Some (dst, ofs, src, w) -> build_state_store e dst ofs src w
    | None ->
      match match_binop_regs2 i asmop with
      | Some (dst, v1, v2, opn) -> build_state_binop e dst v1 v2 opn
      | None ->
        match match_mov_reg2 i with
        | Some (dst, src) -> build_state_move e dst src
        | None -> e

let debug_full_analysis = false


let obs_list_to_set ol =
  List.fold_left (fun acc o -> ObsSet.add o acc) ObsSet.empty ol

(*Helper function that checks the security of one subset*)
let check_security s t = 
  if s = [] then false else begin 
  (*print_endline (Printf.sprintf "Size of s =  %i " (List.length s));*)
  let o : observation = Obs((obs_list_to_set s), masking2_empty ) in 
  let sis = sis o in 
  has_conflicting_share_indices_t sis  t end 
  

(*exists subsets checks if in all observation exists a security breaking subset of at most size t*)
let exists_subset (a_set : RleakSet.t ) (b_set: RleakSet.t) t (cm_at : countermeasures array) =
  let a_arr = Array.of_list (RleakSet.elements a_set) in
  let b_arr = Array.of_list (RleakSet.elements b_set) in
  let n = Array.length b_arr in
  let k = t - 1  in
  let s = 1 in
  let rec choose k s start (acc: observation list) op (a:rleak) =
    (*print_endline (Printf.sprintf "t = %i Size of k =  %i, size of s = %i and size of start = %i " t k s start );*)
    if k = 0 then begin     
      check_security (a.obs :: acc) (s - op) && not (check_security (acc) (s - op -1)) end 
    else if start >= n then
      false
    else
      if (check_security (a.obs :: acc) (s - op) && not (check_security (acc) (s - op -1))) then begin  true end else 
        let x = b_arr.(start) in 
        let op = if x.op then op + 1 else op in 
        if choose (k - 1) (s + 1) (start + 1) (x.obs :: acc) 0 a then
          true
        else
          choose k s (start + 1) acc op a
  in
  Array.iter (fun (a:rleak) ->   
    if  (choose k s 0 [] 0 a) 
      then begin 
        let cm = match a.cm with 
        | Clear OPA -> {cm_at.(a.pos) with clearOPA = true}
        | Clear OPB -> {cm_at.(a.pos) with clearOPB = true}
        | Clear OPR -> {cm_at.(a.pos) with clearOPR = true}
        | Clear OPW -> {cm_at.(a.pos) with clearOPW = true}
        | Scrub x -> {cm_at.(a.pos) with scrub = (x)}
        | None -> cm_at.(a.pos) in 
        cm_at.(a.pos) <- cm end 
      else ()) a_arr
  

(*reduce analyzes the stateful leakages for all instruction i and taggs
i for clearing if needed*)
let reduce asmop (input_map:smap) (rmap:rmap) (omap:smap) (body : 'asm_op instr list) t : analysis_results2 =
  (*print_string ("\n\nHere is the Start of Analyzebody\n\n");*)
  let ins_arr = Array.of_list body in
  let n = Array.length ins_arr in

  let reg_after = Array.make n RegMap2.empty in

  let mem_after   = Array.make n ShareMap.empty in
  let state_after = Array.make n state2_empty in
  let leak_at     = Array.make n leak2_set_empty in
  let countermeasures_at     = Array.make n countermeasures_empty in
  let masking_at = Array.make n masking2_empty in 

  let e = ref (init2_env_from_input_smap_rmap input_map rmap) in

  for idx = 0 to n - 1 do
    let i = ins_arr.(idx) in

    (* 1) compute leak using old env/state *)
    let (e', lk, cm) = build_leak asmop !e i idx in
    leak_at.(idx) <- lk;
    (*countermeasures_at.(idx) <- cm;*) 
    e := e';

    
    (* 2) step to new env *)
    let e' = build_state asmop !e i in


    (*let mask = !e.regmasking in 
    masking_at.(idx) <- mask;*)

    (* 3) snapshot AFTER this instruction *)
    reg_after.(idx) <- e'.rdep;
    mem_after.(idx) <- e'.mdep;
    state_after.(idx) <- e'.st;

    e := e'
  done;
  
  
  let rset = RleakSet. union (!e.rleak) (add_rleak_output omap !e) in 

  let srset = !e.srleak  in
  (*let srset = !e.rleak  in*)


  exists_subset srset rset t countermeasures_at; 
   

  { reg_after; mem_after; state_after; leak_at; masking_at; countermeasures_at}

(*get_Information and get_lvar are helper functions that collect meta information needed to 
build countermeasure instructions*)
let get_Information instr = match instr with 
  | MkI (ii, _) -> ii

let get_lvar i asmop : Expr.var_i option = 
  match match_load_ofs2 i asmop with
  | Some (dst, src, ofs, w) -> Some dst
  | None ->
    match match_store_sp_ofs2 asmop i with
    | Some ((dst:var_i), (ofs:coq_Z), (src:var_i), (w: wsize)) -> Some dst
    | None ->
      match match_binop_regs2 i asmop with
      | Some (dst, v1, v2, op ) -> Some dst
      | None ->
        match match_mov_reg2 i with
        | Some (dst, src) -> Some dst
        | None -> None

module VarISet = Set.Make(Reg2KeyOrd)

(*Helper function determining all registers used in calculation*)
let regs_of_reg_after (reg_after ) : VarISet.t =
  Array.fold_left (fun acc m ->
    RegMap2.fold (fun (r : var_i) (d ) acc -> 
      VarISet.add r acc
    ) m acc
  ) VarISet.empty reg_after

  (*Helper fumction to inserting one clean up scrub*)
  let scrubs_for_all_regs 
  ~(asmop : 'asm_op asmOp)
  ~(ii : IInfo.t)
  (regs : VarISet.t) r0 w : 'asm_op instr list =
  regs
  |> VarISet.elements
  |> List.concat_map (fun r ->
      let scrub = 
       Sectrafo_util_clearing.register_scrub ~asmop ~pd:w ~ii ~reg:r in
       let gvar = {gv = r0; gs = Slocal} in
       let clear = 
       Sectrafo_util_clearing.clear_opA_and_opB ~asmop ~ii ~r0:r0 ~r0_gv:gvar in
      [scrub; clear]
     )
  
  (*Scrubs all registers used in the calculation by insertion of clean up countermeasures*)
  let scrub_all_regs_from_reg_after
  ~(asmop : 'asm_op asmOp)
  ~(reg_after : reg2_deps array)
  (body : 'asm_op instr list) r0 m0 w : 'asm_op instr list =
  match body with
  | [] -> []
  | first_i :: _ ->
      
      let w = match match_load_ofs2 first_i asmop with
        | Some (dst, src, ofs, w) -> w
          | None ->
            match match_store_sp_ofs2 asmop first_i with
            | Some ((dst:var_i), (ofs:coq_Z), (src:var_i), (w: wsize)) -> w
            | None ->
              match match_binop_regs2 first_i asmop with
              | Some (dst, v1, v2, op ) -> w
              | None ->
                match match_mov_reg2 first_i with
                | Some (dst, src) -> w
                | None -> w
        in

      let ii = Sectrafo_util_clearing.ii_of_instr first_i in
      let regs = regs_of_reg_after reg_after in
      let scrubs = scrubs_for_all_regs ~asmop ~ii regs r0 w in
      let (m,ofs) = m0 in

      
      let gv_r0 = {gv = r0; gs = Slocal} in
      let gv_m0 = {gv = m; gs = Slocal} in

      let mult = match w with
            | U8 -> 1 * (ofs + 1)
            | U16 -> 2 * (ofs + 1)
            | U32 -> 4 * (ofs + 1)
            | U64 -> 8 * (ofs + 1)
            | U128 -> 16 * (ofs + 1)
            | U256 -> 32 * (ofs + 1) in
      let mult1 = coqZ_of_int mult in 

      let clearOPAB = Sectrafo_util_clearing.clear_opA_and_opB ~asmop ~ii:ii ~r0:r0 ~r0_gv:gv_r0 in      
      let clearOPW = Sectrafo_util_clearing.clear_opW ~asmop ~pd:w ~ii ~dst:gv_r0 ~sp_gv:gv_m0 ~ofs:mult1 ~al:Aligned ~ws:w in
      let clearOPR = Sectrafo_util_clearing.clear_opR ~asmop ~pd:w ~ii ~dst:r0 ~sp_gv:gv_m0 ~ofs:mult ~al:Aligned ~ws:w in

       body @ [clearOPAB] @ scrubs @ [clearOPW] @ [clearOPR] 


(*Helper function to check if i is tagged for scrubbing*)
let  has_scrub_reg s : var_i option = match s with
    | Reg x -> Some x
    | _ -> None

let has_scrub_mem s : (var_i * int) option = match s with
    | Mem x -> Some x
    | _ -> None
 
let debug_insert_countermeasure = false

(*Transforms one i into a hardend hi by inserting countermeasures clear(OPX) or scrub(V), if clearing
  of opA, opB, opR, opW ord R is tagged *)
let insert_counternmeasure asmop instr (cm :countermeasures) r0 (m0:rkey) i : 'asm_op instr list= 
  let instr_list = [] in 
  (*let (scrub, clear) = cm in*)

  let opA = cm.clearOPA in 
  let opB = cm.clearOPB in 
  let opR = cm.clearOPR in 
  let opW = cm.clearOPW in 
  let sReg = has_scrub_reg cm.scrub in 
  let sMem = has_scrub_mem cm.scrub in 

  let lvar = get_lvar instr asmop in 
  let info = get_Information instr in 
  
  let gv_r0 = {gv = r0; gs = Slocal} in 
  let m0Var, m0Ofs = m0 in
  let gv_m0 = {gv = m0Var; gs = Slocal} in 
  

  let instr_list = match sReg with
   | None -> if opR 
      then begin
        match match_load_ofs2 instr asmop with
          | None -> instr_list 
          | Some (d, _, _, w) -> let mult = match w with
            | U8 -> 1 * (m0Ofs + 1)
            | U16 -> 2 * (m0Ofs + 1)
            | U32 -> 4 * (m0Ofs + 1)
            | U64 -> 8 * (m0Ofs + 1)
            | U128 -> 16 * (m0Ofs + 1)
            | U256 -> 32 * (m0Ofs + 1) in  
          let i = Sectrafo_util_clearing.clear_opR ~asmop ~pd:w ~ii:info ~dst:d ~sp_gv:gv_m0 ~ofs:mult ~al:Aligned ~ws:w in  
          i :: instr_list
        end   
      else instr_list
   | Some x -> match match_load_ofs2 instr asmop with
          | None -> instr_list 
          | Some (d, _, _, w) -> let mult = match w with
            | U8 -> 1 * (m0Ofs + 1)
            | U16 -> 2 * (m0Ofs + 1)
            | U32 -> 4 * (m0Ofs + 1)
            | U64 -> 8 * (m0Ofs + 1)
            | U128 -> 16 * (m0Ofs + 1)
            | U256 -> 32 * (m0Ofs + 1) in  
          let i = Sectrafo_util_clearing.clear_opR ~asmop ~pd:w ~ii:info ~dst:d ~sp_gv:gv_m0 ~ofs:mult ~al:Aligned ~ws:w in  
          i :: instr_list 
  in

  

   let instr_list = match sMem with
      | None -> if opW 
              then begin  match match_store_sp_ofs2 asmop instr with 
                | Some (r, i, _ , w) -> 
                  let gv_r = {gv = r; gs=Slocal} in 
                  let i = Sectrafo_util_clearing.clear_opW ~asmop ~pd:w ~ii:info ~dst:gv_r0 ~sp_gv:gv_r ~ofs:i ~al:Aligned ~ws:w in
                  i::instr_list
                | None -> instr_list
                end else instr_list
        | Some _ -> match match_store_sp_ofs2 asmop instr with 
                  | Some (r, i, _ , w) -> 
                    let gv_r = {gv = r; gs=Slocal} in 
                    let i = Sectrafo_util_clearing.clear_opW ~asmop ~pd:w ~ii:info ~dst:gv_r0 ~sp_gv:gv_r ~ofs:i ~al:Aligned ~ws:w in 
                    i::instr_list
                  | None -> instr_list 
      in

  
  let instr_list = if ((opA || opB || not (sReg = None) || not (sMem = None) )) then begin 
       match lvar  with
        | Some var -> 
            let gvar = {gv = r0; gs = Slocal} in 
            
            let i = Sectrafo_util_clearing.clear_opA_and_opB ~asmop ~ii:info ~r0:r0 ~r0_gv:gvar in
            i::instr_list
        | None ->  instr_list end
      else instr_list in  
  instr_list

  
(*Harden replaces all instruction i of fbody with harden hi by inserting countermeasures
if an observation is tagged for clearing in the countermeasures array cms *)
let rec harden asmop (fbody : 'asm_op instr list) cms idx r0 (m0:rkey) = match fbody with 
    |[] -> []
    | i :: rst ->  (insert_counternmeasure asmop i cms.(idx) r0 m0 idx) @ [i] @ (harden asmop rst cms (idx+1) r0 m0)    



let tag_deps_in_body2 asmop smap rmap omap (body : 'asm_op instr list) r0 (m0:rkey) t w  : 'asm_op instr list =
  
  let res = reduce asmop smap rmap omap body t in
  
  let cms = res.countermeasures_at in 

  let body = harden asmop body cms (0) r0 m0 in 
  
  let body = scrub_all_regs_from_reg_after ~asmop ~reg_after:res.reg_after body r0 m0 w in 

  body

  
(* Transform all functions in the program (shell: identity transform_fun_body). *)
let security_transform_sprog asmop p  =
  
  let transform_fundef (fn, fd) =
    
    let smap = fd.f_extra.sf_masking_layout in 
    let rmap = fd.f_extra.sf_random_layout in 
    let omap = fd.f_extra.sf_output_layout in 

    let t = (Expr.ShareMap.fold (fun k (s,v) t -> if v > t then v else t ) smap 0)  in 
    let w = U32 in 
    
    let (r0, _), _ = ShareMap.min_binding smap in 

    let m0, _ = RandomnessMap.max_binding rmap in 
  
  
    let body' = tag_deps_in_body2 asmop smap rmap omap fd.f_body r0 m0 t w in
    (fn, { fd with f_body = body' })
  in

  { p with p_funcs = List.map transform_fundef p.p_funcs }
      
end
open Utils
open Prog

module TagSet = Set.Make(String)

type tagset = TagSet.t

let pp_tagset fmt s =
  let xs = TagSet.elements s in
  Format.fprintf fmt "{%a}"
    (pp_list ", " Format.pp_print_string) xs

(* --- Configuration: adapt naming convention here --- *)

let is_random_name (name:string) : bool =
  (* rnd, rnd0, rnd_x, rnd123, etc. *)
  BatString.starts_with name "rnd"

let share_of_name (name:string) : (string * int) option =
  (* Accept:
      - a#0, a#1, in1#0, in1#1
      - a_0, a_1
      - a0, a1  (optional; commented by default)
  *)
  let try_hash () =
    match BatString.rsplit name ~by:"#"
    with
    | base, idx ->
      (try Some (base, int_of_string idx) with _ -> None)
    | exception _ -> None
  in
  let try_uscore () =
    match BatString.rsplit name ~by:"_"
    with
    | base, idx ->
      (try Some (base, int_of_string idx) with _ -> None)
    | exception _ -> None
  in
  (* Optional “a0/a1” parsing: enable if you really use that convention
  let try_digit_suffix () =
    let n = String.length name in
    if n >= 2 then
      let d = name.[n-1] in
      if d = '0' || d = '1' then
        Some (String.sub name 0 (n-1), (Char.code d - Char.code '0'))
      else None
    else None
  in
  *)
  match try_hash () with
  | Some _ as r -> r
  | None ->
    match try_uscore () with
    | Some _ as r -> r
    | None -> None

let base_name (v:var) : string =
  (* v_name is typically a string; it may contain suffixes like ".254"
     in prints. We strip off trailing ".<digits>" to stabilize. *)
  let s = v.v_name in
  match BatString.split s ~by:"." with
  | (hd,_) -> hd

let initial_tags_of_var (v:var) : tagset =
  let nm = base_name v in
  if is_random_name nm then TagSet.singleton ("R:" ^ nm)
  else
    match share_of_name nm with
    | Some (base, idx) -> TagSet.singleton (Printf.sprintf "S:%s#%d" base idx)
    | None -> TagSet.empty

(* --- Tag propagation --- *)

let tags_union_list (xs: tagset list) : tagset =
  List.fold_left TagSet.union TagSet.empty xs

(* Extract tags from an expression.
   This compiles now: it handles Pvar and constants.
   Extend it in the section below to support array/memory forms. *)
let rec tags_of_expr (env:(var, tagset) Hashtbl.t) (e:'ty gexpr) : tagset =
  match e with
  | Pvar gv ->
      if is_gkvar gv then
        let v = L.unloc gv.gv in
        BatHashtbl.find_default env v (initial_tags_of_var v)
      else TagSet.empty
  | _ -> TagSet.empty



let tags_of_exprs env (es:'ty gexpr list) : tagset =
  tags_union_list (List.map (tags_of_expr env) es)

let set_tags (env:(var, tagset) Hashtbl.t) (v:var) (t:tagset) : unit =
  (* always union with initial naming-based tags; this ensures
     that explicit share names stay tagged even if assigned from constants *)
  let t = TagSet.union t (initial_tags_of_var v) in
  Hashtbl.replace env v t

(* Assign tags to all variables in an lvalue list *)
let set_tags_lvs env (lvs:'ty glvals) (t:tagset) : unit =
  List.iter (function
    | Lvar lv ->
        let v = L.unloc lv in
        set_tags env v t
    | _ -> ()
  ) lvs

(* Tagging pass over a function body *)
let compute_func_tags (f:('info,'asm) func) : (var, tagset) Hashtbl.t =
  let env : (var, tagset) Hashtbl.t = Hashtbl.create 97 in

  (* seed: arguments/locals by name convention *)
  List.iter (fun a -> set_tags env a (initial_tags_of_var a)) f.f_args;
  List.iter (fun r -> set_tags env (L.unloc r) (initial_tags_of_var (L.unloc r))) f.f_ret;

  let step_instr (i:('info,'asm) instr) : unit =
    match i.i_desc with
    | Cassgn (Lvar x, _tg, _ty, e) ->
        let v = L.unloc x in
        let t = tags_of_expr env e in
        set_tags env v t

    | Copn (lvs, _tg, _op, es) ->
        let t = tags_of_exprs env es in
        set_tags_lvs env lvs t

    | Csyscall (lvs, _op, es) ->
        let t = tags_of_exprs env es in
        set_tags_lvs env lvs t

    | Ccall (lvs, _fn, es) ->
        (* conservative: returns depend on all args *)
        let t = tags_of_exprs env es in
        set_tags_lvs env lvs t

    | Cif (_e, _s1, _s2) -> ()
    | Cwhile (_, _s1, _e, _, _s2) -> ()
    | Cfor _ -> ()
    | _ -> 
     
        (* keep simple for now; if you want, we can do proper CFG-based join *)
        ()
  in

  iter_instr step_instr f.f_body;
  env

  (* Debug printer: prints each assigned var’s tag after the instruction *)
let debug_print_func
    (pp_instr:Format.formatter -> ('i,'a) instr -> unit)
    (f:('i,'a) func) : unit =
    print_string "Start debug_print_func:\n";
  let env : (var, tagset) Hashtbl.t = Hashtbl.create 97 in

  (* seed: arguments/returns by name convention *)
  List.iter (fun a -> set_tags env a (initial_tags_of_var a)) f.f_args;
  List.iter (fun r ->
      let v = L.unloc r in
      set_tags env v (initial_tags_of_var v)
    ) f.f_ret;

  let ppv = Printer.pp_var ~debug:true in

  let show_env_for_vars fmt (vs:Sv.t) =
    let xs = Sv.elements vs in
    Format.fprintf fmt "%a"
      (pp_list ", " (fun fmt v ->
           let t = BatHashtbl.find_default env v (initial_tags_of_var v) in
           Format.fprintf fmt "%a=%a" ppv v pp_tagset t
         )) xs
  in

  let step_instr (i:('i,'a) instr) : unit =
    (* 1) update env according to the instruction *)
    begin match i.i_desc with
    | Cassgn (Lvar x, _tg, _ty, e) ->
        let v = L.unloc x in
        let t = tags_of_expr env e in
        set_tags env v t

    | Copn (lvs, _tg, _op, es) ->
        let t = tags_of_exprs env es in
        set_tags_lvs env lvs t

    | Csyscall (lvs, _op, es) ->
        let t = tags_of_exprs env es in
        set_tags_lvs env lvs t

    | Ccall (lvs, _fn, es) ->
        (* conservative: returns depend on all args *)
        let t = tags_of_exprs env es in
        set_tags_lvs env lvs t

    | _ ->
        ()
    end;

    (* 2) print instruction + tags of defs after applying it *)
    let defs = assigns i.i_desc in
    Format.eprintf "@[<v>%a@;defs: %a@]@.@."
      pp_instr i
      show_env_for_vars defs
  in

  iter_instr step_instr f.f_body


open Utils
open Prog
open Glob_options
open Syntax
open CoreIdent
open Var0
open Conv 

open Annotations
let debug_annot = ref false

let dbg fmt =
  if !debug_annot then Format.eprintf (fmt ^^ "@.")
  else Format.ifprintf Format.err_formatter fmt

(* Debug helper. *)
let pp_simple_attr fmt = function
  | Aint i -> Format.fprintf fmt "Aint(%s)" (Z.to_string i)
  | Aid s -> Format.fprintf fmt "Aid(%s)" s
  | Astring s -> Format.fprintf fmt "Astring(%S)" s
  | Aws _ -> Format.fprintf fmt "Aws(..)"
  | Astruct fields ->
      Format.fprintf fmt "Astruct(%d fields)" (List.length fields)

let pp_attr_opt fmt = function
  | None -> Format.fprintf fmt "None"
  | Some av -> Format.fprintf fmt "Some(%a)" pp_simple_attr (Location.unloc av)

let pp_annot fmt (lab, vopt) =
  Format.fprintf fmt "%s=%a" (Location.unloc lab) pp_attr_opt vopt

let find_key (k : string) (a : annotation list) : attribute option list =
  a
  |> List.filter_map (fun (lab, vopt) ->
        if Location.unloc lab = k then Some vopt else None)

let rec find_field (field : string) (xs : annotation list) : attribute option option =
  match xs with
  | [] -> None
  | (lab, vopt) :: tl ->
      if Location.unloc lab = field then Some vopt
      else find_field field tl

let field_int (field : string) (xs : annotation list) : Z.t option =
  match find_field field xs with
  | Some (Some av) ->
      begin match Location.unloc av with
      | Aint i -> Some i
      | other ->
          dbg "field_int: field=%s present but not int (%a)" field pp_simple_attr other;
          None
      end
  | Some None ->
      dbg "field_int: field=%s present but has no value" field;
      None
  | None ->
      None

let field_string (field : string) (xs : annotation list) : string option =
  match find_field field xs with
  | Some (Some av) ->
      begin match Location.unloc av with
      | Astring s -> Some s
      | Aid id -> Some id
      | other ->
          dbg "field_string: field=%s present but not string/id (%a)" field pp_simple_attr other;
          None
      end
  | Some None ->
      dbg "field_string: field=%s present but has no value" field;
      None
  | None ->
      None

type shares_annot = { lo: int; hi: int; name: string; t: int }

let parse_shares (annots : annotation list) : shares_annot list =
  let raw = find_key "shares" annots in
  if !debug_annot then begin
    dbg "parse_shares: %d candidate(s)" (List.length raw);
    List.iteri (fun idx vopt ->
      dbg "  shares[%d] raw=%a" idx pp_attr_opt vopt
    ) raw
  end;
  raw
  |> List.filter_map (function
      | Some av ->
          begin match Location.unloc av with
          | Astruct fields ->
              let lo = field_int "lo" fields
              and hi = field_int "hi" fields
              and t  = field_int "t"  fields
              and name = field_string "name" fields in
              begin match lo, hi, t, name with
              | Some lo, Some hi, Some t, Some name ->
                  let s = { lo = Z.to_int lo; hi = Z.to_int hi; t = Z.to_int t; name } in
                  dbg "parse_shares: OK lo=%d hi=%d name=%s t=%d"
                    s.lo s.hi s.name s.t;
                  Some s
              | _ ->
                  dbg "parse_shares: BAD struct; fields were:";
                  List.iter (fun a -> dbg "    %a" pp_annot a) fields;
                  None
              end
          | other ->
              dbg "parse_shares: value not a struct (%a)" pp_simple_attr other;
              None
          end
      | None ->
          dbg "parse_shares: shares present with no value (expected struct)";
          None)

let parse_output (annots : annotation list) : shares_annot list =
  let raw = find_key "output" annots in
  if !debug_annot then begin
    dbg "parse_output: %d candidate(s)" (List.length raw);
    List.iteri (fun idx vopt ->
      dbg "  output[%d] raw=%a" idx pp_attr_opt vopt
    ) raw
  end;
  raw
  |> List.filter_map (function
      | Some av ->
          begin match Location.unloc av with
          | Astruct fields ->
              let lo = field_int "lo" fields
              and hi = field_int "hi" fields
              and t  = field_int "t"  fields
              and name = field_string "name" fields in
              begin match lo, hi, t, name with
              | Some lo, Some hi, Some t, Some name ->
                  let s = { lo = Z.to_int lo; hi = Z.to_int hi; t = Z.to_int t; name } in
                  dbg "parse_output: OK lo=%d hi=%d name=%s t=%d"
                    s.lo s.hi s.name s.t;
                  Some s
              | _ ->
                  dbg "parse_output: BAD struct; fields were:";
                  List.iter (fun a -> dbg "    %a" pp_annot a) fields;
                  None
              end
          | other ->
              dbg "parse_output: value not a struct (%a)" pp_simple_attr other;
              None
          end
      | None ->
          dbg "parse_output: output present with no value (expected struct)";
          None)

let parse_randomness_n (annots : annotation list) : int option =
  let raw = find_key "randomness" annots in
  if !debug_annot then begin
    dbg "parse_randomness: %d candidate(s)" (List.length raw);
    List.iteri (fun idx vopt ->
      dbg "  randomness[%d] raw=%a" idx pp_attr_opt vopt
    ) raw
  end;
  match raw with
  | [Some av] ->
      begin match Location.unloc av with
      | Astruct fields ->
          begin match field_int "n" fields with
          | Some n ->
              let n_i = Z.to_int n in
              dbg "parse_randomness: OK n=%d" n_i;
              Some n_i
          | None ->
              dbg "parse_randomness: BAD struct; fields were:";
              List.iter (fun a -> dbg "    %a" pp_annot a) fields;
              None
          end
      | other ->
          dbg "parse_randomness: value not a struct (%a)" pp_simple_attr other;
          None
      end
  | [] ->
      None
  | _ ->
      dbg "parse_randomness: expected exactly one randomness annotation, got %d" (List.length raw);
      None

let has_shares (annots : annotation list) : bool =
  match parse_shares annots with
  | [] -> false
  | _  -> true

 let has_output (annots : annotation list) : bool =
  match parse_output annots with
  | [] -> false
  | _  -> true 

let has_randomness (annots : annotation list) : bool =
  match parse_randomness_n annots with
  | Some _ -> true
  | None   -> false

type share_info = E.share_info  (* (sharename, share_index) *)
type skey = E.skey     (* (base var, element index) *)


module SKeyOrd = E.SKeyOrd

type randomness_info = E.randomness_info  (* (sharename, share_index) *)
type rkey = E.rkey     (* (base var, element index) *)


module RKeyOrd = E.RKeyOrd

module ShareMap = E.ShareMap

type smap = E.smap

module RandomnessMap = E.RandomnessMap

type rmap = E.rmap

let rmap_empty : rmap = RandomnessMap.empty


let concat_rmaps (maps : rmap list) : rmap =
  List.fold_left
    (fun acc m ->
       RandomnessMap.union (fun _ _ v -> Some v) acc m)
    RandomnessMap.empty
    maps

let concat_smaps (maps : smap list) : smap =
  List.fold_left
    (fun acc m ->
       ShareMap.union (fun _ _ v -> Some v) acc m)
    ShareMap.empty
    maps

let rec param_annot_to_rmap var idx rmap : rmap list=
  match idx with
  | -1 -> [rmap_empty]
  | 0 -> [(RandomnessMap.add (var, 0) 0 rmap)] 
  | i -> (RandomnessMap.add (var, i) i rmap) :: param_annot_to_rmap var (i-1) rmap
  
let  add_share_annot_to_smap var lo hi t name smap:smap = 
      let m = ref smap in 

      for idx = lo to hi  do 
        let key = (var, idx) in 
        let value:share_info = (name, idx mod (t+1)) in 
        m:= ShareMap.add key value !m
      done;

      let sm = !m in 
      sm

let rec add_share_annots_to_smap var share_annot smap = 
  match share_annot with
  | [] -> []
  | fst::rst -> (add_share_annot_to_smap var fst.lo fst.hi fst.t fst.name smap) :: add_share_annots_to_smap var rst smap

let rec iter_params_annots var annots rmap = 
  match annots with 
  |[] -> []
  |annot :: rest -> let i = match parse_randomness_n annots with | Some x -> (x-1) | _ -> -1  in print_int (i); print_endline("------------------------------------------------------------------------------------------------------------------------------------------"); concat_rmaps (param_annot_to_rmap var i rmap) :: iter_params_annots var rest rmap

let rec iter_params (params: Expr.var_i list) smap rmap  = 
  match params with 
  | [] -> ([],[])
  | param :: rst -> 
    let annots = param.v_var.vname.v_annot in
    if has_randomness annots then begin   
      let ridx = match parse_randomness_n annots with
      | Some x -> x
      | _ -> 0 in 
      let (a, b) = iter_params rst smap rmap in 
      (a, concat_rmaps(param_annot_to_rmap param (ridx -1) rmap) :: b) end

    else 
      if has_shares annots then begin
        let shares = parse_shares annots in 


        let (a, b) = iter_params rst smap rmap in (concat_smaps(add_share_annots_to_smap param shares smap) :: a,b)
      end else 
      
       let (a, b) = iter_params rst smap rmap in (a,b)
       
let rec iter_params_out (params: Expr.var_i list) omap  = 
  match params with 
  | [] -> ([])
  | param :: rst -> 
    let annots = param.v_var.vname.v_annot in
    if has_output annots then begin
        let output = parse_output annots in 
        let o = iter_params_out rst omap  in (concat_smaps(add_share_annots_to_smap param output omap) :: o)
      end else 
      
       let o = iter_params_out rst omap in o




let rec iter_pfuncs pf rmp smp omp = 
  match pf with 
  | [] -> ([], [], [], [])
  | ((fn: funname),fd) :: rest -> 
    let ((s :smap list) , (r: rmap list), (o: smap list), p) = iter_pfuncs rest rmp smp omp in 
    let (smaps, rmaps) = iter_params fd.Expr.f_params smp rmp in
    let omaps = iter_params_out fd.Expr.f_params omp in 

    let smaps = (concat_smaps smaps :: s) in 
    let rmaps = (concat_rmaps rmaps :: r) in
    let omaps = (concat_smaps omaps :: o) in

    let (fextra:E.stk_fun_extra) = fd.f_extra in 
    let fextra' = {fextra with sf_random_layout = concat_rmaps rmaps; sf_masking_layout = concat_smaps smaps; sf_output_layout = concat_smaps omaps} in 

    let fd' = {fd with f_extra = fextra'} in

    let p = (fn,fd') :: p in 

    (smaps, rmaps, omaps, p) 
  
 
let create_shareMap p = 

let rmp : rmap = RandomnessMap.empty in
let smp :smap = ShareMap.empty in
let omp = ShareMap.empty in 
let (smaps,rmaps, omaps, pf) = iter_pfuncs p rmp smp omp in
(pf,(concat_smaps smaps),(concat_rmaps rmaps), (concat_smaps omaps))


let rekey_smap (swapvar ) (m : smap) : smap =
  ShareMap.fold
    (fun (v, i) value acc ->
      let vt = L.unloc (vari_of_cvari v) in 
      let x: int gvar = swapvar vt in
      let vttt = Conv.cvar_of_var x in 
          let v' = {v with v_var = vttt} in 
      ShareMap.add (v', i) value acc
    )
    m
    ShareMap.empty

let rekey_rmap swapvar  (m : rmap) : rmap =
  RandomnessMap.fold
    (fun (v, i) value acc ->
      let vt = L.unloc (vari_of_cvari v) in 
      let x: int gvar = swapvar vt in
      let vttt = Conv.cvar_of_var x in 
      

      let v' = {v with v_var = vttt} in 
      RandomnessMap.add (v', i) value acc
    )
    m
    
    RandomnessMap.empty

let rec rekey_fds fds swapvar = 
  match fds with 
  | [] -> []
  | ((fn:funname), fd):: rst -> 
    let (fextra:E.stk_fun_extra) = fd.E.f_extra in 
   
    let smap = rekey_smap swapvar fextra.sf_masking_layout in  
    let rmap = rekey_rmap swapvar fextra.sf_random_layout in 
    let omap = rekey_smap swapvar fextra.sf_output_layout in
    let fextra' = {fextra with sf_masking_layout = smap; sf_random_layout = rmap; sf_output_layout = omap} in 
    let fd' = {fd with f_extra = fextra';} in
    
    (fn,fd') :: rekey_fds rst swapvar


let pp_map
    (pp_key : Format.formatter -> 'k -> unit)
    (pp_val : Format.formatter -> 'v -> unit)
    (iter : ('k -> 'v -> unit) -> 'm -> unit)
    (fmt : Format.formatter)
    (m : 'm)
  =
  Format.fprintf fmt "{@[<v>";
  iter
    (fun k v ->
       Format.fprintf fmt "@,%a -> %a" pp_key k pp_val v)
    m;
  Format.fprintf fmt "@]@,}"


let pp_var_i fmt (v : E.var_i) =
  Format.fprintf fmt "{var=%s;}"
    v.v_var.vname.v_name

let pp_share_info fmt (s, i) =
  Format.fprintf fmt "(%s, %d)" s i



let pp_key_vi_int fmt (v, i) =
  Format.fprintf fmt "(%a, %d)" pp_var_i v i
let pp_rmap fmt (m : rmap) =
  pp_map
    pp_key_vi_int
    Format.pp_print_int  
    RandomnessMap.iter
    fmt
    m

let pp_smap (m : smap) =
  Format.printf "{@[<v>";
  ShareMap.iter
    (fun k v ->
       Format.printf "@,%a -> %a" pp_key_vi_int k pp_share_info v)
    m;
  Format.printf "@]@,}@."


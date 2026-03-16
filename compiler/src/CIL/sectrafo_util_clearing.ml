open BinInt
open BinNums
open Bool
open Datatypes
open Ident
open List0
open Compiler_util
open Utils0
open Var0
open Global
open Expr
open Printer
open Sopn
open Seq
open Wsize
open Sectrafo_util
open Memory_model
open Stack_alloc_params


let rec positive_of_int (n : int) : positive =
  if n = 1 then Coq_xH
  else if (n land 1) = 0 then Coq_xO (positive_of_int (n lsr 1))
  else Coq_xI (positive_of_int (n lsr 1))

let coqZ_of_int (n : int) : coq_Z =
  if n = 0 then Z0
  else if n > 0 then Zpos (positive_of_int n)
  else Zneg (positive_of_int (-n))

let rec positive_to_int_opt (p : positive) : int option =
  match p with
  | Coq_xH -> Some 1
  | Coq_xO p' ->
      begin match positive_to_int_opt p' with
      | None -> None
      | Some x -> if x > max_int / 2 then None else Some (2 * x)
      end
  | Coq_xI p' ->
      begin match positive_to_int_opt p' with
      | None -> None
      | Some x -> if x > (max_int - 1) / 2 then None else Some (2 * x + 1)
      end

let coqz_to_int_opt (z : coq_Z) : int option =
  match z with
  | Z0 -> Some 0
  | Zpos p -> positive_to_int_opt p
  | Zneg p ->
      begin match positive_to_int_opt p with
      | None -> None
      | Some x ->
          let bound = if min_int = 0 then 0 else -min_int in
          if x > bound then None else Some (-x)
      end

let coqz_fits_int (z : coq_Z) : bool =
  match coqz_to_int_opt z with Some _ -> true | None -> false

let try_prim_arm (f : bool -> bool -> (string, 'asm_op) result) : 'asm_op option =
  let combos = [ (false,false); (true,false); (false,true); (true,true) ] in
  let rec go = function
    | [] -> None
    | (b1,b2)::tl ->
        match f b1 b2 with
        | Ok op -> Some op
        | Error _ -> go tl
  in
  go combos

let find_asm_op_exn (asmop : 'asm_op asmOp) (mnemonic : string) : 'asm_op =
  match List.find_opt (fun (s, _) -> String.equal s mnemonic) asmop.prim_string with
  | None ->
      failwith ("find_asm_op_exn: mnemonic not found: " ^ mnemonic)
  | Some (_s, pc) ->
      let res =
        match pc with
        | PrimARM f -> try_prim_arm f
        | PrimX86 (suffixes, mk) ->
            let rec go = function
              | [] -> None
              | suf :: tl ->
                  match mk suf with
                  | Some op -> Some op
                  | None -> go tl
            in
            go suffixes
      in
      match res with
      | Some op -> op
      | None ->
          failwith ("find_asm_op_exn: constructor could not build op for: " ^ mnemonic)


let ii_of_instr (i : 'asm_op instr) : IInfo.t =
  match i with
  | MkI (ii, _) -> ii

let const_of_pexpr (e : pexpr) : coq_Z option =
  match e with
  | Pconst z -> Some z
  | Papp1 (_, Pconst z) -> Some z
  | _ -> None

let mk_sp_plus_const ~(pd: 'pd) ~(ws:wsize) ~(sp_gv:gvar) ~(ofs:int) : pexpr =
  add pd (Pvar sp_gv) (cast_const pd (coqZ_of_int ofs))

let mk_sp_plus_const_coqZ ~(pd: 'pd) ~(ws:wsize) ~(sp_gv:gvar) ~(ofs:coq_Z) : pexpr =
  add pd (Pvar sp_gv) (cast_const pd ofs)


let register_scrub
  ~(asmop : 'asm_op asmOp)
  ~(pd : 'pd)
  ~(ii : IInfo.t)
  ~(reg : var_i)
  : 'asm_op instr =
  let mov_o0 = find_asm_op_exn asmop "MOV" in
  MkI (ii, Copn ([Lvar reg], AT_keep, Oasm mov_o0, [cast_const pd Z0]))

let clear_opR
  ~(asmop : 'asm_op asmOp)
  ~(pd : 'pd)
  ~(ii : IInfo.t)
  ~(dst : var_i)
  ~(sp_gv : gvar)
  ~(ofs : int)
  ~(al : aligned)
  ~(ws : wsize)
  : 'asm_op instr =
  let ldr_o0 = find_asm_op_exn asmop "LDR" in
  let addr = mk_sp_plus_const ~pd ~ws ~sp_gv ~ofs in
  let load_arg = Pload (al, ws, addr) in
  MkI (ii, Copn ([Lvar dst], AT_keep, Oasm ldr_o0, [load_arg]))

let clear_opW
  ~(asmop : 'asm_op asmOp)
  ~(pd : 'pd)
  ~(ii : IInfo.t)
  ~(dst : gvar)
  ~(sp_gv : gvar)
  ~(ofs : coq_Z)
  ~(al : aligned)
  ~(ws : wsize)
  : 'asm_op instr =
  let str_o0 = find_asm_op_exn asmop "STR" in
  let addr = mk_sp_plus_const_coqZ ~pd ~ws ~sp_gv ~ofs in
  let vi = sp_gv.gv.v_info in
  let out_mem = Lmem (al, ws, vi, addr) in
  MkI (ii, Copn ([out_mem], AT_keep, Oasm str_o0, [Pvar dst]))


let clear_mem_store_opt
  ~(asmop : 'asm_op asmOp)
  ~(pd : 'pd)
  ~(ii : IInfo.t)
  ~(dst : var_i)
  ~(base_gv : gvar)
  ~(ofs_bytes : coq_Z)
  ~(src_gv : gvar)
  ~(al : aligned)
  ~(ws : wsize)
  : 'asm_op instr=
  
    let str_o0 = find_asm_op_exn asmop "STR" in
    let vi = base_gv.gv.v_info in
    let addr = add pd (Pvar base_gv) (cast_const pd ofs_bytes) in
    let out_mem = Lmem (al, ws, vi, addr) in
    (MkI (ii, Copn ([out_mem], AT_keep, Oasm str_o0, [Pvar src_gv])))

let clear_opA_and_opB
  ~(asmop : 'asm_op asmOp)
  ~(ii : IInfo.t)
  ~(r0 : var_i)
  ~(r0_gv : gvar) : 'asm_op instr =
  let op =
    (* prefer ANDS if available; fall back to AND *)
    match List.find_opt (fun (s, _) -> String.equal s "ANDS") asmop.prim_string with
    | Some _ -> find_asm_op_exn asmop "ANDS"
    | None  -> find_asm_op_exn asmop "AND"
  in
  MkI (ii, Copn ([Lvar r0], AT_keep, Oasm op, [Pvar r0_gv; Pvar r0_gv]))




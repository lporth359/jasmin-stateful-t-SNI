(* pass2_scheduler.ml
   Pass 2: validated scheduling on Linear IR after linearization.

   Features:
   - Basic-block split of Linear.lcmd
   - Dependency DAG inside each block using def/use sets
   - Edge reasons: RAW/WAR/WAW/BARRIER
   - Optional human-readable DAG printing
   - Optional Graphviz DOT exporter per block

   Conservative correctness:
   - Memory-touching Lopn are barriers (no reordering across them)
   - Control-flow, calls, syscalls are barriers

   IMPORTANT:
   Adjust [module L = ...] to match your tree (you already did once).
*)

(* ======= ADAPT THIS LINE to your project layout ======= *)
module L = Linear      (* e.g. module L = CIL.Linear *)

module FE = Fexpr
module E  = Expr
module VS = Var0.SvExtra.Sv

(* ---------------- Debug controls ---------------- *)

let dump_dag_text = false
let dump_dag_all_blocks = false
let dump_dag_only_first_block = false
let dag_print_max_nodes = 200

let export_dot = false
let dot_dir = "dag_dots"

let eprintf_if b fmt =
  if b then Printf.eprintf fmt else Printf.ifprintf stderr fmt

(* ---------------- Coq-number conversions ---------------- *)

let rec int_of_nat = function
  | Datatypes.O -> 0
  | Datatypes.S n -> 1 + int_of_nat n

let rec int_of_positive = function
  | BinNums.Coq_xH -> 1
  | BinNums.Coq_xO p -> 2 * int_of_positive p
  | BinNums.Coq_xI p -> 2 * int_of_positive p + 1

(* ---------------- Small helpers ---------------- *)

let kind_of_lin (li : 'asm_op L.linstr) : string =
  match li.L.li_i with
  | L.Lopn _        -> "Lopn"
  | L.Lsyscall _    -> "Lsyscall"
  | L.Lcall _       -> "Lcall"
  | L.Lret          -> "Lret"
  | L.Lalign        -> "Lalign"
  | L.Llabel _      -> "Llabel"
  | L.Lgoto _       -> "Lgoto"
  | L.Ligoto _      -> "Ligoto"
  | L.LstoreLabel _ -> "LstoreLabel"
  | L.Lcond _       -> "Lcond"

let union_list (xs : VS.t list) : VS.t =
  List.fold_left VS.union VS.empty xs

let intersects (a : VS.t) (b : VS.t) : bool =
  not (VS.is_empty (VS.inter a b))

(* ---------------- Operation + operand shape printing ----------------
   We keep this intentionally lightweight and robust:
   - dst lexpr: reg | store[mem]
   - src rexpr: reg | load[mem]
   - op printed as op#TAG (TAG is sopn_tag as a small integer)
*)

let string_of_lexpr = function
  | FE.LLvar _ -> "reg"
  | FE.Store _ -> "store[mem]"

let string_of_rexpr = function
  | FE.Rexpr _ -> "reg"
  | FE.Load _  -> "load[mem]"

let string_of_lexprs (lxs : FE.lexpr list) =
  "[" ^ String.concat "," (List.map string_of_lexpr lxs) ^ "]"

let string_of_rexprs (rxs : FE.rexpr list) =
  "[" ^ String.concat "," (List.map string_of_rexpr rxs) ^ "]"

let string_of_sopn (op : 'a Sopn.sopn) : string =
  "Here!!!!"
  (*(* Stable numeric tag from extracted code; usually small. *)
  let tag = Sopn.sopn_tag op in
  "op#" ^ string_of_int (int_of_positive tag)*)

let describe_instr (li : 'a L.linstr) : string =
  match li.L.li_i with
  | L.Lopn (lxs, op, rxs) ->
      Printf.sprintf "Lopn %s <- %s %s"
        (string_of_lexprs lxs)
        (string_of_sopn op)
        (string_of_rexprs rxs)
  | _ ->
      kind_of_lin li

(* ---------------- Def/Use/Barrier summary per instruction ---------------- *)

type rw = {
  uses    : VS.t;   (* variables read *)
  defs    : VS.t;   (* variables written *)
  barrier : bool;   (* if true: forbid reordering across this instruction *)
}

let empty_rw = { uses = VS.empty; defs = VS.empty; barrier = false }

(* Expr.var_i is the working type in your build *)
let add_var_i (x : E.var_i) (s : VS.t) : VS.t =
  (* Matches extracted style used in Fexpr.free_vars: add Obj.magic x.v_var *)
  VS.add (Obj.magic x.E.v_var) s

(* FE.lexpr:
   - LLvar x defines variable x
   - Store defines memory (not modeled as variable def here) *)
let defs_lexpr (lx : FE.lexpr) : VS.t =
  match lx with
  | FE.LLvar x -> add_var_i x VS.empty
  | FE.Store _ -> VS.empty

(* Store reads its address expression *)
let uses_lexpr (lx : FE.lexpr) : VS.t =
  match lx with
  | FE.LLvar _ -> VS.empty
  | FE.Store (_, _, addr) -> FE.free_vars addr

let writes_mem_lexpr (lx : FE.lexpr) : bool =
  match lx with
  | FE.Store _ -> true
  | FE.LLvar _ -> false

let reads_mem_rexpr (rx : FE.rexpr) : bool =
  match rx with
  | FE.Load _ -> true
  | FE.Rexpr _ -> false

let uses_rexpr (rx : FE.rexpr) : VS.t =
  FE.free_vars_r rx

let rw_of_linstr (li : 'asm_op L.linstr) : rw =
  match li.L.li_i with
  | L.Lopn (lxs, _op, rxs) ->
      let defs = union_list (List.map defs_lexpr lxs) in
      let uses =
        VS.union
          (union_list (List.map uses_lexpr lxs))
          (union_list (List.map uses_rexpr rxs))
      in
      (* Conservative: any memory-touching op becomes a barrier. *)
      let touches_mem =
        List.exists writes_mem_lexpr lxs
        || List.exists reads_mem_rexpr rxs
      in
      { uses; defs; barrier = touches_mem }

  | L.Lcond (fe, _lbl) ->
      { uses = FE.free_vars fe; defs = VS.empty; barrier = true }

  | L.Ligoto rx ->
      { uses = uses_rexpr rx; defs = VS.empty; barrier = true }

  | L.Lsyscall _ | L.Lcall _ | L.Lret | L.Lalign | L.Llabel _ | L.Lgoto _
  | L.LstoreLabel _ ->
      { empty_rw with barrier = true }

(* ---------------- Basic block splitting ---------------- *)

let is_label (li : 'a L.linstr) : bool =
  match li.L.li_i with
  | L.Llabel _ -> true
  | _ -> false

let is_terminator (li : 'a L.linstr) : bool =
  match li.L.li_i with
  | L.Lcond _ | L.Lgoto _ | L.Ligoto _ | L.Lret | L.Lcall _ | L.Lsyscall _ -> true
  | _ -> false

let split_blocks (cmd : 'a L.lcmd) : 'a L.lcmd list =
  let blocks = ref [] in
  let cur = ref [] in
  let flush () =
    match List.rev !cur with
    | [] -> ()
    | b  -> blocks := b :: !blocks; cur := []
  in
  List.iter (fun li ->
    if is_label li then flush ();
    cur := li :: !cur;
    if is_terminator li then flush ();
  ) cmd;
  flush ();
  List.rev !blocks

let join_blocks (blocks : 'a L.lcmd list) : 'a L.lcmd =
  List.concat blocks

(* ---------------- Dependency DAG with reasons ---------------- *)

type reason =
  | RBarrier   (* conservative pinning: preserve relative order *)
  | RRAW       (* j reads something i defines *)
  | RWAR       (* j overwrites something i reads *)
  | RWAW       (* both write the same variable *)

let string_of_reason = function
  | RBarrier -> "BARRIER"
  | RRAW -> "RAW"
  | RWAR -> "WAR"
  | RWAW -> "WAW"

let reasons_ij (ri : rw) (rj : rw) : reason list =
  let rs = ref [] in
  if ri.barrier || rj.barrier then rs := RBarrier :: !rs;
  if intersects ri.defs rj.uses then rs := RRAW :: !rs;
  if intersects ri.uses rj.defs then rs := RWAR :: !rs;
  if intersects ri.defs rj.defs then rs := RWAW :: !rs;
  List.rev !rs

type edge = int * reason list

let sort_edges (es : edge list) : edge list =
  List.sort (fun (a,_) (b,_) -> compare a b) es

let string_of_reasons (rs : reason list) : string =
  "[" ^ String.concat "," (List.map string_of_reason rs) ^ "]"

(* Build DAG:
   - succ: for each i, list of (j, reasons)
   - indeg: indegree per node
   - edge_count: edges count
*)
let build_dag (rws : rw array) : (edge list array * int array * int) =
  let n = Array.length rws in
  let succ = Array.make n [] in
  let indeg = Array.make n 0 in
  let edges = ref 0 in
  for i = 0 to n - 1 do
    for j = i + 1 to n - 1 do
      let rs = reasons_ij rws.(i) rws.(j) in
      if rs <> [] then begin
        succ.(i) <- (j, rs) :: succ.(i);
        indeg.(j) <- indeg.(j) + 1;
        incr edges
      end
    done
  done;
  (succ, indeg, !edges)

let succ_ids (succ : edge list array) : int list array =
  Array.map (fun es -> List.map fst es) succ

(* ---------------- Human-readable DAG dump ---------------- *)

let print_dag_text ~(func_id:int) ~(blk_id:int) (arr : 'a L.linstr array) (rws : rw array) : unit =
  let n = Array.length arr in
  if n = 0 then ()
  else if n > dag_print_max_nodes then
    eprintf_if dump_dag_text
      "[pass2] DAG f%d b%d: n=%d (skipped: too large)\n" func_id blk_id n
  else begin
    let succ, indeg, edges = build_dag rws in
    eprintf_if dump_dag_text
      "[pass2] DAG f%d b%d: n=%d edges=%d\n" func_id blk_id n edges;
    eprintf_if dump_dag_text
      "  node: i kind barrier uses# defs# indeg :: desc\n";
    eprintf_if dump_dag_text
      "  edge: i -> j [REASONS]\n\n";

    for i = 0 to n - 1 do
      let kind = kind_of_lin arr.(i) in
      let bar = rws.(i).barrier in
      let u = int_of_nat (VS.cardinal rws.(i).uses) in
      let d = int_of_nat (VS.cardinal rws.(i).defs) in
      let desc = describe_instr arr.(i) in

      eprintf_if dump_dag_text
        "  %3d %-10s barrier=%-5b uses=%-3d defs=%-3d indeg=%-3d :: %s\n"
        i kind bar u d indeg.(i) desc;

      let es = sort_edges succ.(i) in
      List.iter (fun (j, rs) ->
        eprintf_if dump_dag_text "      %3d -> %3d %s\n" i j (string_of_reasons rs)
      ) es;

      if es <> [] then eprintf_if dump_dag_text "\n";
    done;

    eprintf_if dump_dag_text "\n";
  end

(* ---------------- Graphviz DOT exporter ---------------- *)

let escape_dot (s : string) : string =
  let b = Buffer.create (String.length s + 16) in
  String.iter (function
    | '"'  -> Buffer.add_string b "\\\""
    | '\\' -> Buffer.add_string b "\\\\"
    | '\n' -> Buffer.add_string b "\\n"
    | c    -> Buffer.add_char b c
  ) s;
  Buffer.contents b

let write_file (path : string) (contents : string) : unit =
  let oc = open_out path in
  output_string oc contents;
  close_out oc

let dot_of_block ~(graph_name:string) (arr : 'a L.linstr array) (rws : rw array) : string =
  let n = Array.length arr in
  let succ, indeg, _edges = build_dag rws in
  let b = Buffer.create 4096 in

  Buffer.add_string b ("digraph " ^ graph_name ^ " {\n");
  Buffer.add_string b "  rankdir=LR;\n";
  Buffer.add_string b "  node [shape=box, fontname=\"monospace\"];\n";
  Buffer.add_string b "  edge [fontname=\"monospace\"];\n\n";

  (* Nodes *)
  for i = 0 to n - 1 do
    let kind = kind_of_lin arr.(i) in
    let bar = rws.(i).barrier in
    let u = int_of_nat (VS.cardinal rws.(i).uses) in
    let d = int_of_nat (VS.cardinal rws.(i).defs) in
    let desc = describe_instr arr.(i) in
    let label =
      Printf.sprintf "%d: %s\\nbarrier=%b uses=%d defs=%d\\n%s"
        i kind bar u d desc
      |> escape_dot
    in
    if bar then
      Buffer.add_string b
        (Printf.sprintf "  n%d [label=\"%s\", style=\"filled\", fillcolor=\"#eeeeee\"];\n" i label)
    else
      Buffer.add_string b
        (Printf.sprintf "  n%d [label=\"%s\"];\n" i label);
  done;

  Buffer.add_string b "\n";

  (* Edges *)
  for i = 0 to n - 1 do
    let es = sort_edges succ.(i) in
    List.iter (fun (j, rs) ->
      let elab = escape_dot (String.concat "," (List.map string_of_reason rs)) in
      Buffer.add_string b
        (Printf.sprintf "  n%d -> n%d [label=\"%s\"];\n" i j elab)
    ) es
  done;

  (* indegrees as comments *)
  Buffer.add_string b "\n  // indegrees:\n";
  for i = 0 to n - 1 do
    Buffer.add_string b (Printf.sprintf "  // n%d indeg=%d\n" i indeg.(i))
  done;

  Buffer.add_string b "}\n";
  Buffer.contents b

let ensure_dir (d : string) : unit =
  try Unix.mkdir d 0o755 with _ -> ()

(* ---------------- Scheduling (topological sort) ---------------- *)

let score (rw : rw) : int =
  int_of_nat (VS.cardinal rw.uses) + int_of_nat (VS.cardinal rw.defs)

let topo_schedule (rws : rw array) : int list option =
  let n = Array.length rws in
  let succ_r, indeg, _edges = build_dag rws in
  let succ = succ_ids succ_r in

  let ready = ref [] in
  for i = 0 to n - 1 do
    if indeg.(i) = 0 then ready := i :: !ready
  done;

  let out = ref [] in

  let pick_best_ready () : int option =
    List.fold_left
      (fun best i ->
         match best with
         | None -> Some i
         | Some b -> if score rws.(i) > score rws.(b) then Some i else Some b)
      None !ready
  in

  let remove_ready x =
    ready := List.filter (fun y -> y <> x) !ready
  in

  while !ready <> [] do
    match pick_best_ready () with
    | None -> ()
    | Some v ->
        remove_ready v;
        out := v :: !out;
        List.iter (fun w ->
          indeg.(w) <- indeg.(w) - 1;
          if indeg.(w) = 0 then ready := w :: !ready
        ) succ.(v)
  done;

  if List.length !out = n then Some (List.rev !out) else None

let validate_topo (order : int list) (rws : rw array) : bool =
  let n = Array.length rws in
  if List.length order <> n then false
  else
    let pos = Array.make n (-1) in
    List.iteri (fun k v -> pos.(v) <- k) order;
    if Array.exists ((=) (-1)) pos then false
    else
      let succ_r, _indeg, _edges = build_dag rws in
      let ok = ref true in
      for u = 0 to n - 1 do
        List.iter (fun (v, _rs) ->
          if pos.(u) >= pos.(v) then ok := false
        ) succ_r.(u)
      done;
      !ok

(* ---------------- Schedule one block ---------------- *)

let schedule_block ~(func_id:int) ~(blk_id:int) (blk : 'a L.lcmd) : 'a L.lcmd =
  let arr = Array.of_list blk in
  let rws = Array.map rw_of_linstr arr in

  (* Text DAG dump *)
  if dump_dag_text then begin
    let should =
      dump_dag_all_blocks || (dump_dag_only_first_block && blk_id = 0)
    in
    if should then print_dag_text ~func_id ~blk_id arr rws
  end;

  (* DOT export *)
  if export_dot then begin
    ensure_dir dot_dir;
    let graph_name = Printf.sprintf "f%d_b%d" func_id blk_id in
    let dot = dot_of_block ~graph_name arr rws in
    let path = Printf.sprintf "%s/%s.dot" dot_dir graph_name in
    write_file path dot
  end;

  match topo_schedule rws with
  | None -> blk
  | Some order ->
      if validate_topo order rws then
        List.map (fun idx -> arr.(idx)) order
      else
        blk

(* ---------------- Schedule a function body ---------------- *)

let schedule_cmd ~(func_id:int) (cmd : 'a L.lcmd) : 'a L.lcmd =
  cmd
  |> split_blocks
  |> List.mapi (fun i blk -> schedule_block ~func_id ~blk_id:i blk)
  |> join_blocks

(* ---------------- Apply to whole linear program ---------------- *)

let schedule_lfundef ~(func_id:int) (fd : 'a L.lfundef) : 'a L.lfundef =
  { fd with L.lfd_body = schedule_cmd ~func_id fd.L.lfd_body }

let schedule_lprog (p : 'a L.lprog) : 'a L.lprog =
  { p with
    L.lp_funcs =
      List.mapi (fun i (fn, fd) -> (fn, schedule_lfundef ~func_id:i fd)) p.L.lp_funcs
  }

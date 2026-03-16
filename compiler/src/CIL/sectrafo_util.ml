open BinNums

exception CoqZ_to_int_overflow

(* Convert a Coq positive to int*)
let rec positive_to_int_opt (p : positive) : int option =
  match p with
  | Coq_xH -> Some 1
  | Coq_xO p' ->
      begin match positive_to_int_opt p' with
      | None -> None
      | Some x ->
          if x > max_int / 2 then None else Some (2 * x)
      end
  | Coq_xI p' ->
      begin match positive_to_int_opt p' with
      | None -> None
      | Some x ->
          if x > (max_int - 1) / 2 then None else Some (2 * x + 1)
      end

let coqz_to_int_opt (z : coq_Z) : int option =
  match z with
  | Z0 -> Some 0
  | Zpos p ->
      positive_to_int_opt p
  | Zneg p ->
      begin match positive_to_int_opt p with
      | None -> None
      | Some x ->
          let bound = if min_int = 0 then 0 else -min_int in
          if x > bound then None else Some (-x)
      end

let coqz_to_int_exn (z : coq_Z) : int =
  match coqz_to_int_opt z with
  | Some i -> i
  | None -> raise CoqZ_to_int_overflow

let coqz_fits_int (z : coq_Z) : bool =
  match coqz_to_int_opt z with
  | Some _ -> true
  | None -> false

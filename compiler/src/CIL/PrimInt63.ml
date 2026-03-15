open Datatypes

(** val add : Uint63.t -> Uint63.t -> Uint63.t **)

let add = Uint63.add

(** val eqb : Uint63.t -> Uint63.t -> bool **)

let eqb = Uint63.equal

(** val leb : Uint63.t -> Uint63.t -> bool **)

let leb = Uint63.le

(** val compares : Uint63.t -> Uint63.t -> comparison **)

let compares = (fun x y -> let c = Uint63.compares x y in if c = 0 then Eq else if c < 0 then Lt else Gt)

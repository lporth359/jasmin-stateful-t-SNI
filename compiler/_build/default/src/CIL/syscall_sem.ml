open BinInt
open BinNums
open Datatypes
open Low_memory
open Syscall
open Type
open Utils0
open Values
open Warray_
open Wsize

(** val exec_getrandom_u :
    'a1 syscall_sem -> 'a1 -> positive -> value list -> (error, 'a1 * value
    list) result **)

let exec_getrandom_u sc_sem scs len vs =
  match match vs with
        | [] -> type_error
        | v :: l -> (match l with
                     | [] -> to_arr len v
                     | _ :: _ -> type_error) with
  | Ok _ ->
    let sd = sc_sem scs (Zpos len) in
    (match WArray.fill len (snd sd) with
     | Ok x -> Ok ((fst sd), ((Varr (len, x)) :: []))
     | Error s -> Error s)
  | Error s -> Error s

(** val exec_syscall_u :
    'a1 syscall_sem -> coq_PointerData -> 'a1 syscall_state_t -> Memory.mem
    -> (Wsize.wsize * BinNums.positive) Syscall_t.syscall_t -> values ->
    (('a1 syscall_state_t * Memory.mem) * values) exec **)

let exec_syscall_u sc_sem _ scs m o vs =
  let Syscall_t.RandomBytes (ws, p) = o in
  let len = Z.to_pos (arr_size ws p) in
  (match exec_getrandom_u sc_sem scs len vs with
   | Ok x -> Ok (((fst x), m), (snd x))
   | Error s -> Error s)

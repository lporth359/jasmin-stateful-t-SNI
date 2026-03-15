open BinInt
open BinNums
open Datatypes
open Prelude
open Div
open Eqtype
open Expr
open Flag_combination
open Sem_type
open Ssralg
open Type
open Utils0
open Word0
open Word_ssrZ
open Wsize

val mk_sem_sop1 : ('a1 -> 'a2) -> 'a1 -> 'a2 exec

val sem_wiop1_typed : signedness -> wiop1 -> sem_t -> sem_t exec

val sem_sop1_typed : sop1 -> sem_t -> sem_t exec

val zlsl : coq_Z -> coq_Z -> coq_Z

val zasr : coq_Z -> coq_Z -> coq_Z

val sem_shift :
  (wsize -> GRing.ComRing.sort -> coq_Z -> GRing.ComRing.sort) -> wsize ->
  GRing.ComRing.sort -> GRing.ComRing.sort -> GRing.ComRing.sort

val sem_shr :
  wsize -> GRing.ComRing.sort -> GRing.ComRing.sort -> GRing.ComRing.sort

val sem_sar :
  wsize -> GRing.ComRing.sort -> GRing.ComRing.sort -> GRing.ComRing.sort

val sem_shl :
  wsize -> GRing.ComRing.sort -> GRing.ComRing.sort -> GRing.ComRing.sort

val sem_ror :
  wsize -> GRing.ComRing.sort -> GRing.ComRing.sort -> GRing.ComRing.sort

val sem_rol :
  wsize -> GRing.ComRing.sort -> GRing.ComRing.sort -> GRing.ComRing.sort

val sem_vadd :
  velem -> wsize -> GRing.ComRing.sort -> GRing.ComRing.sort ->
  GRing.ComRing.sort

val sem_vsub :
  velem -> wsize -> GRing.ComRing.sort -> GRing.ComRing.sort ->
  GRing.ComRing.sort

val sem_vmul :
  velem -> wsize -> GRing.ComRing.sort -> GRing.ComRing.sort ->
  GRing.ComRing.sort

val sem_vshr :
  velem -> wsize -> GRing.ComRing.sort -> GRing.ComRing.sort ->
  GRing.ComRing.sort

val sem_vsar :
  velem -> wsize -> GRing.ComRing.sort -> GRing.ComRing.sort ->
  GRing.ComRing.sort

val sem_vshl :
  velem -> wsize -> GRing.ComRing.sort -> GRing.ComRing.sort ->
  GRing.ComRing.sort

val mk_sem_divmod :
  signedness -> wsize -> (GRing.ComRing.sort -> GRing.ComRing.sort ->
  GRing.ComRing.sort) -> GRing.ComRing.sort -> GRing.ComRing.sort ->
  GRing.ComRing.sort exec

val mk_sem_sop2 : ('a1 -> 'a2 -> 'a3) -> 'a1 -> 'a2 -> 'a3 exec

val mk_sem_wiop2 :
  signedness -> wsize -> (coq_Z -> coq_Z -> coq_Z) -> GRing.ComRing.sort ->
  GRing.ComRing.sort -> GRing.ComRing.sort exec

val mk_sem_wishift :
  signedness -> wsize -> (coq_Z -> coq_Z -> coq_Z) -> GRing.ComRing.sort ->
  GRing.ComRing.sort -> GRing.ComRing.sort exec

val mk_sem_wicmp :
  signedness -> wsize -> (coq_Z -> coq_Z -> bool) -> GRing.ComRing.sort ->
  GRing.ComRing.sort -> bool exec

val sem_wiop2_typed :
  signedness -> wsize -> wiop2 -> sem_t -> sem_t -> sem_t exec

val sem_sop2_typed : sop2 -> sem_t -> sem_t -> sem_t exec

val sem_combine_flags :
  coq_FlagCombinationParams -> combine_flags -> bool -> bool -> bool -> bool
  -> bool

val sem_opN_typed : coq_FlagCombinationParams -> opN -> sem_t exec sem_prod

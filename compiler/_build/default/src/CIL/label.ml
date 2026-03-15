open BinNums
open Var0

type label_kind =
| InternalLabel
| ExternalLabel

type label = positive

type remote_label = funname * label

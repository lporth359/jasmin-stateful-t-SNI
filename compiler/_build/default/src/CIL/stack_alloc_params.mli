open BinNums
open Expr
open Wsize

type mov_kind =
| MK_LEA
| MK_MOV

type 'asm_op stack_alloc_params = { sap_mov_ofs : (lval -> assgn_tag ->
                                                  mov_kind -> pexpr -> pexpr
                                                  -> 'asm_op instr_r option);
                                    sap_immediate : (var_i -> coq_Z ->
                                                    'asm_op instr_r);
                                    sap_swap : (assgn_tag -> var_i -> var_i
                                               -> var_i -> var_i -> 'asm_op
                                               instr_r) }

val add : coq_PointerData -> pexpr -> pexpr -> pexpr

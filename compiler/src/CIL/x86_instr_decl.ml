open BinInt
open BinNums
open Bool
open Datatypes
open List0
open Nat0
open Prelude
open Zpower
open Arch_decl
open EqbOK
open Eqb_core_defs
open Eqtype
open Memory_model
open Sem_type
open Seq
open Sha256
open Sopn
open Ssralg
open Ssrbool
open Ssrnat
open Tuple
open Type
open Utils0
open Waes
open Word0
open Word
open Word_ssrZ
open Wsize
open X86_decl

type __ = Obj.t
let __ = let rec f _ = Obj.repr f in Obj.repr f

type x86_op =
| MOV of wsize
| MOVSX of wsize * wsize
| MOVZX of wsize * wsize
| CMOVcc of wsize
| XCHG of wsize
| ADD of wsize
| SUB of wsize
| MUL of wsize
| IMUL of wsize
| IMULr of wsize
| IMULri of wsize
| DIV of wsize
| IDIV of wsize
| CQO of wsize
| ADC of wsize
| SBB of wsize
| NEG of wsize
| INC of wsize
| DEC of wsize
| LZCNT of wsize
| TZCNT of wsize
| BSR of wsize
| SETcc
| BT of wsize
| CLC
| STC
| LEA of wsize
| TEST of wsize
| CMP of wsize
| AND of wsize
| ANDN of wsize
| OR of wsize
| XOR of wsize
| NOT of wsize
| ROR of wsize
| ROL of wsize
| RCR of wsize
| RCL of wsize
| SHL of wsize
| SHR of wsize
| SAL of wsize
| SAR of wsize
| SHLD of wsize
| SHRD of wsize
| RORX of wsize
| SARX of wsize
| SHRX of wsize
| SHLX of wsize
| MULX_lo_hi of wsize
| ADCX of wsize
| ADOX of wsize
| BSWAP of wsize
| POPCNT of wsize
| BTR of wsize
| BTS of wsize
| PEXT of wsize
| PDEP of wsize
| MOVX of wsize
| POR
| PADD of velem * wsize
| MOVD of wsize
| MOVV of wsize
| VMOV of wsize
| VMOVDQA of wsize
| VMOVDQU of wsize
| VPMOVSX of velem * wsize * velem * wsize
| VPMOVZX of velem * wsize * velem * wsize
| VPAND of wsize
| VPANDN of wsize
| VPOR of wsize
| VPXOR of wsize
| VPADD of velem * wsize
| VPSUB of velem * wsize
| VPAVG of velem * wsize
| VPMULL of velem * wsize
| VPMULH of wsize
| VPMULHU of wsize
| VPMULHRS of wsize
| VPMUL of wsize
| VPMULU of wsize
| VPEXTR of wsize
| VPINSR of velem
| VPSLL of velem * wsize
| VPSRL of velem * wsize
| VPSRA of velem * wsize
| VPSLLV of velem * wsize
| VPSRLV of velem * wsize
| VPSLLDQ of wsize
| VPSRLDQ of wsize
| VPSHUFB of wsize
| VPSHUFD of wsize
| VPSHUFHW of wsize
| VPSHUFLW of wsize
| VPBLEND of velem * wsize
| BLENDV of velem * wsize
| VPACKUS of velem * wsize
| VPACKSS of velem * wsize
| VSHUFPS of wsize
| VPBROADCAST of velem * wsize
| VMOVSHDUP of wsize
| VMOVSLDUP of wsize
| VPALIGNR of wsize
| VBROADCASTI128
| VPUNPCKH of velem * wsize
| VPUNPCKL of velem * wsize
| VEXTRACTI128
| VINSERTI128
| VPERM2I128
| VPERMD
| VPERMQ
| MOVEMASK of velem * wsize
| VPCMPEQ of velem * wsize
| VPCMPGT of velem * wsize
| VPSIGN of velem * wsize
| VPMADDUBSW of wsize
| VPMADDWD of wsize
| VMOVLPD
| VMOVHPD
| VPMINU of velem * wsize
| VPMINS of velem * wsize
| VPMAXU of velem * wsize
| VPMAXS of velem * wsize
| VPABS of velem * wsize
| VPTEST of wsize
| CLFLUSH
| PREFETCHT0
| PREFETCHT1
| PREFETCHT2
| PREFETCHNTA
| LFENCE
| MFENCE
| SFENCE
| RDTSC of wsize
| RDTSCP of wsize
| AESDEC
| VAESDEC of wsize
| AESDECLAST
| VAESDECLAST of wsize
| AESENC
| VAESENC of wsize
| AESENCLAST
| VAESENCLAST of wsize
| AESIMC
| VAESIMC
| AESKEYGENASSIST
| VAESKEYGENASSIST
| PCLMULQDQ
| VPCLMULQDQ of wsize
| SHA256RNDS2
| SHA256MSG1
| SHA256MSG2

(** val x86_op_rect :
    (wsize -> 'a1) -> (wsize -> wsize -> 'a1) -> (wsize -> wsize -> 'a1) ->
    (wsize -> 'a1) -> (wsize -> 'a1) -> (wsize -> 'a1) -> (wsize -> 'a1) ->
    (wsize -> 'a1) -> (wsize -> 'a1) -> (wsize -> 'a1) -> (wsize -> 'a1) ->
    (wsize -> 'a1) -> (wsize -> 'a1) -> (wsize -> 'a1) -> (wsize -> 'a1) ->
    (wsize -> 'a1) -> (wsize -> 'a1) -> (wsize -> 'a1) -> (wsize -> 'a1) ->
    (wsize -> 'a1) -> (wsize -> 'a1) -> (wsize -> 'a1) -> 'a1 -> (wsize ->
    'a1) -> 'a1 -> 'a1 -> (wsize -> 'a1) -> (wsize -> 'a1) -> (wsize -> 'a1)
    -> (wsize -> 'a1) -> (wsize -> 'a1) -> (wsize -> 'a1) -> (wsize -> 'a1)
    -> (wsize -> 'a1) -> (wsize -> 'a1) -> (wsize -> 'a1) -> (wsize -> 'a1)
    -> (wsize -> 'a1) -> (wsize -> 'a1) -> (wsize -> 'a1) -> (wsize -> 'a1)
    -> (wsize -> 'a1) -> (wsize -> 'a1) -> (wsize -> 'a1) -> (wsize -> 'a1)
    -> (wsize -> 'a1) -> (wsize -> 'a1) -> (wsize -> 'a1) -> (wsize -> 'a1)
    -> (wsize -> 'a1) -> (wsize -> 'a1) -> (wsize -> 'a1) -> (wsize -> 'a1)
    -> (wsize -> 'a1) -> (wsize -> 'a1) -> (wsize -> 'a1) -> (wsize -> 'a1)
    -> (wsize -> 'a1) -> 'a1 -> (velem -> wsize -> 'a1) -> (wsize -> 'a1) ->
    (wsize -> 'a1) -> (wsize -> 'a1) -> (wsize -> 'a1) -> (wsize -> 'a1) ->
    (velem -> wsize -> velem -> wsize -> 'a1) -> (velem -> wsize -> velem ->
    wsize -> 'a1) -> (wsize -> 'a1) -> (wsize -> 'a1) -> (wsize -> 'a1) ->
    (wsize -> 'a1) -> (velem -> wsize -> 'a1) -> (velem -> wsize -> 'a1) ->
    (velem -> wsize -> 'a1) -> (velem -> wsize -> 'a1) -> (wsize -> 'a1) ->
    (wsize -> 'a1) -> (wsize -> 'a1) -> (wsize -> 'a1) -> (wsize -> 'a1) ->
    (wsize -> 'a1) -> (velem -> 'a1) -> (velem -> wsize -> 'a1) -> (velem ->
    wsize -> 'a1) -> (velem -> wsize -> 'a1) -> (velem -> wsize -> 'a1) ->
    (velem -> wsize -> 'a1) -> (wsize -> 'a1) -> (wsize -> 'a1) -> (wsize ->
    'a1) -> (wsize -> 'a1) -> (wsize -> 'a1) -> (wsize -> 'a1) -> (velem ->
    wsize -> 'a1) -> (velem -> wsize -> 'a1) -> (velem -> wsize -> 'a1) ->
    (velem -> wsize -> 'a1) -> (wsize -> 'a1) -> (velem -> wsize -> 'a1) ->
    (wsize -> 'a1) -> (wsize -> 'a1) -> (wsize -> 'a1) -> 'a1 -> (velem ->
    wsize -> 'a1) -> (velem -> wsize -> 'a1) -> 'a1 -> 'a1 -> 'a1 -> 'a1 ->
    'a1 -> (velem -> wsize -> 'a1) -> (velem -> wsize -> 'a1) -> (velem ->
    wsize -> 'a1) -> (velem -> wsize -> 'a1) -> (wsize -> 'a1) -> (wsize ->
    'a1) -> 'a1 -> 'a1 -> (velem -> wsize -> 'a1) -> (velem -> wsize -> 'a1)
    -> (velem -> wsize -> 'a1) -> (velem -> wsize -> 'a1) -> (velem -> wsize
    -> 'a1) -> (wsize -> 'a1) -> 'a1 -> 'a1 -> 'a1 -> 'a1 -> 'a1 -> 'a1 ->
    'a1 -> 'a1 -> (wsize -> 'a1) -> (wsize -> 'a1) -> 'a1 -> (wsize -> 'a1)
    -> 'a1 -> (wsize -> 'a1) -> 'a1 -> (wsize -> 'a1) -> 'a1 -> (wsize ->
    'a1) -> 'a1 -> 'a1 -> 'a1 -> 'a1 -> 'a1 -> (wsize -> 'a1) -> 'a1 -> 'a1
    -> 'a1 -> x86_op -> 'a1 **)

let x86_op_rect f f0 f1 f2 f3 f4 f5 f6 f7 f8 f9 f10 f11 f12 f13 f14 f15 f16 f17 f18 f19 f20 f21 f22 f23 f24 f25 f26 f27 f28 f29 f30 f31 f32 f33 f34 f35 f36 f37 f38 f39 f40 f41 f42 f43 f44 f45 f46 f47 f48 f49 f50 f51 f52 f53 f54 f55 f56 f57 f58 f59 f60 f61 f62 f63 f64 f65 f66 f67 f68 f69 f70 f71 f72 f73 f74 f75 f76 f77 f78 f79 f80 f81 f82 f83 f84 f85 f86 f87 f88 f89 f90 f91 f92 f93 f94 f95 f96 f97 f98 f99 f100 f101 f102 f103 f104 f105 f106 f107 f108 f109 f110 f111 f112 f113 f114 f115 f116 f117 f118 f119 f120 f121 f122 f123 f124 f125 f126 f127 f128 f129 f130 f131 f132 f133 f134 f135 f136 f137 f138 f139 f140 f141 f142 f143 f144 f145 f146 f147 f148 f149 = function
| MOV w -> f w
| MOVSX (w, w0) -> f0 w w0
| MOVZX (w, w0) -> f1 w w0
| CMOVcc w -> f2 w
| XCHG w -> f3 w
| ADD w -> f4 w
| SUB w -> f5 w
| MUL w -> f6 w
| IMUL w -> f7 w
| IMULr w -> f8 w
| IMULri w -> f9 w
| DIV w -> f10 w
| IDIV w -> f11 w
| CQO w -> f12 w
| ADC w -> f13 w
| SBB w -> f14 w
| NEG w -> f15 w
| INC w -> f16 w
| DEC w -> f17 w
| LZCNT w -> f18 w
| TZCNT w -> f19 w
| BSR w -> f20 w
| SETcc -> f21
| BT w -> f22 w
| CLC -> f23
| STC -> f24
| LEA w -> f25 w
| TEST w -> f26 w
| CMP w -> f27 w
| AND w -> f28 w
| ANDN w -> f29 w
| OR w -> f30 w
| XOR w -> f31 w
| NOT w -> f32 w
| ROR w -> f33 w
| ROL w -> f34 w
| RCR w -> f35 w
| RCL w -> f36 w
| SHL w -> f37 w
| SHR w -> f38 w
| SAL w -> f39 w
| SAR w -> f40 w
| SHLD w -> f41 w
| SHRD w -> f42 w
| RORX w -> f43 w
| SARX w -> f44 w
| SHRX w -> f45 w
| SHLX w -> f46 w
| MULX_lo_hi w -> f47 w
| ADCX w -> f48 w
| ADOX w -> f49 w
| BSWAP w -> f50 w
| POPCNT w -> f51 w
| BTR w -> f52 w
| BTS w -> f53 w
| PEXT w -> f54 w
| PDEP w -> f55 w
| MOVX w -> f56 w
| POR -> f57
| PADD (v, w) -> f58 v w
| MOVD w -> f59 w
| MOVV w -> f60 w
| VMOV w -> f61 w
| VMOVDQA w -> f62 w
| VMOVDQU h -> f63 h
| VPMOVSX (v, w, v0, w0) -> f64 v w v0 w0
| VPMOVZX (v, w, v0, w0) -> f65 v w v0 w0
| VPAND h -> f66 h
| VPANDN h -> f67 h
| VPOR h -> f68 h
| VPXOR h -> f69 h
| VPADD (h, h0) -> f70 h h0
| VPSUB (h, h0) -> f71 h h0
| VPAVG (v, w) -> f72 v w
| VPMULL (h, h0) -> f73 h h0
| VPMULH w -> f74 w
| VPMULHU w -> f75 w
| VPMULHRS w -> f76 w
| VPMUL h -> f77 h
| VPMULU h -> f78 h
| VPEXTR w -> f79 w
| VPINSR v -> f80 v
| VPSLL (h, h0) -> f81 h h0
| VPSRL (h, h0) -> f82 h h0
| VPSRA (h, h0) -> f83 h h0
| VPSLLV (h, h0) -> f84 h h0
| VPSRLV (h, h0) -> f85 h h0
| VPSLLDQ h -> f86 h
| VPSRLDQ h -> f87 h
| VPSHUFB h -> f88 h
| VPSHUFD h -> f89 h
| VPSHUFHW h -> f90 h
| VPSHUFLW h -> f91 h
| VPBLEND (h, h0) -> f92 h h0
| BLENDV (v, w) -> f93 v w
| VPACKUS (h, h0) -> f94 h h0
| VPACKSS (h, h0) -> f95 h h0
| VSHUFPS h -> f96 h
| VPBROADCAST (v, w) -> f97 v w
| VMOVSHDUP w -> f98 w
| VMOVSLDUP w -> f99 w
| VPALIGNR h -> f100 h
| VBROADCASTI128 -> f101
| VPUNPCKH (h, h0) -> f102 h h0
| VPUNPCKL (h, h0) -> f103 h h0
| VEXTRACTI128 -> f104
| VINSERTI128 -> f105
| VPERM2I128 -> f106
| VPERMD -> f107
| VPERMQ -> f108
| MOVEMASK (v, w) -> f109 v w
| VPCMPEQ (v, w) -> f110 v w
| VPCMPGT (v, w) -> f111 v w
| VPSIGN (v, w) -> f112 v w
| VPMADDUBSW w -> f113 w
| VPMADDWD w -> f114 w
| VMOVLPD -> f115
| VMOVHPD -> f116
| VPMINU (v, w) -> f117 v w
| VPMINS (v, w) -> f118 v w
| VPMAXU (v, w) -> f119 v w
| VPMAXS (v, w) -> f120 v w
| VPABS (v, w) -> f121 v w
| VPTEST h -> f122 h
| CLFLUSH -> f123
| PREFETCHT0 -> f124
| PREFETCHT1 -> f125
| PREFETCHT2 -> f126
| PREFETCHNTA -> f127
| LFENCE -> f128
| MFENCE -> f129
| SFENCE -> f130
| RDTSC w -> f131 w
| RDTSCP w -> f132 w
| AESDEC -> f133
| VAESDEC w -> f134 w
| AESDECLAST -> f135
| VAESDECLAST w -> f136 w
| AESENC -> f137
| VAESENC w -> f138 w
| AESENCLAST -> f139
| VAESENCLAST w -> f140 w
| AESIMC -> f141
| VAESIMC -> f142
| AESKEYGENASSIST -> f143
| VAESKEYGENASSIST -> f144
| PCLMULQDQ -> f145
| VPCLMULQDQ w -> f146 w
| SHA256RNDS2 -> f147
| SHA256MSG1 -> f148
| SHA256MSG2 -> f149

(** val x86_op_rec :
    (wsize -> 'a1) -> (wsize -> wsize -> 'a1) -> (wsize -> wsize -> 'a1) ->
    (wsize -> 'a1) -> (wsize -> 'a1) -> (wsize -> 'a1) -> (wsize -> 'a1) ->
    (wsize -> 'a1) -> (wsize -> 'a1) -> (wsize -> 'a1) -> (wsize -> 'a1) ->
    (wsize -> 'a1) -> (wsize -> 'a1) -> (wsize -> 'a1) -> (wsize -> 'a1) ->
    (wsize -> 'a1) -> (wsize -> 'a1) -> (wsize -> 'a1) -> (wsize -> 'a1) ->
    (wsize -> 'a1) -> (wsize -> 'a1) -> (wsize -> 'a1) -> 'a1 -> (wsize ->
    'a1) -> 'a1 -> 'a1 -> (wsize -> 'a1) -> (wsize -> 'a1) -> (wsize -> 'a1)
    -> (wsize -> 'a1) -> (wsize -> 'a1) -> (wsize -> 'a1) -> (wsize -> 'a1)
    -> (wsize -> 'a1) -> (wsize -> 'a1) -> (wsize -> 'a1) -> (wsize -> 'a1)
    -> (wsize -> 'a1) -> (wsize -> 'a1) -> (wsize -> 'a1) -> (wsize -> 'a1)
    -> (wsize -> 'a1) -> (wsize -> 'a1) -> (wsize -> 'a1) -> (wsize -> 'a1)
    -> (wsize -> 'a1) -> (wsize -> 'a1) -> (wsize -> 'a1) -> (wsize -> 'a1)
    -> (wsize -> 'a1) -> (wsize -> 'a1) -> (wsize -> 'a1) -> (wsize -> 'a1)
    -> (wsize -> 'a1) -> (wsize -> 'a1) -> (wsize -> 'a1) -> (wsize -> 'a1)
    -> (wsize -> 'a1) -> 'a1 -> (velem -> wsize -> 'a1) -> (wsize -> 'a1) ->
    (wsize -> 'a1) -> (wsize -> 'a1) -> (wsize -> 'a1) -> (wsize -> 'a1) ->
    (velem -> wsize -> velem -> wsize -> 'a1) -> (velem -> wsize -> velem ->
    wsize -> 'a1) -> (wsize -> 'a1) -> (wsize -> 'a1) -> (wsize -> 'a1) ->
    (wsize -> 'a1) -> (velem -> wsize -> 'a1) -> (velem -> wsize -> 'a1) ->
    (velem -> wsize -> 'a1) -> (velem -> wsize -> 'a1) -> (wsize -> 'a1) ->
    (wsize -> 'a1) -> (wsize -> 'a1) -> (wsize -> 'a1) -> (wsize -> 'a1) ->
    (wsize -> 'a1) -> (velem -> 'a1) -> (velem -> wsize -> 'a1) -> (velem ->
    wsize -> 'a1) -> (velem -> wsize -> 'a1) -> (velem -> wsize -> 'a1) ->
    (velem -> wsize -> 'a1) -> (wsize -> 'a1) -> (wsize -> 'a1) -> (wsize ->
    'a1) -> (wsize -> 'a1) -> (wsize -> 'a1) -> (wsize -> 'a1) -> (velem ->
    wsize -> 'a1) -> (velem -> wsize -> 'a1) -> (velem -> wsize -> 'a1) ->
    (velem -> wsize -> 'a1) -> (wsize -> 'a1) -> (velem -> wsize -> 'a1) ->
    (wsize -> 'a1) -> (wsize -> 'a1) -> (wsize -> 'a1) -> 'a1 -> (velem ->
    wsize -> 'a1) -> (velem -> wsize -> 'a1) -> 'a1 -> 'a1 -> 'a1 -> 'a1 ->
    'a1 -> (velem -> wsize -> 'a1) -> (velem -> wsize -> 'a1) -> (velem ->
    wsize -> 'a1) -> (velem -> wsize -> 'a1) -> (wsize -> 'a1) -> (wsize ->
    'a1) -> 'a1 -> 'a1 -> (velem -> wsize -> 'a1) -> (velem -> wsize -> 'a1)
    -> (velem -> wsize -> 'a1) -> (velem -> wsize -> 'a1) -> (velem -> wsize
    -> 'a1) -> (wsize -> 'a1) -> 'a1 -> 'a1 -> 'a1 -> 'a1 -> 'a1 -> 'a1 ->
    'a1 -> 'a1 -> (wsize -> 'a1) -> (wsize -> 'a1) -> 'a1 -> (wsize -> 'a1)
    -> 'a1 -> (wsize -> 'a1) -> 'a1 -> (wsize -> 'a1) -> 'a1 -> (wsize ->
    'a1) -> 'a1 -> 'a1 -> 'a1 -> 'a1 -> 'a1 -> (wsize -> 'a1) -> 'a1 -> 'a1
    -> 'a1 -> x86_op -> 'a1 **)

let x86_op_rec f f0 f1 f2 f3 f4 f5 f6 f7 f8 f9 f10 f11 f12 f13 f14 f15 f16 f17 f18 f19 f20 f21 f22 f23 f24 f25 f26 f27 f28 f29 f30 f31 f32 f33 f34 f35 f36 f37 f38 f39 f40 f41 f42 f43 f44 f45 f46 f47 f48 f49 f50 f51 f52 f53 f54 f55 f56 f57 f58 f59 f60 f61 f62 f63 f64 f65 f66 f67 f68 f69 f70 f71 f72 f73 f74 f75 f76 f77 f78 f79 f80 f81 f82 f83 f84 f85 f86 f87 f88 f89 f90 f91 f92 f93 f94 f95 f96 f97 f98 f99 f100 f101 f102 f103 f104 f105 f106 f107 f108 f109 f110 f111 f112 f113 f114 f115 f116 f117 f118 f119 f120 f121 f122 f123 f124 f125 f126 f127 f128 f129 f130 f131 f132 f133 f134 f135 f136 f137 f138 f139 f140 f141 f142 f143 f144 f145 f146 f147 f148 f149 = function
| MOV w -> f w
| MOVSX (w, w0) -> f0 w w0
| MOVZX (w, w0) -> f1 w w0
| CMOVcc w -> f2 w
| XCHG w -> f3 w
| ADD w -> f4 w
| SUB w -> f5 w
| MUL w -> f6 w
| IMUL w -> f7 w
| IMULr w -> f8 w
| IMULri w -> f9 w
| DIV w -> f10 w
| IDIV w -> f11 w
| CQO w -> f12 w
| ADC w -> f13 w
| SBB w -> f14 w
| NEG w -> f15 w
| INC w -> f16 w
| DEC w -> f17 w
| LZCNT w -> f18 w
| TZCNT w -> f19 w
| BSR w -> f20 w
| SETcc -> f21
| BT w -> f22 w
| CLC -> f23
| STC -> f24
| LEA w -> f25 w
| TEST w -> f26 w
| CMP w -> f27 w
| AND w -> f28 w
| ANDN w -> f29 w
| OR w -> f30 w
| XOR w -> f31 w
| NOT w -> f32 w
| ROR w -> f33 w
| ROL w -> f34 w
| RCR w -> f35 w
| RCL w -> f36 w
| SHL w -> f37 w
| SHR w -> f38 w
| SAL w -> f39 w
| SAR w -> f40 w
| SHLD w -> f41 w
| SHRD w -> f42 w
| RORX w -> f43 w
| SARX w -> f44 w
| SHRX w -> f45 w
| SHLX w -> f46 w
| MULX_lo_hi w -> f47 w
| ADCX w -> f48 w
| ADOX w -> f49 w
| BSWAP w -> f50 w
| POPCNT w -> f51 w
| BTR w -> f52 w
| BTS w -> f53 w
| PEXT w -> f54 w
| PDEP w -> f55 w
| MOVX w -> f56 w
| POR -> f57
| PADD (v, w) -> f58 v w
| MOVD w -> f59 w
| MOVV w -> f60 w
| VMOV w -> f61 w
| VMOVDQA w -> f62 w
| VMOVDQU h -> f63 h
| VPMOVSX (v, w, v0, w0) -> f64 v w v0 w0
| VPMOVZX (v, w, v0, w0) -> f65 v w v0 w0
| VPAND h -> f66 h
| VPANDN h -> f67 h
| VPOR h -> f68 h
| VPXOR h -> f69 h
| VPADD (h, h0) -> f70 h h0
| VPSUB (h, h0) -> f71 h h0
| VPAVG (v, w) -> f72 v w
| VPMULL (h, h0) -> f73 h h0
| VPMULH w -> f74 w
| VPMULHU w -> f75 w
| VPMULHRS w -> f76 w
| VPMUL h -> f77 h
| VPMULU h -> f78 h
| VPEXTR w -> f79 w
| VPINSR v -> f80 v
| VPSLL (h, h0) -> f81 h h0
| VPSRL (h, h0) -> f82 h h0
| VPSRA (h, h0) -> f83 h h0
| VPSLLV (h, h0) -> f84 h h0
| VPSRLV (h, h0) -> f85 h h0
| VPSLLDQ h -> f86 h
| VPSRLDQ h -> f87 h
| VPSHUFB h -> f88 h
| VPSHUFD h -> f89 h
| VPSHUFHW h -> f90 h
| VPSHUFLW h -> f91 h
| VPBLEND (h, h0) -> f92 h h0
| BLENDV (v, w) -> f93 v w
| VPACKUS (h, h0) -> f94 h h0
| VPACKSS (h, h0) -> f95 h h0
| VSHUFPS h -> f96 h
| VPBROADCAST (v, w) -> f97 v w
| VMOVSHDUP w -> f98 w
| VMOVSLDUP w -> f99 w
| VPALIGNR h -> f100 h
| VBROADCASTI128 -> f101
| VPUNPCKH (h, h0) -> f102 h h0
| VPUNPCKL (h, h0) -> f103 h h0
| VEXTRACTI128 -> f104
| VINSERTI128 -> f105
| VPERM2I128 -> f106
| VPERMD -> f107
| VPERMQ -> f108
| MOVEMASK (v, w) -> f109 v w
| VPCMPEQ (v, w) -> f110 v w
| VPCMPGT (v, w) -> f111 v w
| VPSIGN (v, w) -> f112 v w
| VPMADDUBSW w -> f113 w
| VPMADDWD w -> f114 w
| VMOVLPD -> f115
| VMOVHPD -> f116
| VPMINU (v, w) -> f117 v w
| VPMINS (v, w) -> f118 v w
| VPMAXU (v, w) -> f119 v w
| VPMAXS (v, w) -> f120 v w
| VPABS (v, w) -> f121 v w
| VPTEST h -> f122 h
| CLFLUSH -> f123
| PREFETCHT0 -> f124
| PREFETCHT1 -> f125
| PREFETCHT2 -> f126
| PREFETCHNTA -> f127
| LFENCE -> f128
| MFENCE -> f129
| SFENCE -> f130
| RDTSC w -> f131 w
| RDTSCP w -> f132 w
| AESDEC -> f133
| VAESDEC w -> f134 w
| AESDECLAST -> f135
| VAESDECLAST w -> f136 w
| AESENC -> f137
| VAESENC w -> f138 w
| AESENCLAST -> f139
| VAESENCLAST w -> f140 w
| AESIMC -> f141
| VAESIMC -> f142
| AESKEYGENASSIST -> f143
| VAESKEYGENASSIST -> f144
| PCLMULQDQ -> f145
| VPCLMULQDQ w -> f146 w
| SHA256RNDS2 -> f147
| SHA256MSG1 -> f148
| SHA256MSG2 -> f149

type is_x86_op =
| Coq_is_MOV of wsize * is_wsize
| Coq_is_MOVSX of wsize * is_wsize * wsize * is_wsize
| Coq_is_MOVZX of wsize * is_wsize * wsize * is_wsize
| Coq_is_CMOVcc of wsize * is_wsize
| Coq_is_XCHG of wsize * is_wsize
| Coq_is_ADD of wsize * is_wsize
| Coq_is_SUB of wsize * is_wsize
| Coq_is_MUL of wsize * is_wsize
| Coq_is_IMUL of wsize * is_wsize
| Coq_is_IMULr of wsize * is_wsize
| Coq_is_IMULri of wsize * is_wsize
| Coq_is_DIV of wsize * is_wsize
| Coq_is_IDIV of wsize * is_wsize
| Coq_is_CQO of wsize * is_wsize
| Coq_is_ADC of wsize * is_wsize
| Coq_is_SBB of wsize * is_wsize
| Coq_is_NEG of wsize * is_wsize
| Coq_is_INC of wsize * is_wsize
| Coq_is_DEC of wsize * is_wsize
| Coq_is_LZCNT of wsize * is_wsize
| Coq_is_TZCNT of wsize * is_wsize
| Coq_is_BSR of wsize * is_wsize
| Coq_is_SETcc
| Coq_is_BT of wsize * is_wsize
| Coq_is_CLC
| Coq_is_STC
| Coq_is_LEA of wsize * is_wsize
| Coq_is_TEST of wsize * is_wsize
| Coq_is_CMP of wsize * is_wsize
| Coq_is_AND of wsize * is_wsize
| Coq_is_ANDN of wsize * is_wsize
| Coq_is_OR of wsize * is_wsize
| Coq_is_XOR of wsize * is_wsize
| Coq_is_NOT of wsize * is_wsize
| Coq_is_ROR of wsize * is_wsize
| Coq_is_ROL of wsize * is_wsize
| Coq_is_RCR of wsize * is_wsize
| Coq_is_RCL of wsize * is_wsize
| Coq_is_SHL of wsize * is_wsize
| Coq_is_SHR of wsize * is_wsize
| Coq_is_SAL of wsize * is_wsize
| Coq_is_SAR of wsize * is_wsize
| Coq_is_SHLD of wsize * is_wsize
| Coq_is_SHRD of wsize * is_wsize
| Coq_is_RORX of wsize * is_wsize
| Coq_is_SARX of wsize * is_wsize
| Coq_is_SHRX of wsize * is_wsize
| Coq_is_SHLX of wsize * is_wsize
| Coq_is_MULX_lo_hi of wsize * is_wsize
| Coq_is_ADCX of wsize * is_wsize
| Coq_is_ADOX of wsize * is_wsize
| Coq_is_BSWAP of wsize * is_wsize
| Coq_is_POPCNT of wsize * is_wsize
| Coq_is_BTR of wsize * is_wsize
| Coq_is_BTS of wsize * is_wsize
| Coq_is_PEXT of wsize * is_wsize
| Coq_is_PDEP of wsize * is_wsize
| Coq_is_MOVX of wsize * is_wsize
| Coq_is_POR
| Coq_is_PADD of velem * is_velem * wsize * is_wsize
| Coq_is_MOVD of wsize * is_wsize
| Coq_is_MOVV of wsize * is_wsize
| Coq_is_VMOV of wsize * is_wsize
| Coq_is_VMOVDQA of wsize * is_wsize
| Coq_is_VMOVDQU of wsize * is_wsize
| Coq_is_VPMOVSX of velem * is_velem * wsize * is_wsize * velem * is_velem
   * wsize * is_wsize
| Coq_is_VPMOVZX of velem * is_velem * wsize * is_wsize * velem * is_velem
   * wsize * is_wsize
| Coq_is_VPAND of wsize * is_wsize
| Coq_is_VPANDN of wsize * is_wsize
| Coq_is_VPOR of wsize * is_wsize
| Coq_is_VPXOR of wsize * is_wsize
| Coq_is_VPADD of velem * is_velem * wsize * is_wsize
| Coq_is_VPSUB of velem * is_velem * wsize * is_wsize
| Coq_is_VPAVG of velem * is_velem * wsize * is_wsize
| Coq_is_VPMULL of velem * is_velem * wsize * is_wsize
| Coq_is_VPMULH of wsize * is_wsize
| Coq_is_VPMULHU of wsize * is_wsize
| Coq_is_VPMULHRS of wsize * is_wsize
| Coq_is_VPMUL of wsize * is_wsize
| Coq_is_VPMULU of wsize * is_wsize
| Coq_is_VPEXTR of wsize * is_wsize
| Coq_is_VPINSR of velem * is_velem
| Coq_is_VPSLL of velem * is_velem * wsize * is_wsize
| Coq_is_VPSRL of velem * is_velem * wsize * is_wsize
| Coq_is_VPSRA of velem * is_velem * wsize * is_wsize
| Coq_is_VPSLLV of velem * is_velem * wsize * is_wsize
| Coq_is_VPSRLV of velem * is_velem * wsize * is_wsize
| Coq_is_VPSLLDQ of wsize * is_wsize
| Coq_is_VPSRLDQ of wsize * is_wsize
| Coq_is_VPSHUFB of wsize * is_wsize
| Coq_is_VPSHUFD of wsize * is_wsize
| Coq_is_VPSHUFHW of wsize * is_wsize
| Coq_is_VPSHUFLW of wsize * is_wsize
| Coq_is_VPBLEND of velem * is_velem * wsize * is_wsize
| Coq_is_BLENDV of velem * is_velem * wsize * is_wsize
| Coq_is_VPACKUS of velem * is_velem * wsize * is_wsize
| Coq_is_VPACKSS of velem * is_velem * wsize * is_wsize
| Coq_is_VSHUFPS of wsize * is_wsize
| Coq_is_VPBROADCAST of velem * is_velem * wsize * is_wsize
| Coq_is_VMOVSHDUP of wsize * is_wsize
| Coq_is_VMOVSLDUP of wsize * is_wsize
| Coq_is_VPALIGNR of wsize * is_wsize
| Coq_is_VBROADCASTI128
| Coq_is_VPUNPCKH of velem * is_velem * wsize * is_wsize
| Coq_is_VPUNPCKL of velem * is_velem * wsize * is_wsize
| Coq_is_VEXTRACTI128
| Coq_is_VINSERTI128
| Coq_is_VPERM2I128
| Coq_is_VPERMD
| Coq_is_VPERMQ
| Coq_is_MOVEMASK of velem * is_velem * wsize * is_wsize
| Coq_is_VPCMPEQ of velem * is_velem * wsize * is_wsize
| Coq_is_VPCMPGT of velem * is_velem * wsize * is_wsize
| Coq_is_VPSIGN of velem * is_velem * wsize * is_wsize
| Coq_is_VPMADDUBSW of wsize * is_wsize
| Coq_is_VPMADDWD of wsize * is_wsize
| Coq_is_VMOVLPD
| Coq_is_VMOVHPD
| Coq_is_VPMINU of velem * is_velem * wsize * is_wsize
| Coq_is_VPMINS of velem * is_velem * wsize * is_wsize
| Coq_is_VPMAXU of velem * is_velem * wsize * is_wsize
| Coq_is_VPMAXS of velem * is_velem * wsize * is_wsize
| Coq_is_VPABS of velem * is_velem * wsize * is_wsize
| Coq_is_VPTEST of wsize * is_wsize
| Coq_is_CLFLUSH
| Coq_is_PREFETCHT0
| Coq_is_PREFETCHT1
| Coq_is_PREFETCHT2
| Coq_is_PREFETCHNTA
| Coq_is_LFENCE
| Coq_is_MFENCE
| Coq_is_SFENCE
| Coq_is_RDTSC of wsize * is_wsize
| Coq_is_RDTSCP of wsize * is_wsize
| Coq_is_AESDEC
| Coq_is_VAESDEC of wsize * is_wsize
| Coq_is_AESDECLAST
| Coq_is_VAESDECLAST of wsize * is_wsize
| Coq_is_AESENC
| Coq_is_VAESENC of wsize * is_wsize
| Coq_is_AESENCLAST
| Coq_is_VAESENCLAST of wsize * is_wsize
| Coq_is_AESIMC
| Coq_is_VAESIMC
| Coq_is_AESKEYGENASSIST
| Coq_is_VAESKEYGENASSIST
| Coq_is_PCLMULQDQ
| Coq_is_VPCLMULQDQ of wsize * is_wsize
| Coq_is_SHA256RNDS2
| Coq_is_SHA256MSG1
| Coq_is_SHA256MSG2

(** val is_x86_op_rect :
    (wsize -> is_wsize -> 'a1) -> (wsize -> is_wsize -> wsize -> is_wsize ->
    'a1) -> (wsize -> is_wsize -> wsize -> is_wsize -> 'a1) -> (wsize ->
    is_wsize -> 'a1) -> (wsize -> is_wsize -> 'a1) -> (wsize -> is_wsize ->
    'a1) -> (wsize -> is_wsize -> 'a1) -> (wsize -> is_wsize -> 'a1) ->
    (wsize -> is_wsize -> 'a1) -> (wsize -> is_wsize -> 'a1) -> (wsize ->
    is_wsize -> 'a1) -> (wsize -> is_wsize -> 'a1) -> (wsize -> is_wsize ->
    'a1) -> (wsize -> is_wsize -> 'a1) -> (wsize -> is_wsize -> 'a1) ->
    (wsize -> is_wsize -> 'a1) -> (wsize -> is_wsize -> 'a1) -> (wsize ->
    is_wsize -> 'a1) -> (wsize -> is_wsize -> 'a1) -> (wsize -> is_wsize ->
    'a1) -> (wsize -> is_wsize -> 'a1) -> (wsize -> is_wsize -> 'a1) -> 'a1
    -> (wsize -> is_wsize -> 'a1) -> 'a1 -> 'a1 -> (wsize -> is_wsize -> 'a1)
    -> (wsize -> is_wsize -> 'a1) -> (wsize -> is_wsize -> 'a1) -> (wsize ->
    is_wsize -> 'a1) -> (wsize -> is_wsize -> 'a1) -> (wsize -> is_wsize ->
    'a1) -> (wsize -> is_wsize -> 'a1) -> (wsize -> is_wsize -> 'a1) ->
    (wsize -> is_wsize -> 'a1) -> (wsize -> is_wsize -> 'a1) -> (wsize ->
    is_wsize -> 'a1) -> (wsize -> is_wsize -> 'a1) -> (wsize -> is_wsize ->
    'a1) -> (wsize -> is_wsize -> 'a1) -> (wsize -> is_wsize -> 'a1) ->
    (wsize -> is_wsize -> 'a1) -> (wsize -> is_wsize -> 'a1) -> (wsize ->
    is_wsize -> 'a1) -> (wsize -> is_wsize -> 'a1) -> (wsize -> is_wsize ->
    'a1) -> (wsize -> is_wsize -> 'a1) -> (wsize -> is_wsize -> 'a1) ->
    (wsize -> is_wsize -> 'a1) -> (wsize -> is_wsize -> 'a1) -> (wsize ->
    is_wsize -> 'a1) -> (wsize -> is_wsize -> 'a1) -> (wsize -> is_wsize ->
    'a1) -> (wsize -> is_wsize -> 'a1) -> (wsize -> is_wsize -> 'a1) ->
    (wsize -> is_wsize -> 'a1) -> (wsize -> is_wsize -> 'a1) -> (wsize ->
    is_wsize -> 'a1) -> 'a1 -> (velem -> is_velem -> wsize -> is_wsize ->
    'a1) -> (wsize -> is_wsize -> 'a1) -> (wsize -> is_wsize -> 'a1) ->
    (wsize -> is_wsize -> 'a1) -> (wsize -> is_wsize -> 'a1) -> (wsize ->
    is_wsize -> 'a1) -> (velem -> is_velem -> wsize -> is_wsize -> velem ->
    is_velem -> wsize -> is_wsize -> 'a1) -> (velem -> is_velem -> wsize ->
    is_wsize -> velem -> is_velem -> wsize -> is_wsize -> 'a1) -> (wsize ->
    is_wsize -> 'a1) -> (wsize -> is_wsize -> 'a1) -> (wsize -> is_wsize ->
    'a1) -> (wsize -> is_wsize -> 'a1) -> (velem -> is_velem -> wsize ->
    is_wsize -> 'a1) -> (velem -> is_velem -> wsize -> is_wsize -> 'a1) ->
    (velem -> is_velem -> wsize -> is_wsize -> 'a1) -> (velem -> is_velem ->
    wsize -> is_wsize -> 'a1) -> (wsize -> is_wsize -> 'a1) -> (wsize ->
    is_wsize -> 'a1) -> (wsize -> is_wsize -> 'a1) -> (wsize -> is_wsize ->
    'a1) -> (wsize -> is_wsize -> 'a1) -> (wsize -> is_wsize -> 'a1) ->
    (velem -> is_velem -> 'a1) -> (velem -> is_velem -> wsize -> is_wsize ->
    'a1) -> (velem -> is_velem -> wsize -> is_wsize -> 'a1) -> (velem ->
    is_velem -> wsize -> is_wsize -> 'a1) -> (velem -> is_velem -> wsize ->
    is_wsize -> 'a1) -> (velem -> is_velem -> wsize -> is_wsize -> 'a1) ->
    (wsize -> is_wsize -> 'a1) -> (wsize -> is_wsize -> 'a1) -> (wsize ->
    is_wsize -> 'a1) -> (wsize -> is_wsize -> 'a1) -> (wsize -> is_wsize ->
    'a1) -> (wsize -> is_wsize -> 'a1) -> (velem -> is_velem -> wsize ->
    is_wsize -> 'a1) -> (velem -> is_velem -> wsize -> is_wsize -> 'a1) ->
    (velem -> is_velem -> wsize -> is_wsize -> 'a1) -> (velem -> is_velem ->
    wsize -> is_wsize -> 'a1) -> (wsize -> is_wsize -> 'a1) -> (velem ->
    is_velem -> wsize -> is_wsize -> 'a1) -> (wsize -> is_wsize -> 'a1) ->
    (wsize -> is_wsize -> 'a1) -> (wsize -> is_wsize -> 'a1) -> 'a1 -> (velem
    -> is_velem -> wsize -> is_wsize -> 'a1) -> (velem -> is_velem -> wsize
    -> is_wsize -> 'a1) -> 'a1 -> 'a1 -> 'a1 -> 'a1 -> 'a1 -> (velem ->
    is_velem -> wsize -> is_wsize -> 'a1) -> (velem -> is_velem -> wsize ->
    is_wsize -> 'a1) -> (velem -> is_velem -> wsize -> is_wsize -> 'a1) ->
    (velem -> is_velem -> wsize -> is_wsize -> 'a1) -> (wsize -> is_wsize ->
    'a1) -> (wsize -> is_wsize -> 'a1) -> 'a1 -> 'a1 -> (velem -> is_velem ->
    wsize -> is_wsize -> 'a1) -> (velem -> is_velem -> wsize -> is_wsize ->
    'a1) -> (velem -> is_velem -> wsize -> is_wsize -> 'a1) -> (velem ->
    is_velem -> wsize -> is_wsize -> 'a1) -> (velem -> is_velem -> wsize ->
    is_wsize -> 'a1) -> (wsize -> is_wsize -> 'a1) -> 'a1 -> 'a1 -> 'a1 ->
    'a1 -> 'a1 -> 'a1 -> 'a1 -> 'a1 -> (wsize -> is_wsize -> 'a1) -> (wsize
    -> is_wsize -> 'a1) -> 'a1 -> (wsize -> is_wsize -> 'a1) -> 'a1 -> (wsize
    -> is_wsize -> 'a1) -> 'a1 -> (wsize -> is_wsize -> 'a1) -> 'a1 -> (wsize
    -> is_wsize -> 'a1) -> 'a1 -> 'a1 -> 'a1 -> 'a1 -> 'a1 -> (wsize ->
    is_wsize -> 'a1) -> 'a1 -> 'a1 -> 'a1 -> x86_op -> is_x86_op -> 'a1 **)

let is_x86_op_rect f f0 f1 f2 f3 f4 f5 f6 f7 f8 f9 f10 f11 f12 f13 f14 f15 f16 f17 f18 f19 f20 f21 f22 f23 f24 f25 f26 f27 f28 f29 f30 f31 f32 f33 f34 f35 f36 f37 f38 f39 f40 f41 f42 f43 f44 f45 f46 f47 f48 f49 f50 f51 f52 f53 f54 f55 f56 f57 f58 f59 f60 f61 f62 f63 f64 f65 f66 f67 f68 f69 f70 f71 f72 f73 f74 f75 f76 f77 f78 f79 f80 f81 f82 f83 f84 f85 f86 f87 f88 f89 f90 f91 f92 f93 f94 f95 f96 f97 f98 f99 f100 f101 f102 f103 f104 f105 f106 f107 f108 f109 f110 f111 f112 f113 f114 f115 f116 f117 f118 f119 f120 f121 f122 f123 f124 f125 f126 f127 f128 f129 f130 f131 f132 f133 f134 f135 f136 f137 f138 f139 f140 f141 f142 f143 f144 f145 f146 f147 f148 f149 _ = function
| Coq_is_MOV (w, p_) -> f w p_
| Coq_is_MOVSX (w, p_, w0, p_0) -> f0 w p_ w0 p_0
| Coq_is_MOVZX (w, p_, w0, p_0) -> f1 w p_ w0 p_0
| Coq_is_CMOVcc (w, p_) -> f2 w p_
| Coq_is_XCHG (w, p_) -> f3 w p_
| Coq_is_ADD (w, p_) -> f4 w p_
| Coq_is_SUB (w, p_) -> f5 w p_
| Coq_is_MUL (w, p_) -> f6 w p_
| Coq_is_IMUL (w, p_) -> f7 w p_
| Coq_is_IMULr (w, p_) -> f8 w p_
| Coq_is_IMULri (w, p_) -> f9 w p_
| Coq_is_DIV (w, p_) -> f10 w p_
| Coq_is_IDIV (w, p_) -> f11 w p_
| Coq_is_CQO (w, p_) -> f12 w p_
| Coq_is_ADC (w, p_) -> f13 w p_
| Coq_is_SBB (w, p_) -> f14 w p_
| Coq_is_NEG (w, p_) -> f15 w p_
| Coq_is_INC (w, p_) -> f16 w p_
| Coq_is_DEC (w, p_) -> f17 w p_
| Coq_is_LZCNT (w, p_) -> f18 w p_
| Coq_is_TZCNT (w, p_) -> f19 w p_
| Coq_is_BSR (w, p_) -> f20 w p_
| Coq_is_SETcc -> f21
| Coq_is_BT (w, p_) -> f22 w p_
| Coq_is_CLC -> f23
| Coq_is_STC -> f24
| Coq_is_LEA (w, p_) -> f25 w p_
| Coq_is_TEST (w, p_) -> f26 w p_
| Coq_is_CMP (w, p_) -> f27 w p_
| Coq_is_AND (w, p_) -> f28 w p_
| Coq_is_ANDN (w, p_) -> f29 w p_
| Coq_is_OR (w, p_) -> f30 w p_
| Coq_is_XOR (w, p_) -> f31 w p_
| Coq_is_NOT (w, p_) -> f32 w p_
| Coq_is_ROR (w, p_) -> f33 w p_
| Coq_is_ROL (w, p_) -> f34 w p_
| Coq_is_RCR (w, p_) -> f35 w p_
| Coq_is_RCL (w, p_) -> f36 w p_
| Coq_is_SHL (w, p_) -> f37 w p_
| Coq_is_SHR (w, p_) -> f38 w p_
| Coq_is_SAL (w, p_) -> f39 w p_
| Coq_is_SAR (w, p_) -> f40 w p_
| Coq_is_SHLD (w, p_) -> f41 w p_
| Coq_is_SHRD (w, p_) -> f42 w p_
| Coq_is_RORX (w, p_) -> f43 w p_
| Coq_is_SARX (w, p_) -> f44 w p_
| Coq_is_SHRX (w, p_) -> f45 w p_
| Coq_is_SHLX (w, p_) -> f46 w p_
| Coq_is_MULX_lo_hi (w, p_) -> f47 w p_
| Coq_is_ADCX (w, p_) -> f48 w p_
| Coq_is_ADOX (w, p_) -> f49 w p_
| Coq_is_BSWAP (w, p_) -> f50 w p_
| Coq_is_POPCNT (w, p_) -> f51 w p_
| Coq_is_BTR (w, p_) -> f52 w p_
| Coq_is_BTS (w, p_) -> f53 w p_
| Coq_is_PEXT (w, p_) -> f54 w p_
| Coq_is_PDEP (w, p_) -> f55 w p_
| Coq_is_MOVX (w, p_) -> f56 w p_
| Coq_is_POR -> f57
| Coq_is_PADD (v, p_, w, p_0) -> f58 v p_ w p_0
| Coq_is_MOVD (w, p_) -> f59 w p_
| Coq_is_MOVV (w, p_) -> f60 w p_
| Coq_is_VMOV (w, p_) -> f61 w p_
| Coq_is_VMOVDQA (w, p_) -> f62 w p_
| Coq_is_VMOVDQU (h, pH) -> f63 h pH
| Coq_is_VPMOVSX (v, p_, w, p_0, v0, p_1, w0, p_2) ->
  f64 v p_ w p_0 v0 p_1 w0 p_2
| Coq_is_VPMOVZX (v, p_, w, p_0, v0, p_1, w0, p_2) ->
  f65 v p_ w p_0 v0 p_1 w0 p_2
| Coq_is_VPAND (h, pH) -> f66 h pH
| Coq_is_VPANDN (h, pH) -> f67 h pH
| Coq_is_VPOR (h, pH) -> f68 h pH
| Coq_is_VPXOR (h, pH) -> f69 h pH
| Coq_is_VPADD (h, pH, h0, pH0) -> f70 h pH h0 pH0
| Coq_is_VPSUB (h, pH, h0, pH0) -> f71 h pH h0 pH0
| Coq_is_VPAVG (v, p_, w, p_0) -> f72 v p_ w p_0
| Coq_is_VPMULL (h, pH, h0, pH0) -> f73 h pH h0 pH0
| Coq_is_VPMULH (w, p_) -> f74 w p_
| Coq_is_VPMULHU (w, p_) -> f75 w p_
| Coq_is_VPMULHRS (w, p_) -> f76 w p_
| Coq_is_VPMUL (h, pH) -> f77 h pH
| Coq_is_VPMULU (h, pH) -> f78 h pH
| Coq_is_VPEXTR (w, p_) -> f79 w p_
| Coq_is_VPINSR (v, p_) -> f80 v p_
| Coq_is_VPSLL (h, pH, h0, pH0) -> f81 h pH h0 pH0
| Coq_is_VPSRL (h, pH, h0, pH0) -> f82 h pH h0 pH0
| Coq_is_VPSRA (h, pH, h0, pH0) -> f83 h pH h0 pH0
| Coq_is_VPSLLV (h, pH, h0, pH0) -> f84 h pH h0 pH0
| Coq_is_VPSRLV (h, pH, h0, pH0) -> f85 h pH h0 pH0
| Coq_is_VPSLLDQ (h, pH) -> f86 h pH
| Coq_is_VPSRLDQ (h, pH) -> f87 h pH
| Coq_is_VPSHUFB (h, pH) -> f88 h pH
| Coq_is_VPSHUFD (h, pH) -> f89 h pH
| Coq_is_VPSHUFHW (h, pH) -> f90 h pH
| Coq_is_VPSHUFLW (h, pH) -> f91 h pH
| Coq_is_VPBLEND (h, pH, h0, pH0) -> f92 h pH h0 pH0
| Coq_is_BLENDV (v, p_, w, p_0) -> f93 v p_ w p_0
| Coq_is_VPACKUS (h, pH, h0, pH0) -> f94 h pH h0 pH0
| Coq_is_VPACKSS (h, pH, h0, pH0) -> f95 h pH h0 pH0
| Coq_is_VSHUFPS (h, pH) -> f96 h pH
| Coq_is_VPBROADCAST (v, p_, w, p_0) -> f97 v p_ w p_0
| Coq_is_VMOVSHDUP (w, p_) -> f98 w p_
| Coq_is_VMOVSLDUP (w, p_) -> f99 w p_
| Coq_is_VPALIGNR (h, pH) -> f100 h pH
| Coq_is_VBROADCASTI128 -> f101
| Coq_is_VPUNPCKH (h, pH, h0, pH0) -> f102 h pH h0 pH0
| Coq_is_VPUNPCKL (h, pH, h0, pH0) -> f103 h pH h0 pH0
| Coq_is_VEXTRACTI128 -> f104
| Coq_is_VINSERTI128 -> f105
| Coq_is_VPERM2I128 -> f106
| Coq_is_VPERMD -> f107
| Coq_is_VPERMQ -> f108
| Coq_is_MOVEMASK (v, p_, w, p_0) -> f109 v p_ w p_0
| Coq_is_VPCMPEQ (v, p_, w, p_0) -> f110 v p_ w p_0
| Coq_is_VPCMPGT (v, p_, w, p_0) -> f111 v p_ w p_0
| Coq_is_VPSIGN (v, p_, w, p_0) -> f112 v p_ w p_0
| Coq_is_VPMADDUBSW (w, p_) -> f113 w p_
| Coq_is_VPMADDWD (w, p_) -> f114 w p_
| Coq_is_VMOVLPD -> f115
| Coq_is_VMOVHPD -> f116
| Coq_is_VPMINU (v, p_, w, p_0) -> f117 v p_ w p_0
| Coq_is_VPMINS (v, p_, w, p_0) -> f118 v p_ w p_0
| Coq_is_VPMAXU (v, p_, w, p_0) -> f119 v p_ w p_0
| Coq_is_VPMAXS (v, p_, w, p_0) -> f120 v p_ w p_0
| Coq_is_VPABS (v, p_, w, p_0) -> f121 v p_ w p_0
| Coq_is_VPTEST (h, pH) -> f122 h pH
| Coq_is_CLFLUSH -> f123
| Coq_is_PREFETCHT0 -> f124
| Coq_is_PREFETCHT1 -> f125
| Coq_is_PREFETCHT2 -> f126
| Coq_is_PREFETCHNTA -> f127
| Coq_is_LFENCE -> f128
| Coq_is_MFENCE -> f129
| Coq_is_SFENCE -> f130
| Coq_is_RDTSC (w, p_) -> f131 w p_
| Coq_is_RDTSCP (w, p_) -> f132 w p_
| Coq_is_AESDEC -> f133
| Coq_is_VAESDEC (w, p_) -> f134 w p_
| Coq_is_AESDECLAST -> f135
| Coq_is_VAESDECLAST (w, p_) -> f136 w p_
| Coq_is_AESENC -> f137
| Coq_is_VAESENC (w, p_) -> f138 w p_
| Coq_is_AESENCLAST -> f139
| Coq_is_VAESENCLAST (w, p_) -> f140 w p_
| Coq_is_AESIMC -> f141
| Coq_is_VAESIMC -> f142
| Coq_is_AESKEYGENASSIST -> f143
| Coq_is_VAESKEYGENASSIST -> f144
| Coq_is_PCLMULQDQ -> f145
| Coq_is_VPCLMULQDQ (w, p_) -> f146 w p_
| Coq_is_SHA256RNDS2 -> f147
| Coq_is_SHA256MSG1 -> f148
| Coq_is_SHA256MSG2 -> f149

(** val is_x86_op_rec :
    (wsize -> is_wsize -> 'a1) -> (wsize -> is_wsize -> wsize -> is_wsize ->
    'a1) -> (wsize -> is_wsize -> wsize -> is_wsize -> 'a1) -> (wsize ->
    is_wsize -> 'a1) -> (wsize -> is_wsize -> 'a1) -> (wsize -> is_wsize ->
    'a1) -> (wsize -> is_wsize -> 'a1) -> (wsize -> is_wsize -> 'a1) ->
    (wsize -> is_wsize -> 'a1) -> (wsize -> is_wsize -> 'a1) -> (wsize ->
    is_wsize -> 'a1) -> (wsize -> is_wsize -> 'a1) -> (wsize -> is_wsize ->
    'a1) -> (wsize -> is_wsize -> 'a1) -> (wsize -> is_wsize -> 'a1) ->
    (wsize -> is_wsize -> 'a1) -> (wsize -> is_wsize -> 'a1) -> (wsize ->
    is_wsize -> 'a1) -> (wsize -> is_wsize -> 'a1) -> (wsize -> is_wsize ->
    'a1) -> (wsize -> is_wsize -> 'a1) -> (wsize -> is_wsize -> 'a1) -> 'a1
    -> (wsize -> is_wsize -> 'a1) -> 'a1 -> 'a1 -> (wsize -> is_wsize -> 'a1)
    -> (wsize -> is_wsize -> 'a1) -> (wsize -> is_wsize -> 'a1) -> (wsize ->
    is_wsize -> 'a1) -> (wsize -> is_wsize -> 'a1) -> (wsize -> is_wsize ->
    'a1) -> (wsize -> is_wsize -> 'a1) -> (wsize -> is_wsize -> 'a1) ->
    (wsize -> is_wsize -> 'a1) -> (wsize -> is_wsize -> 'a1) -> (wsize ->
    is_wsize -> 'a1) -> (wsize -> is_wsize -> 'a1) -> (wsize -> is_wsize ->
    'a1) -> (wsize -> is_wsize -> 'a1) -> (wsize -> is_wsize -> 'a1) ->
    (wsize -> is_wsize -> 'a1) -> (wsize -> is_wsize -> 'a1) -> (wsize ->
    is_wsize -> 'a1) -> (wsize -> is_wsize -> 'a1) -> (wsize -> is_wsize ->
    'a1) -> (wsize -> is_wsize -> 'a1) -> (wsize -> is_wsize -> 'a1) ->
    (wsize -> is_wsize -> 'a1) -> (wsize -> is_wsize -> 'a1) -> (wsize ->
    is_wsize -> 'a1) -> (wsize -> is_wsize -> 'a1) -> (wsize -> is_wsize ->
    'a1) -> (wsize -> is_wsize -> 'a1) -> (wsize -> is_wsize -> 'a1) ->
    (wsize -> is_wsize -> 'a1) -> (wsize -> is_wsize -> 'a1) -> (wsize ->
    is_wsize -> 'a1) -> 'a1 -> (velem -> is_velem -> wsize -> is_wsize ->
    'a1) -> (wsize -> is_wsize -> 'a1) -> (wsize -> is_wsize -> 'a1) ->
    (wsize -> is_wsize -> 'a1) -> (wsize -> is_wsize -> 'a1) -> (wsize ->
    is_wsize -> 'a1) -> (velem -> is_velem -> wsize -> is_wsize -> velem ->
    is_velem -> wsize -> is_wsize -> 'a1) -> (velem -> is_velem -> wsize ->
    is_wsize -> velem -> is_velem -> wsize -> is_wsize -> 'a1) -> (wsize ->
    is_wsize -> 'a1) -> (wsize -> is_wsize -> 'a1) -> (wsize -> is_wsize ->
    'a1) -> (wsize -> is_wsize -> 'a1) -> (velem -> is_velem -> wsize ->
    is_wsize -> 'a1) -> (velem -> is_velem -> wsize -> is_wsize -> 'a1) ->
    (velem -> is_velem -> wsize -> is_wsize -> 'a1) -> (velem -> is_velem ->
    wsize -> is_wsize -> 'a1) -> (wsize -> is_wsize -> 'a1) -> (wsize ->
    is_wsize -> 'a1) -> (wsize -> is_wsize -> 'a1) -> (wsize -> is_wsize ->
    'a1) -> (wsize -> is_wsize -> 'a1) -> (wsize -> is_wsize -> 'a1) ->
    (velem -> is_velem -> 'a1) -> (velem -> is_velem -> wsize -> is_wsize ->
    'a1) -> (velem -> is_velem -> wsize -> is_wsize -> 'a1) -> (velem ->
    is_velem -> wsize -> is_wsize -> 'a1) -> (velem -> is_velem -> wsize ->
    is_wsize -> 'a1) -> (velem -> is_velem -> wsize -> is_wsize -> 'a1) ->
    (wsize -> is_wsize -> 'a1) -> (wsize -> is_wsize -> 'a1) -> (wsize ->
    is_wsize -> 'a1) -> (wsize -> is_wsize -> 'a1) -> (wsize -> is_wsize ->
    'a1) -> (wsize -> is_wsize -> 'a1) -> (velem -> is_velem -> wsize ->
    is_wsize -> 'a1) -> (velem -> is_velem -> wsize -> is_wsize -> 'a1) ->
    (velem -> is_velem -> wsize -> is_wsize -> 'a1) -> (velem -> is_velem ->
    wsize -> is_wsize -> 'a1) -> (wsize -> is_wsize -> 'a1) -> (velem ->
    is_velem -> wsize -> is_wsize -> 'a1) -> (wsize -> is_wsize -> 'a1) ->
    (wsize -> is_wsize -> 'a1) -> (wsize -> is_wsize -> 'a1) -> 'a1 -> (velem
    -> is_velem -> wsize -> is_wsize -> 'a1) -> (velem -> is_velem -> wsize
    -> is_wsize -> 'a1) -> 'a1 -> 'a1 -> 'a1 -> 'a1 -> 'a1 -> (velem ->
    is_velem -> wsize -> is_wsize -> 'a1) -> (velem -> is_velem -> wsize ->
    is_wsize -> 'a1) -> (velem -> is_velem -> wsize -> is_wsize -> 'a1) ->
    (velem -> is_velem -> wsize -> is_wsize -> 'a1) -> (wsize -> is_wsize ->
    'a1) -> (wsize -> is_wsize -> 'a1) -> 'a1 -> 'a1 -> (velem -> is_velem ->
    wsize -> is_wsize -> 'a1) -> (velem -> is_velem -> wsize -> is_wsize ->
    'a1) -> (velem -> is_velem -> wsize -> is_wsize -> 'a1) -> (velem ->
    is_velem -> wsize -> is_wsize -> 'a1) -> (velem -> is_velem -> wsize ->
    is_wsize -> 'a1) -> (wsize -> is_wsize -> 'a1) -> 'a1 -> 'a1 -> 'a1 ->
    'a1 -> 'a1 -> 'a1 -> 'a1 -> 'a1 -> (wsize -> is_wsize -> 'a1) -> (wsize
    -> is_wsize -> 'a1) -> 'a1 -> (wsize -> is_wsize -> 'a1) -> 'a1 -> (wsize
    -> is_wsize -> 'a1) -> 'a1 -> (wsize -> is_wsize -> 'a1) -> 'a1 -> (wsize
    -> is_wsize -> 'a1) -> 'a1 -> 'a1 -> 'a1 -> 'a1 -> 'a1 -> (wsize ->
    is_wsize -> 'a1) -> 'a1 -> 'a1 -> 'a1 -> x86_op -> is_x86_op -> 'a1 **)

let is_x86_op_rec f f0 f1 f2 f3 f4 f5 f6 f7 f8 f9 f10 f11 f12 f13 f14 f15 f16 f17 f18 f19 f20 f21 f22 f23 f24 f25 f26 f27 f28 f29 f30 f31 f32 f33 f34 f35 f36 f37 f38 f39 f40 f41 f42 f43 f44 f45 f46 f47 f48 f49 f50 f51 f52 f53 f54 f55 f56 f57 f58 f59 f60 f61 f62 f63 f64 f65 f66 f67 f68 f69 f70 f71 f72 f73 f74 f75 f76 f77 f78 f79 f80 f81 f82 f83 f84 f85 f86 f87 f88 f89 f90 f91 f92 f93 f94 f95 f96 f97 f98 f99 f100 f101 f102 f103 f104 f105 f106 f107 f108 f109 f110 f111 f112 f113 f114 f115 f116 f117 f118 f119 f120 f121 f122 f123 f124 f125 f126 f127 f128 f129 f130 f131 f132 f133 f134 f135 f136 f137 f138 f139 f140 f141 f142 f143 f144 f145 f146 f147 f148 f149 _ = function
| Coq_is_MOV (w, p_) -> f w p_
| Coq_is_MOVSX (w, p_, w0, p_0) -> f0 w p_ w0 p_0
| Coq_is_MOVZX (w, p_, w0, p_0) -> f1 w p_ w0 p_0
| Coq_is_CMOVcc (w, p_) -> f2 w p_
| Coq_is_XCHG (w, p_) -> f3 w p_
| Coq_is_ADD (w, p_) -> f4 w p_
| Coq_is_SUB (w, p_) -> f5 w p_
| Coq_is_MUL (w, p_) -> f6 w p_
| Coq_is_IMUL (w, p_) -> f7 w p_
| Coq_is_IMULr (w, p_) -> f8 w p_
| Coq_is_IMULri (w, p_) -> f9 w p_
| Coq_is_DIV (w, p_) -> f10 w p_
| Coq_is_IDIV (w, p_) -> f11 w p_
| Coq_is_CQO (w, p_) -> f12 w p_
| Coq_is_ADC (w, p_) -> f13 w p_
| Coq_is_SBB (w, p_) -> f14 w p_
| Coq_is_NEG (w, p_) -> f15 w p_
| Coq_is_INC (w, p_) -> f16 w p_
| Coq_is_DEC (w, p_) -> f17 w p_
| Coq_is_LZCNT (w, p_) -> f18 w p_
| Coq_is_TZCNT (w, p_) -> f19 w p_
| Coq_is_BSR (w, p_) -> f20 w p_
| Coq_is_SETcc -> f21
| Coq_is_BT (w, p_) -> f22 w p_
| Coq_is_CLC -> f23
| Coq_is_STC -> f24
| Coq_is_LEA (w, p_) -> f25 w p_
| Coq_is_TEST (w, p_) -> f26 w p_
| Coq_is_CMP (w, p_) -> f27 w p_
| Coq_is_AND (w, p_) -> f28 w p_
| Coq_is_ANDN (w, p_) -> f29 w p_
| Coq_is_OR (w, p_) -> f30 w p_
| Coq_is_XOR (w, p_) -> f31 w p_
| Coq_is_NOT (w, p_) -> f32 w p_
| Coq_is_ROR (w, p_) -> f33 w p_
| Coq_is_ROL (w, p_) -> f34 w p_
| Coq_is_RCR (w, p_) -> f35 w p_
| Coq_is_RCL (w, p_) -> f36 w p_
| Coq_is_SHL (w, p_) -> f37 w p_
| Coq_is_SHR (w, p_) -> f38 w p_
| Coq_is_SAL (w, p_) -> f39 w p_
| Coq_is_SAR (w, p_) -> f40 w p_
| Coq_is_SHLD (w, p_) -> f41 w p_
| Coq_is_SHRD (w, p_) -> f42 w p_
| Coq_is_RORX (w, p_) -> f43 w p_
| Coq_is_SARX (w, p_) -> f44 w p_
| Coq_is_SHRX (w, p_) -> f45 w p_
| Coq_is_SHLX (w, p_) -> f46 w p_
| Coq_is_MULX_lo_hi (w, p_) -> f47 w p_
| Coq_is_ADCX (w, p_) -> f48 w p_
| Coq_is_ADOX (w, p_) -> f49 w p_
| Coq_is_BSWAP (w, p_) -> f50 w p_
| Coq_is_POPCNT (w, p_) -> f51 w p_
| Coq_is_BTR (w, p_) -> f52 w p_
| Coq_is_BTS (w, p_) -> f53 w p_
| Coq_is_PEXT (w, p_) -> f54 w p_
| Coq_is_PDEP (w, p_) -> f55 w p_
| Coq_is_MOVX (w, p_) -> f56 w p_
| Coq_is_POR -> f57
| Coq_is_PADD (v, p_, w, p_0) -> f58 v p_ w p_0
| Coq_is_MOVD (w, p_) -> f59 w p_
| Coq_is_MOVV (w, p_) -> f60 w p_
| Coq_is_VMOV (w, p_) -> f61 w p_
| Coq_is_VMOVDQA (w, p_) -> f62 w p_
| Coq_is_VMOVDQU (h, pH) -> f63 h pH
| Coq_is_VPMOVSX (v, p_, w, p_0, v0, p_1, w0, p_2) ->
  f64 v p_ w p_0 v0 p_1 w0 p_2
| Coq_is_VPMOVZX (v, p_, w, p_0, v0, p_1, w0, p_2) ->
  f65 v p_ w p_0 v0 p_1 w0 p_2
| Coq_is_VPAND (h, pH) -> f66 h pH
| Coq_is_VPANDN (h, pH) -> f67 h pH
| Coq_is_VPOR (h, pH) -> f68 h pH
| Coq_is_VPXOR (h, pH) -> f69 h pH
| Coq_is_VPADD (h, pH, h0, pH0) -> f70 h pH h0 pH0
| Coq_is_VPSUB (h, pH, h0, pH0) -> f71 h pH h0 pH0
| Coq_is_VPAVG (v, p_, w, p_0) -> f72 v p_ w p_0
| Coq_is_VPMULL (h, pH, h0, pH0) -> f73 h pH h0 pH0
| Coq_is_VPMULH (w, p_) -> f74 w p_
| Coq_is_VPMULHU (w, p_) -> f75 w p_
| Coq_is_VPMULHRS (w, p_) -> f76 w p_
| Coq_is_VPMUL (h, pH) -> f77 h pH
| Coq_is_VPMULU (h, pH) -> f78 h pH
| Coq_is_VPEXTR (w, p_) -> f79 w p_
| Coq_is_VPINSR (v, p_) -> f80 v p_
| Coq_is_VPSLL (h, pH, h0, pH0) -> f81 h pH h0 pH0
| Coq_is_VPSRL (h, pH, h0, pH0) -> f82 h pH h0 pH0
| Coq_is_VPSRA (h, pH, h0, pH0) -> f83 h pH h0 pH0
| Coq_is_VPSLLV (h, pH, h0, pH0) -> f84 h pH h0 pH0
| Coq_is_VPSRLV (h, pH, h0, pH0) -> f85 h pH h0 pH0
| Coq_is_VPSLLDQ (h, pH) -> f86 h pH
| Coq_is_VPSRLDQ (h, pH) -> f87 h pH
| Coq_is_VPSHUFB (h, pH) -> f88 h pH
| Coq_is_VPSHUFD (h, pH) -> f89 h pH
| Coq_is_VPSHUFHW (h, pH) -> f90 h pH
| Coq_is_VPSHUFLW (h, pH) -> f91 h pH
| Coq_is_VPBLEND (h, pH, h0, pH0) -> f92 h pH h0 pH0
| Coq_is_BLENDV (v, p_, w, p_0) -> f93 v p_ w p_0
| Coq_is_VPACKUS (h, pH, h0, pH0) -> f94 h pH h0 pH0
| Coq_is_VPACKSS (h, pH, h0, pH0) -> f95 h pH h0 pH0
| Coq_is_VSHUFPS (h, pH) -> f96 h pH
| Coq_is_VPBROADCAST (v, p_, w, p_0) -> f97 v p_ w p_0
| Coq_is_VMOVSHDUP (w, p_) -> f98 w p_
| Coq_is_VMOVSLDUP (w, p_) -> f99 w p_
| Coq_is_VPALIGNR (h, pH) -> f100 h pH
| Coq_is_VBROADCASTI128 -> f101
| Coq_is_VPUNPCKH (h, pH, h0, pH0) -> f102 h pH h0 pH0
| Coq_is_VPUNPCKL (h, pH, h0, pH0) -> f103 h pH h0 pH0
| Coq_is_VEXTRACTI128 -> f104
| Coq_is_VINSERTI128 -> f105
| Coq_is_VPERM2I128 -> f106
| Coq_is_VPERMD -> f107
| Coq_is_VPERMQ -> f108
| Coq_is_MOVEMASK (v, p_, w, p_0) -> f109 v p_ w p_0
| Coq_is_VPCMPEQ (v, p_, w, p_0) -> f110 v p_ w p_0
| Coq_is_VPCMPGT (v, p_, w, p_0) -> f111 v p_ w p_0
| Coq_is_VPSIGN (v, p_, w, p_0) -> f112 v p_ w p_0
| Coq_is_VPMADDUBSW (w, p_) -> f113 w p_
| Coq_is_VPMADDWD (w, p_) -> f114 w p_
| Coq_is_VMOVLPD -> f115
| Coq_is_VMOVHPD -> f116
| Coq_is_VPMINU (v, p_, w, p_0) -> f117 v p_ w p_0
| Coq_is_VPMINS (v, p_, w, p_0) -> f118 v p_ w p_0
| Coq_is_VPMAXU (v, p_, w, p_0) -> f119 v p_ w p_0
| Coq_is_VPMAXS (v, p_, w, p_0) -> f120 v p_ w p_0
| Coq_is_VPABS (v, p_, w, p_0) -> f121 v p_ w p_0
| Coq_is_VPTEST (h, pH) -> f122 h pH
| Coq_is_CLFLUSH -> f123
| Coq_is_PREFETCHT0 -> f124
| Coq_is_PREFETCHT1 -> f125
| Coq_is_PREFETCHT2 -> f126
| Coq_is_PREFETCHNTA -> f127
| Coq_is_LFENCE -> f128
| Coq_is_MFENCE -> f129
| Coq_is_SFENCE -> f130
| Coq_is_RDTSC (w, p_) -> f131 w p_
| Coq_is_RDTSCP (w, p_) -> f132 w p_
| Coq_is_AESDEC -> f133
| Coq_is_VAESDEC (w, p_) -> f134 w p_
| Coq_is_AESDECLAST -> f135
| Coq_is_VAESDECLAST (w, p_) -> f136 w p_
| Coq_is_AESENC -> f137
| Coq_is_VAESENC (w, p_) -> f138 w p_
| Coq_is_AESENCLAST -> f139
| Coq_is_VAESENCLAST (w, p_) -> f140 w p_
| Coq_is_AESIMC -> f141
| Coq_is_VAESIMC -> f142
| Coq_is_AESKEYGENASSIST -> f143
| Coq_is_VAESKEYGENASSIST -> f144
| Coq_is_PCLMULQDQ -> f145
| Coq_is_VPCLMULQDQ (w, p_) -> f146 w p_
| Coq_is_SHA256RNDS2 -> f147
| Coq_is_SHA256MSG1 -> f148
| Coq_is_SHA256MSG2 -> f149

(** val x86_op_tag : x86_op -> positive **)

let x86_op_tag = function
| MOV _ -> Coq_xH
| MOVSX (_, _) -> Coq_xO Coq_xH
| MOVZX (_, _) -> Coq_xI Coq_xH
| CMOVcc _ -> Coq_xO (Coq_xO Coq_xH)
| XCHG _ -> Coq_xI (Coq_xO Coq_xH)
| ADD _ -> Coq_xO (Coq_xI Coq_xH)
| SUB _ -> Coq_xI (Coq_xI Coq_xH)
| MUL _ -> Coq_xO (Coq_xO (Coq_xO Coq_xH))
| IMUL _ -> Coq_xI (Coq_xO (Coq_xO Coq_xH))
| IMULr _ -> Coq_xO (Coq_xI (Coq_xO Coq_xH))
| IMULri _ -> Coq_xI (Coq_xI (Coq_xO Coq_xH))
| DIV _ -> Coq_xO (Coq_xO (Coq_xI Coq_xH))
| IDIV _ -> Coq_xI (Coq_xO (Coq_xI Coq_xH))
| CQO _ -> Coq_xO (Coq_xI (Coq_xI Coq_xH))
| ADC _ -> Coq_xI (Coq_xI (Coq_xI Coq_xH))
| SBB _ -> Coq_xO (Coq_xO (Coq_xO (Coq_xO Coq_xH)))
| NEG _ -> Coq_xI (Coq_xO (Coq_xO (Coq_xO Coq_xH)))
| INC _ -> Coq_xO (Coq_xI (Coq_xO (Coq_xO Coq_xH)))
| DEC _ -> Coq_xI (Coq_xI (Coq_xO (Coq_xO Coq_xH)))
| LZCNT _ -> Coq_xO (Coq_xO (Coq_xI (Coq_xO Coq_xH)))
| TZCNT _ -> Coq_xI (Coq_xO (Coq_xI (Coq_xO Coq_xH)))
| BSR _ -> Coq_xO (Coq_xI (Coq_xI (Coq_xO Coq_xH)))
| SETcc -> Coq_xI (Coq_xI (Coq_xI (Coq_xO Coq_xH)))
| BT _ -> Coq_xO (Coq_xO (Coq_xO (Coq_xI Coq_xH)))
| CLC -> Coq_xI (Coq_xO (Coq_xO (Coq_xI Coq_xH)))
| STC -> Coq_xO (Coq_xI (Coq_xO (Coq_xI Coq_xH)))
| LEA _ -> Coq_xI (Coq_xI (Coq_xO (Coq_xI Coq_xH)))
| TEST _ -> Coq_xO (Coq_xO (Coq_xI (Coq_xI Coq_xH)))
| CMP _ -> Coq_xI (Coq_xO (Coq_xI (Coq_xI Coq_xH)))
| AND _ -> Coq_xO (Coq_xI (Coq_xI (Coq_xI Coq_xH)))
| ANDN _ -> Coq_xI (Coq_xI (Coq_xI (Coq_xI Coq_xH)))
| OR _ -> Coq_xO (Coq_xO (Coq_xO (Coq_xO (Coq_xO Coq_xH))))
| XOR _ -> Coq_xI (Coq_xO (Coq_xO (Coq_xO (Coq_xO Coq_xH))))
| NOT _ -> Coq_xO (Coq_xI (Coq_xO (Coq_xO (Coq_xO Coq_xH))))
| ROR _ -> Coq_xI (Coq_xI (Coq_xO (Coq_xO (Coq_xO Coq_xH))))
| ROL _ -> Coq_xO (Coq_xO (Coq_xI (Coq_xO (Coq_xO Coq_xH))))
| RCR _ -> Coq_xI (Coq_xO (Coq_xI (Coq_xO (Coq_xO Coq_xH))))
| RCL _ -> Coq_xO (Coq_xI (Coq_xI (Coq_xO (Coq_xO Coq_xH))))
| SHL _ -> Coq_xI (Coq_xI (Coq_xI (Coq_xO (Coq_xO Coq_xH))))
| SHR _ -> Coq_xO (Coq_xO (Coq_xO (Coq_xI (Coq_xO Coq_xH))))
| SAL _ -> Coq_xI (Coq_xO (Coq_xO (Coq_xI (Coq_xO Coq_xH))))
| SAR _ -> Coq_xO (Coq_xI (Coq_xO (Coq_xI (Coq_xO Coq_xH))))
| SHLD _ -> Coq_xI (Coq_xI (Coq_xO (Coq_xI (Coq_xO Coq_xH))))
| SHRD _ -> Coq_xO (Coq_xO (Coq_xI (Coq_xI (Coq_xO Coq_xH))))
| RORX _ -> Coq_xI (Coq_xO (Coq_xI (Coq_xI (Coq_xO Coq_xH))))
| SARX _ -> Coq_xO (Coq_xI (Coq_xI (Coq_xI (Coq_xO Coq_xH))))
| SHRX _ -> Coq_xI (Coq_xI (Coq_xI (Coq_xI (Coq_xO Coq_xH))))
| SHLX _ -> Coq_xO (Coq_xO (Coq_xO (Coq_xO (Coq_xI Coq_xH))))
| MULX_lo_hi _ -> Coq_xI (Coq_xO (Coq_xO (Coq_xO (Coq_xI Coq_xH))))
| ADCX _ -> Coq_xO (Coq_xI (Coq_xO (Coq_xO (Coq_xI Coq_xH))))
| ADOX _ -> Coq_xI (Coq_xI (Coq_xO (Coq_xO (Coq_xI Coq_xH))))
| BSWAP _ -> Coq_xO (Coq_xO (Coq_xI (Coq_xO (Coq_xI Coq_xH))))
| POPCNT _ -> Coq_xI (Coq_xO (Coq_xI (Coq_xO (Coq_xI Coq_xH))))
| BTR _ -> Coq_xO (Coq_xI (Coq_xI (Coq_xO (Coq_xI Coq_xH))))
| BTS _ -> Coq_xI (Coq_xI (Coq_xI (Coq_xO (Coq_xI Coq_xH))))
| PEXT _ -> Coq_xO (Coq_xO (Coq_xO (Coq_xI (Coq_xI Coq_xH))))
| PDEP _ -> Coq_xI (Coq_xO (Coq_xO (Coq_xI (Coq_xI Coq_xH))))
| MOVX _ -> Coq_xO (Coq_xI (Coq_xO (Coq_xI (Coq_xI Coq_xH))))
| POR -> Coq_xI (Coq_xI (Coq_xO (Coq_xI (Coq_xI Coq_xH))))
| PADD (_, _) -> Coq_xO (Coq_xO (Coq_xI (Coq_xI (Coq_xI Coq_xH))))
| MOVD _ -> Coq_xI (Coq_xO (Coq_xI (Coq_xI (Coq_xI Coq_xH))))
| MOVV _ -> Coq_xO (Coq_xI (Coq_xI (Coq_xI (Coq_xI Coq_xH))))
| VMOV _ -> Coq_xI (Coq_xI (Coq_xI (Coq_xI (Coq_xI Coq_xH))))
| VMOVDQA _ -> Coq_xO (Coq_xO (Coq_xO (Coq_xO (Coq_xO (Coq_xO Coq_xH)))))
| VMOVDQU _ -> Coq_xI (Coq_xO (Coq_xO (Coq_xO (Coq_xO (Coq_xO Coq_xH)))))
| VPMOVSX (_, _, _, _) ->
  Coq_xO (Coq_xI (Coq_xO (Coq_xO (Coq_xO (Coq_xO Coq_xH)))))
| VPMOVZX (_, _, _, _) ->
  Coq_xI (Coq_xI (Coq_xO (Coq_xO (Coq_xO (Coq_xO Coq_xH)))))
| VPAND _ -> Coq_xO (Coq_xO (Coq_xI (Coq_xO (Coq_xO (Coq_xO Coq_xH)))))
| VPANDN _ -> Coq_xI (Coq_xO (Coq_xI (Coq_xO (Coq_xO (Coq_xO Coq_xH)))))
| VPOR _ -> Coq_xO (Coq_xI (Coq_xI (Coq_xO (Coq_xO (Coq_xO Coq_xH)))))
| VPXOR _ -> Coq_xI (Coq_xI (Coq_xI (Coq_xO (Coq_xO (Coq_xO Coq_xH)))))
| VPADD (_, _) -> Coq_xO (Coq_xO (Coq_xO (Coq_xI (Coq_xO (Coq_xO Coq_xH)))))
| VPSUB (_, _) -> Coq_xI (Coq_xO (Coq_xO (Coq_xI (Coq_xO (Coq_xO Coq_xH)))))
| VPAVG (_, _) -> Coq_xO (Coq_xI (Coq_xO (Coq_xI (Coq_xO (Coq_xO Coq_xH)))))
| VPMULL (_, _) -> Coq_xI (Coq_xI (Coq_xO (Coq_xI (Coq_xO (Coq_xO Coq_xH)))))
| VPMULH _ -> Coq_xO (Coq_xO (Coq_xI (Coq_xI (Coq_xO (Coq_xO Coq_xH)))))
| VPMULHU _ -> Coq_xI (Coq_xO (Coq_xI (Coq_xI (Coq_xO (Coq_xO Coq_xH)))))
| VPMULHRS _ -> Coq_xO (Coq_xI (Coq_xI (Coq_xI (Coq_xO (Coq_xO Coq_xH)))))
| VPMUL _ -> Coq_xI (Coq_xI (Coq_xI (Coq_xI (Coq_xO (Coq_xO Coq_xH)))))
| VPMULU _ -> Coq_xO (Coq_xO (Coq_xO (Coq_xO (Coq_xI (Coq_xO Coq_xH)))))
| VPEXTR _ -> Coq_xI (Coq_xO (Coq_xO (Coq_xO (Coq_xI (Coq_xO Coq_xH)))))
| VPINSR _ -> Coq_xO (Coq_xI (Coq_xO (Coq_xO (Coq_xI (Coq_xO Coq_xH)))))
| VPSLL (_, _) -> Coq_xI (Coq_xI (Coq_xO (Coq_xO (Coq_xI (Coq_xO Coq_xH)))))
| VPSRL (_, _) -> Coq_xO (Coq_xO (Coq_xI (Coq_xO (Coq_xI (Coq_xO Coq_xH)))))
| VPSRA (_, _) -> Coq_xI (Coq_xO (Coq_xI (Coq_xO (Coq_xI (Coq_xO Coq_xH)))))
| VPSLLV (_, _) -> Coq_xO (Coq_xI (Coq_xI (Coq_xO (Coq_xI (Coq_xO Coq_xH)))))
| VPSRLV (_, _) -> Coq_xI (Coq_xI (Coq_xI (Coq_xO (Coq_xI (Coq_xO Coq_xH)))))
| VPSLLDQ _ -> Coq_xO (Coq_xO (Coq_xO (Coq_xI (Coq_xI (Coq_xO Coq_xH)))))
| VPSRLDQ _ -> Coq_xI (Coq_xO (Coq_xO (Coq_xI (Coq_xI (Coq_xO Coq_xH)))))
| VPSHUFB _ -> Coq_xO (Coq_xI (Coq_xO (Coq_xI (Coq_xI (Coq_xO Coq_xH)))))
| VPSHUFD _ -> Coq_xI (Coq_xI (Coq_xO (Coq_xI (Coq_xI (Coq_xO Coq_xH)))))
| VPSHUFHW _ -> Coq_xO (Coq_xO (Coq_xI (Coq_xI (Coq_xI (Coq_xO Coq_xH)))))
| VPSHUFLW _ -> Coq_xI (Coq_xO (Coq_xI (Coq_xI (Coq_xI (Coq_xO Coq_xH)))))
| VPBLEND (_, _) -> Coq_xO (Coq_xI (Coq_xI (Coq_xI (Coq_xI (Coq_xO Coq_xH)))))
| BLENDV (_, _) -> Coq_xI (Coq_xI (Coq_xI (Coq_xI (Coq_xI (Coq_xO Coq_xH)))))
| VPACKUS (_, _) -> Coq_xO (Coq_xO (Coq_xO (Coq_xO (Coq_xO (Coq_xI Coq_xH)))))
| VPACKSS (_, _) -> Coq_xI (Coq_xO (Coq_xO (Coq_xO (Coq_xO (Coq_xI Coq_xH)))))
| VSHUFPS _ -> Coq_xO (Coq_xI (Coq_xO (Coq_xO (Coq_xO (Coq_xI Coq_xH)))))
| VPBROADCAST (_, _) ->
  Coq_xI (Coq_xI (Coq_xO (Coq_xO (Coq_xO (Coq_xI Coq_xH)))))
| VMOVSHDUP _ -> Coq_xO (Coq_xO (Coq_xI (Coq_xO (Coq_xO (Coq_xI Coq_xH)))))
| VMOVSLDUP _ -> Coq_xI (Coq_xO (Coq_xI (Coq_xO (Coq_xO (Coq_xI Coq_xH)))))
| VPALIGNR _ -> Coq_xO (Coq_xI (Coq_xI (Coq_xO (Coq_xO (Coq_xI Coq_xH)))))
| VBROADCASTI128 -> Coq_xI (Coq_xI (Coq_xI (Coq_xO (Coq_xO (Coq_xI Coq_xH)))))
| VPUNPCKH (_, _) ->
  Coq_xO (Coq_xO (Coq_xO (Coq_xI (Coq_xO (Coq_xI Coq_xH)))))
| VPUNPCKL (_, _) ->
  Coq_xI (Coq_xO (Coq_xO (Coq_xI (Coq_xO (Coq_xI Coq_xH)))))
| VEXTRACTI128 -> Coq_xO (Coq_xI (Coq_xO (Coq_xI (Coq_xO (Coq_xI Coq_xH)))))
| VINSERTI128 -> Coq_xI (Coq_xI (Coq_xO (Coq_xI (Coq_xO (Coq_xI Coq_xH)))))
| VPERM2I128 -> Coq_xO (Coq_xO (Coq_xI (Coq_xI (Coq_xO (Coq_xI Coq_xH)))))
| VPERMD -> Coq_xI (Coq_xO (Coq_xI (Coq_xI (Coq_xO (Coq_xI Coq_xH)))))
| VPERMQ -> Coq_xO (Coq_xI (Coq_xI (Coq_xI (Coq_xO (Coq_xI Coq_xH)))))
| MOVEMASK (_, _) ->
  Coq_xI (Coq_xI (Coq_xI (Coq_xI (Coq_xO (Coq_xI Coq_xH)))))
| VPCMPEQ (_, _) -> Coq_xO (Coq_xO (Coq_xO (Coq_xO (Coq_xI (Coq_xI Coq_xH)))))
| VPCMPGT (_, _) -> Coq_xI (Coq_xO (Coq_xO (Coq_xO (Coq_xI (Coq_xI Coq_xH)))))
| VPSIGN (_, _) -> Coq_xO (Coq_xI (Coq_xO (Coq_xO (Coq_xI (Coq_xI Coq_xH)))))
| VPMADDUBSW _ -> Coq_xI (Coq_xI (Coq_xO (Coq_xO (Coq_xI (Coq_xI Coq_xH)))))
| VPMADDWD _ -> Coq_xO (Coq_xO (Coq_xI (Coq_xO (Coq_xI (Coq_xI Coq_xH)))))
| VMOVLPD -> Coq_xI (Coq_xO (Coq_xI (Coq_xO (Coq_xI (Coq_xI Coq_xH)))))
| VMOVHPD -> Coq_xO (Coq_xI (Coq_xI (Coq_xO (Coq_xI (Coq_xI Coq_xH)))))
| VPMINU (_, _) -> Coq_xI (Coq_xI (Coq_xI (Coq_xO (Coq_xI (Coq_xI Coq_xH)))))
| VPMINS (_, _) -> Coq_xO (Coq_xO (Coq_xO (Coq_xI (Coq_xI (Coq_xI Coq_xH)))))
| VPMAXU (_, _) -> Coq_xI (Coq_xO (Coq_xO (Coq_xI (Coq_xI (Coq_xI Coq_xH)))))
| VPMAXS (_, _) -> Coq_xO (Coq_xI (Coq_xO (Coq_xI (Coq_xI (Coq_xI Coq_xH)))))
| VPABS (_, _) -> Coq_xI (Coq_xI (Coq_xO (Coq_xI (Coq_xI (Coq_xI Coq_xH)))))
| VPTEST _ -> Coq_xO (Coq_xO (Coq_xI (Coq_xI (Coq_xI (Coq_xI Coq_xH)))))
| CLFLUSH -> Coq_xI (Coq_xO (Coq_xI (Coq_xI (Coq_xI (Coq_xI Coq_xH)))))
| PREFETCHT0 -> Coq_xO (Coq_xI (Coq_xI (Coq_xI (Coq_xI (Coq_xI Coq_xH)))))
| PREFETCHT1 -> Coq_xI (Coq_xI (Coq_xI (Coq_xI (Coq_xI (Coq_xI Coq_xH)))))
| PREFETCHT2 ->
  Coq_xO (Coq_xO (Coq_xO (Coq_xO (Coq_xO (Coq_xO (Coq_xO Coq_xH))))))
| PREFETCHNTA ->
  Coq_xI (Coq_xO (Coq_xO (Coq_xO (Coq_xO (Coq_xO (Coq_xO Coq_xH))))))
| LFENCE ->
  Coq_xO (Coq_xI (Coq_xO (Coq_xO (Coq_xO (Coq_xO (Coq_xO Coq_xH))))))
| MFENCE ->
  Coq_xI (Coq_xI (Coq_xO (Coq_xO (Coq_xO (Coq_xO (Coq_xO Coq_xH))))))
| SFENCE ->
  Coq_xO (Coq_xO (Coq_xI (Coq_xO (Coq_xO (Coq_xO (Coq_xO Coq_xH))))))
| RDTSC _ ->
  Coq_xI (Coq_xO (Coq_xI (Coq_xO (Coq_xO (Coq_xO (Coq_xO Coq_xH))))))
| RDTSCP _ ->
  Coq_xO (Coq_xI (Coq_xI (Coq_xO (Coq_xO (Coq_xO (Coq_xO Coq_xH))))))
| AESDEC ->
  Coq_xI (Coq_xI (Coq_xI (Coq_xO (Coq_xO (Coq_xO (Coq_xO Coq_xH))))))
| VAESDEC _ ->
  Coq_xO (Coq_xO (Coq_xO (Coq_xI (Coq_xO (Coq_xO (Coq_xO Coq_xH))))))
| AESDECLAST ->
  Coq_xI (Coq_xO (Coq_xO (Coq_xI (Coq_xO (Coq_xO (Coq_xO Coq_xH))))))
| VAESDECLAST _ ->
  Coq_xO (Coq_xI (Coq_xO (Coq_xI (Coq_xO (Coq_xO (Coq_xO Coq_xH))))))
| AESENC ->
  Coq_xI (Coq_xI (Coq_xO (Coq_xI (Coq_xO (Coq_xO (Coq_xO Coq_xH))))))
| VAESENC _ ->
  Coq_xO (Coq_xO (Coq_xI (Coq_xI (Coq_xO (Coq_xO (Coq_xO Coq_xH))))))
| AESENCLAST ->
  Coq_xI (Coq_xO (Coq_xI (Coq_xI (Coq_xO (Coq_xO (Coq_xO Coq_xH))))))
| VAESENCLAST _ ->
  Coq_xO (Coq_xI (Coq_xI (Coq_xI (Coq_xO (Coq_xO (Coq_xO Coq_xH))))))
| AESIMC ->
  Coq_xI (Coq_xI (Coq_xI (Coq_xI (Coq_xO (Coq_xO (Coq_xO Coq_xH))))))
| VAESIMC ->
  Coq_xO (Coq_xO (Coq_xO (Coq_xO (Coq_xI (Coq_xO (Coq_xO Coq_xH))))))
| AESKEYGENASSIST ->
  Coq_xI (Coq_xO (Coq_xO (Coq_xO (Coq_xI (Coq_xO (Coq_xO Coq_xH))))))
| VAESKEYGENASSIST ->
  Coq_xO (Coq_xI (Coq_xO (Coq_xO (Coq_xI (Coq_xO (Coq_xO Coq_xH))))))
| PCLMULQDQ ->
  Coq_xI (Coq_xI (Coq_xO (Coq_xO (Coq_xI (Coq_xO (Coq_xO Coq_xH))))))
| VPCLMULQDQ _ ->
  Coq_xO (Coq_xO (Coq_xI (Coq_xO (Coq_xI (Coq_xO (Coq_xO Coq_xH))))))
| SHA256RNDS2 ->
  Coq_xI (Coq_xO (Coq_xI (Coq_xO (Coq_xI (Coq_xO (Coq_xO Coq_xH))))))
| SHA256MSG1 ->
  Coq_xO (Coq_xI (Coq_xI (Coq_xO (Coq_xI (Coq_xO (Coq_xO Coq_xH))))))
| SHA256MSG2 ->
  Coq_xI (Coq_xI (Coq_xI (Coq_xO (Coq_xI (Coq_xO (Coq_xO Coq_xH))))))

(** val is_x86_op_inhab : x86_op -> is_x86_op **)

let is_x86_op_inhab = function
| MOV h -> Coq_is_MOV (h, (is_wsize_inhab h))
| MOVSX (h, h0) ->
  Coq_is_MOVSX (h, (is_wsize_inhab h), h0, (is_wsize_inhab h0))
| MOVZX (h, h0) ->
  Coq_is_MOVZX (h, (is_wsize_inhab h), h0, (is_wsize_inhab h0))
| CMOVcc h -> Coq_is_CMOVcc (h, (is_wsize_inhab h))
| XCHG h -> Coq_is_XCHG (h, (is_wsize_inhab h))
| ADD h -> Coq_is_ADD (h, (is_wsize_inhab h))
| SUB h -> Coq_is_SUB (h, (is_wsize_inhab h))
| MUL h -> Coq_is_MUL (h, (is_wsize_inhab h))
| IMUL h -> Coq_is_IMUL (h, (is_wsize_inhab h))
| IMULr h -> Coq_is_IMULr (h, (is_wsize_inhab h))
| IMULri h -> Coq_is_IMULri (h, (is_wsize_inhab h))
| DIV h -> Coq_is_DIV (h, (is_wsize_inhab h))
| IDIV h -> Coq_is_IDIV (h, (is_wsize_inhab h))
| CQO h -> Coq_is_CQO (h, (is_wsize_inhab h))
| ADC h -> Coq_is_ADC (h, (is_wsize_inhab h))
| SBB h -> Coq_is_SBB (h, (is_wsize_inhab h))
| NEG h -> Coq_is_NEG (h, (is_wsize_inhab h))
| INC h -> Coq_is_INC (h, (is_wsize_inhab h))
| DEC h -> Coq_is_DEC (h, (is_wsize_inhab h))
| LZCNT h -> Coq_is_LZCNT (h, (is_wsize_inhab h))
| TZCNT h -> Coq_is_TZCNT (h, (is_wsize_inhab h))
| BSR h -> Coq_is_BSR (h, (is_wsize_inhab h))
| SETcc -> Coq_is_SETcc
| BT h -> Coq_is_BT (h, (is_wsize_inhab h))
| CLC -> Coq_is_CLC
| STC -> Coq_is_STC
| LEA h -> Coq_is_LEA (h, (is_wsize_inhab h))
| TEST h -> Coq_is_TEST (h, (is_wsize_inhab h))
| CMP h -> Coq_is_CMP (h, (is_wsize_inhab h))
| AND h -> Coq_is_AND (h, (is_wsize_inhab h))
| ANDN h -> Coq_is_ANDN (h, (is_wsize_inhab h))
| OR h -> Coq_is_OR (h, (is_wsize_inhab h))
| XOR h -> Coq_is_XOR (h, (is_wsize_inhab h))
| NOT h -> Coq_is_NOT (h, (is_wsize_inhab h))
| ROR h -> Coq_is_ROR (h, (is_wsize_inhab h))
| ROL h -> Coq_is_ROL (h, (is_wsize_inhab h))
| RCR h -> Coq_is_RCR (h, (is_wsize_inhab h))
| RCL h -> Coq_is_RCL (h, (is_wsize_inhab h))
| SHL h -> Coq_is_SHL (h, (is_wsize_inhab h))
| SHR h -> Coq_is_SHR (h, (is_wsize_inhab h))
| SAL h -> Coq_is_SAL (h, (is_wsize_inhab h))
| SAR h -> Coq_is_SAR (h, (is_wsize_inhab h))
| SHLD h -> Coq_is_SHLD (h, (is_wsize_inhab h))
| SHRD h -> Coq_is_SHRD (h, (is_wsize_inhab h))
| RORX h -> Coq_is_RORX (h, (is_wsize_inhab h))
| SARX h -> Coq_is_SARX (h, (is_wsize_inhab h))
| SHRX h -> Coq_is_SHRX (h, (is_wsize_inhab h))
| SHLX h -> Coq_is_SHLX (h, (is_wsize_inhab h))
| MULX_lo_hi h -> Coq_is_MULX_lo_hi (h, (is_wsize_inhab h))
| ADCX h -> Coq_is_ADCX (h, (is_wsize_inhab h))
| ADOX h -> Coq_is_ADOX (h, (is_wsize_inhab h))
| BSWAP h -> Coq_is_BSWAP (h, (is_wsize_inhab h))
| POPCNT h -> Coq_is_POPCNT (h, (is_wsize_inhab h))
| BTR h -> Coq_is_BTR (h, (is_wsize_inhab h))
| BTS h -> Coq_is_BTS (h, (is_wsize_inhab h))
| PEXT h -> Coq_is_PEXT (h, (is_wsize_inhab h))
| PDEP h -> Coq_is_PDEP (h, (is_wsize_inhab h))
| MOVX h -> Coq_is_MOVX (h, (is_wsize_inhab h))
| POR -> Coq_is_POR
| PADD (h, h0) -> Coq_is_PADD (h, (is_velem_inhab h), h0, (is_wsize_inhab h0))
| MOVD h -> Coq_is_MOVD (h, (is_wsize_inhab h))
| MOVV h -> Coq_is_MOVV (h, (is_wsize_inhab h))
| VMOV h -> Coq_is_VMOV (h, (is_wsize_inhab h))
| VMOVDQA h -> Coq_is_VMOVDQA (h, (is_wsize_inhab h))
| VMOVDQU h -> Coq_is_VMOVDQU (h, (is_wsize_inhab h))
| VPMOVSX (h, h0, h1, h2) ->
  Coq_is_VPMOVSX (h, (is_velem_inhab h), h0, (is_wsize_inhab h0), h1,
    (is_velem_inhab h1), h2, (is_wsize_inhab h2))
| VPMOVZX (h, h0, h1, h2) ->
  Coq_is_VPMOVZX (h, (is_velem_inhab h), h0, (is_wsize_inhab h0), h1,
    (is_velem_inhab h1), h2, (is_wsize_inhab h2))
| VPAND h -> Coq_is_VPAND (h, (is_wsize_inhab h))
| VPANDN h -> Coq_is_VPANDN (h, (is_wsize_inhab h))
| VPOR h -> Coq_is_VPOR (h, (is_wsize_inhab h))
| VPXOR h -> Coq_is_VPXOR (h, (is_wsize_inhab h))
| VPADD (h, h0) ->
  Coq_is_VPADD (h, (is_velem_inhab h), h0, (is_wsize_inhab h0))
| VPSUB (h, h0) ->
  Coq_is_VPSUB (h, (is_velem_inhab h), h0, (is_wsize_inhab h0))
| VPAVG (h, h0) ->
  Coq_is_VPAVG (h, (is_velem_inhab h), h0, (is_wsize_inhab h0))
| VPMULL (h, h0) ->
  Coq_is_VPMULL (h, (is_velem_inhab h), h0, (is_wsize_inhab h0))
| VPMULH h -> Coq_is_VPMULH (h, (is_wsize_inhab h))
| VPMULHU h -> Coq_is_VPMULHU (h, (is_wsize_inhab h))
| VPMULHRS h -> Coq_is_VPMULHRS (h, (is_wsize_inhab h))
| VPMUL h -> Coq_is_VPMUL (h, (is_wsize_inhab h))
| VPMULU h -> Coq_is_VPMULU (h, (is_wsize_inhab h))
| VPEXTR h -> Coq_is_VPEXTR (h, (is_wsize_inhab h))
| VPINSR h -> Coq_is_VPINSR (h, (is_velem_inhab h))
| VPSLL (h, h0) ->
  Coq_is_VPSLL (h, (is_velem_inhab h), h0, (is_wsize_inhab h0))
| VPSRL (h, h0) ->
  Coq_is_VPSRL (h, (is_velem_inhab h), h0, (is_wsize_inhab h0))
| VPSRA (h, h0) ->
  Coq_is_VPSRA (h, (is_velem_inhab h), h0, (is_wsize_inhab h0))
| VPSLLV (h, h0) ->
  Coq_is_VPSLLV (h, (is_velem_inhab h), h0, (is_wsize_inhab h0))
| VPSRLV (h, h0) ->
  Coq_is_VPSRLV (h, (is_velem_inhab h), h0, (is_wsize_inhab h0))
| VPSLLDQ h -> Coq_is_VPSLLDQ (h, (is_wsize_inhab h))
| VPSRLDQ h -> Coq_is_VPSRLDQ (h, (is_wsize_inhab h))
| VPSHUFB h -> Coq_is_VPSHUFB (h, (is_wsize_inhab h))
| VPSHUFD h -> Coq_is_VPSHUFD (h, (is_wsize_inhab h))
| VPSHUFHW h -> Coq_is_VPSHUFHW (h, (is_wsize_inhab h))
| VPSHUFLW h -> Coq_is_VPSHUFLW (h, (is_wsize_inhab h))
| VPBLEND (h, h0) ->
  Coq_is_VPBLEND (h, (is_velem_inhab h), h0, (is_wsize_inhab h0))
| BLENDV (h, h0) ->
  Coq_is_BLENDV (h, (is_velem_inhab h), h0, (is_wsize_inhab h0))
| VPACKUS (h, h0) ->
  Coq_is_VPACKUS (h, (is_velem_inhab h), h0, (is_wsize_inhab h0))
| VPACKSS (h, h0) ->
  Coq_is_VPACKSS (h, (is_velem_inhab h), h0, (is_wsize_inhab h0))
| VSHUFPS h -> Coq_is_VSHUFPS (h, (is_wsize_inhab h))
| VPBROADCAST (h, h0) ->
  Coq_is_VPBROADCAST (h, (is_velem_inhab h), h0, (is_wsize_inhab h0))
| VMOVSHDUP h -> Coq_is_VMOVSHDUP (h, (is_wsize_inhab h))
| VMOVSLDUP h -> Coq_is_VMOVSLDUP (h, (is_wsize_inhab h))
| VPALIGNR h -> Coq_is_VPALIGNR (h, (is_wsize_inhab h))
| VBROADCASTI128 -> Coq_is_VBROADCASTI128
| VPUNPCKH (h, h0) ->
  Coq_is_VPUNPCKH (h, (is_velem_inhab h), h0, (is_wsize_inhab h0))
| VPUNPCKL (h, h0) ->
  Coq_is_VPUNPCKL (h, (is_velem_inhab h), h0, (is_wsize_inhab h0))
| VEXTRACTI128 -> Coq_is_VEXTRACTI128
| VINSERTI128 -> Coq_is_VINSERTI128
| VPERM2I128 -> Coq_is_VPERM2I128
| VPERMD -> Coq_is_VPERMD
| VPERMQ -> Coq_is_VPERMQ
| MOVEMASK (h, h0) ->
  Coq_is_MOVEMASK (h, (is_velem_inhab h), h0, (is_wsize_inhab h0))
| VPCMPEQ (h, h0) ->
  Coq_is_VPCMPEQ (h, (is_velem_inhab h), h0, (is_wsize_inhab h0))
| VPCMPGT (h, h0) ->
  Coq_is_VPCMPGT (h, (is_velem_inhab h), h0, (is_wsize_inhab h0))
| VPSIGN (h, h0) ->
  Coq_is_VPSIGN (h, (is_velem_inhab h), h0, (is_wsize_inhab h0))
| VPMADDUBSW h -> Coq_is_VPMADDUBSW (h, (is_wsize_inhab h))
| VPMADDWD h -> Coq_is_VPMADDWD (h, (is_wsize_inhab h))
| VMOVLPD -> Coq_is_VMOVLPD
| VMOVHPD -> Coq_is_VMOVHPD
| VPMINU (h, h0) ->
  Coq_is_VPMINU (h, (is_velem_inhab h), h0, (is_wsize_inhab h0))
| VPMINS (h, h0) ->
  Coq_is_VPMINS (h, (is_velem_inhab h), h0, (is_wsize_inhab h0))
| VPMAXU (h, h0) ->
  Coq_is_VPMAXU (h, (is_velem_inhab h), h0, (is_wsize_inhab h0))
| VPMAXS (h, h0) ->
  Coq_is_VPMAXS (h, (is_velem_inhab h), h0, (is_wsize_inhab h0))
| VPABS (h, h0) ->
  Coq_is_VPABS (h, (is_velem_inhab h), h0, (is_wsize_inhab h0))
| VPTEST h -> Coq_is_VPTEST (h, (is_wsize_inhab h))
| CLFLUSH -> Coq_is_CLFLUSH
| PREFETCHT0 -> Coq_is_PREFETCHT0
| PREFETCHT1 -> Coq_is_PREFETCHT1
| PREFETCHT2 -> Coq_is_PREFETCHT2
| PREFETCHNTA -> Coq_is_PREFETCHNTA
| LFENCE -> Coq_is_LFENCE
| MFENCE -> Coq_is_MFENCE
| SFENCE -> Coq_is_SFENCE
| RDTSC h -> Coq_is_RDTSC (h, (is_wsize_inhab h))
| RDTSCP h -> Coq_is_RDTSCP (h, (is_wsize_inhab h))
| AESDEC -> Coq_is_AESDEC
| VAESDEC h -> Coq_is_VAESDEC (h, (is_wsize_inhab h))
| AESDECLAST -> Coq_is_AESDECLAST
| VAESDECLAST h -> Coq_is_VAESDECLAST (h, (is_wsize_inhab h))
| AESENC -> Coq_is_AESENC
| VAESENC h -> Coq_is_VAESENC (h, (is_wsize_inhab h))
| AESENCLAST -> Coq_is_AESENCLAST
| VAESENCLAST h -> Coq_is_VAESENCLAST (h, (is_wsize_inhab h))
| AESIMC -> Coq_is_AESIMC
| VAESIMC -> Coq_is_VAESIMC
| AESKEYGENASSIST -> Coq_is_AESKEYGENASSIST
| VAESKEYGENASSIST -> Coq_is_VAESKEYGENASSIST
| PCLMULQDQ -> Coq_is_PCLMULQDQ
| VPCLMULQDQ h -> Coq_is_VPCLMULQDQ (h, (is_wsize_inhab h))
| SHA256RNDS2 -> Coq_is_SHA256RNDS2
| SHA256MSG1 -> Coq_is_SHA256MSG1
| SHA256MSG2 -> Coq_is_SHA256MSG2

(** val is_x86_op_functor : x86_op -> is_x86_op -> is_x86_op **)

let rec is_x86_op_functor _ x =
  x

type box_x86_op_MOV =
  wsize
  (* singleton inductive, whose constructor was Box_x86_op_MOV *)

(** val coq_Box_x86_op_MOV_0 : box_x86_op_MOV -> wsize **)

let coq_Box_x86_op_MOV_0 record =
  record

type box_x86_op_MOVSX = { coq_Box_x86_op_MOVSX_0 : wsize;
                          coq_Box_x86_op_MOVSX_1 : wsize }

(** val coq_Box_x86_op_MOVSX_0 : box_x86_op_MOVSX -> wsize **)

let coq_Box_x86_op_MOVSX_0 record =
  record.coq_Box_x86_op_MOVSX_0

(** val coq_Box_x86_op_MOVSX_1 : box_x86_op_MOVSX -> wsize **)

let coq_Box_x86_op_MOVSX_1 record =
  record.coq_Box_x86_op_MOVSX_1

type box_x86_op_SETcc =
| Box_x86_op_SETcc

type box_x86_op_PADD = { coq_Box_x86_op_PADD_0 : velem;
                         coq_Box_x86_op_PADD_1 : wsize }

(** val coq_Box_x86_op_PADD_0 : box_x86_op_PADD -> velem **)

let coq_Box_x86_op_PADD_0 record =
  record.coq_Box_x86_op_PADD_0

(** val coq_Box_x86_op_PADD_1 : box_x86_op_PADD -> wsize **)

let coq_Box_x86_op_PADD_1 record =
  record.coq_Box_x86_op_PADD_1

type box_x86_op_VPMOVSX = { coq_Box_x86_op_VPMOVSX_0 : velem;
                            coq_Box_x86_op_VPMOVSX_1 : wsize;
                            coq_Box_x86_op_VPMOVSX_2 : velem;
                            coq_Box_x86_op_VPMOVSX_3 : wsize }

(** val coq_Box_x86_op_VPMOVSX_0 : box_x86_op_VPMOVSX -> velem **)

let coq_Box_x86_op_VPMOVSX_0 record =
  record.coq_Box_x86_op_VPMOVSX_0

(** val coq_Box_x86_op_VPMOVSX_1 : box_x86_op_VPMOVSX -> wsize **)

let coq_Box_x86_op_VPMOVSX_1 record =
  record.coq_Box_x86_op_VPMOVSX_1

(** val coq_Box_x86_op_VPMOVSX_2 : box_x86_op_VPMOVSX -> velem **)

let coq_Box_x86_op_VPMOVSX_2 record =
  record.coq_Box_x86_op_VPMOVSX_2

(** val coq_Box_x86_op_VPMOVSX_3 : box_x86_op_VPMOVSX -> wsize **)

let coq_Box_x86_op_VPMOVSX_3 record =
  record.coq_Box_x86_op_VPMOVSX_3

type box_x86_op_VPINSR =
  velem
  (* singleton inductive, whose constructor was Box_x86_op_VPINSR *)

(** val coq_Box_x86_op_VPINSR_0 : box_x86_op_VPINSR -> velem **)

let coq_Box_x86_op_VPINSR_0 record =
  record

type x86_op_fields_t = __

(** val x86_op_fields : x86_op -> x86_op_fields_t **)

let x86_op_fields = function
| MOV h -> Obj.magic h
| MOVSX (h, h0) ->
  Obj.magic { coq_Box_x86_op_MOVSX_0 = h; coq_Box_x86_op_MOVSX_1 = h0 }
| MOVZX (h, h0) ->
  Obj.magic { coq_Box_x86_op_MOVSX_0 = h; coq_Box_x86_op_MOVSX_1 = h0 }
| CMOVcc h -> Obj.magic h
| XCHG h -> Obj.magic h
| ADD h -> Obj.magic h
| SUB h -> Obj.magic h
| MUL h -> Obj.magic h
| IMUL h -> Obj.magic h
| IMULr h -> Obj.magic h
| IMULri h -> Obj.magic h
| DIV h -> Obj.magic h
| IDIV h -> Obj.magic h
| CQO h -> Obj.magic h
| ADC h -> Obj.magic h
| SBB h -> Obj.magic h
| NEG h -> Obj.magic h
| INC h -> Obj.magic h
| DEC h -> Obj.magic h
| LZCNT h -> Obj.magic h
| TZCNT h -> Obj.magic h
| BSR h -> Obj.magic h
| BT h -> Obj.magic h
| LEA h -> Obj.magic h
| TEST h -> Obj.magic h
| CMP h -> Obj.magic h
| AND h -> Obj.magic h
| ANDN h -> Obj.magic h
| OR h -> Obj.magic h
| XOR h -> Obj.magic h
| NOT h -> Obj.magic h
| ROR h -> Obj.magic h
| ROL h -> Obj.magic h
| RCR h -> Obj.magic h
| RCL h -> Obj.magic h
| SHL h -> Obj.magic h
| SHR h -> Obj.magic h
| SAL h -> Obj.magic h
| SAR h -> Obj.magic h
| SHLD h -> Obj.magic h
| SHRD h -> Obj.magic h
| RORX h -> Obj.magic h
| SARX h -> Obj.magic h
| SHRX h -> Obj.magic h
| SHLX h -> Obj.magic h
| MULX_lo_hi h -> Obj.magic h
| ADCX h -> Obj.magic h
| ADOX h -> Obj.magic h
| BSWAP h -> Obj.magic h
| POPCNT h -> Obj.magic h
| BTR h -> Obj.magic h
| BTS h -> Obj.magic h
| PEXT h -> Obj.magic h
| PDEP h -> Obj.magic h
| MOVX h -> Obj.magic h
| PADD (h, h0) ->
  Obj.magic { coq_Box_x86_op_PADD_0 = h; coq_Box_x86_op_PADD_1 = h0 }
| MOVD h -> Obj.magic h
| MOVV h -> Obj.magic h
| VMOV h -> Obj.magic h
| VMOVDQA h -> Obj.magic h
| VMOVDQU h -> Obj.magic h
| VPMOVSX (h, h0, h1, h2) ->
  Obj.magic { coq_Box_x86_op_VPMOVSX_0 = h; coq_Box_x86_op_VPMOVSX_1 = h0;
    coq_Box_x86_op_VPMOVSX_2 = h1; coq_Box_x86_op_VPMOVSX_3 = h2 }
| VPMOVZX (h, h0, h1, h2) ->
  Obj.magic { coq_Box_x86_op_VPMOVSX_0 = h; coq_Box_x86_op_VPMOVSX_1 = h0;
    coq_Box_x86_op_VPMOVSX_2 = h1; coq_Box_x86_op_VPMOVSX_3 = h2 }
| VPAND h -> Obj.magic h
| VPANDN h -> Obj.magic h
| VPOR h -> Obj.magic h
| VPXOR h -> Obj.magic h
| VPADD (h, h0) ->
  Obj.magic { coq_Box_x86_op_PADD_0 = h; coq_Box_x86_op_PADD_1 = h0 }
| VPSUB (h, h0) ->
  Obj.magic { coq_Box_x86_op_PADD_0 = h; coq_Box_x86_op_PADD_1 = h0 }
| VPAVG (h, h0) ->
  Obj.magic { coq_Box_x86_op_PADD_0 = h; coq_Box_x86_op_PADD_1 = h0 }
| VPMULL (h, h0) ->
  Obj.magic { coq_Box_x86_op_PADD_0 = h; coq_Box_x86_op_PADD_1 = h0 }
| VPMULH h -> Obj.magic h
| VPMULHU h -> Obj.magic h
| VPMULHRS h -> Obj.magic h
| VPMUL h -> Obj.magic h
| VPMULU h -> Obj.magic h
| VPEXTR h -> Obj.magic h
| VPINSR h -> Obj.magic h
| VPSLL (h, h0) ->
  Obj.magic { coq_Box_x86_op_PADD_0 = h; coq_Box_x86_op_PADD_1 = h0 }
| VPSRL (h, h0) ->
  Obj.magic { coq_Box_x86_op_PADD_0 = h; coq_Box_x86_op_PADD_1 = h0 }
| VPSRA (h, h0) ->
  Obj.magic { coq_Box_x86_op_PADD_0 = h; coq_Box_x86_op_PADD_1 = h0 }
| VPSLLV (h, h0) ->
  Obj.magic { coq_Box_x86_op_PADD_0 = h; coq_Box_x86_op_PADD_1 = h0 }
| VPSRLV (h, h0) ->
  Obj.magic { coq_Box_x86_op_PADD_0 = h; coq_Box_x86_op_PADD_1 = h0 }
| VPSLLDQ h -> Obj.magic h
| VPSRLDQ h -> Obj.magic h
| VPSHUFB h -> Obj.magic h
| VPSHUFD h -> Obj.magic h
| VPSHUFHW h -> Obj.magic h
| VPSHUFLW h -> Obj.magic h
| VPBLEND (h, h0) ->
  Obj.magic { coq_Box_x86_op_PADD_0 = h; coq_Box_x86_op_PADD_1 = h0 }
| BLENDV (h, h0) ->
  Obj.magic { coq_Box_x86_op_PADD_0 = h; coq_Box_x86_op_PADD_1 = h0 }
| VPACKUS (h, h0) ->
  Obj.magic { coq_Box_x86_op_PADD_0 = h; coq_Box_x86_op_PADD_1 = h0 }
| VPACKSS (h, h0) ->
  Obj.magic { coq_Box_x86_op_PADD_0 = h; coq_Box_x86_op_PADD_1 = h0 }
| VSHUFPS h -> Obj.magic h
| VPBROADCAST (h, h0) ->
  Obj.magic { coq_Box_x86_op_PADD_0 = h; coq_Box_x86_op_PADD_1 = h0 }
| VMOVSHDUP h -> Obj.magic h
| VMOVSLDUP h -> Obj.magic h
| VPALIGNR h -> Obj.magic h
| VPUNPCKH (h, h0) ->
  Obj.magic { coq_Box_x86_op_PADD_0 = h; coq_Box_x86_op_PADD_1 = h0 }
| VPUNPCKL (h, h0) ->
  Obj.magic { coq_Box_x86_op_PADD_0 = h; coq_Box_x86_op_PADD_1 = h0 }
| MOVEMASK (h, h0) ->
  Obj.magic { coq_Box_x86_op_PADD_0 = h; coq_Box_x86_op_PADD_1 = h0 }
| VPCMPEQ (h, h0) ->
  Obj.magic { coq_Box_x86_op_PADD_0 = h; coq_Box_x86_op_PADD_1 = h0 }
| VPCMPGT (h, h0) ->
  Obj.magic { coq_Box_x86_op_PADD_0 = h; coq_Box_x86_op_PADD_1 = h0 }
| VPSIGN (h, h0) ->
  Obj.magic { coq_Box_x86_op_PADD_0 = h; coq_Box_x86_op_PADD_1 = h0 }
| VPMADDUBSW h -> Obj.magic h
| VPMADDWD h -> Obj.magic h
| VPMINU (h, h0) ->
  Obj.magic { coq_Box_x86_op_PADD_0 = h; coq_Box_x86_op_PADD_1 = h0 }
| VPMINS (h, h0) ->
  Obj.magic { coq_Box_x86_op_PADD_0 = h; coq_Box_x86_op_PADD_1 = h0 }
| VPMAXU (h, h0) ->
  Obj.magic { coq_Box_x86_op_PADD_0 = h; coq_Box_x86_op_PADD_1 = h0 }
| VPMAXS (h, h0) ->
  Obj.magic { coq_Box_x86_op_PADD_0 = h; coq_Box_x86_op_PADD_1 = h0 }
| VPABS (h, h0) ->
  Obj.magic { coq_Box_x86_op_PADD_0 = h; coq_Box_x86_op_PADD_1 = h0 }
| VPTEST h -> Obj.magic h
| RDTSC h -> Obj.magic h
| RDTSCP h -> Obj.magic h
| VAESDEC h -> Obj.magic h
| VAESDECLAST h -> Obj.magic h
| VAESENC h -> Obj.magic h
| VAESENCLAST h -> Obj.magic h
| VPCLMULQDQ h -> Obj.magic h
| _ -> Obj.magic Box_x86_op_SETcc

(** val x86_op_construct : positive -> x86_op_fields_t -> x86_op option **)

let x86_op_construct p b =
  match p with
  | Coq_xI x ->
    (match x with
     | Coq_xI x0 ->
       (match x0 with
        | Coq_xI x1 ->
          (match x1 with
           | Coq_xI x2 ->
             (match x2 with
              | Coq_xI x3 ->
                (match x3 with
                 | Coq_xI _ -> Some PREFETCHT1
                 | Coq_xO _ ->
                   let { coq_Box_x86_op_PADD_0 = box_x86_op_PADD_0;
                     coq_Box_x86_op_PADD_1 = box_x86_op_PADD_1 } = Obj.magic b
                   in
                   Some (BLENDV (box_x86_op_PADD_0, box_x86_op_PADD_1))
                 | Coq_xH -> Some (VMOV (Obj.magic b)))
              | Coq_xO x3 ->
                (match x3 with
                 | Coq_xI _ ->
                   let { coq_Box_x86_op_PADD_0 = box_x86_op_PADD_0;
                     coq_Box_x86_op_PADD_1 = box_x86_op_PADD_1 } = Obj.magic b
                   in
                   Some (MOVEMASK (box_x86_op_PADD_0, box_x86_op_PADD_1))
                 | Coq_xO x4 ->
                   (match x4 with
                    | Coq_xI _ -> None
                    | Coq_xO _ -> Some AESIMC
                    | Coq_xH -> Some (VPMUL (Obj.magic b)))
                 | Coq_xH -> Some (SHRX (Obj.magic b)))
              | Coq_xH -> Some (ANDN (Obj.magic b)))
           | Coq_xO x2 ->
             (match x2 with
              | Coq_xI x3 ->
                (match x3 with
                 | Coq_xI _ ->
                   let { coq_Box_x86_op_PADD_0 = box_x86_op_PADD_0;
                     coq_Box_x86_op_PADD_1 = box_x86_op_PADD_1 } = Obj.magic b
                   in
                   Some (VPMINU (box_x86_op_PADD_0, box_x86_op_PADD_1))
                 | Coq_xO x4 ->
                   (match x4 with
                    | Coq_xI _ -> None
                    | Coq_xO _ -> Some SHA256MSG2
                    | Coq_xH ->
                      let { coq_Box_x86_op_PADD_0 = box_x86_op_PADD_0;
                        coq_Box_x86_op_PADD_1 = box_x86_op_PADD_1 } =
                        Obj.magic b
                      in
                      Some (VPSRLV (box_x86_op_PADD_0, box_x86_op_PADD_1)))
                 | Coq_xH -> Some (BTS (Obj.magic b)))
              | Coq_xO x3 ->
                (match x3 with
                 | Coq_xI _ -> Some VBROADCASTI128
                 | Coq_xO x4 ->
                   (match x4 with
                    | Coq_xI _ -> None
                    | Coq_xO _ -> Some AESDEC
                    | Coq_xH -> Some (VPXOR (Obj.magic b)))
                 | Coq_xH -> Some (SHL (Obj.magic b)))
              | Coq_xH -> Some SETcc)
           | Coq_xH -> Some (ADC (Obj.magic b)))
        | Coq_xO x1 ->
          (match x1 with
           | Coq_xI x2 ->
             (match x2 with
              | Coq_xI x3 ->
                (match x3 with
                 | Coq_xI _ ->
                   let { coq_Box_x86_op_PADD_0 = box_x86_op_PADD_0;
                     coq_Box_x86_op_PADD_1 = box_x86_op_PADD_1 } = Obj.magic b
                   in
                   Some (VPABS (box_x86_op_PADD_0, box_x86_op_PADD_1))
                 | Coq_xO _ -> Some (VPSHUFD (Obj.magic b))
                 | Coq_xH -> Some POR)
              | Coq_xO x3 ->
                (match x3 with
                 | Coq_xI _ -> Some VINSERTI128
                 | Coq_xO x4 ->
                   (match x4 with
                    | Coq_xI _ -> None
                    | Coq_xO _ -> Some AESENC
                    | Coq_xH ->
                      let { coq_Box_x86_op_PADD_0 = box_x86_op_PADD_0;
                        coq_Box_x86_op_PADD_1 = box_x86_op_PADD_1 } =
                        Obj.magic b
                      in
                      Some (VPMULL (box_x86_op_PADD_0, box_x86_op_PADD_1)))
                 | Coq_xH -> Some (SHLD (Obj.magic b)))
              | Coq_xH -> Some (LEA (Obj.magic b)))
           | Coq_xO x2 ->
             (match x2 with
              | Coq_xI x3 ->
                (match x3 with
                 | Coq_xI _ -> Some (VPMADDUBSW (Obj.magic b))
                 | Coq_xO x4 ->
                   (match x4 with
                    | Coq_xI _ -> None
                    | Coq_xO _ -> Some PCLMULQDQ
                    | Coq_xH ->
                      let { coq_Box_x86_op_PADD_0 = box_x86_op_PADD_0;
                        coq_Box_x86_op_PADD_1 = box_x86_op_PADD_1 } =
                        Obj.magic b
                      in
                      Some (VPSLL (box_x86_op_PADD_0, box_x86_op_PADD_1)))
                 | Coq_xH -> Some (ADOX (Obj.magic b)))
              | Coq_xO x3 ->
                (match x3 with
                 | Coq_xI _ ->
                   let { coq_Box_x86_op_PADD_0 = box_x86_op_PADD_0;
                     coq_Box_x86_op_PADD_1 = box_x86_op_PADD_1 } = Obj.magic b
                   in
                   Some (VPBROADCAST (box_x86_op_PADD_0, box_x86_op_PADD_1))
                 | Coq_xO x4 ->
                   (match x4 with
                    | Coq_xI _ -> None
                    | Coq_xO _ -> Some MFENCE
                    | Coq_xH ->
                      let { coq_Box_x86_op_VPMOVSX_0 = box_x86_op_VPMOVSX_0;
                        coq_Box_x86_op_VPMOVSX_1 = box_x86_op_VPMOVSX_1;
                        coq_Box_x86_op_VPMOVSX_2 = box_x86_op_VPMOVSX_2;
                        coq_Box_x86_op_VPMOVSX_3 = box_x86_op_VPMOVSX_3 } =
                        Obj.magic b
                      in
                      Some (VPMOVZX (box_x86_op_VPMOVSX_0,
                      box_x86_op_VPMOVSX_1, box_x86_op_VPMOVSX_2,
                      box_x86_op_VPMOVSX_3)))
                 | Coq_xH -> Some (ROR (Obj.magic b)))
              | Coq_xH -> Some (DEC (Obj.magic b)))
           | Coq_xH -> Some (IMULri (Obj.magic b)))
        | Coq_xH -> Some (SUB (Obj.magic b)))
     | Coq_xO x0 ->
       (match x0 with
        | Coq_xI x1 ->
          (match x1 with
           | Coq_xI x2 ->
             (match x2 with
              | Coq_xI x3 ->
                (match x3 with
                 | Coq_xI _ -> Some CLFLUSH
                 | Coq_xO _ -> Some (VPSHUFLW (Obj.magic b))
                 | Coq_xH -> Some (MOVD (Obj.magic b)))
              | Coq_xO x3 ->
                (match x3 with
                 | Coq_xI _ -> Some VPERMD
                 | Coq_xO x4 ->
                   (match x4 with
                    | Coq_xI _ -> None
                    | Coq_xO _ -> Some AESENCLAST
                    | Coq_xH -> Some (VPMULHU (Obj.magic b)))
                 | Coq_xH -> Some (RORX (Obj.magic b)))
              | Coq_xH -> Some (CMP (Obj.magic b)))
           | Coq_xO x2 ->
             (match x2 with
              | Coq_xI x3 ->
                (match x3 with
                 | Coq_xI _ -> Some VMOVLPD
                 | Coq_xO x4 ->
                   (match x4 with
                    | Coq_xI _ -> None
                    | Coq_xO _ -> Some SHA256RNDS2
                    | Coq_xH ->
                      let { coq_Box_x86_op_PADD_0 = box_x86_op_PADD_0;
                        coq_Box_x86_op_PADD_1 = box_x86_op_PADD_1 } =
                        Obj.magic b
                      in
                      Some (VPSRA (box_x86_op_PADD_0, box_x86_op_PADD_1)))
                 | Coq_xH -> Some (POPCNT (Obj.magic b)))
              | Coq_xO x3 ->
                (match x3 with
                 | Coq_xI _ -> Some (VMOVSLDUP (Obj.magic b))
                 | Coq_xO x4 ->
                   (match x4 with
                    | Coq_xI _ -> None
                    | Coq_xO _ -> Some (RDTSC (Obj.magic b))
                    | Coq_xH -> Some (VPANDN (Obj.magic b)))
                 | Coq_xH -> Some (RCR (Obj.magic b)))
              | Coq_xH -> Some (TZCNT (Obj.magic b)))
           | Coq_xH -> Some (IDIV (Obj.magic b)))
        | Coq_xO x1 ->
          (match x1 with
           | Coq_xI x2 ->
             (match x2 with
              | Coq_xI x3 ->
                (match x3 with
                 | Coq_xI _ ->
                   let { coq_Box_x86_op_PADD_0 = box_x86_op_PADD_0;
                     coq_Box_x86_op_PADD_1 = box_x86_op_PADD_1 } = Obj.magic b
                   in
                   Some (VPMAXU (box_x86_op_PADD_0, box_x86_op_PADD_1))
                 | Coq_xO _ -> Some (VPSRLDQ (Obj.magic b))
                 | Coq_xH -> Some (PDEP (Obj.magic b)))
              | Coq_xO x3 ->
                (match x3 with
                 | Coq_xI _ ->
                   let { coq_Box_x86_op_PADD_0 = box_x86_op_PADD_0;
                     coq_Box_x86_op_PADD_1 = box_x86_op_PADD_1 } = Obj.magic b
                   in
                   Some (VPUNPCKL (box_x86_op_PADD_0, box_x86_op_PADD_1))
                 | Coq_xO x4 ->
                   (match x4 with
                    | Coq_xI _ -> None
                    | Coq_xO _ -> Some AESDECLAST
                    | Coq_xH ->
                      let { coq_Box_x86_op_PADD_0 = box_x86_op_PADD_0;
                        coq_Box_x86_op_PADD_1 = box_x86_op_PADD_1 } =
                        Obj.magic b
                      in
                      Some (VPSUB (box_x86_op_PADD_0, box_x86_op_PADD_1)))
                 | Coq_xH -> Some (SAL (Obj.magic b)))
              | Coq_xH -> Some CLC)
           | Coq_xO x2 ->
             (match x2 with
              | Coq_xI x3 ->
                (match x3 with
                 | Coq_xI _ ->
                   let { coq_Box_x86_op_PADD_0 = box_x86_op_PADD_0;
                     coq_Box_x86_op_PADD_1 = box_x86_op_PADD_1 } = Obj.magic b
                   in
                   Some (VPCMPGT (box_x86_op_PADD_0, box_x86_op_PADD_1))
                 | Coq_xO x4 ->
                   (match x4 with
                    | Coq_xI _ -> None
                    | Coq_xO _ -> Some AESKEYGENASSIST
                    | Coq_xH -> Some (VPEXTR (Obj.magic b)))
                 | Coq_xH -> Some (MULX_lo_hi (Obj.magic b)))
              | Coq_xO x3 ->
                (match x3 with
                 | Coq_xI _ ->
                   let { coq_Box_x86_op_PADD_0 = box_x86_op_PADD_0;
                     coq_Box_x86_op_PADD_1 = box_x86_op_PADD_1 } = Obj.magic b
                   in
                   Some (VPACKSS (box_x86_op_PADD_0, box_x86_op_PADD_1))
                 | Coq_xO x4 ->
                   (match x4 with
                    | Coq_xI _ -> None
                    | Coq_xO _ -> Some PREFETCHNTA
                    | Coq_xH -> Some (VMOVDQU (Obj.magic b)))
                 | Coq_xH -> Some (XOR (Obj.magic b)))
              | Coq_xH -> Some (NEG (Obj.magic b)))
           | Coq_xH -> Some (IMUL (Obj.magic b)))
        | Coq_xH -> Some (XCHG (Obj.magic b)))
     | Coq_xH ->
       let { coq_Box_x86_op_MOVSX_0 = box_x86_op_MOVSX_0;
         coq_Box_x86_op_MOVSX_1 = box_x86_op_MOVSX_1 } = Obj.magic b
       in
       Some (MOVZX (box_x86_op_MOVSX_0, box_x86_op_MOVSX_1)))
  | Coq_xO x ->
    (match x with
     | Coq_xI x0 ->
       (match x0 with
        | Coq_xI x1 ->
          (match x1 with
           | Coq_xI x2 ->
             (match x2 with
              | Coq_xI x3 ->
                (match x3 with
                 | Coq_xI _ -> Some PREFETCHT0
                 | Coq_xO _ ->
                   let { coq_Box_x86_op_PADD_0 = box_x86_op_PADD_0;
                     coq_Box_x86_op_PADD_1 = box_x86_op_PADD_1 } = Obj.magic b
                   in
                   Some (VPBLEND (box_x86_op_PADD_0, box_x86_op_PADD_1))
                 | Coq_xH -> Some (MOVV (Obj.magic b)))
              | Coq_xO x3 ->
                (match x3 with
                 | Coq_xI _ -> Some VPERMQ
                 | Coq_xO x4 ->
                   (match x4 with
                    | Coq_xI _ -> None
                    | Coq_xO _ -> Some (VAESENCLAST (Obj.magic b))
                    | Coq_xH -> Some (VPMULHRS (Obj.magic b)))
                 | Coq_xH -> Some (SARX (Obj.magic b)))
              | Coq_xH -> Some (AND (Obj.magic b)))
           | Coq_xO x2 ->
             (match x2 with
              | Coq_xI x3 ->
                (match x3 with
                 | Coq_xI _ -> Some VMOVHPD
                 | Coq_xO x4 ->
                   (match x4 with
                    | Coq_xI _ -> None
                    | Coq_xO _ -> Some SHA256MSG1
                    | Coq_xH ->
                      let { coq_Box_x86_op_PADD_0 = box_x86_op_PADD_0;
                        coq_Box_x86_op_PADD_1 = box_x86_op_PADD_1 } =
                        Obj.magic b
                      in
                      Some (VPSLLV (box_x86_op_PADD_0, box_x86_op_PADD_1)))
                 | Coq_xH -> Some (BTR (Obj.magic b)))
              | Coq_xO x3 ->
                (match x3 with
                 | Coq_xI _ -> Some (VPALIGNR (Obj.magic b))
                 | Coq_xO x4 ->
                   (match x4 with
                    | Coq_xI _ -> None
                    | Coq_xO _ -> Some (RDTSCP (Obj.magic b))
                    | Coq_xH -> Some (VPOR (Obj.magic b)))
                 | Coq_xH -> Some (RCL (Obj.magic b)))
              | Coq_xH -> Some (BSR (Obj.magic b)))
           | Coq_xH -> Some (CQO (Obj.magic b)))
        | Coq_xO x1 ->
          (match x1 with
           | Coq_xI x2 ->
             (match x2 with
              | Coq_xI x3 ->
                (match x3 with
                 | Coq_xI _ ->
                   let { coq_Box_x86_op_PADD_0 = box_x86_op_PADD_0;
                     coq_Box_x86_op_PADD_1 = box_x86_op_PADD_1 } = Obj.magic b
                   in
                   Some (VPMAXS (box_x86_op_PADD_0, box_x86_op_PADD_1))
                 | Coq_xO _ -> Some (VPSHUFB (Obj.magic b))
                 | Coq_xH -> Some (MOVX (Obj.magic b)))
              | Coq_xO x3 ->
                (match x3 with
                 | Coq_xI _ -> Some VEXTRACTI128
                 | Coq_xO x4 ->
                   (match x4 with
                    | Coq_xI _ -> None
                    | Coq_xO _ -> Some (VAESDECLAST (Obj.magic b))
                    | Coq_xH ->
                      let { coq_Box_x86_op_PADD_0 = box_x86_op_PADD_0;
                        coq_Box_x86_op_PADD_1 = box_x86_op_PADD_1 } =
                        Obj.magic b
                      in
                      Some (VPAVG (box_x86_op_PADD_0, box_x86_op_PADD_1)))
                 | Coq_xH -> Some (SAR (Obj.magic b)))
              | Coq_xH -> Some STC)
           | Coq_xO x2 ->
             (match x2 with
              | Coq_xI x3 ->
                (match x3 with
                 | Coq_xI _ ->
                   let { coq_Box_x86_op_PADD_0 = box_x86_op_PADD_0;
                     coq_Box_x86_op_PADD_1 = box_x86_op_PADD_1 } = Obj.magic b
                   in
                   Some (VPSIGN (box_x86_op_PADD_0, box_x86_op_PADD_1))
                 | Coq_xO x4 ->
                   (match x4 with
                    | Coq_xI _ -> None
                    | Coq_xO _ -> Some VAESKEYGENASSIST
                    | Coq_xH -> Some (VPINSR (Obj.magic b)))
                 | Coq_xH -> Some (ADCX (Obj.magic b)))
              | Coq_xO x3 ->
                (match x3 with
                 | Coq_xI _ -> Some (VSHUFPS (Obj.magic b))
                 | Coq_xO x4 ->
                   (match x4 with
                    | Coq_xI _ -> None
                    | Coq_xO _ -> Some LFENCE
                    | Coq_xH ->
                      let { coq_Box_x86_op_VPMOVSX_0 = box_x86_op_VPMOVSX_0;
                        coq_Box_x86_op_VPMOVSX_1 = box_x86_op_VPMOVSX_1;
                        coq_Box_x86_op_VPMOVSX_2 = box_x86_op_VPMOVSX_2;
                        coq_Box_x86_op_VPMOVSX_3 = box_x86_op_VPMOVSX_3 } =
                        Obj.magic b
                      in
                      Some (VPMOVSX (box_x86_op_VPMOVSX_0,
                      box_x86_op_VPMOVSX_1, box_x86_op_VPMOVSX_2,
                      box_x86_op_VPMOVSX_3)))
                 | Coq_xH -> Some (NOT (Obj.magic b)))
              | Coq_xH -> Some (INC (Obj.magic b)))
           | Coq_xH -> Some (IMULr (Obj.magic b)))
        | Coq_xH -> Some (ADD (Obj.magic b)))
     | Coq_xO x0 ->
       (match x0 with
        | Coq_xI x1 ->
          (match x1 with
           | Coq_xI x2 ->
             (match x2 with
              | Coq_xI x3 ->
                (match x3 with
                 | Coq_xI _ -> Some (VPTEST (Obj.magic b))
                 | Coq_xO _ -> Some (VPSHUFHW (Obj.magic b))
                 | Coq_xH ->
                   let { coq_Box_x86_op_PADD_0 = box_x86_op_PADD_0;
                     coq_Box_x86_op_PADD_1 = box_x86_op_PADD_1 } = Obj.magic b
                   in
                   Some (PADD (box_x86_op_PADD_0, box_x86_op_PADD_1)))
              | Coq_xO x3 ->
                (match x3 with
                 | Coq_xI _ -> Some VPERM2I128
                 | Coq_xO x4 ->
                   (match x4 with
                    | Coq_xI _ -> None
                    | Coq_xO _ -> Some (VAESENC (Obj.magic b))
                    | Coq_xH -> Some (VPMULH (Obj.magic b)))
                 | Coq_xH -> Some (SHRD (Obj.magic b)))
              | Coq_xH -> Some (TEST (Obj.magic b)))
           | Coq_xO x2 ->
             (match x2 with
              | Coq_xI x3 ->
                (match x3 with
                 | Coq_xI _ -> Some (VPMADDWD (Obj.magic b))
                 | Coq_xO x4 ->
                   (match x4 with
                    | Coq_xI _ -> None
                    | Coq_xO _ -> Some (VPCLMULQDQ (Obj.magic b))
                    | Coq_xH ->
                      let { coq_Box_x86_op_PADD_0 = box_x86_op_PADD_0;
                        coq_Box_x86_op_PADD_1 = box_x86_op_PADD_1 } =
                        Obj.magic b
                      in
                      Some (VPSRL (box_x86_op_PADD_0, box_x86_op_PADD_1)))
                 | Coq_xH -> Some (BSWAP (Obj.magic b)))
              | Coq_xO x3 ->
                (match x3 with
                 | Coq_xI _ -> Some (VMOVSHDUP (Obj.magic b))
                 | Coq_xO x4 ->
                   (match x4 with
                    | Coq_xI _ -> None
                    | Coq_xO _ -> Some SFENCE
                    | Coq_xH -> Some (VPAND (Obj.magic b)))
                 | Coq_xH -> Some (ROL (Obj.magic b)))
              | Coq_xH -> Some (LZCNT (Obj.magic b)))
           | Coq_xH -> Some (DIV (Obj.magic b)))
        | Coq_xO x1 ->
          (match x1 with
           | Coq_xI x2 ->
             (match x2 with
              | Coq_xI x3 ->
                (match x3 with
                 | Coq_xI _ ->
                   let { coq_Box_x86_op_PADD_0 = box_x86_op_PADD_0;
                     coq_Box_x86_op_PADD_1 = box_x86_op_PADD_1 } = Obj.magic b
                   in
                   Some (VPMINS (box_x86_op_PADD_0, box_x86_op_PADD_1))
                 | Coq_xO _ -> Some (VPSLLDQ (Obj.magic b))
                 | Coq_xH -> Some (PEXT (Obj.magic b)))
              | Coq_xO x3 ->
                (match x3 with
                 | Coq_xI _ ->
                   let { coq_Box_x86_op_PADD_0 = box_x86_op_PADD_0;
                     coq_Box_x86_op_PADD_1 = box_x86_op_PADD_1 } = Obj.magic b
                   in
                   Some (VPUNPCKH (box_x86_op_PADD_0, box_x86_op_PADD_1))
                 | Coq_xO x4 ->
                   (match x4 with
                    | Coq_xI _ -> None
                    | Coq_xO _ -> Some (VAESDEC (Obj.magic b))
                    | Coq_xH ->
                      let { coq_Box_x86_op_PADD_0 = box_x86_op_PADD_0;
                        coq_Box_x86_op_PADD_1 = box_x86_op_PADD_1 } =
                        Obj.magic b
                      in
                      Some (VPADD (box_x86_op_PADD_0, box_x86_op_PADD_1)))
                 | Coq_xH -> Some (SHR (Obj.magic b)))
              | Coq_xH -> Some (BT (Obj.magic b)))
           | Coq_xO x2 ->
             (match x2 with
              | Coq_xI x3 ->
                (match x3 with
                 | Coq_xI _ ->
                   let { coq_Box_x86_op_PADD_0 = box_x86_op_PADD_0;
                     coq_Box_x86_op_PADD_1 = box_x86_op_PADD_1 } = Obj.magic b
                   in
                   Some (VPCMPEQ (box_x86_op_PADD_0, box_x86_op_PADD_1))
                 | Coq_xO x4 ->
                   (match x4 with
                    | Coq_xI _ -> None
                    | Coq_xO _ -> Some VAESIMC
                    | Coq_xH -> Some (VPMULU (Obj.magic b)))
                 | Coq_xH -> Some (SHLX (Obj.magic b)))
              | Coq_xO x3 ->
                (match x3 with
                 | Coq_xI _ ->
                   let { coq_Box_x86_op_PADD_0 = box_x86_op_PADD_0;
                     coq_Box_x86_op_PADD_1 = box_x86_op_PADD_1 } = Obj.magic b
                   in
                   Some (VPACKUS (box_x86_op_PADD_0, box_x86_op_PADD_1))
                 | Coq_xO x4 ->
                   (match x4 with
                    | Coq_xI _ -> None
                    | Coq_xO _ -> Some PREFETCHT2
                    | Coq_xH -> Some (VMOVDQA (Obj.magic b)))
                 | Coq_xH -> Some (OR (Obj.magic b)))
              | Coq_xH -> Some (SBB (Obj.magic b)))
           | Coq_xH -> Some (MUL (Obj.magic b)))
        | Coq_xH -> Some (CMOVcc (Obj.magic b)))
     | Coq_xH ->
       let { coq_Box_x86_op_MOVSX_0 = box_x86_op_MOVSX_0;
         coq_Box_x86_op_MOVSX_1 = box_x86_op_MOVSX_1 } = Obj.magic b
       in
       Some (MOVSX (box_x86_op_MOVSX_0, box_x86_op_MOVSX_1)))
  | Coq_xH -> Some (MOV (Obj.magic b))

(** val x86_op_induction :
    (wsize -> is_wsize -> 'a1) -> (wsize -> is_wsize -> wsize -> is_wsize ->
    'a1) -> (wsize -> is_wsize -> wsize -> is_wsize -> 'a1) -> (wsize ->
    is_wsize -> 'a1) -> (wsize -> is_wsize -> 'a1) -> (wsize -> is_wsize ->
    'a1) -> (wsize -> is_wsize -> 'a1) -> (wsize -> is_wsize -> 'a1) ->
    (wsize -> is_wsize -> 'a1) -> (wsize -> is_wsize -> 'a1) -> (wsize ->
    is_wsize -> 'a1) -> (wsize -> is_wsize -> 'a1) -> (wsize -> is_wsize ->
    'a1) -> (wsize -> is_wsize -> 'a1) -> (wsize -> is_wsize -> 'a1) ->
    (wsize -> is_wsize -> 'a1) -> (wsize -> is_wsize -> 'a1) -> (wsize ->
    is_wsize -> 'a1) -> (wsize -> is_wsize -> 'a1) -> (wsize -> is_wsize ->
    'a1) -> (wsize -> is_wsize -> 'a1) -> (wsize -> is_wsize -> 'a1) -> 'a1
    -> (wsize -> is_wsize -> 'a1) -> 'a1 -> 'a1 -> (wsize -> is_wsize -> 'a1)
    -> (wsize -> is_wsize -> 'a1) -> (wsize -> is_wsize -> 'a1) -> (wsize ->
    is_wsize -> 'a1) -> (wsize -> is_wsize -> 'a1) -> (wsize -> is_wsize ->
    'a1) -> (wsize -> is_wsize -> 'a1) -> (wsize -> is_wsize -> 'a1) ->
    (wsize -> is_wsize -> 'a1) -> (wsize -> is_wsize -> 'a1) -> (wsize ->
    is_wsize -> 'a1) -> (wsize -> is_wsize -> 'a1) -> (wsize -> is_wsize ->
    'a1) -> (wsize -> is_wsize -> 'a1) -> (wsize -> is_wsize -> 'a1) ->
    (wsize -> is_wsize -> 'a1) -> (wsize -> is_wsize -> 'a1) -> (wsize ->
    is_wsize -> 'a1) -> (wsize -> is_wsize -> 'a1) -> (wsize -> is_wsize ->
    'a1) -> (wsize -> is_wsize -> 'a1) -> (wsize -> is_wsize -> 'a1) ->
    (wsize -> is_wsize -> 'a1) -> (wsize -> is_wsize -> 'a1) -> (wsize ->
    is_wsize -> 'a1) -> (wsize -> is_wsize -> 'a1) -> (wsize -> is_wsize ->
    'a1) -> (wsize -> is_wsize -> 'a1) -> (wsize -> is_wsize -> 'a1) ->
    (wsize -> is_wsize -> 'a1) -> (wsize -> is_wsize -> 'a1) -> (wsize ->
    is_wsize -> 'a1) -> 'a1 -> (velem -> is_velem -> wsize -> is_wsize ->
    'a1) -> (wsize -> is_wsize -> 'a1) -> (wsize -> is_wsize -> 'a1) ->
    (wsize -> is_wsize -> 'a1) -> (wsize -> is_wsize -> 'a1) -> (wsize ->
    is_wsize -> 'a1) -> (velem -> is_velem -> wsize -> is_wsize -> velem ->
    is_velem -> wsize -> is_wsize -> 'a1) -> (velem -> is_velem -> wsize ->
    is_wsize -> velem -> is_velem -> wsize -> is_wsize -> 'a1) -> (wsize ->
    is_wsize -> 'a1) -> (wsize -> is_wsize -> 'a1) -> (wsize -> is_wsize ->
    'a1) -> (wsize -> is_wsize -> 'a1) -> (velem -> is_velem -> wsize ->
    is_wsize -> 'a1) -> (velem -> is_velem -> wsize -> is_wsize -> 'a1) ->
    (velem -> is_velem -> wsize -> is_wsize -> 'a1) -> (velem -> is_velem ->
    wsize -> is_wsize -> 'a1) -> (wsize -> is_wsize -> 'a1) -> (wsize ->
    is_wsize -> 'a1) -> (wsize -> is_wsize -> 'a1) -> (wsize -> is_wsize ->
    'a1) -> (wsize -> is_wsize -> 'a1) -> (wsize -> is_wsize -> 'a1) ->
    (velem -> is_velem -> 'a1) -> (velem -> is_velem -> wsize -> is_wsize ->
    'a1) -> (velem -> is_velem -> wsize -> is_wsize -> 'a1) -> (velem ->
    is_velem -> wsize -> is_wsize -> 'a1) -> (velem -> is_velem -> wsize ->
    is_wsize -> 'a1) -> (velem -> is_velem -> wsize -> is_wsize -> 'a1) ->
    (wsize -> is_wsize -> 'a1) -> (wsize -> is_wsize -> 'a1) -> (wsize ->
    is_wsize -> 'a1) -> (wsize -> is_wsize -> 'a1) -> (wsize -> is_wsize ->
    'a1) -> (wsize -> is_wsize -> 'a1) -> (velem -> is_velem -> wsize ->
    is_wsize -> 'a1) -> (velem -> is_velem -> wsize -> is_wsize -> 'a1) ->
    (velem -> is_velem -> wsize -> is_wsize -> 'a1) -> (velem -> is_velem ->
    wsize -> is_wsize -> 'a1) -> (wsize -> is_wsize -> 'a1) -> (velem ->
    is_velem -> wsize -> is_wsize -> 'a1) -> (wsize -> is_wsize -> 'a1) ->
    (wsize -> is_wsize -> 'a1) -> (wsize -> is_wsize -> 'a1) -> 'a1 -> (velem
    -> is_velem -> wsize -> is_wsize -> 'a1) -> (velem -> is_velem -> wsize
    -> is_wsize -> 'a1) -> 'a1 -> 'a1 -> 'a1 -> 'a1 -> 'a1 -> (velem ->
    is_velem -> wsize -> is_wsize -> 'a1) -> (velem -> is_velem -> wsize ->
    is_wsize -> 'a1) -> (velem -> is_velem -> wsize -> is_wsize -> 'a1) ->
    (velem -> is_velem -> wsize -> is_wsize -> 'a1) -> (wsize -> is_wsize ->
    'a1) -> (wsize -> is_wsize -> 'a1) -> 'a1 -> 'a1 -> (velem -> is_velem ->
    wsize -> is_wsize -> 'a1) -> (velem -> is_velem -> wsize -> is_wsize ->
    'a1) -> (velem -> is_velem -> wsize -> is_wsize -> 'a1) -> (velem ->
    is_velem -> wsize -> is_wsize -> 'a1) -> (velem -> is_velem -> wsize ->
    is_wsize -> 'a1) -> (wsize -> is_wsize -> 'a1) -> 'a1 -> 'a1 -> 'a1 ->
    'a1 -> 'a1 -> 'a1 -> 'a1 -> 'a1 -> (wsize -> is_wsize -> 'a1) -> (wsize
    -> is_wsize -> 'a1) -> 'a1 -> (wsize -> is_wsize -> 'a1) -> 'a1 -> (wsize
    -> is_wsize -> 'a1) -> 'a1 -> (wsize -> is_wsize -> 'a1) -> 'a1 -> (wsize
    -> is_wsize -> 'a1) -> 'a1 -> 'a1 -> 'a1 -> 'a1 -> 'a1 -> (wsize ->
    is_wsize -> 'a1) -> 'a1 -> 'a1 -> 'a1 -> x86_op -> is_x86_op -> 'a1 **)

let x86_op_induction his_MOV his_MOVSX his_MOVZX his_CMOVcc his_XCHG his_ADD his_SUB his_MUL his_IMUL his_IMULr his_IMULri his_DIV his_IDIV his_CQO his_ADC his_SBB his_NEG his_INC his_DEC his_LZCNT his_TZCNT his_BSR his_SETcc his_BT his_CLC his_STC his_LEA his_TEST his_CMP his_AND his_ANDN his_OR his_XOR his_NOT his_ROR his_ROL his_RCR his_RCL his_SHL his_SHR his_SAL his_SAR his_SHLD his_SHRD his_RORX his_SARX his_SHRX his_SHLX his_MULX_lo_hi his_ADCX his_ADOX his_BSWAP his_POPCNT his_BTR his_BTS his_PEXT his_PDEP his_MOVX his_POR his_PADD his_MOVD his_MOVV his_VMOV his_VMOVDQA his_VMOVDQU his_VPMOVSX his_VPMOVZX his_VPAND his_VPANDN his_VPOR his_VPXOR his_VPADD his_VPSUB his_VPAVG his_VPMULL his_VPMULH his_VPMULHU his_VPMULHRS his_VPMUL his_VPMULU his_VPEXTR his_VPINSR his_VPSLL his_VPSRL his_VPSRA his_VPSLLV his_VPSRLV his_VPSLLDQ his_VPSRLDQ his_VPSHUFB his_VPSHUFD his_VPSHUFHW his_VPSHUFLW his_VPBLEND his_BLENDV his_VPACKUS his_VPACKSS his_VSHUFPS his_VPBROADCAST his_VMOVSHDUP his_VMOVSLDUP his_VPALIGNR his_VBROADCASTI128 his_VPUNPCKH his_VPUNPCKL his_VEXTRACTI128 his_VINSERTI128 his_VPERM2I128 his_VPERMD his_VPERMQ his_MOVEMASK his_VPCMPEQ his_VPCMPGT his_VPSIGN his_VPMADDUBSW his_VPMADDWD his_VMOVLPD his_VMOVHPD his_VPMINU his_VPMINS his_VPMAXU his_VPMAXS his_VPABS his_VPTEST his_CLFLUSH his_PREFETCHT0 his_PREFETCHT1 his_PREFETCHT2 his_PREFETCHNTA his_LFENCE his_MFENCE his_SFENCE his_RDTSC his_RDTSCP his_AESDEC his_VAESDEC his_AESDECLAST his_VAESDECLAST his_AESENC his_VAESENC his_AESENCLAST his_VAESENCLAST his_AESIMC his_VAESIMC his_AESKEYGENASSIST his_VAESKEYGENASSIST his_PCLMULQDQ his_VPCLMULQDQ his_SHA256RNDS2 his_SHA256MSG1 his_SHA256MSG2 _ = function
| Coq_is_MOV (x0, p_) -> his_MOV x0 p_
| Coq_is_MOVSX (x0, p_, x1, p_0) -> his_MOVSX x0 p_ x1 p_0
| Coq_is_MOVZX (x0, p_, x1, p_0) -> his_MOVZX x0 p_ x1 p_0
| Coq_is_CMOVcc (x0, p_) -> his_CMOVcc x0 p_
| Coq_is_XCHG (x0, p_) -> his_XCHG x0 p_
| Coq_is_ADD (x0, p_) -> his_ADD x0 p_
| Coq_is_SUB (x0, p_) -> his_SUB x0 p_
| Coq_is_MUL (x0, p_) -> his_MUL x0 p_
| Coq_is_IMUL (x0, p_) -> his_IMUL x0 p_
| Coq_is_IMULr (x0, p_) -> his_IMULr x0 p_
| Coq_is_IMULri (x0, p_) -> his_IMULri x0 p_
| Coq_is_DIV (x0, p_) -> his_DIV x0 p_
| Coq_is_IDIV (x0, p_) -> his_IDIV x0 p_
| Coq_is_CQO (x0, p_) -> his_CQO x0 p_
| Coq_is_ADC (x0, p_) -> his_ADC x0 p_
| Coq_is_SBB (x0, p_) -> his_SBB x0 p_
| Coq_is_NEG (x0, p_) -> his_NEG x0 p_
| Coq_is_INC (x0, p_) -> his_INC x0 p_
| Coq_is_DEC (x0, p_) -> his_DEC x0 p_
| Coq_is_LZCNT (x0, p_) -> his_LZCNT x0 p_
| Coq_is_TZCNT (x0, p_) -> his_TZCNT x0 p_
| Coq_is_BSR (x0, p_) -> his_BSR x0 p_
| Coq_is_SETcc -> his_SETcc
| Coq_is_BT (x0, p_) -> his_BT x0 p_
| Coq_is_CLC -> his_CLC
| Coq_is_STC -> his_STC
| Coq_is_LEA (x0, p_) -> his_LEA x0 p_
| Coq_is_TEST (x0, p_) -> his_TEST x0 p_
| Coq_is_CMP (x0, p_) -> his_CMP x0 p_
| Coq_is_AND (x0, p_) -> his_AND x0 p_
| Coq_is_ANDN (x0, p_) -> his_ANDN x0 p_
| Coq_is_OR (x0, p_) -> his_OR x0 p_
| Coq_is_XOR (x0, p_) -> his_XOR x0 p_
| Coq_is_NOT (x0, p_) -> his_NOT x0 p_
| Coq_is_ROR (x0, p_) -> his_ROR x0 p_
| Coq_is_ROL (x0, p_) -> his_ROL x0 p_
| Coq_is_RCR (x0, p_) -> his_RCR x0 p_
| Coq_is_RCL (x0, p_) -> his_RCL x0 p_
| Coq_is_SHL (x0, p_) -> his_SHL x0 p_
| Coq_is_SHR (x0, p_) -> his_SHR x0 p_
| Coq_is_SAL (x0, p_) -> his_SAL x0 p_
| Coq_is_SAR (x0, p_) -> his_SAR x0 p_
| Coq_is_SHLD (x0, p_) -> his_SHLD x0 p_
| Coq_is_SHRD (x0, p_) -> his_SHRD x0 p_
| Coq_is_RORX (x0, p_) -> his_RORX x0 p_
| Coq_is_SARX (x0, p_) -> his_SARX x0 p_
| Coq_is_SHRX (x0, p_) -> his_SHRX x0 p_
| Coq_is_SHLX (x0, p_) -> his_SHLX x0 p_
| Coq_is_MULX_lo_hi (x0, p_) -> his_MULX_lo_hi x0 p_
| Coq_is_ADCX (x0, p_) -> his_ADCX x0 p_
| Coq_is_ADOX (x0, p_) -> his_ADOX x0 p_
| Coq_is_BSWAP (x0, p_) -> his_BSWAP x0 p_
| Coq_is_POPCNT (x0, p_) -> his_POPCNT x0 p_
| Coq_is_BTR (x0, p_) -> his_BTR x0 p_
| Coq_is_BTS (x0, p_) -> his_BTS x0 p_
| Coq_is_PEXT (x0, p_) -> his_PEXT x0 p_
| Coq_is_PDEP (x0, p_) -> his_PDEP x0 p_
| Coq_is_MOVX (x0, p_) -> his_MOVX x0 p_
| Coq_is_POR -> his_POR
| Coq_is_PADD (x0, p_, x1, p_0) -> his_PADD x0 p_ x1 p_0
| Coq_is_MOVD (x0, p_) -> his_MOVD x0 p_
| Coq_is_MOVV (x0, p_) -> his_MOVV x0 p_
| Coq_is_VMOV (x0, p_) -> his_VMOV x0 p_
| Coq_is_VMOVDQA (x0, p_) -> his_VMOVDQA x0 p_
| Coq_is_VMOVDQU (h, pH) -> his_VMOVDQU h pH
| Coq_is_VPMOVSX (x0, p_, x1, p_0, x2, p_1, x3, p_2) ->
  his_VPMOVSX x0 p_ x1 p_0 x2 p_1 x3 p_2
| Coq_is_VPMOVZX (x0, p_, x1, p_0, x2, p_1, x3, p_2) ->
  his_VPMOVZX x0 p_ x1 p_0 x2 p_1 x3 p_2
| Coq_is_VPAND (h, pH) -> his_VPAND h pH
| Coq_is_VPANDN (h, pH) -> his_VPANDN h pH
| Coq_is_VPOR (h, pH) -> his_VPOR h pH
| Coq_is_VPXOR (h, pH) -> his_VPXOR h pH
| Coq_is_VPADD (h, pH, h0, pH0) -> his_VPADD h pH h0 pH0
| Coq_is_VPSUB (h, pH, h0, pH0) -> his_VPSUB h pH h0 pH0
| Coq_is_VPAVG (x0, p_, x1, p_0) -> his_VPAVG x0 p_ x1 p_0
| Coq_is_VPMULL (h, pH, h0, pH0) -> his_VPMULL h pH h0 pH0
| Coq_is_VPMULH (x0, p_) -> his_VPMULH x0 p_
| Coq_is_VPMULHU (x0, p_) -> his_VPMULHU x0 p_
| Coq_is_VPMULHRS (x0, p_) -> his_VPMULHRS x0 p_
| Coq_is_VPMUL (h, pH) -> his_VPMUL h pH
| Coq_is_VPMULU (h, pH) -> his_VPMULU h pH
| Coq_is_VPEXTR (x0, p_) -> his_VPEXTR x0 p_
| Coq_is_VPINSR (x0, p_) -> his_VPINSR x0 p_
| Coq_is_VPSLL (h, pH, h0, pH0) -> his_VPSLL h pH h0 pH0
| Coq_is_VPSRL (h, pH, h0, pH0) -> his_VPSRL h pH h0 pH0
| Coq_is_VPSRA (h, pH, h0, pH0) -> his_VPSRA h pH h0 pH0
| Coq_is_VPSLLV (h, pH, h0, pH0) -> his_VPSLLV h pH h0 pH0
| Coq_is_VPSRLV (h, pH, h0, pH0) -> his_VPSRLV h pH h0 pH0
| Coq_is_VPSLLDQ (h, pH) -> his_VPSLLDQ h pH
| Coq_is_VPSRLDQ (h, pH) -> his_VPSRLDQ h pH
| Coq_is_VPSHUFB (h, pH) -> his_VPSHUFB h pH
| Coq_is_VPSHUFD (h, pH) -> his_VPSHUFD h pH
| Coq_is_VPSHUFHW (h, pH) -> his_VPSHUFHW h pH
| Coq_is_VPSHUFLW (h, pH) -> his_VPSHUFLW h pH
| Coq_is_VPBLEND (h, pH, h0, pH0) -> his_VPBLEND h pH h0 pH0
| Coq_is_BLENDV (x0, p_, x1, p_0) -> his_BLENDV x0 p_ x1 p_0
| Coq_is_VPACKUS (h, pH, h0, pH0) -> his_VPACKUS h pH h0 pH0
| Coq_is_VPACKSS (h, pH, h0, pH0) -> his_VPACKSS h pH h0 pH0
| Coq_is_VSHUFPS (h, pH) -> his_VSHUFPS h pH
| Coq_is_VPBROADCAST (x0, p_, x1, p_0) -> his_VPBROADCAST x0 p_ x1 p_0
| Coq_is_VMOVSHDUP (x0, p_) -> his_VMOVSHDUP x0 p_
| Coq_is_VMOVSLDUP (x0, p_) -> his_VMOVSLDUP x0 p_
| Coq_is_VPALIGNR (h, pH) -> his_VPALIGNR h pH
| Coq_is_VBROADCASTI128 -> his_VBROADCASTI128
| Coq_is_VPUNPCKH (h, pH, h0, pH0) -> his_VPUNPCKH h pH h0 pH0
| Coq_is_VPUNPCKL (h, pH, h0, pH0) -> his_VPUNPCKL h pH h0 pH0
| Coq_is_VEXTRACTI128 -> his_VEXTRACTI128
| Coq_is_VINSERTI128 -> his_VINSERTI128
| Coq_is_VPERM2I128 -> his_VPERM2I128
| Coq_is_VPERMD -> his_VPERMD
| Coq_is_VPERMQ -> his_VPERMQ
| Coq_is_MOVEMASK (x0, p_, x1, p_0) -> his_MOVEMASK x0 p_ x1 p_0
| Coq_is_VPCMPEQ (x0, p_, x1, p_0) -> his_VPCMPEQ x0 p_ x1 p_0
| Coq_is_VPCMPGT (x0, p_, x1, p_0) -> his_VPCMPGT x0 p_ x1 p_0
| Coq_is_VPSIGN (x0, p_, x1, p_0) -> his_VPSIGN x0 p_ x1 p_0
| Coq_is_VPMADDUBSW (x0, p_) -> his_VPMADDUBSW x0 p_
| Coq_is_VPMADDWD (x0, p_) -> his_VPMADDWD x0 p_
| Coq_is_VMOVLPD -> his_VMOVLPD
| Coq_is_VMOVHPD -> his_VMOVHPD
| Coq_is_VPMINU (x0, p_, x1, p_0) -> his_VPMINU x0 p_ x1 p_0
| Coq_is_VPMINS (x0, p_, x1, p_0) -> his_VPMINS x0 p_ x1 p_0
| Coq_is_VPMAXU (x0, p_, x1, p_0) -> his_VPMAXU x0 p_ x1 p_0
| Coq_is_VPMAXS (x0, p_, x1, p_0) -> his_VPMAXS x0 p_ x1 p_0
| Coq_is_VPABS (x0, p_, x1, p_0) -> his_VPABS x0 p_ x1 p_0
| Coq_is_VPTEST (h, pH) -> his_VPTEST h pH
| Coq_is_CLFLUSH -> his_CLFLUSH
| Coq_is_PREFETCHT0 -> his_PREFETCHT0
| Coq_is_PREFETCHT1 -> his_PREFETCHT1
| Coq_is_PREFETCHT2 -> his_PREFETCHT2
| Coq_is_PREFETCHNTA -> his_PREFETCHNTA
| Coq_is_LFENCE -> his_LFENCE
| Coq_is_MFENCE -> his_MFENCE
| Coq_is_SFENCE -> his_SFENCE
| Coq_is_RDTSC (x0, p_) -> his_RDTSC x0 p_
| Coq_is_RDTSCP (x0, p_) -> his_RDTSCP x0 p_
| Coq_is_AESDEC -> his_AESDEC
| Coq_is_VAESDEC (x0, p_) -> his_VAESDEC x0 p_
| Coq_is_AESDECLAST -> his_AESDECLAST
| Coq_is_VAESDECLAST (x0, p_) -> his_VAESDECLAST x0 p_
| Coq_is_AESENC -> his_AESENC
| Coq_is_VAESENC (x0, p_) -> his_VAESENC x0 p_
| Coq_is_AESENCLAST -> his_AESENCLAST
| Coq_is_VAESENCLAST (x0, p_) -> his_VAESENCLAST x0 p_
| Coq_is_AESIMC -> his_AESIMC
| Coq_is_VAESIMC -> his_VAESIMC
| Coq_is_AESKEYGENASSIST -> his_AESKEYGENASSIST
| Coq_is_VAESKEYGENASSIST -> his_VAESKEYGENASSIST
| Coq_is_PCLMULQDQ -> his_PCLMULQDQ
| Coq_is_VPCLMULQDQ (x0, p_) -> his_VPCLMULQDQ x0 p_
| Coq_is_SHA256RNDS2 -> his_SHA256RNDS2
| Coq_is_SHA256MSG1 -> his_SHA256MSG1
| Coq_is_SHA256MSG2 -> his_SHA256MSG2

(** val x86_op_eqb_fields :
    (x86_op -> x86_op -> bool) -> positive -> x86_op_fields_t ->
    x86_op_fields_t -> bool **)

let x86_op_eqb_fields _ x a b =
  match x with
  | Coq_xI x0 ->
    (match x0 with
     | Coq_xI x1 ->
       (match x1 with
        | Coq_xI x2 ->
          (match x2 with
           | Coq_xI x3 ->
             (match x3 with
              | Coq_xI x4 ->
                (match x4 with
                 | Coq_xI _ -> true
                 | Coq_xO _ ->
                   let { coq_Box_x86_op_PADD_0 = box_x86_op_PADD_0;
                     coq_Box_x86_op_PADD_1 = box_x86_op_PADD_1 } = Obj.magic a
                   in
                   let { coq_Box_x86_op_PADD_0 = box_x86_op_PADD_2;
                     coq_Box_x86_op_PADD_1 = box_x86_op_PADD_3 } = Obj.magic b
                   in
                   (&&) (velem_eqb box_x86_op_PADD_0 box_x86_op_PADD_2)
                     ((&&) (wsize_eqb box_x86_op_PADD_1 box_x86_op_PADD_3)
                       true)
                 | Coq_xH -> (&&) (wsize_eqb (Obj.magic a) (Obj.magic b)) true)
              | Coq_xO x4 ->
                (match x4 with
                 | Coq_xI _ ->
                   let { coq_Box_x86_op_PADD_0 = box_x86_op_PADD_0;
                     coq_Box_x86_op_PADD_1 = box_x86_op_PADD_1 } = Obj.magic a
                   in
                   let { coq_Box_x86_op_PADD_0 = box_x86_op_PADD_2;
                     coq_Box_x86_op_PADD_1 = box_x86_op_PADD_3 } = Obj.magic b
                   in
                   (&&) (velem_eqb box_x86_op_PADD_0 box_x86_op_PADD_2)
                     ((&&) (wsize_eqb box_x86_op_PADD_1 box_x86_op_PADD_3)
                       true)
                 | Coq_xO x5 ->
                   (match x5 with
                    | Coq_xH ->
                      (&&) (wsize_eqb (Obj.magic a) (Obj.magic b)) true
                    | _ -> true)
                 | Coq_xH -> (&&) (wsize_eqb (Obj.magic a) (Obj.magic b)) true)
              | Coq_xH -> (&&) (wsize_eqb (Obj.magic a) (Obj.magic b)) true)
           | Coq_xO x3 ->
             (match x3 with
              | Coq_xI x4 ->
                (match x4 with
                 | Coq_xI _ ->
                   let { coq_Box_x86_op_PADD_0 = box_x86_op_PADD_0;
                     coq_Box_x86_op_PADD_1 = box_x86_op_PADD_1 } = Obj.magic a
                   in
                   let { coq_Box_x86_op_PADD_0 = box_x86_op_PADD_2;
                     coq_Box_x86_op_PADD_1 = box_x86_op_PADD_3 } = Obj.magic b
                   in
                   (&&) (velem_eqb box_x86_op_PADD_0 box_x86_op_PADD_2)
                     ((&&) (wsize_eqb box_x86_op_PADD_1 box_x86_op_PADD_3)
                       true)
                 | Coq_xO x5 ->
                   (match x5 with
                    | Coq_xH ->
                      let { coq_Box_x86_op_PADD_0 = box_x86_op_PADD_0;
                        coq_Box_x86_op_PADD_1 = box_x86_op_PADD_1 } =
                        Obj.magic a
                      in
                      let { coq_Box_x86_op_PADD_0 = box_x86_op_PADD_2;
                        coq_Box_x86_op_PADD_1 = box_x86_op_PADD_3 } =
                        Obj.magic b
                      in
                      (&&) (velem_eqb box_x86_op_PADD_0 box_x86_op_PADD_2)
                        ((&&) (wsize_eqb box_x86_op_PADD_1 box_x86_op_PADD_3)
                          true)
                    | _ -> true)
                 | Coq_xH -> (&&) (wsize_eqb (Obj.magic a) (Obj.magic b)) true)
              | Coq_xO x4 ->
                (match x4 with
                 | Coq_xI _ -> true
                 | Coq_xO x5 ->
                   (match x5 with
                    | Coq_xH ->
                      (&&) (wsize_eqb (Obj.magic a) (Obj.magic b)) true
                    | _ -> true)
                 | Coq_xH -> (&&) (wsize_eqb (Obj.magic a) (Obj.magic b)) true)
              | Coq_xH -> true)
           | Coq_xH -> (&&) (wsize_eqb (Obj.magic a) (Obj.magic b)) true)
        | Coq_xO x2 ->
          (match x2 with
           | Coq_xI x3 ->
             (match x3 with
              | Coq_xI x4 ->
                (match x4 with
                 | Coq_xI _ ->
                   let { coq_Box_x86_op_PADD_0 = box_x86_op_PADD_0;
                     coq_Box_x86_op_PADD_1 = box_x86_op_PADD_1 } = Obj.magic a
                   in
                   let { coq_Box_x86_op_PADD_0 = box_x86_op_PADD_2;
                     coq_Box_x86_op_PADD_1 = box_x86_op_PADD_3 } = Obj.magic b
                   in
                   (&&) (velem_eqb box_x86_op_PADD_0 box_x86_op_PADD_2)
                     ((&&) (wsize_eqb box_x86_op_PADD_1 box_x86_op_PADD_3)
                       true)
                 | Coq_xO _ ->
                   (&&) (wsize_eqb (Obj.magic a) (Obj.magic b)) true
                 | Coq_xH -> true)
              | Coq_xO x4 ->
                (match x4 with
                 | Coq_xI _ -> true
                 | Coq_xO x5 ->
                   (match x5 with
                    | Coq_xH ->
                      let { coq_Box_x86_op_PADD_0 = box_x86_op_PADD_0;
                        coq_Box_x86_op_PADD_1 = box_x86_op_PADD_1 } =
                        Obj.magic a
                      in
                      let { coq_Box_x86_op_PADD_0 = box_x86_op_PADD_2;
                        coq_Box_x86_op_PADD_1 = box_x86_op_PADD_3 } =
                        Obj.magic b
                      in
                      (&&) (velem_eqb box_x86_op_PADD_0 box_x86_op_PADD_2)
                        ((&&) (wsize_eqb box_x86_op_PADD_1 box_x86_op_PADD_3)
                          true)
                    | _ -> true)
                 | Coq_xH -> (&&) (wsize_eqb (Obj.magic a) (Obj.magic b)) true)
              | Coq_xH -> (&&) (wsize_eqb (Obj.magic a) (Obj.magic b)) true)
           | Coq_xO x3 ->
             (match x3 with
              | Coq_xI x4 ->
                (match x4 with
                 | Coq_xO x5 ->
                   (match x5 with
                    | Coq_xH ->
                      let { coq_Box_x86_op_PADD_0 = box_x86_op_PADD_0;
                        coq_Box_x86_op_PADD_1 = box_x86_op_PADD_1 } =
                        Obj.magic a
                      in
                      let { coq_Box_x86_op_PADD_0 = box_x86_op_PADD_2;
                        coq_Box_x86_op_PADD_1 = box_x86_op_PADD_3 } =
                        Obj.magic b
                      in
                      (&&) (velem_eqb box_x86_op_PADD_0 box_x86_op_PADD_2)
                        ((&&) (wsize_eqb box_x86_op_PADD_1 box_x86_op_PADD_3)
                          true)
                    | _ -> true)
                 | _ -> (&&) (wsize_eqb (Obj.magic a) (Obj.magic b)) true)
              | Coq_xO x4 ->
                (match x4 with
                 | Coq_xI _ ->
                   let { coq_Box_x86_op_PADD_0 = box_x86_op_PADD_0;
                     coq_Box_x86_op_PADD_1 = box_x86_op_PADD_1 } = Obj.magic a
                   in
                   let { coq_Box_x86_op_PADD_0 = box_x86_op_PADD_2;
                     coq_Box_x86_op_PADD_1 = box_x86_op_PADD_3 } = Obj.magic b
                   in
                   (&&) (velem_eqb box_x86_op_PADD_0 box_x86_op_PADD_2)
                     ((&&) (wsize_eqb box_x86_op_PADD_1 box_x86_op_PADD_3)
                       true)
                 | Coq_xO x5 ->
                   (match x5 with
                    | Coq_xH ->
                      let { coq_Box_x86_op_VPMOVSX_0 = box_x86_op_VPMOVSX_0;
                        coq_Box_x86_op_VPMOVSX_1 = box_x86_op_VPMOVSX_1;
                        coq_Box_x86_op_VPMOVSX_2 = box_x86_op_VPMOVSX_2;
                        coq_Box_x86_op_VPMOVSX_3 = box_x86_op_VPMOVSX_3 } =
                        Obj.magic a
                      in
                      let { coq_Box_x86_op_VPMOVSX_0 = box_x86_op_VPMOVSX_4;
                        coq_Box_x86_op_VPMOVSX_1 = box_x86_op_VPMOVSX_5;
                        coq_Box_x86_op_VPMOVSX_2 = box_x86_op_VPMOVSX_6;
                        coq_Box_x86_op_VPMOVSX_3 = box_x86_op_VPMOVSX_7 } =
                        Obj.magic b
                      in
                      (&&)
                        (velem_eqb box_x86_op_VPMOVSX_0 box_x86_op_VPMOVSX_4)
                        ((&&)
                          (wsize_eqb box_x86_op_VPMOVSX_1
                            box_x86_op_VPMOVSX_5)
                          ((&&)
                            (velem_eqb box_x86_op_VPMOVSX_2
                              box_x86_op_VPMOVSX_6)
                            ((&&)
                              (wsize_eqb box_x86_op_VPMOVSX_3
                                box_x86_op_VPMOVSX_7) true)))
                    | _ -> true)
                 | Coq_xH -> (&&) (wsize_eqb (Obj.magic a) (Obj.magic b)) true)
              | Coq_xH -> (&&) (wsize_eqb (Obj.magic a) (Obj.magic b)) true)
           | Coq_xH -> (&&) (wsize_eqb (Obj.magic a) (Obj.magic b)) true)
        | Coq_xH -> (&&) (wsize_eqb (Obj.magic a) (Obj.magic b)) true)
     | Coq_xO x1 ->
       (match x1 with
        | Coq_xI x2 ->
          (match x2 with
           | Coq_xI x3 ->
             (match x3 with
              | Coq_xI x4 ->
                (match x4 with
                 | Coq_xI _ -> true
                 | _ -> (&&) (wsize_eqb (Obj.magic a) (Obj.magic b)) true)
              | Coq_xO x4 ->
                (match x4 with
                 | Coq_xI _ -> true
                 | Coq_xO x5 ->
                   (match x5 with
                    | Coq_xH ->
                      (&&) (wsize_eqb (Obj.magic a) (Obj.magic b)) true
                    | _ -> true)
                 | Coq_xH -> (&&) (wsize_eqb (Obj.magic a) (Obj.magic b)) true)
              | Coq_xH -> (&&) (wsize_eqb (Obj.magic a) (Obj.magic b)) true)
           | Coq_xO x3 ->
             (match x3 with
              | Coq_xI x4 ->
                (match x4 with
                 | Coq_xI _ -> true
                 | Coq_xO x5 ->
                   (match x5 with
                    | Coq_xH ->
                      let { coq_Box_x86_op_PADD_0 = box_x86_op_PADD_0;
                        coq_Box_x86_op_PADD_1 = box_x86_op_PADD_1 } =
                        Obj.magic a
                      in
                      let { coq_Box_x86_op_PADD_0 = box_x86_op_PADD_2;
                        coq_Box_x86_op_PADD_1 = box_x86_op_PADD_3 } =
                        Obj.magic b
                      in
                      (&&) (velem_eqb box_x86_op_PADD_0 box_x86_op_PADD_2)
                        ((&&) (wsize_eqb box_x86_op_PADD_1 box_x86_op_PADD_3)
                          true)
                    | _ -> true)
                 | Coq_xH -> (&&) (wsize_eqb (Obj.magic a) (Obj.magic b)) true)
              | Coq_xO x4 ->
                (match x4 with
                 | Coq_xO x5 ->
                   (match x5 with
                    | Coq_xI _ -> true
                    | _ -> (&&) (wsize_eqb (Obj.magic a) (Obj.magic b)) true)
                 | _ -> (&&) (wsize_eqb (Obj.magic a) (Obj.magic b)) true)
              | Coq_xH -> (&&) (wsize_eqb (Obj.magic a) (Obj.magic b)) true)
           | Coq_xH -> (&&) (wsize_eqb (Obj.magic a) (Obj.magic b)) true)
        | Coq_xO x2 ->
          (match x2 with
           | Coq_xI x3 ->
             (match x3 with
              | Coq_xI x4 ->
                (match x4 with
                 | Coq_xI _ ->
                   let { coq_Box_x86_op_PADD_0 = box_x86_op_PADD_0;
                     coq_Box_x86_op_PADD_1 = box_x86_op_PADD_1 } = Obj.magic a
                   in
                   let { coq_Box_x86_op_PADD_0 = box_x86_op_PADD_2;
                     coq_Box_x86_op_PADD_1 = box_x86_op_PADD_3 } = Obj.magic b
                   in
                   (&&) (velem_eqb box_x86_op_PADD_0 box_x86_op_PADD_2)
                     ((&&) (wsize_eqb box_x86_op_PADD_1 box_x86_op_PADD_3)
                       true)
                 | _ -> (&&) (wsize_eqb (Obj.magic a) (Obj.magic b)) true)
              | Coq_xO x4 ->
                (match x4 with
                 | Coq_xI _ ->
                   let { coq_Box_x86_op_PADD_0 = box_x86_op_PADD_0;
                     coq_Box_x86_op_PADD_1 = box_x86_op_PADD_1 } = Obj.magic a
                   in
                   let { coq_Box_x86_op_PADD_0 = box_x86_op_PADD_2;
                     coq_Box_x86_op_PADD_1 = box_x86_op_PADD_3 } = Obj.magic b
                   in
                   (&&) (velem_eqb box_x86_op_PADD_0 box_x86_op_PADD_2)
                     ((&&) (wsize_eqb box_x86_op_PADD_1 box_x86_op_PADD_3)
                       true)
                 | Coq_xO x5 ->
                   (match x5 with
                    | Coq_xH ->
                      let { coq_Box_x86_op_PADD_0 = box_x86_op_PADD_0;
                        coq_Box_x86_op_PADD_1 = box_x86_op_PADD_1 } =
                        Obj.magic a
                      in
                      let { coq_Box_x86_op_PADD_0 = box_x86_op_PADD_2;
                        coq_Box_x86_op_PADD_1 = box_x86_op_PADD_3 } =
                        Obj.magic b
                      in
                      (&&) (velem_eqb box_x86_op_PADD_0 box_x86_op_PADD_2)
                        ((&&) (wsize_eqb box_x86_op_PADD_1 box_x86_op_PADD_3)
                          true)
                    | _ -> true)
                 | Coq_xH -> (&&) (wsize_eqb (Obj.magic a) (Obj.magic b)) true)
              | Coq_xH -> true)
           | Coq_xO x3 ->
             (match x3 with
              | Coq_xI x4 ->
                (match x4 with
                 | Coq_xI _ ->
                   let { coq_Box_x86_op_PADD_0 = box_x86_op_PADD_0;
                     coq_Box_x86_op_PADD_1 = box_x86_op_PADD_1 } = Obj.magic a
                   in
                   let { coq_Box_x86_op_PADD_0 = box_x86_op_PADD_2;
                     coq_Box_x86_op_PADD_1 = box_x86_op_PADD_3 } = Obj.magic b
                   in
                   (&&) (velem_eqb box_x86_op_PADD_0 box_x86_op_PADD_2)
                     ((&&) (wsize_eqb box_x86_op_PADD_1 box_x86_op_PADD_3)
                       true)
                 | Coq_xO x5 ->
                   (match x5 with
                    | Coq_xH ->
                      (&&) (wsize_eqb (Obj.magic a) (Obj.magic b)) true
                    | _ -> true)
                 | Coq_xH -> (&&) (wsize_eqb (Obj.magic a) (Obj.magic b)) true)
              | Coq_xO x4 ->
                (match x4 with
                 | Coq_xI _ ->
                   let { coq_Box_x86_op_PADD_0 = box_x86_op_PADD_0;
                     coq_Box_x86_op_PADD_1 = box_x86_op_PADD_1 } = Obj.magic a
                   in
                   let { coq_Box_x86_op_PADD_0 = box_x86_op_PADD_2;
                     coq_Box_x86_op_PADD_1 = box_x86_op_PADD_3 } = Obj.magic b
                   in
                   (&&) (velem_eqb box_x86_op_PADD_0 box_x86_op_PADD_2)
                     ((&&) (wsize_eqb box_x86_op_PADD_1 box_x86_op_PADD_3)
                       true)
                 | Coq_xO x5 ->
                   (match x5 with
                    | Coq_xH ->
                      (&&) (wsize_eqb (Obj.magic a) (Obj.magic b)) true
                    | _ -> true)
                 | Coq_xH -> (&&) (wsize_eqb (Obj.magic a) (Obj.magic b)) true)
              | Coq_xH -> (&&) (wsize_eqb (Obj.magic a) (Obj.magic b)) true)
           | Coq_xH -> (&&) (wsize_eqb (Obj.magic a) (Obj.magic b)) true)
        | Coq_xH -> (&&) (wsize_eqb (Obj.magic a) (Obj.magic b)) true)
     | Coq_xH ->
       let { coq_Box_x86_op_MOVSX_0 = box_x86_op_MOVSX_0;
         coq_Box_x86_op_MOVSX_1 = box_x86_op_MOVSX_1 } = Obj.magic a
       in
       let { coq_Box_x86_op_MOVSX_0 = box_x86_op_MOVSX_2;
         coq_Box_x86_op_MOVSX_1 = box_x86_op_MOVSX_3 } = Obj.magic b
       in
       (&&) (wsize_eqb box_x86_op_MOVSX_0 box_x86_op_MOVSX_2)
         ((&&) (wsize_eqb box_x86_op_MOVSX_1 box_x86_op_MOVSX_3) true))
  | Coq_xO x0 ->
    (match x0 with
     | Coq_xI x1 ->
       (match x1 with
        | Coq_xI x2 ->
          (match x2 with
           | Coq_xI x3 ->
             (match x3 with
              | Coq_xI x4 ->
                (match x4 with
                 | Coq_xI _ -> true
                 | Coq_xO _ ->
                   let { coq_Box_x86_op_PADD_0 = box_x86_op_PADD_0;
                     coq_Box_x86_op_PADD_1 = box_x86_op_PADD_1 } = Obj.magic a
                   in
                   let { coq_Box_x86_op_PADD_0 = box_x86_op_PADD_2;
                     coq_Box_x86_op_PADD_1 = box_x86_op_PADD_3 } = Obj.magic b
                   in
                   (&&) (velem_eqb box_x86_op_PADD_0 box_x86_op_PADD_2)
                     ((&&) (wsize_eqb box_x86_op_PADD_1 box_x86_op_PADD_3)
                       true)
                 | Coq_xH -> (&&) (wsize_eqb (Obj.magic a) (Obj.magic b)) true)
              | Coq_xO x4 ->
                (match x4 with
                 | Coq_xI _ -> true
                 | Coq_xO x5 ->
                   (match x5 with
                    | Coq_xI _ -> true
                    | _ -> (&&) (wsize_eqb (Obj.magic a) (Obj.magic b)) true)
                 | Coq_xH -> (&&) (wsize_eqb (Obj.magic a) (Obj.magic b)) true)
              | Coq_xH -> (&&) (wsize_eqb (Obj.magic a) (Obj.magic b)) true)
           | Coq_xO x3 ->
             (match x3 with
              | Coq_xI x4 ->
                (match x4 with
                 | Coq_xI _ -> true
                 | Coq_xO x5 ->
                   (match x5 with
                    | Coq_xH ->
                      let { coq_Box_x86_op_PADD_0 = box_x86_op_PADD_0;
                        coq_Box_x86_op_PADD_1 = box_x86_op_PADD_1 } =
                        Obj.magic a
                      in
                      let { coq_Box_x86_op_PADD_0 = box_x86_op_PADD_2;
                        coq_Box_x86_op_PADD_1 = box_x86_op_PADD_3 } =
                        Obj.magic b
                      in
                      (&&) (velem_eqb box_x86_op_PADD_0 box_x86_op_PADD_2)
                        ((&&) (wsize_eqb box_x86_op_PADD_1 box_x86_op_PADD_3)
                          true)
                    | _ -> true)
                 | Coq_xH -> (&&) (wsize_eqb (Obj.magic a) (Obj.magic b)) true)
              | Coq_xO x4 ->
                (match x4 with
                 | Coq_xO x5 ->
                   (match x5 with
                    | Coq_xI _ -> true
                    | _ -> (&&) (wsize_eqb (Obj.magic a) (Obj.magic b)) true)
                 | _ -> (&&) (wsize_eqb (Obj.magic a) (Obj.magic b)) true)
              | Coq_xH -> (&&) (wsize_eqb (Obj.magic a) (Obj.magic b)) true)
           | Coq_xH -> (&&) (wsize_eqb (Obj.magic a) (Obj.magic b)) true)
        | Coq_xO x2 ->
          (match x2 with
           | Coq_xI x3 ->
             (match x3 with
              | Coq_xI x4 ->
                (match x4 with
                 | Coq_xI _ ->
                   let { coq_Box_x86_op_PADD_0 = box_x86_op_PADD_0;
                     coq_Box_x86_op_PADD_1 = box_x86_op_PADD_1 } = Obj.magic a
                   in
                   let { coq_Box_x86_op_PADD_0 = box_x86_op_PADD_2;
                     coq_Box_x86_op_PADD_1 = box_x86_op_PADD_3 } = Obj.magic b
                   in
                   (&&) (velem_eqb box_x86_op_PADD_0 box_x86_op_PADD_2)
                     ((&&) (wsize_eqb box_x86_op_PADD_1 box_x86_op_PADD_3)
                       true)
                 | _ -> (&&) (wsize_eqb (Obj.magic a) (Obj.magic b)) true)
              | Coq_xO x4 ->
                (match x4 with
                 | Coq_xI _ -> true
                 | Coq_xO x5 ->
                   (match x5 with
                    | Coq_xI _ -> true
                    | Coq_xO _ ->
                      (&&) (wsize_eqb (Obj.magic a) (Obj.magic b)) true
                    | Coq_xH ->
                      let { coq_Box_x86_op_PADD_0 = box_x86_op_PADD_0;
                        coq_Box_x86_op_PADD_1 = box_x86_op_PADD_1 } =
                        Obj.magic a
                      in
                      let { coq_Box_x86_op_PADD_0 = box_x86_op_PADD_2;
                        coq_Box_x86_op_PADD_1 = box_x86_op_PADD_3 } =
                        Obj.magic b
                      in
                      (&&) (velem_eqb box_x86_op_PADD_0 box_x86_op_PADD_2)
                        ((&&) (wsize_eqb box_x86_op_PADD_1 box_x86_op_PADD_3)
                          true))
                 | Coq_xH -> (&&) (wsize_eqb (Obj.magic a) (Obj.magic b)) true)
              | Coq_xH -> true)
           | Coq_xO x3 ->
             (match x3 with
              | Coq_xI x4 ->
                (match x4 with
                 | Coq_xI _ ->
                   let { coq_Box_x86_op_PADD_0 = box_x86_op_PADD_0;
                     coq_Box_x86_op_PADD_1 = box_x86_op_PADD_1 } = Obj.magic a
                   in
                   let { coq_Box_x86_op_PADD_0 = box_x86_op_PADD_2;
                     coq_Box_x86_op_PADD_1 = box_x86_op_PADD_3 } = Obj.magic b
                   in
                   (&&) (velem_eqb box_x86_op_PADD_0 box_x86_op_PADD_2)
                     ((&&) (wsize_eqb box_x86_op_PADD_1 box_x86_op_PADD_3)
                       true)
                 | Coq_xO x5 ->
                   (match x5 with
                    | Coq_xH ->
                      (&&) (velem_eqb (Obj.magic a) (Obj.magic b)) true
                    | _ -> true)
                 | Coq_xH -> (&&) (wsize_eqb (Obj.magic a) (Obj.magic b)) true)
              | Coq_xO x4 ->
                (match x4 with
                 | Coq_xO x5 ->
                   (match x5 with
                    | Coq_xH ->
                      let { coq_Box_x86_op_VPMOVSX_0 = box_x86_op_VPMOVSX_0;
                        coq_Box_x86_op_VPMOVSX_1 = box_x86_op_VPMOVSX_1;
                        coq_Box_x86_op_VPMOVSX_2 = box_x86_op_VPMOVSX_2;
                        coq_Box_x86_op_VPMOVSX_3 = box_x86_op_VPMOVSX_3 } =
                        Obj.magic a
                      in
                      let { coq_Box_x86_op_VPMOVSX_0 = box_x86_op_VPMOVSX_4;
                        coq_Box_x86_op_VPMOVSX_1 = box_x86_op_VPMOVSX_5;
                        coq_Box_x86_op_VPMOVSX_2 = box_x86_op_VPMOVSX_6;
                        coq_Box_x86_op_VPMOVSX_3 = box_x86_op_VPMOVSX_7 } =
                        Obj.magic b
                      in
                      (&&)
                        (velem_eqb box_x86_op_VPMOVSX_0 box_x86_op_VPMOVSX_4)
                        ((&&)
                          (wsize_eqb box_x86_op_VPMOVSX_1
                            box_x86_op_VPMOVSX_5)
                          ((&&)
                            (velem_eqb box_x86_op_VPMOVSX_2
                              box_x86_op_VPMOVSX_6)
                            ((&&)
                              (wsize_eqb box_x86_op_VPMOVSX_3
                                box_x86_op_VPMOVSX_7) true)))
                    | _ -> true)
                 | _ -> (&&) (wsize_eqb (Obj.magic a) (Obj.magic b)) true)
              | Coq_xH -> (&&) (wsize_eqb (Obj.magic a) (Obj.magic b)) true)
           | Coq_xH -> (&&) (wsize_eqb (Obj.magic a) (Obj.magic b)) true)
        | Coq_xH -> (&&) (wsize_eqb (Obj.magic a) (Obj.magic b)) true)
     | Coq_xO x1 ->
       (match x1 with
        | Coq_xI x2 ->
          (match x2 with
           | Coq_xI x3 ->
             (match x3 with
              | Coq_xI x4 ->
                (match x4 with
                 | Coq_xH ->
                   let { coq_Box_x86_op_PADD_0 = box_x86_op_PADD_0;
                     coq_Box_x86_op_PADD_1 = box_x86_op_PADD_1 } = Obj.magic a
                   in
                   let { coq_Box_x86_op_PADD_0 = box_x86_op_PADD_2;
                     coq_Box_x86_op_PADD_1 = box_x86_op_PADD_3 } = Obj.magic b
                   in
                   (&&) (velem_eqb box_x86_op_PADD_0 box_x86_op_PADD_2)
                     ((&&) (wsize_eqb box_x86_op_PADD_1 box_x86_op_PADD_3)
                       true)
                 | _ -> (&&) (wsize_eqb (Obj.magic a) (Obj.magic b)) true)
              | Coq_xO x4 ->
                (match x4 with
                 | Coq_xI _ -> true
                 | Coq_xO x5 ->
                   (match x5 with
                    | Coq_xI _ -> true
                    | _ -> (&&) (wsize_eqb (Obj.magic a) (Obj.magic b)) true)
                 | Coq_xH -> (&&) (wsize_eqb (Obj.magic a) (Obj.magic b)) true)
              | Coq_xH -> (&&) (wsize_eqb (Obj.magic a) (Obj.magic b)) true)
           | Coq_xO x3 ->
             (match x3 with
              | Coq_xI x4 ->
                (match x4 with
                 | Coq_xO x5 ->
                   (match x5 with
                    | Coq_xI _ -> true
                    | Coq_xO _ ->
                      (&&) (wsize_eqb (Obj.magic a) (Obj.magic b)) true
                    | Coq_xH ->
                      let { coq_Box_x86_op_PADD_0 = box_x86_op_PADD_0;
                        coq_Box_x86_op_PADD_1 = box_x86_op_PADD_1 } =
                        Obj.magic a
                      in
                      let { coq_Box_x86_op_PADD_0 = box_x86_op_PADD_2;
                        coq_Box_x86_op_PADD_1 = box_x86_op_PADD_3 } =
                        Obj.magic b
                      in
                      (&&) (velem_eqb box_x86_op_PADD_0 box_x86_op_PADD_2)
                        ((&&) (wsize_eqb box_x86_op_PADD_1 box_x86_op_PADD_3)
                          true))
                 | _ -> (&&) (wsize_eqb (Obj.magic a) (Obj.magic b)) true)
              | Coq_xO x4 ->
                (match x4 with
                 | Coq_xO x5 ->
                   (match x5 with
                    | Coq_xH ->
                      (&&) (wsize_eqb (Obj.magic a) (Obj.magic b)) true
                    | _ -> true)
                 | _ -> (&&) (wsize_eqb (Obj.magic a) (Obj.magic b)) true)
              | Coq_xH -> (&&) (wsize_eqb (Obj.magic a) (Obj.magic b)) true)
           | Coq_xH -> (&&) (wsize_eqb (Obj.magic a) (Obj.magic b)) true)
        | Coq_xO x2 ->
          (match x2 with
           | Coq_xI x3 ->
             (match x3 with
              | Coq_xI x4 ->
                (match x4 with
                 | Coq_xI _ ->
                   let { coq_Box_x86_op_PADD_0 = box_x86_op_PADD_0;
                     coq_Box_x86_op_PADD_1 = box_x86_op_PADD_1 } = Obj.magic a
                   in
                   let { coq_Box_x86_op_PADD_0 = box_x86_op_PADD_2;
                     coq_Box_x86_op_PADD_1 = box_x86_op_PADD_3 } = Obj.magic b
                   in
                   (&&) (velem_eqb box_x86_op_PADD_0 box_x86_op_PADD_2)
                     ((&&) (wsize_eqb box_x86_op_PADD_1 box_x86_op_PADD_3)
                       true)
                 | _ -> (&&) (wsize_eqb (Obj.magic a) (Obj.magic b)) true)
              | Coq_xO x4 ->
                (match x4 with
                 | Coq_xI _ ->
                   let { coq_Box_x86_op_PADD_0 = box_x86_op_PADD_0;
                     coq_Box_x86_op_PADD_1 = box_x86_op_PADD_1 } = Obj.magic a
                   in
                   let { coq_Box_x86_op_PADD_0 = box_x86_op_PADD_2;
                     coq_Box_x86_op_PADD_1 = box_x86_op_PADD_3 } = Obj.magic b
                   in
                   (&&) (velem_eqb box_x86_op_PADD_0 box_x86_op_PADD_2)
                     ((&&) (wsize_eqb box_x86_op_PADD_1 box_x86_op_PADD_3)
                       true)
                 | Coq_xO x5 ->
                   (match x5 with
                    | Coq_xI _ -> true
                    | Coq_xO _ ->
                      (&&) (wsize_eqb (Obj.magic a) (Obj.magic b)) true
                    | Coq_xH ->
                      let { coq_Box_x86_op_PADD_0 = box_x86_op_PADD_0;
                        coq_Box_x86_op_PADD_1 = box_x86_op_PADD_1 } =
                        Obj.magic a
                      in
                      let { coq_Box_x86_op_PADD_0 = box_x86_op_PADD_2;
                        coq_Box_x86_op_PADD_1 = box_x86_op_PADD_3 } =
                        Obj.magic b
                      in
                      (&&) (velem_eqb box_x86_op_PADD_0 box_x86_op_PADD_2)
                        ((&&) (wsize_eqb box_x86_op_PADD_1 box_x86_op_PADD_3)
                          true))
                 | Coq_xH -> (&&) (wsize_eqb (Obj.magic a) (Obj.magic b)) true)
              | Coq_xH -> (&&) (wsize_eqb (Obj.magic a) (Obj.magic b)) true)
           | Coq_xO x3 ->
             (match x3 with
              | Coq_xI x4 ->
                (match x4 with
                 | Coq_xI _ ->
                   let { coq_Box_x86_op_PADD_0 = box_x86_op_PADD_0;
                     coq_Box_x86_op_PADD_1 = box_x86_op_PADD_1 } = Obj.magic a
                   in
                   let { coq_Box_x86_op_PADD_0 = box_x86_op_PADD_2;
                     coq_Box_x86_op_PADD_1 = box_x86_op_PADD_3 } = Obj.magic b
                   in
                   (&&) (velem_eqb box_x86_op_PADD_0 box_x86_op_PADD_2)
                     ((&&) (wsize_eqb box_x86_op_PADD_1 box_x86_op_PADD_3)
                       true)
                 | Coq_xO x5 ->
                   (match x5 with
                    | Coq_xH ->
                      (&&) (wsize_eqb (Obj.magic a) (Obj.magic b)) true
                    | _ -> true)
                 | Coq_xH -> (&&) (wsize_eqb (Obj.magic a) (Obj.magic b)) true)
              | Coq_xO x4 ->
                (match x4 with
                 | Coq_xI _ ->
                   let { coq_Box_x86_op_PADD_0 = box_x86_op_PADD_0;
                     coq_Box_x86_op_PADD_1 = box_x86_op_PADD_1 } = Obj.magic a
                   in
                   let { coq_Box_x86_op_PADD_0 = box_x86_op_PADD_2;
                     coq_Box_x86_op_PADD_1 = box_x86_op_PADD_3 } = Obj.magic b
                   in
                   (&&) (velem_eqb box_x86_op_PADD_0 box_x86_op_PADD_2)
                     ((&&) (wsize_eqb box_x86_op_PADD_1 box_x86_op_PADD_3)
                       true)
                 | Coq_xO x5 ->
                   (match x5 with
                    | Coq_xH ->
                      (&&) (wsize_eqb (Obj.magic a) (Obj.magic b)) true
                    | _ -> true)
                 | Coq_xH -> (&&) (wsize_eqb (Obj.magic a) (Obj.magic b)) true)
              | Coq_xH -> (&&) (wsize_eqb (Obj.magic a) (Obj.magic b)) true)
           | Coq_xH -> (&&) (wsize_eqb (Obj.magic a) (Obj.magic b)) true)
        | Coq_xH -> (&&) (wsize_eqb (Obj.magic a) (Obj.magic b)) true)
     | Coq_xH ->
       let { coq_Box_x86_op_MOVSX_0 = box_x86_op_MOVSX_0;
         coq_Box_x86_op_MOVSX_1 = box_x86_op_MOVSX_1 } = Obj.magic a
       in
       let { coq_Box_x86_op_MOVSX_0 = box_x86_op_MOVSX_2;
         coq_Box_x86_op_MOVSX_1 = box_x86_op_MOVSX_3 } = Obj.magic b
       in
       (&&) (wsize_eqb box_x86_op_MOVSX_0 box_x86_op_MOVSX_2)
         ((&&) (wsize_eqb box_x86_op_MOVSX_1 box_x86_op_MOVSX_3) true))
  | Coq_xH -> (&&) (wsize_eqb (Obj.magic a) (Obj.magic b)) true

(** val x86_op_eqb : x86_op -> x86_op -> bool **)

let x86_op_eqb x1 x2 =
  match x1 with
  | MOV h ->
    eqb_body x86_op_tag x86_op_fields
      (Obj.magic x86_op_eqb_fields (fun _ _ -> true)) (x86_op_tag (MOV h)) h
      x2
  | MOVSX (h, h0) ->
    eqb_body x86_op_tag x86_op_fields
      (Obj.magic x86_op_eqb_fields (fun _ _ -> true))
      (x86_op_tag (MOVSX (h, h0))) { coq_Box_x86_op_MOVSX_0 = h;
      coq_Box_x86_op_MOVSX_1 = h0 } x2
  | MOVZX (h, h0) ->
    eqb_body x86_op_tag x86_op_fields
      (Obj.magic x86_op_eqb_fields (fun _ _ -> true))
      (x86_op_tag (MOVZX (h, h0))) { coq_Box_x86_op_MOVSX_0 = h;
      coq_Box_x86_op_MOVSX_1 = h0 } x2
  | CMOVcc h ->
    eqb_body x86_op_tag x86_op_fields
      (Obj.magic x86_op_eqb_fields (fun _ _ -> true)) (x86_op_tag (CMOVcc h))
      h x2
  | XCHG h ->
    eqb_body x86_op_tag x86_op_fields
      (Obj.magic x86_op_eqb_fields (fun _ _ -> true)) (x86_op_tag (XCHG h)) h
      x2
  | ADD h ->
    eqb_body x86_op_tag x86_op_fields
      (Obj.magic x86_op_eqb_fields (fun _ _ -> true)) (x86_op_tag (ADD h)) h
      x2
  | SUB h ->
    eqb_body x86_op_tag x86_op_fields
      (Obj.magic x86_op_eqb_fields (fun _ _ -> true)) (x86_op_tag (SUB h)) h
      x2
  | MUL h ->
    eqb_body x86_op_tag x86_op_fields
      (Obj.magic x86_op_eqb_fields (fun _ _ -> true)) (x86_op_tag (MUL h)) h
      x2
  | IMUL h ->
    eqb_body x86_op_tag x86_op_fields
      (Obj.magic x86_op_eqb_fields (fun _ _ -> true)) (x86_op_tag (IMUL h)) h
      x2
  | IMULr h ->
    eqb_body x86_op_tag x86_op_fields
      (Obj.magic x86_op_eqb_fields (fun _ _ -> true)) (x86_op_tag (IMULr h))
      h x2
  | IMULri h ->
    eqb_body x86_op_tag x86_op_fields
      (Obj.magic x86_op_eqb_fields (fun _ _ -> true)) (x86_op_tag (IMULri h))
      h x2
  | DIV h ->
    eqb_body x86_op_tag x86_op_fields
      (Obj.magic x86_op_eqb_fields (fun _ _ -> true)) (x86_op_tag (DIV h)) h
      x2
  | IDIV h ->
    eqb_body x86_op_tag x86_op_fields
      (Obj.magic x86_op_eqb_fields (fun _ _ -> true)) (x86_op_tag (IDIV h)) h
      x2
  | CQO h ->
    eqb_body x86_op_tag x86_op_fields
      (Obj.magic x86_op_eqb_fields (fun _ _ -> true)) (x86_op_tag (CQO h)) h
      x2
  | ADC h ->
    eqb_body x86_op_tag x86_op_fields
      (Obj.magic x86_op_eqb_fields (fun _ _ -> true)) (x86_op_tag (ADC h)) h
      x2
  | SBB h ->
    eqb_body x86_op_tag x86_op_fields
      (Obj.magic x86_op_eqb_fields (fun _ _ -> true)) (x86_op_tag (SBB h)) h
      x2
  | NEG h ->
    eqb_body x86_op_tag x86_op_fields
      (Obj.magic x86_op_eqb_fields (fun _ _ -> true)) (x86_op_tag (NEG h)) h
      x2
  | INC h ->
    eqb_body x86_op_tag x86_op_fields
      (Obj.magic x86_op_eqb_fields (fun _ _ -> true)) (x86_op_tag (INC h)) h
      x2
  | DEC h ->
    eqb_body x86_op_tag x86_op_fields
      (Obj.magic x86_op_eqb_fields (fun _ _ -> true)) (x86_op_tag (DEC h)) h
      x2
  | LZCNT h ->
    eqb_body x86_op_tag x86_op_fields
      (Obj.magic x86_op_eqb_fields (fun _ _ -> true)) (x86_op_tag (LZCNT h))
      h x2
  | TZCNT h ->
    eqb_body x86_op_tag x86_op_fields
      (Obj.magic x86_op_eqb_fields (fun _ _ -> true)) (x86_op_tag (TZCNT h))
      h x2
  | BSR h ->
    eqb_body x86_op_tag x86_op_fields
      (Obj.magic x86_op_eqb_fields (fun _ _ -> true)) (x86_op_tag (BSR h)) h
      x2
  | BT h ->
    eqb_body x86_op_tag x86_op_fields
      (Obj.magic x86_op_eqb_fields (fun _ _ -> true)) (x86_op_tag (BT h)) h x2
  | LEA h ->
    eqb_body x86_op_tag x86_op_fields
      (Obj.magic x86_op_eqb_fields (fun _ _ -> true)) (x86_op_tag (LEA h)) h
      x2
  | TEST h ->
    eqb_body x86_op_tag x86_op_fields
      (Obj.magic x86_op_eqb_fields (fun _ _ -> true)) (x86_op_tag (TEST h)) h
      x2
  | CMP h ->
    eqb_body x86_op_tag x86_op_fields
      (Obj.magic x86_op_eqb_fields (fun _ _ -> true)) (x86_op_tag (CMP h)) h
      x2
  | AND h ->
    eqb_body x86_op_tag x86_op_fields
      (Obj.magic x86_op_eqb_fields (fun _ _ -> true)) (x86_op_tag (AND h)) h
      x2
  | ANDN h ->
    eqb_body x86_op_tag x86_op_fields
      (Obj.magic x86_op_eqb_fields (fun _ _ -> true)) (x86_op_tag (ANDN h)) h
      x2
  | OR h ->
    eqb_body x86_op_tag x86_op_fields
      (Obj.magic x86_op_eqb_fields (fun _ _ -> true)) (x86_op_tag (OR h)) h x2
  | XOR h ->
    eqb_body x86_op_tag x86_op_fields
      (Obj.magic x86_op_eqb_fields (fun _ _ -> true)) (x86_op_tag (XOR h)) h
      x2
  | NOT h ->
    eqb_body x86_op_tag x86_op_fields
      (Obj.magic x86_op_eqb_fields (fun _ _ -> true)) (x86_op_tag (NOT h)) h
      x2
  | ROR h ->
    eqb_body x86_op_tag x86_op_fields
      (Obj.magic x86_op_eqb_fields (fun _ _ -> true)) (x86_op_tag (ROR h)) h
      x2
  | ROL h ->
    eqb_body x86_op_tag x86_op_fields
      (Obj.magic x86_op_eqb_fields (fun _ _ -> true)) (x86_op_tag (ROL h)) h
      x2
  | RCR h ->
    eqb_body x86_op_tag x86_op_fields
      (Obj.magic x86_op_eqb_fields (fun _ _ -> true)) (x86_op_tag (RCR h)) h
      x2
  | RCL h ->
    eqb_body x86_op_tag x86_op_fields
      (Obj.magic x86_op_eqb_fields (fun _ _ -> true)) (x86_op_tag (RCL h)) h
      x2
  | SHL h ->
    eqb_body x86_op_tag x86_op_fields
      (Obj.magic x86_op_eqb_fields (fun _ _ -> true)) (x86_op_tag (SHL h)) h
      x2
  | SHR h ->
    eqb_body x86_op_tag x86_op_fields
      (Obj.magic x86_op_eqb_fields (fun _ _ -> true)) (x86_op_tag (SHR h)) h
      x2
  | SAL h ->
    eqb_body x86_op_tag x86_op_fields
      (Obj.magic x86_op_eqb_fields (fun _ _ -> true)) (x86_op_tag (SAL h)) h
      x2
  | SAR h ->
    eqb_body x86_op_tag x86_op_fields
      (Obj.magic x86_op_eqb_fields (fun _ _ -> true)) (x86_op_tag (SAR h)) h
      x2
  | SHLD h ->
    eqb_body x86_op_tag x86_op_fields
      (Obj.magic x86_op_eqb_fields (fun _ _ -> true)) (x86_op_tag (SHLD h)) h
      x2
  | SHRD h ->
    eqb_body x86_op_tag x86_op_fields
      (Obj.magic x86_op_eqb_fields (fun _ _ -> true)) (x86_op_tag (SHRD h)) h
      x2
  | RORX h ->
    eqb_body x86_op_tag x86_op_fields
      (Obj.magic x86_op_eqb_fields (fun _ _ -> true)) (x86_op_tag (RORX h)) h
      x2
  | SARX h ->
    eqb_body x86_op_tag x86_op_fields
      (Obj.magic x86_op_eqb_fields (fun _ _ -> true)) (x86_op_tag (SARX h)) h
      x2
  | SHRX h ->
    eqb_body x86_op_tag x86_op_fields
      (Obj.magic x86_op_eqb_fields (fun _ _ -> true)) (x86_op_tag (SHRX h)) h
      x2
  | SHLX h ->
    eqb_body x86_op_tag x86_op_fields
      (Obj.magic x86_op_eqb_fields (fun _ _ -> true)) (x86_op_tag (SHLX h)) h
      x2
  | MULX_lo_hi h ->
    eqb_body x86_op_tag x86_op_fields
      (Obj.magic x86_op_eqb_fields (fun _ _ -> true))
      (x86_op_tag (MULX_lo_hi h)) h x2
  | ADCX h ->
    eqb_body x86_op_tag x86_op_fields
      (Obj.magic x86_op_eqb_fields (fun _ _ -> true)) (x86_op_tag (ADCX h)) h
      x2
  | ADOX h ->
    eqb_body x86_op_tag x86_op_fields
      (Obj.magic x86_op_eqb_fields (fun _ _ -> true)) (x86_op_tag (ADOX h)) h
      x2
  | BSWAP h ->
    eqb_body x86_op_tag x86_op_fields
      (Obj.magic x86_op_eqb_fields (fun _ _ -> true)) (x86_op_tag (BSWAP h))
      h x2
  | POPCNT h ->
    eqb_body x86_op_tag x86_op_fields
      (Obj.magic x86_op_eqb_fields (fun _ _ -> true)) (x86_op_tag (POPCNT h))
      h x2
  | BTR h ->
    eqb_body x86_op_tag x86_op_fields
      (Obj.magic x86_op_eqb_fields (fun _ _ -> true)) (x86_op_tag (BTR h)) h
      x2
  | BTS h ->
    eqb_body x86_op_tag x86_op_fields
      (Obj.magic x86_op_eqb_fields (fun _ _ -> true)) (x86_op_tag (BTS h)) h
      x2
  | PEXT h ->
    eqb_body x86_op_tag x86_op_fields
      (Obj.magic x86_op_eqb_fields (fun _ _ -> true)) (x86_op_tag (PEXT h)) h
      x2
  | PDEP h ->
    eqb_body x86_op_tag x86_op_fields
      (Obj.magic x86_op_eqb_fields (fun _ _ -> true)) (x86_op_tag (PDEP h)) h
      x2
  | MOVX h ->
    eqb_body x86_op_tag x86_op_fields
      (Obj.magic x86_op_eqb_fields (fun _ _ -> true)) (x86_op_tag (MOVX h)) h
      x2
  | PADD (h, h0) ->
    eqb_body x86_op_tag x86_op_fields
      (Obj.magic x86_op_eqb_fields (fun _ _ -> true))
      (x86_op_tag (PADD (h, h0))) { coq_Box_x86_op_PADD_0 = h;
      coq_Box_x86_op_PADD_1 = h0 } x2
  | MOVD h ->
    eqb_body x86_op_tag x86_op_fields
      (Obj.magic x86_op_eqb_fields (fun _ _ -> true)) (x86_op_tag (MOVD h)) h
      x2
  | MOVV h ->
    eqb_body x86_op_tag x86_op_fields
      (Obj.magic x86_op_eqb_fields (fun _ _ -> true)) (x86_op_tag (MOVV h)) h
      x2
  | VMOV h ->
    eqb_body x86_op_tag x86_op_fields
      (Obj.magic x86_op_eqb_fields (fun _ _ -> true)) (x86_op_tag (VMOV h)) h
      x2
  | VMOVDQA h ->
    eqb_body x86_op_tag x86_op_fields
      (Obj.magic x86_op_eqb_fields (fun _ _ -> true))
      (x86_op_tag (VMOVDQA h)) h x2
  | VMOVDQU h ->
    eqb_body x86_op_tag x86_op_fields
      (Obj.magic x86_op_eqb_fields (fun _ _ -> true))
      (x86_op_tag (VMOVDQU h)) h x2
  | VPMOVSX (h, h0, h1, h2) ->
    eqb_body x86_op_tag x86_op_fields
      (Obj.magic x86_op_eqb_fields (fun _ _ -> true))
      (x86_op_tag (VPMOVSX (h, h0, h1, h2))) { coq_Box_x86_op_VPMOVSX_0 = h;
      coq_Box_x86_op_VPMOVSX_1 = h0; coq_Box_x86_op_VPMOVSX_2 = h1;
      coq_Box_x86_op_VPMOVSX_3 = h2 } x2
  | VPMOVZX (h, h0, h1, h2) ->
    eqb_body x86_op_tag x86_op_fields
      (Obj.magic x86_op_eqb_fields (fun _ _ -> true))
      (x86_op_tag (VPMOVZX (h, h0, h1, h2))) { coq_Box_x86_op_VPMOVSX_0 = h;
      coq_Box_x86_op_VPMOVSX_1 = h0; coq_Box_x86_op_VPMOVSX_2 = h1;
      coq_Box_x86_op_VPMOVSX_3 = h2 } x2
  | VPAND h ->
    eqb_body x86_op_tag x86_op_fields
      (Obj.magic x86_op_eqb_fields (fun _ _ -> true)) (x86_op_tag (VPAND h))
      h x2
  | VPANDN h ->
    eqb_body x86_op_tag x86_op_fields
      (Obj.magic x86_op_eqb_fields (fun _ _ -> true)) (x86_op_tag (VPANDN h))
      h x2
  | VPOR h ->
    eqb_body x86_op_tag x86_op_fields
      (Obj.magic x86_op_eqb_fields (fun _ _ -> true)) (x86_op_tag (VPOR h)) h
      x2
  | VPXOR h ->
    eqb_body x86_op_tag x86_op_fields
      (Obj.magic x86_op_eqb_fields (fun _ _ -> true)) (x86_op_tag (VPXOR h))
      h x2
  | VPADD (h, h0) ->
    eqb_body x86_op_tag x86_op_fields
      (Obj.magic x86_op_eqb_fields (fun _ _ -> true))
      (x86_op_tag (VPADD (h, h0))) { coq_Box_x86_op_PADD_0 = h;
      coq_Box_x86_op_PADD_1 = h0 } x2
  | VPSUB (h, h0) ->
    eqb_body x86_op_tag x86_op_fields
      (Obj.magic x86_op_eqb_fields (fun _ _ -> true))
      (x86_op_tag (VPSUB (h, h0))) { coq_Box_x86_op_PADD_0 = h;
      coq_Box_x86_op_PADD_1 = h0 } x2
  | VPAVG (h, h0) ->
    eqb_body x86_op_tag x86_op_fields
      (Obj.magic x86_op_eqb_fields (fun _ _ -> true))
      (x86_op_tag (VPAVG (h, h0))) { coq_Box_x86_op_PADD_0 = h;
      coq_Box_x86_op_PADD_1 = h0 } x2
  | VPMULL (h, h0) ->
    eqb_body x86_op_tag x86_op_fields
      (Obj.magic x86_op_eqb_fields (fun _ _ -> true))
      (x86_op_tag (VPMULL (h, h0))) { coq_Box_x86_op_PADD_0 = h;
      coq_Box_x86_op_PADD_1 = h0 } x2
  | VPMULH h ->
    eqb_body x86_op_tag x86_op_fields
      (Obj.magic x86_op_eqb_fields (fun _ _ -> true)) (x86_op_tag (VPMULH h))
      h x2
  | VPMULHU h ->
    eqb_body x86_op_tag x86_op_fields
      (Obj.magic x86_op_eqb_fields (fun _ _ -> true))
      (x86_op_tag (VPMULHU h)) h x2
  | VPMULHRS h ->
    eqb_body x86_op_tag x86_op_fields
      (Obj.magic x86_op_eqb_fields (fun _ _ -> true))
      (x86_op_tag (VPMULHRS h)) h x2
  | VPMUL h ->
    eqb_body x86_op_tag x86_op_fields
      (Obj.magic x86_op_eqb_fields (fun _ _ -> true)) (x86_op_tag (VPMUL h))
      h x2
  | VPMULU h ->
    eqb_body x86_op_tag x86_op_fields
      (Obj.magic x86_op_eqb_fields (fun _ _ -> true)) (x86_op_tag (VPMULU h))
      h x2
  | VPEXTR h ->
    eqb_body x86_op_tag x86_op_fields
      (Obj.magic x86_op_eqb_fields (fun _ _ -> true)) (x86_op_tag (VPEXTR h))
      h x2
  | VPINSR h ->
    eqb_body x86_op_tag x86_op_fields
      (Obj.magic x86_op_eqb_fields (fun _ _ -> true)) (x86_op_tag (VPINSR h))
      h x2
  | VPSLL (h, h0) ->
    eqb_body x86_op_tag x86_op_fields
      (Obj.magic x86_op_eqb_fields (fun _ _ -> true))
      (x86_op_tag (VPSLL (h, h0))) { coq_Box_x86_op_PADD_0 = h;
      coq_Box_x86_op_PADD_1 = h0 } x2
  | VPSRL (h, h0) ->
    eqb_body x86_op_tag x86_op_fields
      (Obj.magic x86_op_eqb_fields (fun _ _ -> true))
      (x86_op_tag (VPSRL (h, h0))) { coq_Box_x86_op_PADD_0 = h;
      coq_Box_x86_op_PADD_1 = h0 } x2
  | VPSRA (h, h0) ->
    eqb_body x86_op_tag x86_op_fields
      (Obj.magic x86_op_eqb_fields (fun _ _ -> true))
      (x86_op_tag (VPSRA (h, h0))) { coq_Box_x86_op_PADD_0 = h;
      coq_Box_x86_op_PADD_1 = h0 } x2
  | VPSLLV (h, h0) ->
    eqb_body x86_op_tag x86_op_fields
      (Obj.magic x86_op_eqb_fields (fun _ _ -> true))
      (x86_op_tag (VPSLLV (h, h0))) { coq_Box_x86_op_PADD_0 = h;
      coq_Box_x86_op_PADD_1 = h0 } x2
  | VPSRLV (h, h0) ->
    eqb_body x86_op_tag x86_op_fields
      (Obj.magic x86_op_eqb_fields (fun _ _ -> true))
      (x86_op_tag (VPSRLV (h, h0))) { coq_Box_x86_op_PADD_0 = h;
      coq_Box_x86_op_PADD_1 = h0 } x2
  | VPSLLDQ h ->
    eqb_body x86_op_tag x86_op_fields
      (Obj.magic x86_op_eqb_fields (fun _ _ -> true))
      (x86_op_tag (VPSLLDQ h)) h x2
  | VPSRLDQ h ->
    eqb_body x86_op_tag x86_op_fields
      (Obj.magic x86_op_eqb_fields (fun _ _ -> true))
      (x86_op_tag (VPSRLDQ h)) h x2
  | VPSHUFB h ->
    eqb_body x86_op_tag x86_op_fields
      (Obj.magic x86_op_eqb_fields (fun _ _ -> true))
      (x86_op_tag (VPSHUFB h)) h x2
  | VPSHUFD h ->
    eqb_body x86_op_tag x86_op_fields
      (Obj.magic x86_op_eqb_fields (fun _ _ -> true))
      (x86_op_tag (VPSHUFD h)) h x2
  | VPSHUFHW h ->
    eqb_body x86_op_tag x86_op_fields
      (Obj.magic x86_op_eqb_fields (fun _ _ -> true))
      (x86_op_tag (VPSHUFHW h)) h x2
  | VPSHUFLW h ->
    eqb_body x86_op_tag x86_op_fields
      (Obj.magic x86_op_eqb_fields (fun _ _ -> true))
      (x86_op_tag (VPSHUFLW h)) h x2
  | VPBLEND (h, h0) ->
    eqb_body x86_op_tag x86_op_fields
      (Obj.magic x86_op_eqb_fields (fun _ _ -> true))
      (x86_op_tag (VPBLEND (h, h0))) { coq_Box_x86_op_PADD_0 = h;
      coq_Box_x86_op_PADD_1 = h0 } x2
  | BLENDV (h, h0) ->
    eqb_body x86_op_tag x86_op_fields
      (Obj.magic x86_op_eqb_fields (fun _ _ -> true))
      (x86_op_tag (BLENDV (h, h0))) { coq_Box_x86_op_PADD_0 = h;
      coq_Box_x86_op_PADD_1 = h0 } x2
  | VPACKUS (h, h0) ->
    eqb_body x86_op_tag x86_op_fields
      (Obj.magic x86_op_eqb_fields (fun _ _ -> true))
      (x86_op_tag (VPACKUS (h, h0))) { coq_Box_x86_op_PADD_0 = h;
      coq_Box_x86_op_PADD_1 = h0 } x2
  | VPACKSS (h, h0) ->
    eqb_body x86_op_tag x86_op_fields
      (Obj.magic x86_op_eqb_fields (fun _ _ -> true))
      (x86_op_tag (VPACKSS (h, h0))) { coq_Box_x86_op_PADD_0 = h;
      coq_Box_x86_op_PADD_1 = h0 } x2
  | VSHUFPS h ->
    eqb_body x86_op_tag x86_op_fields
      (Obj.magic x86_op_eqb_fields (fun _ _ -> true))
      (x86_op_tag (VSHUFPS h)) h x2
  | VPBROADCAST (h, h0) ->
    eqb_body x86_op_tag x86_op_fields
      (Obj.magic x86_op_eqb_fields (fun _ _ -> true))
      (x86_op_tag (VPBROADCAST (h, h0))) { coq_Box_x86_op_PADD_0 = h;
      coq_Box_x86_op_PADD_1 = h0 } x2
  | VMOVSHDUP h ->
    eqb_body x86_op_tag x86_op_fields
      (Obj.magic x86_op_eqb_fields (fun _ _ -> true))
      (x86_op_tag (VMOVSHDUP h)) h x2
  | VMOVSLDUP h ->
    eqb_body x86_op_tag x86_op_fields
      (Obj.magic x86_op_eqb_fields (fun _ _ -> true))
      (x86_op_tag (VMOVSLDUP h)) h x2
  | VPALIGNR h ->
    eqb_body x86_op_tag x86_op_fields
      (Obj.magic x86_op_eqb_fields (fun _ _ -> true))
      (x86_op_tag (VPALIGNR h)) h x2
  | VPUNPCKH (h, h0) ->
    eqb_body x86_op_tag x86_op_fields
      (Obj.magic x86_op_eqb_fields (fun _ _ -> true))
      (x86_op_tag (VPUNPCKH (h, h0))) { coq_Box_x86_op_PADD_0 = h;
      coq_Box_x86_op_PADD_1 = h0 } x2
  | VPUNPCKL (h, h0) ->
    eqb_body x86_op_tag x86_op_fields
      (Obj.magic x86_op_eqb_fields (fun _ _ -> true))
      (x86_op_tag (VPUNPCKL (h, h0))) { coq_Box_x86_op_PADD_0 = h;
      coq_Box_x86_op_PADD_1 = h0 } x2
  | MOVEMASK (h, h0) ->
    eqb_body x86_op_tag x86_op_fields
      (Obj.magic x86_op_eqb_fields (fun _ _ -> true))
      (x86_op_tag (MOVEMASK (h, h0))) { coq_Box_x86_op_PADD_0 = h;
      coq_Box_x86_op_PADD_1 = h0 } x2
  | VPCMPEQ (h, h0) ->
    eqb_body x86_op_tag x86_op_fields
      (Obj.magic x86_op_eqb_fields (fun _ _ -> true))
      (x86_op_tag (VPCMPEQ (h, h0))) { coq_Box_x86_op_PADD_0 = h;
      coq_Box_x86_op_PADD_1 = h0 } x2
  | VPCMPGT (h, h0) ->
    eqb_body x86_op_tag x86_op_fields
      (Obj.magic x86_op_eqb_fields (fun _ _ -> true))
      (x86_op_tag (VPCMPGT (h, h0))) { coq_Box_x86_op_PADD_0 = h;
      coq_Box_x86_op_PADD_1 = h0 } x2
  | VPSIGN (h, h0) ->
    eqb_body x86_op_tag x86_op_fields
      (Obj.magic x86_op_eqb_fields (fun _ _ -> true))
      (x86_op_tag (VPSIGN (h, h0))) { coq_Box_x86_op_PADD_0 = h;
      coq_Box_x86_op_PADD_1 = h0 } x2
  | VPMADDUBSW h ->
    eqb_body x86_op_tag x86_op_fields
      (Obj.magic x86_op_eqb_fields (fun _ _ -> true))
      (x86_op_tag (VPMADDUBSW h)) h x2
  | VPMADDWD h ->
    eqb_body x86_op_tag x86_op_fields
      (Obj.magic x86_op_eqb_fields (fun _ _ -> true))
      (x86_op_tag (VPMADDWD h)) h x2
  | VPMINU (h, h0) ->
    eqb_body x86_op_tag x86_op_fields
      (Obj.magic x86_op_eqb_fields (fun _ _ -> true))
      (x86_op_tag (VPMINU (h, h0))) { coq_Box_x86_op_PADD_0 = h;
      coq_Box_x86_op_PADD_1 = h0 } x2
  | VPMINS (h, h0) ->
    eqb_body x86_op_tag x86_op_fields
      (Obj.magic x86_op_eqb_fields (fun _ _ -> true))
      (x86_op_tag (VPMINS (h, h0))) { coq_Box_x86_op_PADD_0 = h;
      coq_Box_x86_op_PADD_1 = h0 } x2
  | VPMAXU (h, h0) ->
    eqb_body x86_op_tag x86_op_fields
      (Obj.magic x86_op_eqb_fields (fun _ _ -> true))
      (x86_op_tag (VPMAXU (h, h0))) { coq_Box_x86_op_PADD_0 = h;
      coq_Box_x86_op_PADD_1 = h0 } x2
  | VPMAXS (h, h0) ->
    eqb_body x86_op_tag x86_op_fields
      (Obj.magic x86_op_eqb_fields (fun _ _ -> true))
      (x86_op_tag (VPMAXS (h, h0))) { coq_Box_x86_op_PADD_0 = h;
      coq_Box_x86_op_PADD_1 = h0 } x2
  | VPABS (h, h0) ->
    eqb_body x86_op_tag x86_op_fields
      (Obj.magic x86_op_eqb_fields (fun _ _ -> true))
      (x86_op_tag (VPABS (h, h0))) { coq_Box_x86_op_PADD_0 = h;
      coq_Box_x86_op_PADD_1 = h0 } x2
  | VPTEST h ->
    eqb_body x86_op_tag x86_op_fields
      (Obj.magic x86_op_eqb_fields (fun _ _ -> true)) (x86_op_tag (VPTEST h))
      h x2
  | RDTSC h ->
    eqb_body x86_op_tag x86_op_fields
      (Obj.magic x86_op_eqb_fields (fun _ _ -> true)) (x86_op_tag (RDTSC h))
      h x2
  | RDTSCP h ->
    eqb_body x86_op_tag x86_op_fields
      (Obj.magic x86_op_eqb_fields (fun _ _ -> true)) (x86_op_tag (RDTSCP h))
      h x2
  | VAESDEC h ->
    eqb_body x86_op_tag x86_op_fields
      (Obj.magic x86_op_eqb_fields (fun _ _ -> true))
      (x86_op_tag (VAESDEC h)) h x2
  | VAESDECLAST h ->
    eqb_body x86_op_tag x86_op_fields
      (Obj.magic x86_op_eqb_fields (fun _ _ -> true))
      (x86_op_tag (VAESDECLAST h)) h x2
  | VAESENC h ->
    eqb_body x86_op_tag x86_op_fields
      (Obj.magic x86_op_eqb_fields (fun _ _ -> true))
      (x86_op_tag (VAESENC h)) h x2
  | VAESENCLAST h ->
    eqb_body x86_op_tag x86_op_fields
      (Obj.magic x86_op_eqb_fields (fun _ _ -> true))
      (x86_op_tag (VAESENCLAST h)) h x2
  | VPCLMULQDQ h ->
    eqb_body x86_op_tag x86_op_fields
      (Obj.magic x86_op_eqb_fields (fun _ _ -> true))
      (x86_op_tag (VPCLMULQDQ h)) h x2
  | x ->
    eqb_body x86_op_tag x86_op_fields
      (Obj.magic x86_op_eqb_fields (fun _ _ -> true)) (x86_op_tag x)
      Box_x86_op_SETcc x2

(** val x86_op_eqb_OK : x86_op -> x86_op -> reflect **)

let x86_op_eqb_OK =
  iffP2 x86_op_eqb

(** val x86_op_eqb_OK_sumbool : x86_op -> x86_op -> bool **)

let x86_op_eqb_OK_sumbool =
  reflect_dec x86_op_eqb x86_op_eqb_OK

(** val coq_HB_unnamed_factory_1 : x86_op Coq_hasDecEq.axioms_ **)

let coq_HB_unnamed_factory_1 =
  { Coq_hasDecEq.eq_op = x86_op_eqb; Coq_hasDecEq.eqP = x86_op_eqb_OK }

(** val x86_instr_decl_x86_op__canonical__eqtype_Equality :
    Equality.coq_type **)

let x86_instr_decl_x86_op__canonical__eqtype_Equality =
  Obj.magic coq_HB_unnamed_factory_1

(** val b_ty : ltype list **)

let b_ty =
  Coq_lbool :: []

(** val b4_ty : ltype list **)

let b4_ty =
  Coq_lbool :: (Coq_lbool :: (Coq_lbool :: (Coq_lbool :: [])))

(** val b5_ty : ltype list **)

let b5_ty =
  Coq_lbool :: (Coq_lbool :: (Coq_lbool :: (Coq_lbool :: (Coq_lbool :: []))))

(** val bw_ty : wsize -> ltype list **)

let bw_ty sz =
  Coq_lbool :: ((Coq_lword sz) :: [])

(** val bw2_ty : wsize -> ltype list **)

let bw2_ty sz =
  Coq_lbool :: ((Coq_lword sz) :: ((Coq_lword sz) :: []))

(** val b2w_ty : wsize -> ltype list **)

let b2w_ty sz =
  Coq_lbool :: (Coq_lbool :: ((Coq_lword sz) :: []))

(** val b4w_ty : wsize -> ltype list **)

let b4w_ty sz =
  Coq_lbool :: (Coq_lbool :: (Coq_lbool :: (Coq_lbool :: ((Coq_lword
    sz) :: []))))

(** val b5w_ty : wsize -> ltype list **)

let b5w_ty sz =
  Coq_lbool :: (Coq_lbool :: (Coq_lbool :: (Coq_lbool :: (Coq_lbool :: ((Coq_lword
    sz) :: [])))))

(** val b5w2_ty : wsize -> ltype list **)

let b5w2_ty sz =
  Coq_lbool :: (Coq_lbool :: (Coq_lbool :: (Coq_lbool :: (Coq_lbool :: ((Coq_lword
    sz) :: ((Coq_lword sz) :: []))))))

(** val w_ty : wsize -> ltype list **)

let w_ty sz =
  (Coq_lword sz) :: []

(** val w2_ty : wsize -> wsize -> ltype list **)

let w2_ty sz sz' =
  (Coq_lword sz) :: ((Coq_lword sz') :: [])

(** val w3_ty : wsize -> ltype list **)

let w3_ty sz =
  (Coq_lword sz) :: ((Coq_lword sz) :: ((Coq_lword sz) :: []))

(** val w8_ty : ltype list **)

let w8_ty =
  (Coq_lword U8) :: []

(** val w128_ty : ltype list **)

let w128_ty =
  (Coq_lword U128) :: []

(** val w256_ty : ltype list **)

let w256_ty =
  (Coq_lword U256) :: []

(** val w2b_ty : wsize -> wsize -> ltype list **)

let w2b_ty sz sz' =
  (Coq_lword sz) :: ((Coq_lword sz') :: (Coq_lbool :: []))

(** val ww8_ty : wsize -> ltype list **)

let ww8_ty sz =
  (Coq_lword sz) :: ((Coq_lword U8) :: [])

(** val ww8b_ty : wsize -> ltype list **)

let ww8b_ty sz =
  (Coq_lword sz) :: ((Coq_lword U8) :: (Coq_lbool :: []))

(** val w2w8_ty : wsize -> ltype list **)

let w2w8_ty sz =
  (Coq_lword sz) :: ((Coq_lword sz) :: ((Coq_lword U8) :: []))

(** val w128w8_ty : ltype list **)

let w128w8_ty =
  (Coq_lword U128) :: ((Coq_lword U8) :: [])

(** val w128ww8_ty : wsize -> ltype list **)

let w128ww8_ty sz =
  (Coq_lword U128) :: ((Coq_lword sz) :: ((Coq_lword U8) :: []))

(** val w256w8_ty : ltype list **)

let w256w8_ty =
  (Coq_lword U256) :: ((Coq_lword U8) :: [])

(** val w256w128w8_ty : ltype list **)

let w256w128w8_ty =
  (Coq_lword U256) :: ((Coq_lword U128) :: ((Coq_lword U8) :: []))

(** val w256x2w8_ty : ltype list **)

let w256x2w8_ty =
  (Coq_lword U256) :: ((Coq_lword U256) :: ((Coq_lword U8) :: []))

(** val coq_SF_of_word : wsize -> GRing.ComRing.sort -> bool **)

let coq_SF_of_word =
  msb

(** val coq_PF_of_word : wsize -> GRing.ComRing.sort -> bool **)

let coq_PF_of_word sz w =
  foldl xorb true
    (map (fun i0 -> wbit_n sz w i0)
      (iota O (S (S (S (S (S (S (S (S O))))))))))

(** val coq_ZF_of_word : wsize -> GRing.ComRing.sort -> bool **)

let coq_ZF_of_word sz w =
  eq_op
    (GRing.ComRing.Exports.coq_GRing_ComRing__to__eqtype_Equality (word sz))
    w
    (GRing.zero
      (GRing.ComRing.Exports.coq_GRing_ComRing__to__GRing_Nmodule (word sz)))

(** val rflags_of_bwop : wsize -> GRing.ComRing.sort -> sem_tuple **)

let rflags_of_bwop sz w =
  Obj.magic ((Some false), ((Some false), ((Some (coq_SF_of_word sz w)),
    ((Some (coq_PF_of_word sz w)), (Some (coq_ZF_of_word sz w))))))

(** val rflags_of_aluop :
    wsize -> GRing.ComRing.sort -> coq_Z -> coq_Z -> sem_tuple **)

let rflags_of_aluop sz w vu vs =
  Obj.magic ((Some
    (negb
      (eq_op coq_BinNums_Z__canonical__eqtype_Equality
        (Obj.magic wsigned sz w) (Obj.magic vs)))), ((Some
    (negb
      (eq_op coq_BinNums_Z__canonical__eqtype_Equality
        (Obj.magic wunsigned sz w) (Obj.magic vu)))), ((Some
    (coq_SF_of_word sz w)), ((Some (coq_PF_of_word sz w)), (Some
    (coq_ZF_of_word sz w))))))

(** val rflags_of_mul : bool -> sem_tuple **)

let rflags_of_mul ov =
  Obj.magic ((Some ov), ((Some ov), (None, (None, None))))

(** val rflags_of_div : sem_tuple **)

let rflags_of_div =
  Obj.magic (None, (None, (None, (None, None))))

(** val rflags_of_andn : wsize -> GRing.ComRing.sort -> sem_tuple **)

let rflags_of_andn sz w =
  Obj.magic ((Some false), ((Some false), ((Some (coq_SF_of_word sz w)),
    (None, (Some (coq_ZF_of_word sz w))))))

(** val rflags_None_w : wsize -> sem_ot -> sem_tuple **)

let rflags_None_w _ w =
  Obj.magic (None, (None, (None, (None, (None, w)))))

(** val rflags_of_aluop_nocf :
    wsize -> GRing.ComRing.sort -> coq_Z -> sem_tuple **)

let rflags_of_aluop_nocf sz w vs =
  Obj.magic ((Some
    (negb
      (eq_op coq_BinNums_Z__canonical__eqtype_Equality
        (Obj.magic wsigned sz w) (Obj.magic vs)))), ((Some
    (coq_SF_of_word sz w)), ((Some (coq_PF_of_word sz w)), (Some
    (coq_ZF_of_word sz w)))))

(** val flags_w :
    __ list -> ltuple -> wsize -> GRing.ComRing.sort -> ltuple **)

let flags_w l1 bs sz w =
  merge_tuple l1 (map (Obj.magic __) (map eval_ltype (w_ty sz))) bs w

(** val flags_w2 : __ list -> ltuple -> wsize -> sem_tuple -> ltuple **)

let flags_w2 l1 bs sz w =
  merge_tuple l1 (map (Obj.magic __) (map eval_ltype (w2_ty sz sz))) bs w

(** val rflags_of_aluop_w :
    wsize -> GRing.ComRing.sort -> coq_Z -> coq_Z -> ltuple **)

let rflags_of_aluop_w sz w vu vs =
  flags_w (map (Obj.magic __) (map eval_ltype b5_ty))
    (rflags_of_aluop sz w vu vs) sz w

(** val rflags_of_aluop_nocf_w :
    wsize -> GRing.ComRing.sort -> coq_Z -> ltuple **)

let rflags_of_aluop_nocf_w sz w vs =
  flags_w (map (Obj.magic __) (map eval_ltype b4_ty))
    (rflags_of_aluop_nocf sz w vs) sz w

(** val rflags_of_bwop_w : wsize -> GRing.ComRing.sort -> ltuple **)

let rflags_of_bwop_w sz w =
  flags_w (map (Obj.magic __) (map eval_ltype b5_ty)) (rflags_of_bwop sz w)
    sz w

(** val prim_8_64 : (wsize -> 'a1) -> 'a1 prim_constructor **)

let prim_8_64 f =
  let range = U64 :: (U32 :: (U16 :: (U8 :: []))) in
  PrimX86 ((map (fun x -> PVp x) range), (fun s ->
  match s with
  | PVp sz -> Some (f sz)
  | PVs (_, _) -> None
  | PVv (_, _) -> None
  | PVsv (_, _, _) -> None
  | PVx (_, _) -> None
  | PVvv (_, _, _, _) -> None))

(** val prim_16_64 : (wsize -> 'a1) -> 'a1 prim_constructor **)

let prim_16_64 f =
  let range = U64 :: (U32 :: (U16 :: [])) in
  PrimX86 ((map (fun x -> PVp x) range), (fun s ->
  match s with
  | PVp sz -> Some (f sz)
  | PVs (_, _) -> None
  | PVv (_, _) -> None
  | PVsv (_, _, _) -> None
  | PVx (_, _) -> None
  | PVvv (_, _, _, _) -> None))

(** val prim_32_64 : (wsize -> 'a1) -> 'a1 prim_constructor **)

let prim_32_64 f =
  let range = U64 :: (U32 :: []) in
  PrimX86 ((map (fun x -> PVp x) range), (fun s ->
  match s with
  | PVp sz -> Some (f sz)
  | PVs (_, _) -> None
  | PVv (_, _) -> None
  | PVsv (_, _, _) -> None
  | PVx (_, _) -> None
  | PVvv (_, _, _, _) -> None))

(** val prim_128_256 : (wsize -> 'a1) -> 'a1 prim_constructor **)

let prim_128_256 f =
  let range = U128 :: (U256 :: []) in
  PrimX86 ((map (fun x -> PVp x) range), (fun s ->
  match s with
  | PVp sz -> Some (f sz)
  | PVs (_, _) -> None
  | PVv (_, _) -> None
  | PVsv (_, _, _) -> None
  | PVx (_, _) -> None
  | PVvv (_, _, _, _) -> None))

(** val prim_movsx : (wsize -> wsize -> x86_op) -> x86_op prim_constructor **)

let prim_movsx =
  let range = (PVx (U16, U8)) :: ((PVx (U32, U8)) :: ((PVx (U64,
    U8)) :: ((PVx (U16, U16)) :: ((PVx (U32, U16)) :: ((PVx (U64,
    U16)) :: ((PVx (U32, U32)) :: ((PVx (U64, U32)) :: [])))))))
  in
  (fun f -> PrimX86 (range, (fun s ->
  match s with
  | PVp _ -> None
  | PVs (_, _) -> None
  | PVv (_, _) -> None
  | PVsv (_, _, _) -> None
  | PVx (szo, szi) -> Some (f szo szi)
  | PVvv (_, _, _, _) -> None)))

(** val prim_movzx : (wsize -> wsize -> x86_op) -> x86_op prim_constructor **)

let prim_movzx =
  let range = (PVx (U16, U8)) :: ((PVx (U32, U8)) :: ((PVx (U64,
    U8)) :: ((PVx (U32, U16)) :: ((PVx (U64, U16)) :: []))))
  in
  (fun f -> PrimX86 (range, (fun s ->
  match s with
  | PVp _ -> None
  | PVs (_, _) -> None
  | PVv (_, _) -> None
  | PVsv (_, _, _) -> None
  | PVx (szo, szi) -> Some (f szo szi)
  | PVvv (_, _, _, _) -> None)))

(** val prim_vv :
    (velem -> wsize -> velem -> wsize -> x86_op) -> x86_op prim_constructor **)

let prim_vv f =
  PrimX86 (((PVvv (VE8, U64, VE16, U128)) :: ((PVvv (VE8, U32, VE32,
    U128)) :: ((PVvv (VE8, U16, VE64, U128)) :: ((PVvv (VE16, U64, VE32,
    U128)) :: ((PVvv (VE16, U32, VE64, U128)) :: ((PVvv (VE32, U64, VE64,
    U128)) :: ((PVvv (VE8, U128, VE16, U256)) :: ((PVvv (VE8, U64, VE32,
    U256)) :: ((PVvv (VE8, U32, VE64, U256)) :: ((PVvv (VE16, U128, VE32,
    U256)) :: ((PVvv (VE16, U64, VE64, U256)) :: ((PVvv (VE32, U128, VE64,
    U256)) :: [])))))))))))), (fun s ->
    match s with
    | PVvv (ve, sz, ve', sz') -> Some (f ve sz ve' sz')
    | _ -> None))

(** val primV_range :
    prim_x86_suffix list -> (velem -> wsize -> x86_op) -> x86_op
    prim_constructor **)

let primV_range range f =
  PrimX86 (range, (fun s ->
    match s with
    | PVv (ve, sz) -> Some (f ve sz)
    | _ -> None))

(** val primV : (velem -> wsize -> x86_op) -> x86_op prim_constructor **)

let primV =
  primV_range
    (flatten
      (map (fun ve -> map (fun sz -> PVv (ve, sz)) (U128 :: (U256 :: [])))
        (VE8 :: (VE16 :: (VE32 :: (VE64 :: []))))))

(** val primV_8_16 : (velem -> wsize -> x86_op) -> x86_op prim_constructor **)

let primV_8_16 =
  primV_range
    (flatten
      (map (fun ve -> map (fun sz -> PVv (ve, sz)) (U128 :: (U256 :: [])))
        (VE8 :: (VE16 :: []))))

(** val primV_8_32 : (velem -> wsize -> x86_op) -> x86_op prim_constructor **)

let primV_8_32 =
  primV_range
    (flatten
      (map (fun ve -> map (fun sz -> PVv (ve, sz)) (U128 :: (U256 :: [])))
        (VE8 :: (VE16 :: (VE32 :: [])))))

(** val primV_16 : (velem -> wsize -> x86_op) -> x86_op prim_constructor **)

let primV_16 =
  primV_range (map (fun sz -> PVv (VE16, sz)) (U128 :: (U256 :: [])))

(** val primV_16_32 :
    (velem -> wsize -> x86_op) -> x86_op prim_constructor **)

let primV_16_32 =
  primV_range
    (flatten
      (map (fun ve -> map (fun sz -> PVv (ve, sz)) (U128 :: (U256 :: [])))
        (VE16 :: (VE32 :: []))))

(** val primV_16_64 :
    (velem -> wsize -> x86_op) -> x86_op prim_constructor **)

let primV_16_64 =
  primV_range
    (flatten
      (map (fun ve -> map (fun sz -> PVv (ve, sz)) (U128 :: (U256 :: [])))
        (VE16 :: (VE32 :: (VE64 :: [])))))

(** val primV_128 : (velem -> wsize -> x86_op) -> x86_op prim_constructor **)

let primV_128 =
  primV_range
    (map (fun ve -> PVv (ve, U128)) (VE8 :: (VE16 :: (VE32 :: (VE64 :: [])))))

(** val primMMX : (velem -> wsize -> x86_op) -> x86_op prim_constructor **)

let primMMX =
  primV_range
    (flatten
      (map (fun ve -> map (fun sz -> PVv (ve, sz)) (U64 :: (U128 :: [])))
        (VE8 :: (VE16 :: (VE32 :: (VE64 :: []))))))

(** val primSV_8_32 :
    (signedness -> velem -> wsize -> x86_op) -> x86_op prim_constructor **)

let primSV_8_32 f =
  PrimX86
    ((flatten
       (map (fun pv -> map pv (U128 :: (U256 :: [])))
         (flatten
           (map (fun sg ->
             map (fun ve x -> PVsv (sg, ve, x))
               (VE8 :: (VE16 :: (VE32 :: [])))) (Signed :: (Unsigned :: [])))))),
    (fun s ->
    match s with
    | PVsv (sg, ve, sz) -> Some (f sg ve sz)
    | _ -> None))

(** val primX : (wsize -> wsize -> x86_op) -> x86_op prim_constructor **)

let primX f =
  PrimX86
    ((flatten
       (map (fun ssz -> map (fun dsz -> PVx (ssz, dsz)) (U64 :: (U32 :: [])))
         (U128 :: (U256 :: [])))), (fun s ->
    match s with
    | PVx (ssz, dsz) -> Some (f ssz dsz)
    | _ -> None))

(** val implicit_flags :
    (register, register_ext, xmm_register, rflag, condt) Arch_decl.arg_desc
    list **)

let implicit_flags =
  map (coq_F x86_decl) (OF :: (CF :: (SF :: (PF :: (ZF :: [])))))

(** val implicit_flags_noCF :
    (register, register_ext, xmm_register, rflag, condt) Arch_decl.arg_desc
    list **)

let implicit_flags_noCF =
  map (coq_F x86_decl) (OF :: (SF :: (PF :: (ZF :: []))))

(** val iCF :
    (register, register_ext, xmm_register, rflag, condt) Arch_decl.arg_desc **)

let iCF =
  coq_F x86_decl CF

(** val reg_msb_flag : wsize -> msb_flag **)

let reg_msb_flag sz =
  if cmp_le wsize_cmp sz U16 then MSB_MERGE else MSB_CLEAR

(** val max_32 : wsize -> wsize **)

let max_32 sz =
  if cmp_le wsize_cmp sz U32 then sz else U32

(** val map_sz :
    wsize -> (register, register_ext, xmm_register, rflag, condt) asm_args ->
    (wsize * (register, register_ext, xmm_register, rflag, condt) asm_arg)
    list **)

let map_sz sz a =
  List0.map (fun a0 -> (sz, a0)) a

(** val pp_name :
    string -> wsize -> (register, register_ext, xmm_register, rflag, condt)
    asm_args -> (register, register_ext, xmm_register, rflag, condt) pp_asm_op **)

let pp_name name sz args =
  { pp_aop_name = name; pp_aop_ext = PP_name; pp_aop_args = (map_sz sz args) }

(** val pp_name_ty :
    string -> wsize list -> (register, register_ext, xmm_register, rflag,
    condt) asm_arg list -> (register, register_ext, xmm_register, rflag,
    condt) pp_asm_op **)

let pp_name_ty name ws args =
  { pp_aop_name = name; pp_aop_ext = PP_name; pp_aop_args = (zip ws args) }

(** val pp_iname :
    string -> wsize -> (register, register_ext, xmm_register, rflag, condt)
    asm_args -> (register, register_ext, xmm_register, rflag, condt) pp_asm_op **)

let pp_iname name sz args =
  { pp_aop_name = name; pp_aop_ext = (PP_iname sz); pp_aop_args =
    (map_sz sz args) }

(** val pp_viname_long :
    string -> velem -> wsize -> (register, register_ext, xmm_register, rflag,
    condt) asm_args -> (register, register_ext, xmm_register, rflag, condt)
    pp_asm_op **)

let pp_viname_long name ve sz args =
  { pp_aop_name = name; pp_aop_ext = (PP_viname (ve, true)); pp_aop_args =
    (map_sz sz args) }

(** val pp_viname :
    string -> velem -> wsize -> (register, register_ext, xmm_register, rflag,
    condt) asm_args -> (register, register_ext, xmm_register, rflag, condt)
    pp_asm_op **)

let pp_viname name ve sz args =
  { pp_aop_name = name; pp_aop_ext = (PP_viname (ve, false)); pp_aop_args =
    (map_sz sz args) }

(** val pp_viname_ww_128 :
    string -> velem -> wsize -> (register, register_ext, xmm_register, rflag,
    condt) asm_arg list -> (register, register_ext, xmm_register, rflag,
    condt) pp_asm_op **)

let pp_viname_ww_128 name ve sz args =
  { pp_aop_name = name; pp_aop_ext = (PP_viname (ve, false)); pp_aop_args =
    (zip (sz :: (sz :: (U128 :: []))) args) }

(** val pp_iname_w_8 :
    string -> wsize -> (register, register_ext, xmm_register, rflag, condt)
    asm_arg list -> (register, register_ext, xmm_register, rflag, condt)
    pp_asm_op **)

let pp_iname_w_8 name sz args =
  { pp_aop_name = name; pp_aop_ext = (PP_iname sz); pp_aop_args =
    (zip (sz :: (U8 :: [])) args) }

(** val pp_iname_ww_8 :
    string -> wsize -> (register, register_ext, xmm_register, rflag, condt)
    asm_arg list -> (register, register_ext, xmm_register, rflag, condt)
    pp_asm_op **)

let pp_iname_ww_8 name sz args =
  { pp_aop_name = name; pp_aop_ext = (PP_iname sz); pp_aop_args =
    (zip (sz :: (sz :: (U8 :: []))) args) }

(** val get_ct :
    (register, register_ext, xmm_register, rflag, condt) asm_arg list ->
    (register, register_ext, xmm_register, rflag, condt)
    pp_asm_op_ext * (register, register_ext, xmm_register, rflag, condt)
    asm_arg list **)

let get_ct args = match args with
| [] -> (PP_error, args)
| a :: args0 -> ((PP_ct a), args0)

(** val pp_ct :
    string -> wsize -> (register, register_ext, xmm_register, rflag, condt)
    asm_arg list -> (register, register_ext, xmm_register, rflag, condt)
    pp_asm_op **)

let pp_ct name sz args =
  let (ext, args0) = get_ct args in
  { pp_aop_name = name; pp_aop_ext = ext; pp_aop_args = (map_sz sz args0) }

(** val pp_cqo :
    wsize -> (register, register_ext, xmm_register, rflag, condt) asm_args ->
    (register, register_ext, xmm_register, rflag, condt) pp_asm_op **)

let pp_cqo sz _ =
  match sz with
  | U8 ->
    let name = "CQO" in
    let ext = PP_error in
    { pp_aop_name = name; pp_aop_ext = ext; pp_aop_args = (map_sz sz []) }
  | U16 ->
    let name = "cwd" in
    let ext = PP_name in
    { pp_aop_name = name; pp_aop_ext = ext; pp_aop_args = (map_sz sz []) }
  | U32 ->
    let name = "cdq" in
    let ext = PP_name in
    { pp_aop_name = name; pp_aop_ext = ext; pp_aop_args = (map_sz sz []) }
  | U64 ->
    let name = "cqo" in
    let ext = PP_name in
    { pp_aop_name = name; pp_aop_ext = ext; pp_aop_args = (map_sz sz []) }
  | U128 ->
    let name = "CQO" in
    let ext = PP_error in
    { pp_aop_name = name; pp_aop_ext = ext; pp_aop_args = (map_sz sz []) }
  | U256 ->
    let name = "CQO" in
    let ext = PP_error in
    { pp_aop_name = name; pp_aop_ext = ext; pp_aop_args = (map_sz sz []) }

(** val c : arg_kind list **)

let c =
  CAcond :: []

(** val r : arg_kind list **)

let r =
  CAreg :: []

(** val rx : arg_kind list **)

let rx =
  CAregx :: []

(** val m : bool -> arg_kind list **)

let m b =
  (CAmem b) :: []

(** val i : wsize -> arg_kind list **)

let i sz =
  (CAimm (CAimmC_none, sz)) :: []

(** val rm : bool -> arg_kind list **)

let rm b =
  CAreg :: ((CAmem b) :: [])

(** val rxm : bool -> arg_kind list **)

let rxm b =
  CAregx :: ((CAmem b) :: [])

(** val rmi : wsize -> arg_kind list **)

let rmi sz =
  CAreg :: ((CAmem true) :: ((CAimm (CAimmC_none, sz)) :: []))

(** val ri : wsize -> arg_kind list **)

let ri sz =
  CAreg :: ((CAimm (CAimmC_none, sz)) :: [])

(** val m_r : arg_kind list list **)

let m_r =
  (m false) :: (r :: [])

(** val r_rm_false : arg_kind list list **)

let r_rm_false =
  r :: ((rm false) :: [])

(** val r_rm : arg_kind list list **)

let r_rm =
  r :: ((rm true) :: [])

(** val r_rmi : wsize -> arg_kind list list **)

let r_rmi sz =
  r :: ((rmi sz) :: [])

(** val m_ri : wsize -> arg_kind list list **)

let m_ri sz =
  (m false) :: ((ri sz) :: [])

(** val xmm : arg_kind list **)

let xmm =
  CAxmm :: []

(** val xmmm : bool -> arg_kind list **)

let xmmm b =
  CAxmm :: ((CAmem b) :: [])

(** val xmmmi : wsize -> arg_kind list **)

let xmmmi sz =
  CAxmm :: ((CAmem true) :: ((CAimm (CAimmC_none, sz)) :: []))

(** val xmm_xmmm : arg_kind list list **)

let xmm_xmmm =
  xmm :: ((xmmm true) :: [])

(** val xmmm_xmm : arg_kind list list **)

let xmmm_xmm =
  (xmmm false) :: (xmm :: [])

(** val xmm_xmm_xmmm : arg_kind list list **)

let xmm_xmm_xmmm =
  xmm :: (xmm :: ((xmmm true) :: []))

(** val xmm_xmm_xmmmi : wsize -> arg_kind list list **)

let xmm_xmm_xmmmi sz =
  xmm :: (xmm :: ((xmmmi sz) :: []))

(** val x86_MOV : wsize -> GRing.ComRing.sort -> GRing.ComRing.sort **)

let x86_MOV _ x =
  x

(** val check_mov : wsize -> arg_kind list list list **)

let check_mov sz =
  (r_rmi sz) :: ((m_ri (max_32 sz)) :: [])

(** val coq_Ox86_MOV_instr :
    (wsize -> (register, register_ext, xmm_register, rflag, condt)
    instr_desc_t) * (string * x86_op prim_constructor) **)

let coq_Ox86_MOV_instr =
  ((fun sz -> { id_valid = (size_8_64 sz); id_msb_flag = (reg_msb_flag sz);
    id_tin = (w_ty sz); id_in = ((coq_Eu x86_decl (S O)) :: []); id_tout =
    (w_ty sz); id_out = ((coq_Eu x86_decl O) :: []); id_semi =
    (sem_prod_ok (map eval_ltype (w_ty sz)) (Obj.magic x86_MOV sz));
    id_args_kinds = (check_mov sz); id_nargs = (S (S O)); id_str_jas =
    (pp_sz "MOV" sz); id_safe = []; id_pp_asm = (pp_iname "mov" sz) }),
    ("MOV", (prim_8_64 (fun x -> MOV x))))

(** val check_movx : wsize -> arg_kind list list list **)

let check_movx _ =
  (rx :: ((rm true) :: [])) :: (((rm true) :: (rx :: [])) :: [])

(** val pp_movd :
    string -> Equality.sort -> (register, register_ext, xmm_register, rflag,
    condt) asm_arg list -> (register, register_ext, xmm_register, rflag,
    condt) pp_asm_op **)

let pp_movd name sz args =
  pp_name_ty
    (if eq_op wsize_wsize__canonical__eqtype_Equality sz (Obj.magic U64)
     then (^) name "q"
     else (^) name "d")
    (match args with
     | [] -> (Obj.magic sz) :: ((Obj.magic sz) :: [])
     | y :: l ->
       (match y with
        | Arch_decl.Reg _ ->
          (match l with
           | [] -> (Obj.magic sz) :: ((Obj.magic sz) :: [])
           | a :: l0 ->
             (match a with
              | XReg _ ->
                (match l0 with
                 | [] -> (Obj.magic sz) :: (U128 :: [])
                 | _ :: _ -> (Obj.magic sz) :: ((Obj.magic sz) :: []))
              | _ -> (Obj.magic sz) :: ((Obj.magic sz) :: [])))
        | XReg _ ->
          (match l with
           | [] -> (Obj.magic sz) :: ((Obj.magic sz) :: [])
           | a :: l0 ->
             (match a with
              | Arch_decl.Reg _ ->
                (match l0 with
                 | [] -> U128 :: ((Obj.magic sz) :: [])
                 | _ :: _ -> (Obj.magic sz) :: ((Obj.magic sz) :: []))
              | _ -> (Obj.magic sz) :: ((Obj.magic sz) :: [])))
        | _ -> (Obj.magic sz) :: ((Obj.magic sz) :: []))) args

(** val x86_MOVX : wsize -> GRing.ComRing.sort -> GRing.ComRing.sort **)

let x86_MOVX _ x =
  x

(** val coq_Ox86_MOVX_instr :
    (wsize -> (register, register_ext, xmm_register, rflag, condt)
    instr_desc_t) * (string * x86_op prim_constructor) **)

let coq_Ox86_MOVX_instr =
  ((fun sz -> { id_valid = (size_32_64 sz); id_msb_flag = (reg_msb_flag sz);
    id_tin = (w_ty sz); id_in = ((coq_Eu x86_decl (S O)) :: []); id_tout =
    (w_ty sz); id_out = ((coq_Eu x86_decl O) :: []); id_semi =
    (sem_prod_ok (map eval_ltype (w_ty sz)) (Obj.magic x86_MOVX sz));
    id_args_kinds = (check_movx sz); id_nargs = (S (S O)); id_str_jas =
    (pp_sz "MOVX" sz); id_safe = []; id_pp_asm =
    (pp_movd "mov" (Obj.magic sz)) }), ("MOVX",
    (prim_32_64 (fun x -> MOVX x))))

(** val check_por : i_args_kinds **)

let check_por =
  (rx :: ((rxm true) :: [])) :: []

(** val x86_POR :
    (register, register_ext, xmm_register, rflag, condt) wreg -> (register,
    register_ext, xmm_register, rflag, condt) wreg -> (register,
    register_ext, xmm_register, rflag, condt) wreg **)

let x86_POR v1 v2 =
  Word0.wor x86_decl.reg_size v1 v2

(** val coq_Ox86_POR_instr :
    (register, register_ext, xmm_register, rflag, condt)
    instr_desc_t * (string * x86_op prim_constructor) **)

let coq_Ox86_POR_instr =
  let desc = { id_valid = true; id_msb_flag = MSB_MERGE; id_tin =
    (w2_ty U64 U64); id_in =
    ((coq_Eu x86_decl O) :: ((coq_Eu x86_decl (S O)) :: [])); id_tout =
    (w_ty U64); id_out = ((coq_Eu x86_decl O) :: []); id_semi =
    (sem_prod_ok (map eval_ltype (w2_ty U64 U64)) (Obj.magic x86_POR));
    id_args_kinds = check_por; id_nargs = (S (S O)); id_str_jas =
    (pp_s "POR"); id_safe = []; id_pp_asm = (pp_name "por" U64) }
  in
  (desc, ("POR", (primM POR)))

(** val check_padd : i_args_kinds **)

let check_padd =
  (rx :: ((rxm true) :: [])) :: ((xmm :: ((xmmm true) :: [])) :: [])

(** val coq_Ox86_PADD_instr :
    (velem -> wsize -> (register, register_ext, xmm_register, rflag, condt)
    instr_desc_t) * (string * x86_op prim_constructor) **)

let coq_Ox86_PADD_instr =
  let padd = "PADD" in
  ((fun ve sz -> { id_valid = (size_64_128 sz); id_msb_flag = MSB_CLEAR;
  id_tin = (w2_ty sz sz); id_in =
  ((coq_Eu x86_decl O) :: ((coq_Eu x86_decl (S O)) :: [])); id_tout =
  (w_ty sz); id_out = ((coq_Eu x86_decl O) :: []); id_semi =
  (sem_prod_ok (map eval_ltype (w2_ty sz sz))
    (Obj.magic lift2_vec (wsize_of_velem ve)
      (GRing.add
        (GRing.ComRing.Exports.coq_GRing_ComRing__to__GRing_Nmodule
          (word (wsize_of_velem ve)))) sz)); id_args_kinds = check_padd;
  id_nargs = (S (S O)); id_str_jas = (pp_ve_sz padd ve sz); id_safe = [];
  id_pp_asm = (pp_viname "padd" ve sz) }), (padd,
  (primMMX (fun x x0 -> PADD (x, x0)))))

(** val check_movsx : wsize -> wsize -> arg_kind list list list **)

let check_movsx _ _ =
  r_rm :: []

(** val pp_movsx :
    Equality.sort -> Equality.sort -> (register, register_ext, xmm_register,
    rflag, condt) asm_arg list -> (register, register_ext, xmm_register,
    rflag, condt) pp_asm_op **)

let pp_movsx szs szd args =
  let ext =
    if (||) (eq_op wsize_wsize__canonical__eqtype_Equality szd szs)
         ((&&)
           (eq_op wsize_wsize__canonical__eqtype_Equality szd (Obj.magic U64))
           (eq_op wsize_wsize__canonical__eqtype_Equality szs (Obj.magic U32)))
    then "xd"
    else "x"
  in
  { pp_aop_name = "movs"; pp_aop_ext = (PP_iname2 (ext, (Obj.magic szs),
  (Obj.magic szd))); pp_aop_args =
  (zip ((Obj.magic szd) :: ((Obj.magic szs) :: [])) args) }

(** val size_MOVSX : wsize -> wsize -> bool **)

let size_MOVSX szi szo =
  match szi with
  | U8 -> size_16_64 szo
  | U16 -> size_16_64 szo
  | U32 -> size_32_64 szo
  | _ -> false

(** val x86_MOVSX : wsize -> wsize -> GRing.ComRing.sort -> sem_tuple **)

let x86_MOVSX szi szo x =
  sign_extend szo szi x

(** val coq_Ox86_MOVSX_instr :
    (wsize -> wsize -> (register, register_ext, xmm_register, rflag, condt)
    instr_desc_t) * (string * x86_op prim_constructor) **)

let coq_Ox86_MOVSX_instr =
  ((fun szo szi -> { id_valid = (size_MOVSX szi szo); id_msb_flag =
    (reg_msb_flag szo); id_tin = (w_ty szi); id_in =
    ((coq_Eu x86_decl (S O)) :: []); id_tout = (w_ty szo); id_out =
    ((coq_Eu x86_decl O) :: []); id_semi =
    (sem_prod_ok (map eval_ltype (w_ty szi)) (Obj.magic x86_MOVSX szi szo));
    id_args_kinds = (check_movsx szi szo); id_nargs = (S (S O)); id_str_jas =
    (pp_sz_sz "MOVSX" true szo szi); id_safe = []; id_pp_asm =
    (pp_movsx (Obj.magic szi) (Obj.magic szo)) }), ("MOVSX",
    (prim_movsx (fun x x0 -> MOVSX (x, x0)))))

(** val pp_movzx :
    wsize -> wsize -> (register, register_ext, xmm_register, rflag, condt)
    asm_arg list -> (register, register_ext, xmm_register, rflag, condt)
    pp_asm_op **)

let pp_movzx szs szd args =
  { pp_aop_name = "movz"; pp_aop_ext = (PP_iname2 ("x", szs, szd));
    pp_aop_args = (zip (szd :: (szs :: [])) args) }

(** val size_MOVZX : wsize -> wsize -> bool **)

let size_MOVZX szi szo =
  match szi with
  | U8 -> size_16_64 szo
  | U16 -> size_32_64 szo
  | _ -> false

(** val x86_MOVZX : wsize -> wsize -> GRing.ComRing.sort -> sem_tuple **)

let x86_MOVZX szi szo x =
  zero_extend szo szi x

(** val coq_Ox86_MOVZX_instr :
    (wsize -> wsize -> (register, register_ext, xmm_register, rflag, condt)
    instr_desc_t) * (string * x86_op prim_constructor) **)

let coq_Ox86_MOVZX_instr =
  ((fun szo szi -> { id_valid = (size_MOVZX szi szo); id_msb_flag =
    (reg_msb_flag szo); id_tin = (w_ty szi); id_in =
    ((coq_Eu x86_decl (S O)) :: []); id_tout = (w_ty szo); id_out =
    ((coq_Eu x86_decl O) :: []); id_semi =
    (sem_prod_ok (map eval_ltype (w_ty szi)) (Obj.magic x86_MOVZX szi szo));
    id_args_kinds = (check_movsx szi szo); id_nargs = (S (S O)); id_str_jas =
    (pp_sz_sz "MOVZX" false szo szi); id_safe = []; id_pp_asm =
    (pp_movzx szi szo) }), ("MOVZX",
    (prim_movzx (fun x x0 -> MOVZX (x, x0)))))

(** val check_xchg : arg_kind list list list **)

let check_xchg =
  m_r :: (r_rm :: [])

(** val x86_XCHG :
    wsize -> GRing.ComRing.sort -> GRing.ComRing.sort -> sem_tuple **)

let x86_XCHG _ v1 v2 =
  Obj.magic (v2, v1)

(** val coq_Ox86_XCHG_instr :
    (wsize -> (register, register_ext, xmm_register, rflag, condt)
    instr_desc_t) * (string * x86_op prim_constructor) **)

let coq_Ox86_XCHG_instr =
  let name = "XCHG" in
  ((fun sz -> { id_valid = (size_8_64 sz); id_msb_flag = (reg_msb_flag sz);
  id_tin = (w2_ty sz sz); id_in =
  ((coq_Eu x86_decl O) :: ((coq_Eu x86_decl (S O)) :: [])); id_tout =
  (w2_ty sz sz); id_out =
  ((coq_Eu x86_decl O) :: ((coq_Eu x86_decl (S O)) :: [])); id_semi =
  (sem_prod_ok (map eval_ltype (w2_ty sz sz)) (Obj.magic x86_XCHG sz));
  id_args_kinds = check_xchg; id_nargs = (S (S O)); id_str_jas =
  (pp_sz name sz); id_safe = []; id_pp_asm = (pp_name "xchg" sz) }), (name,
  (primP (arch_pd x86_decl) (fun x -> XCHG x))))

(** val c_r_rm : arg_kind list list **)

let c_r_rm =
  c :: (r :: ((rm true) :: []))

(** val x86_CMOVcc :
    wsize -> bool -> GRing.ComRing.sort -> GRing.ComRing.sort -> sem_tuple **)

let x86_CMOVcc _ b w2 w3 =
  if b then w2 else w3

(** val coq_Ox86_CMOVcc_instr :
    (wsize -> (register, register_ext, xmm_register, rflag, condt)
    instr_desc_t) * (string * x86_op prim_constructor) **)

let coq_Ox86_CMOVcc_instr =
  ((fun sz -> { id_valid = (size_16_64 sz); id_msb_flag = (reg_msb_flag sz);
    id_tin = (bw2_ty sz); id_in =
    ((coq_Ea x86_decl O) :: ((coq_Eu x86_decl (S (S O))) :: ((coq_Ea x86_decl
                                                               (S O)) :: [])));
    id_tout = (w_ty sz); id_out = ((coq_Ea x86_decl (S O)) :: []); id_semi =
    (sem_prod_ok (map eval_ltype (bw2_ty sz)) (Obj.magic x86_CMOVcc sz));
    id_args_kinds = (c_r_rm :: []); id_nargs = (S (S (S O))); id_str_jas =
    (pp_sz "CMOVcc" sz); id_safe = []; id_pp_asm = (pp_ct "cmov" sz) }),
    ("CMOVcc", (prim_16_64 (fun x -> CMOVcc x))))

(** val check_add : wsize -> arg_kind list list list **)

let check_add sz =
  (m_ri (max_32 sz)) :: ((r_rmi (max_32 sz)) :: [])

(** val x86_ADD :
    wsize -> GRing.ComRing.sort -> GRing.ComRing.sort -> sem_tuple **)

let x86_ADD sz v1 v2 =
  rflags_of_aluop_w sz
    (GRing.add
      (GRing.ComRing.Exports.coq_GRing_ComRing__to__GRing_Nmodule (word sz))
      v1 v2) (Z.add (wunsigned sz v1) (wunsigned sz v2))
    (Z.add (wsigned sz v1) (wsigned sz v2))

(** val coq_Ox86_ADD_instr :
    (wsize -> (register, register_ext, xmm_register, rflag, condt)
    instr_desc_t) * (string * x86_op prim_constructor) **)

let coq_Ox86_ADD_instr =
  ((fun sz -> { id_valid = (size_8_64 sz); id_msb_flag = (reg_msb_flag sz);
    id_tin = (w2_ty sz sz); id_in =
    ((coq_Eu x86_decl O) :: ((coq_Eu x86_decl (S O)) :: [])); id_tout =
    (b5w_ty sz); id_out = (cat implicit_flags ((coq_Eu x86_decl O) :: []));
    id_semi =
    (sem_prod_ok (map eval_ltype (w2_ty sz sz)) (Obj.magic x86_ADD sz));
    id_args_kinds = (check_add sz); id_nargs = (S (S O)); id_str_jas =
    (pp_sz "ADD" sz); id_safe = []; id_pp_asm = (pp_iname "add" sz) }),
    ("ADD", (prim_8_64 (fun x -> ADD x))))

(** val x86_SUB :
    wsize -> GRing.ComRing.sort -> GRing.ComRing.sort -> sem_tuple **)

let x86_SUB sz v1 v2 =
  rflags_of_aluop_w sz
    (GRing.add
      (GRing.ComRing.Exports.coq_GRing_ComRing__to__GRing_Nmodule (word sz))
      v1
      (GRing.opp
        (GRing.ComRing.Exports.coq_GRing_ComRing__to__GRing_Zmodule (word sz))
        v2)) (Z.sub (wunsigned sz v1) (wunsigned sz v2))
    (Z.sub (wsigned sz v1) (wsigned sz v2))

(** val coq_Ox86_SUB_instr :
    (wsize -> (register, register_ext, xmm_register, rflag, condt)
    instr_desc_t) * (string * x86_op prim_constructor) **)

let coq_Ox86_SUB_instr =
  ((fun sz -> { id_valid = (size_8_64 sz); id_msb_flag = (reg_msb_flag sz);
    id_tin = (w2_ty sz sz); id_in =
    ((coq_Eu x86_decl O) :: ((coq_Eu x86_decl (S O)) :: [])); id_tout =
    (b5w_ty sz); id_out = (cat implicit_flags ((coq_Eu x86_decl O) :: []));
    id_semi =
    (sem_prod_ok (map eval_ltype (w2_ty sz sz)) (Obj.magic x86_SUB sz));
    id_args_kinds = (check_add sz); id_nargs = (S (S O)); id_str_jas =
    (pp_sz "SUB" sz); id_safe = []; id_pp_asm = (pp_iname "sub" sz) }),
    ("SUB", (prim_8_64 (fun x -> SUB x))))

(** val check_mul : wsize -> arg_kind list list list **)

let check_mul _ =
  ((rm true) :: []) :: []

(** val x86_MUL :
    wsize -> GRing.ComRing.sort -> GRing.ComRing.sort -> sem_tuple **)

let x86_MUL sz v1 v2 =
  let lo =
    GRing.mul
      (GRing.ComRing.Exports.coq_GRing_ComRing__to__GRing_SemiRing (word sz))
      v1 v2
  in
  let hi = wmulhu sz v1 v2 in
  let ov = wdwordu sz hi lo in
  let ov0 = Z.gtb ov (Z.sub (wbase sz) (Zpos Coq_xH)) in
  flags_w2 (map (Obj.magic __) (map eval_ltype b5_ty)) (rflags_of_mul ov0) sz
    (Obj.magic (hi, lo))

(** val coq_Ox86_MUL_instr :
    (wsize -> (register, register_ext, xmm_register, rflag, condt)
    instr_desc_t) * (string * x86_op prim_constructor) **)

let coq_Ox86_MUL_instr =
  ((fun sz -> { id_valid = (size_16_64 sz); id_msb_flag = (reg_msb_flag sz);
    id_tin = (w2_ty sz sz); id_in =
    ((coq_R x86_decl RAX) :: ((coq_Eu x86_decl O) :: [])); id_tout =
    (b5w2_ty sz); id_out =
    (cat implicit_flags
      ((coq_R x86_decl RDX) :: ((coq_R x86_decl RAX) :: []))); id_semi =
    (sem_prod_ok (map eval_ltype (w2_ty sz sz)) (Obj.magic x86_MUL sz));
    id_args_kinds = (check_mul sz); id_nargs = (S O); id_str_jas =
    (pp_sz "MUL" sz); id_safe = []; id_pp_asm = (pp_iname "mul" sz) }),
    ("MUL", (prim_16_64 (fun x -> MUL x))))

(** val x86_IMUL_overflow :
    wsize -> GRing.ComRing.sort -> GRing.ComRing.sort -> bool **)

let x86_IMUL_overflow sz hi lo =
  let ov = wdwords sz hi lo in
  (||) (Z.ltb ov (wmin_signed sz)) (Z.gtb ov (wmax_signed sz))

(** val x86_IMUL :
    wsize -> GRing.ComRing.sort -> GRing.ComRing.sort -> sem_tuple **)

let x86_IMUL sz v1 v2 =
  let lo =
    GRing.mul
      (GRing.ComRing.Exports.coq_GRing_ComRing__to__GRing_SemiRing (word sz))
      v1 v2
  in
  let hi = wmulhs sz v1 v2 in
  let ov = x86_IMUL_overflow sz hi lo in
  flags_w2 (map (Obj.magic __) (map eval_ltype b5_ty)) (rflags_of_mul ov) sz
    (Obj.magic (hi, lo))

(** val coq_Ox86_IMUL_instr :
    (wsize -> (register, register_ext, xmm_register, rflag, condt)
    instr_desc_t) * (string * x86_op prim_constructor) **)

let coq_Ox86_IMUL_instr =
  ((fun sz -> { id_valid = (size_16_64 sz); id_msb_flag = (reg_msb_flag sz);
    id_tin = (w2_ty sz sz); id_in =
    ((coq_R x86_decl RAX) :: ((coq_Eu x86_decl O) :: [])); id_tout =
    (b5w2_ty sz); id_out =
    (cat implicit_flags
      ((coq_R x86_decl RDX) :: ((coq_R x86_decl RAX) :: []))); id_semi =
    (sem_prod_ok (map eval_ltype (w2_ty sz sz)) (Obj.magic x86_IMUL sz));
    id_args_kinds = (check_mul sz); id_nargs = (S O); id_str_jas =
    (pp_sz "IMUL" sz); id_safe = []; id_pp_asm = (pp_iname "imul" sz) }),
    ("IMUL", (prim_16_64 (fun x -> IMUL x))))

(** val x86_IMULt :
    wsize -> GRing.ComRing.sort -> GRing.ComRing.sort -> sem_tuple **)

let x86_IMULt sz v1 v2 =
  let lo =
    GRing.mul
      (GRing.ComRing.Exports.coq_GRing_ComRing__to__GRing_SemiRing (word sz))
      v1 v2
  in
  let hi = wmulhs sz v1 v2 in
  let ov = x86_IMUL_overflow sz hi lo in
  flags_w (map (Obj.magic __) (map eval_ltype b5_ty)) (rflags_of_mul ov) sz lo

(** val coq_Ox86_IMULr_instr :
    (wsize -> (register, register_ext, xmm_register, rflag, condt)
    instr_desc_t) * (string * x86_op prim_constructor) **)

let coq_Ox86_IMULr_instr =
  ((fun sz -> { id_valid = (size_16_64 sz); id_msb_flag = (reg_msb_flag sz);
    id_tin = (w2_ty sz sz); id_in =
    ((coq_Eu x86_decl O) :: ((coq_Eu x86_decl (S O)) :: [])); id_tout =
    (b5w_ty sz); id_out = (cat implicit_flags ((coq_Eu x86_decl O) :: []));
    id_semi =
    (sem_prod_ok (map eval_ltype (w2_ty sz sz)) (Obj.magic x86_IMULt sz));
    id_args_kinds = (r_rm :: []); id_nargs = (S (S O)); id_str_jas =
    (pp_sz "IMULr" sz); id_safe = []; id_pp_asm = (pp_iname "imul" sz) }),
    ("IMULr", (prim_16_64 (fun x -> IMULr x))))

(** val coq_Ox86_IMULri_instr :
    (wsize -> (register, register_ext, xmm_register, rflag, condt)
    instr_desc_t) * (string * x86_op prim_constructor) **)

let coq_Ox86_IMULri_instr =
  ((fun sz -> { id_valid = (size_16_64 sz); id_msb_flag = (reg_msb_flag sz);
    id_tin = (w2_ty sz sz); id_in =
    ((coq_Eu x86_decl (S O)) :: ((coq_Eu x86_decl (S (S O))) :: []));
    id_tout = (b5w_ty sz); id_out =
    (cat implicit_flags ((coq_Eu x86_decl O) :: [])); id_semi =
    (sem_prod_ok (map eval_ltype (w2_ty sz sz)) (Obj.magic x86_IMULt sz));
    id_args_kinds = ((r :: ((rm true) :: ((i (max_32 sz)) :: []))) :: []);
    id_nargs = (S (S (S O))); id_str_jas = (pp_sz "IMULri" sz); id_safe = [];
    id_pp_asm = (pp_iname "imul" sz) }), ("IMULri",
    (prim_16_64 (fun x -> IMULri x))))

(** val x86_DIV :
    wsize -> GRing.ComRing.sort -> GRing.ComRing.sort -> GRing.ComRing.sort
    -> sem_tuple exec **)

let x86_DIV sz hi lo dv =
  let dd = wdwordu sz hi lo in
  let dv0 = wunsigned sz dv in
  let q = Z.div dd dv0 in
  let r0 = Z.modulo dd dv0 in
  let ov = Z.gtb q (wmax_unsigned sz) in
  if (||)
       (eq_op coq_BinNums_Z__canonical__eqtype_Equality (Obj.magic dv0)
         (Obj.magic Z0)) ov
  then Error ErrArith
  else Ok
         (flags_w2 (map (Obj.magic __) (map eval_ltype b5_ty)) rflags_of_div
           sz (Obj.magic ((wrepr sz q), (wrepr sz r0))))

(** val coq_Ox86_DIV_instr :
    (wsize -> (register, register_ext, xmm_register, rflag, condt)
    instr_desc_t) * (string * x86_op prim_constructor) **)

let coq_Ox86_DIV_instr =
  ((fun sz -> { id_valid = (size_16_64 sz); id_msb_flag = (reg_msb_flag sz);
    id_tin = (w3_ty sz); id_in =
    ((coq_R x86_decl RDX) :: ((coq_R x86_decl RAX) :: ((coq_Eu x86_decl O) :: [])));
    id_tout = (b5w2_ty sz); id_out =
    (cat implicit_flags
      ((coq_R x86_decl RAX) :: ((coq_R x86_decl RDX) :: []))); id_semi =
    (Obj.magic x86_DIV sz); id_args_kinds = (check_mul sz); id_nargs = (S O);
    id_str_jas = (pp_sz "DIV" sz); id_safe = ((X86Division (sz,
    Unsigned)) :: []); id_pp_asm = (pp_iname "div" sz) }), ("DIV",
    (prim_16_64 (fun x -> DIV x))))

(** val x86_IDIV :
    wsize -> GRing.ComRing.sort -> GRing.ComRing.sort -> GRing.ComRing.sort
    -> sem_tuple exec **)

let x86_IDIV sz hi lo dv =
  let dd = wdwords sz hi lo in
  let dv0 = wsigned sz dv in
  let q = Z.quot dd dv0 in
  let r0 = Z.rem dd dv0 in
  let ov = (||) (Z.ltb q (wmin_signed sz)) (Z.gtb q (wmax_signed sz)) in
  if (||)
       (eq_op coq_BinNums_Z__canonical__eqtype_Equality (Obj.magic dv0)
         (Obj.magic Z0)) ov
  then Error ErrArith
  else Ok
         (flags_w2 (map (Obj.magic __) (map eval_ltype b5_ty)) rflags_of_div
           sz (Obj.magic ((wrepr sz q), (wrepr sz r0))))

(** val coq_Ox86_IDIV_instr :
    (wsize -> (register, register_ext, xmm_register, rflag, condt)
    instr_desc_t) * (string * x86_op prim_constructor) **)

let coq_Ox86_IDIV_instr =
  ((fun sz -> { id_valid = (size_16_64 sz); id_msb_flag = (reg_msb_flag sz);
    id_tin = (w3_ty sz); id_in =
    ((coq_R x86_decl RDX) :: ((coq_R x86_decl RAX) :: ((coq_Eu x86_decl O) :: [])));
    id_tout = (b5w2_ty sz); id_out =
    (cat implicit_flags
      ((coq_R x86_decl RAX) :: ((coq_R x86_decl RDX) :: []))); id_semi =
    (Obj.magic x86_IDIV sz); id_args_kinds = (check_mul sz); id_nargs = (S
    O); id_str_jas = (pp_sz "IDIV" sz); id_safe = ((X86Division (sz,
    Signed)) :: []); id_pp_asm = (pp_iname "idiv" sz) }), ("IDIV",
    (prim_16_64 (fun x -> IDIV x))))

(** val x86_CQO : wsize -> GRing.ComRing.sort -> GRing.ComRing.sort **)

let x86_CQO sz w =
  if msb sz w
  then GRing.opp
         (GRing.Ring.Exports.join_GRing_Ring_between_GRing_SemiRing_and_GRing_Zmodule
           (GRing.ComRing.Exports.coq_GRing_ComRing__to__GRing_Ring (word sz)))
         (GRing.one
           (GRing.Ring.Exports.coq_GRing_Ring__to__GRing_SemiRing
             (GRing.ComRing.Exports.coq_GRing_ComRing__to__GRing_Ring
               (word sz))))
  else GRing.zero
         (GRing.Zmodule.Exports.coq_GRing_Zmodule__to__GRing_Nmodule
           (GRing.Ring.Exports.join_GRing_Ring_between_GRing_SemiRing_and_GRing_Zmodule
             (GRing.ComRing.Exports.coq_GRing_ComRing__to__GRing_Ring
               (word sz))))

(** val coq_Ox86_CQO_instr :
    (wsize -> (register, register_ext, xmm_register, rflag, condt)
    instr_desc_t) * (string * x86_op prim_constructor) **)

let coq_Ox86_CQO_instr =
  ((fun sz -> { id_valid = (size_16_64 sz); id_msb_flag = (reg_msb_flag sz);
    id_tin = (w_ty sz); id_in = ((coq_R x86_decl RAX) :: []); id_tout =
    (w_ty sz); id_out = ((coq_R x86_decl RDX) :: []); id_semi =
    (sem_prod_ok (map eval_ltype (w_ty sz)) (Obj.magic x86_CQO sz));
    id_args_kinds = ([] :: []); id_nargs = O; id_str_jas = (pp_sz "CQO" sz);
    id_safe = []; id_pp_asm = (pp_cqo sz) }), ("CQO",
    (prim_16_64 (fun x -> CQO x))))

(** val add_carry : wsize -> coq_Z -> coq_Z -> coq_Z -> GRing.ComRing.sort **)

let add_carry sz x y c0 =
  wrepr sz (Z.add (Z.add x y) c0)

(** val x86_ADC :
    wsize -> GRing.ComRing.sort -> GRing.ComRing.sort -> bool -> sem_tuple **)

let x86_ADC sz v1 v2 c0 =
  let c1 = Z.b2z c0 in
  rflags_of_aluop_w sz (add_carry sz (wunsigned sz v1) (wunsigned sz v2) c1)
    (Z.add (Z.add (wunsigned sz v1) (wunsigned sz v2)) c1)
    (Z.add (Z.add (wsigned sz v1) (wsigned sz v2)) c1)

(** val coq_Ox86_ADC_instr :
    (wsize -> (register, register_ext, xmm_register, rflag, condt)
    instr_desc_t) * (string * x86_op prim_constructor) **)

let coq_Ox86_ADC_instr =
  ((fun sz -> { id_valid = (size_8_64 sz); id_msb_flag = (reg_msb_flag sz);
    id_tin = (w2b_ty sz sz); id_in =
    (cat ((coq_Eu x86_decl O) :: ((coq_Eu x86_decl (S O)) :: [])) (iCF :: []));
    id_tout = (b5w_ty sz); id_out =
    (cat implicit_flags ((coq_Eu x86_decl O) :: [])); id_semi =
    (sem_prod_ok (map eval_ltype (w2b_ty sz sz)) (Obj.magic x86_ADC sz));
    id_args_kinds = (check_add sz); id_nargs = (S (S O)); id_str_jas =
    (pp_sz "ADC" sz); id_safe = []; id_pp_asm = (pp_iname "adc" sz) }),
    ("ADC", (prim_8_64 (fun x -> ADC x))))

(** val sub_borrow :
    wsize -> coq_Z -> coq_Z -> coq_Z -> GRing.ComRing.sort **)

let sub_borrow sz x y c0 =
  wrepr sz (Z.sub (Z.sub x y) c0)

(** val x86_SBB :
    wsize -> GRing.ComRing.sort -> GRing.ComRing.sort -> bool -> sem_tuple **)

let x86_SBB sz v1 v2 c0 =
  let c1 = Z.b2z c0 in
  rflags_of_aluop_w sz (sub_borrow sz (wunsigned sz v1) (wunsigned sz v2) c1)
    (Z.sub (wunsigned sz v1) (Z.add (wunsigned sz v2) c1))
    (Z.sub (wsigned sz v1) (Z.add (wsigned sz v2) c1))

(** val coq_Ox86_SBB_instr :
    (wsize -> (register, register_ext, xmm_register, rflag, condt)
    instr_desc_t) * (string * x86_op prim_constructor) **)

let coq_Ox86_SBB_instr =
  ((fun sz -> { id_valid = (size_8_64 sz); id_msb_flag = (reg_msb_flag sz);
    id_tin = (w2b_ty sz sz); id_in =
    (cat ((coq_Eu x86_decl O) :: ((coq_Eu x86_decl (S O)) :: [])) (iCF :: []));
    id_tout = (b5w_ty sz); id_out =
    (cat implicit_flags ((coq_Eu x86_decl O) :: [])); id_semi =
    (sem_prod_ok (map eval_ltype (w2b_ty sz sz)) (Obj.magic x86_SBB sz));
    id_args_kinds = (check_add sz); id_nargs = (S (S O)); id_str_jas =
    (pp_sz "SBB" sz); id_safe = []; id_pp_asm = (pp_iname "sbb" sz) }),
    ("SBB", (prim_8_64 (fun x -> SBB x))))

(** val check_adcx : wsize -> arg_kind list list list **)

let check_adcx _ =
  r_rm :: []

(** val x86_ADCX :
    wsize -> GRing.ComRing.sort -> GRing.ComRing.sort -> bool -> sem_tuple **)

let x86_ADCX sz v1 v2 c0 =
  let (c1, w) = waddcarry sz v1 v2 c0 in Obj.magic ((Some c1), w)

(** val coq_Ox86_ADCX_instr :
    (wsize -> (register, register_ext, xmm_register, rflag, condt)
    instr_desc_t) * (string * x86_op prim_constructor) **)

let coq_Ox86_ADCX_instr =
  ((fun sz -> { id_valid = (size_32_64 sz); id_msb_flag = (reg_msb_flag sz);
    id_tin = (w2b_ty sz sz); id_in =
    (cat ((coq_Ea x86_decl O) :: ((coq_Eu x86_decl (S O)) :: []))
      ((coq_F x86_decl CF) :: [])); id_tout = (bw_ty sz); id_out =
    ((coq_F x86_decl CF) :: ((coq_Ea x86_decl O) :: [])); id_semi =
    (sem_prod_ok (map eval_ltype (w2b_ty sz sz)) (Obj.magic x86_ADCX sz));
    id_args_kinds = (check_adcx sz); id_nargs = (S (S O)); id_str_jas =
    (pp_sz "ADCX" sz); id_safe = []; id_pp_asm = (pp_iname "adcx" sz) }),
    ("ADCX", (prim_32_64 (fun x -> ADCX x))))

(** val coq_Ox86_ADOX_instr :
    (wsize -> (register, register_ext, xmm_register, rflag, condt)
    instr_desc_t) * (string * x86_op prim_constructor) **)

let coq_Ox86_ADOX_instr =
  ((fun sz -> { id_valid = (size_32_64 sz); id_msb_flag = (reg_msb_flag sz);
    id_tin = (w2b_ty sz sz); id_in =
    (cat ((coq_Ea x86_decl O) :: ((coq_Eu x86_decl (S O)) :: []))
      ((coq_F x86_decl OF) :: [])); id_tout = (bw_ty sz); id_out =
    ((coq_F x86_decl OF) :: ((coq_Ea x86_decl O) :: [])); id_semi =
    (sem_prod_ok (map eval_ltype (w2b_ty sz sz)) (Obj.magic x86_ADCX sz));
    id_args_kinds = (check_adcx sz); id_nargs = (S (S O)); id_str_jas =
    (pp_sz "ADOX" sz); id_safe = []; id_pp_asm = (pp_iname "adox" sz) }),
    ("ADOX", (prim_32_64 (fun x -> ADOX x))))

(** val check_mulx : arg_kind list list list **)

let check_mulx =
  (r :: (r :: ((rm true) :: []))) :: []

(** val x86_MULX_lo_hi :
    wsize -> GRing.ComRing.sort -> GRing.ComRing.sort -> sem_tuple **)

let x86_MULX_lo_hi sz v1 v2 =
  let (hi, lo) = wumul sz v1 v2 in Obj.magic (lo, hi)

(** val coq_Ox86_MULX_lo_hi_instr :
    (wsize -> (register, register_ext, xmm_register, rflag, condt)
    instr_desc_t) * (string * x86_op prim_constructor) **)

let coq_Ox86_MULX_lo_hi_instr =
  let name = "MULX_lo_hi" in
  ((fun sz -> { id_valid = (size_32_64 sz); id_msb_flag = (reg_msb_flag sz);
  id_tin = (w2_ty sz sz); id_in =
  ((coq_R x86_decl RDX) :: ((coq_Eu x86_decl (S (S O))) :: [])); id_tout =
  (w2_ty sz sz); id_out =
  ((coq_Eu x86_decl (S O)) :: ((coq_Eu x86_decl O) :: [])); id_semi =
  (sem_prod_ok (map eval_ltype (w2_ty sz sz)) (Obj.magic x86_MULX_lo_hi sz));
  id_args_kinds = check_mulx; id_nargs = (S (S (S O))); id_str_jas =
  (pp_sz name sz); id_safe = []; id_pp_asm = (pp_iname "mulx" sz) }), (name,
  (prim_32_64 (fun x -> MULX_lo_hi x))))

(** val check_neg : wsize -> arg_kind list list list **)

let check_neg _ =
  ((rm false) :: []) :: []

(** val x86_NEG : wsize -> GRing.ComRing.sort -> sem_tuple **)

let x86_NEG sz w =
  let vs = Z.opp (wsigned sz w) in
  let v =
    GRing.opp
      (GRing.ComRing.Exports.coq_GRing_ComRing__to__GRing_Zmodule (word sz)) w
  in
  flags_w (map (Obj.magic __) (map eval_ltype b5_ty))
    (Obj.magic ((Some
      (negb
        (eq_op coq_BinNums_Z__canonical__eqtype_Equality
          (Obj.magic wsigned sz v) (Obj.magic vs)))), ((Some
      (negb
        (eq_op
          (GRing.ComRing.Exports.coq_GRing_ComRing__to__eqtype_Equality
            (word sz)) w
          (GRing.zero
            (GRing.ComRing.Exports.coq_GRing_ComRing__to__GRing_Nmodule
              (word sz)))))), ((Some (coq_SF_of_word sz v)), ((Some
      (coq_PF_of_word sz v)), (Some (coq_ZF_of_word sz v))))))) sz v

(** val coq_Ox86_NEG_instr :
    (wsize -> (register, register_ext, xmm_register, rflag, condt)
    instr_desc_t) * (string * x86_op prim_constructor) **)

let coq_Ox86_NEG_instr =
  ((fun sz -> { id_valid = (size_8_64 sz); id_msb_flag = (reg_msb_flag sz);
    id_tin = (w_ty sz); id_in = ((coq_Eu x86_decl O) :: []); id_tout =
    (b5w_ty sz); id_out = (cat implicit_flags ((coq_Eu x86_decl O) :: []));
    id_semi =
    (sem_prod_ok (map eval_ltype (w_ty sz)) (Obj.magic x86_NEG sz));
    id_args_kinds = (check_neg sz); id_nargs = (S O); id_str_jas =
    (pp_sz "NEG" sz); id_safe = []; id_pp_asm = (pp_iname "neg" sz) }),
    ("NEG", (prim_8_64 (fun x -> NEG x))))

(** val x86_INC : wsize -> GRing.ComRing.sort -> sem_tuple **)

let x86_INC sz w =
  rflags_of_aluop_nocf_w sz
    (GRing.add
      (GRing.ComRing.Exports.coq_GRing_ComRing__to__GRing_Nmodule (word sz))
      w
      (GRing.one
        (GRing.ComRing.Exports.coq_GRing_ComRing__to__GRing_SemiRing
          (word sz)))) (Z.add (wsigned sz w) (Zpos Coq_xH))

(** val coq_Ox86_INC_instr :
    (wsize -> (register, register_ext, xmm_register, rflag, condt)
    instr_desc_t) * (string * x86_op prim_constructor) **)

let coq_Ox86_INC_instr =
  ((fun sz -> { id_valid = (size_8_64 sz); id_msb_flag = (reg_msb_flag sz);
    id_tin = (w_ty sz); id_in = ((coq_Eu x86_decl O) :: []); id_tout =
    (b4w_ty sz); id_out =
    (cat implicit_flags_noCF ((coq_Eu x86_decl O) :: [])); id_semi =
    (sem_prod_ok (map eval_ltype (w_ty sz)) (Obj.magic x86_INC sz));
    id_args_kinds = (check_neg sz); id_nargs = (S O); id_str_jas =
    (pp_sz "INC" sz); id_safe = []; id_pp_asm = (pp_iname "inc" sz) }),
    ("INC", (prim_8_64 (fun x -> INC x))))

(** val x86_DEC : wsize -> GRing.ComRing.sort -> sem_tuple **)

let x86_DEC sz w =
  rflags_of_aluop_nocf_w sz
    (GRing.add
      (GRing.ComRing.Exports.coq_GRing_ComRing__to__GRing_Nmodule (word sz))
      w
      (GRing.opp
        (GRing.Ring.Exports.join_GRing_Ring_between_GRing_SemiRing_and_GRing_Zmodule
          (GRing.ComRing.Exports.coq_GRing_ComRing__to__GRing_Ring (word sz)))
        (GRing.one
          (GRing.Ring.Exports.coq_GRing_Ring__to__GRing_SemiRing
            (GRing.ComRing.Exports.coq_GRing_ComRing__to__GRing_Ring
              (word sz)))))) (Z.sub (wsigned sz w) (Zpos Coq_xH))

(** val coq_Ox86_DEC_instr :
    (wsize -> (register, register_ext, xmm_register, rflag, condt)
    instr_desc_t) * (string * x86_op prim_constructor) **)

let coq_Ox86_DEC_instr =
  ((fun sz -> { id_valid = (size_8_64 sz); id_msb_flag = (reg_msb_flag sz);
    id_tin = (w_ty sz); id_in = ((coq_Eu x86_decl O) :: []); id_tout =
    (b4w_ty sz); id_out =
    (cat implicit_flags_noCF ((coq_Eu x86_decl O) :: [])); id_semi =
    (sem_prod_ok (map eval_ltype (w_ty sz)) (Obj.magic x86_DEC sz));
    id_args_kinds = (check_neg sz); id_nargs = (S O); id_str_jas =
    (pp_sz "DEC" sz); id_safe = []; id_pp_asm = (pp_iname "dec" sz) }),
    ("DEC", (prim_8_64 (fun x -> DEC x))))

(** val x86_LZCNT : wsize -> GRing.ComRing.sort -> sem_tuple **)

let x86_LZCNT sz w =
  let v = leading_zero sz w in
  flags_w (map (Obj.magic __) (map eval_ltype b5_ty))
    (Obj.magic (None, ((Some (coq_ZF_of_word sz w)), (None, (None, (Some
      (coq_ZF_of_word sz v))))))) sz v

(** val coq_Ox86_LZCNT_instr :
    (wsize -> (register, register_ext, xmm_register, rflag, condt)
    instr_desc_t) * (string * x86_op prim_constructor) **)

let coq_Ox86_LZCNT_instr =
  ((fun sz -> { id_valid = (size_16_64 sz); id_msb_flag = (reg_msb_flag sz);
    id_tin = (w_ty sz); id_in = ((coq_Eu x86_decl (S O)) :: []); id_tout =
    (b5w_ty sz); id_out = (cat implicit_flags ((coq_Eu x86_decl O) :: []));
    id_semi =
    (sem_prod_ok (map eval_ltype (w_ty sz)) (Obj.magic x86_LZCNT sz));
    id_args_kinds = (r_rm :: []); id_nargs = (S (S O)); id_str_jas =
    (pp_sz "LZCNT" sz); id_safe = []; id_pp_asm = (pp_iname "lzcnt" sz) }),
    ("LZCNT", (prim_16_64 (fun x -> LZCNT x))))

(** val x86_TZCNT : wsize -> GRing.ComRing.sort -> sem_tuple **)

let x86_TZCNT sz w =
  let v = trailing_zero sz w in
  flags_w (map (Obj.magic __) (map eval_ltype b5_ty))
    (Obj.magic (None, ((Some (coq_ZF_of_word sz w)), (None, (None, (Some
      (coq_ZF_of_word sz v))))))) sz v

(** val coq_Ox86_TZCNT_instr :
    (wsize -> (register, register_ext, xmm_register, rflag, condt)
    instr_desc_t) * (string * x86_op prim_constructor) **)

let coq_Ox86_TZCNT_instr =
  ((fun sz -> { id_valid = (size_16_64 sz); id_msb_flag = (reg_msb_flag sz);
    id_tin = (w_ty sz); id_in = ((coq_Eu x86_decl (S O)) :: []); id_tout =
    (b5w_ty sz); id_out = (cat implicit_flags ((coq_Eu x86_decl O) :: []));
    id_semi =
    (sem_prod_ok (map eval_ltype (w_ty sz)) (Obj.magic x86_TZCNT sz));
    id_args_kinds = (r_rm :: []); id_nargs = (S (S O)); id_str_jas =
    (pp_sz "TZCNT" sz); id_safe = []; id_pp_asm = (pp_iname "tzcnt" sz) }),
    ("TZCNT", (prim_16_64 (fun x -> TZCNT x))))

(** val x86_BSR : wsize -> GRing.ComRing.sort -> sem_tuple exec **)

let x86_BSR sz w =
  if negb
       (eq_op
         (GRing.ComRing.Exports.coq_GRing_ComRing__to__eqtype_Equality
           (word sz)) w
         (GRing.zero
           (GRing.ComRing.Exports.coq_GRing_ComRing__to__GRing_Nmodule
             (word sz))))
  then Ok
         (flags_w (map (Obj.magic __) (map eval_ltype b5_ty))
           (Obj.magic (None, (None, (None, (None, (Some false)))))) sz
           (GRing.add
             (GRing.ComRing.Exports.coq_GRing_ComRing__to__GRing_Nmodule
               (word sz)) (wrepr sz (Z.sub (wsize_bits sz) (Zpos Coq_xH)))
             (GRing.opp
               (GRing.ComRing.Exports.coq_GRing_ComRing__to__GRing_Zmodule
                 (word sz)) (leading_zero sz w))))
  else let s = ErrArith in Error s

(** val coq_Ox86_BSR_instr :
    (wsize -> (register, register_ext, xmm_register, rflag, condt)
    instr_desc_t) * (string * x86_op prim_constructor) **)

let coq_Ox86_BSR_instr =
  ((fun sz -> { id_valid = (size_16_64 sz); id_msb_flag = MSB_CLEAR; id_tin =
    ((Coq_lword sz) :: []); id_in = ((coq_Eu x86_decl (S O)) :: []);
    id_tout = (b5w_ty sz); id_out =
    (cat implicit_flags ((coq_Ea x86_decl O) :: [])); id_semi =
    (Obj.magic x86_BSR sz); id_args_kinds = (r_rm :: []); id_nargs = (S (S
    O)); id_str_jas = (pp_sz "BSR" sz); id_safe = ((NotZero (sz, O)) :: []);
    id_pp_asm = (pp_iname "bsr" sz) }), ("BSR",
    (prim_16_64 (fun x -> BSR x))))

(** val check_setcc : arg_kind list list list **)

let check_setcc =
  (c :: ((rm false) :: [])) :: []

(** val x86_SETcc : bool -> sem_tuple **)

let x86_SETcc b =
  wrepr U8 (Z.b2z b)

(** val coq_Ox86_SETcc_instr :
    (register, register_ext, xmm_register, rflag, condt)
    instr_desc_t * (string * x86_op prim_constructor) **)

let coq_Ox86_SETcc_instr =
  ({ id_valid = true; id_msb_flag = (reg_msb_flag U8); id_tin = b_ty; id_in =
    ((coq_Eu x86_decl O) :: []); id_tout = w8_ty; id_out =
    ((coq_Eu x86_decl (S O)) :: []); id_semi =
    (sem_prod_ok (map eval_ltype b_ty) (Obj.magic x86_SETcc));
    id_args_kinds = check_setcc; id_nargs = (S (S O)); id_str_jas =
    (pp_s "SETcc"); id_safe = []; id_pp_asm = (pp_ct "set" U8) }, ("SETcc",
    (primM SETcc)))

(** val check_bt : wsize -> arg_kind list list list **)

let check_bt _ =
  (r :: ((ri U8) :: [])) :: []

(** val x86_BT :
    wsize -> GRing.ComRing.sort -> GRing.ComRing.sort -> sem_tuple **)

let x86_BT sz x y =
  Obj.magic (Some (Word0.wbit sz x y))

(** val coq_Ox86_BT_instr :
    (wsize -> (register, register_ext, xmm_register, rflag, condt)
    instr_desc_t) * (string * x86_op prim_constructor) **)

let coq_Ox86_BT_instr =
  ((fun sz -> { id_valid = (size_16_64 sz); id_msb_flag = (reg_msb_flag sz);
    id_tin = (w2_ty sz sz); id_in =
    ((coq_Eu x86_decl O) :: ((coq_Eu x86_decl (S O)) :: [])); id_tout = b_ty;
    id_out = ((coq_F x86_decl CF) :: []); id_semi =
    (sem_prod_ok (map eval_ltype (w2_ty sz sz)) (Obj.magic x86_BT sz));
    id_args_kinds = (check_bt sz); id_nargs = (S (S O)); id_str_jas =
    (pp_sz "BT" sz); id_safe = []; id_pp_asm = (pp_iname "bt" sz) }), ("BT",
    (prim_16_64 (fun x -> BT x))))

(** val x86_CLC : sem_tuple **)

let x86_CLC =
  Obj.magic (Some false)

(** val coq_Ox86_CLC_instr :
    (register, register_ext, xmm_register, rflag, condt)
    instr_desc_t * (string * x86_op prim_constructor) **)

let coq_Ox86_CLC_instr =
  ({ id_valid = true; id_msb_flag = MSB_CLEAR; id_tin = []; id_in = [];
    id_tout = b_ty; id_out = ((coq_F x86_decl CF) :: []); id_semi =
    (sem_prod_ok (map eval_ltype []) x86_CLC); id_args_kinds = ([] :: []);
    id_nargs = O; id_str_jas = (pp_s "CLC"); id_safe = []; id_pp_asm =
    (pp_name "clc" U8) }, ("CLC", (primM CLC)))

(** val x86_STC : sem_tuple **)

let x86_STC =
  Obj.magic (Some true)

(** val coq_Ox86_STC_instr :
    (register, register_ext, xmm_register, rflag, condt)
    instr_desc_t * (string * x86_op prim_constructor) **)

let coq_Ox86_STC_instr =
  ({ id_valid = true; id_msb_flag = MSB_CLEAR; id_tin = []; id_in = [];
    id_tout = b_ty; id_out = ((coq_F x86_decl CF) :: []); id_semi =
    (sem_prod_ok (map eval_ltype []) x86_STC); id_args_kinds = ([] :: []);
    id_nargs = O; id_str_jas = (pp_s "STC"); id_safe = []; id_pp_asm =
    (pp_name "stc" U8) }, ("STC", (primM STC)))

(** val check_lea : wsize -> arg_kind list list list **)

let check_lea _ =
  (r :: ((m true) :: [])) :: []

(** val x86_LEA : wsize -> GRing.ComRing.sort -> sem_tuple **)

let x86_LEA _ addr =
  addr

(** val coq_Ox86_LEA_instr :
    (wsize -> (register, register_ext, xmm_register, rflag, condt)
    instr_desc_t) * (string * x86_op prim_constructor) **)

let coq_Ox86_LEA_instr =
  ((fun sz -> { id_valid = (size_16_64 sz); id_msb_flag = (reg_msb_flag sz);
    id_tin = (w_ty sz); id_in = ((coq_Ec x86_decl (S O)) :: []); id_tout =
    (w_ty sz); id_out = ((coq_Eu x86_decl O) :: []); id_semi =
    (sem_prod_ok (map eval_ltype (w_ty sz)) (Obj.magic x86_LEA sz));
    id_args_kinds = (check_lea sz); id_nargs = (S (S O)); id_str_jas =
    (pp_sz "LEA" sz); id_safe = []; id_pp_asm = (pp_iname "lea" sz) }),
    ("LEA", (prim_16_64 (fun x -> LEA x))))

(** val check_test : wsize -> arg_kind list list list **)

let check_test sz =
  ((rm false) :: ((ri (max_32 sz)) :: [])) :: []

(** val x86_TEST :
    wsize -> GRing.ComRing.sort -> GRing.ComRing.sort -> sem_tuple **)

let x86_TEST sz x y =
  rflags_of_bwop sz (Word0.wand sz x y)

(** val coq_Ox86_TEST_instr :
    (wsize -> (register, register_ext, xmm_register, rflag, condt)
    instr_desc_t) * (string * x86_op prim_constructor) **)

let coq_Ox86_TEST_instr =
  ((fun sz -> { id_valid = (size_8_64 sz); id_msb_flag = (reg_msb_flag sz);
    id_tin = (w2_ty sz sz); id_in =
    ((coq_Eu x86_decl O) :: ((coq_Eu x86_decl (S O)) :: [])); id_tout =
    b5_ty; id_out = implicit_flags; id_semi =
    (sem_prod_ok (map eval_ltype (w2_ty sz sz)) (Obj.magic x86_TEST sz));
    id_args_kinds = (check_test sz); id_nargs = (S (S O)); id_str_jas =
    (pp_sz "TEST" sz); id_safe = []; id_pp_asm = (pp_iname "test" sz) }),
    ("TEST", (prim_8_64 (fun x -> TEST x))))

(** val check_cmp : wsize -> arg_kind list list list **)

let check_cmp sz =
  ((rm false) :: ((ri (max_32 sz)) :: [])) :: (r_rm :: [])

(** val x86_CMP :
    wsize -> GRing.ComRing.sort -> GRing.ComRing.sort -> sem_tuple **)

let x86_CMP sz x y =
  rflags_of_aluop sz
    (GRing.add
      (GRing.ComRing.Exports.coq_GRing_ComRing__to__GRing_Nmodule (word sz))
      x
      (GRing.opp
        (GRing.ComRing.Exports.coq_GRing_ComRing__to__GRing_Zmodule (word sz))
        y)) (Z.sub (wunsigned sz x) (wunsigned sz y))
    (Z.sub (wsigned sz x) (wsigned sz y))

(** val coq_Ox86_CMP_instr :
    (wsize -> (register, register_ext, xmm_register, rflag, condt)
    instr_desc_t) * (string * x86_op prim_constructor) **)

let coq_Ox86_CMP_instr =
  ((fun sz -> { id_valid = (size_8_64 sz); id_msb_flag = (reg_msb_flag sz);
    id_tin = (w2_ty sz sz); id_in =
    ((coq_Eu x86_decl O) :: ((coq_Eu x86_decl (S O)) :: [])); id_tout =
    b5_ty; id_out = implicit_flags; id_semi =
    (sem_prod_ok (map eval_ltype (w2_ty sz sz)) (Obj.magic x86_CMP sz));
    id_args_kinds = (check_cmp sz); id_nargs = (S (S O)); id_str_jas =
    (pp_sz "CMP" sz); id_safe = []; id_pp_asm = (pp_iname "cmp" sz) }),
    ("CMP", (prim_8_64 (fun x -> CMP x))))

(** val x86_AND :
    wsize -> GRing.ComRing.sort -> GRing.ComRing.sort -> sem_tuple **)

let x86_AND sz v1 v2 =
  rflags_of_bwop_w sz (Word0.wand sz v1 v2)

(** val coq_Ox86_AND_instr :
    (wsize -> (register, register_ext, xmm_register, rflag, condt)
    instr_desc_t) * (string * x86_op prim_constructor) **)

let coq_Ox86_AND_instr =
  ((fun sz -> { id_valid = (size_8_64 sz); id_msb_flag = (reg_msb_flag sz);
    id_tin = (w2_ty sz sz); id_in =
    ((coq_Eu x86_decl O) :: ((coq_Eu x86_decl (S O)) :: [])); id_tout =
    (b5w_ty sz); id_out = (cat implicit_flags ((coq_Eu x86_decl O) :: []));
    id_semi =
    (sem_prod_ok (map eval_ltype (w2_ty sz sz)) (Obj.magic x86_AND sz));
    id_args_kinds = (check_cmp sz); id_nargs = (S (S O)); id_str_jas =
    (pp_sz "AND" sz); id_safe = []; id_pp_asm = (pp_iname "and" sz) }),
    ("AND", (prim_8_64 (fun x -> AND x))))

(** val x86_OR :
    wsize -> GRing.ComRing.sort -> GRing.ComRing.sort -> sem_tuple **)

let x86_OR sz v1 v2 =
  rflags_of_bwop_w sz (Word0.wor sz v1 v2)

(** val coq_Ox86_OR_instr :
    (wsize -> (register, register_ext, xmm_register, rflag, condt)
    instr_desc_t) * (string * x86_op prim_constructor) **)

let coq_Ox86_OR_instr =
  ((fun sz -> { id_valid = (size_8_64 sz); id_msb_flag = (reg_msb_flag sz);
    id_tin = (w2_ty sz sz); id_in =
    ((coq_Eu x86_decl O) :: ((coq_Eu x86_decl (S O)) :: [])); id_tout =
    (b5w_ty sz); id_out = (cat implicit_flags ((coq_Eu x86_decl O) :: []));
    id_semi =
    (sem_prod_ok (map eval_ltype (w2_ty sz sz)) (Obj.magic x86_OR sz));
    id_args_kinds = (check_cmp sz); id_nargs = (S (S O)); id_str_jas =
    (pp_sz "OR" sz); id_safe = []; id_pp_asm = (pp_iname "or" sz) }), ("OR",
    (prim_8_64 (fun x -> OR x))))

(** val x86_XOR :
    wsize -> GRing.ComRing.sort -> GRing.ComRing.sort -> sem_tuple **)

let x86_XOR sz v1 v2 =
  rflags_of_bwop_w sz (Word0.wxor sz v1 v2)

(** val coq_Ox86_XOR_instr :
    (wsize -> (register, register_ext, xmm_register, rflag, condt)
    instr_desc_t) * (string * x86_op prim_constructor) **)

let coq_Ox86_XOR_instr =
  ((fun sz -> { id_valid = (size_8_64 sz); id_msb_flag = (reg_msb_flag sz);
    id_tin = (w2_ty sz sz); id_in =
    ((coq_Eu x86_decl O) :: ((coq_Eu x86_decl (S O)) :: [])); id_tout =
    (b5w_ty sz); id_out = (cat implicit_flags ((coq_Eu x86_decl O) :: []));
    id_semi =
    (sem_prod_ok (map eval_ltype (w2_ty sz sz)) (Obj.magic x86_XOR sz));
    id_args_kinds = (check_cmp sz); id_nargs = (S (S O)); id_str_jas =
    (pp_sz "XOR" sz); id_safe = []; id_pp_asm = (pp_iname "xor" sz) }),
    ("XOR", (prim_8_64 (fun x -> XOR x))))

(** val check_andn : wsize -> arg_kind list list list **)

let check_andn _ =
  (r :: (r :: ((rm true) :: []))) :: []

(** val x86_ANDN :
    wsize -> GRing.ComRing.sort -> GRing.ComRing.sort -> sem_tuple **)

let x86_ANDN sz v1 v2 =
  let w = wandn sz v1 v2 in
  flags_w (map (Obj.magic __) (map eval_ltype b5_ty)) (rflags_of_andn sz w)
    sz w

(** val coq_Ox86_ANDN_instr :
    (wsize -> (register, register_ext, xmm_register, rflag, condt)
    instr_desc_t) * (string * x86_op prim_constructor) **)

let coq_Ox86_ANDN_instr =
  ((fun sz -> { id_valid = (size_32_64 sz); id_msb_flag = (reg_msb_flag sz);
    id_tin = (w2_ty sz sz); id_in =
    ((coq_Eu x86_decl (S O)) :: ((coq_Eu x86_decl (S (S O))) :: []));
    id_tout = (b5w_ty sz); id_out =
    (cat implicit_flags ((coq_Eu x86_decl O) :: [])); id_semi =
    (sem_prod_ok (map eval_ltype (w2_ty sz sz)) (Obj.magic x86_ANDN sz));
    id_args_kinds = (check_andn sz); id_nargs = (S (S (S O))); id_str_jas =
    (pp_sz "ANDN" sz); id_safe = []; id_pp_asm = (pp_iname "andn" sz) }),
    ("ANDN", (prim_32_64 (fun x -> ANDN x))))

(** val x86_NOT : wsize -> GRing.ComRing.sort -> sem_tuple **)

let x86_NOT =
  wnot

(** val coq_Ox86_NOT_instr :
    (wsize -> (register, register_ext, xmm_register, rflag, condt)
    instr_desc_t) * (string * x86_op prim_constructor) **)

let coq_Ox86_NOT_instr =
  ((fun sz -> { id_valid = (size_8_64 sz); id_msb_flag = (reg_msb_flag sz);
    id_tin = (w_ty sz); id_in = ((coq_Eu x86_decl O) :: []); id_tout =
    (w_ty sz); id_out = ((coq_Eu x86_decl O) :: []); id_semi =
    (sem_prod_ok (map eval_ltype (w_ty sz)) (Obj.magic x86_NOT sz));
    id_args_kinds = (check_neg sz); id_nargs = (S O); id_str_jas =
    (pp_sz "NOT" sz); id_safe = []; id_pp_asm = (pp_iname "not" sz) }),
    ("NOT", (prim_8_64 (fun x -> NOT x))))

(** val check_ror : wsize -> arg_kind list list list **)

let check_ror _ =
  ((rm false) :: ((ri U8) :: [])) :: []

(** val x86_shift_mask : wsize -> GRing.ComRing.sort **)

let x86_shift_mask = function
| U64 -> wrepr U8 (Zpos (Coq_xI (Coq_xI (Coq_xI (Coq_xI (Coq_xI Coq_xH))))))
| U128 ->
  wrepr U8 (Zpos (Coq_xI (Coq_xI (Coq_xI (Coq_xI (Coq_xI (Coq_xI Coq_xH)))))))
| U256 ->
  wrepr U8 (Zpos (Coq_xI (Coq_xI (Coq_xI (Coq_xI (Coq_xI (Coq_xI (Coq_xI
    Coq_xH))))))))
| _ -> wrepr U8 (Zpos (Coq_xI (Coq_xI (Coq_xI (Coq_xI Coq_xH)))))

(** val x86_ROR :
    wsize -> GRing.ComRing.sort -> GRing.ComRing.sort -> sem_tuple **)

let x86_ROR sz v i0 =
  let i1 = Word0.wand U8 i0 (x86_shift_mask sz) in
  if eq_op
       (GRing.ComRing.Exports.coq_GRing_ComRing__to__eqtype_Equality
         (word U8)) i1
       (GRing.zero
         (GRing.ComRing.Exports.coq_GRing_ComRing__to__GRing_Nmodule
           (word U8)))
  then Obj.magic (None, (None, v))
  else let r0 = wror sz v (wunsigned U8 i1) in
       let cF = msb sz r0 in
       let oF =
         if eq_op
              (GRing.ComRing.Exports.coq_GRing_ComRing__to__eqtype_Equality
                (word U8)) i1
              (GRing.one
                (GRing.ComRing.Exports.coq_GRing_ComRing__to__GRing_SemiRing
                  (word U8)))
         then Some
                (negb
                  (eq_op coq_Datatypes_bool__canonical__eqtype_Equality
                    (Obj.magic cF) (Obj.magic msb sz v)))
         else None
       in
       Obj.magic (oF, ((Some cF), r0))

(** val coq_Ox86_ROR_instr :
    (wsize -> (register, register_ext, xmm_register, rflag, condt)
    instr_desc_t) * (string * x86_op prim_constructor) **)

let coq_Ox86_ROR_instr =
  ((fun sz -> { id_valid = (size_8_64 sz); id_msb_flag = (reg_msb_flag sz);
    id_tin = (ww8_ty sz); id_in =
    ((coq_Eu x86_decl O) :: ((coq_Ef x86_decl (S O) RCX) :: [])); id_tout =
    (b2w_ty sz); id_out =
    ((coq_F x86_decl OF) :: ((coq_F x86_decl CF) :: ((coq_Eu x86_decl O) :: [])));
    id_semi =
    (sem_prod_ok (map eval_ltype (ww8_ty sz)) (Obj.magic x86_ROR sz));
    id_args_kinds = (check_ror sz); id_nargs = (S (S O)); id_str_jas =
    (pp_sz "ROR" sz); id_safe = []; id_pp_asm = (pp_iname_w_8 "ror" sz) }),
    ("ROR", (prim_8_64 (fun x -> ROR x))))

(** val x86_ROL :
    wsize -> GRing.ComRing.sort -> GRing.ComRing.sort -> sem_tuple **)

let x86_ROL sz v i0 =
  let i1 = Word0.wand U8 i0 (x86_shift_mask sz) in
  if eq_op
       (GRing.ComRing.Exports.coq_GRing_ComRing__to__eqtype_Equality
         (word U8)) i1
       (GRing.zero
         (GRing.ComRing.Exports.coq_GRing_ComRing__to__GRing_Nmodule
           (word U8)))
  then Obj.magic (None, (None, v))
  else let r0 = wrol sz v (wunsigned U8 i1) in
       let cF = lsb sz r0 in
       let oF =
         if eq_op
              (GRing.ComRing.Exports.coq_GRing_ComRing__to__eqtype_Equality
                (word U8)) i1
              (GRing.one
                (GRing.ComRing.Exports.coq_GRing_ComRing__to__GRing_SemiRing
                  (word U8)))
         then Some
                (negb
                  (eq_op coq_Datatypes_bool__canonical__eqtype_Equality
                    (Obj.magic msb sz r0) (Obj.magic cF)))
         else None
       in
       Obj.magic (oF, ((Some cF), r0))

(** val coq_Ox86_ROL_instr :
    (wsize -> (register, register_ext, xmm_register, rflag, condt)
    instr_desc_t) * (string * x86_op prim_constructor) **)

let coq_Ox86_ROL_instr =
  ((fun sz -> { id_valid = (size_8_64 sz); id_msb_flag = (reg_msb_flag sz);
    id_tin = (ww8_ty sz); id_in =
    ((coq_Eu x86_decl O) :: ((coq_Ef x86_decl (S O) RCX) :: [])); id_tout =
    (b2w_ty sz); id_out =
    ((coq_F x86_decl OF) :: ((coq_F x86_decl CF) :: ((coq_Eu x86_decl O) :: [])));
    id_semi =
    (sem_prod_ok (map eval_ltype (ww8_ty sz)) (Obj.magic x86_ROL sz));
    id_args_kinds = (check_ror sz); id_nargs = (S (S O)); id_str_jas =
    (pp_sz "ROL" sz); id_safe = []; id_pp_asm = (pp_iname_w_8 "rol" sz) }),
    ("ROL", (prim_8_64 (fun x -> ROL x))))

(** val x86_rotate_with_carry :
    wsize -> (word -> nat -> word) -> (GRing.ComRing.sort -> bool -> bool) ->
    GRing.ComRing.sort -> GRing.ComRing.sort -> bool -> sem_tuple **)

let x86_rotate_with_carry sz rot ovf v i0 cf =
  let i1 = Word0.wand U8 i0 (x86_shift_mask sz) in
  let i2 =
    match sz with
    | U8 ->
      Z.modulo (wunsigned U8 i1) (Zpos (Coq_xI (Coq_xO (Coq_xO Coq_xH))))
    | U16 ->
      Z.modulo (wunsigned U8 i1) (Zpos (Coq_xI (Coq_xO (Coq_xO (Coq_xO
        Coq_xH)))))
    | _ -> wunsigned U8 i1
  in
  let r0 =
    t2w (S (S (wsize_size_minus_1 sz)))
      (tuple (S (S (wsize_size_minus_1 sz)))
        (cons_tuple (S (wsize_size_minus_1 sz)) cf
          (w2t (S (wsize_size_minus_1 sz)) (Obj.magic v))) (fun _ ->
        cf :: (w2t (S (wsize_size_minus_1 sz)) (Obj.magic v))))
  in
  let r1 = rot r0 (Z.to_nat i2) in
  let r2 = w2t (S (nat_of_wsize sz)) r1 in
  let cF = head false r2 in
  let r3 =
    t2w (pred (S (nat_of_wsize sz)))
      (tuple (pred (S (nat_of_wsize sz)))
        (behead_tuple (S (nat_of_wsize sz)) r2) (fun _ -> behead r2))
  in
  let oF =
    if eq_op coq_BinNums_Z__canonical__eqtype_Equality (Obj.magic i2)
         (GRing.one coq_BinNums_Z__canonical__GRing_SemiRing)
    then Some (Obj.magic ovf r3 cF)
    else None
  in
  Obj.magic (oF, ((Some cF), r3))

(** val x86_RCR :
    wsize -> GRing.ComRing.sort -> GRing.ComRing.sort -> bool -> sem_tuple **)

let x86_RCR sz v i0 cf =
  x86_rotate_with_carry sz (rotr (S (nat_of_wsize sz))) (fun _ _ ->
    negb
      (eq_op coq_Datatypes_bool__canonical__eqtype_Equality
        (Obj.magic msb sz v) (Obj.magic cf))) v i0 cf

(** val coq_Ox86_RCR_instr :
    (wsize -> (register, register_ext, xmm_register, rflag, condt)
    instr_desc_t) * (string * x86_op prim_constructor) **)

let coq_Ox86_RCR_instr =
  ((fun sz -> { id_valid = (size_8_64 sz); id_msb_flag = (reg_msb_flag sz);
    id_tin = (ww8b_ty sz); id_in =
    ((coq_Eu x86_decl O) :: ((coq_Ef x86_decl (S O) RCX) :: ((coq_F x86_decl
                                                               CF) :: [])));
    id_tout = (b2w_ty sz); id_out =
    ((coq_F x86_decl OF) :: ((coq_F x86_decl CF) :: ((coq_Eu x86_decl O) :: [])));
    id_semi =
    (sem_prod_ok (map eval_ltype (ww8b_ty sz)) (Obj.magic x86_RCR sz));
    id_args_kinds = (check_ror sz); id_nargs = (S (S O)); id_str_jas =
    (pp_sz "RCR" sz); id_safe = []; id_pp_asm = (pp_iname_w_8 "rcr" sz) }),
    ("RCR", (prim_8_64 (fun x -> RCR x))))

(** val x86_RCL :
    wsize -> GRing.ComRing.sort -> GRing.ComRing.sort -> bool -> sem_tuple **)

let x86_RCL sz v i0 cf =
  x86_rotate_with_carry sz (rotl (S (nat_of_wsize sz))) (fun r0 c0 ->
    negb
      (eq_op coq_Datatypes_bool__canonical__eqtype_Equality
        (Obj.magic msb sz r0) (Obj.magic c0))) v i0 cf

(** val coq_Ox86_RCL_instr :
    (wsize -> (register, register_ext, xmm_register, rflag, condt)
    instr_desc_t) * (string * x86_op prim_constructor) **)

let coq_Ox86_RCL_instr =
  ((fun sz -> { id_valid = (size_8_64 sz); id_msb_flag = (reg_msb_flag sz);
    id_tin = (ww8b_ty sz); id_in =
    ((coq_Eu x86_decl O) :: ((coq_Ef x86_decl (S O) RCX) :: ((coq_F x86_decl
                                                               CF) :: [])));
    id_tout = (b2w_ty sz); id_out =
    ((coq_F x86_decl OF) :: ((coq_F x86_decl CF) :: ((coq_Eu x86_decl O) :: [])));
    id_semi =
    (sem_prod_ok (map eval_ltype (ww8b_ty sz)) (Obj.magic x86_RCL sz));
    id_args_kinds = (check_ror sz); id_nargs = (S (S O)); id_str_jas =
    (pp_sz "RCL" sz); id_safe = []; id_pp_asm = (pp_iname_w_8 "rcl" sz) }),
    ("RCL", (prim_8_64 (fun x -> RCL x))))

(** val rflags_OF :
    wsize -> wsize -> GRing.ComRing.sort -> GRing.ComRing.sort -> bool ->
    bool -> sem_tuple **)

let rflags_OF s sz i0 r0 rc oF =
  let oF0 =
    if eq_op
         (GRing.ComRing.Exports.coq_GRing_ComRing__to__eqtype_Equality
           (word s)) i0
         (GRing.one
           (GRing.ComRing.Exports.coq_GRing_ComRing__to__GRing_SemiRing
             (word s)))
    then Some oF
    else None
  in
  let cF = Some rc in
  let sF = Some (coq_SF_of_word sz r0) in
  let pF = Some (coq_PF_of_word sz r0) in
  let zF = Some (coq_ZF_of_word sz r0) in
  Obj.magic (oF0, (cF, (sF, (pF, (zF, r0)))))

(** val x86_SHL :
    wsize -> GRing.ComRing.sort -> GRing.ComRing.sort -> sem_tuple **)

let x86_SHL sz v i0 =
  let i1 = Word0.wand U8 i0 (x86_shift_mask sz) in
  if eq_op
       (GRing.ComRing.Exports.coq_GRing_ComRing__to__eqtype_Equality
         (word U8)) i1
       (GRing.zero
         (GRing.ComRing.Exports.coq_GRing_ComRing__to__GRing_Nmodule
           (word U8)))
  then rflags_None_w sz v
  else let rc = msb sz (wshl sz v (Z.sub (wunsigned U8 i1) (Zpos Coq_xH))) in
       let r0 = wshl sz v (wunsigned U8 i1) in
       rflags_OF U8 sz i1 r0 rc (addb (msb sz r0) rc)

(** val coq_Ox86_SHL_instr :
    (wsize -> (register, register_ext, xmm_register, rflag, condt)
    instr_desc_t) * (string * x86_op prim_constructor) **)

let coq_Ox86_SHL_instr =
  ((fun sz -> { id_valid = (size_8_64 sz); id_msb_flag = (reg_msb_flag sz);
    id_tin = (ww8_ty sz); id_in =
    ((coq_Eu x86_decl O) :: ((coq_Ef x86_decl (S O) RCX) :: [])); id_tout =
    (b5w_ty sz); id_out = (cat implicit_flags ((coq_Eu x86_decl O) :: []));
    id_semi =
    (sem_prod_ok (map eval_ltype (ww8_ty sz)) (Obj.magic x86_SHL sz));
    id_args_kinds = (check_ror sz); id_nargs = (S (S O)); id_str_jas =
    (pp_sz "SHL" sz); id_safe = []; id_pp_asm = (pp_iname_w_8 "shl" sz) }),
    ("SHL", (prim_8_64 (fun x -> SHL x))))

(** val x86_SHR :
    wsize -> GRing.ComRing.sort -> GRing.ComRing.sort -> sem_tuple **)

let x86_SHR sz v i0 =
  let i1 = Word0.wand U8 i0 (x86_shift_mask sz) in
  if eq_op
       (GRing.ComRing.Exports.coq_GRing_ComRing__to__eqtype_Equality
         (word U8)) i1
       (GRing.zero
         (GRing.ComRing.Exports.coq_GRing_ComRing__to__GRing_Nmodule
           (word U8)))
  then rflags_None_w sz v
  else let rc = lsb sz (wshr sz v (Z.sub (wunsigned U8 i1) (Zpos Coq_xH))) in
       let r0 = wshr sz v (wunsigned U8 i1) in
       rflags_OF U8 sz i1 r0 rc (msb sz v)

(** val coq_Ox86_SHR_instr :
    (wsize -> (register, register_ext, xmm_register, rflag, condt)
    instr_desc_t) * (string * x86_op prim_constructor) **)

let coq_Ox86_SHR_instr =
  ((fun sz -> { id_valid = (size_8_64 sz); id_msb_flag = (reg_msb_flag sz);
    id_tin = (ww8_ty sz); id_in =
    ((coq_Eu x86_decl O) :: ((coq_Ef x86_decl (S O) RCX) :: [])); id_tout =
    (b5w_ty sz); id_out = (cat implicit_flags ((coq_Eu x86_decl O) :: []));
    id_semi =
    (sem_prod_ok (map eval_ltype (ww8_ty sz)) (Obj.magic x86_SHR sz));
    id_args_kinds = (check_ror sz); id_nargs = (S (S O)); id_str_jas =
    (pp_sz "SHR" sz); id_safe = []; id_pp_asm = (pp_iname_w_8 "shr" sz) }),
    ("SHR", (prim_8_64 (fun x -> SHR x))))

(** val coq_Ox86_SAL_instr :
    (wsize -> (register, register_ext, xmm_register, rflag, condt)
    instr_desc_t) * (string * x86_op prim_constructor) **)

let coq_Ox86_SAL_instr =
  ((fun sz -> { id_valid = (size_8_64 sz); id_msb_flag = (reg_msb_flag sz);
    id_tin = (ww8_ty sz); id_in =
    ((coq_Eu x86_decl O) :: ((coq_Ef x86_decl (S O) RCX) :: [])); id_tout =
    (b5w_ty sz); id_out = (cat implicit_flags ((coq_Eu x86_decl O) :: []));
    id_semi =
    (sem_prod_ok (map eval_ltype (ww8_ty sz)) (Obj.magic x86_SHL sz));
    id_args_kinds = (check_ror sz); id_nargs = (S (S O)); id_str_jas =
    (pp_sz "SAL" sz); id_safe = []; id_pp_asm = (pp_iname_w_8 "sal" sz) }),
    ("SAL", (prim_8_64 (fun x -> SAL x))))

(** val x86_SAR :
    wsize -> GRing.ComRing.sort -> GRing.ComRing.sort -> sem_tuple **)

let x86_SAR sz v i0 =
  let i1 = Word0.wand U8 i0 (x86_shift_mask sz) in
  if eq_op
       (GRing.ComRing.Exports.coq_GRing_ComRing__to__eqtype_Equality
         (word U8)) i1
       (GRing.zero
         (GRing.ComRing.Exports.coq_GRing_ComRing__to__GRing_Nmodule
           (word U8)))
  then rflags_None_w sz v
  else let rc = lsb sz (wsar sz v (Z.sub (wunsigned U8 i1) (Zpos Coq_xH))) in
       let r0 = wsar sz v (wunsigned U8 i1) in rflags_OF U8 sz i1 r0 rc false

(** val coq_Ox86_SAR_instr :
    (wsize -> (register, register_ext, xmm_register, rflag, condt)
    instr_desc_t) * (string * x86_op prim_constructor) **)

let coq_Ox86_SAR_instr =
  ((fun sz -> { id_valid = (size_8_64 sz); id_msb_flag = (reg_msb_flag sz);
    id_tin = (ww8_ty sz); id_in =
    ((coq_Eu x86_decl O) :: ((coq_Ef x86_decl (S O) RCX) :: [])); id_tout =
    (b5w_ty sz); id_out = (cat implicit_flags ((coq_Eu x86_decl O) :: []));
    id_semi =
    (sem_prod_ok (map eval_ltype (ww8_ty sz)) (Obj.magic x86_SAR sz));
    id_args_kinds = (check_ror sz); id_nargs = (S (S O)); id_str_jas =
    (pp_sz "SAR" sz); id_safe = []; id_pp_asm = (pp_iname_w_8 "sar" sz) }),
    ("SAR", (prim_8_64 (fun x -> SAR x))))

(** val check_shld : wsize -> arg_kind list list list **)

let check_shld _ =
  ((rm false) :: (r :: ((ri U8) :: []))) :: []

(** val safe_shxd : wsize -> safe_cond list **)

let safe_shxd = function
| U16 ->
  (InRangeMod32 (U8, Z0, (Zpos (Coq_xO (Coq_xO (Coq_xO (Coq_xO Coq_xH))))),
    (S (S O)))) :: []
| _ -> []

(** val x86_SHLD :
    wsize -> GRing.ComRing.sort -> GRing.ComRing.sort -> GRing.ComRing.sort
    -> sem_tuple exec **)

let x86_SHLD sz v1 v2 i0 =
  let i1 = Word0.wand U8 i0 (x86_shift_mask sz) in
  if eq_op
       (GRing.ComRing.Exports.coq_GRing_ComRing__to__eqtype_Equality
         (word U8)) i1
       (GRing.zero
         (GRing.ComRing.Exports.coq_GRing_ComRing__to__GRing_Nmodule
           (word U8)))
  then Ok (rflags_None_w sz v1)
  else let j = Z.sub (wsize_bits sz) (wunsigned U8 i1) in
       if Z.ltb j Z0
       then Error ErrArith
       else let rc =
              msb sz (wshl sz v1 (Z.sub (wunsigned U8 i1) (Zpos Coq_xH)))
            in
            let r1 = wshl sz v1 (wunsigned U8 i1) in
            let r2 = wshr sz v2 j in
            let r0 = Word0.wor sz r1 r2 in
            Ok (rflags_OF U8 sz i1 r0 rc (addb (msb sz r0) rc))

(** val coq_Ox86_SHLD_instr :
    (wsize -> (register, register_ext, xmm_register, rflag, condt)
    instr_desc_t) * (string * x86_op prim_constructor) **)

let coq_Ox86_SHLD_instr =
  ((fun sz -> { id_valid = (size_16_64 sz); id_msb_flag = (reg_msb_flag sz);
    id_tin = (w2w8_ty sz); id_in =
    ((coq_Eu x86_decl O) :: ((coq_Ea x86_decl (S O)) :: ((coq_Ef x86_decl (S
                                                           (S O)) RCX) :: [])));
    id_tout = (b5w_ty sz); id_out =
    (cat implicit_flags ((coq_Eu x86_decl O) :: [])); id_semi =
    (Obj.magic x86_SHLD sz); id_args_kinds = (check_shld sz); id_nargs = (S
    (S (S O))); id_str_jas = (pp_sz "SHLD" sz); id_safe = (safe_shxd sz);
    id_pp_asm = (pp_iname_ww_8 "shld" sz) }), ("SHLD",
    (prim_16_64 (fun x -> SHLD x))))

(** val x86_SHRD :
    wsize -> GRing.ComRing.sort -> GRing.ComRing.sort -> GRing.ComRing.sort
    -> sem_tuple exec **)

let x86_SHRD sz v1 v2 i0 =
  let i1 = Word0.wand U8 i0 (x86_shift_mask sz) in
  if eq_op
       (GRing.ComRing.Exports.coq_GRing_ComRing__to__eqtype_Equality
         (word U8)) i1
       (GRing.zero
         (GRing.ComRing.Exports.coq_GRing_ComRing__to__GRing_Nmodule
           (word U8)))
  then Ok (rflags_None_w sz v1)
  else let j = Z.sub (wsize_bits sz) (wunsigned U8 i1) in
       if Z.ltb j Z0
       then Error ErrArith
       else let rc =
              lsb sz (wshr sz v1 (Z.sub (wunsigned U8 i1) (Zpos Coq_xH)))
            in
            let r1 = wshr sz v1 (wunsigned U8 i1) in
            let r2 = wshl sz v2 j in
            let r0 = Word0.wor sz r1 r2 in
            Ok (rflags_OF U8 sz i1 r0 rc (addb (msb sz r0) (msb sz v1)))

(** val coq_Ox86_SHRD_instr :
    (wsize -> (register, register_ext, xmm_register, rflag, condt)
    instr_desc_t) * (string * x86_op prim_constructor) **)

let coq_Ox86_SHRD_instr =
  ((fun sz -> { id_valid = (size_16_64 sz); id_msb_flag = (reg_msb_flag sz);
    id_tin = (w2w8_ty sz); id_in =
    ((coq_Eu x86_decl O) :: ((coq_Ea x86_decl (S O)) :: ((coq_Ef x86_decl (S
                                                           (S O)) RCX) :: [])));
    id_tout = (b5w_ty sz); id_out =
    (cat implicit_flags ((coq_Eu x86_decl O) :: [])); id_semi =
    (Obj.magic x86_SHRD sz); id_args_kinds = (check_shld sz); id_nargs = (S
    (S (S O))); id_str_jas = (pp_sz "SHRD" sz); id_safe = (safe_shxd sz);
    id_pp_asm = (pp_iname_ww_8 "shrd" sz) }), ("SHRD",
    (prim_16_64 (fun x -> SHRD x))))

(** val check_rorx : wsize -> arg_kind list list list **)

let check_rorx _ =
  (r :: ((rm true) :: ((i U8) :: []))) :: []

(** val x86_RORX :
    wsize -> GRing.ComRing.sort -> GRing.ComRing.sort -> GRing.ComRing.sort **)

let x86_RORX sz v i0 =
  let i1 = Word0.wand U8 i0 (x86_shift_mask sz) in wror sz v (wunsigned U8 i1)

(** val coq_Ox86_RORX_instr :
    (wsize -> (register, register_ext, xmm_register, rflag, condt)
    instr_desc_t) * (string * x86_op prim_constructor) **)

let coq_Ox86_RORX_instr =
  ((fun sz -> { id_valid = (size_32_64 sz); id_msb_flag = (reg_msb_flag sz);
    id_tin = (ww8_ty sz); id_in =
    ((coq_Eu x86_decl (S O)) :: ((coq_Ea x86_decl (S (S O))) :: []));
    id_tout = (w_ty sz); id_out = ((coq_Ea x86_decl O) :: []); id_semi =
    (sem_prod_ok (map eval_ltype (ww8_ty sz)) (Obj.magic x86_RORX sz));
    id_args_kinds = (check_rorx sz); id_nargs = (S (S (S O))); id_str_jas =
    (pp_sz "RORX" sz); id_safe = []; id_pp_asm = (pp_name "rorx" sz) }),
    ("RORX", (prim_32_64 (fun x -> RORX x))))

(** val x86_bmi_shift :
    wsize -> (GRing.ComRing.sort -> coq_Z -> GRing.ComRing.sort) ->
    GRing.ComRing.sort -> GRing.ComRing.sort -> GRing.ComRing.sort **)

let x86_bmi_shift sz op v i0 =
  let i1 = Z.coq_land (wunsigned sz i0) (wunsigned U8 (x86_shift_mask sz)) in
  op v i1

(** val check_sarx : wsize -> arg_kind list list list **)

let check_sarx _ =
  (r :: ((rm true) :: (r :: []))) :: []

(** val x86_SARX :
    wsize -> GRing.ComRing.sort -> GRing.ComRing.sort -> GRing.ComRing.sort **)

let x86_SARX sz =
  x86_bmi_shift sz (wsar sz)

(** val coq_Ox86_SARX_instr :
    (wsize -> (register, register_ext, xmm_register, rflag, condt)
    instr_desc_t) * (string * x86_op prim_constructor) **)

let coq_Ox86_SARX_instr =
  ((fun sz -> { id_valid = (size_32_64 sz); id_msb_flag = MSB_CLEAR; id_tin =
    (w2_ty sz sz); id_in =
    ((coq_Ea x86_decl (S O)) :: ((coq_Eu x86_decl (S (S O))) :: []));
    id_tout = (w_ty sz); id_out = ((coq_Ea x86_decl O) :: []); id_semi =
    (sem_prod_ok (map eval_ltype (w2_ty sz sz)) (Obj.magic x86_SARX sz));
    id_args_kinds = (check_sarx sz); id_nargs = (S (S (S O))); id_str_jas =
    (pp_sz "SARX" sz); id_safe = []; id_pp_asm = (pp_name "sarx" sz) }),
    ("SARX", (prim_32_64 (fun x -> SARX x))))

(** val x86_SHRX :
    wsize -> GRing.ComRing.sort -> GRing.ComRing.sort -> GRing.ComRing.sort **)

let x86_SHRX sz =
  x86_bmi_shift sz (wshr sz)

(** val coq_Ox86_SHRX_instr :
    (wsize -> (register, register_ext, xmm_register, rflag, condt)
    instr_desc_t) * (string * x86_op prim_constructor) **)

let coq_Ox86_SHRX_instr =
  ((fun sz -> { id_valid = (size_32_64 sz); id_msb_flag = MSB_CLEAR; id_tin =
    (w2_ty sz sz); id_in =
    ((coq_Ea x86_decl (S O)) :: ((coq_Eu x86_decl (S (S O))) :: []));
    id_tout = (w_ty sz); id_out = ((coq_Ea x86_decl O) :: []); id_semi =
    (sem_prod_ok (map eval_ltype (w2_ty sz sz)) (Obj.magic x86_SHRX sz));
    id_args_kinds = (check_sarx sz); id_nargs = (S (S (S O))); id_str_jas =
    (pp_sz "SHRX" sz); id_safe = []; id_pp_asm = (pp_name "shrx" sz) }),
    ("SHRX", (prim_32_64 (fun x -> SHRX x))))

(** val x86_SHLX :
    wsize -> GRing.ComRing.sort -> GRing.ComRing.sort -> GRing.ComRing.sort **)

let x86_SHLX sz =
  x86_bmi_shift sz (wshl sz)

(** val coq_Ox86_SHLX_instr :
    (wsize -> (register, register_ext, xmm_register, rflag, condt)
    instr_desc_t) * (string * x86_op prim_constructor) **)

let coq_Ox86_SHLX_instr =
  ((fun sz -> { id_valid = (size_32_64 sz); id_msb_flag = MSB_CLEAR; id_tin =
    (w2_ty sz sz); id_in =
    ((coq_Ea x86_decl (S O)) :: ((coq_Eu x86_decl (S (S O))) :: []));
    id_tout = (w_ty sz); id_out = ((coq_Ea x86_decl O) :: []); id_semi =
    (sem_prod_ok (map eval_ltype (w2_ty sz sz)) (Obj.magic x86_SHLX sz));
    id_args_kinds = (check_sarx sz); id_nargs = (S (S (S O))); id_str_jas =
    (pp_sz "SHLX" sz); id_safe = []; id_pp_asm = (pp_name "shlx" sz) }),
    ("SHLX", (prim_32_64 (fun x -> SHLX x))))

(** val x86_BSWAP : wsize -> GRing.ComRing.sort -> sem_tuple **)

let x86_BSWAP =
  wbswap

(** val coq_Ox86_BSWAP_instr :
    (wsize -> (register, register_ext, xmm_register, rflag, condt)
    instr_desc_t) * (string * x86_op prim_constructor) **)

let coq_Ox86_BSWAP_instr =
  ((fun sz -> { id_valid = (size_32_64 sz); id_msb_flag = (reg_msb_flag sz);
    id_tin = (w_ty sz); id_in = ((coq_Eu x86_decl O) :: []); id_tout =
    (w_ty sz); id_out = ((coq_Eu x86_decl O) :: []); id_semi =
    (sem_prod_ok (map eval_ltype (w_ty sz)) (Obj.magic x86_BSWAP sz));
    id_args_kinds = ((r :: []) :: []); id_nargs = (S O); id_str_jas =
    (pp_sz "BSWAP" sz); id_safe = []; id_pp_asm = (pp_iname "bswap" sz) }),
    ("BSWAP", (prim_32_64 (fun x -> BSWAP x))))

(** val x86_POPCNT : wsize -> GRing.ComRing.sort -> sem_tuple **)

let x86_POPCNT sz v =
  let r0 = popcnt sz v in
  Obj.magic ((Some false), ((Some false), ((Some false), ((Some false),
    ((Some (coq_ZF_of_word sz v)), r0)))))

(** val coq_Ox86_POPCNT_instr :
    (wsize -> (register, register_ext, xmm_register, rflag, condt)
    instr_desc_t) * (string * x86_op prim_constructor) **)

let coq_Ox86_POPCNT_instr =
  ((fun sz -> { id_valid = (size_16_64 sz); id_msb_flag = (reg_msb_flag sz);
    id_tin = (w_ty sz); id_in = ((coq_Eu x86_decl (S O)) :: []); id_tout =
    (b5w_ty sz); id_out = (cat implicit_flags ((coq_Eu x86_decl O) :: []));
    id_semi =
    (sem_prod_ok (map eval_ltype (w_ty sz)) (Obj.magic x86_POPCNT sz));
    id_args_kinds = (r_rm :: []); id_nargs = (S (S O)); id_str_jas =
    (pp_sz "POPCNT" sz); id_safe = []; id_pp_asm = (pp_name "popcnt" sz) }),
    ("POPCNT", (prim_16_64 (fun x -> POPCNT x))))

(** val x86_BTX :
    (wsize -> GRing.ComRing.sort -> GRing.ComRing.sort -> sem_ot) -> wsize ->
    GRing.ComRing.sort -> GRing.ComRing.sort -> sem_tuple **)

let x86_BTX op sz x y =
  let bit = Z.modulo (wunsigned sz y) (wsize_bits sz) in
  Obj.magic ((Some (wbit_n sz x (Z.to_nat bit))),
    (op sz (wrepr sz (Z.pow (Zpos (Coq_xO Coq_xH)) bit)) x))

(** val coq_Ox86_BTR_instr :
    (wsize -> (register, register_ext, xmm_register, rflag, condt)
    instr_desc_t) * (string * x86_op prim_constructor) **)

let coq_Ox86_BTR_instr =
  ((fun sz -> { id_valid = (size_16_64 sz); id_msb_flag = MSB_MERGE; id_tin =
    (w2_ty sz sz); id_in =
    ((coq_Ea x86_decl O) :: ((coq_Eu x86_decl (S O)) :: [])); id_tout =
    (bw_ty sz); id_out =
    ((coq_F x86_decl CF) :: ((coq_Ea x86_decl O) :: [])); id_semi =
    (sem_prod_ok (map eval_ltype (w2_ty sz sz)) (Obj.magic x86_BTX wandn sz));
    id_args_kinds = (check_bt sz); id_nargs = (S (S O)); id_str_jas =
    (pp_sz "BTR" sz); id_safe = []; id_pp_asm = (pp_iname "btr" sz) }),
    ("BTR", (prim_16_64 (fun x -> BTR x))))

(** val coq_Ox86_BTS_instr :
    (wsize -> (register, register_ext, xmm_register, rflag, condt)
    instr_desc_t) * (string * x86_op prim_constructor) **)

let coq_Ox86_BTS_instr =
  ((fun sz -> { id_valid = (size_16_64 sz); id_msb_flag = MSB_MERGE; id_tin =
    (w2_ty sz sz); id_in =
    ((coq_Ea x86_decl O) :: ((coq_Eu x86_decl (S O)) :: [])); id_tout =
    (bw_ty sz); id_out =
    ((coq_F x86_decl CF) :: ((coq_Ea x86_decl O) :: [])); id_semi =
    (sem_prod_ok (map eval_ltype (w2_ty sz sz))
      (Obj.magic x86_BTX Word0.wor sz)); id_args_kinds = (check_bt sz);
    id_nargs = (S (S O)); id_str_jas = (pp_sz "BTS" sz); id_safe = [];
    id_pp_asm = (pp_iname "bts" sz) }), ("BTS",
    (prim_16_64 (fun x -> BTS x))))

(** val x86_PEXT :
    wsize -> GRing.ComRing.sort -> GRing.ComRing.sort -> sem_tuple **)

let x86_PEXT =
  pextr

(** val coq_Ox86_PEXT_instr :
    (wsize -> (register, register_ext, xmm_register, rflag, condt)
    instr_desc_t) * (string * x86_op prim_constructor) **)

let coq_Ox86_PEXT_instr =
  ((fun sz -> { id_valid = (size_32_64 sz); id_msb_flag = MSB_CLEAR; id_tin =
    (w2_ty sz sz); id_in =
    ((coq_Ea x86_decl (S O)) :: ((coq_Eu x86_decl (S (S O))) :: []));
    id_tout = (w_ty sz); id_out = ((coq_Ea x86_decl O) :: []); id_semi =
    (sem_prod_ok (map eval_ltype (w2_ty sz sz)) (Obj.magic x86_PEXT sz));
    id_args_kinds = ((r :: (r :: ((rm true) :: []))) :: []); id_nargs = (S (S
    (S O))); id_str_jas = (pp_sz "PEXT" sz); id_safe = []; id_pp_asm =
    (pp_name "pext" sz) }), ("PEXT", (prim_32_64 (fun x -> PEXT x))))

(** val x86_PDEP :
    wsize -> GRing.ComRing.sort -> GRing.ComRing.sort -> sem_tuple **)

let x86_PDEP =
  pdep

(** val coq_Ox86_PDEP_instr :
    (wsize -> (register, register_ext, xmm_register, rflag, condt)
    instr_desc_t) * (string * x86_op prim_constructor) **)

let coq_Ox86_PDEP_instr =
  ((fun sz -> { id_valid = (size_32_64 sz); id_msb_flag = MSB_CLEAR; id_tin =
    (w2_ty sz sz); id_in =
    ((coq_Ea x86_decl (S O)) :: ((coq_Eu x86_decl (S (S O))) :: []));
    id_tout = (w_ty sz); id_out = ((coq_Ea x86_decl O) :: []); id_semi =
    (sem_prod_ok (map eval_ltype (w2_ty sz sz)) (Obj.magic x86_PDEP sz));
    id_args_kinds = ((r :: (r :: ((rm true) :: []))) :: []); id_nargs = (S (S
    (S O))); id_str_jas = (pp_sz "PDEP" sz); id_safe = []; id_pp_asm =
    (pp_name "pdep" sz) }), ("PDEP", (prim_32_64 (fun x -> PDEP x))))

(** val check_movd : wsize -> arg_kind list list list **)

let check_movd _ =
  (xmm :: ((rm true) :: [])) :: []

(** val x86_MOVD : wsize -> GRing.ComRing.sort -> sem_tuple **)

let x86_MOVD sz v =
  zero_extend U128 sz v

(** val coq_Ox86_MOVD_instr :
    (wsize -> (register, register_ext, xmm_register, rflag, condt)
    instr_desc_t) * (string * x86_op prim_constructor) **)

let coq_Ox86_MOVD_instr =
  ((fun sz -> { id_valid = (size_32_64 sz); id_msb_flag = MSB_MERGE; id_tin =
    (w_ty sz); id_in = ((coq_Eu x86_decl (S O)) :: []); id_tout = w128_ty;
    id_out = ((coq_Eu x86_decl O) :: []); id_semi =
    (sem_prod_ok (map eval_ltype (w_ty sz)) (Obj.magic x86_MOVD sz));
    id_args_kinds = (check_movd sz); id_nargs = (S (S O)); id_str_jas =
    (pp_sz "MOVD" sz); id_safe = []; id_pp_asm =
    (pp_movd "mov" (Obj.magic sz)) }), ("MOVD",
    (prim_32_64 (fun x -> MOVD x))))

(** val coq_Ox86_MOVV_instr :
    (wsize -> (register, register_ext, xmm_register, rflag, condt)
    instr_desc_t) * (string * x86_op prim_constructor) **)

let coq_Ox86_MOVV_instr =
  ((fun sz -> { id_valid = (size_32_64 sz); id_msb_flag = (reg_msb_flag sz);
    id_tin = (w_ty sz); id_in = ((coq_Eu x86_decl (S O)) :: []); id_tout =
    (w_ty sz); id_out = ((coq_Eu x86_decl O) :: []); id_semi =
    (sem_prod_ok (map eval_ltype (w_ty sz)) (Obj.magic x86_MOVX sz));
    id_args_kinds = (((rm false) :: (xmm :: [])) :: []); id_nargs = (S (S
    O)); id_str_jas = (pp_sz "MOVV" sz); id_safe = []; id_pp_asm =
    (pp_movd "mov" (Obj.magic sz)) }), ("MOVV",
    (prim_32_64 (fun x -> MOVV x))))

(** val coq_Ox86_VMOV_instr :
    (wsize -> (register, register_ext, xmm_register, rflag, condt)
    instr_desc_t) * (string * x86_op prim_constructor) **)

let coq_Ox86_VMOV_instr =
  ((fun sz -> { id_valid = (size_32_64 sz); id_msb_flag = MSB_CLEAR; id_tin =
    (w_ty sz); id_in = ((coq_Eu x86_decl (S O)) :: []); id_tout = w128_ty;
    id_out = ((coq_Eu x86_decl O) :: []); id_semi =
    (sem_prod_ok (map eval_ltype (w_ty sz)) (Obj.magic x86_MOVD sz));
    id_args_kinds = (check_movd sz); id_nargs = (S (S O)); id_str_jas =
    (pp_sz "VMOV" sz); id_safe = []; id_pp_asm =
    (pp_movd "vmov" (Obj.magic sz)) }), ("VMOV",
    (prim_32_64 (fun x -> VMOV x))))

(** val check_vmovdq : wsize -> arg_kind list list list **)

let check_vmovdq _ =
  xmm_xmmm :: (xmmm_xmm :: [])

(** val x86_VMOVDQ : wsize -> GRing.ComRing.sort -> sem_tuple **)

let x86_VMOVDQ _ v =
  v

(** val coq_Ox86_VMOVDQA_instr :
    (wsize -> (register, register_ext, xmm_register, rflag, condt)
    instr_desc_t) * (string * x86_op prim_constructor) **)

let coq_Ox86_VMOVDQA_instr =
  ((fun sz -> { id_valid = (size_128_256 sz); id_msb_flag =
    (reg_msb_flag sz); id_tin = (w_ty sz); id_in =
    ((coq_Ea x86_decl (S O)) :: []); id_tout = (w_ty sz); id_out =
    ((coq_Ea x86_decl O) :: []); id_semi =
    (sem_prod_ok (map eval_ltype (w_ty sz)) (Obj.magic x86_VMOVDQ sz));
    id_args_kinds = (check_vmovdq sz); id_nargs = (S (S O)); id_str_jas =
    (pp_sz "VMOVDQA" sz); id_safe = []; id_pp_asm =
    (pp_name "vmovdqa" sz) }), ("VMOVDQA",
    (prim_128_256 (fun x -> VMOVDQA x))))

(** val coq_Ox86_VMOVDQU_instr :
    (wsize -> (register, register_ext, xmm_register, rflag, condt)
    instr_desc_t) * (string * x86_op prim_constructor) **)

let coq_Ox86_VMOVDQU_instr =
  ((fun sz -> { id_valid = (size_128_256 sz); id_msb_flag =
    (reg_msb_flag sz); id_tin = (w_ty sz); id_in =
    ((coq_Eu x86_decl (S O)) :: []); id_tout = (w_ty sz); id_out =
    ((coq_Eu x86_decl O) :: []); id_semi =
    (sem_prod_ok (map eval_ltype (w_ty sz)) (Obj.magic x86_VMOVDQ sz));
    id_args_kinds = (check_vmovdq sz); id_nargs = (S (S O)); id_str_jas =
    (pp_sz "VMOVDQU" sz); id_safe = []; id_pp_asm =
    (pp_name "vmovdqu" sz) }), ("VMOVDQU",
    (prim_128_256 (fun x -> VMOVDQU x))))

(** val pp_vpmovx :
    string -> velem -> wsize -> velem -> wsize -> (register, register_ext,
    xmm_register, rflag, condt) asm_arg list -> (register, register_ext,
    xmm_register, rflag, condt) pp_asm_op **)

let pp_vpmovx name ve sz ve' sz' args =
  { pp_aop_name = name; pp_aop_ext = (PP_viname2 (ve, ve')); pp_aop_args =
    (zip (sz' :: (sz :: [])) args) }

(** val vector_size : velem -> wsize -> coq_Z option **)

let vector_size ve ws =
  let (q, r0) = Z.div_eucl (wsize_size ws) (wsize_size (wsize_of_velem ve)) in
  if eq_op coq_BinNums_Z__canonical__eqtype_Equality (Obj.magic r0)
       (Obj.magic Z0)
  then Some q
  else None

(** val check_vector_length : velem -> wsize -> velem -> wsize -> bool **)

let check_vector_length ve sz ve' sz' =
  match vector_size ve sz with
  | Some i0 ->
    (match vector_size ve' sz' with
     | Some j ->
       eq_op coq_BinNums_Z__canonical__eqtype_Equality (Obj.magic i0)
         (Obj.magic j)
     | None -> false)
  | None -> false

(** val x86_VPMOVSX :
    velem -> wsize -> velem -> wsize -> GRing.ComRing.sort ->
    GRing.ComRing.sort **)

let x86_VPMOVSX ve sz ve' sz' w =
  lift1_vec' (wsize_of_velem ve) (wsize_of_velem ve')
    (sign_extend (wsize_of_velem ve') (wsize_of_velem ve)) sz sz' w

(** val coq_Ox86_VPMOVSX_instr :
    (velem -> wsize -> velem -> wsize -> (register, register_ext,
    xmm_register, rflag, condt) instr_desc_t) * (string * x86_op
    prim_constructor) **)

let coq_Ox86_VPMOVSX_instr =
  let name = "VPMOVSX" in
  ((fun ve sz ve' sz' -> { id_valid =
  ((&&) (size_128_256 sz') (check_vector_length ve sz ve' sz'));
  id_msb_flag = MSB_CLEAR; id_tin = ((Coq_lword sz) :: []); id_in =
  ((coq_Eu x86_decl (S O)) :: []); id_tout = ((Coq_lword sz') :: []);
  id_out = ((coq_Eu x86_decl O) :: []); id_semi =
  (sem_prod_ok (map eval_ltype ((Coq_lword sz) :: []))
    (Obj.magic x86_VPMOVSX ve sz ve' sz')); id_args_kinds =
  ((xmm :: ((xmmm true) :: [])) :: []); id_nargs = (S (S O)); id_str_jas =
  (pp_ve_sz_ve_sz name ve sz ve' sz'); id_safe = []; id_pp_asm =
  (pp_vpmovx "vpmovsx" ve sz ve' sz') }), (name,
  (prim_vv (fun x x0 x1 x2 -> VPMOVSX (x, x0, x1, x2)))))

(** val x86_VPMOVZX :
    velem -> wsize -> velem -> wsize -> GRing.ComRing.sort ->
    GRing.ComRing.sort **)

let x86_VPMOVZX ve sz ve' sz' w =
  lift1_vec' (wsize_of_velem ve) (wsize_of_velem ve')
    (zero_extend (wsize_of_velem ve') (wsize_of_velem ve)) sz sz' w

(** val coq_Ox86_VPMOVZX_instr :
    (velem -> wsize -> velem -> wsize -> (register, register_ext,
    xmm_register, rflag, condt) instr_desc_t) * (string * x86_op
    prim_constructor) **)

let coq_Ox86_VPMOVZX_instr =
  let name = "VPMOVZX" in
  ((fun ve sz ve' sz' -> { id_valid =
  ((&&) (size_128_256 sz') (check_vector_length ve sz ve' sz'));
  id_msb_flag = MSB_CLEAR; id_tin = ((Coq_lword sz) :: []); id_in =
  ((coq_Eu x86_decl (S O)) :: []); id_tout = ((Coq_lword sz') :: []);
  id_out = ((coq_Eu x86_decl O) :: []); id_semi =
  (sem_prod_ok (map eval_ltype ((Coq_lword sz) :: []))
    (Obj.magic x86_VPMOVZX ve sz ve' sz')); id_args_kinds =
  ((xmm :: ((xmmm true) :: [])) :: []); id_nargs = (S (S O)); id_str_jas =
  (pp_ve_sz_ve_sz name ve sz ve' sz'); id_safe = []; id_pp_asm =
  (pp_vpmovx "vpmovzx" ve sz ve' sz') }), (name,
  (prim_vv (fun x x0 x1 x2 -> VPMOVZX (x, x0, x1, x2)))))

(** val check_xmm_xmm_xmmm : wsize -> arg_kind list list list **)

let check_xmm_xmm_xmmm _ =
  xmm_xmm_xmmm :: []

(** val x86_VPAND :
    wsize -> GRing.ComRing.sort -> GRing.ComRing.sort -> GRing.ComRing.sort **)

let x86_VPAND =
  Word0.wand

(** val coq_Ox86_VPAND_instr :
    (wsize -> (register, register_ext, xmm_register, rflag, condt)
    instr_desc_t) * (string * x86_op prim_constructor) **)

let coq_Ox86_VPAND_instr =
  ((fun sz -> { id_valid = (size_128_256 sz); id_msb_flag = MSB_CLEAR;
    id_tin = (w2_ty sz sz); id_in =
    ((coq_Ea x86_decl (S O)) :: ((coq_Eu x86_decl (S (S O))) :: []));
    id_tout = (w_ty sz); id_out = ((coq_Ea x86_decl O) :: []); id_semi =
    (sem_prod_ok (map eval_ltype (w2_ty sz sz)) (Obj.magic x86_VPAND sz));
    id_args_kinds = (check_xmm_xmm_xmmm sz); id_nargs = (S (S (S O)));
    id_str_jas = (pp_sz "VPAND" sz); id_safe = []; id_pp_asm =
    (pp_name "vpand" sz) }), ("VPAND", (prim_128_256 (fun x -> VPAND x))))

(** val x86_VPANDN :
    wsize -> GRing.ComRing.sort -> GRing.ComRing.sort -> GRing.ComRing.sort **)

let x86_VPANDN =
  wandn

(** val coq_Ox86_VPANDN_instr :
    (wsize -> (register, register_ext, xmm_register, rflag, condt)
    instr_desc_t) * (string * x86_op prim_constructor) **)

let coq_Ox86_VPANDN_instr =
  ((fun sz -> { id_valid = (size_128_256 sz); id_msb_flag = MSB_CLEAR;
    id_tin = (w2_ty sz sz); id_in =
    ((coq_Ea x86_decl (S O)) :: ((coq_Eu x86_decl (S (S O))) :: []));
    id_tout = (w_ty sz); id_out = ((coq_Ea x86_decl O) :: []); id_semi =
    (sem_prod_ok (map eval_ltype (w2_ty sz sz)) (Obj.magic x86_VPANDN sz));
    id_args_kinds = (check_xmm_xmm_xmmm sz); id_nargs = (S (S (S O)));
    id_str_jas = (pp_sz "VPANDN" sz); id_safe = []; id_pp_asm =
    (pp_name "vpandn" sz) }), ("VPANDN", (prim_128_256 (fun x -> VPANDN x))))

(** val x86_VPOR :
    wsize -> GRing.ComRing.sort -> GRing.ComRing.sort -> GRing.ComRing.sort **)

let x86_VPOR =
  Word0.wor

(** val coq_Ox86_VPOR_instr :
    (wsize -> (register, register_ext, xmm_register, rflag, condt)
    instr_desc_t) * (string * x86_op prim_constructor) **)

let coq_Ox86_VPOR_instr =
  ((fun sz -> { id_valid = (size_128_256 sz); id_msb_flag = MSB_CLEAR;
    id_tin = (w2_ty sz sz); id_in =
    ((coq_Ea x86_decl (S O)) :: ((coq_Eu x86_decl (S (S O))) :: []));
    id_tout = (w_ty sz); id_out = ((coq_Ea x86_decl O) :: []); id_semi =
    (sem_prod_ok (map eval_ltype (w2_ty sz sz)) (Obj.magic x86_VPOR sz));
    id_args_kinds = (check_xmm_xmm_xmmm sz); id_nargs = (S (S (S O)));
    id_str_jas = (pp_sz "VPOR" sz); id_safe = []; id_pp_asm =
    (pp_name "vpor" sz) }), ("VPOR", (prim_128_256 (fun x -> VPOR x))))

(** val x86_VPXOR :
    wsize -> GRing.ComRing.sort -> GRing.ComRing.sort -> GRing.ComRing.sort **)

let x86_VPXOR =
  Word0.wxor

(** val coq_Ox86_VPXOR_instr :
    (wsize -> (register, register_ext, xmm_register, rflag, condt)
    instr_desc_t) * (string * x86_op prim_constructor) **)

let coq_Ox86_VPXOR_instr =
  ((fun sz -> { id_valid = (size_128_256 sz); id_msb_flag = MSB_CLEAR;
    id_tin = (w2_ty sz sz); id_in =
    ((coq_Ea x86_decl (S O)) :: ((coq_Eu x86_decl (S (S O))) :: []));
    id_tout = (w_ty sz); id_out = ((coq_Ea x86_decl O) :: []); id_semi =
    (sem_prod_ok (map eval_ltype (w2_ty sz sz)) (Obj.magic x86_VPXOR sz));
    id_args_kinds = (check_xmm_xmm_xmmm sz); id_nargs = (S (S (S O)));
    id_str_jas = (pp_sz "VPXOR" sz); id_safe = []; id_pp_asm =
    (pp_name "vpxor" sz) }), ("VPXOR", (prim_128_256 (fun x -> VPXOR x))))

(** val x86_VPADD :
    velem -> wsize -> GRing.ComRing.sort -> GRing.ComRing.sort ->
    GRing.ComRing.sort **)

let x86_VPADD ve sz =
  lift2_vec (wsize_of_velem ve)
    (GRing.add
      (GRing.ComRing.Exports.coq_GRing_ComRing__to__GRing_Nmodule
        (word (wsize_of_velem ve)))) sz

(** val coq_Ox86_VPADD_instr :
    (velem -> wsize -> (register, register_ext, xmm_register, rflag, condt)
    instr_desc_t) * (string * x86_op prim_constructor) **)

let coq_Ox86_VPADD_instr =
  ((fun ve sz -> { id_valid = (size_128_256 sz); id_msb_flag = MSB_CLEAR;
    id_tin = (w2_ty sz sz); id_in =
    ((coq_Ea x86_decl (S O)) :: ((coq_Eu x86_decl (S (S O))) :: []));
    id_tout = (w_ty sz); id_out = ((coq_Ea x86_decl O) :: []); id_semi =
    (sem_prod_ok (map eval_ltype (w2_ty sz sz)) (Obj.magic x86_VPADD ve sz));
    id_args_kinds = (check_xmm_xmm_xmmm sz); id_nargs = (S (S (S O)));
    id_str_jas = (pp_ve_sz "VPADD" ve sz); id_safe = []; id_pp_asm =
    (pp_viname "vpadd" ve sz) }), ("VPADD",
    (primV (fun x x0 -> VPADD (x, x0)))))

(** val x86_VPSUB :
    velem -> wsize -> GRing.ComRing.sort -> GRing.ComRing.sort ->
    GRing.ComRing.sort **)

let x86_VPSUB ve sz =
  lift2_vec (wsize_of_velem ve) (fun x y ->
    GRing.add
      (GRing.ComRing.Exports.coq_GRing_ComRing__to__GRing_Nmodule
        (word (wsize_of_velem ve))) x
      (GRing.opp
        (GRing.ComRing.Exports.coq_GRing_ComRing__to__GRing_Zmodule
          (word (wsize_of_velem ve))) y)) sz

(** val coq_Ox86_VPSUB_instr :
    (velem -> wsize -> (register, register_ext, xmm_register, rflag, condt)
    instr_desc_t) * (string * x86_op prim_constructor) **)

let coq_Ox86_VPSUB_instr =
  ((fun ve sz -> { id_valid = (size_128_256 sz); id_msb_flag = MSB_CLEAR;
    id_tin = (w2_ty sz sz); id_in =
    ((coq_Ea x86_decl (S O)) :: ((coq_Eu x86_decl (S (S O))) :: []));
    id_tout = (w_ty sz); id_out = ((coq_Ea x86_decl O) :: []); id_semi =
    (sem_prod_ok (map eval_ltype (w2_ty sz sz)) (Obj.magic x86_VPSUB ve sz));
    id_args_kinds = (check_xmm_xmm_xmmm sz); id_nargs = (S (S (S O)));
    id_str_jas = (pp_ve_sz "VPSUB" ve sz); id_safe = []; id_pp_asm =
    (pp_viname "vpsub" ve sz) }), ("VPSUB",
    (primV (fun x x0 -> VPSUB (x, x0)))))

(** val x86_VPAVG :
    velem -> wsize -> GRing.ComRing.sort -> GRing.ComRing.sort ->
    GRing.ComRing.sort **)

let x86_VPAVG ve sz v1 v2 =
  let avg = fun x y ->
    wrepr (wsize_of_velem ve)
      (Z.div
        (Z.add
          (Z.add (wunsigned (wsize_of_velem ve) x)
            (wunsigned (wsize_of_velem ve) y)) (Zpos Coq_xH)) (Zpos (Coq_xO
        Coq_xH)))
  in
  lift2_vec (wsize_of_velem ve) avg sz v1 v2

(** val coq_Ox86_VPAVG_instr :
    (velem -> wsize -> (register, register_ext, xmm_register, rflag, condt)
    instr_desc_t) * (string * x86_op prim_constructor) **)

let coq_Ox86_VPAVG_instr =
  ((fun ve sz -> { id_valid =
    ((&&) (size_8_16 (wsize_of_velem ve)) (size_128_256 sz)); id_msb_flag =
    MSB_CLEAR; id_tin = (w2_ty sz sz); id_in =
    ((coq_Ea x86_decl (S O)) :: ((coq_Eu x86_decl (S (S O))) :: []));
    id_tout = (w_ty sz); id_out = ((coq_Ea x86_decl O) :: []); id_semi =
    (sem_prod_ok (map eval_ltype (w2_ty sz sz)) (Obj.magic x86_VPAVG ve sz));
    id_args_kinds = (check_xmm_xmm_xmmm sz); id_nargs = (S (S (S O)));
    id_str_jas = (pp_ve_sz "VPAVG" ve sz); id_safe = []; id_pp_asm =
    (pp_viname "vpavg" ve sz) }), ("VPAVG",
    (primV_8_16 (fun x x0 -> VPAVG (x, x0)))))

(** val x86_VPMULL :
    velem -> wsize -> GRing.ComRing.sort -> GRing.ComRing.sort ->
    GRing.ComRing.sort **)

let x86_VPMULL ve sz v1 v2 =
  lift2_vec (wsize_of_velem ve)
    (GRing.mul
      (GRing.ComRing.Exports.coq_GRing_ComRing__to__GRing_SemiRing
        (word (wsize_of_velem ve)))) sz v1 v2

(** val coq_Ox86_VPMULL_instr :
    (velem -> wsize -> (register, register_ext, xmm_register, rflag, condt)
    instr_desc_t) * (string * x86_op prim_constructor) **)

let coq_Ox86_VPMULL_instr =
  ((fun ve sz -> { id_valid =
    ((&&) (size_16_32 (wsize_of_velem ve)) (size_128_256 sz)); id_msb_flag =
    MSB_CLEAR; id_tin = (w2_ty sz sz); id_in =
    ((coq_Ea x86_decl (S O)) :: ((coq_Eu x86_decl (S (S O))) :: []));
    id_tout = (w_ty sz); id_out = ((coq_Ea x86_decl O) :: []); id_semi =
    (sem_prod_ok (map eval_ltype (w2_ty sz sz)) (Obj.magic x86_VPMULL ve sz));
    id_args_kinds = (check_xmm_xmm_xmmm sz); id_nargs = (S (S (S O)));
    id_str_jas = (pp_ve_sz "VPMULL" ve sz); id_safe = []; id_pp_asm =
    (pp_viname "vpmull" ve sz) }), ("VPMULL",
    (primV_16_32 (fun x x0 -> VPMULL (x, x0)))))

(** val x86_VPMUL :
    wsize -> GRing.ComRing.sort -> GRing.ComRing.sort -> GRing.ComRing.sort **)

let x86_VPMUL =
  wpmul

(** val coq_Ox86_VPMUL_instr :
    (wsize -> (register, register_ext, xmm_register, rflag, condt)
    instr_desc_t) * (string * x86_op prim_constructor) **)

let coq_Ox86_VPMUL_instr =
  ((fun sz -> { id_valid = (size_128_256 sz); id_msb_flag = MSB_CLEAR;
    id_tin = (w2_ty sz sz); id_in =
    ((coq_Eu x86_decl (S O)) :: ((coq_Eu x86_decl (S (S O))) :: []));
    id_tout = (w_ty sz); id_out = ((coq_Eu x86_decl O) :: []); id_semi =
    (sem_prod_ok (map eval_ltype (w2_ty sz sz)) (Obj.magic x86_VPMUL sz));
    id_args_kinds = (check_xmm_xmm_xmmm sz); id_nargs = (S (S (S O)));
    id_str_jas = (pp_sz "VPMUL" sz); id_safe = []; id_pp_asm =
    (pp_name "vpmuldq" sz) }), ("VPMUL", (prim_128_256 (fun x -> VPMUL x))))

(** val x86_VPMULU :
    wsize -> GRing.ComRing.sort -> GRing.ComRing.sort -> GRing.ComRing.sort **)

let x86_VPMULU =
  wpmulu

(** val coq_Ox86_VPMULU_instr :
    (wsize -> (register, register_ext, xmm_register, rflag, condt)
    instr_desc_t) * (string * x86_op prim_constructor) **)

let coq_Ox86_VPMULU_instr =
  ((fun sz -> { id_valid = (size_128_256 sz); id_msb_flag = MSB_CLEAR;
    id_tin = (w2_ty sz sz); id_in =
    ((coq_Eu x86_decl (S O)) :: ((coq_Eu x86_decl (S (S O))) :: []));
    id_tout = (w_ty sz); id_out = ((coq_Eu x86_decl O) :: []); id_semi =
    (sem_prod_ok (map eval_ltype (w2_ty sz sz)) (Obj.magic x86_VPMULU sz));
    id_args_kinds = (check_xmm_xmm_xmmm sz); id_nargs = (S (S (S O)));
    id_str_jas = (pp_sz "VPMULU" sz); id_safe = []; id_pp_asm =
    (pp_name "vpmuludq" sz) }), ("VPMULU",
    (prim_128_256 (fun x -> VPMULU x))))

(** val x86_VPMULH :
    wsize -> GRing.ComRing.sort -> GRing.ComRing.sort -> GRing.ComRing.sort **)

let x86_VPMULH sz v1 v2 =
  lift2_vec U16 (wmulhs U16) sz v1 v2

(** val coq_Ox86_VPMULH_instr :
    (wsize -> (register, register_ext, xmm_register, rflag, condt)
    instr_desc_t) * (string * x86_op prim_constructor) **)

let coq_Ox86_VPMULH_instr =
  ((fun sz -> { id_valid = (size_128_256 sz); id_msb_flag =
    (reg_msb_flag sz); id_tin = (w2_ty sz sz); id_in =
    ((coq_Eu x86_decl (S O)) :: ((coq_Eu x86_decl (S (S O))) :: []));
    id_tout = (w_ty sz); id_out = ((coq_Eu x86_decl O) :: []); id_semi =
    (sem_prod_ok (map eval_ltype (w2_ty sz sz)) (Obj.magic x86_VPMULH sz));
    id_args_kinds = (check_xmm_xmm_xmmm sz); id_nargs = (S (S (S O)));
    id_str_jas = (pp_ve_sz "VPMULH" VE16 sz); id_safe = []; id_pp_asm =
    (pp_viname "vpmulh" VE16 sz) }), ("VPMULH",
    (primV_16 (fun _ x -> VPMULH x))))

(** val x86_VPMULHU :
    wsize -> GRing.ComRing.sort -> GRing.ComRing.sort -> GRing.ComRing.sort **)

let x86_VPMULHU sz v1 v2 =
  lift2_vec U16 (wmulhu U16) sz v1 v2

(** val coq_Ox86_VPMULHU_instr :
    (wsize -> (register, register_ext, xmm_register, rflag, condt)
    instr_desc_t) * (string * x86_op prim_constructor) **)

let coq_Ox86_VPMULHU_instr =
  ((fun sz -> { id_valid = (size_128_256 sz); id_msb_flag =
    (reg_msb_flag sz); id_tin = (w2_ty sz sz); id_in =
    ((coq_Eu x86_decl (S O)) :: ((coq_Eu x86_decl (S (S O))) :: []));
    id_tout = (w_ty sz); id_out = ((coq_Eu x86_decl O) :: []); id_semi =
    (sem_prod_ok (map eval_ltype (w2_ty sz sz)) (Obj.magic x86_VPMULHU sz));
    id_args_kinds = (check_xmm_xmm_xmmm sz); id_nargs = (S (S (S O)));
    id_str_jas = (pp_ve_sz "VPMULHU" VE16 sz); id_safe = []; id_pp_asm =
    (pp_viname "vpmulhu" VE16 sz) }), ("VPMULHU",
    (primV_16 (fun _ x -> VPMULHU x))))

(** val x86_VPMULHRS :
    wsize -> GRing.ComRing.sort -> GRing.ComRing.sort -> GRing.ComRing.sort **)

let x86_VPMULHRS sz v1 v2 =
  lift2_vec U16 (wmulhrs U16) sz v1 v2

(** val coq_Ox86_VPMULHRS_instr :
    (wsize -> (register, register_ext, xmm_register, rflag, condt)
    instr_desc_t) * (string * x86_op prim_constructor) **)

let coq_Ox86_VPMULHRS_instr =
  ((fun sz -> { id_valid = (size_128_256 sz); id_msb_flag =
    (reg_msb_flag sz); id_tin = (w2_ty sz sz); id_in =
    ((coq_Eu x86_decl (S O)) :: ((coq_Eu x86_decl (S (S O))) :: []));
    id_tout = (w_ty sz); id_out = ((coq_Eu x86_decl O) :: []); id_semi =
    (sem_prod_ok (map eval_ltype (w2_ty sz sz)) (Obj.magic x86_VPMULHRS sz));
    id_args_kinds = (check_xmm_xmm_xmmm sz); id_nargs = (S (S (S O)));
    id_str_jas = (pp_ve_sz "VPMULHRS" VE16 sz); id_safe = []; id_pp_asm =
    (pp_viname "vpmulhrs" VE16 sz) }), ("VPMULHRS",
    (primV_16 (fun _ x -> VPMULHRS x))))

(** val check_vpextr : wsize -> arg_kind list list list **)

let check_vpextr _ =
  ((rm false) :: (xmm :: ((i U8) :: []))) :: []

(** val pp_viname_t :
    string -> velem -> wsize list -> (register, register_ext, xmm_register,
    rflag, condt) asm_arg list -> (register, register_ext, xmm_register,
    rflag, condt) pp_asm_op **)

let pp_viname_t name ve ts args =
  { pp_aop_name = name; pp_aop_ext = (PP_viname (ve, false)); pp_aop_args =
    (zip ts args) }

(** val x86_nelem_mask : wsize -> wsize -> GRing.ComRing.sort **)

let x86_nelem_mask sze szc =
  wrepr U8
    (Z.sub (two_power_nat (subn (wsize_log2 szc) (wsize_log2 sze))) (Zpos
      Coq_xH))

(** val x86_VPEXTR :
    wsize -> GRing.ComRing.sort -> GRing.ComRing.sort -> sem_tuple **)

let x86_VPEXTR ve v i0 =
  let i1 = Word0.wand U8 i0 (x86_nelem_mask ve U128) in
  nth
    (GRing.zero
      (GRing.ComRing.Exports.coq_GRing_ComRing__to__GRing_Nmodule (word ve)))
    (Obj.magic split_vec U128 (nat_of_wsize ve) v)
    (Z.to_nat (wunsigned U8 i1))

(** val coq_Ox86_VPEXTR_instr :
    (wsize -> (register, register_ext, xmm_register, rflag, condt)
    instr_desc_t) * (string * x86_op prim_constructor) **)

let coq_Ox86_VPEXTR_instr =
  ((fun sz ->
    let ve = match sz with
             | U8 -> VE8
             | U16 -> VE16
             | U32 -> VE32
             | _ -> VE64 in
    { id_valid = (size_8_64 sz); id_msb_flag = MSB_CLEAR; id_tin = w128w8_ty;
    id_in = ((coq_Eu x86_decl (S O)) :: ((coq_Eu x86_decl (S (S O))) :: []));
    id_tout = (w_ty sz); id_out = ((coq_Eu x86_decl O) :: []); id_semi =
    (sem_prod_ok (map eval_ltype w128w8_ty) (Obj.magic x86_VPEXTR sz));
    id_args_kinds = (check_vpextr sz); id_nargs = (S (S (S O))); id_str_jas =
    (pp_sz "VPEXTR" sz); id_safe = []; id_pp_asm =
    (pp_viname_t "vpextr" ve
      ((if eq_op wsize_wsize__canonical__eqtype_Equality (Obj.magic sz)
             (Obj.magic U32)
        then U32
        else U64) :: (U128 :: (U8 :: [])))) }), ("VPEXTR",
    (prim_8_64 (fun x -> VPEXTR x))))

(** val pp_vpinsr :
    velem -> (register, register_ext, xmm_register, rflag, condt) asm_arg
    list -> (register, register_ext, xmm_register, rflag, condt) pp_asm_op **)

let pp_vpinsr ve args =
  let rs = match ve with
           | VE64 -> U64
           | _ -> U32 in
  { pp_aop_name = "vpinsr"; pp_aop_ext = (PP_viname (ve, false));
  pp_aop_args = (zip (U128 :: (U128 :: (rs :: (U8 :: [])))) args) }

(** val check_vpinsr : wsize -> arg_kind list list list **)

let check_vpinsr _ =
  (xmm :: (xmm :: ((rm true) :: ((i U8) :: [])))) :: []

(** val x86_VPINSR :
    velem -> GRing.ComRing.sort -> GRing.ComRing.sort -> GRing.ComRing.sort
    -> sem_tuple **)

let x86_VPINSR ve v1 v2 i0 =
  let i1 = Word0.wand U8 i0 (x86_nelem_mask (wsize_of_velem ve) U128) in
  wpinsr (wsize_of_velem ve) v1 v2 i1

(** val coq_Ox86_VPINSR_instr :
    (velem -> (register, register_ext, xmm_register, rflag, condt)
    instr_desc_t) * (string * x86_op prim_constructor) **)

let coq_Ox86_VPINSR_instr =
  ((fun ve -> { id_valid = true; id_msb_flag = MSB_CLEAR; id_tin =
    (w128ww8_ty (wsize_of_velem ve)); id_in =
    ((coq_Eu x86_decl (S O)) :: ((coq_Eu x86_decl (S (S O))) :: ((coq_Eu
                                                                   x86_decl
                                                                   (S (S (S
                                                                   O)))) :: [])));
    id_tout = w128_ty; id_out = ((coq_Eu x86_decl O) :: []); id_semi =
    (sem_prod_ok (map eval_ltype (w128ww8_ty (wsize_of_velem ve)))
      (Obj.magic x86_VPINSR ve)); id_args_kinds =
    (check_vpinsr (wsize_of_velem ve)); id_nargs = (S (S (S (S O))));
    id_str_jas = (pp_ve_sz "VPINSR" ve U128); id_safe = []; id_pp_asm =
    (pp_vpinsr ve) }), ("VPINSR", (primV_128 (fun ve _ -> VPINSR ve))))

(** val check_xmm_xmm_imm8 : wsize -> arg_kind list list list **)

let check_xmm_xmm_imm8 _ =
  (xmm :: (xmm :: ((i U8) :: []))) :: []

(** val x86_u128_shift :
    wsize -> wsize -> (GRing.ComRing.sort -> coq_Z -> GRing.ComRing.sort) ->
    GRing.ComRing.sort -> GRing.ComRing.sort -> sem_tuple **)

let x86_u128_shift sz' sz op v c0 =
  lift1_vec sz' (fun v0 -> op v0 (wunsigned U128 c0)) sz v

(** val check_xmm_xmm_xmmmi : wsize -> arg_kind list list list **)

let check_xmm_xmm_xmmmi _ =
  (xmm_xmm_xmmmi U8) :: []

(** val x86_VPSLL :
    velem -> wsize -> GRing.ComRing.sort -> GRing.ComRing.sort -> sem_tuple **)

let x86_VPSLL ve sz =
  x86_u128_shift (wsize_of_velem ve) sz (wshl (wsize_of_velem ve))

(** val coq_Ox86_VPSLL_instr :
    (velem -> wsize -> (register, register_ext, xmm_register, rflag, condt)
    instr_desc_t) * (string * x86_op prim_constructor) **)

let coq_Ox86_VPSLL_instr =
  ((fun ve sz -> { id_valid =
    ((&&) (size_16_64 (wsize_of_velem ve)) (size_128_256 sz)); id_msb_flag =
    (reg_msb_flag sz); id_tin = (w2_ty sz U128); id_in =
    ((coq_Ea x86_decl (S O)) :: ((coq_Eu x86_decl (S (S O))) :: []));
    id_tout = (w_ty sz); id_out = ((coq_Ea x86_decl O) :: []); id_semi =
    (sem_prod_ok (map eval_ltype (w2_ty sz U128)) (Obj.magic x86_VPSLL ve sz));
    id_args_kinds = (check_xmm_xmm_xmmmi sz); id_nargs = (S (S (S O)));
    id_str_jas = (pp_ve_sz "VPSLL" ve sz); id_safe = []; id_pp_asm =
    (pp_viname_ww_128 "vpsll" ve sz) }), ("VPSLL",
    (primV_16_64 (fun x x0 -> VPSLL (x, x0)))))

(** val x86_VPSRL :
    velem -> wsize -> GRing.ComRing.sort -> GRing.ComRing.sort -> sem_tuple **)

let x86_VPSRL ve sz =
  x86_u128_shift (wsize_of_velem ve) sz (wshr (wsize_of_velem ve))

(** val coq_Ox86_VPSRL_instr :
    (velem -> wsize -> (register, register_ext, xmm_register, rflag, condt)
    instr_desc_t) * (string * x86_op prim_constructor) **)

let coq_Ox86_VPSRL_instr =
  ((fun ve sz -> { id_valid =
    ((&&) (size_16_64 (wsize_of_velem ve)) (size_128_256 sz)); id_msb_flag =
    (reg_msb_flag sz); id_tin = (w2_ty sz U128); id_in =
    ((coq_Ea x86_decl (S O)) :: ((coq_Eu x86_decl (S (S O))) :: []));
    id_tout = (w_ty sz); id_out = ((coq_Ea x86_decl O) :: []); id_semi =
    (sem_prod_ok (map eval_ltype (w2_ty sz U128)) (Obj.magic x86_VPSRL ve sz));
    id_args_kinds = (check_xmm_xmm_xmmmi sz); id_nargs = (S (S (S O)));
    id_str_jas = (pp_ve_sz "VPSRL" ve sz); id_safe = []; id_pp_asm =
    (pp_viname_ww_128 "vpsrl" ve sz) }), ("VPSRL",
    (primV_16_64 (fun x x0 -> VPSRL (x, x0)))))

(** val x86_VPSRA :
    velem -> wsize -> GRing.ComRing.sort -> GRing.ComRing.sort -> sem_tuple **)

let x86_VPSRA ve sz =
  x86_u128_shift (wsize_of_velem ve) sz (wsar (wsize_of_velem ve))

(** val coq_Ox86_VPSRA_instr :
    (velem -> wsize -> (register, register_ext, xmm_register, rflag, condt)
    instr_desc_t) * (string * x86_op prim_constructor) **)

let coq_Ox86_VPSRA_instr =
  ((fun ve sz -> { id_valid =
    ((&&) (size_16_32 (wsize_of_velem ve)) (size_128_256 sz)); id_msb_flag =
    (reg_msb_flag sz); id_tin = (w2_ty sz U128); id_in =
    ((coq_Ea x86_decl (S O)) :: ((coq_Eu x86_decl (S (S O))) :: []));
    id_tout = (w_ty sz); id_out = ((coq_Ea x86_decl O) :: []); id_semi =
    (sem_prod_ok (map eval_ltype (w2_ty sz U128)) (Obj.magic x86_VPSRA ve sz));
    id_args_kinds = (check_xmm_xmm_xmmmi sz); id_nargs = (S (S (S O)));
    id_str_jas = (pp_ve_sz "VPSRA" ve sz); id_safe = []; id_pp_asm =
    (pp_viname_ww_128 "vpsra" ve sz) }), ("VPSRA",
    (primV_16_32 (fun x x0 -> VPSRA (x, x0)))))

(** val x86_u128_shift_variable :
    wsize -> wsize -> (GRing.ComRing.sort -> coq_Z -> GRing.ComRing.sort) ->
    GRing.ComRing.sort -> GRing.ComRing.sort -> sem_tuple **)

let x86_u128_shift_variable ve sz op v1 v2 =
  lift2_vec ve (fun v3 v4 -> op v3 (Z.min (wunsigned ve v4) (wsize_bits ve)))
    sz v1 v2

(** val x86_VPSLLV :
    wsize -> wsize -> GRing.ComRing.sort -> GRing.ComRing.sort -> sem_tuple **)

let x86_VPSLLV ve sz =
  x86_u128_shift_variable ve sz (wshl ve)

(** val coq_Ox86_VPSLLV_instr :
    (velem -> wsize -> (register, register_ext, xmm_register, rflag, condt)
    instr_desc_t) * (string * x86_op prim_constructor) **)

let coq_Ox86_VPSLLV_instr =
  ((fun ve sz -> { id_valid =
    ((&&) (size_16_64 (wsize_of_velem ve)) (size_128_256 sz)); id_msb_flag =
    MSB_CLEAR; id_tin = (w2_ty sz sz); id_in =
    ((coq_Ea x86_decl (S O)) :: ((coq_Eu x86_decl (S (S O))) :: []));
    id_tout = (w_ty sz); id_out = ((coq_Ea x86_decl O) :: []); id_semi =
    (sem_prod_ok (map eval_ltype (w2_ty sz sz))
      (Obj.magic x86_VPSLLV (wsize_of_velem ve) sz)); id_args_kinds =
    (check_xmm_xmm_xmmm sz); id_nargs = (S (S (S O))); id_str_jas =
    (pp_ve_sz "VPSLLV" ve sz); id_safe = []; id_pp_asm =
    (pp_viname "vpsllv" ve sz) }), ("VPSLLV",
    (primV_16_64 (fun x x0 -> VPSLLV (x, x0)))))

(** val x86_VPSRLV :
    wsize -> wsize -> GRing.ComRing.sort -> GRing.ComRing.sort -> sem_tuple **)

let x86_VPSRLV ve sz =
  x86_u128_shift_variable ve sz (wshr ve)

(** val coq_Ox86_VPSRLV_instr :
    (velem -> wsize -> (register, register_ext, xmm_register, rflag, condt)
    instr_desc_t) * (string * x86_op prim_constructor) **)

let coq_Ox86_VPSRLV_instr =
  ((fun ve sz -> { id_valid =
    ((&&) (size_16_64 (wsize_of_velem ve)) (size_128_256 sz)); id_msb_flag =
    MSB_CLEAR; id_tin = (w2_ty sz sz); id_in =
    ((coq_Ea x86_decl (S O)) :: ((coq_Eu x86_decl (S (S O))) :: []));
    id_tout = (w_ty sz); id_out = ((coq_Ea x86_decl O) :: []); id_semi =
    (sem_prod_ok (map eval_ltype (w2_ty sz sz))
      (Obj.magic x86_VPSRLV (wsize_of_velem ve) sz)); id_args_kinds =
    (check_xmm_xmm_xmmm sz); id_nargs = (S (S (S O))); id_str_jas =
    (pp_ve_sz "VPSRLV" ve sz); id_safe = []; id_pp_asm =
    (pp_viname "vpsrlv" ve sz) }), ("VPSRLV",
    (primV_16_64 (fun x x0 -> VPSRLV (x, x0)))))

(** val x86_vpsxldq :
    wsize -> (GRing.ComRing.sort -> GRing.ComRing.sort -> sem_tuple) ->
    GRing.ComRing.sort -> GRing.ComRing.sort -> sem_tuple **)

let x86_vpsxldq _ op =
  op

(** val x86_VPSLLDQ :
    wsize -> GRing.ComRing.sort -> GRing.ComRing.sort -> sem_tuple **)

let x86_VPSLLDQ sz =
  x86_vpsxldq sz (wpslldq sz)

(** val coq_Ox86_VPSLLDQ_instr :
    (wsize -> (register, register_ext, xmm_register, rflag, condt)
    instr_desc_t) * (string * x86_op prim_constructor) **)

let coq_Ox86_VPSLLDQ_instr =
  ((fun sz -> { id_valid = (size_128_256 sz); id_msb_flag =
    (reg_msb_flag sz); id_tin = (ww8_ty sz); id_in =
    ((coq_Eu x86_decl (S O)) :: ((coq_Ea x86_decl (S (S O))) :: []));
    id_tout = (w_ty sz); id_out = ((coq_Ea x86_decl O) :: []); id_semi =
    (sem_prod_ok (map eval_ltype (ww8_ty sz)) (Obj.magic x86_VPSLLDQ sz));
    id_args_kinds = (check_xmm_xmm_imm8 sz); id_nargs = (S (S (S O)));
    id_str_jas = (pp_sz "VPSLLDQ" sz); id_safe = []; id_pp_asm =
    (pp_name "vpslldq" sz) }), ("VPSLLDQ",
    (prim_128_256 (fun x -> VPSLLDQ x))))

(** val x86_VPSRLDQ :
    wsize -> GRing.ComRing.sort -> GRing.ComRing.sort -> sem_tuple **)

let x86_VPSRLDQ sz =
  x86_vpsxldq sz (wpsrldq sz)

(** val coq_Ox86_VPSRLDQ_instr :
    (wsize -> (register, register_ext, xmm_register, rflag, condt)
    instr_desc_t) * (string * x86_op prim_constructor) **)

let coq_Ox86_VPSRLDQ_instr =
  ((fun sz -> { id_valid = (size_128_256 sz); id_msb_flag =
    (reg_msb_flag sz); id_tin = (ww8_ty sz); id_in =
    ((coq_Eu x86_decl (S O)) :: ((coq_Ea x86_decl (S (S O))) :: []));
    id_tout = (w_ty sz); id_out = ((coq_Ea x86_decl O) :: []); id_semi =
    (sem_prod_ok (map eval_ltype (ww8_ty sz)) (Obj.magic x86_VPSRLDQ sz));
    id_args_kinds = (check_xmm_xmm_imm8 sz); id_nargs = (S (S (S O)));
    id_str_jas = (pp_sz "VPSRLDQ" sz); id_safe = []; id_pp_asm =
    (pp_name "vpsrldq" sz) }), ("VPSRLDQ",
    (prim_128_256 (fun x -> VPSRLDQ x))))

(** val x86_VPSHUFB :
    wsize -> GRing.ComRing.sort -> GRing.ComRing.sort -> GRing.ComRing.sort **)

let x86_VPSHUFB sz =
  lift2_vec U128 (wpshufb U128) sz

(** val coq_Ox86_VPSHUFB_instr :
    (wsize -> (register, register_ext, xmm_register, rflag, condt)
    instr_desc_t) * (string * x86_op prim_constructor) **)

let coq_Ox86_VPSHUFB_instr =
  ((fun sz -> { id_valid = (size_128_256 sz); id_msb_flag = MSB_CLEAR;
    id_tin = (w2_ty sz sz); id_in =
    ((coq_Ea x86_decl (S O)) :: ((coq_Eu x86_decl (S (S O))) :: []));
    id_tout = (w_ty sz); id_out = ((coq_Ea x86_decl O) :: []); id_semi =
    (sem_prod_ok (map eval_ltype (w2_ty sz sz)) (Obj.magic x86_VPSHUFB sz));
    id_args_kinds = (check_xmm_xmm_xmmm sz); id_nargs = (S (S (S O)));
    id_str_jas = (pp_sz "VPSHUFB" sz); id_safe = []; id_pp_asm =
    (pp_name "vpshufb" sz) }), ("VPSHUFB",
    (prim_128_256 (fun x -> VPSHUFB x))))

(** val check_xmm_xmmm_imm8 : wsize -> arg_kind list list list **)

let check_xmm_xmmm_imm8 _ =
  (xmm :: ((xmmm true) :: ((i U8) :: []))) :: []

(** val x86_vpshuf :
    wsize -> (GRing.ComRing.sort -> coq_Z -> GRing.ComRing.sort) ->
    GRing.ComRing.sort -> GRing.ComRing.sort -> sem_tuple **)

let x86_vpshuf _ op v1 v2 =
  op v1 (wunsigned U8 v2)

(** val x86_VPSHUFHW :
    wsize -> GRing.ComRing.sort -> GRing.ComRing.sort -> sem_tuple **)

let x86_VPSHUFHW sz =
  x86_vpshuf sz (wpshufhw sz)

(** val coq_Ox86_VPSHUFHW_instr :
    (wsize -> (register, register_ext, xmm_register, rflag, condt)
    instr_desc_t) * (string * x86_op prim_constructor) **)

let coq_Ox86_VPSHUFHW_instr =
  ((fun sz -> { id_valid = (size_128_256 sz); id_msb_flag =
    (reg_msb_flag sz); id_tin = (ww8_ty sz); id_in =
    ((coq_Eu x86_decl (S O)) :: ((coq_Ea x86_decl (S (S O))) :: []));
    id_tout = (w_ty sz); id_out = ((coq_Ea x86_decl O) :: []); id_semi =
    (sem_prod_ok (map eval_ltype (ww8_ty sz)) (Obj.magic x86_VPSHUFHW sz));
    id_args_kinds = (check_xmm_xmmm_imm8 sz); id_nargs = (S (S (S O)));
    id_str_jas = (pp_sz "VPSHUFHW" sz); id_safe = []; id_pp_asm =
    (pp_name "vpshufhw" sz) }), ("VPSHUFHW",
    (prim_128_256 (fun x -> VPSHUFHW x))))

(** val x86_VPSHUFLW :
    wsize -> GRing.ComRing.sort -> GRing.ComRing.sort -> sem_tuple **)

let x86_VPSHUFLW sz =
  x86_vpshuf sz (wpshuflw sz)

(** val coq_Ox86_VPSHUFLW_instr :
    (wsize -> (register, register_ext, xmm_register, rflag, condt)
    instr_desc_t) * (string * x86_op prim_constructor) **)

let coq_Ox86_VPSHUFLW_instr =
  ((fun sz -> { id_valid = (size_128_256 sz); id_msb_flag =
    (reg_msb_flag sz); id_tin = (ww8_ty sz); id_in =
    ((coq_Eu x86_decl (S O)) :: ((coq_Ea x86_decl (S (S O))) :: []));
    id_tout = (w_ty sz); id_out = ((coq_Ea x86_decl O) :: []); id_semi =
    (sem_prod_ok (map eval_ltype (ww8_ty sz)) (Obj.magic x86_VPSHUFLW sz));
    id_args_kinds = (check_xmm_xmmm_imm8 sz); id_nargs = (S (S (S O)));
    id_str_jas = (pp_sz "VPSHUFLW" sz); id_safe = []; id_pp_asm =
    (pp_name "vpshuflw" sz) }), ("VPSHUFLW",
    (prim_128_256 (fun x -> VPSHUFLW x))))

(** val x86_VPSHUFD :
    wsize -> GRing.ComRing.sort -> GRing.ComRing.sort -> sem_tuple **)

let x86_VPSHUFD sz =
  x86_vpshuf sz (wpshufd sz)

(** val coq_Ox86_VPSHUFD_instr :
    (wsize -> (register, register_ext, xmm_register, rflag, condt)
    instr_desc_t) * (string * x86_op prim_constructor) **)

let coq_Ox86_VPSHUFD_instr =
  ((fun sz -> { id_valid = (size_128_256 sz); id_msb_flag =
    (reg_msb_flag sz); id_tin = (ww8_ty sz); id_in =
    ((coq_Eu x86_decl (S O)) :: ((coq_Ea x86_decl (S (S O))) :: []));
    id_tout = (w_ty sz); id_out = ((coq_Ea x86_decl O) :: []); id_semi =
    (sem_prod_ok (map eval_ltype (ww8_ty sz)) (Obj.magic x86_VPSHUFD sz));
    id_args_kinds = (check_xmm_xmmm_imm8 sz); id_nargs = (S (S (S O)));
    id_str_jas = (pp_sz "VPSHUFD" sz); id_safe = []; id_pp_asm =
    (pp_name "vpshufd" sz) }), ("VPSHUFD",
    (prim_128_256 (fun x -> VPSHUFD x))))

(** val x86_VPUNPCKH :
    velem -> wsize -> GRing.ComRing.sort -> GRing.ComRing.sort ->
    GRing.ComRing.sort **)

let x86_VPUNPCKH ve sz =
  wpunpckh sz ve

(** val coq_Ox86_VPUNPCKH_instr :
    (velem -> wsize -> (register, register_ext, xmm_register, rflag, condt)
    instr_desc_t) * (string * x86_op prim_constructor) **)

let coq_Ox86_VPUNPCKH_instr =
  ((fun ve sz -> { id_valid = (size_128_256 sz); id_msb_flag = MSB_CLEAR;
    id_tin = (w2_ty sz sz); id_in =
    ((coq_Ea x86_decl (S O)) :: ((coq_Eu x86_decl (S (S O))) :: []));
    id_tout = (w_ty sz); id_out = ((coq_Ea x86_decl O) :: []); id_semi =
    (sem_prod_ok (map eval_ltype (w2_ty sz sz))
      (Obj.magic x86_VPUNPCKH ve sz)); id_args_kinds =
    (check_xmm_xmm_xmmm sz); id_nargs = (S (S (S O))); id_str_jas =
    (pp_ve_sz "VPUNPCKH" ve sz); id_safe = []; id_pp_asm =
    (pp_viname_long "vpunpckh" ve sz) }), ("VPUNPCKH",
    (primV (fun x x0 -> VPUNPCKH (x, x0)))))

(** val x86_VPUNPCKL :
    velem -> wsize -> GRing.ComRing.sort -> GRing.ComRing.sort ->
    GRing.ComRing.sort **)

let x86_VPUNPCKL ve sz =
  wpunpckl sz ve

(** val coq_Ox86_VPUNPCKL_instr :
    (velem -> wsize -> (register, register_ext, xmm_register, rflag, condt)
    instr_desc_t) * (string * x86_op prim_constructor) **)

let coq_Ox86_VPUNPCKL_instr =
  ((fun ve sz -> { id_valid = (size_128_256 sz); id_msb_flag = MSB_CLEAR;
    id_tin = (w2_ty sz sz); id_in =
    ((coq_Ea x86_decl (S O)) :: ((coq_Eu x86_decl (S (S O))) :: []));
    id_tout = (w_ty sz); id_out = ((coq_Ea x86_decl O) :: []); id_semi =
    (sem_prod_ok (map eval_ltype (w2_ty sz sz))
      (Obj.magic x86_VPUNPCKL ve sz)); id_args_kinds =
    (check_xmm_xmm_xmmm sz); id_nargs = (S (S (S O))); id_str_jas =
    (pp_ve_sz "VPUNPCKL" ve sz); id_safe = []; id_pp_asm =
    (pp_viname_long "vpunpckl" ve sz) }), ("VPUNPCKL",
    (primV (fun x x0 -> VPUNPCKL (x, x0)))))

(** val check_xmm_xmm_xmmm_imm8 : wsize -> arg_kind list list list **)

let check_xmm_xmm_xmmm_imm8 _ =
  (xmm :: (xmm :: ((xmmm true) :: ((i U8) :: [])))) :: []

(** val wpblendw :
    GRing.ComRing.sort -> GRing.ComRing.sort -> GRing.ComRing.sort ->
    GRing.ComRing.sort **)

let wpblendw m0 w1 w2 =
  let v1 = split_vec U128 (nat_of_wsize U16) w1 in
  let v2 = split_vec U128 (nat_of_wsize U16) w2 in
  let b = split_vec U8 (S O) m0 in
  let r0 =
    map3 (fun b0 v3 v4 ->
      if eq_op
           (GRing.Ring.Exports.coq_GRing_Ring__to__eqtype_Equality
             (reverse_coercion (word_word__canonical__GRing_Ring O) __))
           (Obj.magic b0)
           (GRing.one
             (GRing.Ring.Exports.coq_GRing_Ring__to__GRing_SemiRing
               (reverse_coercion (word_word__canonical__GRing_Ring O) __)))
      then v4
      else v3) b v1 v2
  in
  make_vec U16 U128 (Obj.magic r0)

(** val x86_VPBLEND :
    Equality.sort -> wsize -> GRing.ComRing.sort -> GRing.ComRing.sort ->
    GRing.ComRing.sort -> sem_tuple **)

let x86_VPBLEND ve sz v1 v2 m0 =
  if eq_op wsize_wsize__canonical__eqtype_Equality ve (Obj.magic U32)
  then wpblendd sz v1 v2 m0
  else lift2_vec U128 (wpblendw m0) sz v1 v2

(** val coq_Ox86_VPBLEND_instr :
    (velem -> wsize -> (register, register_ext, xmm_register, rflag, condt)
    instr_desc_t) * (string * x86_op prim_constructor) **)

let coq_Ox86_VPBLEND_instr =
  ((fun ve sz -> { id_valid =
    (let ve0 = wsize_of_velem ve in (&&) (size_16_32 ve0) (size_128_256 sz));
    id_msb_flag = (reg_msb_flag sz); id_tin = (w2w8_ty sz); id_in =
    ((coq_Ea x86_decl (S O)) :: ((coq_Eu x86_decl (S (S O))) :: ((coq_Ea
                                                                   x86_decl
                                                                   (S (S (S
                                                                   O)))) :: [])));
    id_tout = (w_ty sz); id_out = ((coq_Ea x86_decl O) :: []); id_semi =
    (sem_prod_ok (map eval_ltype (w2w8_ty sz))
      (Obj.magic x86_VPBLEND (wsize_of_velem ve) sz)); id_args_kinds =
    (check_xmm_xmm_xmmm_imm8 sz); id_nargs = (S (S (S (S O)))); id_str_jas =
    (pp_ve_sz "VPBLEND" ve sz); id_safe = []; id_pp_asm =
    (pp_viname "vpblend" ve sz) }), ("VPBLEND",
    (primV_16_32 (fun x x0 -> VPBLEND (x, x0)))))

(** val check_xmm_xmm_xmmm_xmm : wsize -> arg_kind list list list **)

let check_xmm_xmm_xmmm_xmm _ =
  (xmm :: (xmm :: ((xmmm true) :: (xmm :: [])))) :: []

(** val x86_BLENDV :
    velem -> wsize -> GRing.ComRing.sort -> GRing.ComRing.sort ->
    GRing.ComRing.sort -> sem_tuple **)

let x86_BLENDV =
  blendv

(** val coq_Ox86_BLENDV_instr :
    (velem -> wsize -> (register, register_ext, xmm_register, rflag, condt)
    instr_desc_t) * (string * x86_op prim_constructor) **)

let coq_Ox86_BLENDV_instr =
  ((fun ve sz -> { id_valid = (size_128_256 sz); id_msb_flag = MSB_CLEAR;
    id_tin = (w3_ty sz); id_in =
    ((coq_Ea x86_decl (S O)) :: ((coq_Eu x86_decl (S (S O))) :: ((coq_Ea
                                                                   x86_decl
                                                                   (S (S (S
                                                                   O)))) :: [])));
    id_tout = (w_ty sz); id_out = ((coq_Ea x86_decl O) :: []); id_semi =
    (sem_prod_ok (map eval_ltype (w3_ty sz)) (Obj.magic x86_BLENDV ve sz));
    id_args_kinds = (check_xmm_xmm_xmmm_xmm sz); id_nargs = (S (S (S (S
    O)))); id_str_jas = (pp_ve_sz "BLENDV" ve sz); id_safe = []; id_pp_asm =
    (pp_name
      (match ve with
       | VE8 -> "vpblendvb"
       | VE16 -> "<assert false>"
       | VE32 -> "vblendvps"
       | VE64 -> "vblendvpd") sz) }), ("BLENDV",
    (primV_range
      (flatten
        (map (fun ve -> map (fun sz -> PVv (ve, sz)) (U128 :: (U256 :: [])))
          (VE8 :: (VE32 :: (VE64 :: []))))) (fun x x0 -> BLENDV (x, x0)))))

(** val coq_SaturatedSignedToUnsigned :
    wsize -> wsize -> GRing.ComRing.sort -> GRing.ComRing.sort **)

let coq_SaturatedSignedToUnsigned sz1 sz2 w =
  let i1 = wsigned sz1 w in
  let i2 = cmp_max Z.compare Z0 (cmp_min Z.compare i1 (wmax_unsigned sz2)) in
  wrepr sz2 i2

(** val coq_SaturatedSignedToSigned :
    wsize -> wsize -> GRing.ComRing.sort -> GRing.ComRing.sort **)

let coq_SaturatedSignedToSigned sz1 sz2 w =
  let i1 = wsigned sz1 w in
  let i2 =
    cmp_max Z.compare (wmin_signed sz2)
      (cmp_min Z.compare i1 (wmax_signed sz2))
  in
  wrepr sz2 i2

(** val vpack2 :
    wsize -> wsize -> wsize -> (GRing.ComRing.sort -> GRing.ComRing.sort) ->
    GRing.ComRing.sort -> GRing.ComRing.sort -> GRing.ComRing.sort **)

let vpack2 sz1 sz2 sz op w1 w2 =
  make_vec sz2 sz
    (cat (map (Obj.magic op) (split_vec sz (nat_of_wsize sz1) w1))
      (map (Obj.magic op) (split_vec sz (nat_of_wsize sz1) w2)))

(** val x86_VPACKUS :
    Equality.sort -> wsize -> GRing.ComRing.sort -> GRing.ComRing.sort ->
    sem_tuple **)

let x86_VPACKUS ve sz v1 v2 =
  let doit = fun sz0 v3 v4 ->
    if eq_op wsize_wsize__canonical__eqtype_Equality ve (Obj.magic U32)
    then vpack2 U32 U16 sz0 (coq_SaturatedSignedToUnsigned U32 U16) v3 v4
    else vpack2 U16 U8 sz0 (coq_SaturatedSignedToUnsigned U16 U8) v3 v4
  in
  if eq_op wsize_wsize__canonical__eqtype_Equality (Obj.magic sz)
       (Obj.magic U128)
  then doit sz v1 v2
  else lift2_vec U128 (doit U128) sz v1 v2

(** val coq_Ox86_VPACKUS_instr :
    (velem -> wsize -> (register, register_ext, xmm_register, rflag, condt)
    instr_desc_t) * (string * x86_op prim_constructor) **)

let coq_Ox86_VPACKUS_instr =
  ((fun ve sz -> { id_valid =
    (let ve0 = wsize_of_velem ve in (&&) (size_16_32 ve0) (size_128_256 sz));
    id_msb_flag = MSB_CLEAR; id_tin = (w2_ty sz sz); id_in =
    ((coq_Ea x86_decl (S O)) :: ((coq_Eu x86_decl (S (S O))) :: []));
    id_tout = (w_ty sz); id_out = ((coq_Ea x86_decl O) :: []); id_semi =
    (sem_prod_ok (map eval_ltype (w2_ty sz sz))
      (Obj.magic x86_VPACKUS (wsize_of_velem ve) sz)); id_args_kinds =
    (check_xmm_xmm_xmmm sz); id_nargs = (S (S (S O))); id_str_jas =
    (pp_ve_sz "VPACKUS" ve sz); id_safe = []; id_pp_asm =
    (pp_name
      (if eq_op wsize_wsize__canonical__eqtype_Equality (Obj.magic U16)
            (Obj.magic wsize_of_velem ve)
       then "vpackuswb"
       else "vpackusdw") sz) }), ("VPACKUS",
    (primV_16_32 (fun x x0 -> VPACKUS (x, x0)))))

(** val x86_VPACKSS :
    Equality.sort -> wsize -> GRing.ComRing.sort -> GRing.ComRing.sort ->
    sem_tuple **)

let x86_VPACKSS ve sz v1 v2 =
  let doit = fun sz0 v3 v4 ->
    if eq_op wsize_wsize__canonical__eqtype_Equality ve (Obj.magic U32)
    then vpack2 U32 U16 sz0 (coq_SaturatedSignedToSigned U32 U16) v3 v4
    else vpack2 U16 U8 sz0 (coq_SaturatedSignedToSigned U16 U8) v3 v4
  in
  if eq_op wsize_wsize__canonical__eqtype_Equality (Obj.magic sz)
       (Obj.magic U128)
  then doit sz v1 v2
  else lift2_vec U128 (doit U128) sz v1 v2

(** val coq_Ox86_VPACKSS_instr :
    (velem -> wsize -> (register, register_ext, xmm_register, rflag, condt)
    instr_desc_t) * (string * x86_op prim_constructor) **)

let coq_Ox86_VPACKSS_instr =
  ((fun ve sz -> { id_valid =
    (let ve0 = wsize_of_velem ve in (&&) (size_16_32 ve0) (size_128_256 sz));
    id_msb_flag = MSB_CLEAR; id_tin = (w2_ty sz sz); id_in =
    ((coq_Ea x86_decl (S O)) :: ((coq_Eu x86_decl (S (S O))) :: []));
    id_tout = (w_ty sz); id_out = ((coq_Ea x86_decl O) :: []); id_semi =
    (sem_prod_ok (map eval_ltype (w2_ty sz sz))
      (Obj.magic x86_VPACKSS (wsize_of_velem ve) sz)); id_args_kinds =
    (check_xmm_xmm_xmmm sz); id_nargs = (S (S (S O))); id_str_jas =
    (pp_ve_sz "VPACKSS" ve sz); id_safe = []; id_pp_asm =
    (pp_name
      (if eq_op wsize_wsize__canonical__eqtype_Equality (Obj.magic U16)
            (Obj.magic wsize_of_velem ve)
       then "vpacksswb"
       else "vpackssdw") sz) }), ("VPACKSS",
    (primV_16_32 (fun x x0 -> VPACKSS (x, x0)))))

(** val wshufps_128 :
    GRing.ComRing.sort -> GRing.ComRing.sort -> GRing.ComRing.sort ->
    GRing.ComRing.sort **)

let wshufps_128 o s1 s2 =
  make_vec U32 U128
    ((Obj.magic wpshufd1 s1 o O) :: ((Obj.magic wpshufd1 s1 o (S O)) :: (
    (Obj.magic wpshufd1 s2 o (S (S O))) :: ((Obj.magic wpshufd1 s2 o (S (S (S
                                              O)))) :: []))))

(** val x86_VSHUFPS :
    wsize -> GRing.ComRing.sort -> GRing.ComRing.sort -> GRing.ComRing.sort
    -> sem_tuple **)

let x86_VSHUFPS sz s1 s2 o =
  lift2_vec U128 (wshufps_128 o) sz s1 s2

(** val coq_Ox86_VSHUFPS_instr :
    (wsize -> (register, register_ext, xmm_register, rflag, condt)
    instr_desc_t) * (string * x86_op prim_constructor) **)

let coq_Ox86_VSHUFPS_instr =
  ((fun sz -> { id_valid = (size_128_256 sz); id_msb_flag =
    (reg_msb_flag sz); id_tin = (w2w8_ty sz); id_in =
    ((coq_Ea x86_decl (S O)) :: ((coq_Eu x86_decl (S (S O))) :: ((coq_Ea
                                                                   x86_decl
                                                                   (S (S (S
                                                                   O)))) :: [])));
    id_tout = (w_ty sz); id_out = ((coq_Ea x86_decl O) :: []); id_semi =
    (sem_prod_ok (map eval_ltype (w2w8_ty sz)) (Obj.magic x86_VSHUFPS sz));
    id_args_kinds = (check_xmm_xmm_xmmm_imm8 sz); id_nargs = (S (S (S (S
    O)))); id_str_jas = (pp_sz "VSHUFPS" sz); id_safe = []; id_pp_asm =
    (pp_name "vshufps" sz) }), ("VSHUFPS",
    (prim_128_256 (fun x -> VSHUFPS x))))

(** val pp_vpbroadcast :
    velem -> wsize -> (register, register_ext, xmm_register, rflag, condt)
    asm_arg list -> (register, register_ext, xmm_register, rflag, condt)
    pp_asm_op **)

let pp_vpbroadcast ve sz args =
  { pp_aop_name = "vpbroadcast"; pp_aop_ext = (PP_viname (ve, false));
    pp_aop_args = (zip (sz :: ((wsize_of_velem ve) :: [])) args) }

(** val check_xmm_xmmm : wsize -> arg_kind list list list **)

let check_xmm_xmmm _ =
  (xmm :: ((xmmm true) :: [])) :: []

(** val x86_VPBROADCAST :
    wsize -> wsize -> GRing.ComRing.sort -> sem_tuple **)

let x86_VPBROADCAST =
  wpbroadcast

(** val coq_Ox86_VPBROADCAST_instr :
    (velem -> wsize -> (register, register_ext, xmm_register, rflag, condt)
    instr_desc_t) * (string * x86_op prim_constructor) **)

let coq_Ox86_VPBROADCAST_instr =
  ((fun ve sz -> { id_valid = (size_128_256 sz); id_msb_flag =
    (reg_msb_flag sz); id_tin = (w_ty (wsize_of_velem ve)); id_in =
    ((coq_Eu x86_decl (S O)) :: []); id_tout = (w_ty sz); id_out =
    ((coq_Ea x86_decl O) :: []); id_semi =
    (sem_prod_ok (map eval_ltype (w_ty (wsize_of_velem ve)))
      (Obj.magic x86_VPBROADCAST (wsize_of_velem ve) sz)); id_args_kinds =
    (check_xmm_xmmm sz); id_nargs = (S (S O)); id_str_jas =
    (pp_ve_sz "VPBROADCAST" ve sz); id_safe = []; id_pp_asm =
    (pp_vpbroadcast ve sz) }), ("VPBROADCAST",
    (primV (fun x x0 -> VPBROADCAST (x, x0)))))

(** val x86_VMOVSHDUP : wsize -> GRing.ComRing.sort -> sem_tuple **)

let x86_VMOVSHDUP sz v =
  wdup_hi (wsize_of_velem VE32) sz v

(** val coq_Ox86_VMOVSHDUP_instr :
    (wsize -> (register, register_ext, xmm_register, rflag, condt)
    instr_desc_t) * (string * x86_op prim_constructor) **)

let coq_Ox86_VMOVSHDUP_instr =
  ((fun sz -> { id_valid = (size_128_256 sz); id_msb_flag =
    (reg_msb_flag sz); id_tin = (w_ty sz); id_in =
    ((coq_Eu x86_decl (S O)) :: []); id_tout = (w_ty sz); id_out =
    ((coq_Eu x86_decl O) :: []); id_semi =
    (sem_prod_ok (map eval_ltype (w_ty sz)) (Obj.magic x86_VMOVSHDUP sz));
    id_args_kinds = (check_xmm_xmmm sz); id_nargs = (S (S O)); id_str_jas =
    (pp_sz "VMOVSHDUP" sz); id_safe = []; id_pp_asm =
    (pp_name "vmovshdup" sz) }), ("VMOVSHDUP",
    (prim_128_256 (fun x -> VMOVSHDUP x))))

(** val x86_VMOVSLDUP : wsize -> GRing.ComRing.sort -> sem_tuple **)

let x86_VMOVSLDUP sz v =
  wdup_lo (wsize_of_velem VE32) sz v

(** val coq_Ox86_VMOVSLDUP_instr :
    (wsize -> (register, register_ext, xmm_register, rflag, condt)
    instr_desc_t) * (string * x86_op prim_constructor) **)

let coq_Ox86_VMOVSLDUP_instr =
  ((fun sz -> { id_valid = (size_128_256 sz); id_msb_flag =
    (reg_msb_flag sz); id_tin = (w_ty sz); id_in =
    ((coq_Eu x86_decl (S O)) :: []); id_tout = (w_ty sz); id_out =
    ((coq_Eu x86_decl O) :: []); id_semi =
    (sem_prod_ok (map eval_ltype (w_ty sz)) (Obj.magic x86_VMOVSLDUP sz));
    id_args_kinds = (check_xmm_xmmm sz); id_nargs = (S (S O)); id_str_jas =
    (pp_sz "VMOVSLDUP" sz); id_safe = []; id_pp_asm =
    (pp_name "vmovsldup" sz) }), ("VMOVSLDUP",
    (prim_128_256 (fun x -> VMOVSLDUP x))))

(** val x86_VPALIGNR128 :
    GRing.ComRing.sort -> GRing.ComRing.sort -> GRing.ComRing.sort ->
    GRing.ComRing.sort **)

let x86_VPALIGNR128 m0 v1 v2 =
  let v = make_vec U128 U256 (v2 :: (v1 :: [])) in
  let v' =
    wshr U256 v
      (Z.mul (wunsigned U8 m0) (Zpos (Coq_xO (Coq_xO (Coq_xO Coq_xH)))))
  in
  nth
    (GRing.zero
      (GRing.ComRing.Exports.coq_GRing_ComRing__to__GRing_Nmodule (word U128)))
    (Obj.magic split_vec U256 (nat_of_wsize U128) v') O

(** val x86_VPALIGNR :
    wsize -> GRing.ComRing.sort -> GRing.ComRing.sort -> GRing.ComRing.sort
    -> sem_tuple **)

let x86_VPALIGNR sz v1 v2 m0 =
  lift2_vec U128 (x86_VPALIGNR128 m0) sz v1 v2

(** val coq_Ox86_VPALIGNR_instr :
    (wsize -> (register, register_ext, xmm_register, rflag, condt)
    instr_desc_t) * (string * x86_op prim_constructor) **)

let coq_Ox86_VPALIGNR_instr =
  ((fun sz -> { id_valid = (size_128_256 sz); id_msb_flag = MSB_CLEAR;
    id_tin = (w2w8_ty sz); id_in =
    ((coq_Eu x86_decl (S O)) :: ((coq_Eu x86_decl (S (S O))) :: ((coq_Eu
                                                                   x86_decl
                                                                   (S (S (S
                                                                   O)))) :: [])));
    id_tout = (w_ty sz); id_out = ((coq_Eu x86_decl O) :: []); id_semi =
    (sem_prod_ok (map eval_ltype (w2w8_ty sz)) (Obj.magic x86_VPALIGNR sz));
    id_args_kinds = (check_xmm_xmm_xmmm_imm8 sz); id_nargs = (S (S (S (S
    O)))); id_str_jas = (pp_sz "VPALIGNR" sz); id_safe = []; id_pp_asm =
    (pp_name "vpalignr" sz) }), ("VPALIGNR",
    (prim_128_256 (fun x -> VPALIGNR x))))

(** val coq_Ox86_VBROADCASTI128_instr :
    (register, register_ext, xmm_register, rflag, condt)
    instr_desc_t * (string * x86_op prim_constructor) **)

let coq_Ox86_VBROADCASTI128_instr =
  ({ id_valid = true; id_msb_flag = MSB_CLEAR; id_tin = w128_ty; id_in =
    ((coq_Eu x86_decl (S O)) :: []); id_tout = w256_ty; id_out =
    ((coq_Eu x86_decl O) :: []); id_semi =
    (sem_prod_ok (map eval_ltype w128_ty)
      (Obj.magic x86_VPBROADCAST U128 U256)); id_args_kinds =
    ((xmm :: ((m true) :: [])) :: []); id_nargs = (S (S O)); id_str_jas =
    (pp_s "VPBROADCAST_2u128"); id_safe = []; id_pp_asm =
    (pp_name_ty "vbroadcasti128" (U256 :: (U128 :: []))) },
    ("VPBROADCAST_2u128", (primM VBROADCASTI128)))

(** val check_xmmm_xmm_imm8 : wsize -> arg_kind list list list **)

let check_xmmm_xmm_imm8 _ =
  ((xmmm false) :: (xmm :: ((i U8) :: []))) :: []

(** val x86_VEXTRACTI128 :
    GRing.ComRing.sort -> GRing.ComRing.sort -> sem_tuple **)

let x86_VEXTRACTI128 v i0 =
  let r0 = if lsb U8 i0 then wshr U256 v (Z.of_nat (nat_of_wsize U128)) else v
  in
  zero_extend U128 U256 r0

(** val coq_Ox86_VEXTRACTI128_instr :
    (register, register_ext, xmm_register, rflag, condt)
    instr_desc_t * (string * x86_op prim_constructor) **)

let coq_Ox86_VEXTRACTI128_instr =
  ({ id_valid = true; id_msb_flag = MSB_CLEAR; id_tin = w256w8_ty; id_in =
    ((coq_Eu x86_decl (S O)) :: ((coq_Eu x86_decl (S (S O))) :: []));
    id_tout = w128_ty; id_out = ((coq_Eu x86_decl O) :: []); id_semi =
    (sem_prod_ok (map eval_ltype w256w8_ty) (Obj.magic x86_VEXTRACTI128));
    id_args_kinds = (check_xmmm_xmm_imm8 U256); id_nargs = (S (S (S O)));
    id_str_jas = (pp_s "VEXTRACTI128"); id_safe = []; id_pp_asm =
    (pp_name_ty "vextracti128" (U128 :: (U256 :: (U8 :: [])))) },
    ("VEXTRACTI128", (primM VEXTRACTI128)))

(** val x86_VINSERTI128 :
    GRing.ComRing.sort -> GRing.ComRing.sort -> GRing.ComRing.sort ->
    sem_tuple **)

let x86_VINSERTI128 =
  winserti128

(** val coq_Ox86_VINSERTI128_instr :
    (register, register_ext, xmm_register, rflag, condt)
    instr_desc_t * (string * x86_op prim_constructor) **)

let coq_Ox86_VINSERTI128_instr =
  ({ id_valid = true; id_msb_flag = MSB_CLEAR; id_tin = w256w128w8_ty;
    id_in =
    ((coq_Eu x86_decl (S O)) :: ((coq_Eu x86_decl (S (S O))) :: ((coq_Eu
                                                                   x86_decl
                                                                   (S (S (S
                                                                   O)))) :: [])));
    id_tout = w256_ty; id_out = ((coq_Eu x86_decl O) :: []); id_semi =
    (sem_prod_ok (map eval_ltype w256w128w8_ty) (Obj.magic x86_VINSERTI128));
    id_args_kinds = (check_xmm_xmm_xmmm_imm8 U256); id_nargs = (S (S (S (S
    O)))); id_str_jas = (pp_s "VINSERTI128"); id_safe = []; id_pp_asm =
    (pp_name_ty "vinserti128" (U256 :: (U256 :: (U128 :: (U8 :: []))))) },
    ("VINSERTI128", (primM VINSERTI128)))

(** val x86_VPERM2I128 :
    GRing.ComRing.sort -> GRing.ComRing.sort -> GRing.ComRing.sort ->
    sem_tuple **)

let x86_VPERM2I128 =
  wperm2i128

(** val coq_Ox86_VPERM2I128_instr :
    (register, register_ext, xmm_register, rflag, condt)
    instr_desc_t * (string * x86_op prim_constructor) **)

let coq_Ox86_VPERM2I128_instr =
  ({ id_valid = true; id_msb_flag = MSB_CLEAR; id_tin = w256x2w8_ty; id_in =
    ((coq_Eu x86_decl (S O)) :: ((coq_Eu x86_decl (S (S O))) :: ((coq_Eu
                                                                   x86_decl
                                                                   (S (S (S
                                                                   O)))) :: [])));
    id_tout = w256_ty; id_out = ((coq_Eu x86_decl O) :: []); id_semi =
    (sem_prod_ok (map eval_ltype w256x2w8_ty) (Obj.magic x86_VPERM2I128));
    id_args_kinds = (check_xmm_xmm_xmmm_imm8 U256); id_nargs = (S (S (S (S
    O)))); id_str_jas = (pp_s "VPERM2I128"); id_safe = []; id_pp_asm =
    (pp_name_ty "vperm2i128" (U256 :: (U256 :: (U256 :: (U8 :: []))))) },
    ("VPERM2I128", (primM VPERM2I128)))

(** val x86_VPERMD : GRing.ComRing.sort -> GRing.ComRing.sort -> sem_tuple **)

let x86_VPERMD v1 v2 =
  wpermd U256 v1 v2

(** val coq_Ox86_VPERMD_instr :
    (register, register_ext, xmm_register, rflag, condt)
    instr_desc_t * (string * x86_op prim_constructor) **)

let coq_Ox86_VPERMD_instr =
  ({ id_valid = true; id_msb_flag = MSB_CLEAR; id_tin = (w2_ty U256 U256);
    id_in = ((coq_Eu x86_decl (S O)) :: ((coq_Eu x86_decl (S (S O))) :: []));
    id_tout = w256_ty; id_out = ((coq_Eu x86_decl O) :: []); id_semi =
    (sem_prod_ok (map eval_ltype (w2_ty U256 U256)) (Obj.magic x86_VPERMD));
    id_args_kinds = (check_xmm_xmm_xmmm U256); id_nargs = (S (S (S O)));
    id_str_jas = (pp_s "VPERMD"); id_safe = []; id_pp_asm =
    (pp_name "vpermd" U256) }, ("VPERMD", (primM VPERMD)))

(** val x86_VPERMQ : GRing.ComRing.sort -> GRing.ComRing.sort -> sem_tuple **)

let x86_VPERMQ =
  wpermq

(** val coq_Ox86_VPERMQ_instr :
    (register, register_ext, xmm_register, rflag, condt)
    instr_desc_t * (string * x86_op prim_constructor) **)

let coq_Ox86_VPERMQ_instr =
  ({ id_valid = true; id_msb_flag = MSB_CLEAR; id_tin = w256w8_ty; id_in =
    ((coq_Eu x86_decl (S O)) :: ((coq_Eu x86_decl (S (S O))) :: []));
    id_tout = w256_ty; id_out = ((coq_Eu x86_decl O) :: []); id_semi =
    (sem_prod_ok (map eval_ltype w256w8_ty) (Obj.magic x86_VPERMQ));
    id_args_kinds = (check_xmm_xmmm_imm8 U256); id_nargs = (S (S (S O)));
    id_str_jas = (pp_s "VPERMQ"); id_safe = []; id_pp_asm =
    (pp_name_ty "vpermq" (U256 :: (U256 :: (U8 :: [])))) }, ("VPERMQ",
    (primM VPERMQ)))

(** val coq_Ox86_MOVEMASK_instr :
    (velem -> wsize -> (register, register_ext, xmm_register, rflag, condt)
    instr_desc_t) * (string * x86_op prim_constructor) **)

let coq_Ox86_MOVEMASK_instr =
  ((fun ve sz -> { id_valid =
    ((&&)
      (in_mem (Obj.magic wsize_of_velem ve)
        (mem (seq_predType wsize_wsize__canonical__eqtype_Equality)
          (Obj.magic (U8 :: (U32 :: (U64 :: [])))))) (size_128_256 sz));
    id_msb_flag = MSB_CLEAR; id_tin = (w_ty sz); id_in =
    ((coq_Eu x86_decl (S O)) :: []); id_tout = (w_ty U32); id_out =
    ((coq_Eu x86_decl O) :: []); id_semi =
    (sem_prod_ok (map eval_ltype (w_ty sz)) (Obj.magic movemask ve sz));
    id_args_kinds = ((r :: (xmm :: [])) :: []); id_nargs = (S (S O));
    id_str_jas = (pp_ve_sz "MOVEMASK" ve sz); id_safe = []; id_pp_asm =
    (pp_name_ty
      (match ve with
       | VE8 -> "vpmovmskb"
       | VE16 -> "<assert false>"
       | VE32 -> "vmovmskps"
       | VE64 -> "vmovmskpd") (U32 :: (sz :: []))) }), ("MOVEMASK",
    (primV_range
      (flatten
        (map (fun ve -> map (fun sz -> PVv (ve, sz)) (U128 :: (U256 :: [])))
          (VE8 :: (VE32 :: (VE64 :: []))))) (fun x x0 -> MOVEMASK (x, x0)))))

(** val x86_VPCMPEQ :
    velem -> wsize -> GRing.ComRing.sort -> GRing.ComRing.sort -> sem_tuple **)

let x86_VPCMPEQ ve sz v1 v2 =
  wpcmpeq (wsize_of_velem ve) sz v1 v2

(** val coq_Ox86_VPCMPEQ_instr :
    (velem -> wsize -> (register, register_ext, xmm_register, rflag, condt)
    instr_desc_t) * (string * x86_op prim_constructor) **)

let coq_Ox86_VPCMPEQ_instr =
  ((fun ve sz -> { id_valid =
    ((&&) (size_8_64 (wsize_of_velem ve)) (size_128_256 sz)); id_msb_flag =
    MSB_CLEAR; id_tin = (w2_ty sz sz); id_in =
    ((coq_Eu x86_decl (S O)) :: ((coq_Eu x86_decl (S (S O))) :: []));
    id_tout = (w_ty sz); id_out = ((coq_Eu x86_decl O) :: []); id_semi =
    (sem_prod_ok (map eval_ltype (w2_ty sz sz)) (Obj.magic x86_VPCMPEQ ve sz));
    id_args_kinds = (check_xmm_xmm_xmmm sz); id_nargs = (S (S (S O)));
    id_str_jas = (pp_ve_sz "VPCMPEQ" ve sz); id_safe = []; id_pp_asm =
    (pp_viname "vpcmpeq" ve sz) }), ("VPCMPEQ",
    (primV (fun x x0 -> VPCMPEQ (x, x0)))))

(** val x86_VPCMPGT :
    velem -> wsize -> GRing.ComRing.sort -> GRing.ComRing.sort -> sem_tuple **)

let x86_VPCMPGT ve sz v1 v2 =
  wpcmpgt (wsize_of_velem ve) sz v1 v2

(** val coq_Ox86_VPCMPGT_instr :
    (velem -> wsize -> (register, register_ext, xmm_register, rflag, condt)
    instr_desc_t) * (string * x86_op prim_constructor) **)

let coq_Ox86_VPCMPGT_instr =
  ((fun ve sz -> { id_valid =
    ((&&) (size_8_64 (wsize_of_velem ve)) (size_128_256 sz)); id_msb_flag =
    MSB_CLEAR; id_tin = (w2_ty sz sz); id_in =
    ((coq_Eu x86_decl (S O)) :: ((coq_Eu x86_decl (S (S O))) :: []));
    id_tout = (w_ty sz); id_out = ((coq_Eu x86_decl O) :: []); id_semi =
    (sem_prod_ok (map eval_ltype (w2_ty sz sz)) (Obj.magic x86_VPCMPGT ve sz));
    id_args_kinds = (check_xmm_xmm_xmmm sz); id_nargs = (S (S (S O)));
    id_str_jas = (pp_ve_sz "VPCMPGT" ve sz); id_safe = []; id_pp_asm =
    (pp_viname "vpcmpgt" ve sz) }), ("VPCMPGT",
    (primV (fun x x0 -> VPCMPGT (x, x0)))))

(** val x86_VPSIGN :
    velem -> wsize -> GRing.ComRing.sort -> GRing.ComRing.sort -> sem_tuple **)

let x86_VPSIGN ve sz v1 v2 =
  lift2_vec (wsize_of_velem ve) (fun x m0 ->
    match wsigned (wsize_of_velem ve) m0 with
    | Z0 ->
      GRing.zero
        (GRing.ComRing.Exports.coq_GRing_ComRing__to__GRing_Nmodule
          (word (wsize_of_velem ve)))
    | Zpos _ -> x
    | Zneg _ ->
      GRing.opp
        (GRing.ComRing.Exports.coq_GRing_ComRing__to__GRing_Zmodule
          (word (wsize_of_velem ve))) x) sz v1 v2

(** val coq_Ox86_VPSIGN_instr :
    (velem -> wsize -> (register, register_ext, xmm_register, rflag, condt)
    instr_desc_t) * (string * x86_op prim_constructor) **)

let coq_Ox86_VPSIGN_instr =
  ((fun ve sz -> { id_valid =
    ((&&) (size_8_32 (wsize_of_velem ve)) (size_128_256 sz)); id_msb_flag =
    MSB_CLEAR; id_tin = (w2_ty sz sz); id_in =
    ((coq_Ea x86_decl (S O)) :: ((coq_Eu x86_decl (S (S O))) :: []));
    id_tout = (w_ty sz); id_out = ((coq_Ea x86_decl O) :: []); id_semi =
    (sem_prod_ok (map eval_ltype (w2_ty sz sz)) (Obj.magic x86_VPSIGN ve sz));
    id_args_kinds = (check_xmm_xmm_xmmm sz); id_nargs = (S (S (S O)));
    id_str_jas = (pp_ve_sz "VPSIGN" ve sz); id_safe = []; id_pp_asm =
    (pp_viname "vpsign" ve sz) }), ("VPSIGN",
    (primV_8_32 (fun x x0 -> VPSIGN (x, x0)))))

(** val x86_VPMADDUBSW :
    wsize -> GRing.ComRing.sort -> GRing.ComRing.sort -> sem_tuple **)

let x86_VPMADDUBSW =
  wpmaddubsw

(** val coq_Ox86_VPMADDUBSW_instr :
    (wsize -> (register, register_ext, xmm_register, rflag, condt)
    instr_desc_t) * (string * x86_op prim_constructor) **)

let coq_Ox86_VPMADDUBSW_instr =
  ((fun sz -> { id_valid = (size_128_256 sz); id_msb_flag = MSB_CLEAR;
    id_tin = (w2_ty sz sz); id_in =
    ((coq_Eu x86_decl (S O)) :: ((coq_Eu x86_decl (S (S O))) :: []));
    id_tout = (w_ty sz); id_out = ((coq_Eu x86_decl O) :: []); id_semi =
    (sem_prod_ok (map eval_ltype (w2_ty sz sz)) (Obj.magic x86_VPMADDUBSW sz));
    id_args_kinds = (check_xmm_xmm_xmmm sz); id_nargs = (S (S (S O)));
    id_str_jas = (pp_sz "VPMADDUBSW" sz); id_safe = []; id_pp_asm =
    (pp_name_ty "vpmaddubsw" (sz :: (sz :: (sz :: [])))) }), ("VPMADDUBSW",
    (prim_128_256 (fun x -> VPMADDUBSW x))))

(** val x86_VPMADDWD :
    wsize -> GRing.ComRing.sort -> GRing.ComRing.sort -> sem_tuple **)

let x86_VPMADDWD =
  wpmaddwd

(** val coq_Ox86_VPMADDWD_instr :
    (wsize -> (register, register_ext, xmm_register, rflag, condt)
    instr_desc_t) * (string * x86_op prim_constructor) **)

let coq_Ox86_VPMADDWD_instr =
  ((fun sz -> { id_valid = (size_128_256 sz); id_msb_flag = MSB_CLEAR;
    id_tin = (w2_ty sz sz); id_in =
    ((coq_Eu x86_decl (S O)) :: ((coq_Eu x86_decl (S (S O))) :: []));
    id_tout = (w_ty sz); id_out = ((coq_Eu x86_decl O) :: []); id_semi =
    (sem_prod_ok (map eval_ltype (w2_ty sz sz)) (Obj.magic x86_VPMADDWD sz));
    id_args_kinds = (check_xmm_xmm_xmmm sz); id_nargs = (S (S (S O)));
    id_str_jas = (pp_sz "VPMADDWD" sz); id_safe = []; id_pp_asm =
    (pp_name_ty "vpmaddwd" (sz :: (sz :: (sz :: [])))) }), ("VPMADDWD",
    (prim_128_256 (fun x -> VPMADDWD x))))

(** val check_movpd : arg_kind list list list **)

let check_movpd =
  ((m false) :: (xmm :: [])) :: []

(** val x86_VMOVLPD : GRing.ComRing.sort -> sem_tuple **)

let x86_VMOVLPD v =
  zero_extend U64 U128 v

(** val coq_Ox86_VMOVLPD_instr :
    (register, register_ext, xmm_register, rflag, condt)
    instr_desc_t * (string * x86_op prim_constructor) **)

let coq_Ox86_VMOVLPD_instr =
  ({ id_valid = true; id_msb_flag = MSB_CLEAR; id_tin = (w_ty U128); id_in =
    ((coq_Eu x86_decl (S O)) :: []); id_tout = (w_ty U64); id_out =
    ((coq_Eu x86_decl O) :: []); id_semi =
    (sem_prod_ok (map eval_ltype (w_ty U128)) (Obj.magic x86_VMOVLPD));
    id_args_kinds = check_movpd; id_nargs = (S (S O)); id_str_jas =
    (pp_s "VMOVLPD"); id_safe = []; id_pp_asm =
    (pp_name_ty "vmovlpd" (U64 :: (U128 :: []))) }, ("VMOVLPD",
    (primM VMOVLPD)))

(** val x86_VMOVHPD : GRing.ComRing.sort -> sem_tuple **)

let x86_VMOVHPD v =
  zero_extend U64 U128
    (wshr U128 v (Zpos (Coq_xO (Coq_xO (Coq_xO (Coq_xO (Coq_xO (Coq_xO
      Coq_xH))))))))

(** val coq_Ox86_VMOVHPD_instr :
    (register, register_ext, xmm_register, rflag, condt)
    instr_desc_t * (string * x86_op prim_constructor) **)

let coq_Ox86_VMOVHPD_instr =
  ({ id_valid = true; id_msb_flag = MSB_CLEAR; id_tin = (w_ty U128); id_in =
    ((coq_Eu x86_decl (S O)) :: []); id_tout = (w_ty U64); id_out =
    ((coq_Eu x86_decl O) :: []); id_semi =
    (sem_prod_ok (map eval_ltype (w_ty U128)) (Obj.magic x86_VMOVHPD));
    id_args_kinds = check_movpd; id_nargs = (S (S O)); id_str_jas =
    (pp_s "VMOVHPD"); id_safe = []; id_pp_asm =
    (pp_name_ty "vmovhpd" (U64 :: (U128 :: []))) }, ("VMOVHPD",
    (primM VMOVHPD)))

(** val x86_VPMINS :
    velem -> wsize -> GRing.ComRing.sort -> GRing.ComRing.sort -> sem_tuple **)

let x86_VPMINS ve sz x y =
  wmin Signed (wsize_of_velem ve) sz x y

(** val coq_Ox86_VPMINS_instr :
    (velem -> wsize -> (register, register_ext, xmm_register, rflag, condt)
    instr_desc_t) * (string * x86_op prim_constructor) **)

let coq_Ox86_VPMINS_instr =
  ((fun ve sz -> { id_valid =
    (let ve0 = wsize_of_velem ve in (&&) (size_8_32 ve0) (size_128_256 sz));
    id_msb_flag = MSB_CLEAR; id_tin = (w2_ty sz sz); id_in =
    ((coq_Ea x86_decl (S O)) :: ((coq_Eu x86_decl (S (S O))) :: []));
    id_tout = (w_ty sz); id_out = ((coq_Ea x86_decl O) :: []); id_semi =
    (sem_prod_ok (map eval_ltype (w2_ty sz sz)) (Obj.magic x86_VPMINS ve sz));
    id_args_kinds = (check_xmm_xmm_xmmm sz); id_nargs = (S (S (S O)));
    id_str_jas = (pp_ve_sz "VPMINS" ve sz); id_safe = []; id_pp_asm =
    (pp_viname "vpmins" ve sz) }), ("VPMINS",
    (primV_8_32 (fun x x0 -> VPMINS (x, x0)))))

(** val x86_VPMINU :
    velem -> wsize -> GRing.ComRing.sort -> GRing.ComRing.sort -> sem_tuple **)

let x86_VPMINU ve sz x y =
  wmin Unsigned (wsize_of_velem ve) sz x y

(** val coq_Ox86_VPMINU_instr :
    (velem -> wsize -> (register, register_ext, xmm_register, rflag, condt)
    instr_desc_t) * (string * x86_op prim_constructor) **)

let coq_Ox86_VPMINU_instr =
  ((fun ve sz -> { id_valid =
    (let ve0 = wsize_of_velem ve in (&&) (size_8_32 ve0) (size_128_256 sz));
    id_msb_flag = MSB_CLEAR; id_tin = (w2_ty sz sz); id_in =
    ((coq_Ea x86_decl (S O)) :: ((coq_Eu x86_decl (S (S O))) :: []));
    id_tout = (w_ty sz); id_out = ((coq_Ea x86_decl O) :: []); id_semi =
    (sem_prod_ok (map eval_ltype (w2_ty sz sz)) (Obj.magic x86_VPMINU ve sz));
    id_args_kinds = (check_xmm_xmm_xmmm sz); id_nargs = (S (S (S O)));
    id_str_jas = (pp_ve_sz "VPMINU" ve sz); id_safe = []; id_pp_asm =
    (pp_viname "vpminu" ve sz) }), ("VPMINU",
    (primV_8_32 (fun x x0 -> VPMINU (x, x0)))))

(** val x86_VPMAXS :
    velem -> wsize -> GRing.ComRing.sort -> GRing.ComRing.sort -> sem_tuple **)

let x86_VPMAXS ve sz x y =
  wmax Signed (wsize_of_velem ve) sz x y

(** val coq_Ox86_VPMAXS_instr :
    (velem -> wsize -> (register, register_ext, xmm_register, rflag, condt)
    instr_desc_t) * (string * x86_op prim_constructor) **)

let coq_Ox86_VPMAXS_instr =
  ((fun ve sz -> { id_valid =
    (let ve0 = wsize_of_velem ve in (&&) (size_8_32 ve0) (size_128_256 sz));
    id_msb_flag = MSB_CLEAR; id_tin = (w2_ty sz sz); id_in =
    ((coq_Ea x86_decl (S O)) :: ((coq_Eu x86_decl (S (S O))) :: []));
    id_tout = (w_ty sz); id_out = ((coq_Ea x86_decl O) :: []); id_semi =
    (sem_prod_ok (map eval_ltype (w2_ty sz sz)) (Obj.magic x86_VPMAXS ve sz));
    id_args_kinds = (check_xmm_xmm_xmmm sz); id_nargs = (S (S (S O)));
    id_str_jas = (pp_ve_sz "VPMAXS" ve sz); id_safe = []; id_pp_asm =
    (pp_viname "vpmaxs" ve sz) }), ("VPMAXS",
    (primV_8_32 (fun x x0 -> VPMAXS (x, x0)))))

(** val x86_VPMAXU :
    velem -> wsize -> GRing.ComRing.sort -> GRing.ComRing.sort -> sem_tuple **)

let x86_VPMAXU ve sz x y =
  wmax Unsigned (wsize_of_velem ve) sz x y

(** val coq_Ox86_VPMAXU_instr :
    (velem -> wsize -> (register, register_ext, xmm_register, rflag, condt)
    instr_desc_t) * (string * x86_op prim_constructor) **)

let coq_Ox86_VPMAXU_instr =
  ((fun ve sz -> { id_valid =
    (let ve0 = wsize_of_velem ve in (&&) (size_8_32 ve0) (size_128_256 sz));
    id_msb_flag = MSB_CLEAR; id_tin = (w2_ty sz sz); id_in =
    ((coq_Ea x86_decl (S O)) :: ((coq_Eu x86_decl (S (S O))) :: []));
    id_tout = (w_ty sz); id_out = ((coq_Ea x86_decl O) :: []); id_semi =
    (sem_prod_ok (map eval_ltype (w2_ty sz sz)) (Obj.magic x86_VPMAXU ve sz));
    id_args_kinds = (check_xmm_xmm_xmmm sz); id_nargs = (S (S (S O)));
    id_str_jas = (pp_ve_sz "VPMAXU" ve sz); id_safe = []; id_pp_asm =
    (pp_viname "vpmaxu" ve sz) }), ("VPMAXU",
    (primV_8_32 (fun x x0 -> VPMAXU (x, x0)))))

(** val x86_VPABS : velem -> wsize -> GRing.ComRing.sort -> sem_tuple **)

let x86_VPABS =
  wabs

(** val coq_Ox86_VPABS_instr :
    (velem -> wsize -> (register, register_ext, xmm_register, rflag, condt)
    instr_desc_t) * (string * x86_op prim_constructor) **)

let coq_Ox86_VPABS_instr =
  ((fun ve sz -> { id_valid =
    (let ve0 = wsize_of_velem ve in (&&) (size_8_32 ve0) (size_128_256 sz));
    id_msb_flag = (reg_msb_flag sz); id_tin = (w_ty sz); id_in =
    ((coq_Eu x86_decl (S O)) :: []); id_tout = (w_ty sz); id_out =
    ((coq_Ea x86_decl O) :: []); id_semi =
    (sem_prod_ok (map eval_ltype (w_ty sz)) (Obj.magic x86_VPABS ve sz));
    id_args_kinds = (check_xmm_xmmm sz); id_nargs = (S (S O)); id_str_jas =
    (pp_ve_sz "VPABS" ve sz); id_safe = []; id_pp_asm =
    (pp_viname "vpabs" ve sz) }), ("VPABS",
    (primV_8_32 (fun x x0 -> VPABS (x, x0)))))

(** val check_vptest : wsize -> arg_kind list list list **)

let check_vptest _ =
  xmm_xmmm :: []

(** val x86_VPTEST :
    wsize -> GRing.ComRing.sort -> GRing.ComRing.sort -> sem_tuple **)

let x86_VPTEST sz x y =
  Obj.magic ((Some false), ((Some
    (eq_op
      (GRing.ComRing.Exports.coq_GRing_ComRing__to__eqtype_Equality (word sz))
      (wandn sz x y)
      (GRing.zero
        (GRing.ComRing.Exports.coq_GRing_ComRing__to__GRing_Nmodule (word sz))))),
    ((Some false), ((Some false), (Some
    (coq_ZF_of_word sz (Word0.wand sz x y)))))))

(** val coq_Ox86_VPTEST_instr :
    (wsize -> (register, register_ext, xmm_register, rflag, condt)
    instr_desc_t) * (string * x86_op prim_constructor) **)

let coq_Ox86_VPTEST_instr =
  ((fun sz -> { id_valid = (size_128_256 sz); id_msb_flag = MSB_MERGE;
    id_tin = (w2_ty sz sz); id_in =
    ((coq_Eu x86_decl O) :: ((coq_Eu x86_decl (S O)) :: [])); id_tout =
    b5_ty; id_out = implicit_flags; id_semi =
    (sem_prod_ok (map eval_ltype (w2_ty sz sz)) (Obj.magic x86_VPTEST sz));
    id_args_kinds = (check_vptest sz); id_nargs = (S (S O)); id_str_jas =
    (pp_sz "VPTEST" sz); id_safe = []; id_pp_asm = (pp_name "vptest" sz) }),
    ("VPTEST", (prim_128_256 (fun x -> VPTEST x))))

(** val coq_Ox86_RDTSC_instr :
    (wsize -> (register, register_ext, xmm_register, rflag, condt)
    instr_desc_t) * (string * x86_op prim_constructor) **)

let coq_Ox86_RDTSC_instr =
  ((fun sz -> { id_valid = (size_32_64 sz); id_msb_flag = MSB_CLEAR; id_tin =
    []; id_in = []; id_tout = (w2_ty sz sz); id_out =
    ((coq_R x86_decl RDX) :: ((coq_R x86_decl RAX) :: [])); id_semi =
    (Obj.magic (Error ErrSemUndef)); id_args_kinds = ([] :: []); id_nargs =
    O; id_str_jas = (pp_sz "RDTSC" sz); id_safe = (ScFalse :: []);
    id_pp_asm = (pp_name_ty "rdtsc" (sz :: (sz :: []))) }), ("RDTSC",
    (prim_32_64 (fun x -> RDTSC x))))

(** val coq_Ox86_RDTSCP_instr :
    (wsize -> (register, register_ext, xmm_register, rflag, condt)
    instr_desc_t) * (string * x86_op prim_constructor) **)

let coq_Ox86_RDTSCP_instr =
  ((fun sz -> { id_valid = (size_32_64 sz); id_msb_flag = MSB_CLEAR; id_tin =
    []; id_in = []; id_tout = (w3_ty sz); id_out =
    ((coq_R x86_decl RDX) :: ((coq_R x86_decl RAX) :: ((coq_R x86_decl RCX) :: [])));
    id_semi = (Obj.magic (Error ErrSemUndef)); id_args_kinds = ([] :: []);
    id_nargs = O; id_str_jas = (pp_sz "RDTSCP" sz); id_safe =
    (ScFalse :: []); id_pp_asm =
    (pp_name_ty "rdtscp" (sz :: (sz :: (sz :: [])))) }), ("RDTSCP",
    (prim_32_64 (fun x -> RDTSCP x))))

(** val coq_Ox86_CLFLUSH_instr :
    (register, register_ext, xmm_register, rflag, condt)
    instr_desc_t * (string * x86_op prim_constructor) **)

let coq_Ox86_CLFLUSH_instr =
  ({ id_valid = true; id_msb_flag = MSB_CLEAR; id_tin = ((Coq_lword
    (arch_pd x86_decl)) :: []); id_in = ((coq_Ec x86_decl O) :: []);
    id_tout = []; id_out = []; id_semi =
    (sem_prod_ok (map eval_ltype ((Coq_lword (arch_pd x86_decl)) :: []))
      (Obj.magic (fun _ -> ()))); id_args_kinds = (((m true) :: []) :: []);
    id_nargs = (S O); id_str_jas = (pp_s "CLFLUSH"); id_safe = [];
    id_pp_asm = (pp_name "clflush" U8) }, ("CLFLUSH", (primM CLFLUSH)))

(** val coq_Ox86_PREFETCHT0_instr :
    (register, register_ext, xmm_register, rflag, condt)
    instr_desc_t * (string * x86_op prim_constructor) **)

let coq_Ox86_PREFETCHT0_instr =
  ({ id_valid = true; id_msb_flag = MSB_CLEAR; id_tin = ((Coq_lword
    (arch_pd x86_decl)) :: []); id_in = ((coq_Ec x86_decl O) :: []);
    id_tout = []; id_out = []; id_semi =
    (sem_prod_ok (map eval_ltype ((Coq_lword (arch_pd x86_decl)) :: []))
      (Obj.magic (fun _ -> ()))); id_args_kinds = (((m true) :: []) :: []);
    id_nargs = (S O); id_str_jas = (pp_s "PREFETCHT0"); id_safe = [];
    id_pp_asm = (pp_name "prefetcht0" U8) }, ("PREFETCHT0",
    (primM PREFETCHT0)))

(** val coq_Ox86_PREFETCHT1_instr :
    (register, register_ext, xmm_register, rflag, condt)
    instr_desc_t * (string * x86_op prim_constructor) **)

let coq_Ox86_PREFETCHT1_instr =
  ({ id_valid = true; id_msb_flag = MSB_CLEAR; id_tin = ((Coq_lword
    (arch_pd x86_decl)) :: []); id_in = ((coq_Ec x86_decl O) :: []);
    id_tout = []; id_out = []; id_semi =
    (sem_prod_ok (map eval_ltype ((Coq_lword (arch_pd x86_decl)) :: []))
      (Obj.magic (fun _ -> ()))); id_args_kinds = (((m true) :: []) :: []);
    id_nargs = (S O); id_str_jas = (pp_s "PREFETCHT1"); id_safe = [];
    id_pp_asm = (pp_name "prefetcht1" U8) }, ("PREFETCHT1",
    (primM PREFETCHT1)))

(** val coq_Ox86_PREFETCHT2_instr :
    (register, register_ext, xmm_register, rflag, condt)
    instr_desc_t * (string * x86_op prim_constructor) **)

let coq_Ox86_PREFETCHT2_instr =
  ({ id_valid = true; id_msb_flag = MSB_CLEAR; id_tin = ((Coq_lword
    (arch_pd x86_decl)) :: []); id_in = ((coq_Ec x86_decl O) :: []);
    id_tout = []; id_out = []; id_semi =
    (sem_prod_ok (map eval_ltype ((Coq_lword (arch_pd x86_decl)) :: []))
      (Obj.magic (fun _ -> ()))); id_args_kinds = (((m true) :: []) :: []);
    id_nargs = (S O); id_str_jas = (pp_s "PREFETCHT2"); id_safe = [];
    id_pp_asm = (pp_name "prefetcht2" U8) }, ("PREFETCHT2",
    (primM PREFETCHT2)))

(** val coq_Ox86_PREFETCHNTA_instr :
    (register, register_ext, xmm_register, rflag, condt)
    instr_desc_t * (string * x86_op prim_constructor) **)

let coq_Ox86_PREFETCHNTA_instr =
  ({ id_valid = true; id_msb_flag = MSB_CLEAR; id_tin = ((Coq_lword
    (arch_pd x86_decl)) :: []); id_in = ((coq_Ec x86_decl O) :: []);
    id_tout = []; id_out = []; id_semi =
    (sem_prod_ok (map eval_ltype ((Coq_lword (arch_pd x86_decl)) :: []))
      (Obj.magic (fun _ -> ()))); id_args_kinds = (((m true) :: []) :: []);
    id_nargs = (S O); id_str_jas = (pp_s "PREFETCHNTA"); id_safe = [];
    id_pp_asm = (pp_name "prefetchnta" U8) }, ("PREFETCHNTA",
    (primM PREFETCHNTA)))

(** val coq_Ox86_LFENCE_instr :
    (register, register_ext, xmm_register, rflag, condt)
    instr_desc_t * (string * x86_op prim_constructor) **)

let coq_Ox86_LFENCE_instr =
  ({ id_valid = true; id_msb_flag = MSB_CLEAR; id_tin = []; id_in = [];
    id_tout = []; id_out = []; id_semi =
    (sem_prod_ok (map eval_ltype []) (Obj.magic ())); id_args_kinds =
    ([] :: []); id_nargs = O; id_str_jas = (pp_s "LFENCE"); id_safe = [];
    id_pp_asm = (pp_name "lfence" U8) }, ("LFENCE", (primM LFENCE)))

(** val coq_Ox86_MFENCE_instr :
    (register, register_ext, xmm_register, rflag, condt)
    instr_desc_t * (string * x86_op prim_constructor) **)

let coq_Ox86_MFENCE_instr =
  ({ id_valid = true; id_msb_flag = MSB_CLEAR; id_tin = []; id_in = [];
    id_tout = []; id_out = []; id_semi =
    (sem_prod_ok (map eval_ltype []) (Obj.magic ())); id_args_kinds =
    ([] :: []); id_nargs = O; id_str_jas = (pp_s "MFENCE"); id_safe = [];
    id_pp_asm = (pp_name "mfence" U8) }, ("MFENCE", (primM MFENCE)))

(** val coq_Ox86_SFENCE_instr :
    (register, register_ext, xmm_register, rflag, condt)
    instr_desc_t * (string * x86_op prim_constructor) **)

let coq_Ox86_SFENCE_instr =
  ({ id_valid = true; id_msb_flag = MSB_CLEAR; id_tin = []; id_in = [];
    id_tout = []; id_out = []; id_semi =
    (sem_prod_ok (map eval_ltype []) (Obj.magic ())); id_args_kinds =
    ([] :: []); id_nargs = O; id_str_jas = (pp_s "SFENCE"); id_safe = [];
    id_pp_asm = (pp_name "sfence" U8) }, ("SFENCE", (primM SFENCE)))

(** val x86_AESDEC : GRing.ComRing.sort -> GRing.ComRing.sort -> sem_tuple **)

let x86_AESDEC =
  wAESDEC

(** val x86_AESDECLAST :
    GRing.ComRing.sort -> GRing.ComRing.sort -> sem_tuple **)

let x86_AESDECLAST =
  wAESDECLAST

(** val x86_AESENC : GRing.ComRing.sort -> GRing.ComRing.sort -> sem_tuple **)

let x86_AESENC =
  wAESENC

(** val x86_AESENCLAST :
    GRing.ComRing.sort -> GRing.ComRing.sort -> sem_tuple **)

let x86_AESENCLAST =
  wAESENCLAST

(** val x86_AESIMC : GRing.ComRing.sort -> sem_tuple **)

let x86_AESIMC =
  coq_InvMixColumns

(** val x86_AESKEYGENASSIST :
    GRing.ComRing.sort -> GRing.ComRing.sort -> sem_tuple **)

let x86_AESKEYGENASSIST =
  wAESKEYGENASSIST

(** val mk_instr_aes2 :
    string -> string -> x86_op -> sem_tuple sem_prod -> msb_flag ->
    (register, register_ext, xmm_register, rflag, condt)
    instr_desc_t * (string * x86_op prim_constructor) **)

let mk_instr_aes2 jname aname constr x86_sem msb_flag0 =
  ({ id_valid = true; id_msb_flag = msb_flag0; id_tin = (w2_ty U128 U128);
    id_in = ((coq_Eu x86_decl O) :: ((coq_Eu x86_decl (S O)) :: []));
    id_tout = (w_ty U128); id_out = ((coq_Eu x86_decl O) :: []); id_semi =
    (sem_prod_ok (map eval_ltype (w2_ty U128 U128)) x86_sem); id_args_kinds =
    (check_xmm_xmmm U128); id_nargs = (S (S O)); id_str_jas = (pp_s jname);
    id_safe = []; id_pp_asm = (pp_name_ty aname (U128 :: (U128 :: []))) },
    (jname, (primM constr)))

(** val mk_instr_aes3 :
    string -> string -> (wsize -> x86_op) -> (GRing.ComRing.sort ->
    GRing.ComRing.sort -> GRing.ComRing.sort) -> (wsize -> (register,
    register_ext, xmm_register, rflag, condt)
    instr_desc_t) * (string * x86_op prim_constructor) **)

let mk_instr_aes3 jname aname constr x86_sem =
  ((fun sz -> { id_valid = (size_128_256 sz); id_msb_flag = MSB_CLEAR;
    id_tin = (w2_ty sz sz); id_in =
    ((coq_Eu x86_decl (S O)) :: ((coq_Eu x86_decl (S (S O))) :: []));
    id_tout = (w_ty sz); id_out = ((coq_Eu x86_decl O) :: []); id_semi =
    (sem_prod_ok (map eval_ltype (w2_ty sz sz))
      (Obj.magic lift2_vec U128 x86_sem sz)); id_args_kinds =
    (check_xmm_xmm_xmmm sz); id_nargs = (S (S (S O))); id_str_jas =
    (pp_sz jname sz); id_safe = []; id_pp_asm =
    (pp_name_ty aname (sz :: (sz :: (sz :: [])))) }), (jname,
    (prim_128_256 constr)))

(** val coq_Ox86_AESDEC_instr :
    (register, register_ext, xmm_register, rflag, condt)
    instr_desc_t * (string * x86_op prim_constructor) **)

let coq_Ox86_AESDEC_instr =
  mk_instr_aes2 "AESDEC" "aesdec" AESDEC (Obj.magic x86_AESDEC) MSB_MERGE

(** val coq_Ox86_VAESDEC_instr :
    (wsize -> (register, register_ext, xmm_register, rflag, condt)
    instr_desc_t) * (string * x86_op prim_constructor) **)

let coq_Ox86_VAESDEC_instr =
  mk_instr_aes3 "VAESDEC" "vaesdec" (fun x -> VAESDEC x) wAESDEC

(** val coq_Ox86_AESDECLAST_instr :
    (register, register_ext, xmm_register, rflag, condt)
    instr_desc_t * (string * x86_op prim_constructor) **)

let coq_Ox86_AESDECLAST_instr =
  mk_instr_aes2 "AESDECLAST" "aesdeclast" AESDECLAST
    (Obj.magic x86_AESDECLAST) MSB_MERGE

(** val coq_Ox86_VAESDECLAST_instr :
    (wsize -> (register, register_ext, xmm_register, rflag, condt)
    instr_desc_t) * (string * x86_op prim_constructor) **)

let coq_Ox86_VAESDECLAST_instr =
  mk_instr_aes3 "VAESDECLAST" "vaesdeclast" (fun x -> VAESDECLAST x)
    wAESDECLAST

(** val coq_Ox86_AESENC_instr :
    (register, register_ext, xmm_register, rflag, condt)
    instr_desc_t * (string * x86_op prim_constructor) **)

let coq_Ox86_AESENC_instr =
  mk_instr_aes2 "AESENC" "aesenc" AESENC (Obj.magic x86_AESENC) MSB_MERGE

(** val coq_Ox86_VAESENC_instr :
    (wsize -> (register, register_ext, xmm_register, rflag, condt)
    instr_desc_t) * (string * x86_op prim_constructor) **)

let coq_Ox86_VAESENC_instr =
  mk_instr_aes3 "VAESENC" "vaesenc" (fun x -> VAESENC x) wAESENC

(** val coq_Ox86_AESENCLAST_instr :
    (register, register_ext, xmm_register, rflag, condt)
    instr_desc_t * (string * x86_op prim_constructor) **)

let coq_Ox86_AESENCLAST_instr =
  mk_instr_aes2 "AESENCLAST" "aesenclast" AESENCLAST
    (Obj.magic x86_AESENCLAST) MSB_MERGE

(** val coq_Ox86_VAESENCLAST_instr :
    (wsize -> (register, register_ext, xmm_register, rflag, condt)
    instr_desc_t) * (string * x86_op prim_constructor) **)

let coq_Ox86_VAESENCLAST_instr =
  mk_instr_aes3 "VAESENCLAST" "vaesenclast" (fun x -> VAESENCLAST x)
    wAESENCLAST

(** val coq_Ox86_AESIMC_instr :
    (register, register_ext, xmm_register, rflag, condt)
    instr_desc_t * (string * x86_op prim_constructor) **)

let coq_Ox86_AESIMC_instr =
  ({ id_valid = true; id_msb_flag = MSB_MERGE; id_tin = (w_ty U128); id_in =
    ((coq_Eu x86_decl (S O)) :: []); id_tout = (w_ty U128); id_out =
    ((coq_Eu x86_decl O) :: []); id_semi =
    (sem_prod_ok (map eval_ltype (w_ty U128)) (Obj.magic x86_AESIMC));
    id_args_kinds = (check_xmm_xmmm U128); id_nargs = (S (S O)); id_str_jas =
    (pp_s "AESIMC"); id_safe = []; id_pp_asm =
    (pp_name_ty "aesimc" (U128 :: (U128 :: []))) }, ("AESIMC",
    (primM AESIMC)))

(** val coq_Ox86_VAESIMC_instr :
    (register, register_ext, xmm_register, rflag, condt)
    instr_desc_t * (string * x86_op prim_constructor) **)

let coq_Ox86_VAESIMC_instr =
  ({ id_valid = true; id_msb_flag = MSB_CLEAR; id_tin = (w_ty U128); id_in =
    ((coq_Eu x86_decl (S O)) :: []); id_tout = (w_ty U128); id_out =
    ((coq_Eu x86_decl O) :: []); id_semi =
    (sem_prod_ok (map eval_ltype (w_ty U128)) (Obj.magic x86_AESIMC));
    id_args_kinds = (check_xmm_xmmm U128); id_nargs = (S (S O)); id_str_jas =
    (pp_s "VAESIMC"); id_safe = []; id_pp_asm =
    (pp_name_ty "vaesimc" (U128 :: (U128 :: []))) }, ("VAESIMC",
    (primM VAESIMC)))

(** val coq_Ox86_AESKEYGENASSIST_instr :
    (register, register_ext, xmm_register, rflag, condt)
    instr_desc_t * (string * x86_op prim_constructor) **)

let coq_Ox86_AESKEYGENASSIST_instr =
  ({ id_valid = true; id_msb_flag = MSB_MERGE; id_tin = (w2_ty U128 U8);
    id_in = ((coq_Eu x86_decl (S O)) :: ((coq_Eu x86_decl (S (S O))) :: []));
    id_tout = (w_ty U128); id_out = ((coq_Eu x86_decl O) :: []); id_semi =
    (sem_prod_ok (map eval_ltype (w2_ty U128 U8))
      (Obj.magic x86_AESKEYGENASSIST)); id_args_kinds =
    (check_xmm_xmmm_imm8 U128); id_nargs = (S (S (S O))); id_str_jas =
    (pp_s "AESKEYGENASSIST"); id_safe = []; id_pp_asm =
    (pp_name_ty "aeskeygenassist" (U128 :: (U128 :: (U8 :: [])))) },
    ("AESKEYGENASSIST", (primM AESKEYGENASSIST)))

(** val coq_Ox86_VAESKEYGENASSIST_instr :
    (register, register_ext, xmm_register, rflag, condt)
    instr_desc_t * (string * x86_op prim_constructor) **)

let coq_Ox86_VAESKEYGENASSIST_instr =
  ({ id_valid = true; id_msb_flag = MSB_CLEAR; id_tin = (w2_ty U128 U8);
    id_in = ((coq_Eu x86_decl (S O)) :: ((coq_Eu x86_decl (S (S O))) :: []));
    id_tout = (w_ty U128); id_out = ((coq_Eu x86_decl O) :: []); id_semi =
    (sem_prod_ok (map eval_ltype (w2_ty U128 U8))
      (Obj.magic x86_AESKEYGENASSIST)); id_args_kinds =
    (check_xmm_xmmm_imm8 U128); id_nargs = (S (S (S O))); id_str_jas =
    (pp_s "VAESKEYGENASSIST"); id_safe = []; id_pp_asm =
    (pp_name_ty "vaeskeygenassist" (U128 :: (U128 :: (U8 :: [])))) },
    ("VAESKEYGENASSIST", (primM VAESKEYGENASSIST)))

(** val wclmulq :
    GRing.ComRing.sort -> GRing.ComRing.sort -> GRing.ComRing.sort **)

let wclmulq x1 x2 =
  let x = zero_extend U128 U64 x1 in
  foldr (fun k r0 ->
    Word0.wxor U128
      (if wbit_n U64 x2 k
       then wshl U128 x (Z.of_nat k)
       else GRing.zero
              (GRing.ComRing.Exports.coq_GRing_ComRing__to__GRing_Nmodule
                (word U128))) r0)
    (GRing.zero
      (GRing.ComRing.Exports.coq_GRing_ComRing__to__GRing_Nmodule (word U128)))
    (iota O (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S
      (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S
      (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S
      O)))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))

(** val wVPCLMULDQD :
    wsize -> GRing.ComRing.sort -> GRing.ComRing.sort -> GRing.ComRing.sort
    -> GRing.ComRing.sort **)

let wVPCLMULDQD sz w1 w2 k =
  let get1 = fun w ->
    nth
      (Obj.magic GRing.zero
        (GRing.ComRing.Exports.coq_GRing_ComRing__to__GRing_Nmodule
          (word U64))) (split_vec U128 (nat_of_wsize U64) w)
      (nat_of_bool (wbit_n U8 k O))
  in
  let get2 = fun w ->
    nth
      (Obj.magic GRing.zero
        (GRing.ComRing.Exports.coq_GRing_ComRing__to__GRing_Nmodule
          (word U64))) (split_vec U128 (nat_of_wsize U64) w)
      (nat_of_bool (wbit_n U8 k (S (S (S (S O))))))
  in
  let f = fun w3 w4 -> wclmulq (Obj.magic get1 w3) (Obj.magic get2 w4) in
  make_vec U128 sz
    (map2 (Obj.magic f) (split_vec sz (nat_of_wsize U128) w1)
      (split_vec sz (nat_of_wsize U128) w2))

(** val x86_VPCLMULQDQ :
    wsize -> GRing.ComRing.sort -> GRing.ComRing.sort -> GRing.ComRing.sort
    -> sem_tuple **)

let x86_VPCLMULQDQ =
  wVPCLMULDQD

(** val coq_Ox86_PCLMULQDQ_instr :
    (register, register_ext, xmm_register, rflag, condt)
    instr_desc_t * (string * x86_op prim_constructor) **)

let coq_Ox86_PCLMULQDQ_instr =
  ({ id_valid = true; id_msb_flag = MSB_CLEAR; id_tin = ((Coq_lword
    U128) :: ((Coq_lword U128) :: ((Coq_lword U8) :: []))); id_in =
    ((coq_Eu x86_decl O) :: ((coq_Eu x86_decl (S O)) :: ((coq_Eu x86_decl (S
                                                           (S O))) :: [])));
    id_tout = (w_ty U128); id_out = ((coq_Eu x86_decl O) :: []); id_semi =
    (sem_prod_ok
      (map eval_ltype ((Coq_lword U128) :: ((Coq_lword U128) :: ((Coq_lword
        U8) :: [])))) (Obj.magic x86_VPCLMULQDQ U128)); id_args_kinds =
    (check_xmm_xmmm_imm8 U128); id_nargs = (S (S (S O))); id_str_jas =
    (pp_s "PCLMULQDQ"); id_safe = []; id_pp_asm =
    (pp_name_ty "pclmulqdq" (U128 :: (U128 :: (U8 :: [])))) }, ("PCLMULQDQ",
    (primM PCLMULQDQ)))

(** val coq_Ox86_VPCLMULQDQ_instr :
    (wsize -> (register, register_ext, xmm_register, rflag, condt)
    instr_desc_t) * (string * x86_op prim_constructor) **)

let coq_Ox86_VPCLMULQDQ_instr =
  ((fun sz -> { id_valid = (size_128_256 sz); id_msb_flag = MSB_CLEAR;
    id_tin = ((Coq_lword sz) :: ((Coq_lword sz) :: ((Coq_lword U8) :: [])));
    id_in =
    ((coq_Eu x86_decl (S O)) :: ((coq_Eu x86_decl (S (S O))) :: ((coq_Eu
                                                                   x86_decl
                                                                   (S (S (S
                                                                   O)))) :: [])));
    id_tout = (w_ty sz); id_out = ((coq_Eu x86_decl O) :: []); id_semi =
    (sem_prod_ok
      (map eval_ltype ((Coq_lword sz) :: ((Coq_lword sz) :: ((Coq_lword
        U8) :: [])))) (Obj.magic x86_VPCLMULQDQ sz)); id_args_kinds =
    (check_xmm_xmm_xmmm_imm8 sz); id_nargs = (S (S (S (S O)))); id_str_jas =
    (pp_sz "VPCLMULQDQ" sz); id_safe = []; id_pp_asm =
    (pp_name "vpclmulqdq" sz) }), ("VPCLMULQDQ",
    (prim_128_256 (fun x -> VPCLMULQDQ x))))

(** val coq_Ox86_SHA256RNDS2_instr :
    (register, register_ext, xmm_register, rflag, condt)
    instr_desc_t * (string * x86_op prim_constructor) **)

let coq_Ox86_SHA256RNDS2_instr =
  ({ id_valid = true; id_msb_flag = MSB_MERGE; id_tin = (w3_ty U128); id_in =
    ((coq_Eu x86_decl O) :: ((coq_Eu x86_decl (S O)) :: ((Arch_decl.ADExplicit
    ((AK_mem Unaligned), (S (S O)), (ACR_vector XMM0))) :: []))); id_tout =
    (w_ty U128); id_out = ((coq_Eu x86_decl O) :: []); id_semi =
    (sem_prod_ok (map eval_ltype (w3_ty U128)) (Obj.magic sha256rnds2));
    id_args_kinds = ((xmm :: ((xmmm true) :: (xmm :: []))) :: []); id_nargs =
    (S (S (S O))); id_str_jas = (pp_s "SHA256RNDS2"); id_safe = [];
    id_pp_asm =
    (pp_name_ty "sha256rnds2" (U128 :: (U128 :: (U128 :: [])))) },
    ("SHA256RNDS2", (primM SHA256RNDS2)))

(** val coq_Ox86_SHA256MSG1_instr :
    (register, register_ext, xmm_register, rflag, condt)
    instr_desc_t * (string * x86_op prim_constructor) **)

let coq_Ox86_SHA256MSG1_instr =
  ({ id_valid = true; id_msb_flag = MSB_MERGE; id_tin = (w2_ty U128 U128);
    id_in = ((coq_Eu x86_decl O) :: ((coq_Eu x86_decl (S O)) :: []));
    id_tout = (w_ty U128); id_out = ((coq_Eu x86_decl O) :: []); id_semi =
    (sem_prod_ok (map eval_ltype (w2_ty U128 U128)) (Obj.magic sha256msg1));
    id_args_kinds = (check_xmm_xmmm U128); id_nargs = (S (S O)); id_str_jas =
    (pp_s "SHA256MSG1"); id_safe = []; id_pp_asm =
    (pp_name_ty "sha256msg1" (U128 :: (U128 :: []))) }, ("SHA256MSG1",
    (primM SHA256MSG1)))

(** val coq_Ox86_SHA256MSG2_instr :
    (register, register_ext, xmm_register, rflag, condt)
    instr_desc_t * (string * x86_op prim_constructor) **)

let coq_Ox86_SHA256MSG2_instr =
  ({ id_valid = true; id_msb_flag = MSB_MERGE; id_tin = (w2_ty U128 U128);
    id_in = ((coq_Eu x86_decl O) :: ((coq_Eu x86_decl (S O)) :: []));
    id_tout = (w_ty U128); id_out = ((coq_Eu x86_decl O) :: []); id_semi =
    (sem_prod_ok (map eval_ltype (w2_ty U128 U128)) (Obj.magic sha256msg2));
    id_args_kinds = (check_xmm_xmmm U128); id_nargs = (S (S O)); id_str_jas =
    (pp_s "SHA256MSG2"); id_safe = []; id_pp_asm =
    (pp_name_ty "sha256msg2" (U128 :: (U128 :: []))) }, ("SHA256MSG2",
    (primM SHA256MSG2)))

(** val x86_instr_desc :
    x86_op -> (register, register_ext, xmm_register, rflag, condt)
    instr_desc_t **)

let x86_instr_desc = function
| MOV sz -> fst coq_Ox86_MOV_instr sz
| MOVSX (sz, sz') -> fst coq_Ox86_MOVSX_instr sz sz'
| MOVZX (sz, sz') -> fst coq_Ox86_MOVZX_instr sz sz'
| CMOVcc sz -> fst coq_Ox86_CMOVcc_instr sz
| XCHG sz -> fst coq_Ox86_XCHG_instr sz
| ADD sz -> fst coq_Ox86_ADD_instr sz
| SUB sz -> fst coq_Ox86_SUB_instr sz
| MUL sz -> fst coq_Ox86_MUL_instr sz
| IMUL sz -> fst coq_Ox86_IMUL_instr sz
| IMULr sz -> fst coq_Ox86_IMULr_instr sz
| IMULri sz -> fst coq_Ox86_IMULri_instr sz
| DIV sz -> fst coq_Ox86_DIV_instr sz
| IDIV sz -> fst coq_Ox86_IDIV_instr sz
| CQO sz -> fst coq_Ox86_CQO_instr sz
| ADC sz -> fst coq_Ox86_ADC_instr sz
| SBB sz -> fst coq_Ox86_SBB_instr sz
| NEG sz -> fst coq_Ox86_NEG_instr sz
| INC sz -> fst coq_Ox86_INC_instr sz
| DEC sz -> fst coq_Ox86_DEC_instr sz
| LZCNT sz -> fst coq_Ox86_LZCNT_instr sz
| TZCNT sz -> fst coq_Ox86_TZCNT_instr sz
| BSR sz -> fst coq_Ox86_BSR_instr sz
| SETcc -> fst coq_Ox86_SETcc_instr
| BT sz -> fst coq_Ox86_BT_instr sz
| CLC -> fst coq_Ox86_CLC_instr
| STC -> fst coq_Ox86_STC_instr
| LEA sz -> fst coq_Ox86_LEA_instr sz
| TEST sz -> fst coq_Ox86_TEST_instr sz
| CMP sz -> fst coq_Ox86_CMP_instr sz
| AND sz -> fst coq_Ox86_AND_instr sz
| ANDN sz -> fst coq_Ox86_ANDN_instr sz
| OR sz -> fst coq_Ox86_OR_instr sz
| XOR sz -> fst coq_Ox86_XOR_instr sz
| NOT sz -> fst coq_Ox86_NOT_instr sz
| ROR sz -> fst coq_Ox86_ROR_instr sz
| ROL sz -> fst coq_Ox86_ROL_instr sz
| RCR sz -> fst coq_Ox86_RCR_instr sz
| RCL sz -> fst coq_Ox86_RCL_instr sz
| SHL sz -> fst coq_Ox86_SHL_instr sz
| SHR sz -> fst coq_Ox86_SHR_instr sz
| SAL sz -> fst coq_Ox86_SAL_instr sz
| SAR sz -> fst coq_Ox86_SAR_instr sz
| SHLD sz -> fst coq_Ox86_SHLD_instr sz
| SHRD sz -> fst coq_Ox86_SHRD_instr sz
| RORX sz -> fst coq_Ox86_RORX_instr sz
| SARX sz -> fst coq_Ox86_SARX_instr sz
| SHRX sz -> fst coq_Ox86_SHRX_instr sz
| SHLX sz -> fst coq_Ox86_SHLX_instr sz
| MULX_lo_hi sz -> fst coq_Ox86_MULX_lo_hi_instr sz
| ADCX sz -> fst coq_Ox86_ADCX_instr sz
| ADOX sz -> fst coq_Ox86_ADOX_instr sz
| BSWAP sz -> fst coq_Ox86_BSWAP_instr sz
| POPCNT sz -> fst coq_Ox86_POPCNT_instr sz
| BTR sz -> fst coq_Ox86_BTR_instr sz
| BTS sz -> fst coq_Ox86_BTS_instr sz
| PEXT sz -> fst coq_Ox86_PEXT_instr sz
| PDEP sz -> fst coq_Ox86_PDEP_instr sz
| MOVX sz -> fst coq_Ox86_MOVX_instr sz
| POR -> fst coq_Ox86_POR_instr
| PADD (ve, sz) -> fst coq_Ox86_PADD_instr ve sz
| MOVD sz -> fst coq_Ox86_MOVD_instr sz
| MOVV sz -> fst coq_Ox86_MOVV_instr sz
| VMOV sz -> fst coq_Ox86_VMOV_instr sz
| VMOVDQA sz -> fst coq_Ox86_VMOVDQA_instr sz
| VMOVDQU sz -> fst coq_Ox86_VMOVDQU_instr sz
| VPMOVSX (ve, sz, ve', sz') -> fst coq_Ox86_VPMOVSX_instr ve sz ve' sz'
| VPMOVZX (ve, sz, ve', sz') -> fst coq_Ox86_VPMOVZX_instr ve sz ve' sz'
| VPAND sz -> fst coq_Ox86_VPAND_instr sz
| VPANDN sz -> fst coq_Ox86_VPANDN_instr sz
| VPOR sz -> fst coq_Ox86_VPOR_instr sz
| VPXOR sz -> fst coq_Ox86_VPXOR_instr sz
| VPADD (sz, sz') -> fst coq_Ox86_VPADD_instr sz sz'
| VPSUB (sz, sz') -> fst coq_Ox86_VPSUB_instr sz sz'
| VPAVG (sz, sz') -> fst coq_Ox86_VPAVG_instr sz sz'
| VPMULL (sz, sz') -> fst coq_Ox86_VPMULL_instr sz sz'
| VPMULH sz -> fst coq_Ox86_VPMULH_instr sz
| VPMULHU sz -> fst coq_Ox86_VPMULHU_instr sz
| VPMULHRS sz -> fst coq_Ox86_VPMULHRS_instr sz
| VPMUL sz -> fst coq_Ox86_VPMUL_instr sz
| VPMULU sz -> fst coq_Ox86_VPMULU_instr sz
| VPEXTR ve -> fst coq_Ox86_VPEXTR_instr ve
| VPINSR sz -> fst coq_Ox86_VPINSR_instr sz
| VPSLL (sz, sz') -> fst coq_Ox86_VPSLL_instr sz sz'
| VPSRL (sz, sz') -> fst coq_Ox86_VPSRL_instr sz sz'
| VPSRA (sz, sz') -> fst coq_Ox86_VPSRA_instr sz sz'
| VPSLLV (sz, sz') -> fst coq_Ox86_VPSLLV_instr sz sz'
| VPSRLV (sz, sz') -> fst coq_Ox86_VPSRLV_instr sz sz'
| VPSLLDQ sz -> fst coq_Ox86_VPSLLDQ_instr sz
| VPSRLDQ sz -> fst coq_Ox86_VPSRLDQ_instr sz
| VPSHUFB sz -> fst coq_Ox86_VPSHUFB_instr sz
| VPSHUFD sz -> fst coq_Ox86_VPSHUFD_instr sz
| VPSHUFHW sz -> fst coq_Ox86_VPSHUFHW_instr sz
| VPSHUFLW sz -> fst coq_Ox86_VPSHUFLW_instr sz
| VPBLEND (ve, sz) -> fst coq_Ox86_VPBLEND_instr ve sz
| BLENDV (ve, sz) -> fst coq_Ox86_BLENDV_instr ve sz
| VPACKUS (ve, sz) -> fst coq_Ox86_VPACKUS_instr ve sz
| VPACKSS (ve, sz) -> fst coq_Ox86_VPACKSS_instr ve sz
| VSHUFPS sz -> fst coq_Ox86_VSHUFPS_instr sz
| VPBROADCAST (sz, sz') -> fst coq_Ox86_VPBROADCAST_instr sz sz'
| VMOVSHDUP sz -> fst coq_Ox86_VMOVSHDUP_instr sz
| VMOVSLDUP sz -> fst coq_Ox86_VMOVSLDUP_instr sz
| VPALIGNR sz -> fst coq_Ox86_VPALIGNR_instr sz
| VBROADCASTI128 -> fst coq_Ox86_VBROADCASTI128_instr
| VPUNPCKH (sz, sz') -> fst coq_Ox86_VPUNPCKH_instr sz sz'
| VPUNPCKL (sz, sz') -> fst coq_Ox86_VPUNPCKL_instr sz sz'
| VEXTRACTI128 -> fst coq_Ox86_VEXTRACTI128_instr
| VINSERTI128 -> fst coq_Ox86_VINSERTI128_instr
| VPERM2I128 -> fst coq_Ox86_VPERM2I128_instr
| VPERMD -> fst coq_Ox86_VPERMD_instr
| VPERMQ -> fst coq_Ox86_VPERMQ_instr
| MOVEMASK (ve, sz) -> fst coq_Ox86_MOVEMASK_instr ve sz
| VPCMPEQ (ve, sz) -> fst coq_Ox86_VPCMPEQ_instr ve sz
| VPCMPGT (ve, sz) -> fst coq_Ox86_VPCMPGT_instr ve sz
| VPSIGN (ve, sz) -> fst coq_Ox86_VPSIGN_instr ve sz
| VPMADDUBSW sz -> fst coq_Ox86_VPMADDUBSW_instr sz
| VPMADDWD sz -> fst coq_Ox86_VPMADDWD_instr sz
| VMOVLPD -> fst coq_Ox86_VMOVLPD_instr
| VMOVHPD -> fst coq_Ox86_VMOVHPD_instr
| VPMINU (ve, sz) -> fst coq_Ox86_VPMINU_instr ve sz
| VPMINS (ve, sz) -> fst coq_Ox86_VPMINS_instr ve sz
| VPMAXU (ve, sz) -> fst coq_Ox86_VPMAXU_instr ve sz
| VPMAXS (ve, sz) -> fst coq_Ox86_VPMAXS_instr ve sz
| VPABS (ve, sz) -> fst coq_Ox86_VPABS_instr ve sz
| VPTEST sz -> fst coq_Ox86_VPTEST_instr sz
| CLFLUSH -> fst coq_Ox86_CLFLUSH_instr
| PREFETCHT0 -> fst coq_Ox86_PREFETCHT0_instr
| PREFETCHT1 -> fst coq_Ox86_PREFETCHT1_instr
| PREFETCHT2 -> fst coq_Ox86_PREFETCHT2_instr
| PREFETCHNTA -> fst coq_Ox86_PREFETCHNTA_instr
| LFENCE -> fst coq_Ox86_LFENCE_instr
| MFENCE -> fst coq_Ox86_MFENCE_instr
| SFENCE -> fst coq_Ox86_SFENCE_instr
| RDTSC sz -> fst coq_Ox86_RDTSC_instr sz
| RDTSCP sz -> fst coq_Ox86_RDTSCP_instr sz
| AESDEC -> fst coq_Ox86_AESDEC_instr
| VAESDEC sz -> fst coq_Ox86_VAESDEC_instr sz
| AESDECLAST -> fst coq_Ox86_AESDECLAST_instr
| VAESDECLAST sz -> fst coq_Ox86_VAESDECLAST_instr sz
| AESENC -> fst coq_Ox86_AESENC_instr
| VAESENC sz -> fst coq_Ox86_VAESENC_instr sz
| AESENCLAST -> fst coq_Ox86_AESENCLAST_instr
| VAESENCLAST sz -> fst coq_Ox86_VAESENCLAST_instr sz
| AESIMC -> fst coq_Ox86_AESIMC_instr
| VAESIMC -> fst coq_Ox86_VAESIMC_instr
| AESKEYGENASSIST -> fst coq_Ox86_AESKEYGENASSIST_instr
| VAESKEYGENASSIST -> fst coq_Ox86_VAESKEYGENASSIST_instr
| PCLMULQDQ -> fst coq_Ox86_PCLMULQDQ_instr
| VPCLMULQDQ sz -> fst coq_Ox86_VPCLMULQDQ_instr sz
| SHA256RNDS2 -> fst coq_Ox86_SHA256RNDS2_instr
| SHA256MSG1 -> fst coq_Ox86_SHA256MSG1_instr
| SHA256MSG2 -> fst coq_Ox86_SHA256MSG2_instr

(** val x86_prim_string : (string * x86_op prim_constructor) list **)

let x86_prim_string =
  (snd coq_Ox86_MOV_instr) :: ((snd coq_Ox86_MOVSX_instr) :: ((snd
                                                                coq_Ox86_MOVZX_instr) :: (
    (snd coq_Ox86_CMOVcc_instr) :: ((snd coq_Ox86_XCHG_instr) :: ((snd
                                                                    coq_Ox86_BSWAP_instr) :: (
    (snd coq_Ox86_POPCNT_instr) :: ((snd coq_Ox86_BTR_instr) :: ((snd
                                                                   coq_Ox86_BTS_instr) :: (
    (snd coq_Ox86_PEXT_instr) :: ((snd coq_Ox86_PDEP_instr) :: ((snd
                                                                  coq_Ox86_CQO_instr) :: (
    (snd coq_Ox86_ADD_instr) :: ((snd coq_Ox86_SUB_instr) :: ((snd
                                                                coq_Ox86_MUL_instr) :: (
    (snd coq_Ox86_IMUL_instr) :: ((snd coq_Ox86_IMULr_instr) :: ((snd
                                                                   coq_Ox86_IMULri_instr) :: (
    (snd coq_Ox86_DIV_instr) :: ((snd coq_Ox86_IDIV_instr) :: ((snd
                                                                 coq_Ox86_ADC_instr) :: (
    (snd coq_Ox86_ADCX_instr) :: ((snd coq_Ox86_ADOX_instr) :: ((snd
                                                                  coq_Ox86_MULX_lo_hi_instr) :: (
    (snd coq_Ox86_SBB_instr) :: ((snd coq_Ox86_NEG_instr) :: ((snd
                                                                coq_Ox86_INC_instr) :: (
    (snd coq_Ox86_DEC_instr) :: ((snd coq_Ox86_LZCNT_instr) :: ((snd
                                                                  coq_Ox86_TZCNT_instr) :: (
    (snd coq_Ox86_BSR_instr) :: ((snd coq_Ox86_SETcc_instr) :: ((snd
                                                                  coq_Ox86_BT_instr) :: (
    (snd coq_Ox86_CLC_instr) :: ((snd coq_Ox86_STC_instr) :: ((snd
                                                                coq_Ox86_LEA_instr) :: (
    (snd coq_Ox86_TEST_instr) :: ((snd coq_Ox86_CMP_instr) :: ((snd
                                                                 coq_Ox86_AND_instr) :: (
    (snd coq_Ox86_ANDN_instr) :: ((snd coq_Ox86_OR_instr) :: ((snd
                                                                coq_Ox86_XOR_instr) :: (
    (snd coq_Ox86_NOT_instr) :: ((snd coq_Ox86_ROL_instr) :: ((snd
                                                                coq_Ox86_ROR_instr) :: (
    (snd coq_Ox86_RCL_instr) :: ((snd coq_Ox86_RCR_instr) :: ((snd
                                                                coq_Ox86_SHL_instr) :: (
    (snd coq_Ox86_SHR_instr) :: ((snd coq_Ox86_SAR_instr) :: ((snd
                                                                coq_Ox86_SAL_instr) :: (
    (snd coq_Ox86_SHLD_instr) :: ((snd coq_Ox86_SHRD_instr) :: ((snd
                                                                  coq_Ox86_RORX_instr) :: (
    (snd coq_Ox86_SARX_instr) :: ((snd coq_Ox86_SHRX_instr) :: ((snd
                                                                  coq_Ox86_SHLX_instr) :: (
    (snd coq_Ox86_MOVX_instr) :: ((snd coq_Ox86_POR_instr) :: ((snd
                                                                 coq_Ox86_PADD_instr) :: (
    (snd coq_Ox86_MOVD_instr) :: ((snd coq_Ox86_MOVV_instr) :: ((snd
                                                                  coq_Ox86_VMOV_instr) :: (
    (snd coq_Ox86_VPMOVSX_instr) :: ((snd coq_Ox86_VPMOVZX_instr) :: (
    (snd coq_Ox86_VPINSR_instr) :: ((snd coq_Ox86_VEXTRACTI128_instr) :: (
    (snd coq_Ox86_VMOVDQA_instr) :: ((snd coq_Ox86_VMOVDQU_instr) :: (
    (snd coq_Ox86_VPAND_instr) :: ((snd coq_Ox86_VPANDN_instr) :: ((snd
                                                                    coq_Ox86_VPOR_instr) :: (
    (snd coq_Ox86_VPXOR_instr) :: ((snd coq_Ox86_VPADD_instr) :: ((snd
                                                                    coq_Ox86_VPSUB_instr) :: (
    (snd coq_Ox86_VPAVG_instr) :: ((snd coq_Ox86_VPMULL_instr) :: ((snd
                                                                    coq_Ox86_VPMUL_instr) :: (
    (snd coq_Ox86_VPMULU_instr) :: ((snd coq_Ox86_VPMULH_instr) :: ((snd
                                                                    coq_Ox86_VPMULHU_instr) :: (
    (snd coq_Ox86_VPMULHRS_instr) :: ((snd coq_Ox86_VPSLL_instr) :: (
    (snd coq_Ox86_VPSRL_instr) :: ((snd coq_Ox86_VPSRA_instr) :: ((snd
                                                                    coq_Ox86_VPSLLV_instr) :: (
    (snd coq_Ox86_VPSRLV_instr) :: ((snd coq_Ox86_VPSLLDQ_instr) :: (
    (snd coq_Ox86_VPSRLDQ_instr) :: ((snd coq_Ox86_VPSHUFB_instr) :: (
    (snd coq_Ox86_VPSHUFHW_instr) :: ((snd coq_Ox86_VPSHUFLW_instr) :: (
    (snd coq_Ox86_VPSHUFD_instr) :: ((snd coq_Ox86_VSHUFPS_instr) :: (
    (snd coq_Ox86_VPUNPCKH_instr) :: ((snd coq_Ox86_VPUNPCKL_instr) :: (
    (snd coq_Ox86_VPBLEND_instr) :: ((snd coq_Ox86_BLENDV_instr) :: (
    (snd coq_Ox86_VPACKUS_instr) :: ((snd coq_Ox86_VPACKSS_instr) :: (
    (snd coq_Ox86_VPBROADCAST_instr) :: ((snd coq_Ox86_VMOVSHDUP_instr) :: (
    (snd coq_Ox86_VMOVSLDUP_instr) :: ((snd coq_Ox86_VPALIGNR_instr) :: (
    (snd coq_Ox86_VBROADCASTI128_instr) :: ((snd coq_Ox86_VPERM2I128_instr) :: (
    (snd coq_Ox86_VPERMD_instr) :: ((snd coq_Ox86_VPERMQ_instr) :: ((snd
                                                                    coq_Ox86_VINSERTI128_instr) :: (
    (snd coq_Ox86_VPEXTR_instr) :: ((snd coq_Ox86_MOVEMASK_instr) :: (
    (snd coq_Ox86_VPCMPEQ_instr) :: ((snd coq_Ox86_VPCMPGT_instr) :: (
    (snd coq_Ox86_VPSIGN_instr) :: ((snd coq_Ox86_VPMADDUBSW_instr) :: (
    (snd coq_Ox86_VPMADDWD_instr) :: ((snd coq_Ox86_VMOVLPD_instr) :: (
    (snd coq_Ox86_VMOVHPD_instr) :: ((snd coq_Ox86_VPMINU_instr) :: (
    (snd coq_Ox86_VPMINS_instr) :: ((snd coq_Ox86_VPMAXU_instr) :: ((snd
                                                                    coq_Ox86_VPMAXS_instr) :: (
    (snd coq_Ox86_VPABS_instr) :: ((snd coq_Ox86_VPTEST_instr) :: ((snd
                                                                    coq_Ox86_CLFLUSH_instr) :: (
    (snd coq_Ox86_PREFETCHT0_instr) :: ((snd coq_Ox86_PREFETCHT1_instr) :: (
    (snd coq_Ox86_PREFETCHT2_instr) :: ((snd coq_Ox86_PREFETCHNTA_instr) :: (
    (snd coq_Ox86_LFENCE_instr) :: ((snd coq_Ox86_MFENCE_instr) :: ((snd
                                                                    coq_Ox86_SFENCE_instr) :: (
    (snd coq_Ox86_RDTSC_instr) :: ((snd coq_Ox86_RDTSCP_instr) :: ((snd
                                                                    coq_Ox86_AESDEC_instr) :: (
    (snd coq_Ox86_VAESDEC_instr) :: ((snd coq_Ox86_AESDECLAST_instr) :: (
    (snd coq_Ox86_VAESDECLAST_instr) :: ((snd coq_Ox86_AESENC_instr) :: (
    (snd coq_Ox86_VAESENC_instr) :: ((snd coq_Ox86_AESENCLAST_instr) :: (
    (snd coq_Ox86_VAESENCLAST_instr) :: ((snd coq_Ox86_AESIMC_instr) :: (
    (snd coq_Ox86_VAESIMC_instr) :: ((snd coq_Ox86_AESKEYGENASSIST_instr) :: (
    (snd coq_Ox86_VAESKEYGENASSIST_instr) :: ((snd coq_Ox86_PCLMULQDQ_instr) :: (
    (snd coq_Ox86_VPCLMULQDQ_instr) :: ((snd coq_Ox86_SHA256RNDS2_instr) :: (
    (snd coq_Ox86_SHA256MSG1_instr) :: ((snd coq_Ox86_SHA256MSG2_instr) :: (("VPMAX",
    (primSV_8_32 (fun signedness0 ve sz ->
      match signedness0 with
      | Signed -> VPMAXS (ve, sz)
      | Unsigned -> VPMAXU (ve, sz)))) :: (("VPMIN",
    (primSV_8_32 (fun signedness0 ve sz ->
      match signedness0 with
      | Signed -> VPMINS (ve, sz)
      | Unsigned -> VPMINU (ve, sz)))) :: []))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))

(** val eqC_x86_op : x86_op eqTypeC **)

let eqC_x86_op =
  { beq = x86_op_eqb; ceqP = x86_op_eqb_OK }

(** val x86_op_decl :
    (register, register_ext, xmm_register, rflag, condt, x86_op) asm_op_decl **)

let x86_op_decl =
  { Arch_decl._eqT = eqC_x86_op; instr_desc_op = x86_instr_desc;
    Arch_decl.prim_string = x86_prim_string }

type x86_prog =
  (register, register_ext, xmm_register, rflag, condt, x86_op) asm_prog

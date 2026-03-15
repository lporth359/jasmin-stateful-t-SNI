
(* The type of tokens. *)

type token = 
  | WHILE
  | UNDERSCORE
  | UNALIGNED
  | T_W of (Syntax.swsize)
  | T_INT_CAST of (Syntax.sign)
  | T_INT
  | T_BOOL
  | TYPE
  | TRUE
  | TO
  | SWSIZE of (Syntax.swsize)
  | SVSIZE of (Syntax.svsize)
  | STRING of (string)
  | STAR
  | STACK
  | SLASH of (Syntax.sign option)
  | SHARP
  | SEMICOLON
  | RPAREN
  | ROR
  | ROL
  | RETURN
  | REQUIRE
  | REG
  | RBRACKET
  | RBRACE
  | RARROW
  | QUESTIONMARK
  | POINTER
  | PLUS
  | PIPEPIPE
  | PIPE
  | PERCENT of (Syntax.sign option)
  | PARAM
  | NID of (string)
  | NAMESPACE
  | MUTABLE
  | MINUS
  | LTLT
  | LT of (Syntax.sign option)
  | LPAREN
  | LE of (Syntax.sign option)
  | LBRACKET
  | LBRACE
  | INT of (Syntax.int_representation)
  | INLINE
  | IF
  | HAT
  | GTGT of (Syntax.sign option)
  | GT of (Syntax.sign option)
  | GLOBAL
  | GE of (Syntax.sign option)
  | FROM
  | FOR
  | FN
  | FALSE
  | EXPORT
  | EXEC
  | EQEQ
  | EQ
  | EOF
  | ELSE
  | DOWNTO
  | DOT
  | CONSTANT
  | COMMA
  | COLONCOLON
  | COLON
  | BANGEQ
  | BANG
  | ARRAYINIT
  | AMPAMP
  | AMP
  | ALIGNED

(* This exception is raised by the monolithic API functions. *)

exception Error

(* The monolithic API. *)

val module_: (Lexing.lexbuf -> token) -> Lexing.lexbuf -> (Syntax.pprogram)

module MenhirInterpreter : sig
  
  (* The incremental API. *)
  
  include MenhirLib.IncrementalEngine.INCREMENTAL_ENGINE
    with type token = token
  
  (* The indexed type of terminal symbols. *)
  
  type _ terminal = 
    | T_error : unit terminal
    | T_WHILE : unit terminal
    | T_UNDERSCORE : unit terminal
    | T_UNALIGNED : unit terminal
    | T_T_W : (Syntax.swsize) terminal
    | T_T_INT_CAST : (Syntax.sign) terminal
    | T_T_INT : unit terminal
    | T_T_BOOL : unit terminal
    | T_TYPE : unit terminal
    | T_TRUE : unit terminal
    | T_TO : unit terminal
    | T_SWSIZE : (Syntax.swsize) terminal
    | T_SVSIZE : (Syntax.svsize) terminal
    | T_STRING : (string) terminal
    | T_STAR : unit terminal
    | T_STACK : unit terminal
    | T_SLASH : (Syntax.sign option) terminal
    | T_SHARP : unit terminal
    | T_SEMICOLON : unit terminal
    | T_RPAREN : unit terminal
    | T_ROR : unit terminal
    | T_ROL : unit terminal
    | T_RETURN : unit terminal
    | T_REQUIRE : unit terminal
    | T_REG : unit terminal
    | T_RBRACKET : unit terminal
    | T_RBRACE : unit terminal
    | T_RARROW : unit terminal
    | T_QUESTIONMARK : unit terminal
    | T_POINTER : unit terminal
    | T_PLUS : unit terminal
    | T_PIPEPIPE : unit terminal
    | T_PIPE : unit terminal
    | T_PERCENT : (Syntax.sign option) terminal
    | T_PARAM : unit terminal
    | T_NID : (string) terminal
    | T_NAMESPACE : unit terminal
    | T_MUTABLE : unit terminal
    | T_MINUS : unit terminal
    | T_LTLT : unit terminal
    | T_LT : (Syntax.sign option) terminal
    | T_LPAREN : unit terminal
    | T_LE : (Syntax.sign option) terminal
    | T_LBRACKET : unit terminal
    | T_LBRACE : unit terminal
    | T_INT : (Syntax.int_representation) terminal
    | T_INLINE : unit terminal
    | T_IF : unit terminal
    | T_HAT : unit terminal
    | T_GTGT : (Syntax.sign option) terminal
    | T_GT : (Syntax.sign option) terminal
    | T_GLOBAL : unit terminal
    | T_GE : (Syntax.sign option) terminal
    | T_FROM : unit terminal
    | T_FOR : unit terminal
    | T_FN : unit terminal
    | T_FALSE : unit terminal
    | T_EXPORT : unit terminal
    | T_EXEC : unit terminal
    | T_EQEQ : unit terminal
    | T_EQ : unit terminal
    | T_EOF : unit terminal
    | T_ELSE : unit terminal
    | T_DOWNTO : unit terminal
    | T_DOT : unit terminal
    | T_CONSTANT : unit terminal
    | T_COMMA : unit terminal
    | T_COLONCOLON : unit terminal
    | T_COLON : unit terminal
    | T_BANGEQ : unit terminal
    | T_BANG : unit terminal
    | T_ARRAYINIT : unit terminal
    | T_AMPAMP : unit terminal
    | T_AMP : unit terminal
    | T_ALIGNED : unit terminal
  
  (* The indexed type of nonterminal symbols. *)
  
  type _ nonterminal = 
    | N_writable : (Syntax.writable) nonterminal
    | N_var : (Annotations.pident) nonterminal
    | N_utype_array : (Syntax.psizetype) nonterminal
    | N_utype : (Syntax.swsize) nonterminal
    | N_top_annotation : (Annotations.annotations) nonterminal
    | N_top : (Syntax.pitem) nonterminal
    | N_swsize : (Syntax.swsize) nonterminal
    | N_svsize : (Syntax.svsize) nonterminal
    | N_struct_annot : (Annotations.annotations) nonterminal
    | N_storage : (Syntax.pstorage) nonterminal
    | N_stor_type : (Syntax.pstotype) nonterminal
    | N_simple_attribute : (Annotations.simple_attribute) nonterminal
    | N_separated_nonempty_list_option_COMMA__var_ : (Annotations.pident list) nonterminal
    | N_separated_nonempty_list_empty_var_ : (Annotations.pident list) nonterminal
    | N_separated_nonempty_list_COMMA_var_ : (Annotations.pident list) nonterminal
    | N_separated_nonempty_list_COMMA_range_ : ((string * string) list) nonterminal
    | N_separated_nonempty_list_COMMA_plvalue_ : (Syntax.plvalue list) nonterminal
    | N_separated_nonempty_list_COMMA_pexpr_ : (Syntax.pexpr list) nonterminal
    | N_separated_nonempty_list_COMMA_loc_decl__ : ((Annotations.pident * Syntax.pexpr) Location.located list) nonterminal
    | N_separated_nonempty_list_COMMA_annotation_ : (Annotations.annotations) nonterminal
    | N_separated_nonempty_list_COMMA_annot_stor_type_ : ((Annotations.annotations * Syntax.pstotype) list) nonterminal
    | N_separated_nonempty_list_COMMA_annot_pparamdecl_ : ((Annotations.annotations * Syntax.paramdecls) list) nonterminal
    | N_separated_nonempty_list_COLONCOLON_NID_ : (string list) nonterminal
    | N_range : (string * string) nonterminal
    | N_ptype_r : (Syntax.ptype_r) nonterminal
    | N_ptype : (Syntax.ptype) nonterminal
    | N_ptr : (Syntax.ptr) nonterminal
    | N_prim : (Annotations.pident) nonterminal
    | N_prequire1 : (Syntax.prequire) nonterminal
    | N_prequire : (Annotations.pident option * Syntax.prequire list) nonterminal
    | N_pparamdecl_empty_ : (Syntax.paramdecls) nonterminal
    | N_pparam : (Syntax.pparam) nonterminal
    | N_pointer : (Syntax.writable option) nonterminal
    | N_plvalues : (Syntax.plvals) nonterminal
    | N_plvalue_r : (Syntax.plvalue_r) nonterminal
    | N_plvalue : (Syntax.plvalue) nonterminal
    | N_pinstr_r : (Syntax.pinstr_r) nonterminal
    | N_pinstr : (Syntax.pinstr) nonterminal
    | N_pif : (Syntax.pinstr_r) nonterminal
    | N_pglobal : (Syntax.pglobal) nonterminal
    | N_pgexpr : (Syntax.gpexpr) nonterminal
    | N_pfundef : (Syntax.pfundef) nonterminal
    | N_pfunbody : (Syntax.pfunbody) nonterminal
    | N_pexpr_r : (Syntax.pexpr_r) nonterminal
    | N_pexpr : (Syntax.pexpr) nonterminal
    | N_pexec : (Syntax.pexec) nonterminal
    | N_peqop : (Syntax.peqop) nonterminal
    | N_pelseif : (Syntax.pblock_r) nonterminal
    | N_pelse : (Syntax.pblock) nonterminal
    | N_pblock_r : (Syntax.pblock_r) nonterminal
    | N_pblock : (Syntax.pblock) nonterminal
    | N_option_writable_ : (Syntax.writable option) nonterminal
    | N_option_unaligned_ : ([ `Aligned | `Unaligned ] option) nonterminal
    | N_option_prefix_RARROW_tuple_annot_stor_type___ : ((Annotations.annotations * Syntax.pstotype) list option) nonterminal
    | N_option_prefix_IF_pexpr__ : (Syntax.pexpr option) nonterminal
    | N_option_pointer_ : (Syntax.writable option option) nonterminal
    | N_option_pblock_ : (Syntax.pblock option) nonterminal
    | N_option_loc_castop1__ : (Syntax.castop) nonterminal
    | N_option_from_ : (Annotations.pident option) nonterminal
    | N_option_call_conv_ : (Syntax.pcall_conv option) nonterminal
    | N_option_attribute_ : (Annotations.attribute option) nonterminal
    | N_option_arr_access_len_ : (Syntax.pexpr option) nonterminal
    | N_option_access_type_ : ((unit option * Syntax.swsize Location.located) option) nonterminal
    | N_option___anonymous_1_ : (Annotations.pident list option) nonterminal
    | N_option_DOT_ : (unit option) nonterminal
    | N_option_COMMA_ : (unit option) nonterminal
    | N_option_COLON_ : (unit option) nonterminal
    | N_nonempty_list_prequire1_ : (Syntax.prequire list) nonterminal
    | N_module_ : (Syntax.pprogram) nonterminal
    | N_loption_separated_nonempty_list_COMMA_var__ : (Annotations.pident list) nonterminal
    | N_loption_separated_nonempty_list_COMMA_range__ : ((string * string) list) nonterminal
    | N_loption_separated_nonempty_list_COMMA_pexpr__ : (Syntax.pexpr list) nonterminal
    | N_loption_separated_nonempty_list_COMMA_annotation__ : (Annotations.annotations) nonterminal
    | N_loption_separated_nonempty_list_COMMA_annot_stor_type__ : ((Annotations.annotations * Syntax.pstotype) list) nonterminal
    | N_loption_separated_nonempty_list_COMMA_annot_pparamdecl__ : ((Annotations.annotations * Syntax.paramdecls) list) nonterminal
    | N_list_top_annotation_ : (Annotations.annotations list) nonterminal
    | N_list_pinstr_ : (Syntax.pblock_r) nonterminal
    | N_list_loc_top__ : (Syntax.pprogram) nonterminal
    | N_keyword : (string) nonterminal
    | N_int : (Z.t) nonterminal
    | N_implicites : (Annotations.annotations Location.located) nonterminal
    | N_from : (Annotations.pident) nonterminal
    | N_castop1 : (Syntax.castop1) nonterminal
    | N_castop : (Syntax.castop) nonterminal
    | N_cast : (Syntax.cast) nonterminal
    | N_call_conv : (Syntax.pcall_conv) nonterminal
    | N_attribute : (Annotations.simple_attribute Location.located) nonterminal
    | N_arr_access_len : (Syntax.pexpr) nonterminal
    | N_arr_access_i : ((unit option * Syntax.swsize Location.located) option * Syntax.pexpr *
  Syntax.pexpr option * [ `Aligned | `Unaligned ] option) nonterminal
    | N_arr_access : (Warray_.arr_access *
  (Syntax.swsize Location.located option * Syntax.pexpr *
   Syntax.pexpr option * [ `Aligned | `Unaligned ] option)) nonterminal
    | N_annotations : (Annotations.annotations) nonterminal
    | N_annotationlabel : (Syntax.prequire) nonterminal
    | N_annotation : (Annotations.annotation) nonterminal
    | N_annot_stor_type : (Annotations.annotations * Syntax.pstotype) nonterminal
    | N_annot_pparamdecl : (Annotations.annotations * Syntax.paramdecls) nonterminal
  
  (* The inspection API. *)
  
  include MenhirLib.IncrementalEngine.INSPECTION
    with type 'a lr1state := 'a lr1state
    with type production := production
    with type 'a terminal := 'a terminal
    with type 'a nonterminal := 'a nonterminal
    with type 'a env := 'a env
  
end

(* The entry point(s) to the incremental API. *)

module Incremental : sig
  
  val module_: Lexing.position -> (Syntax.pprogram) MenhirInterpreter.checkpoint
  
end

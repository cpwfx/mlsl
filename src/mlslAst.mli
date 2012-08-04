(* File: mlslAst.mli *)

module Swizzle : sig
	exception Not_swizzle

	type component = 
	| X
	| Y
	| Z
	| W

	type t =
	| S1 of component
	| S2 of component * component
	| S3 of component * component * component
	| S4 of component * component * component * component

	val component_of_char : char -> component
	val of_string : string -> t
	val try_of_string : string -> t option
	val component_to_string : component -> string
	val to_string : t -> string

	val size : t -> int

	val component_id : component -> int
	val max_component_id : t -> int
end

type typ =
| TBool
| TFloat
| TInt
| TMat44
| TSampler2D
| TSamplerCube
| TUnit
| TVec2
| TVec3
| TVec4
| TArrow    of typ * typ
| TPair     of typ * typ
| TRecord   of (string * typ) list
| TVertex   of (string * typ) list
| TFragment of (string * typ) list
| TVertexTop

type typ_term =
	{ tt_pos : Lexing.position
	; tt_typ : typ
	}

type expr =
	{ e_pos  : Lexing.position
	; e_kind : expr_kind
	}
and expr_kind =
| EVar     of string
| EVarying of string
| EInt     of int
| ESwizzle of expr * Swizzle.t
| ERecord  of record_field_value list
| ESelect  of expr * string
| EPair    of expr * expr
| EMul     of expr * expr
| EApp     of expr * expr
and record_field_value =
	{ rfv_pos   : Lexing.position
	; rfv_name  : string
	; rfv_value : expr
	}

type attr_semantics =
	{ asem_name : string
	; asem_pos  : Lexing.position
	}

type topdef_kind =
| TDAttrDecl
	of string         (* name      *)
	*  attr_semantics (* semantics *)
	*  typ_term       (* type      *)
| TDConstDecl 
	of string         (* name *)
	*  typ_term       (* type *)
| TDSamplerDecl
	of string         (* name *)
	*  typ_term       (* type *)
| TDFragmentShader
	of string         (* name *)
	*  expr           (* body *)
| TDVertexShader
	of string         (* name *)
	*  expr           (* body *)
| TDShader
	of string         (* name *)
	*  expr           (* definition *)

type topdef =
	{ td_pos  : Lexing.position
	; td_kind : topdef_kind
	}

val make_expr   : Lexing.position -> expr_kind -> expr
val make_app    : expr -> expr list -> expr
val make_select : Lexing.position -> expr -> string -> expr

val is_reg_type     : typ -> bool
val is_data_type    : typ -> bool
val is_sampler_type : typ -> bool

val string_of_typ : int -> typ -> string

val foreachShader : topdef list -> (topdef -> string -> expr -> unit) -> unit

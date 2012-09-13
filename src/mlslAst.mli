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
| TMat of Misc.Dim.dim * Misc.Dim.dim
| TSampler2D
| TSamplerCube
| TUnit
| TVec of Misc.Dim.dim
| TArrow    of typ * typ
| TPair     of typ * typ
| TRecord   of (string * typ) list
| TVertex   of (string * typ) list
| TFragment of (string * typ) list
| TVertexTop

type typ_term =
	{ tt_pos : Errors.position
	; tt_typ : typ
	}

type pattern =
	{ p_pos  : Errors.position
	; p_kind : pattern_kind
	}
and pattern_kind =
| PAny
| PVar      of string
| PTypedVar of string * typ_term
| PTrue
| PFalse
| PPair     of pattern * pattern
| PConstrU  of string
| PConstrP  of string * pattern

type binop =
| BOEq
| BONeq
| BOLe
| BOLt
| BOGe
| BOGt
| BOAdd
| BOSub
| BOMul
| BODiv
| BOMod
| BODot
| BOCross
| BOJoin
| BOPow
(* Operators unavailabe from code (only from builtins) *)
| BOMin

type unop =
| UONeg
| UOPlus

type expr =
	{ e_pos  : Errors.position
	; e_kind : expr_kind
	}
and expr_kind =
| EVar      of string
| EVarying  of string
| EInt      of int
| ETrue
| EFalse
| EFloat    of float
| ESwizzle  of expr * Swizzle.t
| ERecord   of record_field_value list
| ESelect   of expr * string
| EPair     of expr * expr
| EBinOp    of binop * expr * expr
| EUnOp     of unop * expr
| EAbs      of pattern * expr
| EApp      of expr * expr
| ELet      of pattern * expr * expr
| EFix      of pattern * expr
| EIf       of expr * expr * expr
| EMatch    of expr * match_pattern list
| EFragment of expr
| EVertex   of expr
| EConstrU  of string
| EConstrP  of string * expr
and record_field_value =
	{ rfv_pos   : Errors.position
	; rfv_name  : string
	; rfv_value : expr
	}
and match_pattern =
	{ mp_patterns  : pattern list
	; mp_condition : expr option
	; mp_action    : expr
	}

type topdef_kind =
| TDAttrDecl
	of string         (* name      *)
	*  typ_term       (* type      *)
| TDConstDecl 
	of string         (* name *)
	*  typ_term       (* type *)
| TDSamplerDecl
	of string         (* name *)
	*  typ_term       (* type *)
| TDLocalDef
	of pattern        (* pattern *)
	*  expr           (* value   *)
| TDShader
	of string         (* name *)
	*  expr           (* definition *)

type topdef =
	{ td_pos  : Errors.position
	; td_kind : topdef_kind
	}

val make_pattern          : Lexing.position -> pattern_kind -> pattern
val make_list_pattern_rev : Lexing.position -> pattern list -> pattern

val make_expr     : Lexing.position -> expr_kind -> expr
val make_abs_rev  : Lexing.position -> pattern list -> expr -> expr
val make_app      : expr -> expr list -> expr
val make_select   : Lexing.position -> expr -> string -> expr
val make_let_rec  : Lexing.position -> (pattern * expr) list -> expr -> expr
val make_list_rev : Lexing.position -> expr list -> expr

val make_topdef_let_rec : Lexing.position -> (pattern * expr) list -> topdef

val is_reg_type     : typ -> bool
val is_data_type    : typ -> bool
val is_sampler_type : typ -> bool

val string_of_typ : int -> typ -> string
val binop_name : binop -> string
val unop_name  : unop -> string

val foreachShader : topdef list -> (topdef -> string -> expr -> unit) -> unit

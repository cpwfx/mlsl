(* File: mlslAst.mli *)

type typ =
| TBool
| TFloat
| TInt
| TMat44
| TUnit
| TVec2
| TVec3
| TVec4
| TArrow  of typ * typ
| TRecord of (string * typ) list
| TVertex of (string * typ) list

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
| ERecord  of record_field_value list
| EPair    of expr * expr
| EMul     of expr * expr
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

val make_expr : Lexing.position -> expr_kind -> expr

val is_reg_type  : typ -> bool
val is_data_type : typ -> bool

val string_of_typ : int -> typ -> string

(* File: mlslAst.ml *)

type typ =
| TFloat
| TInt
| TVec2
| TVec3
| TVec4
| TArrow of typ * typ

type typ_term =
	{ tt_pos : Lexing.position
	; tt_typ : typ
	}

type topdecl_type =
| TDConstDecl 
	of string   (* name *)
	*  typ_term (* type *)

type topdecl =
	{ td_pos  : Lexing.position
	; td_type : topdecl_type
	}

(* File: midlang.ml *)

type typ =
| TFloat
| TMat44
| TVec2
| TVec3
| TVec4

type variable =
	{ var_id   : int
	; var_typ  : typ
	}

type semantics =
| SPosition

type attr =
	{ attr_semantics : semantics
	; attr_name      : string
	; attr_var       : variable
	}

type param =
	{ param_name : string
	; param_var  : variable
	}

type instr =
| IMov of variable * variable

type shader =
	{ sh_name     : string
	; sh_attr     : attr list
	; sh_v_const  : param list
	; sh_f_const  : param list
	; sh_varying  : param list
	; sh_vertex   : instr list
	; sh_fragment : instr list
	}

let unfold_shader name expr =
	Errors.error_p expr.MlslAst.e_pos "Unimplemented: unfold_shader.";
	None

(* TODO: better optimizer *)
let optimize s = s

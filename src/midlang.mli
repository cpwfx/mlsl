(* File: midlang.mli *)

type typ =
| TFloat
| TInt
| TMat44
| TVec2
| TVec3
| TVec4

type variable_sort =
| VSAttribute
| VSConstant
| VSTemporary
| VSVarying

type variable =
	{ var_id   : int
	; var_typ  : typ
	; var_sort : variable_sort
	}

module Variable : sig
	type t = variable
	val compare : t -> t -> int
end

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

type dim =
| Dim2
| Dim3
| Dim4

type instr_kind =
| IMov     of variable * variable
| IMulFF   of variable * variable * variable
| IMulMV   of variable * variable * variable * dim * dim
| IMulVF   of variable * variable * variable * dim
| IRet     of variable
type instr =
	{ ins_id   : int
	; ins_kind : instr_kind
	}

val create_instr : instr_kind -> instr

type shader =
	{ sh_name     : string
	; sh_attr     : attr list
	; sh_v_const  : param list
	; sh_f_const  : param list
	; sh_varying  : param list
	; sh_vertex   : instr list
	; sh_fragment : instr list
	}

val unfold_shader : string -> MlslAst.expr -> shader option

val optimize : shader -> shader

val string_of_typ : typ -> string

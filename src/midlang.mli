(* File: midlang.mli *)

type dim =
| Dim2
| Dim3
| Dim4

val int_of_dim : dim -> int
val range_of_dim : dim -> int list
val dim_of_ast : MlslAst.dim -> dim

type typ =
| TFloat
| TInt
| TMat of dim * dim
| TVec of dim

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

type sampler_dim =
| SDim2D
| SDimCube

type sampler =
	{ sampler_id   : int
	; sampler_name : string
	; sampler_dim  : sampler_dim
	}

type semantics =
| SInput0
| SInput1
| SInput2
| SInput3
| SInput4
| SInput5
| SInput6
| SInput7
| SPosition
| STexcoord0
| STexcoord1
| STexcoord2
| STexcoord3

type attr =
	{ attr_semantics : semantics
	; attr_name      : string
	; attr_var       : variable
	}

type param =
	{ param_name : string
	; param_var  : variable
	}

type binop =
| BOAddF
| BOAddM  of dim * dim
| BOAddV  of dim
| BOSubF
| BOSubM  of dim * dim
| BOSubV  of dim
| BOMulFF
| BOMulMF of dim * dim
| BOMulMM of dim * dim * dim
| BOMulMV of dim * dim
| BOMulVF of dim
| BOMulVV of dim
| BODivFF
| BODivFV of dim
| BODivMF of dim * dim
| BODivVF of dim
| BODivVV of dim
| BOModFF
| BOModFV of dim
| BOModMF of dim * dim
| BOModVF of dim
| BOModVV of dim
| BODot   of dim
| BOCross2
| BOCross3
| BOPowFF
| BOPowVF of dim
| BOPowVV of dim

type unop =
| UONegF
| UONegM of dim * dim
| UONegV of dim

type instr_kind =
| IMov     of variable * variable
| IBinOp   of variable * variable * variable * binop
| IUnOp    of variable * variable * unop
| ISwizzle of variable * variable * MlslAst.Swizzle.t
| ITex     of variable * variable * sampler
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
	; sh_samplers : sampler list
	; sh_vertex   : instr list
	; sh_fragment : instr list
	}

val unfold_shader : string -> TopDef.value -> shader option

val optimize : shader -> shader

val string_of_typ : typ -> string

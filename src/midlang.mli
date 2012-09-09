(* File: midlang.mli *)

type typ =
| TBool
| TInt
| TFloat
| TMat of Misc.Dim.dim * Misc.Dim.dim
| TVec of Misc.Dim.dim

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

type conversion =
| CBool2Int
| CBool2Float
| CInt2Float

type binop =
| BOOrB
| BOAndB
| BOAddI
| BOAddF
| BOAddM  of Misc.Dim.dim * Misc.Dim.dim
| BOAddV  of Misc.Dim.dim
| BOSubI
| BOSubF
| BOSubM  of Misc.Dim.dim * Misc.Dim.dim
| BOSubV  of Misc.Dim.dim
| BOMulI
| BOMulFF
| BOMulMF of Misc.Dim.dim * Misc.Dim.dim
| BOMulMM of Misc.Dim.dim * Misc.Dim.dim * Misc.Dim.dim
| BOMulMV of Misc.Dim.dim * Misc.Dim.dim
| BOMulVF of Misc.Dim.dim
| BOMulVV of Misc.Dim.dim
| BODivI
| BODivFF
| BODivFV of Misc.Dim.dim
| BODivMF of Misc.Dim.dim * Misc.Dim.dim
| BODivVF of Misc.Dim.dim
| BODivVV of Misc.Dim.dim
| BOModI
| BOModFF
| BOModFV of Misc.Dim.dim
| BOModMF of Misc.Dim.dim * Misc.Dim.dim
| BOModVF of Misc.Dim.dim
| BOModVV of Misc.Dim.dim
| BODot   of Misc.Dim.dim
| BOCross2
| BOCross3
| BOPowI
| BOPowFF
| BOPowVF of Misc.Dim.dim
| BOPowVV of Misc.Dim.dim
| BOMinI
| BOMinF
| BOMinV  of Misc.Dim.dim

type unop =
| UONotB
| UONegI
| UONegF
| UONegM of Misc.Dim.dim * Misc.Dim.dim
| UONegV of Misc.Dim.dim

type instr_kind =
| IMov        of variable * variable
| IConstBool  of variable * bool
| IConstInt   of variable * int
| IConstFloat of variable * float
| IConstVec   of variable * Misc.Dim.dim * float array
| IConstMat   of variable * Misc.Dim.dim * Misc.Dim.dim * float array array
| IConvert    of variable * variable * conversion
| IBinOp      of variable * variable * variable * binop
| IUnOp       of variable * variable * unop
| ISwizzle    of variable * variable * MlslAst.Swizzle.t
| ITex        of variable * variable * sampler
| IRet        of variable
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

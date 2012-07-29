(* File: agal.mli *)

type variable_sort =
| VSAttribute
| VSConstant
| VSTemporary
| VSOutput
| VSVarying

type variable =
	{         var_id   : int
	;         var_size : int * int
	; mutable var_reg  : (int * int) option
	;         var_sort : variable_sort
	}

type attr =
	{ attr_name      : string
	; attr_var       : variable
	}

type const_named =
	{ cn_name : string
	; cn_var  : variable
	}

type const_value =
	{ cv_value : float array array
	; cv_var   : variable
	}

type const =
| ConstNamed of const_named
| ConstValue of const_value

type dest_mask =
	{ dmask_x : bool
	; dmask_y : bool
	; dmask_z : bool
	; dmask_w : bool
	}
type dest =
	{ dst_var  : variable
	; dst_row  : int
	; dst_mask : dest_mask
	}

type source_offset =
	{ srcoff_var       : variable
	; srcoff_row       : int
	; srcoff_component : int
	}

type source =
	{ src_var     : variable
	; src_row     : int
	; src_swizzle : int array
	; src_offset  : source_offset option
	}

type instr_kind =
| IMov of dest * source
| IMul of dest * source * source
| IM44 of dest * source * source
type instr =
	{ ins_id   : int
	; ins_kind : instr_kind
	}

type shader_globals =
	{ shg_attr    : attr list
	; shg_v_const : const list
	; shg_f_const : const list
	; shg_varying : variable list
	}

type shader =
	{ sh_name     : string
	; sh_glob     : shader_globals
	; sh_vertex   : instr list
	; sh_fragment : instr list
	}

val build : Midlang.shader -> shader option

val optimize : shader -> shader

val finalize : shader -> shader option

val write : shader -> unit -> unit

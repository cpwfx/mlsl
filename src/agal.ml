(* File: agal.ml *)

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
	{ cv_size  : int * int
	; cv_value : float array array
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

(* ========================================================================= *)

let var_map = Hashtbl.create 32

let fresh_var = Misc.Fresh.create ()

let map_variable var =
	try
		Hashtbl.find var_map var.Midlang.var_id
	with
	| Not_found -> begin
		let result =
			{ var_id   = Misc.Fresh.next fresh_var
			; var_size =
				begin match var.Midlang.var_typ with
				| Midlang.TFloat -> (1, 1)
				| Midlang.TInt   -> (1, 1)
				| Midlang.TMat44 -> (4, 4)
				| Midlang.TVec2  -> (1, 2)
				| Midlang.TVec3  -> (1, 3)
				| Midlang.TVec4  -> (1, 4)
				end
			; var_reg  = None
			; var_sort =
				begin match var.Midlang.var_sort with
				| Midlang.VSAttribute -> VSAttribute
				| Midlang.VSConstant  -> VSConstant
				| Midlang.VSTemporary -> VSTemporary
				| Midlang.VSVarying   -> VSVarying
				end
			} in
		Hashtbl.replace var_map var.Midlang.var_id result;
		result
	end

let map_attr_variable attr =
	let var = map_variable attr.Midlang.attr_var in
	begin match attr.Midlang.attr_semantics with
	| Midlang.SPosition -> var.var_reg <- Some (0, 0)
	end;
	var

let map_attr attr =
	{ attr_name = attr.Midlang.attr_name
	; attr_var  = map_attr_variable attr
	}

let map_const const =
	ConstNamed
		{ cn_name = const.Midlang.param_name
		; cn_var  = map_variable const.Midlang.param_var
		}

let map_varying var =
	map_variable var.Midlang.param_var

let map_globals sh =
	{ shg_attr    = List.map map_attr    sh.Midlang.sh_attr
	; shg_v_const = List.map map_const   sh.Midlang.sh_v_const
	; shg_f_const = List.map map_const   sh.Midlang.sh_f_const
	; shg_varying = List.map map_varying sh.Midlang.sh_varying
	}

let add_const globals vs_const fs_const =
	{ globals with
	  shg_v_const = globals.shg_v_const @ vs_const
	; shg_f_const = globals.shg_f_const @ fs_const
	}

let build_code globals code =
	Errors.error "Unimplemented: Agal.build_code.";
	None

let build sh =
	Hashtbl.clear var_map;
	let globals = map_globals sh in
	Misc.Opt.bind (build_code globals sh.Midlang.sh_vertex)   (fun (vs, vs_const) ->
	Misc.Opt.bind (build_code globals sh.Midlang.sh_fragment) (fun (fs, fs_const) ->
		Some
			{ sh_name     = sh.Midlang.sh_name
			; sh_glob     = add_const globals vs_const fs_const
			; sh_vertex   = vs
			; sh_fragment = fs
			}
	))

(* TODO: better optimizer *)
let optimize sh = sh

let finalize sh =
	Errors.error "Unimplemented: Agal.finalize.";
	None

let write sh () =
	Errors.error "Unimplemented: Agal.write."

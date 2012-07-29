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

(* ========================================================================= *)

let variable_of_const c =
	match c with
	| ConstNamed cn -> cn.cn_var
	| ConstValue cv -> cv.cv_var

(* ========================================================================= *)

let var_map = Hashtbl.create 32

let fresh_var = Misc.Fresh.create ()
let fresh_ins = Misc.Fresh.create ()

let map_variable var =
	try
		Hashtbl.find var_map var.Midlang.var_id
	with
	| Not_found -> begin
		let result =
			{ var_id   = Misc.Fresh.next fresh_var
			; var_size =
				begin match var.Midlang.var_typ with
				| Midlang.TFloat       -> (1, 1)
				| Midlang.TInt         -> (1, 1)
				| Midlang.TMat(d1, d2) -> (Midlang.int_of_dim d1, Midlang.int_of_dim d2)
				| Midlang.TVec d       -> (1, Midlang.int_of_dim d)
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

let dst_mask_float = { dmask_x = true; dmask_y = false; dmask_z = false; dmask_w = false }
let dst_mask_vec2  = { dmask_x = true; dmask_y = true;  dmask_z = false; dmask_w = false }
let dst_mask_vec3  = { dmask_x = true; dmask_y = true;  dmask_z = true;  dmask_w = false }
let dst_mask_vec4  = { dmask_x = true; dmask_y = true;  dmask_z = true;  dmask_w = true  }

let swizzle_nop () = [| 0; 1; 2; 3 |]

let make_dest_float reg =
	{ dst_var  = map_variable reg
	; dst_row  = 0
	; dst_mask = dst_mask_float
	}

let make_dest dim reg =
	{ dst_var  = map_variable reg
	; dst_row  = 0
	; dst_mask =
		begin match dim with
		| Midlang.Dim2 -> dst_mask_vec2
		| Midlang.Dim3 -> dst_mask_vec3
		| Midlang.Dim4 -> dst_mask_vec4
		end
	}

let make_dest_output () =
	{ dst_var  =
		{ var_id   = Misc.Fresh.next fresh_var
		; var_size = (1, 4)
		; var_reg  = Some (0, 0)
		; var_sort = VSOutput
		}
	; dst_row = 0
	; dst_mask = dst_mask_vec4
	}

let make_source reg =
	{ src_var     = map_variable reg
	; src_row     = 0
	; src_swizzle = swizzle_nop ()
	; src_offset  = None
	}

let make_source_float reg =
	{ src_var     = map_variable reg
	; src_row     = 0
	; src_swizzle = [| 0; 0; 0; 0 |]
	; src_offset  = None
	}

let create_instr kind =
	{ ins_id   = Misc.Fresh.next fresh_ins
	; ins_kind = kind
	}

let build_ins globals const ins =
	match ins.Midlang.ins_kind with
	| Midlang.IMov(dst, src) ->
		begin match dst.Midlang.var_typ with
		| Midlang.TFloat ->
			Some (create_instr (IMov(make_dest_float dst, make_source src)), const)
		| Midlang.TInt ->
			Some (create_instr (IMov(make_dest_float dst, make_source src)), const)
		| Midlang.TMat(d1, d2) ->
			Errors.error "Unimplemented: Agal.build_ins IMov mat.";
			None
		| Midlang.TVec dim ->
			Some (create_instr (IMov(make_dest dim dst, make_source src)), const)
		end
	| Midlang.IMulFF _ ->
		Errors.error "Unimplemented: Agal.build_ins IMulFF.";
		None
	| Midlang.IMulMV(rv, vm, vv, dim, Midlang.Dim4) ->
		Some (create_instr (IM44(make_dest dim rv, make_source vv, make_source vm)), const)
	| Midlang.IMulMV(_, _, _, d1, d2) ->
		Errors.error (Printf.sprintf "Unimplemented: Agal.build_ins IMulMV%d%d."
			(Midlang.int_of_dim d1) (Midlang.int_of_dim d2));
		None
	| Midlang.IMulVF(res, vec, flo, dim) ->
		Some (create_instr (IMul(make_dest dim res, make_source vec, make_source_float flo)), const)
	| Midlang.IRet reg ->
		Some (create_instr (IMov(make_dest_output (), make_source reg)), const)

let rec build_code' globals acc const code =
	match code with
	| [] -> Some (List.rev acc, const)
	| ins :: code ->
		Misc.Opt.bind (build_ins globals const ins) (fun (ins, const) ->
			build_code' globals (ins :: acc) const code
		)

let build_code globals code =
	build_code' globals [] [] code

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

(* ========================================================================= *)

module Variable = struct
	type t = variable
	let compare v1 v2 = compare v1.var_id v2.var_id
end

module VarSet = Set.Make(Variable)

module LiveVar = struct
	let entry_tab = Hashtbl.create 32
	let exit_tab = Hashtbl.create 32

	let transfer instr a =
		match instr.ins_kind with
		| IMov(dest, src) -> VarSet.add src.src_var a
		| IM44(dest, src1, src2) | IMul(dest, src1, src2) -> 
			VarSet.add src1.src_var (VarSet.add src2.src_var a)

	let rec compute_loop last code =
		match code with
		| [] -> ()
		| instr :: code ->
			Hashtbl.replace exit_tab instr.ins_id last;
			let next = transfer instr last in
			Hashtbl.replace entry_tab instr.ins_id next;
			compute_loop next code

	let compute code =
		compute_loop VarSet.empty (List.rev code)

	let is_alive ins var =
		VarSet.mem var (Hashtbl.find exit_tab ins.ins_id)
end

let rec bind_glob_registers' free name cnt vars =
	match vars with
	| [] -> true
	| v::vars ->
		v.var_reg <- Some (free, 0);
		let free = free + fst v.var_size in
		if free < cnt then
			bind_glob_registers' free name cnt vars
		else begin
			Errors.error (Printf.sprintf "I can't pack %s into %d registers"
				name cnt);
			false
		end

let bind_glob_registers name cnt vars =
	bind_glob_registers' 0 name cnt vars

let bind_register_var reg_map shader var =
	let (sz_reg, sz_fld) = var.var_size in
	let alloc_var reg fld =
		for ri = 0 to sz_reg - 1 do
			for fi = 0 to sz_fld - 1 do
				reg_map.(reg + ri).(fld + fi) <- var :: reg_map.(reg + ri).(fld + fi)
			done
		done
	in
	let rec can_alloc ri fi reg fld =
		if ri >= sz_reg then true
		else if fi >= sz_fld then
			can_alloc (ri + 1) 0 reg fld
		else
			(reg + ri < Array.length reg_map)
			&& (fld + fi < 4)
			&& (Misc.ListExt.is_empty reg_map.(reg + ri).(fld + fi))
			&& can_alloc ri (fi + 1) reg fld
	in
	let rec alloc_loop reg fld =
		if reg >= Array.length reg_map then begin
			Errors.error (Printf.sprintf
				"Can not alloc all temporary variables in %s shader." shader);
			false
		end else if fld >= 4 then
			alloc_loop (reg + 1) 0
		else if can_alloc 0 0 reg fld then begin
			alloc_var reg fld;
			var.var_reg <- Some(reg, fld);
			true
		end else
			alloc_loop reg (fld+1)
	in
	match var.var_reg with
	| Some _ -> true
	| None ->
		alloc_loop 0 0

let rec bind_registers' reg_map shader code =
	match code with
	| [] -> true
	| ins :: code ->
		Array.iter (fun field_map ->
			Array.iteri (fun i vars -> 
				field_map.(i) <-
					List.filter (LiveVar.is_alive ins) vars
			) field_map) reg_map;
		begin match ins.ins_kind with
		| IMov(dest, _) | IMul(dest, _, _) | IM44(dest, _, _) ->
			bind_register_var reg_map shader dest.dst_var
		end && bind_registers' reg_map shader code

let bind_registers shader cnt code =
	let reg_map = Array.init cnt (fun _ -> Array.make 4 []) in
	bind_registers' reg_map shader code

let finalize sh =
	let ok =
		bind_glob_registers "vertex shader constants" 128 
			(List.map variable_of_const sh.sh_glob.shg_v_const) &&
		bind_glob_registers "fragment shader constants" 28 
			(List.map variable_of_const sh.sh_glob.shg_f_const) &&
		bind_glob_registers "varying registers" 
			8 sh.sh_glob.shg_varying &&
		(LiveVar.compute sh.sh_vertex;   bind_registers "vertex" 8 sh.sh_vertex) &&
		(LiveVar.compute sh.sh_fragment; bind_registers "fragment" 8 sh.sh_fragment)
	in
	if ok then Some sh
	else None

(* ========================================================================= *)

let write sh () =
	Errors.error "Unimplemented: Agal.write."

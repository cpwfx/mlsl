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
	; attr_typ       : Midlang.typ
	; attr_var       : variable
	}

type const_named =
	{ cn_name : string
	; cn_typ  : Midlang.typ
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

type sampler_filter =
| SFltrNearest
| SFltrLinear

type sampler_mipmap =
| SMipDisable
| SMipNearest
| SMipLinear

type sampler_wrapping =
| SWrapClamp
| SWrapRepeat

type sampler =
	{         sam_name     : string
	; mutable sam_index    : int option
	;         sam_filter   : sampler_filter
	;         sam_mipmap   : sampler_mipmap
	;         sam_wrapping : sampler_wrapping
	;         sam_dim      : Midlang.sampler_dim
	}

type instr_kind =
| IMov of dest * source
| IMul of dest * source * source
| IM44 of dest * source * source
| ITex of dest * source * sampler
type instr =
	{ ins_id   : int
	; ins_kind : instr_kind
	}

type shader_globals =
	{ shg_attr     : attr list
	; shg_v_const  : const list
	; shg_f_const  : const list
	; shg_varying  : variable list
	; shg_samplers : sampler list
	}

type shader =
	{ sh_name     : string
	; sh_glob     : shader_globals
	; sh_vertex   : instr list
	; sh_fragment : instr list
	}

(* ========================================================================= *)

let varsort_to_int sort =
	match sort with
	| VSAttribute -> 0
	| VSConstant  -> 1
	| VSTemporary -> 2
	| VSOutput    -> 3
	| VSVarying   -> 4

let variable_of_const c =
	match c with
	| ConstNamed cn -> cn.cn_var
	| ConstValue cv -> cv.cv_var

(* ========================================================================= *)

let var_map     = Hashtbl.create 32
let sampler_map = Hashtbl.create 32

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
	| Midlang.SInput0    -> var.var_reg <- Some (0, 0)
	| Midlang.SInput1    -> var.var_reg <- Some (1, 0)
	| Midlang.SInput2    -> var.var_reg <- Some (2, 0)
	| Midlang.SInput3    -> var.var_reg <- Some (3, 0)
	| Midlang.SInput4    -> var.var_reg <- Some (4, 0)
	| Midlang.SInput5    -> var.var_reg <- Some (5, 0)
	| Midlang.SInput6    -> var.var_reg <- Some (6, 0)
	| Midlang.SInput7    -> var.var_reg <- Some (7, 0)
	| Midlang.SPosition  -> var.var_reg <- Some (0, 0)
	| Midlang.STexcoord0 -> var.var_reg <- Some (4, 0)
	| Midlang.STexcoord1 -> var.var_reg <- Some (5, 0)
	| Midlang.STexcoord2 -> var.var_reg <- Some (6, 0)
	| Midlang.STexcoord3 -> var.var_reg <- Some (7, 0)
	end;
	var

let map_attr attr =
	{ attr_name = attr.Midlang.attr_name
	; attr_typ  = attr.Midlang.attr_var.Midlang.var_typ
	; attr_var  = map_attr_variable attr
	}

let map_const const =
	ConstNamed
		{ cn_name = const.Midlang.param_name
		; cn_typ  = const.Midlang.param_var.Midlang.var_typ
		; cn_var  = map_variable const.Midlang.param_var
		}

let map_varying var =
	map_variable var.Midlang.param_var

let map_sampler sam = 
	try
		Hashtbl.find sampler_map sam.Midlang.sampler_name
	with
	| Not_found -> begin
		let result =
			{ sam_name     = sam.Midlang.sampler_name
			; sam_index    = None
			; sam_filter   = SFltrLinear
			; sam_mipmap   = SMipLinear
			; sam_wrapping = SWrapRepeat
			; sam_dim      = sam.Midlang.sampler_dim
			} in
		Hashtbl.replace sampler_map sam.Midlang.sampler_name result;
		result
	end

let map_globals sh =
	{ shg_attr     = List.map map_attr    sh.Midlang.sh_attr
	; shg_v_const  = List.map map_const   sh.Midlang.sh_v_const
	; shg_f_const  = List.map map_const   sh.Midlang.sh_f_const
	; shg_varying  = List.map map_varying sh.Midlang.sh_varying
	; shg_samplers = List.map map_sampler sh.Midlang.sh_samplers
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
		Some (create_instr 
			(IMul(make_dest dim res, make_source vec, make_source_float flo)), 
			const)
	| Midlang.ITex(rv, vc, sam) ->
		Some (create_instr 
			(ITex(make_dest Midlang.Dim4 rv, make_source vc, map_sampler sam)),
			const)
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
		| IMov(dest, src) | ITex(dest, src, _) -> VarSet.add src.src_var a
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
		if free <= cnt then
			bind_glob_registers' free name cnt vars
		else begin
			Errors.error (Printf.sprintf "I can't pack %s into %d registers"
				name cnt);
			false
		end

let bind_glob_registers name cnt vars =
	bind_glob_registers' 0 name cnt vars

let rec bind_sampler_registers' free samplers =
	match samplers with
	| [] -> true
	| s :: samplers ->
		s.sam_index <- Some free;
		let free = free + 1 in
		if free <= 8 then
			bind_sampler_registers' free samplers
		else begin
			Errors.error "Too many samplers.";
			false
		end

let bind_sampler_registers samplers =
	bind_sampler_registers' 0 samplers

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
		| IMov(dest, _) | IMul(dest, _, _) | IM44(dest, _, _) 
		| ITex(dest, _, _) ->
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
		bind_sampler_registers sh.sh_glob.shg_samplers &&
		(LiveVar.compute sh.sh_vertex;   bind_registers "vertex" 8 sh.sh_vertex) &&
		(LiveVar.compute sh.sh_fragment; bind_registers "fragment" 8 sh.sh_fragment)
	in
	if ok then Some sh
	else None

(* ========================================================================= *)

let create_attr_json attrs =
	Json.JsList (Json.create_list (List.map (fun attr ->
		Json.JsObj (Json.create_obj
		[ "name",        Json.JsString attr.attr_name
		; "type",        Json.JsString (Midlang.string_of_typ attr.attr_typ)
		; "register",    Json.JsInt (fst (Misc.Opt.value attr.attr_var.var_reg))
		; "fieldOffset", Json.JsInt (snd (Misc.Opt.value attr.attr_var.var_reg))
		])) attrs))

let create_const_json const =
	let named_const = Json.create_list [] in
	let value_const = Json.create_list [] in
	List.iter (fun c ->
		match c with
		| ConstNamed cn ->
			Json.list_add named_const (Json.JsObj (Json.create_obj
				[ "name",        Json.JsString cn.cn_name
				; "type",        Json.JsString (Midlang.string_of_typ cn.cn_typ)
				; "register",    Json.JsInt (fst (Misc.Opt.value cn.cn_var.var_reg))
				; "fieldOffset", Json.JsInt (snd (Misc.Opt.value cn.cn_var.var_reg))
				]))
		| ConstValue cv ->
			Errors.error "Unimplemented Agal.create_const_json ConstValue"
	) const;
	Json.JsObj (Json.create_obj 
		[ "params", Json.JsList named_const
		; "values", Json.JsList value_const
		])

let create_sampler_json samplers =
	let sam_list = Json.create_list [] in
	List.iter (fun s ->
		Json.list_add sam_list (Json.JsObj (Json.create_obj
			[ "name", Json.JsString s.sam_name
			; "index", Json.JsInt (Misc.Opt.value s.sam_index)
			; "type", Json.JsString (
				match s.sam_dim with
				| Midlang.SDim2D   -> "sampler2D"
				| Midlang.SDimCube -> "samplerCube"
				)
			]))
	) samplers;
	Json.JsList sam_list

let create_mask_bin mask offset =
	(
		(if mask.dmask_x then 1 else 0) lor
		(if mask.dmask_y then 2 else 0) lor
		(if mask.dmask_z then 4 else 0) lor
		(if mask.dmask_w then 8 else 0)
	) lsl offset

let write_dest out dst =
	(* Register number *)
	LittleEndian.write_short out 
		(dst.dst_row + (fst (Misc.Opt.value dst.dst_var.var_reg)));
	(* Write mask *)
	LittleEndian.write_byte out 
		(create_mask_bin dst.dst_mask (snd (Misc.Opt.value dst.dst_var.var_reg)));
	(* Register type *)
	LittleEndian.write_byte out (varsort_to_int dst.dst_var.var_sort)

let create_swizzle_bin swizzle fld_offset dest_offset =
	let comp0 =
		if dest_offset > 0 then 0 
		else swizzle.(0 - dest_offset) + fld_offset in
	let comp1 =
		if dest_offset > 1 then 0 
		else swizzle.(1 - dest_offset) + fld_offset in
	let comp2 =
		if dest_offset > 2 then 0 
		else swizzle.(2 - dest_offset) + fld_offset in
	let comp3 =
		if dest_offset > 3 then 0 
		else swizzle.(3 - dest_offset) + fld_offset in
	(comp0 land 3) lor
	((comp1 land 3) lsl 2) lor
	((comp2 land 3) lsl 4) lor
	((comp3 land 3) lsl 6)

let write_source out src dest_offset =
	(* Register number *)
	LittleEndian.write_short out
		(src.src_row + (fst (Misc.Opt.value src.src_var.var_reg)));
	(* Indirect offset *)
	begin match src.src_offset with
	| None -> (* direct *)
		LittleEndian.write_byte out 0
	| Some off -> (* indirect *)
		LittleEndian.write_byte out
			(off.srcoff_row + (fst (Misc.Opt.value off.srcoff_var.var_reg)))
	end;
	(* Swizzle *)
	LittleEndian.write_byte out 
		(create_swizzle_bin 
			src.src_swizzle 
			(snd (Misc.Opt.value src.src_var.var_reg)) 
			dest_offset);
	(* Register type *)
	LittleEndian.write_byte out (varsort_to_int src.src_var.var_sort);
	(* Indirect offset *)
	begin match src.src_offset with
	| None -> (* direct *)
		LittleEndian.write_byte  out 0;
		LittleEndian.write_short out 0x0080
	| Some off ->
		(* Index register type *)
		LittleEndian.write_byte out (varsort_to_int off.srcoff_var.var_sort);
		(* Index register component select *)
		LittleEndian.write_byte out 
			(off.srcoff_component + snd (Misc.Opt.value off.srcoff_var.var_reg));
		(* Indirect flag *)
		LittleEndian.write_byte out 0x80
	end

let sampler_filter_bin filter =
	match filter with
	| SFltrNearest -> 0
	| SFltrLinear  -> 1

let sampler_mipmap_bin mipmap =
	match mipmap with
	| SMipDisable -> 0
	| SMipNearest -> 1
	| SMipLinear  -> 2

let sampler_wrapping_bin wrap =
	match wrap with
	| SWrapClamp  -> 0
	| SWrapRepeat -> 1

let sampler_dim_bin dim =
	match dim with
	| Midlang.SDim2D   -> 0
	| Midlang.SDimCube -> 1

let write_sampler out sam =
	(* Sampler index *)
	LittleEndian.write_short out (Misc.Opt.value sam.sam_index);
	(* Unused *)
	LittleEndian.write_short out 0;
	(* Register type (Sampler) *)
	LittleEndian.write_byte out 5;
	(* 4bits unused and 4bit dimension *)
	LittleEndian.write_byte out ((sampler_dim_bin sam.sam_dim) lsl 4);
	(* 4bits special flag (0) and 4bit wrapping *)
	LittleEndian.write_byte out ((sampler_wrapping_bin sam.sam_wrapping) lsl 4);
	(* 4bit mipmap and 4bit filter *)
	LittleEndian.write_byte out (
		(sampler_mipmap_bin sam.sam_mipmap) lor
		((sampler_filter_bin sam.sam_filter) lsl 4))

let rec write_bytecode out code =
	match code with
	| [] -> ()
	| ins :: code ->
		begin match ins.ins_kind with
		| IMov(dst, src) ->
			LittleEndian.write_int out 0x00;
			write_dest out dst;
			write_source out src (snd (Misc.Opt.value (dst.dst_var.var_reg)));
			LittleEndian.write_int64 out Int64.zero (* dummy source *)
		| IMul(dst, src1, src2) ->
			LittleEndian.write_int out 0x03;
			write_dest out dst;
			write_source out src1 (snd (Misc.Opt.value (dst.dst_var.var_reg)));
			write_source out src2 (snd (Misc.Opt.value (dst.dst_var.var_reg)));
		| IM44(dst, src1, src2) ->
			LittleEndian.write_int out 0x18;
			write_dest out dst;
			write_source out src1 0;
			write_source out src2 0
		| ITex(dst, src, sam) ->
			LittleEndian.write_int out 0x28;
			write_dest out dst;
			write_source out src 0;
			write_sampler out sam
		end;
		write_bytecode out code

let write_program vertex path code =
	Misc.IO.with_out_channel path true (fun out ->
		LittleEndian.write_byte out 0xA0;                      (* magic        *)
		LittleEndian.write_int  out 0x01;                      (* version      *)
		LittleEndian.write_byte out 0xA1;                      (* shadertypeid *)
		LittleEndian.write_byte out (if vertex then 0 else 1); (* shadertype   *)
		write_bytecode out code
	)

let write sh () =
	Json.write (sh.sh_name ^ ".json") (Json.create_obj
		[ "attr",   create_attr_json sh.sh_glob.shg_attr
		; "vconst", create_const_json sh.sh_glob.shg_v_const
		; "fconst", create_const_json sh.sh_glob.shg_f_const
		; "sampler", create_sampler_json sh.sh_glob.shg_samplers
		]);
	write_program true  (sh.sh_name ^ ".vs") sh.sh_vertex;
	write_program false (sh.sh_name ^ ".fs") sh.sh_fragment

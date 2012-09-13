(* File: agal.ml *)

open Misc.Dim

type variable_sort =
| VSAttribute
| VSConstant
| VSTemporary
| VSOutput
| VSVarying

type variable =
	{         var_id         : int
	;         var_size       : int * int
	; mutable var_reg        : (int * int) option
	; mutable var_vec3output : bool (* set to true when can not contain w component *)
	;         var_sort       : variable_sort
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
	{         dst_var      : variable
	;         dst_row      : int
	;         dst_mask     : dest_mask
	; mutable dst_use_mask : bool
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
| IAdd of dest * source * source
| ISub of dest * source * source
| IMul of dest * source * source
| IDiv of dest * source * source
| IMin of dest * source * source
| IMax of dest * source * source
| IFrc of dest * source
| IPow of dest * source * source
| ICrs of dest * source * source
| IDp3 of dest * source * source
| IDp4 of dest * source * source
| INeg of dest * source
| ISat of dest * source
| IM33 of dest * source * source
| IM44 of dest * source * source
| IM34 of dest * source * source
| ITex of dest * source * sampler
(* Pseudo-instructions (generated instruction depends on locations 
 * binded to registers). *)
| ICrs2 of dest * source * source
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

type program_type =
| PVertex
| PFragment

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

let fresh_var = Misc.Fresh.create ()
let fresh_ins = Misc.Fresh.create ()

let create_instr kind =
	{ ins_id   = Misc.Fresh.next fresh_ins
	; ins_kind = kind
	}

type instr_source =
| InstrSource1       of source
| InstrSource2       of source * source
| InstrSourceSampler of source * sampler

let instr_dest instr =
	match instr.ins_kind with
	| IMov(dest, _)    | IAdd(dest, _, _) | ISub(dest, _, _) | IMul(dest, _, _)
	| IDiv(dest, _, _) | IMin(dest, _, _) | IMax(dest, _, _) | IFrc(dest, _)    
	| IPow(dest, _, _) | ICrs(dest, _, _) | IDp3(dest, _, _) | IDp4(dest, _, _) 
	| INeg(dest, _)    | ISat(dest, _)    | IM33(dest, _, _) | IM44(dest, _, _) 
	| IM34(dest, _, _) | ITex(dest, _, _) | ICrs2(dest, _, _) -> dest

let instr_source instr =
	match instr.ins_kind with
	| IMov(_, src) | IFrc(_, src) | INeg(_, src) | ISat(_, src) -> InstrSource1 src
	| IAdd(_, src1, src2) | ISub(_, src1, src2) | IMul(_, src1, src2) 
	| IDiv(_, src1, src2) | IMin(_, src1, src2) | IMax(_, src1, src2) 
	| IPow(_, src1, src2) | ICrs(_, src1, src2) | IDp3(_, src1, src2)
	| IDp4(_, src1, src2) | IM33(_, src1, src2) | IM44(_, src1, src2) 
	| IM34(_, src1, src2) | ICrs2(_, src1, src2) -> InstrSource2(src1, src2)
	| ITex(_, src, sam) -> InstrSourceSampler(src, sam)

let set_instr_source1 instr src1 =
	create_instr (
		match instr.ins_kind with
		| IMov(dst, _)       -> IMov(dst, src1)
		| IAdd(dst, _, src2) -> IAdd(dst, src1, src2)
		| ISub(dst, _, src2) -> ISub(dst, src1, src2)
		| IMul(dst, _, src2) -> IMul(dst, src1, src2)
		| IDiv(dst, _, src2) -> IDiv(dst, src1, src2)
		| IMin(dst, _, src2) -> IMin(dst, src1, src2)
		| IMax(dst, _, src2) -> IMax(dst, src1, src2)
		| IFrc(dst, _)       -> IFrc(dst, src1)
		| IPow(dst, _, src2) -> IPow(dst, src1, src2)
		| ICrs(dst, _, src2) -> ICrs(dst, src1, src2)
		| IDp3(dst, _, src2) -> IDp3(dst, src1, src2)
		| IDp4(dst, _, src2) -> IDp4(dst, src1, src2)
		| INeg(dst, _)       -> INeg(dst, src1)
		| ISat(dst, _)       -> ISat(dst, src1)
		| IM33(dst, _, src2) -> IM33(dst, src1, src2)
		| IM44(dst, _, src2) -> IM44(dst, src1, src2)
		| IM34(dst, _, src2) -> IM34(dst, src1, src2)
		| ITex(dst, _, sam)  -> ITex(dst, src1, sam)
		| ICrs2(dst, _, src2) -> ICrs2(dst, src1, src2)
	)

(* ========================================================================= *)

let var_map     = Hashtbl.create 32
let sampler_map = Hashtbl.create 32
let const_map   = Hashtbl.create 32

let get_const_reg const =
	try
		Hashtbl.find const_map const
	with
	| Not_found -> begin
		let result =
			{ var_id         = Misc.Fresh.next fresh_var
			; var_size       = (List.length const, List.length (List.hd const))
			; var_reg        = None
			; var_vec3output = false
			; var_sort       = VSConstant
			} in
		Hashtbl.replace const_map const result;
		result
	end

let map_variable var =
	try
		Hashtbl.find var_map var.Midlang.var_id
	with
	| Not_found -> begin
		let result =
			{ var_id         = Misc.Fresh.next fresh_var
			; var_size       =
				begin match var.Midlang.var_typ with
				| Midlang.TBool        -> (1, 1)
				| Midlang.TInt         -> (1, 1)
				| Midlang.TFloat       -> (1, 1)
				| Midlang.TMat(d1, d2) -> (Misc.Dim.int_of_dim d1, Misc.Dim.int_of_dim d2)
				| Midlang.TVec d       -> (1, Misc.Dim.int_of_dim d)
				end
			; var_reg        = None
			; var_vec3output = false
			; var_sort       =
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

let make_temp_reg rows cols =
	{ var_id         = Misc.Fresh.next fresh_var
	; var_size       = (rows, cols)
	; var_reg        = None
	; var_vec3output = false
	; var_sort       = VSTemporary
	}

let map_attr attr =
	{ attr_name = attr.Midlang.param_name
	; attr_typ  = attr.Midlang.param_var.Midlang.var_typ
	; attr_var  = map_variable attr.Midlang.param_var
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
			; sam_mipmap   = SMipDisable
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

let dst_mask_float = { dmask_x = true;  dmask_y = false; dmask_z = false; dmask_w = false }
let dst_mask_vec2  = { dmask_x = true;  dmask_y = true;  dmask_z = false; dmask_w = false }
let dst_mask_vec3  = { dmask_x = true;  dmask_y = true;  dmask_z = true;  dmask_w = false }
let dst_mask_vec4  = { dmask_x = true;  dmask_y = true;  dmask_z = true;  dmask_w = true  }
let dst_mask_x     = { dmask_x = true;  dmask_y = false; dmask_z = false; dmask_w = false }
let dst_mask_y     = { dmask_x = false; dmask_y = true;  dmask_z = false; dmask_w = false }
let dst_mask_z     = { dmask_x = false; dmask_y = false; dmask_z = true;  dmask_w = false }
let dst_mask_w     = { dmask_x = false; dmask_y = false; dmask_z = false; dmask_w = true  }

let swizzle_nop () = [| 0; 1; 2; 3 |]
let swizzle_dp2 () = [| 0; 1; 0; 1 |]

let make_swizzle swizzle =
	match swizzle with
	| MlslAst.Swizzle.S1 c1 ->
		[| MlslAst.Swizzle.component_id c1; 0; 0; 0 |]
	| MlslAst.Swizzle.S2(c1, c2) ->
		[| MlslAst.Swizzle.component_id c1; MlslAst.Swizzle.component_id c2; 0; 0 |]
	| MlslAst.Swizzle.S3(c1, c2, c3) ->
		[| MlslAst.Swizzle.component_id c1; MlslAst.Swizzle.component_id c2
		;  MlslAst.Swizzle.component_id c3; 0 |]
	| MlslAst.Swizzle.S4(c1, c2, c3, c4) ->
		[| MlslAst.Swizzle.component_id c1; MlslAst.Swizzle.component_id c2
		;  MlslAst.Swizzle.component_id c3; MlslAst.Swizzle.component_id c4 |]

let make_dest_float_reg reg =
	{ dst_var      = reg
	; dst_row      = 0
	; dst_mask     = dst_mask_float
	; dst_use_mask = true
	}

let make_dest_float reg = make_dest_float_reg (map_variable reg)

let make_dest_row_reg row dim reg =
	{ dst_var  = reg
	; dst_row  = row
	; dst_mask = 
		begin match dim with
		| Misc.Dim.Dim2 -> dst_mask_vec2
		| Misc.Dim.Dim3 -> dst_mask_vec3
		| Misc.Dim.Dim4 -> dst_mask_vec4
		end
	; dst_use_mask = true
	}
let make_dest dim reg = make_dest_row_reg 0 dim (map_variable reg)
let make_dest_row row dim reg = make_dest_row_reg row dim (map_variable reg)
let make_dest_reg dim reg = make_dest_row_reg 0 dim reg

let make_dest_comp comp reg =
	{ dst_var  = map_variable reg
	; dst_row  = 0
	; dst_mask =
		begin match comp with
		| 0 -> dst_mask_x
		| 1 -> dst_mask_y
		| 2 -> dst_mask_z
		| 3 -> dst_mask_w
		| _ -> raise (Misc.Internal_error (Printf.sprintf "Invalid component index: %d" comp))
		end
	; dst_use_mask = true
	}

let make_dest_comp_rng rng_start rng_len reg =
	{ dst_var  = map_variable reg
	; dst_row  = 0
	; dst_mask =
		{ dmask_x = rng_start <= 0 && 0 < rng_start + rng_len
		; dmask_y = rng_start <= 1 && 1 < rng_start + rng_len
		; dmask_z = rng_start <= 2 && 2 < rng_start + rng_len
		; dmask_w = rng_start <= 3 && 3 < rng_start + rng_len
		}
	; dst_use_mask = true
	}

let make_dest_comp_reg comp reg =
	{ dst_var  = reg
	; dst_row  = 0
	; dst_mask =
		begin match comp with
		| 0 -> dst_mask_x
		| 1 -> dst_mask_y
		| 2 -> dst_mask_z
		| 3 -> dst_mask_w
		| _ -> raise (Misc.Internal_error (Printf.sprintf "Invalid component index: %d" comp))
		end
	; dst_use_mask = true
	}

let make_dest_output () =
	{ dst_var  =
		{ var_id         = Misc.Fresh.next fresh_var
		; var_size       = (1, 4)
		; var_reg        = Some (0, 0)
		; var_vec3output = false
		; var_sort       = VSOutput
		}
	; dst_row = 0
	; dst_mask = dst_mask_vec4
	; dst_use_mask = true
	}

let make_source_row row reg =
	{ src_var     = map_variable reg
	; src_row     = row
	; src_swizzle = swizzle_nop ()
	; src_offset  = None
	}

let make_source_row_dp2 row reg =
	{ src_var     = map_variable reg
	; src_row     = row
	; src_swizzle = swizzle_dp2 ()
	; src_offset  = None
	}

let make_source reg = make_source_row 0 reg
let make_source_dp2 reg = make_source_row_dp2 0 reg

let make_source_float_reg reg =
	{ src_var     = reg
	; src_row     = 0
	; src_swizzle = [| 0; 0; 0; 0 |]
	; src_offset  = None
	}

let make_source_float reg = make_source_float_reg (map_variable reg)

let make_source_row_dim_reg row dim reg =
	{ src_var     = reg
	; src_row     = row
	; src_swizzle =
		[| 0
		;  1
		;  min 2 (Misc.Dim.int_of_dim dim)
		;  min 3 (Misc.Dim.int_of_dim dim)
		|]
	; src_offset  = None
	}

let make_source_row_dim row dim reg = make_source_row_dim_reg row dim (map_variable reg)
let make_source_dim dim reg = make_source_row_dim 0 dim reg
let make_source_dim_reg dim reg = make_source_row_dim_reg 0 dim reg

let make_swizzled_source reg swizzle =
	{ src_var     = map_variable reg
	; src_row     = 0
	; src_swizzle = make_swizzle swizzle
	; src_offset  = None
	}

let make_source_reg reg =
	{ src_var     = reg
	; src_row     = 0
	; src_swizzle = swizzle_nop ()
	; src_offset  = None
	}

let make_source_shift shift dim reg =
	Printf.printf "SHIFT : %d, DIM : %d\n" shift (int_of_dim dim);
	let shift_field field =
		max 0 (min (field - shift) (int_of_dim dim - 1)) in
	{ src_var     = map_variable reg
	; src_row     = 0
	; src_swizzle = [| 0; shift_field 1; shift_field 2; shift_field 3 |]
	; src_offset  = None
	}

let make_const_float value =
	{ src_var     = get_const_reg [[value]]
	; src_row     = 0
	; src_swizzle = [| 0; 0; 0; 0 |]
	; src_offset  = None
	}

let make_const_vec dim value =
	{ src_var     = get_const_reg [ Array.to_list value ]
	; src_row     = 0
	; src_swizzle = [| 0; 0; 0; 0 |]
	; src_offset  = None
	}

let build_binop rv r1 r2 op =
	match op with
	| Midlang.BOOrB ->
		Some [create_instr (IMax(make_dest_float rv, make_source r1, make_source r2))]
	| Midlang.BOAddB | Midlang.BOAddI | Midlang.BOAddF ->
		Some [create_instr (IAdd(make_dest_float rv, make_source r1, make_source r2))]
	| Midlang.BOAddM(dim1, dim) ->
		Some (List.map (fun row -> create_instr (IAdd(
					make_dest_row row dim rv, 
					make_source_row row r1, 
					make_source_row row r2))
				) (Misc.Dim.range_of_dim dim1))
	| Midlang.BOAddV dim ->
		Some [create_instr (IAdd(make_dest dim rv, make_source r1, make_source r2))]
	| Midlang.BOSubI | Midlang.BOSubF ->
		Some [create_instr (ISub(make_dest_float rv, make_source r1, make_source r2))]
	| Midlang.BOSubM(dim1, dim) ->
		Some (List.map (fun row -> create_instr (ISub(
					make_dest_row row dim rv, 
					make_source_row row r1, 
					make_source_row row r2))
				) (Misc.Dim.range_of_dim dim1))
	| Midlang.BOSubV dim ->
		Some [create_instr (ISub(make_dest dim rv, make_source r1, make_source r2))]
	| Midlang.BOAndB | Midlang.BOMulI | Midlang.BOMulFF ->
		Some [create_instr (IMul(make_dest_float rv, make_source r1, make_source r2))]
	| Midlang.BOMulMF(dim1, dim) ->
		Some (List.map (fun row -> create_instr (IMul(
					make_dest_row row dim rv, 
					make_source_row row r1, 
					make_source_float r2))
				) (Misc.Dim.range_of_dim dim1))
	| Midlang.BOMulMM(dim1, dim2, dim3) ->
		Errors.error "Unimplemented Agal.build_binop IMulMM%d%d%d"
			(Misc.Dim.int_of_dim dim1) (Misc.Dim.int_of_dim dim2) (Misc.Dim.int_of_dim dim3);
		None
	| Midlang.BOMulMV(dim, Misc.Dim.Dim2) ->
		let tmp = make_temp_reg 1 (Misc.Dim.int_of_dim dim) in
		Some (List.map (fun row -> create_instr (IDp4(
					make_dest_comp_reg row tmp,
					make_source_row_dp2 row r1,
					make_source_dp2 r2))
				) (Misc.Dim.range_of_dim dim) @
			[create_instr (
				IMul(make_dest dim rv, make_source_reg tmp, make_const_float 0.5)) ])
	| Midlang.BOMulMV(Misc.Dim.Dim2, Misc.Dim.Dim3) ->
		Some (List.map (fun row -> create_instr (IDp3(
					make_dest_comp row rv,
					make_source_row row r1,
					make_source r2))
				) [ 0; 1 ])
	| Midlang.BOMulMV(Misc.Dim.Dim3, Misc.Dim.Dim3) ->
		Some [create_instr 
			(IM33(make_dest Misc.Dim.Dim3 rv, make_source r2, make_source r1))]
	| Midlang.BOMulMV(Misc.Dim.Dim4, Misc.Dim.Dim3) ->
		Some
		[ create_instr 
			(IM33(make_dest Misc.Dim.Dim3 rv, make_source r2, make_source r1))
		; create_instr
			(IDp3(make_dest_comp 3 rv, make_source r2, make_source_row 3 r1))
		]
	| Midlang.BOMulMV(Misc.Dim.Dim2, Misc.Dim.Dim4) ->
		Some (List.map (fun row -> create_instr (IDp4(
					make_dest_comp row rv,
					make_source_row row r1,
					make_source r2))
				) [ 0; 1 ])
	| Midlang.BOMulMV(Misc.Dim.Dim3, Misc.Dim.Dim4) ->
		Some [create_instr 
			(IM34(make_dest Misc.Dim.Dim3 rv, make_source r2, make_source r1))]
	| Midlang.BOMulMV(Misc.Dim.Dim4, Misc.Dim.Dim4) ->
		Some [create_instr 
			(IM44(make_dest Misc.Dim.Dim4 rv, make_source r2, make_source r1))]
	| Midlang.BOMulVF dim ->
		Some [create_instr
			(IMul(make_dest dim rv, make_source r1, make_source_float r2))]
	| Midlang.BOMulVV dim ->
		Some [create_instr
			(IMul(make_dest dim rv, make_source r1, make_source r2))]
	| Midlang.BODivI ->
		let tmp1 = make_temp_reg 1 1 in
		let tmp2 = make_temp_reg 1 1 in
		Some
		[ create_instr (IDiv(make_dest_float_reg tmp1, make_source_float r1, make_source_float r2))
		; create_instr (IFrc(make_dest_float_reg tmp2, make_source_reg tmp1))
		; create_instr (ISub(make_dest_float rv, make_source_reg tmp1, make_source_reg tmp2))
		]
	| Midlang.BODivFF ->
		Some
		[ create_instr
			(IDiv(make_dest_float rv, make_source_float r1, make_source_float r2))]
	| Midlang.BODivFV dim ->
		Some [create_instr
			(IDiv(make_dest dim rv, make_source_float r1, make_source_dim dim r2))]
	| Midlang.BODivMF(dim1, dim) ->
		Some (List.map (fun row -> create_instr (IDiv(
					make_dest_row row dim rv,
					make_source_row_dim row dim r1,
					make_source_dim dim r2))
				) (Misc.Dim.range_of_dim dim1))
	| Midlang.BODivVF dim ->
		Some [create_instr
			(IDiv(make_dest dim rv, make_source_dim dim r1, make_source_float r2))]
	| Midlang.BODivVV dim ->
		Some [create_instr
			(IDiv(make_dest dim rv, make_source_dim dim r1, make_source_dim dim r2))]
	| Midlang.BOModI | Midlang.BOModFF ->
		let tmp1 = make_temp_reg 1 1 in
		let tmp2 = make_temp_reg 1 1 in
		Some
			[ create_instr (IDiv(
				make_dest_float_reg tmp1, make_source_float r1, make_source_float r2))
			; create_instr (IFrc(
				make_dest_float_reg tmp2, make_source_float_reg tmp1))
			; create_instr (IMul(
				make_dest_float rv, make_source_float_reg tmp2, make_source_float r2))
			]
	| Midlang.BOModFV dim ->
		let d = Misc.Dim.int_of_dim dim in
		let tmp1 = make_temp_reg 1 d in
		let tmp2 = make_temp_reg 1 d in
		Some
			[ create_instr (IDiv(
				make_dest_reg dim tmp1, make_source_float r1, make_source_dim dim r2))
			; create_instr (IFrc(
				make_dest_reg dim tmp2, make_source_dim_reg dim tmp1))
			; create_instr (IMul(
				make_dest dim rv, make_source_dim_reg dim tmp2, make_source_dim dim r2))
			]
	| Midlang.BOModMF(dim1, dim) ->
		let d = Misc.Dim.int_of_dim dim in
		Some (Misc.ListExt.concat_map (fun row -> 
			let tmp1 = make_temp_reg 1 d in
			let tmp2 = make_temp_reg 1 d in
			[ create_instr (IDiv(
				make_dest_reg dim tmp1, make_source_row_dim row dim r1, make_source_dim dim r2))
			; create_instr (IFrc(
				make_dest_reg dim tmp2, make_source_dim_reg dim tmp1))
			; create_instr (IMul(
				make_dest_row row dim rv, make_source_dim_reg dim tmp2, make_source_dim dim r2))
			]) (Misc.Dim.range_of_dim dim1))
	| Midlang.BOModVF dim ->
		let d = Misc.Dim.int_of_dim dim in
		let tmp1 = make_temp_reg 1 d in
		let tmp2 = make_temp_reg 1 d in
		Some
			[ create_instr (IDiv(
				make_dest_reg dim tmp1, make_source_dim dim r1, make_source_float r2))
			; create_instr (IFrc(
				make_dest_reg dim tmp2, make_source_dim_reg dim tmp1))
			; create_instr (IMul(
				make_dest dim rv, make_source_dim_reg dim tmp2, make_source_float r2))
			]
	| Midlang.BOModVV dim ->
		let d = Misc.Dim.int_of_dim dim in
		let tmp1 = make_temp_reg 1 d in
		let tmp2 = make_temp_reg 1 d in
		Some
			[ create_instr (IDiv(
				make_dest_reg dim tmp1, make_source_dim dim r1, make_source_dim dim r2))
			; create_instr (IFrc(
				make_dest_reg dim tmp2, make_source_dim_reg dim tmp1))
			; create_instr (IMul(
				make_dest dim rv, make_source_dim_reg dim tmp2, make_source_dim dim r2))
			]
	| Midlang.BODot Misc.Dim.Dim2 ->
		let tmp = make_temp_reg 1 2 in
		Some
			[ create_instr (IDp4(
				make_dest_reg Misc.Dim.Dim2 tmp, make_source_dp2 r1, make_source_dp2 r2))
			; create_instr (IMul(
				make_dest Misc.Dim.Dim2 rv, make_source_reg tmp, make_const_float 0.5))
			]
	| Midlang.BODot Misc.Dim.Dim3 ->
		Some [create_instr
			(IDp3(make_dest_float rv, make_source r1, make_source r2))]
	| Midlang.BODot Misc.Dim.Dim4 ->
		Some [create_instr
			(IDp4(make_dest_float rv, make_source r1, make_source r2))]
	| Midlang.BOCross2 ->
		Some [create_instr 
			(ICrs2(make_dest_float rv, make_source r1, make_source r2))]
	| Midlang.BOCross3 ->
		Some [create_instr 
			(ICrs(make_dest Misc.Dim.Dim3 rv, make_source r1, make_source r2))]
	| Midlang.BOJoinFF ->
		Some
			[ create_instr (IMov(make_dest_comp 0 rv, make_source_float r1))
			; create_instr (IMov(make_dest_comp 1 rv, make_source_float r2))
			]
	| Midlang.BOJoinFV d ->
		Some
			[ create_instr (IMov(make_dest_comp 0 rv, make_source_float r1))
			; create_instr (IMov(make_dest_comp_rng 1 (int_of_dim d) rv, 
				make_source_shift 1 d r2))
			]
	| Midlang.BOJoinVF d ->
		Some
			[ create_instr (IMov(make_dest_comp_rng 0 (int_of_dim d) rv, make_source r1))
			; create_instr (IMov(make_dest_comp (int_of_dim d) rv, make_source_float r2))
			]
	| Midlang.BOJoinVV ->
		Some
			[ create_instr (IMov(make_dest_comp_rng 0 2 rv, make_source r1))
			; create_instr (IMov(make_dest_comp_rng 2 2 rv, make_source_shift 2 Dim2 r2))
			]
	| Midlang.BOJoinVM(rdim, dim) ->
		Some (create_instr (IMov(make_dest dim rv, make_source r1)) ::
			List.map (fun row ->
				create_instr (IMov(make_dest_row (row + 1) dim rv, make_source_row row r2))
			) (range_of_dim rdim))
	| Midlang.BOJoinMV(rdim, dim) ->
		Some (List.map (fun row ->
				create_instr (IMov(make_dest_row row dim rv, make_source_row row r1))
			) (range_of_dim rdim) @ 
			[ create_instr (IMov(make_dest_row (int_of_dim rdim) dim rv, make_source r2 )) ])
	| Midlang.BOJoinMM dim ->
		Some
			[ create_instr (IMov(make_dest_row 0 dim rv, make_source_row 0 r1))
			; create_instr (IMov(make_dest_row 1 dim rv, make_source_row 1 r1))
			; create_instr (IMov(make_dest_row 2 dim rv, make_source_row 0 r2))
			; create_instr (IMov(make_dest_row 3 dim rv, make_source_row 1 r2))
			]
	| Midlang.BOPowI | Midlang.BOPowFF ->
		Some [create_instr 
			(IPow(make_dest_float rv, make_source_float r1, make_source_float r2))]
	| Midlang.BOPowVF dim ->
		Some [create_instr 
			(IPow(make_dest dim rv, make_source_dim dim r1, make_source_float r2))]
	| Midlang.BOPowVV dim ->
		Some [create_instr 
			(IPow(make_dest dim rv, make_source_dim dim r1, make_source_dim dim r2))]
	| Midlang.BOMinI | Midlang.BOMinF ->
		Some [create_instr
			(IMin(make_dest_float rv, make_source_float r1, make_source_float r2))]
	| Midlang.BOMinV dim ->
		Some [create_instr
			(IMin(make_dest dim rv, make_source_dim dim r1, make_source_dim dim r2))]

let build_unop rv r1 op =
	match op with
	| Midlang.UONotB ->
		Some [create_instr(ISub(make_dest_float rv, make_const_float 1.0, make_source_float r1))]
	| Midlang.UONegI | Midlang.UONegF ->
		Some [create_instr(INeg(make_dest_float rv, make_source_float r1))]
	| Midlang.UONegM(dim1, dim) ->
		Some (List.map (fun row -> 
					create_instr (INeg(make_dest_row row dim rv, make_source_row_dim row dim r1))
				) (Misc.Dim.range_of_dim dim1))
	| Midlang.UONegV dim ->
		Some [create_instr(INeg(make_dest dim rv, make_source r1))]

let build_ins globals ins =
	match ins.Midlang.ins_kind with
	| Midlang.IMov(dst, src) ->
		begin match dst.Midlang.var_typ with
		| Midlang.TBool | Midlang.TInt | Midlang.TFloat ->
			Some [create_instr (IMov(make_dest_float dst, make_source src))]
		| Midlang.TMat(d1, d2) ->
			Some (List.map (fun row -> create_instr (IMov(
				make_dest_row   row d2 dst,
				make_source_row row src))
			) (Misc.Dim.range_of_dim d1))
		| Midlang.TVec dim ->
			Some [create_instr (IMov(make_dest dim dst, make_source src))]
		end
	| Midlang.IConstBool(dst, b) ->
		Some [create_instr (IMov(
			make_dest_float dst, 
			make_const_float (if b then 1.0 else 0.0)))]
	| Midlang.IConstInt(dst, i) ->
		Some [create_instr (IMov(
			make_dest_float dst,
			make_const_float (float_of_int i)))]
	| Midlang.IConstFloat(dst, f) ->
		Some [create_instr (IMov(
			make_dest_float dst,
			make_const_float f))]
	| Midlang.IConstVec(dst, dim, v) ->
		Some [create_instr (IMov(
			make_dest_float dst,
			make_const_vec dim v))]
	| Midlang.IConstMat(dst, d1, d2, m) ->
		Some (List.map (fun row -> create_instr (IMov(
			make_dest_row row d2 dst,
			make_const_vec d2 m.(row)))
		) (Misc.Dim.range_of_dim d1))
	| Midlang.IConvert(rv, r1, conv) ->
		begin match conv with
		| Midlang.CBool2Int | Midlang.CBool2Float | Midlang.CInt2Float ->
			Some [create_instr (IMov(make_dest_float rv, make_source_float r1))]
		end
	| Midlang.IBinOp(rv, r1, r2, op) ->
		build_binop rv r1 r2 op
	| Midlang.IUnOp(rv, r1, op) ->
		build_unop rv r1 op
	| Midlang.ISwizzle(dst, src, swizzle) ->
		begin match dst.Midlang.var_typ with
		| Midlang.TFloat | Midlang.TInt | Midlang.TBool ->
			Some [create_instr 
					(IMov(make_dest_float dst, make_swizzled_source src swizzle))]
		| Midlang.TVec dim ->
			Some [create_instr 
					(IMov(make_dest dim dst, make_swizzled_source src swizzle))] 
		| _ -> raise (Misc.Internal_error "Invalid swizzle output type")
		end
	| Midlang.ITex(rv, vc, sam) ->
		Some [create_instr 
			(ITex(make_dest Misc.Dim.Dim4 rv, make_source vc, map_sampler sam))]
	| Midlang.IRet reg ->
		Some [create_instr (IMov(make_dest_output (), make_source reg))]

let rec build_code' globals acc code =
	match code with
	| [] -> Some(List.rev acc)
	| ins :: code ->
		Misc.Opt.bind (build_ins globals ins) (fun ins ->
			build_code' globals ((List.rev ins) @ acc) code
		)

let build_code globals code =
	Hashtbl.clear const_map;
	Misc.Opt.map_f (build_code' globals [] code) (fun code ->
			let const = Hashtbl.fold (fun c var const ->
				ConstValue
					{ cv_value = Array.of_list (List.map Array.of_list c)
					; cv_var   = var
					} :: const) const_map []
			in (code, const)
		)

(* AGAL doesn't allow to use two consts as source registers *)
let fixup_const_args ins =
	match instr_source ins with
	| InstrSource2(src1, src2) when 
		src1.src_var.var_sort = VSConstant &&
		src2.src_var.var_sort = VSConstant ->
		let tmp = make_temp_reg 1 4 in
		[ create_instr (IMov (make_dest_reg Dim4 tmp, make_source_dim_reg Dim4 src1.src_var))
		; set_instr_source1 ins { src1 with src_var = tmp }
		]
	| _ -> [ ins ]

let build sh =
	Hashtbl.clear var_map;
	let globals = map_globals sh in
	Misc.Opt.bind (build_code globals sh.Midlang.sh_vertex)   (fun (vs, vs_const) ->
	Misc.Opt.bind (build_code globals sh.Midlang.sh_fragment) (fun (fs, fs_const) ->
		Some
			{ sh_name     = sh.Midlang.sh_name
			; sh_glob     = add_const globals vs_const fs_const
			; sh_vertex   = Misc.ListExt.concat_map fixup_const_args vs
			; sh_fragment = Misc.ListExt.concat_map fixup_const_args fs
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
		| IMov(_, src) | IFrc(_, src) | INeg(_, src) | ISat(_, src) 
		| ITex(_, src, _) -> 
			VarSet.add src.src_var a
		| IAdd(_, src1, src2) | ISub(_, src1, src2) | IMul(_, src1, src2)
		| IDiv(_, src1, src2) | IMin(_, src1, src2) | IMax(_, src1, src2)
		| IPow(_, src1, src2) | ICrs(_, src1, src2) | IDp3(_, src1, src2)
		| IDp4(_, src1, src2) | IM33(_, src1, src2) | IM44(_, src1, src2) 
		| IM34(_, src1, src2) | ICrs2(_, src1, src2) ->
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

module Vec3Output = struct
	let compute_instr instr =
		match instr with
		| IMov _ | IAdd _ | ISub _ | IMul _ | IDiv _ | IMin _ | IMax _ | IFrc _
		| IPow _ | IDp3 _ | IDp4 _ | INeg _ | ISat _ | IM44 _ | ITex _ -> 
			()
		| ICrs(dest, _, _) | IM33(dest, _, _) | IM34(dest, _, _) 
		| ICrs2(dest, _, _) ->
			dest.dst_var.var_vec3output <- true

	let rec compute code =
		match code with
		| [] -> ()
		| instr :: code ->
			compute_instr instr.ins_kind;
			compute code
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
			Errors.error "I can't pack %s into %d registers" name cnt;
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
	let max_fld = if var.var_vec3output then 3 else 4 in
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
			&& (fld + fi < max_fld)
			&& (Misc.ListExt.is_empty reg_map.(reg + ri).(fld + fi))
			&& can_alloc ri (fi + 1) reg fld
	in
	let rec alloc_loop reg fld =
		if reg >= Array.length reg_map then begin
			Errors.error
				"Can not alloc all temporary variables in %s shader." shader;
			false
		end else if fld >= max_fld then
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
		begin bind_register_var reg_map shader (instr_dest ins).dst_var
		end && bind_registers' reg_map shader code

let bind_registers shader cnt code =
	let reg_map = Array.init cnt (fun _ -> Array.make 4 []) in
	bind_registers' reg_map shader code

(* Remove mask in a first write to register *)
let remove_first_mask =
	let module Register = struct
		type t = variable
		let compare v1 v2 =
			if v1.var_sort = v2.var_sort then
				compare (fst (Misc.Opt.value v1.var_reg)) (fst (Misc.Opt.value v2.var_reg))
			else
				compare v1.var_sort v2.var_sort
	end in
	let module RegSet = Set.Make(Register) in
	let rec remove_first_mask reg_set code =
		match code with
		| [] -> ()
		| ins :: code ->
			let dest = instr_dest ins in
			if not (RegSet.mem dest.dst_var reg_set) then
			begin
				dest.dst_use_mask <- false;
				remove_first_mask (RegSet.add dest.dst_var reg_set) code
			end
			else remove_first_mask reg_set code
	in remove_first_mask RegSet.empty

let finalize sh =
	let ok =
		bind_glob_registers "attributes" 8 
			(List.map (fun attr -> attr.attr_var) sh.sh_glob.shg_attr) &&
		bind_glob_registers "vertex shader constants" 128 
			(List.map variable_of_const sh.sh_glob.shg_v_const) &&
		bind_glob_registers "fragment shader constants" 28 
			(List.map variable_of_const sh.sh_glob.shg_f_const) &&
		bind_glob_registers "varying registers" 8 sh.sh_glob.shg_varying &&
		bind_sampler_registers sh.sh_glob.shg_samplers &&
		begin
			LiveVar.compute sh.sh_vertex;
			Vec3Output.compute sh.sh_vertex;
			bind_registers "vertex" 8 sh.sh_vertex
		end &&
		begin
			LiveVar.compute sh.sh_fragment;
			Vec3Output.compute sh.sh_fragment;
			bind_registers "fragment" 8 sh.sh_fragment
		end
	in
	if ok then begin
		remove_first_mask sh.sh_vertex;
		remove_first_mask sh.sh_fragment;
		Some sh
	end else None

(* ========================================================================= *)

module ConstArray = struct
	type t = float array array * bool array

	let create_empty () = (Array.make_matrix 128 4 0.0, Array.make 128 false)

	let add (ca_val, ca_uf) row col value =
		Array.iteri (fun rs vec ->
			ca_uf.(row + rs) <- true;
			Array.iteri (fun cs fl ->
				ca_val.(row + rs).(col + cs) <- fl
			) vec
		) value

	let to_json (ca_val, ca_uf) =
		let result_list = Json.create_list [] in
		Array.iteri (fun row vec ->
			if ca_uf.(row) then
				Json.list_add result_list (Json.JsObj (Json.create_obj
					[ "register", Json.JsInt row
					; "value", Json.JsList (Json.create_list (
						List.map (fun f -> Json.JsFloat f) (Array.to_list vec)))
					]))
			else ()
		) ca_val;
		Json.JsList result_list
end

let create_attr_json attrs =
	Json.JsList (Json.create_list (List.map (fun attr ->
		Json.JsObj (Json.create_obj
		[ "name",        Json.JsString attr.attr_name
		; "type",        Json.JsString (Midlang.string_of_typ attr.attr_typ)
		; "register",    Json.JsInt (fst (Misc.Opt.value attr.attr_var.var_reg))
		; "fieldOffset", Json.JsInt (snd (Misc.Opt.value attr.attr_var.var_reg))
		])) attrs))

let create_const_params_json const =
	let param_list = Json.create_list [] in
	List.iter (fun c ->
		match c with
		| ConstNamed cn ->
			Json.list_add param_list (Json.JsObj (Json.create_obj
				[ "name",        Json.JsString cn.cn_name
				; "type",        Json.JsString (Midlang.string_of_typ cn.cn_typ)
				; "register",    Json.JsInt (fst (Misc.Opt.value cn.cn_var.var_reg))
				; "fieldOffset", Json.JsInt (snd (Misc.Opt.value cn.cn_var.var_reg))
				]))
		| ConstValue cv -> ()
	) const;
	Json.JsList param_list

let create_const_values_json const =
	let value_list = ConstArray.create_empty () in
	List.iter (fun c ->
		match c with
		| ConstNamed cn -> ()
		| ConstValue cv ->
			let (row, col) = Misc.Opt.value cv.cv_var.var_reg in
			ConstArray.add value_list row col cv.cv_value
	) const;
	ConstArray.to_json value_list

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

let create_mask_bin mask offset use_mask =
	if use_mask then
		(
			(if mask.dmask_x then 1 else 0) lor
			(if mask.dmask_y then 2 else 0) lor
			(if mask.dmask_z then 4 else 0) lor
			(if mask.dmask_w then 8 else 0)
		) lsl offset
	else 15

let dest_bytecode bytecode dst =
	(* Register number *)
	Misc.ByteArray.append_short bytecode
		(dst.dst_row + (fst (Misc.Opt.value dst.dst_var.var_reg)));
	(* Write mask *)
	Misc.ByteArray.append_byte bytecode
		(create_mask_bin 
			dst.dst_mask 
			(snd (Misc.Opt.value dst.dst_var.var_reg)) 
			dst.dst_use_mask);
	(* Register type *)
	Misc.ByteArray.append_byte bytecode (varsort_to_int dst.dst_var.var_sort)

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

let source_bytecode bytecode src dest_offset =
	(* Register number *)
	Misc.ByteArray.append_short bytecode
		(src.src_row + (fst (Misc.Opt.value src.src_var.var_reg)));
	(* Indirect offset *)
	begin match src.src_offset with
	| None -> (* direct *)
		Misc.ByteArray.append_byte bytecode 0
	| Some off -> (* indirect *)
		Misc.ByteArray.append_byte bytecode 
			(off.srcoff_row + (fst (Misc.Opt.value off.srcoff_var.var_reg)))
	end;
	(* Swizzle *)
	Misc.ByteArray.append_byte bytecode
		(create_swizzle_bin 
			src.src_swizzle 
			(snd (Misc.Opt.value src.src_var.var_reg)) 
			dest_offset);
	(* Register type *)
	Misc.ByteArray.append_byte bytecode (varsort_to_int src.src_var.var_sort);
	(* Indirect offset *)
	begin match src.src_offset with
	| None -> (* direct *)
		Misc.ByteArray.append_byte  bytecode 0;
		Misc.ByteArray.append_short bytecode 0x0000
	| Some off ->
		(* Index register type *)
		Misc.ByteArray.append_byte bytecode (varsort_to_int off.srcoff_var.var_sort);
		(* Index register component select *)
		Misc.ByteArray.append_byte bytecode
			(off.srcoff_component + snd (Misc.Opt.value off.srcoff_var.var_reg));
		(* Indirect flag *)
		Misc.ByteArray.append_byte bytecode 0x80
	end

let crs2_source_bytecode bytecode src dst_field =
	let real_src =
		{ src with src_swizzle =
			match dst_field with
			| 0 ->
				[| src.src_swizzle.(2)
				;  src.src_swizzle.(0)
				;  src.src_swizzle.(1)
				;  src.src_swizzle.(3)
				|]
			| 1 ->
				[| src.src_swizzle.(1)
				;  src.src_swizzle.(2)
				;  src.src_swizzle.(0)
				;  src.src_swizzle.(3)
				|]
			| 2 -> src.src_swizzle
			| _ -> raise (Misc.Internal_error "Invalid cross product output field")
		} in
	source_bytecode bytecode real_src 0

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

let sampler_bytecode bytecode sam =
	(* Sampler index *)
	Misc.ByteArray.append_short bytecode (Misc.Opt.value sam.sam_index);
	(* Unused *)
	Misc.ByteArray.append_short bytecode 0;
	(* Register type (Sampler) *)
	Misc.ByteArray.append_byte bytecode 5;
	(* 4bits unused and 4bit dimension *)
	Misc.ByteArray.append_byte bytecode ((sampler_dim_bin sam.sam_dim) lsl 4);
	(* 4bits special flag (0) and 4bit wrapping *)
	Misc.ByteArray.append_byte bytecode ((sampler_wrapping_bin sam.sam_wrapping) lsl 4);
	(* 4bit mipmap and 4bit filter *)
	Misc.ByteArray.append_byte bytecode (
		(sampler_mipmap_bin sam.sam_mipmap) lor
		((sampler_filter_bin sam.sam_filter) lsl 4))

let instr_bytecode bytecode ins =
	match ins.ins_kind with
	| IMov(dst, src) ->
		Misc.ByteArray.append_int bytecode 0x00;
		dest_bytecode bytecode dst;
		source_bytecode bytecode src (snd (Misc.Opt.value (dst.dst_var.var_reg)));
		Misc.ByteArray.append_int64 bytecode Int64.zero (* dummy source *)
	| IAdd(dst, src1, src2) ->
		Misc.ByteArray.append_int bytecode 0x01;
		dest_bytecode bytecode dst;
		source_bytecode bytecode src1 (snd (Misc.Opt.value (dst.dst_var.var_reg)));
		source_bytecode bytecode src2 (snd (Misc.Opt.value (dst.dst_var.var_reg)));
	| ISub(dst, src1, src2) ->
		Misc.ByteArray.append_int bytecode 0x02;
		dest_bytecode bytecode dst;
		source_bytecode bytecode src1 (snd (Misc.Opt.value (dst.dst_var.var_reg)));
		source_bytecode bytecode src2 (snd (Misc.Opt.value (dst.dst_var.var_reg)));
	| IMul(dst, src1, src2) ->
		Misc.ByteArray.append_int bytecode 0x03;
		dest_bytecode bytecode dst;
		source_bytecode bytecode src1 (snd (Misc.Opt.value (dst.dst_var.var_reg)));
		source_bytecode bytecode src2 (snd (Misc.Opt.value (dst.dst_var.var_reg)));
	| IDiv(dst, src1, src2) ->
		Misc.ByteArray.append_int bytecode 0x04;
		dest_bytecode bytecode dst;
		source_bytecode bytecode src1 (snd (Misc.Opt.value (dst.dst_var.var_reg)));
		source_bytecode bytecode src2 (snd (Misc.Opt.value (dst.dst_var.var_reg)));
	| IMin(dst, src1, src2) ->
		Misc.ByteArray.append_int bytecode 0x06;
		dest_bytecode bytecode dst;
		source_bytecode bytecode src1 (snd (Misc.Opt.value (dst.dst_var.var_reg)));
		source_bytecode bytecode src2 (snd (Misc.Opt.value (dst.dst_var.var_reg)));
	| IMax(dst, src1, src2) ->
		Misc.ByteArray.append_int bytecode 0x07;
		dest_bytecode bytecode dst;
		source_bytecode bytecode src1 (snd (Misc.Opt.value (dst.dst_var.var_reg)));
		source_bytecode bytecode src2 (snd (Misc.Opt.value (dst.dst_var.var_reg)));
	| IFrc(dst, src) ->
		Misc.ByteArray.append_int bytecode 0x08;
		dest_bytecode bytecode dst;
		source_bytecode bytecode src (snd (Misc.Opt.value (dst.dst_var.var_reg)));
		Misc.ByteArray.append_int64 bytecode Int64.zero (* dummy source *)
	| IPow(dst, src1, src2) ->
		Misc.ByteArray.append_int bytecode 0x0B;
		dest_bytecode bytecode dst;
		source_bytecode bytecode src1 (snd (Misc.Opt.value (dst.dst_var.var_reg)));
		source_bytecode bytecode src2 (snd (Misc.Opt.value (dst.dst_var.var_reg)));
	| ICrs(dst, src1, src2) ->
		Misc.ByteArray.append_int bytecode 0x11;
		dest_bytecode bytecode dst;
		source_bytecode bytecode src1 0;
		source_bytecode bytecode src2 0
	| IDp3(dst, src1, src2) ->
		Misc.ByteArray.append_int bytecode 0x12;
		dest_bytecode bytecode dst;
		source_bytecode bytecode src1 0;
		source_bytecode bytecode src2 0
	| IDp4(dst, src1, src2) ->
		Misc.ByteArray.append_int bytecode 0x13;
		dest_bytecode bytecode dst;
		source_bytecode bytecode src1 0;
		source_bytecode bytecode src2 0
	| INeg(dst, src) ->
		Misc.ByteArray.append_int bytecode 0x15;
		dest_bytecode bytecode dst;
		source_bytecode bytecode src (snd (Misc.Opt.value (dst.dst_var.var_reg)));
		Misc.ByteArray.append_int64 bytecode Int64.zero (* dummy source *)
	| ISat(dst, src) ->
		Misc.ByteArray.append_int bytecode 0x16;
		dest_bytecode bytecode dst;
		source_bytecode bytecode src (snd (Misc.Opt.value (dst.dst_var.var_reg)));
		Misc.ByteArray.append_int64 bytecode Int64.zero (* dummy source *)
	| IM33(dst, src1, src2) ->
		Misc.ByteArray.append_int bytecode 0x17;
		dest_bytecode bytecode dst;
		source_bytecode bytecode src1 0;
		source_bytecode bytecode src2 0
	| IM44(dst, src1, src2) ->
		Misc.ByteArray.append_int bytecode 0x18;
		dest_bytecode bytecode dst;
		source_bytecode bytecode src1 0;
		source_bytecode bytecode src2 0
	| IM34(dst, src1, src2) ->
		Misc.ByteArray.append_int bytecode 0x19;
		dest_bytecode bytecode dst;
		source_bytecode bytecode src1 0;
		source_bytecode bytecode src2 0
	| ITex(dst, src, sam) ->
		Misc.ByteArray.append_int bytecode 0x19;
		dest_bytecode bytecode dst;
		source_bytecode bytecode src 0;
		sampler_bytecode bytecode sam
	| ICrs2(dst, src1, src2) ->
		Misc.ByteArray.append_int bytecode 0x11;
		dest_bytecode bytecode dst;
		crs2_source_bytecode bytecode src1 (snd (Misc.Opt.value (dst.dst_var.var_reg)));
		crs2_source_bytecode bytecode src2 (snd (Misc.Opt.value (dst.dst_var.var_reg)))

let register_name program_type sort row =
	match sort with
	| VSAttribute -> "va" ^ string_of_int row
	| VSConstant  ->
		begin match program_type with
		| PVertex -> "vc"
		| PFragment -> "fc"
		end ^ string_of_int row
	| VSTemporary ->
		begin match program_type with
		| PVertex -> "vt"
		| PFragment -> "ft"
		end ^ string_of_int row
	| VSOutput    -> 
		begin match program_type with
		| PVertex -> "op"
		| PFragment -> "oc"
		end
	| VSVarying   -> "v" ^ string_of_int row

let dest_asm program_type dst =
	let reg_name =
		register_name program_type dst.dst_var.var_sort
			(dst.dst_row + (fst (Misc.Opt.value dst.dst_var.var_reg))) in
	let mask =
		create_mask_bin dst.dst_mask (snd (Misc.Opt.value dst.dst_var.var_reg)) dst.dst_use_mask in
	reg_name ^
		(if mask = 15 then ""
		else
			"." ^
			(if mask land 1 = 1 then "x" else "") ^
			(if mask land 2 = 2 then "y" else "") ^
			(if mask land 4 = 4 then "z" else "") ^
			(if mask land 8 = 8 then "w" else "")
		)

let create_swizzle_asm swizzle fld_offset dest_offset =
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
	let comp_to_str c =
		match c with
		| 0 -> "x"
		| 1 -> "y"
		| 2 -> "z"
		| 3 -> "w"
		| _ -> "w" in
	"." ^ (comp_to_str comp0) ^ (comp_to_str comp1) ^
	      (comp_to_str comp2) ^ (comp_to_str comp3)

let source_asm program_type src dest_offset =
	let reg_name =
		register_name program_type src.src_var.var_sort
			(src.src_row + (fst (Misc.Opt.value src.src_var.var_reg))) in
	let offset =
		begin match src.src_offset with
		| None -> ""
		| Some off ->
			Errors.error "Unimplemented: write_source_asm with indirect offset.";
			"[]"
		end in
	let swizzle =
		create_swizzle_asm 
			src.src_swizzle 
			(snd (Misc.Opt.value src.src_var.var_reg)) 
			dest_offset in
	reg_name ^ offset ^ (if swizzle = ".xyzw" then "" else swizzle)

let crs2_source_asm program_type src dst_field =
	let real_src =
		{ src with src_swizzle =
			match dst_field with
			| 0 ->
				[| src.src_swizzle.(2)
				;  src.src_swizzle.(0)
				;  src.src_swizzle.(1)
				;  src.src_swizzle.(3)
				|]
			| 1 ->
				[| src.src_swizzle.(1)
				;  src.src_swizzle.(2)
				;  src.src_swizzle.(0)
				;  src.src_swizzle.(3)
				|]
			| 2 -> src.src_swizzle
			| _ -> raise (Misc.Internal_error "Invalid cross product output field")
		} in
	source_asm program_type real_src 0

let sampler_asm sampler =
	let write_sampler_dim dim =
		match dim with
		| Midlang.SDim2D   -> "2d"
		| Midlang.SDimCube -> "cube"
	in
	let write_sampler_filter filter =
		match filter with
		| SFltrNearest -> "nearest"
		| SFltrLinear  -> "linear"
	in
	let write_sampler_mipmap mipmap =
		match mipmap with
		| SMipDisable -> "mipnone"
		| SMipNearest -> "mipnearest"
		| SMipLinear  -> "miplinear"
	in
	let write_sampler_wrapping wrapping =
		match wrapping with
		| SWrapClamp  -> "clamp"
		| SWrapRepeat -> "repeat"
	in
	Printf.sprintf "fs%d<%s,%s,%s,%s>" 
		(Misc.Opt.value         sampler.sam_index)
		(write_sampler_dim      sampler.sam_dim)
		(write_sampler_filter   sampler.sam_filter)
		(write_sampler_mipmap   sampler.sam_mipmap)
		(write_sampler_wrapping sampler.sam_wrapping)

let instr_asm program_type buffer ins =
	match ins.ins_kind with
	| IMov(dst, src) -> Printf.bprintf buffer "mov %s, %s\n"
		(dest_asm program_type dst)
		(source_asm program_type src (snd (Misc.Opt.value (dst.dst_var.var_reg))))
	| IAdd(dst, src1, src2) -> Printf.bprintf buffer "add %s, %s, %s\n"
		(dest_asm program_type dst)
		(source_asm program_type src1 (snd (Misc.Opt.value (dst.dst_var.var_reg))))
		(source_asm program_type src2 (snd (Misc.Opt.value (dst.dst_var.var_reg))))
	| ISub(dst, src1, src2) -> Printf.bprintf buffer "sub %s, %s, %s\n"
		(dest_asm program_type dst)
		(source_asm program_type src1 (snd (Misc.Opt.value (dst.dst_var.var_reg))))
		(source_asm program_type src2 (snd (Misc.Opt.value (dst.dst_var.var_reg))))
	| IMul(dst, src1, src2) -> Printf.bprintf buffer "mul %s, %s, %s\n"
		(dest_asm program_type dst)
		(source_asm program_type src1 (snd (Misc.Opt.value (dst.dst_var.var_reg))))
		(source_asm program_type src2 (snd (Misc.Opt.value (dst.dst_var.var_reg))))
	| IDiv(dst, src1, src2) -> Printf.bprintf buffer "div %s, %s, %s\n"
		(dest_asm program_type dst)
		(source_asm program_type src1 (snd (Misc.Opt.value (dst.dst_var.var_reg))))
		(source_asm program_type src2 (snd (Misc.Opt.value (dst.dst_var.var_reg))))
	| IMin(dst, src1, src2) -> Printf.bprintf buffer "min %s, %s, %s\n"
		(dest_asm program_type dst)
		(source_asm program_type src1 (snd (Misc.Opt.value (dst.dst_var.var_reg))))
		(source_asm program_type src2 (snd (Misc.Opt.value (dst.dst_var.var_reg))))
	| IMax(dst, src1, src2) -> Printf.bprintf buffer "max %s, %s, %s\n"
		(dest_asm program_type dst)
		(source_asm program_type src1 (snd (Misc.Opt.value (dst.dst_var.var_reg))))
		(source_asm program_type src2 (snd (Misc.Opt.value (dst.dst_var.var_reg))))
	| IFrc(dst, src) -> Printf.bprintf buffer "frc %s, %s\n"
		(dest_asm program_type dst)
		(source_asm program_type src (snd (Misc.Opt.value (dst.dst_var.var_reg))))
	| IPow(dst, src1, src2) -> Printf.bprintf buffer "pow %s, %s, %s\n"
		(dest_asm program_type dst)
		(source_asm program_type src1 (snd (Misc.Opt.value (dst.dst_var.var_reg))))
		(source_asm program_type src2 (snd (Misc.Opt.value (dst.dst_var.var_reg))))
	| ICrs(dst, src1, src2) -> Printf.bprintf buffer "crs %s, %s, %s\n"
		(dest_asm program_type dst)
		(source_asm program_type src1 0)
		(source_asm program_type src2 0)
	| IDp3(dst, src1, src2) -> Printf.bprintf buffer "dp3 %s, %s, %s\n"
		(dest_asm program_type dst)
		(source_asm program_type src1 0)
		(source_asm program_type src2 0)
	| IDp4(dst, src1, src2) -> Printf.bprintf buffer "dp4 %s, %s, %s\n"
		(dest_asm program_type dst)
		(source_asm program_type src1 0)
		(source_asm program_type src2 0)
	| INeg(dst, src) -> Printf.bprintf buffer "neg %s, %s\n"
		(dest_asm program_type dst)
		(source_asm program_type src (snd (Misc.Opt.value (dst.dst_var.var_reg))))
	| ISat(dst, src) -> Printf.bprintf buffer "sat %s, %s\n"
		(dest_asm program_type dst)
		(source_asm program_type src (snd (Misc.Opt.value (dst.dst_var.var_reg))))
	| IM33(dst, src1, src2) -> Printf.bprintf buffer "m33 %s, %s, %s\n"
		(dest_asm program_type dst)
		(source_asm program_type src1 0)
		(source_asm program_type src2 0)
	| IM44(dst, src1, src2) -> Printf.bprintf buffer "m44 %s, %s, %s\n"
		(dest_asm program_type dst)
		(source_asm program_type src1 0)
		(source_asm program_type src2 0)
	| IM34(dst, src1, src2) -> Printf.bprintf buffer "m34 %s, %s, %s\n"
		(dest_asm program_type dst)
		(source_asm program_type src1 0)
		(source_asm program_type src2 0)
	| ITex(dst, src, sam) -> Printf.bprintf buffer "tex %s, %s, %s\n"
		(dest_asm program_type dst)
		(source_asm program_type src 0)
		(sampler_asm sam)
	| ICrs2(dst, src1, src2) -> Printf.bprintf buffer "crs %s, %s, %s\n"
		(dest_asm program_type dst)
		(crs2_source_asm program_type src1 (snd (Misc.Opt.value (dst.dst_var.var_reg))))
		(crs2_source_asm program_type src2 (snd (Misc.Opt.value (dst.dst_var.var_reg))))

let program_bytecode program_type code =
	let bytecode = Misc.ByteArray.create Misc.ByteArray.LittleEndian in
	Misc.ByteArray.append_byte bytecode 0xA0; (* magic        *)
	Misc.ByteArray.append_int  bytecode 0x01; (* version      *)
	Misc.ByteArray.append_byte bytecode 0xA1; (* shadertypeid *)
	Misc.ByteArray.append_byte bytecode (
		match program_type with
		| PVertex -> 0
		| PFragment -> 1);                    (* shadertype   *)
	List.iter (instr_bytecode bytecode) code;
	bytecode

let program_asm program_type code =
	let buffer = Buffer.create 32 in
	List.iter (instr_asm program_type buffer) code;
	Buffer.contents buffer

type source_type =
| STBytecode
| STAssembler

let write_agal src_tp sh () =
	Json.write (sh.sh_name ^ "_vp.json") (Json.create_obj
		[ "source", Json.JsObj (Json.create_obj
			[ "type",  Json.JsString (
				match src_tp with
				| STBytecode  -> "bytecode"
				| STAssembler -> "assembler")
			; "value", Json.JsString (
				match src_tp with
				| STBytecode ->
					Misc.Base64.of_byte_array (program_bytecode PVertex sh.sh_vertex)
				| STAssembler ->
					program_asm PVertex sh.sh_vertex)
			])
		; "attr", create_attr_json sh.sh_glob.shg_attr
		; "constParams", create_const_params_json sh.sh_glob.shg_v_const
		; "constValues", create_const_values_json sh.sh_glob.shg_v_const
		]);
	Json.write (sh.sh_name ^ "_fp.json") (Json.create_obj
		[ "source", Json.JsObj (Json.create_obj
			[ "type",  Json.JsString (
				match src_tp with
				| STBytecode  -> "bytecode"
				| STAssembler -> "assembler")
			; "value", Json.JsString (
				match src_tp with
				| STBytecode ->
					Misc.Base64.of_byte_array (program_bytecode PFragment sh.sh_fragment)
				| STAssembler ->
					program_asm PFragment sh.sh_fragment)
			])
		; "sampler", create_sampler_json sh.sh_glob.shg_samplers
		; "constParams", create_const_params_json sh.sh_glob.shg_f_const
		; "constValues", create_const_values_json sh.sh_glob.shg_f_const
		])

let write     = write_agal STBytecode
let write_asm = write_agal STAssembler

let do_all shader =
	Misc.Opt.iter (build shader) (fun aprog ->
	let aprog_opt = optimize aprog in
	Misc.Opt.iter (finalize aprog_opt) (fun aprog_fin ->
	Final.add_action (write aprog_fin)))

let do_all_asm shader =
	Misc.Opt.iter (build shader) (fun aprog ->
	let aprog_opt = optimize aprog in
	Misc.Opt.iter (finalize aprog_opt) (fun aprog_fin ->
	Final.add_action (write_asm aprog_fin)))

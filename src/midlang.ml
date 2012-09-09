(* File: midlang.ml *)

open Misc.Dim

module StrMap = Map.Make(String)

let instr_fresh   = Misc.Fresh.create ()
let var_fresh     = Misc.Fresh.create ()
let sampler_fresh = Misc.Fresh.create ()

type typ =
| TBool
| TInt
| TFloat
| TMat  of dim * dim
| TVec  of dim

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

module Variable = struct
	type t = variable
	let compare x y = compare x.var_id y.var_id
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
| BOAddB (* Boolean alternative when the case of two true values is impossibile *)
| BOAddI
| BOAddF
| BOAddM  of dim * dim
| BOAddV  of dim
| BOSubI
| BOSubF
| BOSubM  of dim * dim
| BOSubV  of dim
| BOMulI
| BOMulFF
| BOMulMF of dim * dim
| BOMulMM of dim * dim * dim
| BOMulMV of dim * dim
| BOMulVF of dim
| BOMulVV of dim
| BODivI
| BODivFF
| BODivFV of dim
| BODivMF of dim * dim
| BODivVF of dim
| BODivVV of dim
| BOModI
| BOModFF
| BOModFV of dim
| BOModMF of dim * dim
| BOModVF of dim
| BOModVV of dim
| BODot   of dim
| BOCross2
| BOCross3
| BOPowI
| BOPowFF
| BOPowVF of dim
| BOPowVV of dim
| BOMinI
| BOMinF
| BOMinV  of dim

type unop =
| UONotB
| UONegI
| UONegF
| UONegM of dim * dim
| UONegV of dim

type instr_kind =
| IMov        of variable * variable
| IConstBool  of variable * bool
| IConstInt   of variable * int
| IConstFloat of variable * float
| IConstVec   of variable * dim * float array
| IConstMat   of variable * dim * dim * float array array
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

let create_instr kind =
	{ ins_id   = Misc.Fresh.next instr_fresh
	; ins_kind = kind
	}

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

let string_of_typ tp =
	match tp with
	| TBool        -> "bool"
	| TInt         -> "int"
	| TFloat       -> "float"
	| TMat(d1, d2) -> Printf.sprintf "mat%d%d" (int_of_dim d1) (int_of_dim d2)
	| TVec d       -> Printf.sprintf "vec%d" (int_of_dim d)

let vectyp_of_int n =
	match n with
	| 1 -> TFloat
	| 2 -> TVec Dim2
	| 3 -> TVec Dim3
	| 4 -> TVec Dim4
	| _ -> raise (Misc.Internal_error (Printf.sprintf "Invalid vector size: %d" n))

(* ========================================================================= *)

let create_var_ast sort ast_typ =
	{ var_id = Misc.Fresh.next var_fresh
	; var_typ =
		begin match ast_typ with
		| MlslAst.TBool  -> TBool
		| MlslAst.TInt   -> TInt
		| MlslAst.TFloat -> TFloat
		| MlslAst.TMat(d1, d2) -> TMat(d1, d2)
		| MlslAst.TVec d       -> TVec d
		| MlslAst.TSampler2D | MlslAst.TSamplerCube 
		| MlslAst.TUnit | MlslAst.TArrow _ | MlslAst.TPair _
		| MlslAst.TRecord _ | MlslAst.TVertex _ | MlslAst.TFragment _
		| MlslAst.TVertexTop -> raise (Misc.Internal_error "Variable of invalid type.")
		end
	; var_sort = sort
	}

let create_variable sort typ =
	{ var_id   = Misc.Fresh.next var_fresh
	; var_typ  = typ
	; var_sort = sort
	}

let create_sampler_ast name typ =
	{ sampler_id   = Misc.Fresh.next sampler_fresh
	; sampler_name = name
	; sampler_dim  =
		begin match typ with
		| MlslAst.TSampler2D   -> SDim2D
		| MlslAst.TSamplerCube -> SDimCube
		| _ -> raise (Misc.Internal_error "Sampler of invalid type.")
		end
	}

(* ========================================================================= *)

let credits = ref 1024

let attr_map    = Hashtbl.create 32
let v_const_map = Hashtbl.create 32
let f_const_map = Hashtbl.create 32
let varying_map = Hashtbl.create 32
let sampler_map = Hashtbl.create 32

let create_attr_list () =
	Misc.ListExt.map_filter (fun (name, semantics, _) ->
		if Hashtbl.mem attr_map name then
			Some
				{ attr_name      = name
				; attr_semantics =
					begin match semantics.MlslAst.asem_name with
					| "INPUT0"    -> SInput0
					| "INPUT1"    -> SInput1
					| "INPUT2"    -> SInput2
					| "INPUT3"    -> SInput3
					| "INPUT4"    -> SInput4
					| "INPUT5"    -> SInput5
					| "INPUT6"    -> SInput6
					| "INPUT7"    -> SInput7
					| "POSITION"  -> SPosition
					| "TEXCOORD0" -> STexcoord0
					| "TEXCOORD1" -> STexcoord1
					| "TEXCOORD2" -> STexcoord2
					| "TEXCOORD3" -> STexcoord3
					| sem -> 
						Errors.error "Internal error: unknown semantics %s." sem;
						SPosition
					end
				; attr_var       = Hashtbl.find attr_map name
				}
		else None) (TopDef.attr_list ())

let create_v_const_list () =
	Misc.ListExt.map_filter (fun (name, _) ->
		if Hashtbl.mem v_const_map name then
			Some
				{ param_name = name
				; param_var  = Hashtbl.find v_const_map name
				}
		else None) (TopDef.const_list ())

let create_f_const_list () =
	Misc.ListExt.map_filter (fun (name, _) ->
		if Hashtbl.mem f_const_map name then
			Some
				{ param_name = name
				; param_var  = Hashtbl.find f_const_map name
				}
		else None) (TopDef.const_list ())

let create_varying_list () =
	Hashtbl.fold (fun name var l ->
		{ param_name = name
		; param_var  = var
		} :: l) varying_map []

let create_sampler_list () =
	Misc.ListExt.map_filter (fun (name, _) ->
		try
			Some (Hashtbl.find sampler_map name)
		with
		| Not_found -> None) (TopDef.sampler_list ())

(* ========================================================================= *)

type program_type =
| PVertex
| PFragment

type reg_value =
	{         rv_pos  : Errors.position
	; mutable rv_kind : reg_value_kind option
	}
and reg_value_kind =
| RVReg        of variable
| RVRecord     of reg_value StrMap.t
| RVSampler    of sampler
| RVPair       of reg_value * reg_value
| RVFunc       of reg_value StrMap.t * MlslAst.pattern * MlslAst.expr
| RVIfFunc     of variable * reg_value * reg_value
| RVValue      of TopDef.value

let make_reg_value pos kind =
	{ rv_pos  = pos
	; rv_kind = Some kind
	}

let reg_value_of_value value =
	{ rv_pos  = value.TopDef.v_pos
	; rv_kind = Some (RVValue value)
	}

let make_value pos kind =
	{ rv_pos  = pos
	; rv_kind = Some (RVValue (TopDef.make_value pos kind))
	}

exception Unfold_exception

let reg_value_kind pos rv =
	match rv.rv_kind with
	| None ->
		Errors.error_p pos "Ivalid fixpoint: This value (defined at %s) was used during its evaluation."
			(Errors.string_of_pos rv.rv_pos);
		raise Unfold_exception
	| Some kind -> kind

let value_kind pos value =
	EvalPrim.with_exn Unfold_exception (fun () -> EvalPrim.value_kind pos value)

let reg_value_kind_of_attr pos program_type name semantics typ =
	match program_type with
	| PVertex ->
		RVReg (
			try
				Hashtbl.find attr_map name
			with
			| Not_found ->
				let v = create_var_ast VSAttribute typ.MlslAst.tt_typ in
				Hashtbl.add attr_map name v;
				v
			)
	| PFragment ->
		Errors.error_p pos "Attributes are not available for fragment programs.";
		raise Unfold_exception

let reg_value_kind_of_const pos program_type name typ =
	let const_map =
		match program_type with
		| PVertex -> v_const_map
		| PFragment -> f_const_map
	in
	RVReg (
		try
			Hashtbl.find const_map name
		with
		| Not_found ->
			let v = create_var_ast VSConstant typ.MlslAst.tt_typ in
			Hashtbl.add const_map name v;
			v
		)

let reg_value_kind_of_sampler pos program_type name typ =
	match program_type with
	| PVertex -> 
		Errors.error_p pos "Samplers are not available for vertex programs.";
		raise Unfold_exception
	| PFragment ->
		RVSampler (
			try
				Hashtbl.find sampler_map name
			with
			| Not_found ->
				let v = create_sampler_ast name typ.MlslAst.tt_typ in
				Hashtbl.add sampler_map name v;
				v
			)

let const_or_reg_value_kind pos program_type rv =
	let kind = reg_value_kind pos rv in
	match kind with
	| RVReg _ | RVRecord _ | RVSampler _ | RVPair _ | RVFunc _ | RVIfFunc _ -> kind
	| RVValue value ->
	begin match value_kind pos value with
	| TopDef.VAttr(name, semantics, typ) ->
		reg_value_kind_of_attr pos program_type name semantics typ
	| TopDef.VConst(name, typ) ->
		reg_value_kind_of_const pos program_type name typ
	| TopDef.VSampler(name, typ) ->
		reg_value_kind_of_sampler pos program_type name typ
	| TopDef.VFragment _ | TopDef.VVertex _ | TopDef.VBool _ | TopDef.VInt _ 
	| TopDef.VFloat _ | TopDef.VVec _ | TopDef.VMat _ | TopDef.VRecord _ 
	| TopDef.VPair _ | TopDef.VFunc _ | TopDef.VConstrU _ | TopDef.VConstrP _ -> kind
	end

let concrete_reg_value_kind pos program_type code rv =
	let kind = reg_value_kind pos rv in
	match kind with
	| RVReg _ | RVRecord _ | RVSampler _ | RVPair _ | RVFunc _ | RVIfFunc _ -> kind
	| RVValue value ->
	begin match value_kind pos value with
	| TopDef.VAttr(name, semantics, typ) ->
		reg_value_kind_of_attr pos program_type name semantics typ
	| TopDef.VConst(name, typ) ->
		reg_value_kind_of_const pos program_type name typ
	| TopDef.VSampler(name, typ) ->
		reg_value_kind_of_sampler pos program_type name typ
	| TopDef.VFragment _ ->
		Errors.error_p pos "Fragment program can not be a concrete value.";
		raise Unfold_exception
	| TopDef.VVertex _ ->
		Errors.error_p pos "Vertex program can not be a concrete value.";
		raise Unfold_exception
	| TopDef.VBool b ->
		let rreg = create_variable VSTemporary TBool in
		Misc.ImpList.add code (create_instr (IConstBool(rreg, b)));
		RVReg rreg
	| TopDef.VInt n ->
		let rreg = create_variable VSTemporary TInt in
		Misc.ImpList.add code (create_instr (IConstInt(rreg, n)));
		RVReg rreg
	| TopDef.VFloat f ->
		let rreg = create_variable VSTemporary TFloat in
		Misc.ImpList.add code (create_instr (IConstFloat(rreg, f)));
		RVReg rreg
	| TopDef.VVec(dim, v) ->
		let rreg = create_variable VSTemporary (TVec dim) in
		Misc.ImpList.add code (create_instr (IConstVec(rreg, dim, v)));
		RVReg rreg
	| TopDef.VMat(d1, d2, m) ->
		let rreg = create_variable VSTemporary (TMat(d1, d2)) in
		Misc.ImpList.add code (create_instr (IConstMat(rreg, d1, d2, m)));
		RVReg rreg
	| TopDef.VRecord rd ->
		RVRecord (StrMap.map reg_value_of_value rd)
	| TopDef.VPair(v1, v2) ->
		RVPair(reg_value_of_value v1, reg_value_of_value v2)
	| TopDef.VFunc(closure, pat, body) ->
		RVFunc(StrMap.map reg_value_of_value closure, pat, body)
	| TopDef.VConstrU _ ->
		Errors.error_p pos "Unimplemented: concrete_reg_value_kind VConstrU";
		raise Unfold_exception
	| TopDef.VConstrP _ ->
		Errors.error_p pos "Unimplemented: concrete_reg_value_kind VConstrP";
		raise Unfold_exception
	end

let string_of_rvkind kind =
	match kind with
	| RVReg _        -> "data type value"
	| RVRecord _     -> "record"
	| RVPair _       -> "pair"
	| RVSampler _    -> "sampler"
	| RVFunc _       -> "function"
	| RVIfFunc _     -> "function with condition"
	| RVValue _      -> "high level value"

let cast_regval_to_type pos code rv tp =
	Errors.error_p pos "Unimpleneted: cast_regval_to_type.";
	raise Unfold_exception

let rec bind_pattern code gamma pat rv =
	match pat.MlslAst.p_kind with
	| MlslAst.PAny   -> gamma
	| MlslAst.PVar x -> StrMap.add x rv gamma
	| MlslAst.PTypedVar(x, tp) ->
		let rv' = cast_regval_to_type pat.MlslAst.p_pos code rv tp.MlslAst.tt_typ in
			StrMap.add x rv' gamma
	| MlslAst.PTrue ->
		Errors.error_p pat.MlslAst.p_pos "Unimplemented: bind_pattern PTrue";
		raise Unfold_exception
	| MlslAst.PFalse ->
		Errors.error_p pat.MlslAst.p_pos "Unimplemented: bind_pattern PTrue";
		raise Unfold_exception
	| MlslAst.PPair _ ->
		Errors.error_p pat.MlslAst.p_pos "Unimplemented: bind_pattern PPair";
		raise Unfold_exception
	| MlslAst.PConstrU _ ->
		Errors.error_p pat.MlslAst.p_pos "Unimplemented: bind_pattern PConstrU";
		raise Unfold_exception
	| MlslAst.PConstrP _ ->
		Errors.error_p pat.MlslAst.p_pos "Unimplemented: bind_pattern PConstrP";
		raise Unfold_exception

let ins tp f code =
	let rreg = create_variable VSTemporary tp in
	Misc.ImpList.add code (create_instr (f rreg));
	rreg
let ins_convert_b2i code reg =
	ins TInt (fun rr -> IConvert(rr, reg, CBool2Int)) code
let ins_convert_b2f code reg =
	ins TFloat (fun rr -> IConvert(rr, reg, CBool2Float)) code
let ins_convert_i2f code reg = 
	ins TFloat (fun rr -> IConvert(rr, reg, CInt2Float)) code
let ins_binop tp op code reg1 reg2 = 
	ins tp (fun rr -> IBinOp(rr, reg1, reg2, op)) code
let ins_unop tp op code reg =
	ins tp (fun rr -> IUnOp(rr, reg, op)) code

let ins_binop_and_b           = ins_binop TBool  BOAndB
let ins_binop_add_b           = ins_binop TBool  BOAddB
let ins_binop_add_i           = ins_binop TInt   BOAddI
let ins_binop_add_f           = ins_binop TFloat BOAddF
let ins_binop_add_v d         = ins_binop (TVec d) (BOAddV d)
let ins_binop_add_m d1 d2     = ins_binop (TMat(d1, d2)) (BOAddM(d1, d2))
let ins_binop_sub_i           = ins_binop TInt   BOSubI
let ins_binop_sub_f           = ins_binop TFloat BOSubF
let ins_binop_sub_v d         = ins_binop (TVec d) (BOSubV d)
let ins_binop_sub_m d1 d2     = ins_binop (TMat(d1, d2)) (BOSubM(d1, d2))
let ins_binop_mul_i           = ins_binop TInt   BOMulI
let ins_binop_mul_ff          = ins_binop TFloat BOMulFF
let ins_binop_mul_vf d        = ins_binop (TVec d) (BOMulVF d)
let ins_binop_mul_vv d        = ins_binop (TVec d) (BOMulVV d)
let ins_binop_mul_mf d1 d2    = ins_binop (TMat(d1, d2)) (BOMulMF(d1, d2))
let ins_binop_mul_mv d1 d2    = ins_binop (TVec d1) (BOMulMV(d1, d2))
let ins_binop_mul_mm d1 d2 d3 = ins_binop (TMat(d1, d3)) (BOMulMM(d1, d2, d3))
let ins_binop_div_i           = ins_binop TInt   BODivI
let ins_binop_div_ff          = ins_binop TFloat BODivFF
let ins_binop_div_fv d        = ins_binop (TVec d) (BODivFV d)
let ins_binop_div_vf d        = ins_binop (TVec d) (BODivVF d)
let ins_binop_div_vv d        = ins_binop (TVec d) (BODivVV d)
let ins_binop_div_mf d1 d2    = ins_binop (TMat(d1, d2)) (BODivMF(d1, d2))
let ins_binop_mod_i           = ins_binop TInt   BOModI
let ins_binop_mod_ff          = ins_binop TFloat BOModFF
let ins_binop_mod_fv d        = ins_binop (TVec d) (BOModFV d)
let ins_binop_mod_vf d        = ins_binop (TVec d) (BOModVF d)
let ins_binop_mod_vv d        = ins_binop (TVec d) (BOModVV d)
let ins_binop_mod_mf d1 d2    = ins_binop (TMat(d1, d2)) (BOModMF(d1, d2))
let ins_binop_dot d           = ins_binop TFloat (BODot d)
let ins_binop_cross2          = ins_binop TFloat BOCross2
let ins_binop_cross3          = ins_binop (TVec Dim3) BOCross3
let ins_binop_pow_i           = ins_binop TInt BOPowI
let ins_binop_pow_ff          = ins_binop TFloat BOPowFF
let ins_binop_pow_vf d        = ins_binop (TVec d) (BOPowVF d)
let ins_binop_pow_vv d        = ins_binop (TVec d) (BOPowVV d)
let ins_binop_min_i           = ins_binop TInt BOMinI
let ins_binop_min_f           = ins_binop TFloat BOMinF
let ins_binop_min_v d         = ins_binop (TVec d) (BOMinV d)

let ins_unop_not_b       = ins_unop TBool UONotB
let ins_unop_neg_i       = ins_unop TInt UONegI
let ins_unop_neg_f       = ins_unop TFloat UONegF
let ins_unop_neg_v d     = ins_unop (TVec d) (UONegV d)
let ins_unop_neg_m d1 d2 = ins_unop (TMat(d1, d2)) (UONegM(d1, d2))

let ins_convert_2f code reg =
	match reg.var_typ with
	| TBool -> ins_convert_b2f code reg
	| TInt  -> ins_convert_i2f code reg
	| TFloat -> reg
	| _ -> raise (Misc.Internal_error "Ivalid conversion")

let unfold_add pos code reg1 reg2 =
	match reg1.var_typ, reg2.var_typ with
	| TInt, TInt ->
		make_reg_value pos (RVReg (ins_binop_add_i code reg1 reg2))
	| TInt, TFloat ->
		make_reg_value pos (RVReg (ins_binop_add_f code (ins_convert_i2f code reg1) reg2))
	| TFloat, TInt ->
		make_reg_value pos (RVReg (ins_binop_add_f code reg1 (ins_convert_i2f code reg2)))
	| TFloat, TFloat ->
		make_reg_value pos (RVReg (ins_binop_add_f code reg1 reg2))
	| TMat(d1, d2), TMat(d1', d2') when d1 = d1' && d2 = d2' ->
		make_reg_value pos (RVReg (ins_binop_add_m d1 d2 code reg1 reg2))
	| TVec d, TVec d' when d = d' ->
		make_reg_value pos (RVReg (ins_binop_add_v d code reg1 reg2))
	| tp1, tp2 ->
		Errors.error_p pos "Addition for types %s * %s is not defined."
			(string_of_typ tp1) (string_of_typ tp2);
		raise Unfold_exception

let unfold_sub pos code reg1 reg2 =
	match reg1.var_typ, reg2.var_typ with
	| TInt, TInt ->
		make_reg_value pos (RVReg (ins_binop_sub_i code reg1 reg2))
	| TInt, TFloat ->
		make_reg_value pos (RVReg (ins_binop_sub_f code (ins_convert_i2f code reg1) reg2))
	| TFloat, TInt ->
		make_reg_value pos (RVReg (ins_binop_sub_f code reg1 (ins_convert_i2f code reg1)))
	| TFloat, TFloat ->
		make_reg_value pos (RVReg (ins_binop_sub_f code reg1 reg2))
	| TMat(d1, d2), TMat(d1', d2') when d1 = d1' && d2 = d2' ->
		make_reg_value pos (RVReg (ins_binop_sub_m d1 d2 code reg1 reg2))
	| TVec d, TVec d' when d = d' ->
		make_reg_value pos (RVReg (ins_binop_sub_v d code reg1 reg2))
	| tp1, tp2 ->
		Errors.error_p pos "Subtraction for types %s * %s is not defined."
			(string_of_typ tp1) (string_of_typ tp2);
		raise Unfold_exception

let unfold_mul pos code reg1 reg2 =
	match reg1.var_typ, reg2.var_typ with
	| TInt, TInt ->
		make_reg_value pos (RVReg (ins_binop_mul_i code reg1 reg2))
	| TInt, TFloat ->
		make_reg_value pos (RVReg (ins_binop_mul_ff code (ins_convert_i2f code reg1) reg2))
	| TFloat, TInt ->
		make_reg_value pos (RVReg (ins_binop_mul_ff code reg1 (ins_convert_i2f code reg2)))
	| TFloat, TFloat ->
		make_reg_value pos (RVReg (ins_binop_mul_ff code reg1 reg2))
	| TInt, TMat(d1, d2) ->
		make_reg_value pos (RVReg (ins_binop_mul_mf d1 d2 code reg2 (ins_convert_i2f code reg1)))
	| TFloat, TMat(d1, d2) ->
		make_reg_value pos (RVReg (ins_binop_mul_mf d1 d2 code reg2 reg1))
	| TInt, TVec d ->
		make_reg_value pos (RVReg (ins_binop_mul_vf d code reg2 (ins_convert_i2f code reg1)))
	| TFloat, TVec d ->
		make_reg_value pos (RVReg (ins_binop_mul_vf d code reg2 reg1))
	| TMat(d1, d2), TInt ->
		make_reg_value pos (RVReg (ins_binop_mul_mf d1 d2 code reg1 (ins_convert_i2f code reg2)))
	| TMat(d1, d2), TFloat ->
		make_reg_value pos (RVReg (ins_binop_mul_mf d1 d2 code reg1 reg2))
	| TMat(d1, d2), TMat(d3, d4) when d2 = d3 ->
		make_reg_value pos (RVReg (ins_binop_mul_mm d1 d2 d4 code reg1 reg2))
	| TMat(d1, d2), TVec d when d2 = d ->
		make_reg_value pos (RVReg (ins_binop_mul_mv d1 d2 code reg1 reg2))
	| TVec d, TInt ->
		make_reg_value pos (RVReg (ins_binop_mul_vf d code reg1 (ins_convert_i2f code reg2)))
	| TVec d, TFloat  ->
		make_reg_value pos (RVReg (ins_binop_mul_vf d code reg1 reg2))
	| TVec d1, TVec d2 when d1 = d2 ->
		make_reg_value pos (RVReg (ins_binop_mul_vv d1 code reg1 reg2))
	| tp1, tp2 ->
		Errors.error_p pos "Multiplication for types %s * %s is not defined."
			(string_of_typ tp1) (string_of_typ tp2);
		raise Unfold_exception

let unfold_div pos code reg1 reg2 =
	match reg1.var_typ, reg2.var_typ with
	| TInt, TInt ->
		make_reg_value pos (RVReg (ins_binop_div_i code reg1 reg2))
	| TInt, TFloat ->
		make_reg_value pos (RVReg (ins_binop_div_ff code (ins_convert_i2f code reg1) reg2))
	| TFloat, TInt ->
		make_reg_value pos (RVReg (ins_binop_div_ff code reg1 (ins_convert_i2f code reg2)))
	| TFloat, TFloat ->
		make_reg_value pos (RVReg (ins_binop_div_ff code reg1 reg2))
	| TInt, TVec d ->
		make_reg_value pos (RVReg (ins_binop_div_fv d code (ins_convert_i2f code reg1) reg2))
	| TFloat, TVec d ->
		make_reg_value pos (RVReg (ins_binop_div_fv d code reg1 reg2))
	| TMat(d1, d2), TInt ->
		make_reg_value pos (RVReg (ins_binop_div_mf d1 d2 code reg1 (ins_convert_i2f code reg2)))
	| TMat(d1, d2), TFloat -> 
		make_reg_value pos (RVReg (ins_binop_div_mf d1 d2 code reg1 reg2))
	| TVec d, TInt ->
		make_reg_value pos (RVReg (ins_binop_div_vf d code reg1 (ins_convert_i2f code reg2)))
	| TVec d, TFloat ->
		make_reg_value pos (RVReg (ins_binop_div_vf d code reg1 reg2))
	| TVec d, TVec d' when d = d' ->
		make_reg_value pos (RVReg (ins_binop_div_vv d code reg1 reg2))
	| tp1, tp2 ->
		Errors.error_p pos "Division for types %s * %s is not defined."
			(string_of_typ tp1) (string_of_typ tp2);
		raise Unfold_exception

let unfold_mod pos code reg1 reg2 =
	match reg1.var_typ, reg2.var_typ with
	| TInt, TInt ->
		make_reg_value pos (RVReg (ins_binop_mod_i code reg1 reg2))
	| TInt, TFloat ->
		make_reg_value pos (RVReg (ins_binop_mod_ff code (ins_convert_i2f code reg1) reg2))
	| TFloat, TInt ->
		make_reg_value pos (RVReg (ins_binop_mod_ff code reg1 (ins_convert_i2f code reg2)))
	| TFloat, TFloat ->
		make_reg_value pos (RVReg (ins_binop_mod_ff code reg1 reg2))
	| TInt, TVec d ->
		make_reg_value pos (RVReg (ins_binop_mod_fv d code (ins_convert_i2f code reg1) reg2))
	| TFloat, TVec d ->
		make_reg_value pos (RVReg (ins_binop_mod_fv d code reg1 reg2))
	| TMat(d1, d2), TInt ->
		make_reg_value pos (RVReg (ins_binop_mod_mf d1 d2 code reg1 (ins_convert_i2f code reg2)))
	| TMat(d1, d2), TFloat ->
		make_reg_value pos (RVReg (ins_binop_mod_mf d1 d2 code reg1 reg2))
	| TVec d, TInt ->
		make_reg_value pos (RVReg (ins_binop_mod_vf d code reg1 (ins_convert_i2f code reg2)))
	| TVec d, TFloat ->
		make_reg_value pos (RVReg (ins_binop_mod_vf d code reg1 reg2))
	| TVec d, TVec d' when d = d' ->
		make_reg_value pos (RVReg (ins_binop_mod_vv d code reg1 reg2))
	| tp1, tp2 ->
		Errors.error_p pos "Modulo for types %s * %s is not defined."
			(string_of_typ tp1) (string_of_typ tp2);
		raise Unfold_exception

let unfold_dot pos code reg1 reg2 =
	match reg1.var_typ, reg2.var_typ with
	| (TInt | TFloat), (TInt | TFloat) ->
		unfold_mul pos code reg1 reg2
	| TVec d, TVec d' when d = d' ->
		make_reg_value pos (RVReg (ins_binop_dot d code reg1 reg2))
	| tp1, tp2 ->
		Errors.error_p pos "Dot product for types %s * %s is not defined."
			(string_of_typ tp1) (string_of_typ tp2);
		raise Unfold_exception

let unfold_cross pos code reg1 reg2 =
	match reg1.var_typ, reg2.var_typ with
	| TVec Dim2, TVec Dim2 ->
		make_reg_value pos (RVReg (ins_binop_cross2 code reg1 reg2))
	| TVec Dim3, TVec Dim3 ->
		make_reg_value pos (RVReg (ins_binop_cross3 code reg1 reg2))
	| tp1, tp2 ->
		Errors.error_p pos "Cross product for types %s * %s is not defined."
			(string_of_typ tp1) (string_of_typ tp2);
		raise Unfold_exception

let unfold_pow pos code reg1 reg2 =
	match reg1.var_typ, reg2.var_typ with
	| TInt, TInt ->
		make_reg_value pos (RVReg (ins_binop_pow_i code reg1 reg2))
	| TInt, TFloat ->
		make_reg_value pos (RVReg (ins_binop_pow_ff code (ins_convert_i2f code reg1) reg2))
	| TFloat, TInt ->
		make_reg_value pos (RVReg (ins_binop_pow_ff code reg1 (ins_convert_i2f code reg2)))
	| TFloat, TFloat ->
		make_reg_value pos (RVReg (ins_binop_pow_ff code reg1 reg2))
	| TVec d, TInt ->
		make_reg_value pos (RVReg (ins_binop_pow_vf d code reg1 (ins_convert_i2f code reg2)))
	| TVec d, TFloat ->
		make_reg_value pos (RVReg (ins_binop_pow_vf d code reg1 reg2))
	| TVec d, TVec d' when d = d' ->
		make_reg_value pos (RVReg (ins_binop_pow_vv d code reg1 reg2))
	| tp1, tp2 ->
		Errors.error_p pos "Power for types %s * %s is not defined."
			(string_of_typ tp1) (string_of_typ tp2);
		raise Unfold_exception

let unfold_min pos code reg1 reg2 =
	match reg1.var_typ, reg2.var_typ with
	| TInt, TInt ->
		make_reg_value pos (RVReg (ins_binop_min_i code reg1 reg2))
	| TInt, TFloat ->
		make_reg_value pos (RVReg (ins_binop_min_f code (ins_convert_i2f code reg1) reg2))
	| TFloat, TInt ->
		make_reg_value pos (RVReg (ins_binop_min_f code reg1 (ins_convert_i2f code reg2)))
	| TFloat, TFloat ->
		make_reg_value pos (RVReg (ins_binop_min_f code reg1 reg2))
	| TVec d, TVec d' when d = d' ->
		make_reg_value pos (RVReg (ins_binop_min_v d code reg1 reg2))
	| tp1, tp2 ->
		Errors.error_p pos "Minimum for types %s * %s is not defined."
			(string_of_typ tp1) (string_of_typ tp2);
		raise Unfold_exception

let unfold_neg pos code reg =
	match reg.var_typ with
	| TInt -> make_reg_value pos (RVReg (ins_unop_neg_i code reg))
	| TFloat -> make_reg_value pos (RVReg (ins_unop_neg_f code reg))
	| TMat(d1, d2) -> make_reg_value pos (RVReg (ins_unop_neg_m d1 d2 code reg))
	| TVec d -> make_reg_value pos (RVReg (ins_unop_neg_v d code reg))
	| tp ->
		Errors.error_p pos "Unary minus for type %s is not defined."
			(string_of_typ tp);
		raise Unfold_exception

let unfold_uplus pos code reg =
	match reg.var_typ with
	| TFloat | TMat _ | TVec _ -> make_reg_value pos (RVReg reg)
	| tp ->
		Errors.error_p pos "Unary plus for type %s is not defined."
			(string_of_typ tp);
		raise Unfold_exception

let unfold_code_var gamma expr x =
	try
		StrMap.find x gamma
	with
	| Not_found ->
		begin match TopDef.check_name x with
		| None ->
			Errors.error_p expr.MlslAst.e_pos "Unbound variable %s" x;
			raise Unfold_exception
		| Some value -> reg_value_of_value value
		end

let make_varying pos program_type name =
	match program_type with
	| PVertex ->
		Errors.error_p pos "Varying variables are not allowed in vertex programs.";
		raise Unfold_exception
	| PFragment ->
		begin try
			let vr = Hashtbl.find varying_map name in
				make_reg_value pos (RVReg vr)
			with
			| Not_found ->
				Errors.error_p pos "Undefinded varying variable $%s." name;
				raise Unfold_exception
		end

let unfold_swizzle pos program_type code rv swizzle =
	match const_or_reg_value_kind pos program_type rv with
	| RVReg rvreg ->
		begin match rvreg.var_typ with
		| TInt | TFloat ->
			if MlslAst.Swizzle.max_component_id swizzle = 0 then
				let rreg = create_variable VSTemporary 
					(vectyp_of_int (MlslAst.Swizzle.size swizzle)) in
				Misc.ImpList.add code 
					(create_instr (ISwizzle(rreg, rvreg, swizzle)));
				make_reg_value pos (RVReg rreg)
			else begin
				Errors.error_p pos
					"Value defined at %s has type float, can not be swizzled using pattern %s."
					(Errors.string_of_pos rv.rv_pos)
					(MlslAst.Swizzle.to_string swizzle);
				raise Unfold_exception
			end
		| TVec d ->
			if MlslAst.Swizzle.max_component_id swizzle < int_of_dim d then
				let rreg = create_variable VSTemporary 
					(vectyp_of_int (MlslAst.Swizzle.size swizzle)) in
				Misc.ImpList.add code 
					(create_instr (ISwizzle(rreg, rvreg, swizzle)));
				make_reg_value pos (RVReg rreg)
			else begin
				Errors.error_p pos
					"Value defined at %s has type %s, can not be swizzled using pattern %s."
					(Errors.string_of_pos rv.rv_pos)
					(string_of_typ rvreg.var_typ)
					(MlslAst.Swizzle.to_string swizzle);
				raise Unfold_exception
			end
		| tp ->
			Errors.error_p pos "Value defined at %s has type %s, can not be swizzled."
				(Errors.string_of_pos rv.rv_pos)
				(string_of_typ tp);
			raise Unfold_exception
		end
	| RVValue value ->
		EvalPrim.with_exn Unfold_exception (fun () ->
			reg_value_of_value (EvalPrim.eval_swizzle pos value swizzle)
		)
	| kind ->
		Errors.error_p pos
			"Value defined at %s is a %s, can not be swizzled."
			(Errors.string_of_pos rv.rv_pos)
			(string_of_rvkind kind);
		raise Unfold_exception

let register_of_const_or_reg pos code kind =
	match kind with
	| RVReg reg -> reg
	| RVValue value ->
		begin match value_kind pos value with
		| TopDef.VBool b ->
			let rreg = create_variable VSTemporary TBool in
			Misc.ImpList.add code (create_instr (IConstBool(rreg, b)));
			rreg
		| TopDef.VInt n ->
			let rreg = create_variable VSTemporary TInt in
			Misc.ImpList.add code (create_instr (IConstInt(rreg, n)));
			rreg
		| TopDef.VFloat f ->
			let rreg = create_variable VSTemporary TFloat in
			Misc.ImpList.add code (create_instr (IConstFloat(rreg, f)));
			rreg
		| TopDef.VVec(dim, v) ->
			let rreg = create_variable VSTemporary (TVec dim) in
			Misc.ImpList.add code (create_instr (IConstVec(rreg, dim, v)));
			rreg
		| TopDef.VMat(d1, d2, m) ->
			let rreg = create_variable VSTemporary (TMat(d1, d2)) in
			Misc.ImpList.add code (create_instr (IConstMat(rreg, d1, d2, m)));
			rreg
		| _ ->
			Errors.error_p pos "Value defined at %s is not a primitive value."
				(Errors.string_of_pos value.TopDef.v_pos);
			raise Unfold_exception
		end
	| _ ->
		Errors.error_p pos "This is not a primitive value.";
		raise Unfold_exception

let unfold_binop pos program_type code op rkind1 rkind2 =
	match rkind1, rkind2 with
	| RVValue v1, RVValue v2 ->
		EvalPrim.with_exn Unfold_exception (fun () ->
			reg_value_of_value (EvalPrim.eval_binop pos op v1 v2)
		)
	| _ ->
		let r1 = register_of_const_or_reg pos code rkind1 in
		let r2 = register_of_const_or_reg pos code rkind2 in
		begin match op with
		| MlslAst.BOEq ->
			Errors.error_p pos "Unimplemented: unfold_code EBinOp(BOEq, _, _)";
			raise Unfold_exception
		| MlslAst.BONeq ->
			Errors.error_p pos "Unimplemented: unfold_code EBinOp(BONeq, _, _)";
			raise Unfold_exception
		| MlslAst.BOGe ->
			Errors.error_p pos "Unimplemented: unfold_code EBinOp(BOLe, _, _)";
			raise Unfold_exception
		| MlslAst.BOLt ->
			Errors.error_p pos "Unimplemented: unfold_code EBinOp(BOLt, _, _)";
			raise Unfold_exception
		| MlslAst.BOLe ->
			Errors.error_p pos "Unimplemented: unfold_code EBinOp(BOGe, _, _)";
			raise Unfold_exception
		| MlslAst.BOGt ->
			Errors.error_p pos "Unimplemented: unfold_code EBinOp(BOGt, _, _)";
			raise Unfold_exception
		| MlslAst.BOAdd ->
			unfold_add pos code r1 r2
		| MlslAst.BOSub ->
			unfold_sub pos code r1 r2
		| MlslAst.BOMul -> 
			unfold_mul pos code r1 r2
		| MlslAst.BODiv ->
			unfold_div pos code r1 r2
		| MlslAst.BOMod ->
			unfold_mod pos code r1 r2
		| MlslAst.BODot ->
			unfold_dot pos code r1 r2
		| MlslAst.BOCross ->
			unfold_cross pos code r1 r2
		| MlslAst.BOPow ->
			unfold_pow pos code r1 r2
		| MlslAst.BOMin ->
			unfold_min pos code r1 r2
		end

let unfold_unop pos program_type code op kind =
	match kind with
	| RVValue value ->
		EvalPrim.with_exn Unfold_exception (fun () ->
			reg_value_of_value (EvalPrim.eval_unop pos op value)
		)
	| RVReg r ->
		begin match op with
		| MlslAst.UONeg ->
			unfold_neg pos code r
		| MlslAst.UOPlus ->
			unfold_uplus pos code r
		end
	| kind ->
		Errors.error_p pos 
			"Operand of %s can not be a %s."
			(MlslAst.unop_name op)
			(string_of_rvkind kind);
		raise Unfold_exception

let rec merge_if_branches pos program_type code cond_var result1 result2 =
	let rv1 = concrete_reg_value_kind pos program_type code result1 in
	let rv2 = concrete_reg_value_kind pos program_type code result2 in
	match rv1, rv2 with
	| RVReg reg1, RVReg reg2 ->
		begin match reg1.var_typ, reg2.var_typ with
		| TBool, TBool ->
			make_reg_value pos (RVReg (
				ins_binop_add_b code
					(ins_binop_and_b code cond_var reg1)
					(ins_binop_and_b code (ins_unop_not_b code cond_var) reg2)
				))
		| TInt, TInt ->
			make_reg_value pos (RVReg (
				ins_binop_add_i code
					(ins_binop_mul_i code (ins_convert_b2i code cond_var) reg1)
					(ins_binop_mul_i code 
						(ins_convert_b2i code (ins_unop_not_b code cond_var)) 
						reg2)
				))
		| (TInt | TFloat), (TInt | TFloat) ->
			make_reg_value pos (RVReg (
				ins_binop_add_f code
					(ins_binop_mul_ff code 
						(ins_convert_b2f code cond_var)
						(ins_convert_2f code reg1))
					(ins_binop_mul_ff code
						(ins_convert_b2f code (ins_unop_not_b code cond_var))
						(ins_convert_2f code reg2))
				))
		| TVec d, TVec d' when d = d' ->
			make_reg_value pos (RVReg (
				ins_binop_add_v d code
					(ins_binop_mul_vf d code reg1 (ins_convert_b2f code cond_var))
					(ins_binop_mul_vf d code reg2 (ins_convert_b2f code (ins_unop_not_b code cond_var)))
				))
		| TMat(d1, d2), TMat(d1', d2') when d1 = d1' && d2 = d2' ->
			make_reg_value pos (RVReg (
				ins_binop_add_m d1 d2 code
					(ins_binop_mul_mf d1 d2 code reg1 (ins_convert_b2f code cond_var))
					(ins_binop_mul_mf d1 d2 code reg2 (ins_convert_b2f code (ins_unop_not_b code cond_var)))
				))
		| tp1, tp2 ->
			Errors.error_p pos "Values of types %s and %s can not be merged."
				(string_of_typ tp1) (string_of_typ tp2);
			raise Unfold_exception
		end
	| RVRecord rd1, RVRecord rd2 ->
		make_reg_value pos (RVRecord (StrMap.merge (fun name v1 v2 ->
			match v1, v2 with
			| Some rv1, Some rv2 ->
				Some(merge_if_branches pos program_type code cond_var rv1 rv2)
			| _ -> None
			) rd1 rd2))
	| RVPair(va1, va2), RVPair(vb1, vb2) ->
		make_reg_value pos (RVPair
			( merge_if_branches pos program_type code cond_var va1 vb1
			, merge_if_branches pos program_type code cond_var va2 vb2
			))
	| (RVSampler _ | RVFunc _ | RVIfFunc _), (RVSampler _ | RVFunc _ | RVIfFunc _) ->
		make_reg_value pos (RVIfFunc(cond_var, result1, result2))
	| kind1, kind2 ->
		Errors.error_p pos "Can not merge %s and %s."
			(string_of_rvkind kind1) (string_of_rvkind kind2);
		raise Unfold_exception

let rec unfold_if_statement pos program_type code gamma cnd e1 e2 =
	let rvcnd = unfold_code program_type code gamma cnd in
	match const_or_reg_value_kind pos program_type rvcnd with
	| RVReg reg ->
		begin match reg.var_typ with
		| TBool ->
			let result1 = unfold_code program_type code gamma e1 in
			let result2 = unfold_code program_type code gamma e2 in
			merge_if_branches pos program_type code reg result1 result2
		| typ ->
			Errors.error_p pos 
				"Condition in if-statement defined at %s has type %s, but expected bool."
				(Errors.string_of_pos rvcnd.rv_pos)
				(string_of_typ typ);
			raise Unfold_exception
		end
	| RVValue value ->
		begin match value_kind pos value with
		| TopDef.VBool b ->
			unfold_code program_type code gamma (if b then e1 else e2)
		| kind ->
			Errors.error_p pos 
				"Condition in if-statement defined at %s is a %s, but expected boolean value."
				(Errors.string_of_pos rvcnd.rv_pos)
				(TopDef.string_of_value_kind kind);
			raise Unfold_exception
		end
	| kind ->
		Errors.error_p pos 
			"Condition in if-statement defined at %s is a %s, but expected boolean value."
			(Errors.string_of_pos rvcnd.rv_pos)
			(string_of_rvkind kind);
		raise Unfold_exception

and unfold_app pos program_type code gamma func arg =
	match concrete_reg_value_kind pos program_type code func with
	| RVSampler sampler ->
		begin match concrete_reg_value_kind pos program_type code arg with
		| RVReg coordreg ->
			begin match coordreg.var_typ with
			| TVec Dim2 ->
				let rreg = create_variable VSTemporary (TVec Dim4) in
				Misc.ImpList.add code 
					(create_instr (ITex(rreg, coordreg, sampler)));
				make_reg_value pos (RVReg rreg)
			| tp ->
				Errors.error_p pos
					"Texture coordinates defined at %s have type %s, but expected vec2."
					(Errors.string_of_pos arg.rv_pos)
					(string_of_typ tp);
				raise Unfold_exception
			end
		| kind ->
			Errors.error_p pos
				"Value defined at %s is a %s, can not be used as texture coordinates."
				(Errors.string_of_pos arg.rv_pos)
				(string_of_rvkind kind);
			raise Unfold_exception
		end
	| RVFunc(closure, pat, body) ->
		if !credits <= 0 then begin
			Errors.error_p pos
				"Too complex functional code. Unfolding requires more than 1024 function applications.";
			raise Unfold_exception
		end else begin
			credits := !credits - 1;
			let gamma = bind_pattern code closure pat arg in
			unfold_code program_type code gamma body
		end
	| RVIfFunc(cond_var, func1, func2) ->
		let result1 = unfold_app pos program_type code gamma func1 arg in
		let result2 = unfold_app pos program_type code gamma func2 arg in
		merge_if_branches pos program_type code cond_var result1 result2
	| _ ->
		Errors.error_p pos
			"Value defined at %s is not a function/sampler, can not be applied."
			(Errors.string_of_pos func.rv_pos);
			raise Unfold_exception

and unfold_code program_type code gamma expr =
	match expr.MlslAst.e_kind with
	| MlslAst.EVar x ->
		unfold_code_var gamma expr x
	| MlslAst.EVarying x ->
		make_varying expr.MlslAst.e_pos program_type x
	| MlslAst.EInt n ->
		make_value expr.MlslAst.e_pos (TopDef.VInt n)
	| MlslAst.EFloat f ->
		make_value expr.MlslAst.e_pos (TopDef.VFloat f)
	| MlslAst.ETrue ->
		make_value expr.MlslAst.e_pos (TopDef.VBool true)
	| MlslAst.EFalse ->
		make_value expr.MlslAst.e_pos (TopDef.VBool false)
	| MlslAst.ESwizzle(e, swizzle) ->
		let rv = unfold_code program_type code gamma e in
		unfold_swizzle expr.MlslAst.e_pos program_type code rv swizzle
	| MlslAst.ERecord rd ->
		let rd' =
			List.fold_left (fun regMap field ->
				let rv = unfold_code program_type code gamma field.MlslAst.rfv_value in
					StrMap.add field.MlslAst.rfv_name rv regMap
				) StrMap.empty rd
		in
			make_reg_value expr.MlslAst.e_pos (RVRecord rd')
	| MlslAst.ESelect(e, field) ->
		Errors.error_p expr.MlslAst.e_pos "Unimplemented: unfold_code ESelect.";
		raise Unfold_exception
	| MlslAst.EPair(e1, e2) ->
		Errors.error_p expr.MlslAst.e_pos "Unimplemented: unfold_code EPair.";
		raise Unfold_exception
	| MlslAst.EBinOp(op, e1, e2) ->
		let rv1 = unfold_code program_type code gamma e1 in
		let rv2 = unfold_code program_type code gamma e2 in
		unfold_binop expr.MlslAst.e_pos program_type code op
			(const_or_reg_value_kind e1.MlslAst.e_pos program_type rv1)
			(const_or_reg_value_kind e2.MlslAst.e_pos program_type rv2)
	| MlslAst.EUnOp(op, e) ->
		let rv = unfold_code program_type code gamma e in
		unfold_unop expr.MlslAst.e_pos program_type code op
			(const_or_reg_value_kind e.MlslAst.e_pos program_type rv)
	| MlslAst.EAbs(pat, e) ->
		make_reg_value expr.MlslAst.e_pos (RVFunc(gamma, pat, e))
	| MlslAst.EApp(e1, e2) ->
		let func = unfold_code program_type code gamma e1 in
		let arg  = unfold_code program_type code gamma e2 in
		unfold_app expr.MlslAst.e_pos program_type code gamma func arg
	| MlslAst.ELet(pat, e1, e2) ->
		let rv1   = unfold_code program_type code gamma e1 in
		let gamma = bind_pattern code gamma pat rv1  in
			unfold_code program_type code gamma e2
	| MlslAst.EFix(pat, e) ->
		Errors.error_p expr.MlslAst.e_pos "Unimplemented: unfold_code EFix.";
		raise Unfold_exception
	| MlslAst.EIf(cnd, e1, e2) ->
		unfold_if_statement expr.MlslAst.e_pos program_type code gamma cnd e1 e2
	| MlslAst.EMatch _ ->
		Errors.error_p expr.MlslAst.e_pos "Unimplemented: unfold_code EMatch.";
		raise Unfold_exception
	| MlslAst.EFragment _ | MlslAst.EVertex _ ->
		Errors.error_p expr.MlslAst.e_pos
			"Can not unfold shader inside shader.";
		raise Unfold_exception
	| MlslAst.EConstrU _ ->
		Errors.error_p expr.MlslAst.e_pos "Unimplemented: unfold_code EConstrU.";
		raise Unfold_exception
	| MlslAst.EConstrP _ ->
		Errors.error_p expr.MlslAst.e_pos "Unimplemented: unfold_code EConstrP.";
		raise Unfold_exception

let unfold_vertex gamma expr code =
	let reg_val = unfold_code PVertex code gamma expr in
	let rv_kind = concrete_reg_value_kind expr.MlslAst.e_pos PVertex code reg_val in
	match rv_kind with
	| RVRecord rd ->
		if not (StrMap.mem "position" rd) then begin
			Errors.error_p expr.MlslAst.e_pos 
				"This record defined at %s has not \"position\" field."
				(Errors.string_of_pos reg_val.rv_pos);
			raise Unfold_exception
		end else begin
			let ok = StrMap.fold (fun v_name v_rv st ->
				if v_name = "position" then st
				else match concrete_reg_value_kind expr.MlslAst.e_pos PVertex code v_rv with
				| RVReg vr ->
					let vv = create_variable VSVarying vr.var_typ in
					Misc.ImpList.add code (create_instr (IMov(vv, vr)));
					Hashtbl.add varying_map v_name vv; 
					st
				| _ ->
					Errors.error_p expr.MlslAst.e_pos 
						"Field %s defined at %s is not a primitive value."
						v_name (Errors.string_of_pos v_rv.rv_pos);
					false
				) rd true in
			let result_rv_kind = 
				concrete_reg_value_kind expr.MlslAst.e_pos PVertex code 
					(StrMap.find "position" rd) in
			match result_rv_kind with
			| RVReg rr ->
				begin match rr.var_typ with
				| TVec Dim4 ->
					Misc.ImpList.add code (create_instr (IRet rr));
					if ok then code
					else raise Unfold_exception
				| tp ->
					Errors.error_p expr.MlslAst.e_pos
						"Result position of vertex shader defined at %s has type %s, but expected vec4."
						(Errors.string_of_pos (StrMap.find "position" rd).rv_pos)
						(string_of_typ tp); 
					raise Unfold_exception
				end
			| _ ->
				Errors.error_p expr.MlslAst.e_pos
				"Result position of vertex shader defined at %s is not a primitive value."
				(Errors.string_of_pos (StrMap.find "position" rd).rv_pos); 
				raise Unfold_exception
		end
	| _ ->
		Errors.error_p expr.MlslAst.e_pos "Non record result of vertex shader.";
		raise Unfold_exception

let unfold_fragment gamma expr code =
	let reg_val = unfold_code PFragment code gamma expr in
	let rv_kind = concrete_reg_value_kind expr.MlslAst.e_pos PFragment code reg_val in
	match rv_kind with
	| RVReg col ->
		begin match col.var_typ with
		| TVec Dim4 ->
			Misc.ImpList.add code (create_instr (IRet col));
			code
		| tp ->
			Errors.error_p expr.MlslAst.e_pos
				"Result position of fragment shader defined at %s has type %s, but expected vec4."
				(Errors.string_of_pos reg_val.rv_pos)
				(string_of_typ tp); 
			raise Unfold_exception
		end
	| _ ->
		Errors.error_p expr.MlslAst.e_pos
			"Result of fragment shader defined at %s is not a primitive value."
			(Errors.string_of_pos reg_val.rv_pos); 
		raise Unfold_exception

let unfold_shader pos name value =
	credits := 1024;
	try
		match value_kind pos value with
		| TopDef.VPair(vs, fs) ->
			begin match value_kind pos vs, value_kind pos fs with
			| TopDef.VVertex(vs_gamma, vs_expr), TopDef.VFragment(fs_gamma, fs_expr) ->
				Hashtbl.clear attr_map;
				Hashtbl.clear v_const_map;
				Hashtbl.clear f_const_map;
				Hashtbl.clear varying_map;
				let vs_code   = Misc.ImpList.create () in
				let fs_code   = Misc.ImpList.create () in
				let vs_gamma' = StrMap.map reg_value_of_value vs_gamma in
				let fs_gamma' = StrMap.map reg_value_of_value fs_gamma in
				let vertex    = unfold_vertex vs_gamma' vs_expr vs_code in
				let fragment  = unfold_fragment fs_gamma' fs_expr fs_code in
					Some
						{ sh_name     = name
						; sh_attr     = create_attr_list ()
						; sh_v_const  = create_v_const_list ()
						; sh_f_const  = create_f_const_list ()
						; sh_varying  = create_varying_list ()
						; sh_samplers = create_sampler_list ()
						; sh_vertex   = Misc.ImpList.to_list vertex
						; sh_fragment = Misc.ImpList.to_list fragment
						}
			| TopDef.VVertex _, _ ->
				Errors.error_p fs.TopDef.v_pos 
					"This expression is not a fragment shader.";
				None
			| _, _ ->
				Errors.error_p vs.TopDef.v_pos 
					"This expression is not a vertex shader.";
				None
			end
		| _ ->
			Errors.error_p value.TopDef.v_pos 
				"Shader must be a pair of vertex and fragment shaders.";
			None
	with
	| Unfold_exception -> None

(* TODO: better optimizer *)
let optimize s = s
